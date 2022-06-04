lappend ::auto_path [pwd]
package require client_utils

### variables
namespace eval atl_cp {
	set terrain_priority {
		plain 0
		mountain 1
		forest 2
		mystforest 2
		jungle 3
		swamp 3
		lake 3
		hill 4
		desert 5
		wasteland 6
		tundra 7
	}
}

namespace eval gui {
	set currentTurn 0
}

### situation analysis
itcl::class SitRep {
	public variable overall_state
	public variable unit_state
	public variable regions
	public variable import_regions

	constructor {} {
		set ret [evaluateSituation]
		set overall_state [dGet $ret State]
		set unit_state [dGet $ret Units]
		set import_regions [db onecolumn {SELECT val FROM notes WHERE key="import_regions"}]
	}

	method saveState {} {
		::db eval {
			INSERT OR REPLACE INTO notes
			VALUES("import_regions", $import_regions)
		}
	}

	method inRegion {x y z} {
		return [info exists regions($x,$y,$z)]
	}

	method evaluateSituation {} {
		set ret [dict create]

		set units [db eval {
			SELECT detail.x, detail.y, detail.z, units.id, units.name, units.uid, units.items, units.orders
			FROM detail JOIN units
			ON detail.id=units.regionId
			WHERE detail.turn=$::currentTurn and units.detail='own'
		}]
		dict set ret Units $units

		if {$units eq ""} {
			dict set ret "State" "lost"
			return $ret
		}

		set in_nexus 1
		set all_leaders 1
		foreach {x y z id name uid items orders} $units {
			if {$z != 0} {
				set in_nexus 0
			}
			if {[lsearch $items *LEAD*] == -1} {
				set all_leaders 0
			}
		}

		if {$in_nexus || $all_leaders} {
			dict set ret "State" "start"
		} else {
			dict set ret "State" "main"
		}

		return $ret
	}

	method createOrders {}
	method buyGuards {budget claim x y z taxers}
}

### helper functions
proc isCoast {x y z} {
	foreach d {n nw ne sw se s} {
		set loc [moveCoord $x $y $d]
		set nx [lindex $loc 0]
		set ny [lindex $loc 1]

		set terrain [::db eval {
			SELECT type
			FROM terrain
			WHERE x=$nx AND y=$ny AND z=$z
		}]
		if {$terrain eq "ocean"} {
			return 1
		}
	}
	return 0
}

proc selectNewHex {sitRep x y z} {
	# TODO more move strategy
	# move away from poles
	if {$y < $::max_y / 2 - 2} {
		# north pole
		set dirs {s sw se nw ne n}
	} elseif {$y > $::max_y / 2 +2} {
		# south pole
		set dirs {n nw ne sw se s}
	} else {
		# equator
		set dirs {nw se sw ne n s}
	}
	set d_vals [list]

	set keep_out [::db onecolumn { SELECT val FROM notes WHERE key="keep_out"}]

	foreach d $dirs {
		set loc [moveCoord $x $y $d]
		set nx [lindex $loc 0]
		set ny [lindex $loc 1]

		if {$nx < 0 || $ny < 0 || $nx > $::max_x || $ny > $::max_y} {
			lappend d_vals 0
			continue
		}

		if {[lsearch $keep_out [list $nx $ny $z]] != -1} {
			lappend d_vals 0
			continue
		}

		# don't go to a hex we're already in
		if {[$sitRep inRegion $nx $ny $z]} {
			lappend d_vals 0
			continue
		}

		set terrain [::db onecolumn {
			SELECT type
			FROM terrain
			WHERE x=$nx AND y=$ny AND z=$z
		}]

		# can't move into ocean
		if {$terrain eq "ocean"} {
			lappend d_vals 0
			continue
		}

		switch $terrain {
			"plain" { set base_val 9 }

			"forest" -
			"mystforest" -
			"mountain" { set base_val 6 }

			default { set base_val 3 }
		}

		# degrade cities
		set res [::db onecolumn {
			SELECT city
			FROM terrain
			WHERE x=$nx AND y=$ny AND z=$z
		}]
		if {$res ne ""} {
			incr base_val -2
		} elseif {[isCoast $nx $ny $z]} {
			# degrade moving to coast
			incr base_val -1
		}

		lappend d_vals $base_val
	}

	set best_d [lindex [lsort -integer -decreasing -indices $d_vals] 0]
	if {[lindex $d_vals $best_d] == 0} {
		return ""
	}
	return [lindex $dirs $best_d]
}

proc getAddRaceAlign {db abbr} {
	# pull global table
	set race_align [$db onecolumn {SELECT val FROM notes WHERE key="race_align"}]

	# look up abbr
	set ra [dGet $race_align $abbr]
	if {$ra ne ""} {
		return $ra
	}

	# pull race description
	set race [$db onecolumn {SELECT desc FROM items WHERE abbr=$abbr}]
	set desc [dGet $race "Desc"]

	# look for alignment
	set i [lsearch $desc "*This race has alignment *"]
	if {$i != -1} {
		# extract alignment
		set rl [lindex $desc $i]
		regexp {This race has alignment (.*)} $rl -> ra
		if {$ra eq ""} {
			puts "No alignment for '$abbr'"
			set ra "neutral"
		} else {
			# update db
			set race_align [dict set $race_align $abbr $ra]
			$db eval {INSERT OR REPLACE INTO notes VALUES("race_align", $race_align)}
		}
	}

	return $ra
}

proc isAlignCompat {db abbr} {
	set ra [getAddRaceAlign $db $abbr]
	# anyone can buy neutral
	if {$ra eq "neutral"} {
		return 1
	}

	# pull faction alignment
	set alignment [$db onecolumn {SELECT val FROM notes WHERE key="alignment"}]
	if {$alignment eq ""} {
		puts "Faction alignment not set!"
		return 1
	}

	if {$alignment eq "neutral"} {
		# set alignment?
		return 0
	}

	if {$alignment ne $ra} {
		return 0
	}
	return 1
}

itcl::body SitRep::buyGuards {budget claim x y z taxers} {
	set rdata [db eval {
		SELECT id, sells, race, tax
		FROM detail
		WHERE x=$x AND y=$y AND z=$z AND turn=$::currentTurn
		ORDER BY turn DESC LIMIT 1
	}]
	if {$rdata eq ""} {
		puts "Unable to retrieve region data"
		exit 1
	}
	foreach {regionId sells peasants maxTax} $rdata {}

	if {$maxTax eq ""} { set maxTax 0 }
	set taxersNeeded [expr {$maxTax / 50}]
	if {$taxersNeeded == 0} {
		return [list "" "" 0]
	}
	if {$taxersNeeded <= $taxers} {
		return [list "" "" 0]
	}
	incr taxersNeeded -$taxers

	set ret [getBuyRace $sells $peasants]
	foreach {maxRace raceList price} $ret {}

	regexp {\[(.+)\]} [lindex $raceList 0] -> abbr

	# get alignment of local race
	set ra [getAddRaceAlign ::db $abbr]

	# pull faction alignment
	set alignment [db onecolumn {SELECT val FROM notes WHERE key="alignment"}]
	if {$alignment ne "" && $alignment ne "neutral"} {
		if {$ra ne "neutral" && $alignment ne $ra} {
			# note region for import of guards matching alignment
			lappendU import_regions [list $x $y $z]
			return [list "" "" 0]
		}
	}

	# limit by cash on hand
	# start without maintenance cost
	set maxBuy [expr {$budget / ($price + 10)}]
	set numBuy [expr {min($taxersNeeded, $maxBuy, $maxRace)}]
	set need_support 0

	# if not claiming, and taxers cannot support
	if {!$claim && $numBuy * 10 > $taxers * 50} {
		# drop buy
		set maxBuy [expr {$budget / ($price + 20)}]
		set numBuy [expr {min($taxersNeeded, $maxBuy, $maxRace)}]
		set need_support [expr {10 * $numBuy - $taxers * 50}]
	}
	if {$numBuy == 0} {
		return [list "" "" 1]
	}

	lappend ol "FORM 19" "name unit Guard"

	set give_amt [expr {$numBuy * ($price + 10)}]

	if {$claim} {
		lappend ol "claim $give_amt"
	}
	if {$need_support} {
		incr give_amt $need_support
	}

	lappend ol "avoid 0" "behind 0"
	lappend ol "buy $numBuy $abbr"
	lappend ol "study COMB"
	lappend ol "turn" "@tax" "endturn"
	lappend ol "END"

	return [list \
		$give_amt \
		$ol \
		[expr {$taxersNeeded - $numBuy}]]
}

proc checkStay {rid x y z} {
	set res [db eval {
		SELECT name
		FROM units
		WHERE regionId=$rid AND detail<>'own'
	}]
	foreach n $res {
		if {$n eq "- Tribe of Centaurs"} {
			continue
		}
		set keep_out [::db onecolumn { SELECT val FROM notes WHERE key="keep_out"}]
		lappendU keep_out [list $x $y $z]
		::db eval { INSERT OR REPLACE INTO notes VALUES("keep_out", $keep_out)}
		return 1
	}
	return 0
}

proc addOrder {ulist o} {
	foreach ui $ulist {
		set ol [$ui cget -orders]
		lappend ol {*}$o
		$ui configure -orders $ol

		set unit_id [$ui cget -db_id]
		db eval {
			UPDATE units SET orders=$ol
			WHERE id=$unit_id
		}
	}
}

proc rampFirstHex {sitRep units} {
	foreach {x y z unit_id name unum il ol} $units {}
	set rid [::db onecolumn {SELECT id FROM detail WHERE x=$x AND y=$y AND z=$z AND turn=$::currentTurn}]
	set all_u [getUnitObjects $rid]

	# find faction leader
	set u ""
	foreach ui $all_u {
		if {[lsearch [$ui cget -items] *FLEAD*] != -1} {
			set u $ui
		}
	}
	set unit_id [$u cget -db_id]

	# TODO check to make sure we got out of the starting city (exit wasn't blocked)
	# or that we landed in a good spot
	set no_exits [::db onecolumn { SELECT val FROM notes WHERE key="no_exits"}]
	if {$no_exits ne ""} {
		set max_jumps [::db onecolumn { SELECT val FROM notes WHERE key="jump"}]
		if {$max_jumps eq ""} { set max_jumps 0 }
		set num_jumps [::db onecolumn { SELECT val FROM notes WHERE key="num_jumps"}]
		if {$num_jumps eq ""} { set num_jumps 0 }

		# examine surrounding terrain
		set ct_plain 0
		set ct_forest 0
		set ct_mountain 0
		set ct_ocean 0
		foreach d {n nw ne sw se s} {
			set loc [moveCoord $x $y $d]
			set nx [lindex $loc 0]
			set ny [lindex $loc 1]
			lappend loc $z

			set terrain [::db eval {
				SELECT type
				FROM terrain
				WHERE x=$nx AND y=$ny AND z=$z
			}]
			switch $terrain {
				"plain" { incr ct_plain }
				"mystforest" -
				"forest" { incr ct_forest }
				"mountain" { incr ct_mountain }
				"ocean" { incr ct_ocean }
			}
		}

		set stay 0; # probably bad
		set near_jumps [expr {$num_jumps > $max_jumps - 2}]

		if {$ct_ocean > 0} {
			# leave stay 0
		} elseif {$ct_plain > 4 || ($ct_plain > 0 && $ct_forest + $ct_mountain > 2)} {
			# amazing!
			set stay 1
		} elseif {$ct_plain + $ct_forest + $ct_mountain > 1} {
			# take anything ok
			set stay 1
		}

		if {$near_jumps && $ct_plain + $ct_forest + $ct_mountain > 0} {
			# take anything not terrible
			set stay 1
		} elseif {$num_jumps >= $max_jumps} {
			# out of time
			set stay 1
		}

		if {!$stay} {
			advanceLeaders $u $all_u
			set ol [$u cget -orders]

			# TODO drop scout

			lappend ol [formGateCmd $u $all_u]
			db eval {
				UPDATE units SET orders=$ol
				WHERE id=$unit_id
			}
			::db eval { UPDATE notes SET val=val + 1 WHERE key="num_jumps" }
			return
		}
	}

	if {[checkStay $rid $x $y $z]} {
		set new_dir [selectNewHex $sitRep $x $y $z]
		if {$new_dir ne ""} {
			addOrder $all_u [list "MOVE $new_dir"]
		}
		return
	}

	advanceLeaders $u $all_u
	set ol [$u cget -orders]

	# TODO calculate a good budget to use
	set budget 2600

	set ret [$sitRep buyGuards $budget 1 $x $y $z 0]
	foreach {s_need form_orders rem} $ret {}

	set ol [concat $ol $form_orders]

	db eval {
		UPDATE units SET orders=$ol
		WHERE id=$unit_id
	}
}

proc formGateCmd {u all_u {multi_lead 0}} {
	set gate_order "CAST GATE RANDOM"
	if {[llength $all_u] > 1} {
		append gate_order " UNITS"
		foreach ui $all_u {
			if {$ui ne $u} {
				append gate_order " [$ui cget -num]"
			}
		}
		for {set i 0} {$i < $multi_lead} {incr i} {
			append gate_order " NEW [expr {$i + 1}]"
		}
	}
	return $gate_order
}

proc pickStartDirection {rid units} {
	set exits [list]

	set res [db eval {
		SELECT dir, dest
		FROM nexus_exits
	}]
	foreach {dir dest} $res {
		set ex_n [dict create]
		dict set ex_n Dir $dir
		dict set ex_n Loc $dest

		foreach {x y z} $dest {}
		set terrain [db eval {
			SELECT type FROM terrain
			WHERE x=$x and y=$y and z=$z
		}]
		dict set ex_n Terrain $terrain
		dict set ex_n Score [dict get $atl_cp::terrain_priority $terrain]

		lappend exits $ex_n
	}

	# create unit objects
	set all_u [getUnitObjects $rid]
	set multi_lead 0

	# find faction leader
	foreach ui $all_u {
		set items [$ui cget -items]
		if {[lsearch -index 2 $items *FLEAD*] ne -1} {
			set u $ui
			continue
		}
		set i [lsearch -index 2 $items *LEAD*]
		if {$i != -1} {
			set ct [lindex $items $i 0]
			if {$ct > 1} {
				set multi_lead [expr {$ct - 1}]
				# split leaders into 1 man units
				set ol [$ui cget -orders]
				for {set i 0} {$i < $multi_lead} {incr i} {
					set ni [expr {$i + 1}]
					lappend ol "FORM $ni"
					lappend ol "CLAIM 50"
					lappend ol "STUDY STEA"
					lappend ol "END"
					lappend ol "GIVE NEW $ni 1 LEAD"
				}
				lappend ol "CLAIM 200"
				lappend ol "STUDY TACT"
				$ui configure -orders $ol
			}
		}
	}

	set ol [$u cget -orders]
	#lappend ol "faction war 3 trade 2"

	if {$exits eq ""} {
		# must jump
		::db eval {
			INSERT OR REPLACE INTO notes
			VALUES("no_exits", "1")
		}

		# create gate order
		lappend ol [formGateCmd $u $all_u $multi_lead]

		$u configure -orders $ol

		# you can study and jump
		advanceLeaders $u $all_u

		# starting orders
		addOrder $all_u [list "behind 1" "avoid 1"]
		return
	}

	foreach {x y z unit_id name uid il ol} $units {
	}

	# find the best
	set sorted_exits [lsort -index 7 $exits]
	set e [lindex $sorted_exits 0]
	set best_score [dict get $e Score]
	set best_dir [dict get $e Dir]
	foreach e [lrange $sorted_exits 1 end] {
		if {$best_score < [dict get $e Score]} {
			break
		}
		# TODO selection criteria
	}

	set best_loc [dict get $res $best_dir]
	foreach {x y z} $best_loc {}
	if {$y > 15} {
		set dir2 nw
	} else {
		set dir2 se
	}

	lappend ol [format {move %s %s} $best_dir $dir2]
	db eval {
		UPDATE units SET orders=$ol
		WHERE id=$unit_id
	}
}

# have leader do something
proc advanceLeader {u} {
	if {$u eq ""} return

	set ol [$u cget -orders]
	set il [$u cget -items]
	set sl [$u cget -skills]

	set silver [$u countItem SILV]

	# fire first
	set i [lsearch $sl *FIRE*]
	if {$i == -1} {
		# requires force
		set i [lsearch $sl *FORC*]
		if {$i == -1} {
			if {$silver < 100} {
				lappend ol "claim [expr {100 - $silver}]"
			}
			lappend ol "STUDY FORC"
		} else {
			if {$silver < 100} {
				lappend ol "claim [expr {100 - $silver}]"
			}
			lappend ol "STUDY FIRE"
			lappend ol "turn" "combat fire" "endturn"
		}
	} else {
		# got fire, go for tact
		if {$silver < 200} {
			lappend ol "claim [expr {200 - $silver}]"
		}
		lappend ol "STUDY TACT"
	}

	$u configure -orders $ol
}

proc doAdvance {skill u ldrs teacher} {
	set ldr_ids [list]
	set cost 100
	if {$skill == "TACT"} {
		set cost 200
	}

	foreach l $ldrs {
		if {$l == $teacher} {
			continue
		}

		lappend ldr_ids [$l cget -num]
		set silver [$l countItem SILV]
		set ol [list]
		if {$silver < $cost} {
			lappend ol "claim [expr {$cost - $silver}]"
		}
		lappend ol "STUDY $skill"
		addOrder $l $ol
	}

	if {$teacher == $u} {
		set teach_order [list "TEACH [join $ldr_ids " "]"]
		addOrder $u $teach_order
	} else {
		lappend ldr_ids [$u cget -num]
		set teach_order [list "TEACH [join $ldr_ids " "]"]

		set silver [$u countItem SILV]
		set ol [list]
		if {$silver < $cost} {
			lappend ol "claim [expr {$cost - $silver}]"
		}
		lappend ol "STUDY $skill"
		addOrder $u $ol

		addOrder $teacher $teach_order
	}
}

proc findTeacher {ldrs skill} {
	foreach l $ldrs {
		set sl [$l cget -skills]
		set i [lsearch $sl [format {*%s*} $skill]]
		if {$i != -1} {
			return $l
		}
	}
	return ""
}

proc advanceLeaders {u all_u} {
	# build leader list
	set ldrs [list]
	foreach ui $all_u {
		if {$ui eq $u} { continue }

		set il [$ui cget -items]
		set idx [lsearch -index 2 $il *LEAD*]
		set ct [lindex $il $idx 0]
		if {$ct == 1} {
			lappend ldrs $ui
		}
	}

	if {[llength $ldrs] == 0} {
		# old game format - one leader
		advanceLeader $u
	} else {
		# pull first leader skills
		set first [lindex $ldrs 0]
		set sl [$first cget -skills]

		# check for FIRE
		set i [lsearch $sl *FIRE*]
		if {$i == -1} {
			# requires force
			set i [lsearch $sl *FORC*]
			if {$i == -1} {
				doAdvance "FORC" $u $ldrs $u
			} else {
				doAdvance "FIRE" $u $ldrs $u
			}
		} else {
			# check for TACT
			# TODO: don't use faction leader as metric
			set fl_sl [$u cget -skills]
			set i [lsearch $fl_sl *TACT*]
			if {$i == -1} {
				set teacher [findTeacher $ldrs TACT]
				doAdvance "TACT" $u $ldrs $teacher
			}
		}
	}
}

proc processRegion {sitRep rid} {
	set units [getUnitObjects $rid]

	# pull region info
	set res [::db eval {
		SELECT x,y,z, wages, tax, entertainment, wants, sells, products, exitDirs
		FROM detail
		WHERE id=$rid
	}]
	foreach {x y z wages tax ente wants sells products exits} $res {}

	# get all the funds here
	set totalSilver 0
	set leader ""
	set taxers 0
	set couriers [list]
	set funds [list]
	foreach u $units {
		set silver [$u countItem SILV]
		if {$silver != 0} {
			incr totalSilver $silver
			lappend funds $u $silver
		}

		set items [$u cget -items]
		set ol [$u cget -orders]
		set sl [$u cget -skills]
		if {[regexp {FLEAD} $items] != 0} {
			# leader
			set leader $u
		} elseif {[ordersMatch $ol "tax"] != -1} {
			incr taxers [countMen $items]
		} elseif {[regexp {COMB} $sl] != 0} {
		}

		if {[$u cget -name] eq "Courier"} {
			lappend couriers $u
		}
	}

	# fund leader training
	if {$leader ne ""} {
		set s_need 200
		if {$totalSilver >= $s_need} {
			for {set i 0} {$s_need > 0 && $i < [llength $funds]} {incr i 2} {
				set u [lindex $funds $i]
				set s [lindex $funds $i+1]

				if {$s >= $s_need} {
					# goal achieved
					set s_give $s_need
					set s_left [expr {$s - $s_need}]
					set s_need 0
				} elseif {$s > 0} {
					set s_need [expr {$s_need - $s}]
					set s_give $s
					set s_left 0
				}

				# create the give order
				if {$s_give > 0} {
					set order_text "GIVE [$leader cget -num] $s_give SILV"

					set give_o [$u cget -orders]
					lappend give_o $order_text
					$u configure -orders $give_o

					# update giving unit
					$u setItem SILV $s_left
					set funds [lreplace $funds $i+1 $i+1 $s_left]
					incr totalSilver -$s_give

					# update leader
					set leader_funds [$leader countItem SILV]
					$leader setItem SILV [expr {$leader_funds + $s_give}]
				}
			}
		}

		advanceLeaders $leader $units
	}

	if {[llength $units] == [llength $couriers]} {
		if {[checkStay $rid $x $y $z]} {
			set new_dir [selectNewHex $sitRep $x $y $z]
			if {$new_dir eq ""} {
				return
			}

			foreach u $units {
				set unit_id [$u cget -db_id]
				set ol [$u cget -orders]
				lappend ol "MOVE $new_dir"
				db eval {
					UPDATE units SET orders=$ol
					WHERE id=$unit_id
				}
			}
			return
		}
	}

	# buy tax men
	## don't buy men in a keep out zone
	set keep_out [::db onecolumn { SELECT val FROM notes WHERE key="keep_out"}]
	if {[lsearch $keep_out [list $x $y $z]] != -1} {
		return
	}

	set ret [$sitRep buyGuards $totalSilver 0 $x $y $z $taxers]
	foreach {s_need form_orders rem} $ret {}

	if {$s_need > 0} {
		for {set i 0} {$s_need > 0 && $i < [llength $funds]} {incr i 2} {
			set u [lindex $funds $i]
			set s [lindex $funds $i+1]

			if {$s >= $s_need} {
				# goal achieved
				set s_give $s_need
				set s_left [expr {$s - $s_need}]
				set s_need 0
			} elseif {$s > 0} {
				set s_need [expr {$s_need - $s}]
				set s_give $s
				set s_left 0
			}

			# create the give order
			if {$s_give > 0} {
				set order_text "GIVE NEW 19 $s_give SILV"

				set give_o [$u cget -orders]
				lappend give_o $order_text
				$u configure -orders $give_o

				# update giving unit
				$u setItem SILV $s_left
				set funds [lreplace $funds $i+1 $i+1 $s_left]
				incr totalSilver -$s_give
			}
		}

		# have first unit do the form
		set form_unit [lindex $units 0]
		set ol [$form_unit cget -orders]
		lappend ol {*}$form_orders
		$form_unit configure -orders $ol
	}

	# consider expansion
	if {$rem == 0} {
		set courier_id ""
		set rdata [db eval {
			SELECT id, sells, race, tax
			FROM detail
			WHERE x=$x AND y=$y AND z=$z AND turn=$::currentTurn
			ORDER BY turn DESC LIMIT 1
		}]
		foreach {regionId sells peasants maxTax} $rdata {}

		set ret [getBuyRace $sells $peasants]
		foreach {maxRace raceList price} $ret {}
		if {![regexp {\[(.+)\]} [lindex $raceList 0] -> abbr]} {
			set abbr ""
		}

		set will_export 0
		set export_needs 0
		if {$abbr ne "" && [isAlignCompat ::db $abbr]} {
			set form_unit [lindex $units 0]
			set ol [$form_unit cget -orders]

			set import_regions [$sitRep cget -import_regions]
			for {set i 0} {$i < [llength $import_regions]} {incr i} {
				set ir [lindex $import_regions $i]
				set d [getDistance $x $y $z {*}$ir]
				if {$d == 1} {
					foreach {tx ty tz} $ir break
					set tax [db onecolumn {
						SELECT tax
						FROM detail
						WHERE x=$tx AND y=$ty AND z=$tz
						ORDER BY turn DESC LIMIT 1
					}]
					set taxers_needed [expr {$tax / 50}]
					set budget $totalSilver
					set maxBuy [expr {$budget / ($price + 10 + 10 + 10)}]
					set numBuy [expr {min($taxers_needed, $maxBuy, $maxRace)}]
					if {$numBuy == 0} {
						break
					}
					set will_export 1
					incr export_needs [expr {$numBuy * $price}]

					# calculate maintenance
					set item_desc [dGet [db onecolumn {SELECT desc FROM items WHERE abbr=$abbr}] Desc]
					if {[regexp {This race takes (.*) hits to kill} $item_desc -> hits]} {
						set maint [expr {$numBuy * $hits * 5}]
						# need 10 for study comb, and maintenance for this turn and next
						incr export_needs [expr {(10 + $maint * 2) * $numBuy}]
					}

					set dir [moveToward $x $y $z {*}$ir]
					set courier_id "NEW 30"
					lappend ol \
					 	 {FORM 30} \
					 	 {NAME UNIT "Guard"} \
					 	 "BUY $numBuy $abbr" \
					 	 {AVOID 0} \
					 	 {BEHIND 0} \
					 	 {NOAID 1} \
					 	 {STUDY COMB} \
					 	 {TURN} \
					 	 "MOVE $dir" \
					 	 {ENDTURN} \
					 	 {TURN} \
					 	 "@tax" \
					 	 {ENDTURN} \
					 	 {END}

					$form_unit configure -orders $ol

					set import_regions [lreplace $import_regions $i $i]
					$sitRep configure -import_regions $import_regions
					break
				}
			}
		}

		set new_dir ""
		if {!$will_export} {
			set new_dir [selectNewHex $sitRep $x $y $z]
		}

		if {$new_dir ne ""} {
			if {[llength $couriers] == 0} {
				if {$abbr ne "" && [isAlignCompat ::db $abbr]} {
					set courier_id "NEW 20"
					lappend ol \
						 {FORM 20} \
						 {NAME UNIT "Courier"} \
						 "BUY 1 $abbr" \
						 {AVOID 1} \
						 {BEHIND 1} \
						 {NOAID 1} \
						 "MOVE $new_dir" \
						 {END}

					$form_unit configure -orders $ol
				}
			} else {
				set u [lindex $couriers 0]
				set ol [$u cget -orders]
				lappend ol "MOVE $new_dir"
				$u configure -orders $ol
				set courier_id [$u cget -num]
			}
		}

		if {$courier_id ne ""} {
			for {set i 0} {$totalSilver > 0 && $i < [llength $funds]} {incr i 2} {
				set u [lindex $funds $i]
				set s [lindex $funds $i+1]

				if {[$u cget -num] eq $courier_id} {
					incr totalSilver -$s
					continue
				}

				if {$export_needs && $s >= $export_needs} {
					set s_give $export_needs
					set s_left [expr {$s - $export_needs}]
				} else {
					set s_give $s
					set s_left 0
				}

				# create the give order
				if {$s_give > 0} {
					set order_text "GIVE $courier_id $s_give SILV"

					set give_o [$u cget -orders]
					lappend give_o $order_text
					$u configure -orders $give_o

					# update giving unit
					$u setItem SILV $s_left
					incr totalSilver -$s_give
				}

				if {$export_needs} {
					if {$s >= $export_needs} {
						break
					}
					incr export_needs -$s_give
				}
			}
		}
	}

	# save out orders
	foreach u $units {
		set ol [$u cget -orders]
		set unit_id [$u cget -db_id]

		db eval {
			UPDATE units SET orders=$ol
			WHERE id=$unit_id
		}
	}
}

itcl::body SitRep::createOrders {} {
	# pull all regions where we have units
	set res [::db eval {
		SELECT detail.x, detail.y, detail.z, detail.id
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn AND units.detail='own'
		GROUP BY detail.z, detail.x, detail.y
	}]

	foreach {x y z rid} $res {
		set regions($x,$y,$z) $rid
	}

	set units $unit_state
	if {$overall_state == "start"} {
		# in nexus
		set zlevel [lindex $units 2]
		if {$zlevel == 0} {
			# we're in the nexus
			# choose an exit
			pickStartDirection $rid $units
			return
		}
		#else, exited nexus
		rampFirstHex $this $units
		return
	}
	#else post-start

	# foreach region
	set tax_regions [list]
	foreach {x y z rid} $res {
		processRegion $this $rid
		set ct [curTax $rid 1e6]
		if {$ct > 0} {
			lappend tax_regions [list $x $y $z]
		}
	}

	# shuffle couriers
	set res [::db eval {
		SELECT detail.x, detail.y, detail.z, units.id, units.orders
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn and units.detail='own' and units.name LIKE "Courier%"
	}]

	## find distance from each courier to each tax region
	set couriers [list]
	set courier_dists [list]
	set cmin_dists [list]

	foreach {x y z uid ol} $res {
		set u [Unit #auto Id $uid Orders $ol Region [list $x $y $z]]
		$u filterInstantOrders
		if {[string match -nocase {*move*} [$u cget -orders]]} {
			continue
		}
		if {[llength $couriers] >= [llength $tax_regions]} {
			# TODO trim far couriers (convert to scouts?)
			break
		}

		lappend couriers $u

		# calc distance to each taxing region
		set cdist [list]
		set min_dist 1000000
		foreach tr $tax_regions {
			set d [getDistance $x $y $z {*}$tr]
			lappend cdist $d
			set min_dist [expr {min($d, $min_dist)}]
		}
		lappend cmin_dists $min_dist
		lappend courier_dists $cdist
	}

	while {[llength $couriers]} {
		# move the furthest courier towards his closest region
		set furthest_idx [lindex [lsort -integer -decreasing -indices $cmin_dists] 0]
		set dist [lindex $cmin_dists $furthest_idx]
		set dists [lindex $courier_dists $furthest_idx]
		set city_idx [lsearch -integer $dists $dist]
		set loc [lindex $tax_regions $city_idx]

		set u [lindex $couriers $furthest_idx]
		$u moveTo {*}$loc

		set unit_id [$u cget -db_id]
		set ol [$u cget -orig_orders]

		db eval {
			UPDATE units SET orders=$ol
			WHERE id=$unit_id
		}

		set couriers [lreplace $couriers $furthest_idx $furthest_idx]
		set courier_dists [lreplace $courier_dists $furthest_idx $furthest_idx]
		set cmin_dists [lreplace $cmin_dists $furthest_idx $furthest_idx]
	}
}

proc processData {} {
	db eval {SELECT type, val FROM events} {
		if {$type eq "REWARD" || $type eq "BATTLE"} {
			continue
		}
		if {$type eq "ERROR"} {
			# what to do?
			continue
		}
		if {$type ne "EVENT"} {
			puts "New event type $type"
			continue
		}
		set sub [dGet $val SUB]
		if {$sub eq "FORBID"} {
			set loc [dict get $val LOC]
			set keep_out [::db onecolumn {SELECT val FROM notes WHERE key="keep_out"}]
			if {[lsearch $keep_out $loc] == -1} {
				lappend keep_out $loc
				::db eval {INSERT OR REPLACE INTO notes VALUES("keep_out", $keep_out)}
			}
		}
	}
}

# main
if {![info exists debug]} {
	if {$argc < 2} {
		puts "Usage $argv0 <command> <dir>"
		puts "Commands: "
		foreach c {new add gen} {
			puts "\t$c"
		}
		exit
	}

	set cmd [lindex $argv 0]
	cd [lindex $argv 1]

	if {$cmd eq "new"} {
		if {[llength $argv] % 2 == 1} {
			puts "Usage $argv0 new <dir> \[key value\]*"
			exit 1
		}
		createDb "game.db"

		foreach {k v} [lrange $argv 2 end] {
			if {$k eq "-f"} {
				set f [open $v]
				set l [gets $f]
				while {![eof $f]} {
					set k1 [lindex $l 0]
					set v1 [lindex $l 1]
					if {$k1 eq ""} {
						break
					}
					if {[llength $l] != 2} {
						puts "Bad key/value '$k1' '$v1'"
						exit
					}

					::db eval {
						INSERT OR REPLACE INTO notes
						VALUES($k1, $v1)
					}

					set l [gets $f]
				}
			} else {
				::db eval {
					INSERT OR REPLACE INTO notes
					VALUES($k, $v)
				}
			}
		}
		exit 0
	}

	#else open game db
	set errMsg [openDb [glob "*.db"]]
	if {$errMsg ne ""} {
		puts $errMsg
		exit 1
	}

	if {$cmd eq "add"} {
		loadData [lindex $argv 2]
		processData
		exit 0
	}

	if {$cmd ne "gen"} {
		puts "Unkwown command '$cmd"
		exit 1
	}

	# generate orders for current turn
	set sitRep [SitRep #auto]
	$sitRep createOrders

	$sitRep saveState

	writeOrders [format {orders.%d} [expr {$::currentTurn + 1}]]
}


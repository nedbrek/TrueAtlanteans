lappend ::auto_path [pwd]
package require client_utils

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
		foreach {x y z id name uid items orders} $units {
			if {$z != 0} {
				set in_nexus 0
			}
		}

		if {$in_nexus} {
			dict set ret "State" "start"
		} else {
			dict set ret "State" "main"
		}

		return $ret
	}

	method createOrders {}
	method buyGuards {budget claim x y z taxers}
}

###
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
	if {$x < $::max_x / 2 - 2} {
		# north pole
		set dirs {s sw se nw ne n}
	} elseif {$x > $::max_x / 2 +2} {
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

	set taxersNeeded [expr {$maxTax / 50}]
	if {$taxersNeeded == 0} {
		puts "No tax in region!"
		exit -1
	}
	if {$taxersNeeded <= $taxers} {
		return [list "" "" 0]
	}
	incr taxersNeeded -$taxers

	set ret [getBuyRace $sells $peasants]
	foreach {maxRace raceList price} $ret {}

	regexp {\[(.+)\]} [lindex $raceList 0] -> abbr

	set alignment [db onecolumn {SELECT val FROM notes WHERE key="alignment"}]
	set race_align [db onecolumn {SELECT val FROM notes WHERE key="race_align"}]
	if {$race_align eq ""} {
		set ra "neutral"
	} else {
		set ra [dGet $race_align $abbr]
		if {$ra eq ""} {
			puts "No alignment for '$abbr'"
		}
	}

	if {$alignment ne "" && $alignment ne "neutral"} {
		if {$alignment ne $ra} {
			# note region for import of guards matching alignment
			lappend import_regions [list $x $y $z]
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

	lappend ol "form 1" "name unit Guard"

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
	lappend ol "end"

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
		lappend keep_out [list $x $y $z]
		::db eval { INSERT OR REPLACE INTO notes VALUES("keep_out", $keep_out)}
		return 1
	}
	return 0
}

proc rampFirstHex {sitRep units} {
	foreach {x y z unit_id name unum il ol} $units {}
	set rid [::db onecolumn {SELECT id FROM detail WHERE x=$x AND y=$y AND z=$z AND turn=$::currentTurn}]
	set all_u [getUnitObjects $rid]
	set u [lindex $all_u 0]; # TODO fix me

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
			}
		}

		set stay 0; # probably bad
		set near_jumps [expr {$num_jumps > $max_jumps - 2}]

		# amazing!
		if {$ct_plain > 4 || ($ct_plain > 0 && $ct_forest + $ct_mountain > 2)} {
			set stay 1
		} elseif {$ct_plain + $ct_forest + $ct_mountain > 1} {
			# take anything ok
			set stay 1
		} elseif {$near_jumps && $ct_plain + $ct_forest + $ct_mountain > 0} {
			# take anything not terrible
			set stay 1
		} elseif {$num_jumps >= $max_jumps} {
			# out of time
			set stay 1
		}

		if {!$stay} {
			advanceLeader $u $all_u
			set ol [$u cget -orders]

			# TODO drop scout

			lappend ol "CAST GATE RANDOM"
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
			lappend ol "MOVE $new_dir"
			db eval {
				UPDATE units SET orders=$ol
				WHERE id=$unit_id
			}
		}
		return
	}

	advanceLeader $u $all_u
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
					lappend ol "END"
					lappend ol "GIVE NEW $ni 1 LEAD"
				}
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
		lappend ol $gate_order

		$u configure -orders $ol

		# you can study and jump
		advanceLeader $u $all_u

		# save orders
		foreach ui $all_u {
			set ol [$ui cget -orders]

			lappend ol "behind 1" "avoid 1"

			set unit_id [$ui cget -db_id]
			db eval {
				UPDATE units SET orders=$ol
				WHERE id=$unit_id
			}
		}
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
proc advanceLeader {u all_u} {
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
		if {[regexp {PATT} $sl] != 0} {
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

		advanceLeader $leader $units
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
				set order_text "GIVE NEW 1 $s_give SILV"

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
		regexp {\[(.+)\]} [lindex $raceList 0] -> abbr

		set form_unit [lindex $units 0]
		set ol [$form_unit cget -orders]

		set will_export 0
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

				set dir [moveToward $x $y $z {*}$ir]
				set courier_id "NEW 30"
				lappend ol \
					 {FORM 30} \
					 {NAME UNIT "Guard"} \
					 "BUY $numBuy $abbr" \
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

		set new_dir ""
		if {!$will_export} {
			set new_dir [selectNewHex $sitRep $x $y $z]
		}

		if {$new_dir ne ""} {
			if {[llength $couriers] == 0} {
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

				set s_give $s
				set s_left 0

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

# main
if {![info exists debug]} {
	if {$argc < 2} {
		puts "Usage $argv0 <command> <dir>"
		puts "Commands: "
		foreach c {new add gen} {
			puts "\t$c"
		}
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


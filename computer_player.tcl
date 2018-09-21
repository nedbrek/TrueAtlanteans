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

	constructor {} {
		set ret [evaluateSituation]
		set overall_state [dGet $ret State]
		set unit_state [dGet $ret Units]
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
		} elseif {[llength $units] == 8} {
			dict set ret "State" "start"
		} else {
			dict set ret "State" "main"
		}

		return $ret
	}
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

proc selectNewHex {x y z} {
	set start_cities [::db eval {
		SELECT dest
		FROM nexus_exits
	}]

	# TODO move strategy
	set dirs {n nw ne sw se s}
	set d_vals [list]

	foreach d $dirs {
		set loc [moveCoord $x $y $d]
		set nx [lindex $loc 0]
		set ny [lindex $loc 1]
		lappend loc $z

		# don't go back to a start city
		if {[lsearch $start_cities $loc] != -1} {
			lappend d_vals 0
			continue
		}

		# TODO don't go to a hex we're already in

		set terrain [::db eval {
			SELECT type
			FROM terrain
			WHERE x=$nx AND y=$ny AND z=$z
		}]
		if {$terrain eq "ocean"} {
			lappend d_vals 0
			continue
		}

		# degrade moving to coast
		if {[isCoast $nx $ny $z]} {
			lappend d_vals 1
			continue
		}

		if {$terrain eq "plains"} {
			lappend d_vals 10
		} else {
			lappend d_vals 2
		}
	}

	set best_d [lindex [lsort -decreasing -indices $d_vals] 0]
	if {[lindex $d_vals $best_d] == 0} {
		return ""
	}
	return [lindex $dirs $best_d]
}

proc buyGuards {budget claim x y z taxers} {
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

	regexp {\[(.+)\]} [lindex $raceList 0] -> abbr

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

proc rampFirstHex {units} {
	foreach {x y z unit_id name uid il ol} $units {}

	# TODO check to make sure we got out of the starting city (exit wasn't blocked)
	# TODO calculate a good budget to use
	set budget 2600

	set ret [buyGuards $budget 1 $x $y $z 0]
	foreach {s_need form_orders rem} $ret {}

	set ol [concat $ol $form_orders]
	lappend ol "claim 100"
	lappend ol "study FORC"

	db eval {
		UPDATE units SET orders=$ol
		WHERE id=$unit_id
	}
}

proc pickStartDirection {units} {
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

	foreach {x y z unit_id name uid il ol} $units {
		lappend ol "behind 1" "avoid 1"
		lappend ol [format {move %s %s} $best_dir $dir2]
		db eval {
			UPDATE units SET orders=$ol
			WHERE id=$unit_id
		}
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

proc processRegion {rid} {
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

		advanceLeader $leader
	}

	# buy tax men
	set ret [buyGuards $totalSilver 0 $x $y $z $taxers]
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
		set new_dir [selectNewHex $x $y $z]

		if {$new_dir ne ""} {
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

			for {set i 0} {$totalSilver > 0 && $i < [llength $funds]} {incr i 2} {
				set u [lindex $funds $i]
				set s [lindex $funds $i+1]

				set s_give $s
				set s_left 0

				# create the give order
				if {$s_give > 0} {
					set order_text "GIVE NEW 20 $s_give SILV"

					set give_o [$u cget -orders]
					lappend give_o $order_text
					$u configure -orders $give_o

					# update giving unit
					$u setItem SILV $s_left
					set funds [lreplace $funds $i+1 $i+1 $s_left]
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

proc createOrders {sitRep} {
	set units [$sitRep cget -unit_state]
	if {[$sitRep cget -overall_state] == "start"} {
		# only one unit
		set zlevel [lindex $units 2]
		if {$zlevel == 0} {
			# we're in the nexus
			# choose an exit
			pickStartDirection $units
			return
		}
		#else, exited nexus
		rampFirstHex $units
		return
	}
	#else post-start

	# pull all regions where we have units
	set res [::db eval {
		SELECT detail.x, detail.y, detail.z, detail.id
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn AND units.detail='own'
		GROUP BY detail.z, detail.x, detail.y
	}]

	# foreach region
	set tax_regions [list]
	foreach {x y z rid} $res {
		processRegion $rid
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
		lappend couriers $u

		# calc distance to each taxing region
		set cdist [list]
		set min_dist 1e6
		foreach tr $tax_regions {
			set d [getDistance $x $y $z {*}$tr]
			lappend cdist $d
			set min_dist [expr {min($d, $min_dist)}]
		}
		lappend cmin_dists $min_dist
		lappend courier_dists $cdist
	}

	if {[llength $couriers] > [llength $tax_regions]} {
		# TODO trim far coutiers (convert to scouts?)
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
		createDb "game.db"
		exit 0
	}

	#else open game db
	set errMsg [openDb "game.db"]
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
	createOrders $sitRep

	writeOrders [format {orders.%d} [expr {$::currentTurn + 1}]]
}


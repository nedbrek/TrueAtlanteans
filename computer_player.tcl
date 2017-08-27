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

proc evaluateSituation {} {
	set ret [dict create]

	set units [db eval {
		SELECT detail.x, detail.y, detail.z, units.id, units.name, units.uid, units.items, units.orders
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn and units.detail='own'
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

proc buyGuards {budget x y z} {
	set rdata [db eval {
		SELECT id, sells, race, tax
		FROM detail
		WHERE x=$x AND y=$y AND z=$z AND turn=$gui::currentTurn
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

	set ret [getBuyRace $sells $peasants]
	foreach {maxRace raceList price} $ret {}

	# limit by cash on hand
	# TODO configure maintenance cost
	set maxBuy [expr {$budget / ($price + 20)}]
	set numBuy [expr {min($taxersNeeded, $maxBuy, $maxRace)}]
	set claimAmt [expr {$numBuy * ($price + 10)}]

	regexp {\[(.+)\]} [lindex $raceList 0] -> abbr

	lappend ol "form 1" "name unit Guard" "claim $claimAmt"
	lappend ol "avoid 0" "behind 0"
	lappend ol "buy $numBuy $abbr"
	lappend ol "study COMB"
	lappend ol "end"

	return $ol
}

proc rampFirstHex {units} {
	foreach {x y z unit_id name uid il ol} $units {}

	# TODO check to make sure we got out of the starting city (exit wasn't blocked)
	# TODO calculate a good budget to use
	set budget 3000

	set ret [buyGuards $budget $x $y $z]

	set ol [concat $ol $ret]
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

proc processRegion {ul rid} {
	set res [::db eval {
		SELECT x,y,z, wages, tax, entertainment, wants, sells, products, exitDirs
		FROM detail
		WHERE id=$rid
	}]
	puts "Process $ul $rid $res"
}

proc createOrders {sitRep} {
	set units [dGet $sitRep Units]
	if {[dict get $sitRep State] == "start"} {
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
	# process per region
	set res [::db eval {
		SELECT units.id, units.items, units.orders, detail.x, detail.y, detail.z, detail.id
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn AND units.detail='own'
		ORDER BY detail.z, detail.x, detail.y
	}]

	set loc ""
	set old_rid 0
	set ul [list]
	foreach {uid il ol x y z rid} $res {
		set u [dict create UID $uid IL $il OL $ol]

		set newLoc [list $x $y $z]
		if {$loc eq ""} {
			set loc $newLoc
			set old_rid $rid
			# start list
			set ul [list $u]
		} elseif {$loc ne $newLoc} {

			# location change - process current list with old rid
			processRegion $ul $old_rid

			# create new list
			set ul [list $u]

			set loc $newLoc
			set old_rid $rid
		} else {
			# append to list
			lappend ul $u
		}
	}

	processRegion $ul $old_rid
}

proc saveOrders {} {
	set filename [format {orders.%d} $gui::currentTurn]

	set f [open $filename "w"]

	set pid [::db onecolumn { SELECT player_id FROM settings }]
	set ppass [::db onecolumn { SELECT player_pass FROM settings }]
	if {$ppass eq ""} {
		puts $f "#atlantis $pid"
	} else {
		puts $f "#atlantis $pid \"$ppass\""
	}

	set res [::db eval {
		SELECT units.name, units.uid, units.orders, detail.x, detail.y, detail.z
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn AND units.detail='own'
		ORDER BY detail.z, detail.x, detail.y
	}]

	set loc ""
	foreach {u uid ol x y z} $res {
		if {$ol eq ""} continue

		set newLoc [list $x $y $z]
		if {$loc eq "" || $loc ne $newLoc} {
			set loc $newLoc
			puts $f ";*** $x $y $z ***"
		}

		puts $f "unit $uid"
		puts $f "; $u"
		puts $f "[join $ol "\n"]\n"
	}

	puts $f "#end"
	close $f
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
	puts "cd [lindex $argv 1]"
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
	set ::men [db eval {select abbr from items where type="race"}]
	set gui::currentTurn [db eval {select max(turn) from detail}]

	set sitRep [evaluateSituation]
	createOrders $sitRep
	saveOrders
}


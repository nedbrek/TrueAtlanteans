package require atlantis_dbtools
package require atlantis_reader
package require Itcl
package provide client_utils 1.0

# can be stored in db, or calculated from terrain
set max_x 32
set max_y 32

proc checkBool {s} {
	return [expr {$s != 0 && $s != 1}]
}

proc cleanOrder {o} {
	# strip whitespace
	set o [string trim $o]

	if {[string index $o 0] eq "@"} {
		# repeat
		set o [string trimleft $o "@"]

		if {[string is digit [string index $o 0]]} {
			# @n <order>
			set o [regsub {[[:digit:]]+} $o ""]
		} else {
			# it was a simple repeat
		}

		set o [string trim $o]
	}

	return $o
}

### classes
itcl::class Unit {
	public variable db_id
	public variable name; # not including "(num)"
	public variable num; # just the num
	public variable items; # list of items
	public variable orders
	public variable orig_orders
	public variable skills
	public variable flags
	public variable region
	public variable object
	public variable parent

	constructor {args} {
		set db_id  [dGet $args Id]
		set name   [dGet $args Name]
		set num    [dGet $args Num]
		set items  [dGet $args Items]
		set orders [dGet $args Orders]
		set skills [dGet $args Skills]
		set flags  [dGet $args Flags]
		set region [dGet $args Region]
		set object [dGet $args Object]
		set parent [dGet $args Parent]

		set orig_orders $orders

		if {$name eq ""} {
			if {[lindex $num 0] eq "new"} {
				set name $num
			} else {
				set name "Unit $num"
			}
		}
	}

	method print {} {
		puts "$name ($num) '$flags' '$skills' '$orders' '$items'"
	}

	# filter instant orders, including "form <id>"/end and "turn/endturn"
	# return new units (created by form)
	method filterInstantOrders {}

	method moveTo {x y z} {
		foreach {cur_x cur_y cur_z} $region {}
		if {$cur_z != $z} {
			puts "Can't navigate between levels"
			return
		}
		if {$x == $cur_x && $y == $cur_y} {
			return
		}
		# TODO handle weight and weather
		if {$cur_y < $y} {
			set d "s"
		} else {
			set d "n"
		}
		if {$cur_x < $x} {
			append d "e"
		} elseif {$cur_x > $x} {
			append d "w"
		}
		# TODO decide when to use wrap
		# TODO move multiple
		set o "MOVE $d"
		lappend orders $o
		lappend orig_orders $o
	}

	method countItem {abbr} {
		return [::countItem $items $abbr]
	}

	method setItem {abbr ct} {
		set full_abbr [format {[%s]} $abbr]
		for {set i 0} {$i < [llength $items]} {incr i} {
			set il [lindex $items $i]
			if {[lindex $il 2] eq $full_abbr} {
				set il [lreplace $il 0 0 $ct]
				set items [lreplace $items $i $i $il]
				return
			}
		}
		set name [db onecolumn {SELECT name FROM items WHERE abbr=$abbr}]
		lappend items [list $ct $name $full_abbr]
	}
}

itcl::body Unit::filterInstantOrders {} {
	set bool_flags {
		avoid
		hold
		behind
		noaid
		guard
		nocross
		autotax
		share
	}
	set enum_flags {
		consume
		reveal
	}
	set enum_flag_vals {
		""
		unit
		faction
	}

	foreach {x y z} $region {}

	set new_units [list]

	set new_orders [list]
	set skip 0
	for {set i 0} {$i < [llength $orders]} {incr i} {
		set o [cleanOrder [lindex $orders $i]]
		set cmd [string tolower [lindex $o 0]]
		set upper_cmd [string toupper [lindex $o 0]]

		if {$skip} {
			if {$cmd eq "endturn"} {
				set skip 0
			}
			continue
		}

		if {$cmd eq "turn"} {
			set skip 1
			continue
		}

		# handle bool flags
		if {[lsearch $bool_flags $cmd] != -1} {
			set arg [lindex $o 1]
			# apply flags
			if {$arg == 1} {
				# set flag
				if {[dict exists $flags $upper_cmd]} {
					# already set
				} else {
					lappend flags $upper_cmd 1
				}
			} elseif {$arg == 0} {
				# remove flag
				set ki [lsearch $flags $upper_cmd]
				if {$ki != -1} {
					# set - unset it
					set flags [lreplace $flags $ki $ki+1]
				} else {
					# not set - nothing to do
				}
			} else {
				puts "$name ($x, $y, $z) $cmd bad argument $arg"
			}

			continue
		}

		# enum flags
		if {[lsearch $enum_flags $cmd] != -1} {
			set arg [lindex $o 1]
			if {[lsearch -nocase $enum_flag_vals $arg] == -1} {
				puts "$name ($x, $y, $z) $cmd bad argument $arg"
			}
			# TODO: apply flags
			continue
		}

		# TODO spoils
		if {$cmd eq "spoils"} {
			continue
		}

		# anything but form
		if {![regexp -nocase {^form +(.*)} $o -> new_id]} {
			lappend new_orders $o
			continue
		}

		set new_o [list]
		for {incr i} {$i < [llength $orders]} {incr i} {
			set o [lindex $orders $i]
			if {[regexp -nocase { *end *$} $o]} {
				break
			}
			lappend new_o $o
		}

		set new_unit [itcl::code [Unit #auto Num "new $new_id" Orders $new_o \
		    Flags $flags Region $region Object $object Parent $this]]
		lappend new_units $new_unit

		set tmp [{*}$new_unit filterInstantOrders]
		if {$tmp ne ""} {
			puts "Error: nested FORM will not work [{*}$u cget -name] ($x, $y, $z)"
			itcl:delete object $tmp
		}
	}

	set orders $new_orders
	return $new_units
}

### other functions
proc getLevelXscale {z} {
	# TODO figure out for particular game
	set scales { 1 1 1 2 }
	return [lindex $scales $z]
}

proc getLevelYscale {z} {
	# TODO figure out for particular game
	set scales { 1 1 2 4 }
	return [lindex $scales $z]
}

proc getDistance {x1 y1 z1 x2 y2 z2 {penalty 4}} {
	set one_x [expr $x1 * [getLevelXscale $z1]]
	set one_y [expr $y1 * [getLevelYscale $z1]]

	set two_x [expr $x2 * [getLevelXscale $z2]]
	set two_y [expr $y2 * [getLevelYscale $z2]]

	set maxy [expr {abs($one_y - $two_y)}]
	set maxx [expr {abs($one_x - $two_x)}]

	set max2 [expr {abs($one_x + $::max_x - $two_x)}]
	set maxx [expr {min($maxx, $max2)}]

	set max2 [expr {abs($one_x - ($two_x + $::max_x))}]
	set maxx [expr {min($maxx, $max2)}]

	if {$maxy > $maxx} {
		set maxx [expr {($maxx + $maxy) / 2}]
	}

	if {$z1 != $z2} {
		set zdist [expr {abs($z1 -$z2)}]
		incr maxx [expr {$penalty * $zdist}]
	}

	return $maxx
}

proc moveToward {cur_x cur_y cur_z x y z} {
	if {$cur_z != $z} {
		puts "Can't navigate between levels"
		return
	}
	if {$x == $cur_x && $y == $cur_y} {
		return
	}
	# TODO handle weight and weather
	if {$cur_y < $y} {
		set d "s"
	} else {
		set d "n"
	}
	if {$cur_x < $x} {
		append d "e"
	} elseif {$cur_x > $x} {
		append d "w"
	}
	# TODO decide when to use wrap
	# TODO move multiple
	return $d
}

proc moveCoord {x y dir} {
	switch $dir {
		n { incr y -2 }
		s { incr y  2 }

		nw { incr x -1; incr y -1 }
		ne { incr x  1; incr y -1 }

		se { incr x 1; incr y 1 }
		sw { incr x -1; incr y 1 }

		w {
			incr x -1

			if {$x & 1} {
				incr y -1
			} else {
				incr y 1
			}
		}

		e {
			incr x

			if {$x & 1} {
				incr y -1
			} else {
				incr y 1
			}
		}
	}

	# wrap around
	set maxX [::db eval { SELECT max(cast(x as integer)) FROM terrain }]
	if {$x < 0} { set x $maxX }
	if {$x > $maxX} { set x 0 }

	return [list $x $y]
}

proc parseMan {t} {
	set ret [dict create]

	set i [lsearch $t "*This race may study *"]
	set skill_spec [lindex $t $i]

	if {[regexp { *This race may study all skills to level ([[:digit:]]+)} $skill_spec -> all_level]} {
		dict set ret ALL $all_level
	} elseif {[regexp { *This race may study all non-magic skills to level ([[:digit:]]+)} $skill_spec -> all_level]} {
		dict set ret ALL $all_level
	} else {
		regexp { *This race may study (.*) to level (.*) and all other.* to level ([^ ]*)} $skill_spec -> specs spec_level other_level
		dict set ret SPEC_LVL $spec_level
		dict set ret OTH_LVL $other_level

		foreach s [split $specs ","] {
			regexp {.* \[(.*)\]} $s -> skill_name
			dict lappend ret SPEC $skill_name
		}
		# TODO magic level is next list index
	}

	dict set ret DESC [lreplace $t $i $i]

	return $ret
}

proc loadData {filename} {
	set tfile [open $filename]

	updateDb db [reader::parseFile $tfile]
	close $tfile
}

proc writeOrders {fname} {
	set f [open $fname "w"]

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
		WHERE detail.turn=$::currentTurn AND units.detail='own'
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

proc getBuyRace {sells peasants} {
	set maxRace 0
	set raceList [list]
	set price 0

	foreach {saleItem cost} $sells {
		set race [string trim [lindex $saleItem end] {[]}]

		if {[lsearch $::men $race] == -1} {
			set fullRace [lrange $saleItem 1 end-1]
			if {$peasants ne $fullRace} {
				continue
			}
			lappend ::men $race
		}
		set ct [lindex $saleItem 0]

		lappend raceList [join [lrange $saleItem 1 end]]
		set maxRace [expr {max($maxRace, $ct)}]
		set price $cost
	}

	return [list $maxRace $raceList $price]
}

proc getUnitObjects {id} {
	set res [::db eval {
		SELECT x, y, z
		FROM detail
		WHERE id=$id
	}]
	foreach {x y z} $res {}
	set region "$x $y $z"

	set ret [list]
	set units [::db eval {
		SELECT id, name, uid, orders, items, skills, flags
		FROM units
		WHERE units.regionId=$id AND units.detail='own'
	}]
	foreach {id name num orders items skills flags} $units {
		lappend ret [Unit #auto Id $id \
		    Name $name Num $num Items $items Orders $orders Skills $skills Flags $flags Region $region Object ""
		]
	}

	return $ret
}

# output a list like in the original report
proc outputList {f lvals} {
	if {$lvals eq ""} {
		puts $f " none."
	} else {
		puts -nonewline $f " [lindex $lvals 0]"
		foreach v [lrange $lvals 1 end] {
			puts -nonewline $f ", $v"
		}
		puts $f "."
	}
}

proc writeMap {fname} {
	set data [db eval {
	    SELECT x, y, z, type, city, region, wages, pop, race, tax, entertainment, products, sells, wants, detail.exitDirs
	    FROM terrain join detail
	    USING(x,y,z)
	    GROUP BY terrain.x, terrain.y, terrain.z
	    ORDER BY detail.turn
	}]

	set f [open $fname "w"]
	foreach {x y z type city region wages pop race tax entertainment products sells wants exits} $data {
		puts -nonewline $f "$type ($x,$y"
		if {$z > 1} {
			puts -nonewline $f ",$z"
		}
		puts -nonewline $f ") in $region"
		if {$city ne ""} {
			puts -nonewline $f ", contains [lindex $city 0] \[[lindex $city 1]\]"
		}
		if {$pop ne ""} {
			puts -nonewline $f ", $pop peasants ($race), \$$tax"
		}
		puts $f "."

		puts $f "------------------------------------------------------------"
		set wg [lindex $wages 0]
		set mwg [lindex $wages 1]
		if {$wg eq ""} { set wg 0 }
		if {$mwg eq ""} { set mwg 0 }
		puts $f "  Wages: \$$wg (Max: \$$mwg)."

		puts -nonewline $f "  Wanted:"
		outputList $f $wants

		puts -nonewline $f "  For Sale:"
		outputList $f $sells

		if {$entertainment eq "none"} { set entertainment 0 }
		puts $f "  Entertainment available: \$$entertainment."

		puts -nonewline $f "  Products:"
		outputList $f $products

		puts $f ""
		set dir_map {
			North n
			Northwest nw
			Northeast ne
			South s
			Southwest sw
			Southeast se
		}
		puts $f "Exits:"
		foreach e $exits {
			set new_loc [moveCoord $x $y [dict get $dir_map $e]]
			foreach {x2 y2} $new_loc {}
			set d2 [db eval {SELECT type, region, city FROM terrain WHERE x=$x2 AND y=$y2 AND z=$z}]
			foreach {t2 r2 c2} $d2 {}

			puts -nonewline $f "  $e: $t2 ($x2,$y2"
			if {$z > 1} {
				puts -nonewline $f ",$z"
			}
			puts -nonewline $f ") in $r2"
			if {$c2 ne ""} {
				puts -nonewline $f ", contains [lindex $c2 0] \[[lindex $c2 1]\]"
			}
			puts $f "."
		}
		if {[llength $exits] == 0} {
			puts $f "  none"
		}

		puts $f ""
		puts $f ""
		puts $f ""
	}
	close $f
}


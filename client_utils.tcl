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
			if {[checkBool $arg]} {
				puts "$name ($x, $y, $z) $cmd bad argument $arg"
			}
			# TODO: apply flags
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
		if {![regexp {^form +(.*)} $o -> new_id]} {
			lappend new_orders $o
			continue
		}

		set new_o [list]
		for {incr i} {$i < [llength $orders]} {incr i} {
			set o [lindex $orders $i]
			if {[regexp { *end *$} $o]} {
				break
			}
			lappend new_o $o
		}

		set new_unit [itcl::code [Unit #auto Num "new $new_id" Orders $new_o Flags $flags Region $region Object $object]]
		lappend new_units $new_unit

		set tmp [$new_unit filterInstantOrders]
		if {$tmp ne ""} {
			puts "Error: nested FORM will not work [$u cget -name] ($x, $y, $z)"
			itcl:delete object $tmp
		}
	}

	set orders $new_orders
	return $new_units
}

### other functions
proc getDistance {x1 y1 z1 x2 y2 z2} {
	set maxy [expr {abs($y1 - $y2)}]
	set maxx [expr {abs($x1 - $x2)}]

	set max2 [expr {abs($x1 + $::max_x - $x2)}]
	set maxx [expr {min($maxx, $max2)}]

	set max2 [expr {abs($x1 - ($x2 + $::max_x))}]
	set maxx [expr {min($maxx, $max2)}]

	if {$maxy > $maxx} {
		return [expr {($maxx + $maxy) / 2}]
	}

	return $maxx
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
	if {$x == -1} { set x $maxX }
	if {$x > $maxX} { set x 0 }

	return [list $x $y]
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


package require atlantis_dbtools
package require atlantis_reader
package require Itcl
package provide client_utils 1.0

### classes
itcl::class Unit {
	public variable db_id
	public variable name; # not including "(num)"
	public variable num; # just the num
	public variable items; # list of items
	public variable orders
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

proc loadData {filename} {
	set tfile [open $filename]

	updateDb db [reader::parseFile $tfile]
	close $tfile
}

proc getBuyRace {sells peasants} {
	set maxRace 0
	set raceList [list]
	set price 0

	foreach {saleItem cost} $sells {
		set race [string trim [lindex $saleItem end] {[]}]

		if {[lsearch $::men $race] == -1} {
			set fullRace [lrange $saleItem 1 end-1]
			if {[lsearch $peasants $fullRace] == -1} {
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


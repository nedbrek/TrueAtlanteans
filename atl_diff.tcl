#!/usr/bin/env tclsh
lappend ::auto_path [file dirname $argv0]
package require client_utils

if {$argc < 2} {
	puts "Usage $argv0 <file1> <file2>"
	exit 1
}
set match 1

proc dGet {d k} {
	if {[dict exists $d $k]} {
		return [dict get $d $k]
	}
	return ""
}

proc findRegionIdx {regions2 loc} {
	for {set i 0} {$i < [llength $regions2]} {incr i} {
		set r [lindex $regions2 $i]
		set l [dict get $r Location]
		if {$l eq $loc} {
			return $i
		}
	}
	return -1
}

proc findUnitIdx {units2 id} {
	for {set i 0} {$i < [llength $units2]} {incr i} {
		set u [lindex $units2 $i]

		set name2 [dict get $u Name]
		set id2 [lindex [extractUnitNameNum $name2] 1]
		if {$id == $id2} {
			return $i
		}
	}
	return -1
}

proc findItemIdx {items2 abbr} {
	for {set i 0} {$i < [llength $items2]} {incr i} {
		set i2 [lindex $items2 $i]
		set abbr2 [lindex $i2 2]
		if {$abbr eq $abbr2} {
			return $i
		}
	}
	return -1
}

proc findObjectIdx {objects2 name} {
	for {set i 0} {$i < [llength $objects2]} {incr i} {
		set o2 [lindex $objects2 $i]
		set name2 [dict get $o2 Name]
		if {$name eq $name2} {
			return $i
		}
	}
	return -1
}

proc diffUnits {units1 units2} {
	if {$units1 eq $units2} {
		return
	}

	while {[llength $units1]} {
		set u1 [lindex $units1 0]

		set name1 [dict get $u1 Name]
		# use id in name to match (names should match, but aren't important)
		set id1 [lindex [extractUnitNameNum $name1] 1]
		set u2i [findUnitIdx $units2 $id1]
		if {$u2i == -1} {
			set ::match 0
			puts "File 2 missing unit '$u1'"
			set units1 [lrange $units1 1 end]
			continue
		}

		set u2 [lindex $units2 $u2i]
		# skip Desc and Report (foreign or own)
		if {[dict get $u1 Faction] ne [dict get $u2 Faction]} {
			puts "Factions mismatch '$u1' '$u2'"
			set ::match 0
		}
		if {[dict get $u1 Flags] ne [dict get $u2 Flags]} {
			puts "Flags mismatch '$u1' '$u2'"
			set ::match 0
		}

		# Items (list of lists)
		set items1 [dict get $u1 Items]
		set items2 [dict get $u2 Items]
		if {[lsort $items1] ne [lsort $items2]} {
			while {[llength $items1]} {
				set i1 [lindex $items1 0]
				set abbr1 [lindex $i1 2]
				set i2i [findItemIdx $items2 $abbr1]
				if {$i2i != -1} {
					set i2 [lindex $items2 $i2i]
					if {[dict get $i1 0] != [dict get $i2 0]} {
						puts "Counts mismatch '$i1' '$i2'"
						set ::match 0
					}
					set items2 [lreplace $items2 $i2i $i2i]
				} else {
					set ::match 0
					puts "File 2 unit '$u2' missing item '$i1'"
				}
				set items1 [lrange $items1 1 end]
			}

			foreach i2 $items2 {
				puts "File2 has extra item '$i2'"
				set ::match 0
			}
		}

		# remove corresponding unit2
		set units2 [lreplace $units2 $u2i $u2i]
		# pop front
		set units1 [lrange $units1 1 end]
	}

	foreach u2 $units2 {
		set ::match 0
		puts "File has extra unit '$u2'"
	}
}

###
set tfile1 [open [lindex $argv 0]]
set data1 [reader::parseFile $tfile1]
close $tfile1

set tfile2 [open [lindex $argv 1]]
set data2 [reader::parseFile $tfile2]
close $tfile2

if {[lsort $data1] eq [lsort $data2]} {
	# easy, all data matches
	puts "Files match."
	exit 0
}

# Regions (list of dict)
set regions1 [dict get $data1 Regions]
set regions2 [dict get $data2 Regions]

set data1 [dict remove $data1 Regions]
set data2 [dict remove $data2 Regions]

if {[lsort $regions1] ne [lsort $regions2]} {
	# detailed diff
	while {[llength $regions1]} {
		set r1 [lindex $regions1 0]
		set loc [dict get $r1 Location]

		set r2i [findRegionIdx $regions2 $loc]
		if {$r2i == -1} {
			set ::match 0
			puts "Location $loc not found in file 2"
			set regions1 [lrange $regions1 1 end]
			continue
		}

		set r2 [lindex $regions2 $r2i]

		if {[lsort $r1] ne [lsort $r2]} {
			set keys {
				Terrain
				Location
				Region
				Town
				Population
				Race
				MaxTax
				Wage
				MaxWage
				Entertainment
			}
			foreach k $keys {
				set v1 [dict get $r1 $k]
				set v2 [dict get $r2 $k]
				if {$v1 ne $v2} {
					puts "Mismatch in region $loc, '$k': '$v1' '$v2'"
					set match 0
				}
			}

			# TODO Sells Products

			# TODO Exits

			# Units (list of dict)
			set units1 [dict get $r1 Units]
			set units2 [dict get $r2 Units]
			diffUnits $units1 $units2

			# Objects (list of dict)
			set objects1 [dGet $r1 Objects]
			set objects2 [dGet $r2 Objects]

			while {[llength $objects1]} {
				set o1 [lindex $objects1 0]

				# Name ObjectName Flags Comment Units
				set name1 [dict get $o1 Name]
				set o2i [findObjectIdx $objects2 $name1]

				if {$o2i != -1} {
					set units1 [dict get $objects1 Units]
					set units2 [dict get $objects2 Units]
					diffUnits $units1 $units2

					set objects2 [lreplace $objects2 $o2i $o2i]
				} else {
					set ::match 0
					puts "File 2 missing object '$o1'"
				}

				set objects1 [lrange $objects1 1 end]
			}
			foreach o2 $objects2 {
				set ::match 0
				puts "File 2 has extra object '$o2'"
			}
		}

		# remove matching regions
		set regions1 [lrange $regions1 1 end]
		set regions2 [lreplace $regions2 $r2i $r2i]
	}
}

# Items (list of dict)
set items1 [dict get $data1 Items]
set items2 [dict get $data2 Items]

set data1 [dict remove $data1 Items]
set data2 [dict remove $data2 Items]

# TODO

# Battles
set battles1 [dict get $data1 Battles]
set battles2 [dict get $data2 Battles]

set data1 [dict remove $data1 Battles]
set data2 [dict remove $data2 Battles]

# TODO

# Events
set events1 [dict get $data1 Events]
set events2 [dict get $data2 Events]

set data1 [dict remove $data1 Events]
set data2 [dict remove $data2 Events]

# TODO

# Skills
set skills1 [dict get $data1 Skills]
set skills2 [dict get $data2 Skills]

set data1 [dict remove $data1 Skills]
set data2 [dict remove $data2 Skills]

# TODO

# Objects
set objects1 [dict get $data1 Objects]
set objects2 [dict get $data2 Objects]

set data1 [dict remove $data1 Objects]
set data2 [dict remove $data2 Objects]

# TODO

# compare remaining key/value pairs
# FactName WarNum TradeNum MagicNum Month Year Tax Trade Mage Appr Unclaimed
# PlayerNum PlayerPass Alignment
if {[lsort $data1] eq [lsort $data2]} {
	if {$match} {
		puts "Files match2"
	}
	exit 0
}

set keys1 [dict keys $data1]

while {[llength $keys1]} {
	# peek front
	set k1 [lindex $keys1 0]

	# pull value 1
	set val1 [dict get $data1 $k1]
	if {![dict exists $data2 $k1]} {
		set match 0
		puts "File2 missing key '$k1'"
	} else {
		set val2 [dict get $data2 $k1]
		if {$val1 ne $val2} {
			set match 0
			puts "Key '$k1' values '$val1' and '$val2'"
		}
		set data2 [dict remove $data2 $k1]
	}

	set data1 [dict remove $data1 $k1]

	# pop front
	set keys1 [lrange $keys1 1 end]
}

foreach {k v} $data2 {
	set match 0
	puts "File2 has extra key '$k' value '$v'"
}

if {$match} {
	puts "Files match1"
}


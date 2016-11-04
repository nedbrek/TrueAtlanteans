variable nextLine ""

variable unitFlags {
	{on guard} {GUARD 1}
	{avoiding} {AVOID 1}
	{behind} {BEHIND 1}
	{holding} {HOLD 1}
	{receiving no aid} {NOAID 1}
	{consuming faction's food} {CONSUME FACTION}
	{consuming unit's food} {CONSUME UNIT}
	{revealing faction} {REVEAL FACTION}
	{taxing} {AUTOTAX 1}
	{sharing} {SHARE 1}
}

proc dGet {d k} {
	if {![dict exists $d $k]} { return "" }

	return [string trim [dict get $d $k]]
}

proc getSection {f} {
	variable nextLine
	set ret $nextLine

	set i1 [regexp -inline -indices {^[ \t]*[^ \t]} $nextLine]
	set i1 [lindex $i1 0]
	set l [string trimright [gets $f]]

	while {![eof $f]} {
		if {$nextLine eq ""} {
			append ret $l
			set nextLine $l
			set i1 [regexp -inline -indices {^[ \t]*[^ \t]} $nextLine]
			set i1 [lindex $i1 0]
			set l [gets $f]
			continue
		}

		set i2 [regexp -inline -indices {^[ \t]*[^ \t]} $l]
		set i2 [lindex $i2 0]
		if {$l eq "" || [lindex $i1 1] == [lindex $i2 1]} {
			set nextLine $l
			return $ret
		}
		append ret $l

		set l [gets $f]
	}

	return $ret
}

proc searchListOfDict {l i key val} {
	set d [lindex $l $i]
	if {![dict exists $d $key]} {
		puts "Could not find $key with $val in $d (l($i))"
		exit 1
	}
	set v [dict get $d $key]

	return [expr {$v eq $val}]
}

# try and pull a unit's orders (unit is in region xy)
proc doRegionOrders {f regionVar xy} {
	variable nextLine
	set v [getSection $f]
	if {[eof $f]} { return "" }

	if {[lindex $v 0] ne "unit"} {
		set loc [lindex $v 2]
		set xy [string map {( "" ) "" , " "} $loc]
		return $xy
	}

	# save name
	set nameLine $nextLine
	set nextLine ""

	# pull the orders
	set orders ""
	set v [gets $f]
	while {$v ne ""} {
		# skip comments
		if {[string index $v 0] ne ";"} {
			lappend orders $v
		}

		set v [gets $f]
	}

	if {$orders eq ""} {
		# no orders for this unit
		return $xy
	}

	upvar $regionVar regions

	if {[llength $xy] == 2} {
		lappend xy 1
	}
	set i 0
	while {![searchListOfDict $regions $i "Location" $xy]} {
		incr i
	}
	set r [lindex $regions $i]

	# try and get name
	regexp {^;(.* \([[:digit:]]+\)), } $nameLine -> unitName

	set units [dGet $r Units]
	set j 0
	while {$j < [llength $units] &&
	       ![searchListOfDict $units $j "Name" $unitName]} {
		incr j
	}

	if {$j < [llength $units]} {
		# put the orders into the unit list
		set u [lindex $units $j]
		dict set u "Orders" $orders
		set units [lreplace $units $j $j $u]

		# update region
		dict set r Units $units
		set regions [lreplace $regions $i $i $r]
	}

	return $xy
}

proc translateUndergroundName {zName} {
	if {$zName eq ""} {
		return 1
	}

	if {$zName eq "nexus"} {
		return 0
	}

	if {[string is integer $zName]} {
		return [expr {int($zName)}]
	}

	set z 1
	while {[regsub {^very } $zName "" tmp]} {
		incr z
		set zName $tmp
	}
	if {[regsub {^deep } $zName "" tmp]} {
		incr z
		set zName $tmp
	}

	if {$zName eq "underworld"} {
		return [expr {$z + 1}]
	}

	if {$zName eq "underdeep"} {
		return [expr {$z + 64}]
	}

	return [expr {$z + 128}]
}

proc parseLocation {loc} {
	regexp {\(([[:digit:]]+),([[:digit:]]+),?([0-9a-zA-Z ]+)?} \
	   $loc -> x y z

	set l [list $x $y]
	lappend l [translateUndergroundName $z]
}

proc parseRegion {v} {
	# crack the region definition into chunks
	# terrain (one word) location (x,y[,z] <underworld>?) in Region contains...
	set r [regexp {([^() ]+) (\([[:digit:],]+[^)]*\)) in (.*)} $v -> \
	  terrain loc rest]
	if {$r != 1} {
		puts "Unable to parse region '$v'"
		exit
	}

	# Terrain
	set ret [dict create Terrain $terrain]

	# Location
	set l [parseLocation $loc]
	dict set ret Location $l

	set rest [string map {"\n" " "} [string trimright $rest "."]]
	set lm [split $rest ","]

	# Region
	dict set ret Region [lindex $lm 0]

	# Check for town
	set i 1
	set lmi [lindex $lm $i]
	set town ""
	if {[lindex $lmi 0] eq "contains"} {
		incr i ;# pull peasants from next i

		set town [lindex $lmi 1]

		set j 2
		while {![regexp {\[.*\]} [lindex $lmi $j]]} {
			append town " " [lindex $lmi $j]
			incr j
		}

		set fullType [lindex $lmi $j]
		set type [string map {\[ "" \] "" , ""} $fullType]
		lappend town $type
	}

	dict set ret Town $town

	# Population, Race
	set lmi [lindex $lm $i]
	if {$lmi eq ""} {return $ret} ;# done
	incr i

	regexp {([[:digit:]]+) +peasants +\(([^)]+)\)} $lmi -> pop race
	if {![info exists pop]} {
		puts "Could not parse region race token '$lmi' in region $v"
		exit
	}
	dict set ret Population $pop
	dict set ret Race $race

	# Max Taxes
	dict set ret MaxTax [string map {\$ "" . ""} [lindex $lm $i]]

	return $ret
}

# fields - originally comma separated list of a bunch of stuff
# return index of first item (after flags)
proc unitItemsIdx {fields} {
	# field 0 - name (and report type)
	# field 1 - faction, sometimes...
	# fields 2+ flags
	set i 1
	while {![regexp {\[[[:alnum:]]{3,4}\]} [lindex $fields $i]]} {
		incr i
		if {$i > [llength $fields]} {
			puts "Error in $fields"
			exit
		}
	}
	return $i
}

proc repairItemList {l} {
	set ret ""
	foreach i $l {
		if {[string is integer [lindex $i 0]]} {
			lappend ret [list [lindex $i 0] [lrange $i 1 end-1] [lindex $i end]]
		} else {
			lappend ret [list 1 [lrange $i 0 end-1] [lindex $i end]]
		}
	}
	return $ret
}

proc fixSkills {skills} {
	set ret ""
	foreach s $skills {
		set name [lrange $s 0 end-3]
		set abbr [string map {"\[" "" "]" ""} [lindex $s end-2]]
		set lvl  [lindex $s end-1]
		set pts  [string map {"(" "" ")" ""} [lindex $s end]]

		lappend ret [list $name $abbr $lvl $pts]
	}

	return $ret
}

proc parseUnit {v} {
	variable unitFlags

	# what sort of report is this
	set quality own
	if {[lindex $v 0] == "-"} {
		set quality foreign
	}

	# get unit name
	set comma [string first "," $v]
	set n [string range $v 2 $comma-1]

	set groups [split $v "."]

	set group0 [split [lindex $groups 0] ","]
	set itemIdx [unitItemsIdx $group0]

	set uflags ""
	set flags [lrange $group0 1 $itemIdx-1]
	foreach f $flags {
		set f [string trim $f]
		set f [regsub -all { +} $f " "]

		if {[regexp {.*\([[:digit:]]+\)$} $f] == 1} {
			continue
		}

		set i [dict exists $unitFlags $f]
		if {$i == 0} {
			puts "Unknown flag '$f'"
			dict set unitFlags $f ""
		} else {
			dict set uflags {*}[dict get $unitFlags $f]
		}
	}

	set items [lrange $group0 $itemIdx end]
	set items [repairItemList $items]

	set u [dict create Name $n Desc {} Report $quality Items $items]
	dict set u Flags $uflags

	# group 3 - skills
	if {$quality eq "own"} {
		set group3 [string map {"\n" " "} [lindex $groups 3]]
		set skills [split [lrange $group3 1 end] ","]

		dict set u Skills [fixSkills $skills]
	}

	return $u
}

# convert a list of sales items {num long short price}
# into a list with two items {{num long short} price}
proc fixSales {sells} {
	set ret ""
	foreach s $sells {
		set price [lindex $s end]
		set s [regsub {[[:digit:]]+$} $s ""]
		lappend ret [string trim $s] $price
	}

	return $ret
}

proc getRegion {f} {
	variable nextLine
	set v [getSection $f]
	if {$v eq "Orders Template (Long Format):"} {
		return ""
	}
	set region [parseRegion $v]
	set nextLine "" ;# clear the -----

	# weather
	set v [getSection $f]
	regexp {was (.*) last month; it will be (.*) next} $v -> old new
	dict set region WeatherOld $old
	dict set region WeatherNew $new

	# check for Nexus
	set v [getSection $f]
	if {[regexp {Nexus is a} $v]} {
		set v [getSection $f]
	}

	# wages
	dict set region Wage    [string map {\$ ""} [lindex $v 1]]
	dict set region MaxWage [string map {\$ "" \) "" . ""} [lindex $v 3]]

	# wants
	set v [getSection $f]
	set wants [string map {"\n" " " "\$" "" " at " " "} [string trimright $v "."]]
	set wants [split [regsub " Wanted: " $wants ""] ","]
	if {$wants ne "none"} {
		dict set region Wants [fixSales $wants]
	}

	# for sale
	set v [getSection $f]
	set sell [string map {"\n" " " "\$" "" " at " " "} [string trimright $v "."]]
	set sell [split [regsub " For Sale: " $sell ""] ","]
	if {$sell ne "none"} {
		dict set region Sells [fixSales $sell]
	}

	# entertainment
	set v [getSection $f]
	if {[regexp {Entertainment available: \$([[:digit:]]+)\.} $v -> ente]} {

		dict set region Entertainment $ente

		# products
		set v [getSection $f]
	}
	set v [string map {"\n" " "} [string trimright $v "."]]
	dict set region Products [split [regsub " Products: " $v ""] ","]

	# exits
	set v [getSection $f]
	set v [string map {\[ "" \] ""} $v]
	set exits [split [lrange $v 1 end] "."]

	set eout ""
	foreach e $exits {
		if {$e eq ""} continue
		if {![string is list $e]} {
			puts "odd '$e'"
			puts "exits '$exits'"
			puts "region '$region'"
			exit
		}

		lappend eout [lindex $e 0]
		set terrain  [lindex $e 2]

		set loc [lindex $e 3]
		set lxy [parseLocation $loc]

		set ci [lsearch $e "contains"]
		if {$ci == -1} {
			set exRegion [lrange $e 5 end]
			set town ""
		} else {
			set exRegion [string trimright [lrange $e 5 $ci-1] ","]

			set townName [lrange $e $ci+1 end-1]
			set townType [string trimright [lindex $e end] "."]

			set town [list $townName $townType]
		}

		lappend eout [list Location $lxy Terrain $terrain Town $town \
		  Region $exRegion]
	}
	dict set region Exits $eout

	# units
	set hadBuilding 0
	set oldNextLine $nextLine
	set filePtr [tell $f]
	set v [getSection $f]
	if {[regexp {There is a Gate here} $v]} {
		set oldNextLine $nextLine
		set filePtr [tell $f]
		set v [getSection $f]
	}

	while {[lindex $v 0] eq "-" ||
	       [lindex $v 0] eq "*" ||
	       [lindex $v 0] eq "+"} {

		# check that building reports are last
		if {[lindex $v 0] eq "+"} {
			set hadBuilding 1

			set lines [split [string trimright $v "."] "."]
			set hdr [lindex $lines 0]
			regexp {\+ ([^:]+) : (.*)} $hdr -> oname odesc
			set object [dict create Name $oname]

			set objFlags [split [string trimright $odesc "."] ","]
			dict set object ObjectName [lindex $objFlags 0]

			if {[llength $lines] == 1} {
				dict lappend region Objects $object

				set oldNextLine $nextLine
				set filePtr [tell $f]

				set v [getSection $f]

				continue
			}

			set i 1
			set j 2
			while {$i < [llength $lines]} {
				while {$j < [llength $lines] &&
				       [lindex [lindex $lines $j] 0] ne "*" &&
				       [lindex [lindex $lines $j] 0] ne "-"} {
					incr j
				}

				set v1 [join [lrange $lines $i $j-1] "."]
				set u [parseUnit $v1]

				dict lappend object Units $u

				set i $j
				incr j
			}

			dict lappend region Objects $object

			set oldNextLine $nextLine
			set filePtr [tell $f]

			set v [getSection $f]
			continue
		}

		if {$hadBuilding} {
			puts "Error building intermixed with units in '$v'"
			exit
		}

		set u [parseUnit $v]

		dict lappend region Units $u

		set oldNextLine $nextLine
		set filePtr [tell $f]

		set v [getSection $f]
	}

	seek $f $filePtr
	set nextLine $oldNextLine

	return $region
}

proc parseSkill {v} {
	set l [split [string trimright $v "."] "."]
	return $l
}

proc parseObject {v} {
	set l [split [string trimright $v "."] "."]
	return $l
}

proc parseItem {v} {
	set l [split [string trimright $v "."] "."]
	set sl0 [split [lindex $l 0] ","]

	dict set d Name [lindex $sl0 0]
	dict set d Weight [lindex [lindex $sl0 1] end]
	# lindex 2 can swim

	if {[llength $l] == 1} {
	# simple item
		dict set d Type item
		return $d
	}

	set l1 [lindex $l 1]
	if {[lindex $l1 0] ne "This"} {
		# production item
		dict set d Type item
		dict set d Desc $l1
		return $d
	}
	#else other items

	if {[lindex $l1 1] eq "race"} {
		dict set d Type race
		dict set d Desc $l1
		return $d
	}

	set type [lindex $l1 end]
	dict set d Type $type
	dict set d Desc [lrange $l 2 end]
	switch $type {
		monster {
		}

		armor {
			dict set d Protect [lindex $l 2]
			dict set d Produce [lindex $l 3]
		}

		tool {
			dict set d Boost [lindex $l 2]
			dict set d Produce [lindex $l 3]
		}

		good {
			dict set d Bought  [lindex $l 2]
			dict set d Sold    [lindex $l 3]
			dict set d Produce [lindex $l 4]
		}

		mount {
		}

		weapon {
			dict set d Skill [lindex $l 2]
		}
	}

	return $d
}

proc parseFile {f} {
	# initial headers
	set v [getSection $f]
	# Atlantis Report For:

	set v [getSection $f]
	# Faction Name (number) (War n,Trade n, Magic n)

	set v [getSection $f]
	# <Month>, Year <number>
	set v [string map {"," ""} $v]
	set turn [dict create Month [lindex $v 0]]
	dict set turn [lindex $v 1] [lindex $v 2]

	# extract items after events
	set itemList [list]
	set skillList [list]
	set objList [list]

	set v [getSection $f]
	while {![regexp {^Unclaimed silver:} $v]} {
		set v [getSection $f]

		if {$v eq "Skill reports:"} {
			set skillF [open "skills.txt" a]

			set v [getSection $f]
			while {![regexp {^Declared Attitudes} $v] &&
			       $v ne "Object reports:" &&
			       $v ne "Item reports:"} {

				set skillDesc [parseSkill $v]
				puts $skillF $skillDesc
				lappend skillList $skillDesc

				set v [getSection $f]
			}
		}

		if {$v eq "Item reports:"} {
			set itemF [open "items.txt" a]

			set v [getSection $f]

			while {![regexp {^Declared Attitudes} $v] &&
			       $v ne "Object reports:"} {
				set itemDesc [parseItem $v]
				puts $itemF $itemDesc
				lappend itemList $itemDesc

				set v [getSection $f]
			}

			close $itemF
		}

		if {$v eq "Object reports:"} {
			set objectF [open "objects.txt" a]

			set v [getSection $f]
			while {![regexp {^Declared Attitudes} $v]} {
				set objDesc [parseObject $v]
				puts $objectF $objDesc
				lappend objList $objDesc

				set v [getSection $f]
			}
		}
	}
	dict set turn "Items" $itemList
	dict set turn "Skills" $skillList
	dict set turn "Objects" $objList

	# unclaimed silver

	# regions
	set regions ""
	set regionData [getRegion $f]
	while {$regionData ne ""} {
		lappend regions $regionData
		set regionData [getRegion $f]
	}

	# orders template
	# faction number and pass
	set v [getSection $f]
	if {[lindex $v 0] ne "#atlantis"} {
		puts "Parse error: expected start of order template"
		exit 1
	}
	dict set turn PlayerNum [lindex $v 1]

	# orders
	set v [getSection $f]
	set loc [lindex $v 2]
	set xy [string map {( "" ) "" , " "} $loc]

	while {$xy ne ""} {
		set xy [doRegionOrders $f regions $xy]
	}

	# done
	dict set turn Regions $regions
	return $turn
}

################
if {![info exists debug]} {
	if {$argc < 1} {
		puts "Usage $argv0 <filename>"
		exit
	}

	foreach fname $argv {
		set f [open $fname]

		set l [join [lrange [file split $fname] end-1 end] "_"]
		set ofile [format {%s%s} "c" $l]

		set chn [open $ofile "w"]
		puts $chn [parseFile $f]

		close $f
		close $chn
		set nextLine ""
	}
}


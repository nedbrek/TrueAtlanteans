package provide atlantis_reader 1.0

namespace eval reader {
set debug 1

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
	{revealing unit} {REVEAL UNIT}
	{taxing} {AUTOTAX 1}
	{sharing} {SHARE 1}
	{walking battle spoils} {SPOILS WALK}
	{riding battle spoils} {SPOILS RIDE}
	{flying battle spoils} {SPOILS FLY}
	{weightless battle spoils} {SPOILS NONE}
	{won't cross water} {NOCROSS 1}
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

	set in_building 0
	if {[string range $nextLine 0 0] eq "+"} {
		set in_building 1
	}

	while {![eof $f]} {
		if {$nextLine eq ""} {
			append ret $l
			set nextLine $l
			set in_building 0
			if {[string range $nextLine 0 0] eq "+"} {
				set in_building 1
			}
			set i1 [regexp -inline -indices {^[ \t]*[^ \t]} $nextLine]
			set i1 [lindex $i1 0]
			set l [gets $f]
			continue
		}

		set i2 [regexp -inline -indices {^[ \t]*[^ \t]} $l]
		set i2 [lindex $i2 0]
		set first_char [string range $l 2 2]
		if {$l eq "" || [lindex $i1 1] == [lindex $i2 1] || ($in_building && ($first_char == "*" || $first_char == "-"))} {
			set nextLine $l
			return [regsub -all { +} $ret " "]
		}
		append ret $l

		set l [gets $f]
	}

	return [regsub -all { +} $ret " "]
}

proc getOrderSection {f} {
	set v [getSection $f]
	if {[string range $v 0 3] ne ";***"} {
		return $v
	}

	if {[lindex $v end] ne "***"} {
		set v2 [getSection $f]
		if {[string range $v 0 0] != ";"} {
			puts "Error in getOrderSection(): '$v' '$v2'"
			exit 1
		}
		append v [string range $v2 1 end]
	}
	return $v
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
	set v [getOrderSection $f]
	if {[eof $f]} { return "" }

	if {[lindex $v 0] ne "unit"} {
		set loc [lindex $v 2]
		set xy [string map {( "" ) "" , " "} $loc]
		return $xy
	}

	# extract name from description
	if {[string index $nextLine 0] eq ";"} {
		set nameLine $nextLine
		set nextLine ""
	} else {
		# TODO find unit by id (v[1])
		#set nameLine [lindex $v 1]
		set nameLine ""
	}

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

	if {$orders eq "" || $nameLine eq ""} {
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
	set units [dGet $r Units]
	set j 0

	# try and get name
	if {![regexp {^;(.* \([[:digit:]]+\)), } $nameLine -> unitName]} {
		if {![regexp {^unit ([[:digit:]]+)} [lindex $orders 0] -> unitName]} {
			if {$orders eq [list "#end"]} {
				return $xy
			}
			puts "Can't find name in $nameLine '$orders'"
			set j [llength $units]
		} else {
			set orders [lrange $orders 1 end]
		}
	}

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
		return $xy
	}

	#else check objects
	set objs [dGet $r Objects]
	for {set oi 0} {$oi < [llength $objs]} {incr oi} {
		set o [lindex $objs $oi]
		set units [dGet $o Units]
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

			# update object
			dict set o Units $units
			set objs [lreplace $objs $oi $oi $o]

			# update region
			dict set r Objects $objs
			set regions [lreplace $regions $i $i $r]
		}
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
	if {![regexp {\(([[:digit:]]+),([[:digit:]]+),?([0-9a-zA-Z ]+)?} \
	   $loc -> x y z]} {
		return ""
	}

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
	set i 0
	while {![regexp {\[[[:alnum:]]{3,6}\]} [lindex $fields $i]]} {
		incr i
		if {$i > [llength $fields]} {
			error "Error in $fields"
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
	set first_char [lindex $v 0]
	if {$first_char eq "-" || $first_char eq "%" || $first_char eq ":"} {
		set quality foreign
	}

	# get unit name
	set paren [string first ")" $v]
	set n [string range $v 2 $paren]
	if {[regexp {\.} $n]} {
		set n2 [string map {. "_"} $n]
		set v [string replace $v 2 $paren $n2]
	}

	# strip description
	set after_name [string range $v $paren end]
	set d [regexp {;(.*)$} $after_name -> desc]
	if {$d == 0} {
		set desc ""
	} else {
		set desc [string trim $desc]
		set after_name [regsub {;.*$} $after_name ""]
		set v [string replace $v $paren end $after_name]
	}

	# filter . from faction
	set paren2 [string first ")" $v $paren+1]
	if {$paren2 != -1} {
		set fac_substr [string range $v 2 $paren2]
		if {[regexp {\.} $fac_substr]} {
			set n2 [string map {. "_"} $fac_substr]
			set v [string replace $v 2 $paren2 $n2]
		}
	}

	set groups [split [string range $v $paren+3 end] "."]

	set group0 [split [lindex $groups 0] ","]
	if {[catch {unitItemsIdx $group0} itemIdx]} {
		error "parseUnit: '$v'"
	}

	set uflags ""
	set fact ""
	set flags [lrange $group0 0 $itemIdx-1]
	foreach f $flags {
		set f [string trim $f]
		set f [regsub -all { +} $f " "]

		# look for faction
		if {[regexp {.*\([[:digit:]]+\)$} $f] == 1} {
			set fact $f
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

	set u [dict create Name $n Desc $desc Report $quality Items $items]
	dict set u Faction $fact
	dict set u Flags $uflags

	# group 1 - weight (ignore)
	# group 2 - capacity (ignore)
	# group 3 - skills
	set i 3
	if {$quality eq "own"} {
		set group3 [string map {"\n" " "} [lindex $groups $i]]
		if {[regexp {Upkeep:} $group3]} {
			incr i
			set group3 [string map {"\n" " "} [lindex $groups $i]]
		}
		set skills [split [lrange $group3 1 end] ","]

		dict set u Skills [fixSkills $skills]
	}
	incr i

	# group 4 - combat spell
	set group4 [lindex $groups $i]
	if {$group4 ne ""} {
		if {[regexp { *Combat spell: *(.*)} $group4 -> cspell]} {
			dict set u CombatSpell $cspell
			incr i
		}
	}

	# group 5 - ready
	set group5 [lindex $groups $i]
	if {$group5 ne ""} {
		if {[regexp { *Ready weapon: *(.*)} $group5 -> rweapon]} {
			incr i
			set group5 [lindex $groups $i]
		}
		if {[regexp { *Ready armor: *(.*)} $group5 -> rarmor]} {
			incr i
		}
	}

	# group 6 - can study
	set group6 [lindex $groups $i]
	if {$group6 ne ""} {
		if {[regexp { *Can Study: *(.*)} $group6 -> cstudy]} {
			dict set u CanStudy [split $cstudy ","]
		} elseif {[regexp { *Ready weapons: *(.*)} $group6 -> readyw]} {
			dict set u ReadyWeapons [split $readyw ","]
		} else {
			puts "Error CanStudy: '$group6'"
			exit
		}
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
	if {[eof $f] || [regexp {^Orders Template \(.* Format\):$} $v]} {
		return ""
	}
	set region [parseRegion $v]
	set nextLine "" ;# clear the -----

	# weather
	set v [getSection $f]
	if {[regexp {was (.*) last month; it will be (.*) next} $v -> old new]} {
		dict set region WeatherOld $old
		dict set region WeatherNew $new
		set v [getSection $f]
	}

	# check for Nexus
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
		if {$e eq "" || $e eq "none"} continue
		if {![string is list $e]} {
			puts "odd '$e'"
			puts "exits '$exits'"
			puts "region '$region'"
			exit
		}

		# check for "Direction: terrain (loc) in region
		# versus "Direction <space> :"
		set off 3
		if {[regexp {[A-Z][a-z]*:} $e]} {
			# no space
			lappend eout [string trimright [lindex $e 0]]
			set terrain  [lindex $e 1]
			incr off -1
		} else {
			lappend eout [lindex $e 0]
			set terrain  [lindex $e 2]
		}

		set loc [lindex $e $off]
		set lxy [parseLocation $loc]
		if {$lxy eq ""} {
			puts "Failed to parseLocation '$exits'"
		}
		incr off 2

		set ci [lsearch $e "contains"]
		if {$ci == -1} {
			set exRegion [lrange $e $off end]
			set town ""
		} else {
			set exRegion [string trimright [lrange $e $off $ci-1] ","]

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
	if {[regexp {There is a Gate here \((.*)\)} $v -> gate_details]} {
		dict set region Gate $gate_details

		set oldNextLine $nextLine
		set filePtr [tell $f]
		set v [getSection $f]
	}

	while {![eof $f] &&
		    ([lindex $v 0] eq "-" ||
	       [lindex $v 0] eq "*" ||
	       [lindex $v 0] eq ":" ||
	       [lindex $v 0] eq "%" ||
	       [lindex $v 0] eq "+")} {

		# check that building reports are last
		if {[lindex $v 0] eq "+"} {
			set hadBuilding 1

			regexp {\+ ([^:]+) : ([^;]*);?(.*)?} $v -> oname odesc ocomment
			set object [dict create Name $oname]

			set objFlags [split [string trimright $odesc "."] ","]
			dict set object ObjectName [lindex $objFlags 0]
			dict set object Flags [lrange $objFlags 1 end]
			dict set object Comment [string trimleft $ocomment]

			set oldNextLine $nextLine
			set filePtr [tell $f]
			set next_v [getSection $f]
			while {[string range $next_v 0 0] == " "} {
				set u [parseUnit [string range $next_v 1 end]]
				dict lappend object Units $u

				set oldNextLine $nextLine
				set filePtr [tell $f]
				set next_v [getSection $f]
			}

			dict lappend region Objects $object
			set v $next_v

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
	# split sentences into a list
	set l [split [string trimright $v "."] "."]
	# grab first item
	set l0 [lindex $l 0]

	# skill name [ABBR] level: Overall description
	# (note skill name can have spaces)
	# Some skills have a description "No skill report"
	regexp {([^[]+) \[([^]]+)\] ([[:digit:]]+): (.*)} $l0 -> name abbr lvl rest
	lset l 0 $rest

	# getSection() can introduce multiple spaces
	set l [regsub -all {  } $l " "]

	set i [lsearch $l {*This skill costs*}]
	if {$i != -1} {
		set li [lindex $l $i]
		regexp { *This skill costs ([[:digit:]]+) silver per month of study} $li -> cost
		# remove it
		set l [lreplace $l $i $i]
	} else {
		set cost ""
	}

	return [dict create Name $name Abbr $abbr Level $lvl Cost $cost Desc $l]
}

proc parseObject {v} {
	set l [split [string trimright $v "."] "."]
	return $l
}

proc parseItem {v} {
	set v [regsub -all { +} $v " "]
	set l [split [string trimright $v "."] "."]
	set sl0 [split [lindex $l 0] ","]

	dict set d Name [lindex $sl0 0]

	if {[llength $l] == 1} {
		# simple item
		dict set d Type item
		return $d
	}

	set sl01 [lindex $sl0 1]
	if {[string trim [lindex $sl01 0]] eq "weight"} {
		dict set d Weight [lindex $sl01 end]
	}

	# parse indexes 2 to end
	set carries [dict create]
	foreach c [lrange $sl0 2 end] {
		if {[lindex $c 1] eq "capacity"} {
			dict set carries [string trim [lindex $c 0]] [lindex $c 2]
		} elseif {[string trim $c] eq "can ride"} {
			dict set carries "riding" 0
		} elseif {[string trim $c] eq "can walk"} {
			dict set carries "walking" 0
		} elseif {[string trim $c] eq "can swim"} {
			dict set carries "swimming" 0
		} elseif {[string trim $c] eq "can fly"} {
			dict set carries "flying" 0
		} elseif {[lindex $c 0] eq "moves"} {
			dict set carries "move" [lindex $c 1]
		} elseif {[lindex $c end] ne "withdraw"} {
			puts "Parse error in capacity of item $v"
			exit 1
		}
	}
	if {$carries ne ""} {
		dict set d Capacity $carries
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
		dict set d Desc [lrange $l 1 end]
		return $d
	}

	# handle ships as items
	if {[lrange $l1 0 3] eq "This is a ship"} {
		dict set d Type ship
		dict set d Desc [lrange $l 1 end]
		return $d
	}

	set type_index "end"
	set tmp [lsearch $l1 "and"]
	if {$tmp != -1} {
		set type_index [expr {$tmp - 1}]
	}

	set type [lindex $l1 $type_index]
	dict set d Type $type
	switch $type {
		monster {
			dict set d Desc [lrange $l 2 end]
		}

		armor {
			dict set d Desc [lrange $l 2 end]
			dict set d Protect [lindex $l 2]
			dict set d Produce [lindex $l 3]
		}

		tool {
			dict set d Desc [lrange $l 2 end]
			dict set d Boost [lindex $l 2]
			dict set d Produce [lindex $l 3]
		}

		good {
			dict set d Desc [lrange $l 2 end]
			dict set d Bought  [lindex $l 2]
			dict set d Sold    [lindex $l 3]
			dict set d Produce [lindex $l 4]
		}

		mount {
			dict set d Desc [lrange $l 2 end]
		}

		weapon {
			dict set d Desc [lrange $l 1 end]
			dict set d Skill [lindex $l 2]
		}
	}

	return $d
}

proc parseBattle {f} {
	set v [getSection $f]
	if {$v eq "Events during turn:" ||
	    [regexp {Declared Attitudes} $v]} {
		return [list "" $v]
	}
	if {[regexp {rises? from the grave to } $v]} {
		return [list "" $v PREV]
	}

	set ret [dict create]

	# Attacker (Id) attacks defender (id) in terrain (x,y) in region
	if {[regexp {([^()]+) \(([[:digit:]]+)\) attacks ([^()]+) \(([[:digit:]]+)\) in [^()]+ \(([[:digit:]]+),([[:digit:]]+)(,.*)?\) +in .+!} $v -> attacker att_id defender def_id x y z]} {
		dict set ret "Attacker" $attacker
		dict set ret "AttId" $att_id
		dict set ret "Defender" $defender
		dict set ret "DefId" $def_id
		if {$z eq ""} {set z 1}
	} elseif {[regexp {([^()]+) \(([[:digit:]]+)\) attempts to assassinate ([^()]+) \(([[:digit:]]+)\) in [^()]+ \(([[:digit:]]+),([[:digit:]]+)(,.*)?\) +in .+!} $v -> attacker att_id defender def_id x y z]} {
		dict set ret "Attacker" $attacker
		dict set ret "AttId" $att_id
		dict set ret "Defender" $defender
		dict set ret "DefId" $def_id
		if {$z eq ""} {set z 1}
	} elseif {[regexp {([^()]+) \(([[:digit:]]+)\) is assassinated in [^()]+ \(([[:digit:]]+),([[:digit:]]+)(,.*)?\) +in} $v -> defender def_id x y z]} {
		dict set ret "Attacker" "assassin"
		dict set ret "AttId" 0
		dict set ret "Defender" $defender
		dict set ret "DefId" $def_id
		if {$z eq ""} {
			set z 1
		} else {
			set z [string trim [lindex $z 0] ","]
		}
		dict set ret "XY" [list $x $y $z]
		return [list $ret ""]
	} else {
		puts "Parse error in battle on '$v'"
		exit 1
	}

	set z [string trim [lindex $z 0] ","]
	dict set ret "XY" [list $x $y $z]

	set v [getSection $f]
	# Attackers:
	if {$v ne "Attackers:"} {
		puts "Parse error in battle"
		exit 1
	}

	set v [getSection $f]
	while {$v ne "Defenders:"} {
		# Name (Id), Faction (Id), flags, items, skills; description
		# (repeat per unit)
		dict lappend ret Attackers $v
		set v [getSection $f]
	}

	set v [getSection $f]
	while {$v ne "Round 1:" && ![regexp {gets a free round of attacks} $v]} {
		# Name (Id), Faction (Id), flags, items, skills; description
		dict lappend ret Defenders $v
		set v [getSection $f]
	}

	# battle begins
	if {[regexp {(.+) gets a free round of attacks} $v -> first]} {
		dict set ret "Tactics" $first
	}

	# <offensive unit> loses N.
	# <defensive unit> loses N.
	# <unit> is destroyed!
	# Total Casualties:
	# <unit> loses N.
	# Damaged units: <Id>.

	set v [getSection $f]
	while {![regexp {^Spoils: ([^.]+).} $v -> spoils]} {
		dict lappend ret RAW $v
		set v [getSection $f]
	}

	# Spoils:
	dict set ret "Spoils" $spoils

	return [list $ret ""]
}

proc isHeader {v} {
	if {$v eq "Skill reports:"} { return 1 }
	if {$v eq "Object reports:"} { return 1 }
	if {$v eq "Item reports:"} { return 1 }
	if {$v eq "Battles during turn:"} { return 1 }
	if {$v eq "Errors during turn:"} { return 1 }
	if {$v eq "Events during turn:"} { return 1 }
	if {[regexp {^Declared Attitudes} $v]} { return 1 }

	return 0
}

proc parseFile {f} {
	variable nextLine
	set nextLine ""

	set turn [dict create]

	# initial headers
	set v [getSection $f]
	# Atlantis Report For:
	while {![eof $f] && $v ne "Atlantis Report For:"} {
		set v [getSection $f]
	}
	if {[eof $f]} {
		puts "Start of report not found"
		exit 1
	}

	set v [getSection $f]
	# Faction Name (number) (War n,Trade n, Magic n)
	if {![regexp {([^(]+) \(([[:digit:]]+)\) \(War ([[:digit:]]+), Trade ([[:digit:]]+)(, Magic ([[:digit:]]+))?\)} $v -> \
	   fact_name fact_num war_num trade_num magic_num]} {
			if {[regexp {([^(]+) \(([[:digit:]]+)\) \(Martial ([[:digit:]]+)(, Magic ([[:digit:]]+))?\)} $v -> \
	   fact_name fact_num martial_num magic_num]} {
				set war_num $martial_num
				set trade_num $martial_num
			} else {
				if {![regexp {([^(]+) \(([[:digit:]]+)\)} $v -> fact_name fact_num]} {
					puts "Error parsing faction name '$v'"
					exit 1
				}
				# no faction points
				set war_num 0
				set trade_num 0
				set magic_num 0
			}
	}

	dict set turn FactName $fact_name
	dict set turn WarNum $war_num
	dict set turn TradeNum $trade_num
	dict set turn MagicNum [expr {$magic_num eq "" ? 0 : $magic_num}]

	set v [getSection $f]
	# <Month>, Year <number>
	set v [string map {"," ""} $v]
	dict set turn Month [lindex $v 0]
	if {[lindex $v 1] ne "Year"} {
		puts "Parse error in turn month/year"
		exit 1
	}
	dict set turn Year [lindex $v 2]

	# extract items after events
	set itemList [list]
	set skillList [list]
	set objList [list]
	set eventList [list]
	set alignment ""

	set v [getSection $f]
	while {![regexp {^Unclaimed silver:} $v]} {

		if {$v eq "Faction Status:"} {
			# tax
			set v [getSection $f]
			if {[regexp {Tax Regions: ([[:digit:]]+) \(([[:digit:]]+)\)} $v -> tax_use tax_max]} {
				set v [getSection $f]
			} else {
				set tax_use 0
				set tax_max 1000
			}

			# trade
			if {[regexp {Trade Regions: ([[:digit:]]+) \(([[:digit:]]+)\)} $v -> trade_use trade_max]} {
				set v [getSection $f]
			} else {
				set trade_use 0
				set trade_max 1000
			}

			# combined tax and trade
			if {[regexp {Regions: ([[:digit:]]+) \(([[:digit:]]+)\)} $v -> trade_use trade_max]} {
				set v [getSection $f]
			} else {
				set trade_use 0
				set trade_max 1000
			}

			# quartermasters (optional)
			if {[regexp {Quartermasters: ([[:digit:]]+) \(([[:digit:]]+)\)} $v -> qm_use qm_max]} {
				set v [getSection $f]
			} else {
				set qm_use 0
				set qm_max 1000
			}

			# mages
			regexp {Mages: ([[:digit:]]+) \(([[:digit:]]+)\)} $v -> mage_use mage_max

			# apprentices (if they exist)
			set v [getSection $f]
			if {[regexp {Apprentices: ([[:digit:]]+) \(([[:digit:]]+)\)} $v -> appr_use appr_max]} {
				set v [getSection $f]
			} else {
				set appr_use ""
				set appr_max ""
			}

			dict set turn Tax [list $tax_use $tax_max]
			dict set turn Trade [list $trade_use $trade_max]
			dict set turn Quartermaster [list $qm_use $qm_max]
			dict set turn Mage [list $mage_use $mage_max]
			dict set turn Appr [list $appr_use $appr_max]

		} elseif {[regexp {Your faction is (evil|neutral|good)} $v -> alignment]} {
			set v [getSection $f]
		} elseif {$v eq "Skill reports:"} {
			#set skillF [open "skills.txt" a]

			set v [getSection $f]
			while {![regexp {^Declared Attitudes} $v] &&
			       $v ne "Object reports:" &&
			       $v ne "Item reports:"} {

				set skillDesc [parseSkill $v]
				#puts $skillF $skillDesc
				lappend skillList $skillDesc

				set v [getSection $f]
			}
		} elseif {$v eq "Item reports:"} {
			#set itemF [open "items.txt" a]

			set v [getSection $f]

			while {![regexp {^Declared Attitudes} $v] &&
			       $v ne "Object reports:"} {
				set itemDesc [parseItem $v]
				#puts $itemF $itemDesc
				lappend itemList $itemDesc

				set v [getSection $f]
			}

			#close $itemF
		} elseif {$v eq "Object reports:"} {
			#set objectF [open "objects.txt" a]

			set v [getSection $f]
			while {![regexp {^Declared Attitudes} $v]} {
				set objDesc [parseObject $v]
				#puts $objectF $objDesc
				lappend objList $objDesc

				set v [getSection $f]
			}
		} elseif {$v eq "Battles during turn:"} {
			set battleList [list]
			set battle_ret [parseBattle $f]
			set battle [lindex $battle_ret 0]
			while {$battle ne ""} {
				set tmp_battle $battle
				set battle_ret [parseBattle $f]
				set battle [lindex $battle_ret 0]
				if {[lindex $battle_ret 2] eq "PREV"} {
					dict lappend tmp_battle RAW [lindex $battle_ret 1]
				}
				lappend battleList $tmp_battle
			}
			dict set turn "Battles" $battleList

			# we've read ahead into the next session
			set v [lindex $battle_ret 1]

		} elseif {$v eq "Errors during turn:"} {
			set v [getSection $f]
			while {![isHeader $v]} {
				# add as event
				if {[regexp {[^(]+\(([[:digit:]]+)\): (.*)} $v -> unit_num event_desc]} {
					lappend eventList [dict create TYPE ERROR UNIT $unit_num DESC $event_desc]
				} elseif {[regexp {QUIT: Must give the correct password.} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} elseif {[regexp {Incorrect password on #atlantis line.} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} elseif {[regexp {Order given without a unit selected.} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} elseif {[regexp { is not your unit.} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} elseif {[regexp {DECLARE: Non-existent faction} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} elseif {[regexp {DECLARE: Invalid attitude.} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} elseif {[regexp {DECLARE: Can't declare towards your own faction.} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} elseif {[regexp {END: without FORM.} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} elseif {[regexp {ENDTURN: without TURN.} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} elseif {[regexp {DECLARE ALIGNMENT:} $v]} {
					lappend eventList [dict create TYPE ERROR DESC $v]
				} else {
					puts "Error parsing error '$v'"
					exit 1
				}
				set v [getSection $f]
			}
		} elseif {$v eq "Events during turn:"} {
			set v [getSection $f]
			while {![isHeader $v]} {
				# add as event
				if {[regexp {Times reward of ([[:digit:]]+) silver.} $v -> reward]} {
					lappend eventList [dict create TYPE REWARD AMT $reward]
				} elseif {[regexp {Reward of ([[:digit:]]+) silver.} $v -> reward]} {
					lappend eventList [dict create TYPE REWARD AMT $reward]
				} elseif {[regexp {[^(]+\(([[:digit:]]+)\):? (.*)} $v -> unit_num event_desc]} {
					set event [dict create TYPE EVENT UNIT $unit_num DESC $event_desc]

					if {[lindex $event_desc 1] eq "forbidden"} {
						set xy_text [lindex $event_desc 5]
						set xy [parseLocation $xy_text]
						if {$xy eq ""} {
							puts "Error parsing event '$v'"
						} else {
							dict set event LOC $xy
							dict set event SUB FORBID
						}
					}

					lappend eventList $event
				} elseif {[regexp {.* sails from .* to } $v]} {
					lappend eventList [dict create TYPE SAIL DESC $v]
				} elseif {[regexp {.* is caught attempting to assassinate [^(]* \(([[:digit:]]+)\)} $v -> unit_num]} {
					lappend eventList [dict create TYPE EVENT UNIT $unit_num DESC $v]
				} elseif {[regexp {.* is stopped by guards } $v]} {
					lappend eventList [dict create TYPE SAIL DESC $v]
				} elseif {[regexp {Times will be sent to your faction.} $v]} {
				} elseif {[regexp {Password is now: } $v]} {
				} elseif {[regexp {The address of } $v]} {
					lappend eventList [dict create TYPE EVENT DESC $v]
				} elseif {[regexp {Units will now have a leading sign to show your attitude to them} $v]} {
				} else {
					puts "Error parsing event '$v'"
					exit 1
				}
				set v [getSection $f]
			}
		} else {
			# Events during turn:
			set v [getSection $f]
		}
	}
	dict set turn "Alignment" $alignment
	dict set turn "Events" $eventList
	dict set turn "Items" $itemList
	dict set turn "Skills" $skillList
	dict set turn "Objects" $objList

	# unclaimed silver
	regexp {Unclaimed silver: ([[:digit:]]+)\.} $v -> unclaim
	dict set turn "Unclaimed" $unclaim

	# regions
	set regions ""
	set regionData [getRegion $f]
	while {$regionData ne ""} {
		lappend regions $regionData
		set regionData [getRegion $f]
	}

	# orders template
	# faction number and pass
	set v [split [getSection $f]]
	if {[lindex $v 0] ne "#atlantis"} {
		dict set turn Regions $regions
		return $turn
	}
	dict set turn PlayerNum [lindex $v 1]
	set pass [lindex $v 2]
	if {[string index $pass 0] == "\""} {
		set pass [string range $pass 1 end-1]
	}
	dict set turn PlayerPass $pass

	# orders
	set v [getOrderSection $f]
	set loc [lindex $v 2]
	set xy [string map {( "" ) "" , " "} $loc]

	while {$xy ne ""} {
		set xy [doRegionOrders $f regions $xy]
	}

	# done
	dict set turn Regions $regions
	return $turn
}

}

################
if {!$reader::debug} {
	if {$argc < 1} {
		puts "Usage $argv0 <filename>"
		exit
	}

	foreach fname $argv {
		set f [open $fname]

		set l [join [lrange [file split $fname] end-1 end] "_"]
		set ofile [format {%s%s} "c" $l]

		set chn [open $ofile "w"]
		puts $chn [reader::parseFile $f]

		close $f
		close $chn
	}
}


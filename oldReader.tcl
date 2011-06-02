set ::nextLine ""

proc getSection {f} {
	set ret $::nextLine

	set i1 [regexp -inline -indices {^[ \t]*[^ \t]} $::nextLine]
	set i1 [lindex $i1 0]
	set l [string trimright [gets $f]]

	while {![eof $f]} {
		if {$::nextLine eq ""} {
			append ret $l
			set ::nextLine $l
			set i1 [regexp -inline -indices {^[ \t]*[^ \t]} $::nextLine]
			set i1 [lindex $i1 0]
			set l [gets $f]
			continue
		}

		set i2 [regexp -inline -indices {^[ \t]*[^ \t]} $l]
		set i2 [lindex $i2 0]
		if {$l eq "" || [lindex $i1 1] == [lindex $i2 1]} {
			set ::nextLine $l
			return $ret
		}
		append ret $l

		set l [gets $f]
	}

	return $ret
}

proc searchListOfDict {l i key val} {
	set d [lindex $l $i]
	set v [dict get $d $key]

	return [expr {$v eq $val}]
}

# try and pull a unit's orders (unit is in region xy)
proc doRegionOrders {f regionVar xy} {
	set v [getSection $f]
	if {[eof $f]} { return "" }

	if {[lindex $v 0] ne "unit"} {
		set loc [lindex $v 2]
		set xy [string map {( "" ) "" , " "} $loc]
		return $xy
	}

	# save name
	set nameLine $::nextLine
	set ::nextLine ""

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

	set i 0
	while {![searchListOfDict $regions $i "Location" $xy]} {
		incr i
	}
	set r [lindex $regions $i]

	# try and get name
	regexp {^;(.* \([[:digit:]]+\)), } $nameLine -> unitName

	set units [dict get $r Units]
	set j 0
	while {![searchListOfDict $units $j "Name" $unitName]} {
		incr j
	}

	# put the orders into the unit list
	set u [lindex $units $j]
	dict set u "Orders" $orders
	set units [lreplace $units $j $j $u]

	# update region
	dict set r Units $units
	set regions [lreplace $regions $i $i $r]

	return $xy
}

proc parseRegion {v} {
	set lm [split $v " "]

	# Terrain
	set ret [dict create Terrain [lindex $lm 0]]

	# Location
	set loc [lindex $lm 1]
	regexp {\(([[:digit:]]+),([[:digit:]]+)\)} $loc -> x y
	dict set ret Location [list $x $y]

	# Region
	dict set ret Region [string trimright [lindex $lm 3] ","]

	# Check for town
	set i 4
	set town ""
	if {[lindex $lm $i] eq "contains"} {
		incr i
		set town [lindex $lm $i]

		incr i
		while {![regexp {\[.*\]} [lindex $lm $i]]} {
			append town " " [lindex $lm $i]
			incr i
		}

		set fullType [lindex $lm $i]
		incr i
		set type [string map {\[ "" \] "" , ""} $fullType]
		lappend town $type
	}

	dict set ret Town $town

	# Population
	dict set ret Population [lindex $lm $i]
	incr i
	incr i ;# peasants

	# Race
	set race [lindex $lm $i]
	if {![regexp {\(.*\)} $race]} {
		incr i
		append race " " [lindex $lm $i]
	}
	set raceFinal [string map {\( "" \) "" , ""} $race]
	dict set ret Race $raceFinal
	incr i

	# Max Taxes
	dict set ret MaxTax [string map {\$ "" . ""} [lindex $lm $i]]

	return $ret
}

proc getRegion {f} {
	set v [getSection $f]
	if {$v eq "Orders Template (Long Format):"} {
		return ""
	}
	set region [parseRegion $v]
	set ::nextLine "" ;# clear the -----

	# weather
	set v [getSection $f]
	regexp {was (.*) last month; it will be (.*) next month.} $v -> old new
	dict set region WeatherOld $old
	dict set region WeatherNew $new

	# wages
	set v [getSection $f]
	dict set region Wage    [string map {\$ ""} [lindex $v 1]]
	dict set region MaxWage [string map {\$ "" \) "" . ""} [lindex $v 3]]

	# wants
	set v [getSection $f]
	dict set region Units {}

	# for sale
	set v [getSection $f]

	# entertainment
	set v [getSection $f]

	# products
	set v [getSection $f]

	# exits
	set v [getSection $f]
	set v [string map {\[ "" \] ""} $v]
	set exits [split [lrange $v 1 end] "."]

	set eout ""
	foreach e $exits {
		if {$e eq ""} continue
		lappend eout [lindex $e 0]
		set terrain  [lindex $e 2]
		set loc      [lindex $e 3]
		regexp {\(([[:digit:]]+),([[:digit:]]+)\)} $loc -> x y
		set lxy [list $x $y]

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
	set oldNextLine $::nextLine
	set filePtr [tell $f]
	set v [getSection $f]
	while {[lindex $v 0] == "-" ||
	       [lindex $v 0] == "*" ||
	       [lindex $v 0] == "+"} {

		# skip building reports
		if {[lindex $v 0] != "+"} {
			set quality own
			if {[lindex $v 0] == "-"} {
				set quality foreign
			}
			set comma [string first "," $v]
			set n [string range $v 2 $comma-1]
			set u [dict create Name $n Desc {} Report $quality]
			dict lappend region Units $u
		}

		set oldNextLine $::nextLine
		set filePtr [tell $f]

		set v [getSection $f]
	}

	seek $f $filePtr
	set ::nextLine $oldNextLine

	return $region
}

################
if {$argc != 1} {
	puts "Usage $argv0 <filename>"
	exit
}

set f [open [lindex $argv 0]]

# initial headers
set v [getSection $f]
while {$v ne "Atlantis Report For:"} {
	set v [getSection $f]
}

set v [getSection $f]
while {[lindex $v 0] ne "Neddites"} {
	set v [getSection $f]
}

set v [getSection $f]
puts "Month [string map {"," ""} $v]"

# skip all the events
set v [getSection $f]
while {![regexp {^Unclaimed silver:} $v]} {
	set v [getSection $f]
}

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

# orders
set v [getSection $f]
set loc [lindex $v 2]
set xy [string map {( "" ) "" , " "} $loc]

while {$xy ne ""} {
	set xy [doRegionOrders $f regions $xy]
}

# done
puts "Regions [list $regions]"


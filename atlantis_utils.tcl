package provide atlantis_utils 1.0

### general utilities
proc dGet {d k} {
	if {![dict exists $d $k]} { return "" }

	return [string trim [dict get $d $k]]
}

set ::monthNames {
	January
	February
	March
	April
	May
	June
	July
	August
	September
	October
	November
	December
}

proc calcTurnNo {m y} {
	set mN [lsearch $::monthNames $m]

	return [expr ($y-1)*12 + $mN + 1]
}

# map from product to skill
set ::production {
	GRAI FARM
	LIVE RANC
	FISH FISH
	WOOD LUMB
	IRON MINI
	MITH MINI
	STON QUAR
	HERB HERB
	LASS HERB
	HORS HORS
	CAME CAME
	GEM  GCUT
	PARM ARMO
	CARM ARMO
	AXE  WEAP
	BHAM WEAP
	MSTA WEAP
	MSWO WEAP
	SPEA WEAP
	SWOR WEAP
	XBOW WEAP
	BAG  HERB
	IRWD LUMB
	FUR  HUNT
}

# return list with name and unit id from a string in the form "Name (id)"
proc extractUnitNameNum {full_name {no_error 0}} {
	if {![regexp {([^(]+) \(([[:digit:]]+)\)} $full_name -> unit_name unit_num]} {
		if {!$no_error} {
			puts "Parse error in unit num '$full_name'"
		}
		return [list $full_name]
	}
	return [list $unit_name $unit_num]
}

# return index if current orders contain 'str' (-1 on no match)
# e.g. ordersMatch $ol "tax"
# ordersMatch $ol "produce"
# (useful for reports on keeping in faction limits)
proc ordersMatch {ol str} {
	# handle delayed orders
	set inTurn 0
	set idx -1
	foreach o $ol {
		incr idx
		# if in turn block
		if {$inTurn > 0} {
			# only endturn matters
			if {[string match -nocase "endturn" $o]} {
				incr inTurn -1
			}

			continue
		}

		# look for start of turn block
		if {[regexp -nocase {^@?turn\M} $o]} {
			incr inTurn
			continue
		}

		# check for match
		if {[regexp -nocase "^@? *$str\\M" $o]} {
			return $idx
		}
	}

	# no match
	return -1
}

# return the total number of men in an item list
proc countMen {il} {
	set count 0
	foreach i $il {
		set abbr [string trim [lindex $i 2] {[]}]
		if {[lsearch $::men $abbr] != -1} {
			incr count [lindex $i 0]
		}
	}
	return $count
}

# return a dict with key of product name and 0 for value
proc buildProductDict {maxProducts} {
	set ret ""
	foreach p $maxProducts {
		set key [string trim [lindex $p end] {[]}]
		set val [lindex $p 0]
		dict set ret $key $val
	}
	return $ret
}


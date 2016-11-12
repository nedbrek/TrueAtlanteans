package provide atlantis_utils 1.0

# men will be auto-populated from db
set ::men {
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
	BHAM WEAP
	MSTA WEAP
	MSWO WEAP
	SPEA WEAP
	SWOR WEAP
	XBOW WEAP
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
		if {[regexp -nocase "^@?$str\\M" $o]} {
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


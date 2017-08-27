package require atlantis_dbtools
package require atlantis_reader
package provide client_utils 1.0

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


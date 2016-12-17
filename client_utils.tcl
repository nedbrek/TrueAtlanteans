package require atlantis_dbtools
package require atlantis_reader
package provide client_utils 1.0

proc loadData {filename} {
	set tfile [open $filename]

	updateDb db [reader::parseFile $tfile]
	close $tfile
}


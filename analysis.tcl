lappend ::auto_path [pwd]
package require client_utils

if {$argc < 2} {
	puts "Usage $argv0 <base_dir> <db_dir>"
	exit
}
set base_dir [lindex $argv 0]
set db_dir [lindex $argv 1]

namespace eval gui {
	set currentTurn 0
}

set turns [glob [file join $base_dir "turn.*"]]
foreach t [lsort -dictionary $turns] {
	set turn [regsub {turn.} [file tail $t] ""]
	puts "$turn $t"
	set reports [glob -nocomplain [file join $t "report.*"]]
	foreach r [lsort -dictionary $reports] {
		set report [regsub {report.} [file tail $r] ""]
		if {$report == 1} {
			continue
		}
		set db_file [file join $db_dir [format {p%d.db} $report]]
		set db_cmd "db$report"

		# check if we have opened the db
		if {![info exists dbs($report)]} {
			# if file exists
			if {[glob -nocomplain $db_file] ne ""} {
				# open it
				set err_msg [openDb $db_file]
				if {$err_msg ne ""} {
					puts $err_msg
					exit
				}
			} else {
				# create it
				createDb $db_file
			}
			rename ::db $db_cmd
			set dbs($report) $db_cmd
		}
		rename $dbs($report) ::db
		loadData $r
		rename ::db $dbs($report)

		puts "   $report $r"
	}
}


#!/usr/bin/env tclsh
if {$argc < 3} {
	puts "Usage $argv0 <command> <game_dir> <local_dir>"
	exit 1
}

# process arguments
set cmd [lindex $argv 0]
set game_path [lindex $argv 1]
set local_path [lindex $argv 2]
puts "Running '$cmd' on '$game_path'"

# pull all turns
set turns [glob [file join $game_path "turn*"]]

### commands
proc atl_check {turn_num} {
	set prev_num [expr {$turn_num - 1}]
	set prev_dir [file join $::game_path [subst {turn$prev_num}]]

	set cur_dir [file join $::game_path [subst {turn$turn_num}]]

	# previous game.out to current game.in
	set cmd [list diff [file join $prev_dir "game.out"] [file join $cur_dir "game.in"]]
	puts $cmd
	set out [exec {*}$cmd]

	set reports [glob [file join $cur_dir "report.*"]]
	foreach r $reports {
		set player_num [regsub {report\.} [file tail $r] ""]
		if {$player_num == 1} {
			# GM report
			continue
		}
		# report
		set cmd [list diff $r [file join $::local_path$player_num turn$turn_num.$player_num]]
		puts $cmd
		set out [exec {*}$cmd]

		# orders
		if {$turn_num > 1} {
			set cmd [list diff [file join $::local_path$player_num orders.$turn_num] [regsub {report} $r "orders"]]
			puts $cmd
			set out [exec {*}$cmd]
		}
	}
}

proc atl_regen {turn_num} {
	set cur_dir [file join $::game_path [subst {turn$turn_num}]]
	set reports [glob [file join $cur_dir "report.*"]]
	foreach r $reports {
		set player_num [regsub {report\.} [file tail $r] ""]
		if {$player_num == 1} {
			# GM report
			continue
		}
		set local_dir [format {%s%s} $::local_path $player_num]
		if {$turn_num == 1} {
			puts "delete [file join $local_dir game.db]"
			file delete [file join $local_dir game.db]
			set jmp 4
			if {$player_num == 4} {
				set jmp 0
			}
			puts "tclsh computer_player.tcl new $local_dir max_x 32 max_y 32 jump $jmp"
			exec tclsh computer_player.tcl new $local_dir max_x 32 max_y 32 jump $jmp
		}
		puts "tclsh computer_player.tcl add $local_dir turn$turn_num.$player_num"
		exec tclsh computer_player.tcl add $local_dir turn$turn_num.$player_num
		puts "tclsh computer_player.tcl gen $local_dir"
		set res [exec tclsh computer_player.tcl gen $local_dir]
		if {$res ne ""} { puts $res }
	}
}

proc atl_install {turn_num} {
	set prev_num [expr {$turn_num - 1}]
	set prev_dir [file join $::game_path [subst {turn$prev_num}]]

	set cur_dir [file join $::game_path [subst {turn$turn_num}]]

	# previous game.out to current game.in
	set cmd [list cp [file join $prev_dir "game.out"] [file join $cur_dir "game.in"]]
	puts $cmd
	file copy -force [file join $prev_dir "game.out"] [file join $cur_dir "game.in"]

	set reports [glob [file join $cur_dir "report.*"]]
	foreach r $reports {
		set player_num [regsub {report\.} [file tail $r] ""]
		if {$player_num == 1} {
			# GM report
			continue
		}
		# report
		set cmd [list cp $r [file join $::local_path$player_num turn$turn_num.$player_num]]
		puts $cmd
		file copy -force $r [file join $::local_path$player_num turn$turn_num.$player_num]

		# orders
		if {$turn_num > 1} {
			set cmd [list diff [file join $::local_path$player_num orders.$turn_num] [regsub {report} $r "orders"]]
			puts $cmd
			file copy -force [file join $::local_path$player_num orders.$turn_num] [regsub {report} $r "orders"]
		}
	}
}

### main
foreach t [lsort $turns] {
	set turn_num [regsub {turn} [file tail $t] ""]
	if {$turn_num == 0} {
		# nothing happening
		continue
	}
	atl_$cmd $turn_num
}


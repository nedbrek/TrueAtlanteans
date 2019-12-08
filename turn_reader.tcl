#!/usr/bin/env tclsh
lappend ::auto_path [file dirname $argv0]
package require client_utils

if {$argc < 1} {
	puts "Usage $argv0 <report file>"
	exit 1
}

###
set tfile [open [lindex $argv 0]]
set data [reader::parseFile $tfile]
close $tfile

puts $data


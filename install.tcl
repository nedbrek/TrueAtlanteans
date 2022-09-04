if {![file exists TrueAtlanteans-master.zip]} {
	package require http 2
	package require twapi_crypto
	http::register https 443 [list ::twapi::tls_socket]
	set token [http::geturl https://github.com/nedbrek/TrueAtlanteans/archive/refs/heads/master.zip]
	set ncode [http::ncode $token]
	if {$ncode == 302} {
		set meta [http::meta $token]
		set new_loc [dict get $meta "Location"]
		unset meta
		http::cleanup $token
		set token [http::geturl $new_loc]
	}
	set data [http::data $token]

	set zfile [open TrueAtlanteans-master.zip w]
	fconfigure $zfile -translation binary
	puts -nonewline $zfile $data
	http::cleanup $token
	close $zfile
}
exit


package require sqlite3

wm withdraw .

set ::zoomLevels {
	 3
	 6
	13
	20
	27
	45
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

set ::terrainColors {
	jungle   #205020
	mountain #704018
	ocean    #000090
	plain    #ffffc0
	swamp    #a0a040
}

##############################################################################
proc dGet {d k} {
	return [string trim [dict get $d $k]]
}

proc lGet {l i} {
	return [string trim [lindex $l $i]]
}

proc calcTurnNo {m y} {
	set mN [lsearch $::monthNames $m]

	return [expr ($y-1)*12 + $mN + 1]
}

# use 1,2,rad3 triangles
proc setN {newN} {
	# length of a hexside in pixels
	set ::n $newN
	# n times radical 3
	set ::nrad3 [expr $newN * sqrt(3)]
}
setN 27

# plot one hexagon (flat top, not pointy top) with the upper left vertex at x y
# (NOTE: the left-middle vertex will be left of x)
# place it in canvas obj
# return the item id
proc plot_hex_full {obj x y} {
	global n nrad3

	set hexId [$obj create polygon \
      $x                 $y \
[expr $x + 2 * $n]       $y \
[expr $x + 3 * $n] [expr $y +     $nrad3] \
[expr $x + 2 * $n] [expr $y + 2 * $nrad3] \
      $x           [expr $y + 2 * $nrad3] \
[expr $x -     $n] [expr $y +     $nrad3] \
-outline black -tags hex]

	return $hexId
}

# plot the hex (x,y) (where x and y are integers 0..i)
# adds the tag hex_x_y
proc plot_hex_num {obj x y} {
	global n nrad3
	
	set oddRow [expr $y & 1]
	set yNum [expr $y / 2]
	set yOff 0
	if {$oddRow} {
		set yOff $nrad3
	}

	set hexId [plot_hex_full $obj [expr ($x * 3 * $n) + $n] [expr $yNum * 2 * $nrad3 + $yOff]]

	set tags [$obj itemcget $hexId -tags]
	lappend tags [format "hex_%d_%d" $x $y]
	$obj itemconfigure $hexId -tags $tags
	return $hexId
}

if {0} {
	db eval {SELECT x, y, type FROM terrain} res {
		puts -nonewline "x = $res(x) y = $res(y) "
		puts "type $res(type)"
	}
}

# draw all the regions in the db data
proc drawDB {w db} {
	db eval {SELECT x, y, type FROM terrain} res {
		set hexId [plot_hex_num $w $res(x) $res(y)]

		$w itemconfigure $hexId -fill [dict get $::terrainColors $res(type)]
	}
}

proc updateDb {db tdata} {
	set turnNo [calcTurnNo [dGet $tdata Month] [dGet $tdata Year]]

	set regions [dGet $tdata Regions]
	foreach r $regions {
		set loc [dGet $r Location]
		set x [lindex $loc 0]
		set y [lindex $loc 1]
		set ttype [dGet $r Terrain]
		$db eval {INSERT OR REPLACE INTO terrain VALUES ($x, $y, $ttype);}

		set weather [list [dGet $r WeatherOld] [dGet $r WeatherNew]]
		set wages   [list [dGet $r Wage] [dGet $r MaxWage]]
		set region  [dGet $r Region]
		set city    [dGet $r Town]
		set pop     [dGet $r Population]
		set race    [dGet $r Race]
		set tax     [dGet $r MaxTax]
		$db eval {
			INSERT OR REPLACE INTO detail
			(x, y, turn, weather, wages, region, city, pop, race, tax)
			VALUES(
			$x, $y, $turnNo, $weather, $wages, $region, $city, $pop, $race, $tax
			);
		}

		set exits [dGet $r Exits]
		foreach {d e} $exits {
			set loc [dGet $e Location]
			set x [lindex $loc 0]
			set y [lindex $loc 1]
			set ttype [dGet $e Terrain]
			$db eval {INSERT OR REPLACE INTO terrain VALUES ($x, $y, $ttype);}
		}
	}
}

#	set cx [$w canvasx $x]
#	set cy [$w canvasy $y]
#	set hexId [$w find closest $cx $cy]

proc displayRegion {x y} {
	set t .t.fL.tDesc
	$t delete 1.0 end

	set terrain [db eval {SELECT type FROM terrain WHERE x=$x AND y=$y;}]
	if {$terrain ne ""} {
		$t insert end "$terrain "
	}

	$t insert end "($x,$y)"

	set rdata [db eval {
		SELECT turn, weather, wages, region, city, pop, race, tax FROM detail
		WHERE x=$x and y=$y
		ORDER BY turn DESC LIMIT 1
	}]

	if {[llength $rdata] == 0} { return }

	$t insert end " in [lGet $rdata 3]\n"
	$t insert end "Data from turn: [lGet $rdata 0]\n"

	set city [lGet $rdata 4]
	if {[llength $city]} {
		$t insert end "contains [lGet $city 0]"
		$t insert end " \[[lGet $city 1]\]\n"
	}

	$t insert end "[lGet $rdata 5] peasants "
	$t insert end "([lGet $rdata 6]), \$[lGet $rdata 7].\n"
	$t insert end "------------------------------------\n"
	set weather [lindex $rdata 1]
	$t insert end "The weather was [lGet $weather 0] last month;\n"
	$t insert end "it will be [lGet $weather 1] next month.\n"

	set wages [lindex $rdata 2]
	$t insert end "Wages: \$[lGet $wages 0] (Max: \$[lGet $wages 1]).\n"
}

# process user click on hex
proc hexClick {w x y} {
	# where is the click
	set hexId [$w find withtag current]
	if {$hexId eq ""} {return}

	# what was the old active hex
	set curTags [$w gettags $hexId]

	# was it already active, then done
	set i [lsearch $curTags active]
	if {$i != -1} { return }

	# restore normalcy
	$w itemconfigure active -outline black
	$w itemconfigure active -width 1

	# move "active" tag from old to current
	$w dtag active
	$w addtag active withtag current

	# show active
	$w itemconfigure active -outline red
	$w itemconfigure active -width 4
	$w raise active

	set tags [$w itemcget $hexId -tags]
	set i [lsearch -regexp $tags {hex_[[:digit:]]+_[[:digit:]]}]
	if {$i == -1} { return }

	set hexTag [lindex $tags $i]
	regexp {hex_([[:digit:]]+)_([[:digit:]]+)} $hexTag -> hx hy

	displayRegion $hx $hy
}

proc loadData {filename} {
	set tfile [open $filename]
	set tdata [read $tfile]
	close $tfile

	updateDb db [regsub -all {\n} $tdata " "]
}

##############################################################################
toplevel .t
#bind .t <Destroy> {exit}
wm title .t "True Atlantians"

pack [frame .t.fR] -side right -fill both -expand 1

# need scrollbars to navigate
scrollbar .t.fR.canvasX -command ".t.fR.screen xview" -orient horizontal
scrollbar .t.fR.canvasY -command ".t.fR.screen yview" -orient vertical

set w [canvas .t.fR.screen -bg white -xscrollcommand ".t.fR.canvasX set" \
-yscrollcommand ".t.fR.canvasY set" \
-scrollregion "0 0 4000 6000"]

pack .t.fR.canvasX -side bottom -fill x
pack .t.fR.canvasY -side right  -fill y
pack .t.fR.screen  -side right  -fill both -expand 1

pack [frame .t.fL] -side left -anchor nw
pack [text .t.fL.tDesc -width 40 -height 9] -side top

### bindings
# canvas normally doesn't want focus
bind $w <Enter> {focus %W}
bind .t.fL.tDesc <Enter> {focus %W}

# bind mousewheel to vertical scrolling
bind $w <MouseWheel> {%W yview scroll [expr %D < 0 ? 1 : -1] units}

# bind click
bind $w <1> {hexClick %W %x %y}

########
sqlite3 db "test1.db"
# terrain table: (x, y) -> terrain type
db eval {CREATE TABLE terrain (x not null, y not null, type not null, unique(x,y));}

# detailed table: (x, y) -> turn info gathered, wants?, sells?, weather(cur,
# next) wage(per, max), region, city
db eval {
	CREATE TABLE detail (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		x not null,
		y not null, 
		turn not null,
		weather not null,
		wages not null,
		region not null,
		city not null,
		pop not null,
		race not null,
		tax not null,
		unique(x,y,turn)
	);
}


loadData 1/creport.3
loadData 2/creport.3
loadData 3/creport.3

drawDB $w db


wm withdraw .

set zoomLevels {
	 3
	 6
	13
	20
	27
	45
}

set terrainColors {
	mountain #704018
	ocean    #000090
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
# place it in canvas obj, with options opt
# add the item id to the global hexes array
proc plot_hex_full {obj x y} {
	global n nrad3
	
	set hexId [eval $obj create polygon \
      $x                 $y \
[expr $x + 2 * $n]       $y \
[expr $x + 3 * $n] [expr $y +     $nrad3] \
[expr $x + 2 * $n] [expr $y + 2 * $nrad3] \
      $x           [expr $y + 2 * $nrad3] \
[expr $x -     $n] [expr $y +     $nrad3] \
-outline black -tags hex]

	return $hexId
}

proc plot_hex_num {obj x y} {
	global n nrad3
	
	set oddRow [expr $y & 1]
	set yNum [expr $y / 2]
	set yOff 0
	if {$oddRow} {
		set yOff $nrad3
	}

	set hexId [plot_hex_full $obj [expr ($x * 3 * $n) + $n] [expr $yNum * 2 * $nrad3 + $yOff]]

	$obj itemconfigure $hexId -tags [format "hex_%d_%d" $x $y]
	return $hexId
}

proc drawRegion {w rdata} {
	set hexId [plot_hex_num $w {*}[dict get $rdata Location]]
	
	set ttype  [dict get $rdata Terrain]
	set tcolor [dict get $::terrainColors $ttype]

	$w itemconfigure $hexId -fill $tcolor
}

proc drawMap {w tdata} {
	set regions [dict get $tdata Regions]
	foreach r $regions {
		drawRegion $w $r

		set exits [dict get $r Exits]
		foreach {d e} $exits {
			drawRegion $w $e
		}
	}
}

#	set cx [$w canvasx $x]
#	set cy [$w canvasy $y]
#	set hexId [$w find closest $cx $cy]
proc hexClick {w x y} {
	set hexId [$w find withtag current]
	if {$hexId eq ""} {return}

	set tags [$w itemcget $hexId -tags]
	set i [lsearch -regexp $tags {hex_[[:digit:]]+_[[:digit:]]}]
	if {$i == -1} { return }

	set hexTag [lindex $tags $i]
	regexp {hex_([[:digit:]]+)_([[:digit:]]+)} $hexTag -> hx hy
	puts "Hex $hx $hy"
}

##############################################################################
toplevel .t
#bind .t <Destroy> {exit}
wm title .t "True Atlantians"

# need scrollbars to navigate
scrollbar .t.canvasX -command ".t.screen xview" -orient horizontal
scrollbar .t.canvasY -command ".t.screen yview" -orient vertical

set w [canvas .t.screen -bg white -xscrollcommand ".t.canvasX set" \
-yscrollcommand ".t.canvasY set" \
-scrollregion "0 0 4000 6000"]

pack .t.canvasX -side bottom -fill x
pack .t.canvasY -side right  -fill y
pack .t.screen  -side left   -fill both -expand true

# canvas normally doesn't want focus
bind $w <Enter> {focus %W}
# bind mousewheel to vertical scrolling
bind $w <MouseWheel> {%W yview scroll [expr %D < 0 ? 1 : -1] units}

# bind click
bind $w <1> {hexClick %W %x %y}

set tfile [open creport.3]
set tdata [read $tfile]
close $tfile

drawMap $w $tdata

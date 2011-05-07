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

# draw the hexes described in region data
proc drawRegion {w rdata} {
	set hexId [plot_hex_num $w {*}[dict get $rdata Location]]
	
	set ttype  [dict get $rdata Terrain]
	set tcolor [dict get $::terrainColors $ttype]

	$w itemconfigure $hexId -fill $tcolor
}

# draw all the regions in the turn data
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
pack .t.screen  -side right  -fill both -expand true

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

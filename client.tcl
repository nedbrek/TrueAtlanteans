package require Tk
package require Itcl
lappend ::auto_path [pwd]
package require client_utils

wm withdraw .

### classes
itcl::class Unit {
	public variable name; # not including "(num)"
	public variable num; # just the num
	public variable items; # list of items
	public variable orders
	public variable skills
	public variable flags
	public variable region
	public variable object

	constructor {args} {
		set name   [dGet $args Name]
		set num    [dGet $args Num]
		set items  [dGet $args Items]
		set orders [dGet $args Orders]
		set skills [dGet $args Skills]
		set flags  [dGet $args Flags]
		set region [dGet $args Region]
		set object [dGet $args Object]

		if {$name eq ""} {
			if {[lindex $num 0] eq "new"} {
				set name $num
			} else {
				set name "Unit $num"
			}
		}
	}

	method print {} {
		puts "$name ($num) '$flags' '$skills' '$orders' '$items'"
	}

	# filter instant orders, including "form <id>"/end and "turn/endturn"
	# return new units (created by form)
	method filterInstantOrders {}

	method countItem {abbr} {
		return [::countItem $items $abbr]
	}

	method setItem {abbr ct} {
		set full_abbr [format {[%s]} $abbr]
		for {set i 0} {$i < [llength $items]} {incr i} {
			set il [lindex $items $i]
			if {[lindex $il 2] eq $full_abbr} {
				set il [lreplace $il 0 0 $ct]
				set items [lreplace $items $i $i $il]
				return
			}
		}
		set name [db onecolumn {SELECT name FROM items WHERE abbr=$abbr}]
		lappend items [list $ct $name $full_abbr]
	}
}

### gui constants
set ::zoomLevels {
	 3
	 6
	13
	20
	27
	45
}

set ::terrainColors {
	cavern      #f0d800
	chasm       #d88040
	deepforest  #00c000
	desert      #f0d800
	forest      #00c000
	hill        #a04018
	jungle      #205020
	lake        #0000ff
	mountain    #704018
	mystforest  #004000
	ocean       #000040
	plain       #ffffc0
	swamp       #a0a040
	tunnels     #704018
	tundra      #00ffff
	underforest #00c000
	wasteland   #d88040
	grotto      #d88040
	nexus       #ffffc0
}

### game constants
set ::directions {
	Southeast
	South
	Southwest
	North
	Northeast
	Northwest
}

set ::unitFlags {
	GUARD   g
	AVOID   a
	BEHIND  b
	HOLD    h
	AUTOTAX t
	NOAID   i
	NOCROSS x
	SHARE   s
}

set ::boats {
	Longboat
	Clipper
	Galleon
}

namespace eval gui {
	set currentTurn 0

	set viewLevel 1

	set prevUnit ""
	set prevId   ""

	set rightX 0
	set rightY 0
	set forSaleOpen 0
}

##############################################################################
### general utilities
proc lGet {l i} {
	return [string trim [lindex $l $i]]
}

# return the union of lists 'a' and 'b'
proc lunion {a b} {
	foreach el $a {set ary($el) ""}
	foreach el $b {set ary($el) ""}
	return [array names ary]
}

##############################################################################
### Atlantis specific utilities
proc getZlevel {} {
	set zlevel [expr {$gui::viewLevel - 1}]
	set zList [::db eval {select distinct z from terrain order by cast(z as integer)}]
	if {$zlevel > [llength $zList]} {
		return 1
	}

	return [lindex $zList $zlevel]
}

proc extractUnitNameNum {full_name} {
	if {![regexp {([^(]+) \(([[:digit:]]+)\)} $full_name -> unit_name unit_num]} {
		puts "Parse error in unit num '$full_name'"
		return $full_name
	}
	return [list $unit_name $unit_num]
}

##############################################################################
### drawing
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
-outline darkgray -outlinestipple gray25 -tags hex]

	return $hexId
}

proc drawRoad {dir row col} {
	set w .t.fR.screen
	set x [col2x $col]
	set y [row2y $row]

	switch -nocase $dir {
		n {
			set id [$w create line [expr $x + $::n  ] [expr $y             ] [expr $x + $::n] [expr $y + $::nrad3]]
		}
		s {
			set id [$w create line [expr $x + $::n  ] [expr $y + 2*$::nrad3] [expr $x + $::n] [expr $y + $::nrad3]]
		}
		nw {
			set id [$w create line [expr $x - $::n/2] [expr $y + $::nrad3/2] [expr $x + $::n] [expr $y + $::nrad3]]
		}
		ne {
			set id [$w create line [expr $x + 2.5*$::n] [expr $y + $::nrad3/2] [expr $x + $::n] [expr $y + $::nrad3]]
		}
		sw {
			set id [$w create line [expr $x - $::n/2] [expr $y + 1.5*$::nrad3] [expr $x + $::n] [expr $y + $::nrad3]]
		}
		se {
			set id [$w create line [expr $x + 2.5*$::n] [expr $y + 1.5*$::nrad3] [expr $x + $::n] [expr $y + $::nrad3]]
		}
	}

	$w addtag road withtag $id
	$w addtag icon withtag $id
	$w itemconfigure $id -width 2
}

# calculate the x and y coord of the top-left of the hex
proc col2x {col} {
	return [expr ($col * 3 * $::n) + $::n]
}

proc row2y {row} {
	set oddRow [expr int($row+.5) & 1]
	set yNum   [expr $row / 2]
	set yOff 0
	if {$oddRow} {
		set yOff $::nrad3
	}

	return [expr $yNum * 2 * $::nrad3 + $yOff]
}

# plot the hex (x,y) (where x and y are integers 0..i)
# adds the tag hex_x_y
proc plot_hex_num {obj x y} {
	set hexId [plot_hex_full $obj [col2x $x] [row2y $y]]

	set tags [$obj itemcget $hexId -tags]
	lappend tags [format "hex_%d_%d" $x $y]
	$obj itemconfigure $hexId -tags $tags
	return $hexId
}

proc drawExitWall {w d x y} {
	switch $d {
		South {
			set id [$w create line [expr $x] [expr $y + 2*$::nrad3] [expr $x + 2*$::n] [expr $y + 2*$::nrad3]]
		}

		Southeast {
			set id [$w create line [expr $x + 2*$::n] [expr $y + 2*$::nrad3] [expr $x + 3*$::n] [expr $y + $::nrad3]]
		}

		Southwest {
			set id [$w create line [expr $x - $::n] [expr $y + $::nrad3] [expr $x] [expr $y + 2*$::nrad3]]
		}

		North {
			set id [$w create line [expr $x] [expr $y] [expr $x + 2*$::n] [expr $y]]
		}

		Northeast {
			set id [$w create line [expr $x + 2*$::n] [expr $y] [expr $x + 3*$::n] [expr $y + $::nrad3]]
		}

		Northwest {
			set id [$w create line [expr $x - $::n] [expr $y + $::nrad3] [expr $x] [expr $y]]
		}
	}

	$w addtag wall withtag $id
	$w addtag icon withtag $id
	$w itemconfigure $id -width 4
	$w itemconfigure $id -fill darkgray
}

proc drawNexus {w data} {
	# draw nexus
	foreach {col row type city ct rid exitDirs} $data {

		set hexId [plot_hex_full $w [col2x [expr {$col+1}]] [row2y [expr {$row+3}]]]
		set tags [$w itemcget $hexId -tags]
		lappend tags [format "nexus_%d_%d" $col $row]
		$w itemconfigure $hexId -tags $tags
	}

	# draw exits
	set exitData [::db eval {
		SELECT dir, dest from nexus_exits
	}]

	foreach {d col row} {Northwest 0 2 North 1 1 Northeast 2 2 Southwest 0 4 South 1 5 Southeast 2 4} {
		set loc [dGet $exitData $d]
		set hexId [plot_hex_full $w [col2x $col] [row2y $row]]
		set tags [$w itemcget $hexId -tags]
		set x [lindex $loc 0]
		set y [lindex $loc 1]
		lappend tags [format "hex_%d_%d" $x $y]
		$w itemconfigure $hexId -tags $tags

		# terrain
		set terrain [db eval {SELECT type FROM terrain WHERE x=$x and y=$y and z=1}]
		$w itemconfigure $hexId -fill [dict get $::terrainColors $terrain]
	}

	$w configure -scrollregion [$w bbox all]
}

# draw all the regions in the db data
proc drawDB {w db} {
	$w delete all

	set zlevel [getZlevel]

	set data [$db eval {
			SELECT x, y, type, city, detail.turn, detail.id, detail.exitDirs
			FROM terrain left outer join detail
			USING(x,y,z)
			WHERE z=$zlevel
			GROUP BY terrain.x, terrain.y, terrain.z
			ORDER BY detail.turn
	}]

	if {$zlevel == 0} {
		drawNexus $w $data
		return
	}

	foreach {col row type city ct rid exitDirs} $data {

		if {[info exists drawn($col,$row)]} {continue} 
		set drawn($col,$row) ""

		set hexId [plot_hex_num $w $col $row]

		$w itemconfigure $hexId -fill [dict get $::terrainColors $type]

		set c [$w coords $hexId]
		set x [lindex $c 0]
		set y [lindex $c 1]

		# draw missing exit walls
		if {$exitDirs ne ""} {
			foreach d $::directions {
				if {[lsearch $exitDirs $d] == -1} {
					drawExitWall $w $d $x $y
				}
			}
		}

		# draw city icon
		if {$city ne ""} {
			switch [lGet $city end] {
				village {set cityIcon "-"}
				town    {set cityIcon "+"}
				city    {set cityIcon "*"}
				default {
					puts "Unknown city type: '[lindex $city end]'"
					set cityIcon "?"
				}
			}
			$w create text [expr $x+$::n] [expr $y+$::nrad3] -text $cityIcon -tags icon
		}

		# show unit flags
		if {$ct == $gui::currentTurn} {
			set res [$db eval {
				SELECT detail
				FROM units
				WHERE regionId=$rid
				GROUP BY detail
			}]

			if {[lsearch $res "own"] != -1} {
				$w create text [expr $x] [expr $y+2*$::nrad3] -text "@" \
				  -anchor sw -tags icon
			}
			if {[lsearch $res "foreign"] != -1} {
				$w create text [expr $x+2*$::n] [expr $y+2*$::nrad3] -text "!" \
				  -anchor se -fill red -tags icon
			}

			set res [$db eval {
				SELECT orders
				FROM units
				WHERE regionId=$rid
			}]

			set hasTax  0
			set hasProd 0
			foreach ol $res {
				if {!$hasTax && [ordersMatch $ol "tax"] != -1} {
					set hasTax 1
				}

				if {!$hasProd && [ordersMatch $ol "produce"] != -1} {
					set hasProd 1
				}
				if {!$hasProd && [ordersMatch $ol "build"] != -1} {
					set hasProd 1
				}
			}

			if {$hasTax} {
				$w create text [expr $x-$::n] [expr $y+$::nrad3] -text "\$" \
				  -anchor w -fill darkgreen -tags icon
			}
			if {$hasProd} {
				$w create text [expr $x] [expr $y] -text "P" \
				  -anchor nw -tags icon
			}
		}

		# tag unexplored hexes
		if {$ct eq ""} {
			set hexOverId [plot_hex_num $w $col $row]
			$w itemconfigure $hexOverId -fill gray -stipple gray12
		}

		# pull buildings
		set objects [$db eval {
			SELECT desc FROM objects
			WHERE regionId=$rid
		}]
		set hasOtherBuild 0
		foreach desc $objects {
			if {[regexp -nocase {^Road (.+)} $desc -> dir]} {
				drawRoad $dir $row $col
			} elseif {$desc eq "Shaft"} {
				$w create text [expr $x+2.5*$::n] [expr $y+$::nrad3] -text "H" \
				  -anchor e -tags icon
			} elseif {[lsearch $::boats $desc] != -1} {
				# TODO draw ship icon
			} elseif {!$hasOtherBuild} {
				set hasOtherBuild 1
			}
		}
		if {$hasOtherBuild} {
			$w create text [expr $x+2*$::n] [expr $y] -text "B" \
			  -anchor ne -tags icon
		}
	}

	drawMarkers $w $db

	$w configure -scrollregion [$w bbox all]
}

##############################################################################
### gui
proc copyTree {w} {
	clipboard clear
	set items [$w selection]
	foreach i $items {
		clipboard append "[$w item $i -text]\t[concat [$w item $i -values] \t]\n"
	}
}
bind Treeview <Control-c> {copyTree %W}

proc saveUnitOrders {unit_id w} {
	if {![$w edit modified]} {
		return
	}

	set orders [split [string trimright [$w get 1.0 end]] "\n"]
	db eval {
		UPDATE units SET orders=$orders
		WHERE id=$unit_id
	}
}

proc orderBoxReset {w} {
	if {$gui::prevUnit ne ""} {
		saveUnitOrders $gui::prevId $w
	}

	.t.fL.fItems.t configure -state normal
	.t.fL.fItems.t delete 1.0 end
	.t.fL.fItems.t configure -state disabled

	$w configure -state normal
	$w delete 1.0 end
	$w edit reset
	$w edit modified 0
}

# return the x and y coordinates of the hex under the mouse
proc getSelectionXY {} {
	set tags [.t.fR.screen gettags active]

	set i [lsearch -regexp $tags {hex_[[:digit:]]+_[[:digit:]]}]
	if {$i == -1} {
		# check for nexus
		set i [lsearch -regexp $tags {nexus_[[:digit:]]+_[[:digit:]]}]
		if {$i == -1} { return "" }
		set hexTag [lindex $tags $i]
		regexp {nexus_([[:digit:]]+)_([[:digit:]]+)} $hexTag -> x y

	} else {
		set hexTag [lindex $tags $i]
		regexp {hex_([[:digit:]]+)_([[:digit:]]+)} $hexTag -> x y
	}

	return [list $x $y]
}

# make the unit with 'name' active from the combox
proc showUnit {name} {
	orderBoxReset .t.fL.tOrd

	set w .t.fR.screen

	# retrieve unit in this hex
	## start with regionId
	set xy [getSelectionXY]
	if {$xy eq ""} {return}
	set x [lindex $xy 0]
	set y [lindex $xy 1]

	set zlevel [getZlevel]
	set detail [db eval {
		SELECT id, turn
		FROM detail
		WHERE x=$x AND y=$y AND z=$zlevel
		ORDER BY turn DESC LIMIT 1
	}]

	## got it, use it to retrieve the unit
	set regionId [lindex $detail 0]

	set gui::prevUnit $name

	set data [db eval {
		SELECT orders, id, items, skills, detail, flags, faction, desc
		FROM units
		WHERE regionId=$regionId AND name=$name
		ORDER BY id
	}]
	set orders      [lindex $data 0]
	set gui::prevId [lindex $data 1]
	set items       [lindex $data 2]
	set skills      [lindex $data 3]
	set detail      [lindex $data 4]
	set flags       [lindex $data 5]
	set fact        [lindex $data 6]
	set desc        [lindex $data 7]

	# fill the items box (stick skills in too)
	set t .t.fL.fItems.t
	$t configure -state normal

	$t insert end "Flags: "
	foreach {f l} $::unitFlags {
		set v [dGet $flags $f]
		if {$v eq "1"} {
			$t insert end "$l "
		}
	}
	set v [dGet $flags REVEAL]
	if {$v eq "UNIT"} {
		$t insert end "r "
	} elseif {$v eq "FACTION"} {
		$t insert end "R "
	}
	set v [dGet $flags CONSUME]
	if {$v eq "UNIT"} {
		$t insert end "c"
	} elseif {$v eq "FACTION"} {
		$t insert end "C"
	}
	$t insert end "\n"

	$t insert end "Skills: "
	if {$skills eq ""} {
		$t insert end "<none>\n"
	} else {
		foreach s $skills {
			$t insert end "[join $s]\n"
		}
	}

	if {$detail ne "own"} {
		$t insert end "Faction: '$fact'\n"
	}
	if {$desc ne ""} {
		$t insert end "'$desc'\n"
	}

	$t insert end "-------- [countMen $items] men\n"

	foreach i $items {
		$t insert end "[join $i]\n"
	}
	$t configure -state disabled
	# done with items

	# populate orders box
	foreach o $orders {
		.t.fL.tOrd insert end "$o\n"
	}
	.t.fL.tOrd edit modified 0

	# don't let people modify foreign unit orders
	if {$detail eq "own"} {
		.t.fL.tOrd configure -state normal
	} else {
		.t.fL.tOrd configure -state disabled
	}
}

# update the left frame with all the region details
proc displayRegion {x y nexus} {
	# clean up
	orderBoxReset .t.fL.tOrd

	.t.fL.lProd configure -text ""

	set mt .t.fL.fMarket.tv
	set marketChildren [$mt children {}]
	if {[llength $marketChildren]} {
		set gui::forSaleOpen [$mt item [lindex $marketChildren 0] -open]
	}
	$mt delete $marketChildren

	# clear current unit, in case there is none
	.t.fL.cbMyUnits set ""
	.t.fL.cbMyUnits configure -values ""

	set t .t.fL.tDesc
	$t delete 1.0 end

	# pull region terrain info
	set zlevel [getZlevel]
	if {!$nexus && $zlevel == 0} {set zlevel 1}
	set data [db eval {
		SELECT type, city, region
		FROM terrain
		WHERE x=$x AND y=$y AND z=$zlevel
	}]

	set terrain [lGet $data 0]
	if {$terrain ne ""} {
		$t insert end "$terrain "
	}

	$t insert end "($x,$y) in [lGet $data 2]\n"
	set city [lGet $data 1]
	if {[llength $city]} {
		$t insert end "contains [lGet $city 0]"
		$t insert end " \[[lGet $city 1]\]\n"
	}


	# pull the latest turn data
	set rdata [db eval {
		SELECT turn, weather, wages, pop, race, tax, entertainment, id, products, sells, wants
		FROM detail
		WHERE x=$x AND y=$y AND z=$zlevel
		ORDER BY turn DESC LIMIT 1
	}]

	# if no detail info, done
	if {[llength $rdata] == 0} { return }

	set turn [lindex $rdata 0]
	$t insert end "Data from turn: $turn\n"
	set weather [lindex $rdata 1]
	set wages   [lindex $rdata 2]

	$t insert end "[lGet $rdata 3] peasants "
	$t insert end "([lGet $rdata 4]), \$[lGet $rdata 5].\n"
	$t insert end "------------------------------------\n"
	$t insert end "The weather was [lGet $weather 0] last month;\n"
	$t insert end "it will be [lGet $weather 1] next month.\n"

	$t insert end "Wages: \$[lGet $wages 0] (Max: \$[lGet $wages 1]).\n"
	$t insert end "Entertainment: \$[lindex $rdata 6].\n"

	set regionId [lindex $rdata 7]

	# pull buildings
	set objects [db eval {
		SELECT name, desc FROM objects
		WHERE regionId=$regionId
	}]
	foreach {name desc} $objects {
		$t insert end "$name - $desc\n"
	}

	# region resources for production
	.t.fL.lProd configure -text [join [lindex $rdata 8]]

	# market
	set sells [lindex $rdata 9]
	set wants [lindex $rdata 10]
	if {[llength $sells] == 0} {
		.t.fL.fMarket.tv insert {} 0 -text "Nothing for sale" -open $gui::forSaleOpen
	} else {
		set tvi [.t.fL.fMarket.tv insert {} 0 -text "For sale" -open $gui::forSaleOpen]
	}

	foreach {i c} $sells {
		.t.fL.fMarket.tv insert $tvi end -text "$i @ \$$c"
	}

	if {[llength $wants] == 0} {
		.t.fL.fMarket.tv insert {} end -text "Wanted: nothing"
	} else {
		set tvi [.t.fL.fMarket.tv insert {} end -text "Wanted"]
	}

	foreach {i c} $wants {
		.t.fL.fMarket.tv insert $tvi end -text "$i @ \$$c"
	}

	# unit processing
	set units [db eval {
		SELECT name, detail, faction
		FROM units
		WHERE regionId=$regionId
		ORDER BY detail DESC
	}]

	## set up units combox
	set unitList {}
	set state "start"
	foreach {name detail fact} $units {
		if {$detail eq "own"} {
			## don't show owned units from the past
			if {$turn != $gui::currentTurn} { continue }
			set state "own"
		} elseif {$state ne "start"} {
			set state "start"
			lappend unitList "-----"
		}

		lappend unitList $name
	}

	.t.fL.cbMyUnits configure -values $unitList
	if {[llength $unitList] != 0} {
		.t.fL.cbMyUnits current 0
		showUnit [.t.fL.cbMyUnits get]
	}
}

# find the first of 'ids' with canvas item 'type'
proc findType {w ids type} {
	foreach i $ids {
		if {[$w type $i] eq $type} {
			return $i
		}
	}
	return ""
}

proc makeNormal {w tag} {
	$w itemconfigure $tag -outline darkgray
	$w itemconfigure $tag -outlinestipple gray25
	$w itemconfigure $tag -width 1
}

proc makeNotDone {w tag} {
	$w itemconfigure $tag -outline blue
	$w itemconfigure $tag -outlinestipple ""
	$w itemconfigure $tag -width 2
}

# make region x and y selected in w
proc selectRegion {w x y {nexus 0}} {
	set baseTag "hex_%d_%d"
	if {$nexus} {
		set baseTag "nexus_%d_%d"
	}
	# see if hex is active
	set curTags [$w gettags [format $baseTag $x $y]]
	if {$curTags eq ""} {return}

	set i [lsearch $curTags "active"]
	if {$i != -1} {return}

	# deselect current active
	set oldTags [$w gettags active]
	if {$oldTags ne ""} {
		if {[lsearch $oldTags "notdone"] != -1} {
			# not done hex
			makeNotDone $w active
		} else {
			# normal hex
			makeNormal $w active
		}
	}

	# move active tag
	$w dtag active
	$w addtag active withtag [format $baseTag $x $y]

	# show active
	$w itemconfigure active -outline red
	$w itemconfigure active -outlinestipple ""
	$w itemconfigure active -width 4
	$w raise active
	$w raise icon

	displayRegion $x $y $nexus
}

proc arrow {w dir} {
	set xy [getSelectionXY]
	if {$xy eq ""} {return}
	set x [lindex $xy 0]
	set y [lindex $xy 1]

	switch $dir {
		up { incr y -2 }
		dn { incr y  2 }

		ul { incr x -1; incr y -1 }
		ur { incr x  1; incr y -1 }

		lr { incr x 1; incr y 1 }
		ll { incr x -1; incr y 1 }

		lt {
			incr x -1

			if {$x & 1} {
				incr y -1
			} else {
				incr y 1
			}
		}

		rt {
			incr x

			if {$x & 1} {
				incr y -1
			} else {
				incr y 1
			}
		}
	}

	# wrap around
	set maxX [::db eval { SELECT max(cast(x as integer)) FROM terrain }]
	if {$x == -1} { set x $maxX }
	if {$x > $maxX} { set x 0 }

	set curTags [$w gettags [format "hex_%d_%d" $x $y]]
	if {$curTags eq ""} { return }

	selectRegion $w $x $y
	centerHex $w $x $y
}

# process user click on hex
proc hexClick {w x y} {
	# where is the click
	set hexId [$w find withtag current]
	if {$hexId eq ""} {return}

	# what if the use clicks on a icon
	if {[$w type $hexId] ne "polygon"} {
		# find the hex with the icon
		set c [$w bbox $hexId]
		set x1 [expr [lindex $c 0]+2]
		set y1 [expr [lindex $c 1]+2]
		set x2 [expr [lindex $c 2]-2]
		set y2 [expr [lindex $c 3]-2]
		set hexes [$w find overlapping $x1 $y1 $x2 $y2]
		set hexId [findType $w $hexes "polygon"]
		if {$hexId eq ""} {
			return
		}
	}

	# was it already active, then done
	set tags [$w gettags $hexId]

	set i [lsearch $tags "active"]
	if {$i != -1} { return }

	set i [lsearch -regexp $tags {hex_[[:digit:]]+_[[:digit:]]}]
	if {$i == -1} {
		set i [lsearch -regexp $tags {nexus_[[:digit:]]+_[[:digit:]]}]
		if {$i != -1} {
			set hexTag [lindex $tags $i]
			regexp {nexus_([[:digit:]]+)_([[:digit:]]+)} $hexTag -> hx hy

			selectRegion $w $hx $hy 1
		}
		return
	}

	set hexTag [lindex $tags $i]
	regexp {hex_([[:digit:]]+)_([[:digit:]]+)} $hexTag -> hx hy

	selectRegion $w $hx $hy
}

# only switch focus if the widgets are part of the same toplevel
proc switchFocus {w} {
	set curFocus [focus]
	if {$curFocus eq ""} {return}

	set sf [split $curFocus "."]
	set sw [split $w        "."]

	if {[lindex $sf 1] eq [lindex $sw 1]} {
		focus $w
	}
}

##############################################################################
### menu callbacks
# helper for "Add Report"
proc dnLevel {} {
	incr gui::viewLevel
	drawDB .t.fR.screen db
}

proc upLevel {} {
	if {$gui::viewLevel > 1} {
		incr gui::viewLevel -1
		drawDB .t.fR.screen db
	}
}

proc zoomIn {} {
	set i [lsearch $::zoomLevels $::n]
	if {$i == -1 || $i+1 == [llength $::zoomLevels]} {return}

	incr i
	setN [lindex $::zoomLevels $i]
	drawDB .t.fR.screen db
}

proc zoomOut {} {
	set i [lsearch $::zoomLevels $::n]
	if {$i == -1 || $i == 0} {return}

	incr i -1
	setN [lindex $::zoomLevels $i]
	drawDB .t.fR.screen db
}

proc newGame {} {
	set types {
		{{Game Database} {.db}}
		{{All Files} *}
	}
	set ofile [tk_getSaveFile -filetypes $types]
	if {$ofile eq ""} { return }

	wm title .t "True Atlanteans - [file tail $ofile]"

	createDb $ofile
	.t.fR.screen delete all
}

proc doOpen {} {
	set gui::prevUnit ""
	set gui::prevId   ""

	set types {
		{{Game Database} {.db}}
		{{All Files} *}
	}
	set ofile [tk_getOpenFile -filetypes $types]
	if {$ofile eq ""} { return }

	set errMsg [openDb $ofile]
	if {$errMsg ne ""} {
		tk_messageBox -message $errMsg
	}

	set ::men [db eval {select abbr from items where type="race"}]

	set gui::currentTurn [db eval {select max(turn) from detail}]
	set gui::viewLevel 1

	wm title .t "True Atlanteans - [file tail $ofile] Turn $gui::currentTurn"

	# pull settings from db
	set res [db eval {
		SELECT geom_top, zoom_level, view_level, forSale_open
		FROM settings WHERE id=1
	}]

	foreach {geom zoom view forSale} $res {
		wm geometry .t $geom
		setN [lindex $::zoomLevels $zoom]
		set gui::viewLevel $view
		set gui::forSaleOpen $forSale
	}

	drawDB .t.fR.screen db
}

proc doAdd {} {
	set ofiles [tk_getOpenFile -multiple 1]
	if {$ofiles eq ""} { return }

	foreach f $ofiles {
		loadData $f
	}

	set ::men [db eval {select abbr from items where type="race"}]

	set gui::currentTurn [db eval {select max(turn) from detail}]
	set txt [wm title .t]
	wm title .t "True Atlanteans - [lindex $txt 3] Turn $gui::currentTurn"

	drawDB .t.fR.screen db
}

proc drawMarkers {w db} {
	set zlevel [getZlevel]
	set res [$db eval {
		SELECT x,y,done FROM active_markers
		WHERE z=$zlevel
	}]

	foreach {x y done} $res {
		if {$done} {continue}

		$w addtag notdone withtag [format "hex_%d_%d" $x $y]
	}

	makeNotDone $w notdone
}

proc clearNotDone {w x y} {
	# update db
	set zlevel [getZlevel]
	::db eval {
		UPDATE active_markers SET done=1
		WHERE x=$x AND y=$y AND z=$zlevel
	}

	# delete the tag
	set hexTag [format "hex_%d_%d" $x $y]
	$w dtag $hexTag notdone

	# update the map
	set curTags [$w gettags $hexTag]
	if {[lsearch $curTags "active"] == -1} {
		makeNormal $w $hexTag
	}
}

# mark all the hexes where we currently have units
proc markActive {} {
	# forget the past
	::db eval {DROP TABLE active_markers}

	# create the marker table
	::db eval {
		CREATE TABLE active_markers(
			x TEXT not null,
			y TEXT not null,
			z TEXT not null,
			done not null,
			  unique(x,y,z)
		)
	}

	# populate it with coordinates where we have units right now
	::db eval {
		INSERT INTO active_markers
		(x,y,z,done)
			SELECT detail.x, detail.y, detail.z, 0
			FROM detail JOIN units
			ON detail.id=units.regionId
			WHERE detail.turn=$gui::currentTurn AND units.detail='own'
			GROUP BY detail.x, detail.y, detail.z
	}

	drawMarkers .t.fR.screen db
}

proc clearNotDoneCur {w} {
	set xy [getSelectionXY]
	if {$xy eq ""} {return}
	set x [lindex $xy 0]
	set y [lindex $xy 1]

	clearNotDone $w $x $y
}

# calculate the number of men needed to fully tax a hex
proc calcTaxers {} {
	set xy [getSelectionXY]
	if {$xy eq ""} {return}
	set x [lindex $xy 0]
	set y [lindex $xy 1]

	set zlevel [getZlevel]
	set maxTax [::db eval {
		SELECT tax
		FROM detail
		WHERE x=$x AND y=$y AND z=$zlevel
		ORDER BY turn DESC LIMIT 1
	}]
	tk_messageBox -message "[expr ($maxTax+49)/50] taxmen"
}

proc selectUnitFromList {w} {
	set i [lindex [$w curselection] 0]
	set str [$w get $i]
	regexp {^(.+) \(([[:digit:]]+),([[:digit:]]+),?([[:digit:]]*)\)$} \
	  $str -> name x y z
	set zlevel [getZlevel]
	if {$zlevel ne $z} {
		if {$z eq ""} {set z 1}
		set gui::viewLevel $z
		drawDB .t.fR.screen db
	}
	selectRegion .t.fR.screen $x $y
	.t.fL.cbMyUnits set $name
	showUnit $name
}

proc makeUnitListbox {t title res} {
	if {![winfo exists $t]} {
		toplevel $t
		wm title $t $title
		pack [frame $t.fTop] -side top

		scrollbar $t.fTop.vs -command "$t.fTop.tl yview"

		pack [listbox $t.fTop.tl -width 40 -height 40 \
-yscrollcommand "$t.fTop.vs set"] -side left -expand 1 -fill both

		pack $t.fTop.vs -side left -fill y

		bind $t.fTop.tl <Double-1> {selectUnitFromList %W}
	}

	$t.fTop.tl delete 0 end
	foreach {n x y z} $res {
		if {$z eq ""} {
			$t.fTop.tl insert end "$n ($x,$y)"
		} else {
			$t.fTop.tl insert end "$n ($x,$y,$z)"
		}
	}
}

proc findForeignUnits {} {
	set res [db eval {
		SELECT units.name, detail.x, detail.y, detail.z
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn AND units.detail<>'own'
	}]

	makeUnitListbox .tForeignUnits "Foreign Units" $res
}

proc findIdleUnits {} {
	set res [db eval {
		SELECT units.name, detail.x, detail.y, detail.z
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn AND units.detail='own'
		   AND units.orders=''
	}]

	makeUnitListbox .tIdleUnits "Idle Units" $res
}

proc selectRegionFromList {w} {
	set i [lindex [$w curselection] 0]
	set str [$w get $i]
	regexp {^\(([[:digit:]]+),([[:digit:]]+),?([[:digit:]]*)\) } \
	  $str -> x y z

	set zlevel [getZlevel]
	if {$zlevel ne $z} {
		if {$z eq ""} {set z 1}
		set gui::viewLevel $z
		drawDB .t.fR.screen db
	}

	selectRegion .t.fR.screen $x $y
}

proc reportTax {} {
	set res [::db eval {
		SELECT x,y,z,curTax(id,tax) as ct,tax
		FROM detail
		WHERE turn=$gui::currentTurn AND ct > 0
		ORDER BY ct DESC
	}]

	set t .tTaxRegions

	if {![winfo exists $t]} {
		toplevel $t
		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tl yview"

		pack [listbox $t.fTop.tl -width 40 -height 40 \
-yscrollcommand "$t.fTop.vs set"] -side left -expand 1 -fill both

		pack $t.fTop.vs -side left -fill y

		bind $t.fTop.tl <Double-1> {selectRegionFromList %W}
	} else {
		$t.fTop.tl delete 0 end
	}

	set totalTax 0
	foreach {x y z tx max} $res {
		incr totalTax $tx
		set delta [expr $max - $tx]
		if {$z eq ""} {
			$t.fTop.tl insert end "($x,$y) - $tx ($max - $delta)"
		} else {
			$t.fTop.tl insert end "($x,$y,$z) - $tx ($max - $delta)"
		}
	}

	wm title $t "Tax Report: $totalTax ([expr [llength $res]/5] regions)"
}

proc safeLsortIdxS {col a b} {
	set la [lindex $a $col]
	set lb [lindex $b $col]
	if {$la eq ""} {
		return [expr {$lb eq "" ? 0 : -1}]
	}
	if {$lb eq ""} {
		return 1
	}
	set c [string compare $la $lb]
	if {$c == 0} {
		set la1 [lindex $a $col+1]
		set lb1 [lindex $b $col+1]
		if {$la1 eq "" || $lb1 eq ""} {
			return 0
		}
		return [expr {$la1 - $lb1}]
	}
	return $c
}

proc safeLsortIdxI {col a b} {
	set la [lindex $a $col]
	set lb [lindex $b $col]
	if {$la eq ""} {
		return [expr {$lb eq "" ? 0 : -1}]
	}
	if {$lb eq ""} {
		return 1
	}
	return [expr $la - $lb]
}

proc sortProdList {tv col isInt} {

	set childList [$tv children {}]
	foreach i $childList {
		set terrain [$tv item $i -text]
		lappend vals [list $terrain {*}[$tv item $i -values]]
	}

	set command safeLsortIdxI
	if {!$isInt} {
		set command safeLsortIdxS
	}
	lappend command $col

	set vals2 [lsort -decreasing -command $command $vals]

	$tv delete [$tv children {}]
	foreach v $vals2 {
		$tv insert {} end -text [lindex $v 0] -values [lrange $v 1 end]
	}
}

# build the resource report window
proc reportResources {} {
	# pull all production values
	set res [db eval {
		SELECT x,y,z,curProduce(id, products) as cp
		FROM detail
		WHERE turn=$gui::currentTurn
	}]

	# build the window
	set t .tRegionResources

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Resource Report"
		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.tv -side left -expand 1 -fill both
	}

	# populate it
	# (every hex has a variable number of production columns)
	$t.fTop.tv delete [$t.fTop.tv children {}]
	set maxCol 3
	foreach {x y z prod} $res {
		set terrain [db eval {SELECT type from terrain
			WHERE x=$x AND y=$y AND z=$z}]
		$t.fTop.tv insert {} end -text $terrain -values [list $x $y $z {*}$prod]
		set maxCol [expr max($maxCol, 3+[llength $prod])]
	}

	# configure all the columns
	set cols ""
	for {set i 1} {$i <= $maxCol} {incr i} { lappend cols $i }
	$t.fTop.tv configure -columns $cols

	# x,y,z
	$t.fTop.tv column 1 -width 34
	$t.fTop.tv column 2 -width 34
	$t.fTop.tv column 3 -width 34
	$t.fTop.tv heading 1 -text "x"
	$t.fTop.tv heading 2 -text "y"
	$t.fTop.tv heading 3 -text "z"

	# production items
	for {set i 4} {$i <= $maxCol} {incr i} {
		$t.fTop.tv column $i -width 52
		# allow sorting
		$t.fTop.tv heading $i -command [list sortProdList $t.fTop.tv $i [expr $i&1]]
	}
}

proc searchUnits {} {
	# build the window
	set t .tSearchUnits

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Find Units"
		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.tv -side left -expand 1 -fill both
	}
	$t.fTop.tv delete [$t.fTop.tv children {}]

	# configure all the columns
	set cols ""
	for {set i 1} {$i <= 3} {incr i} { lappend cols $i }
	$t.fTop.tv configure -columns $cols

	$t.fTop.tv column 1 -width 34
	$t.fTop.tv column 2 -width 34
	$t.fTop.tv column 3 -width 34
	$t.fTop.tv heading 1 -text "x"
	$t.fTop.tv heading 2 -text "y"
	$t.fTop.tv heading 3 -text "z"

	# populate it
	# TODO allow user to edit name
	set res [getUnits "Courier %"]
	foreach {x y z name} $res {
		$t.fTop.tv insert {} end -text $name -values [list $x $y $z]
	}

	# allow sorting
	$t.fTop.tv heading #0 -command [list sortProdList $t.fTop.tv 0 0]
	for {set i 1} {$i <= 3} {incr i} {
		$t.fTop.tv heading $i -command [list sortProdList $t.fTop.tv $i 1]
	}
}

proc itemView {} {
	# build the window
	set t .tSearchItems

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Item Viewer"
		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.tv -side left -expand 1 -fill both
	}
	$t.fTop.tv delete [$t.fTop.tv children {}]

	# configure all the columns
	set cols ""
	for {set i 1} {$i <= 4} {incr i} { lappend cols $i }
	$t.fTop.tv configure -columns $cols

	$t.fTop.tv column 1 -width 65
	$t.fTop.tv column 2 -width 79
	$t.fTop.tv column 3 -width 34
	$t.fTop.tv column #0 -stretch 0
	$t.fTop.tv column 1 -stretch 0
	$t.fTop.tv column 2 -stretch 0
	$t.fTop.tv column 3 -stretch 0
	$t.fTop.tv heading 1 -text "Abbr"
	$t.fTop.tv heading 2 -text "Type"
	$t.fTop.tv heading 3 -text "Wt"
	$t.fTop.tv heading 4 -text "Desc"

	# populate it
	set res [db eval {
		SELECT name, abbr, type, desc
		FROM items
	}]
	foreach {name abbr type desc} $res {
		set wt [dGet $desc Weight]
		set d1 [dGet $desc Desc]
		if {$type eq "item" || $type eq "race"} {
			$t.fTop.tv insert {} end -text $name -values [list $abbr $type $wt $d1]
		} else {
			set id [$t.fTop.tv insert {} end -text $name -values [list $abbr $type $wt [lindex $d1 1]]]
			for {set i 0} {$i < [llength $d1]} {incr i} {
				if {$i == 1} { continue }
				$t.fTop.tv insert $id end -text "" -values [list "" "" "" [lindex $d1 $i]]
			}
		}
	}

	# allow sorting
	$t.fTop.tv heading #0 -command [list sortProdList $t.fTop.tv 0 0]
	for {set i 1} {$i <= 4} {incr i} {
		$t.fTop.tv heading $i -command [list sortProdList $t.fTop.tv $i [expr {$i == 3}]]
	}
}

# return number of hexes where production is underway
proc ctProd {} {
	set res [db eval {
		SELECT units.name, units.orders, detail.x, detail.y, detail.z
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn AND units.detail='own'
	}]

	foreach {u ol x y z} $res {
		if {[ordersMatch $ol "produce"] != -1} {
			set prod($x,$y,$z) 1
			continue
		}

		if {[ordersMatch $ol "build"] != -1} {
			set prod($x,$y,$z) 1
			continue
		}
	}

	return [llength [array names prod]]
}

proc checkBool {s} {
	return [expr {$s != 0 && $s != 1}]
}

proc checkOrder {u o x y z ctxt} {
	if {$o eq ""} {
		return 0
	}

	set op [split $o " "]
	set c [lindex $op 0]
	switch -nocase $c {
		declare { 
			# change diplomatic stance
			# TODO use a dialog
			return 0
		}

		faction {
			# change faction type
			# TODO use a dialog
			return 0
		}

		password {
			# change password
			# TODO use a dialog
			return 0
		}

		restart {
			# start over
			# TODO use a dialog
			return 0
		}

		quit {
			# delete faction
			# TODO use a dialog
			return 0
		}

		show {
			# get description again
			# TODO use a dialog
			return 0
		}

		name {
			# change name of something
			# TODO check args
			return 0
		}

		describe {
			# TODO check args
			return 0
		}

		cast {
			# cast a spell
			# TODO check args
			return 0
		}

		combat {
			# set combat spell
			# TODO check args
			return 0
		}

		claim {
			# pull funds from faction bank
			set amt [lindex $op 1]
			# TODO check against unclaimed amount
			set cur_silv [$u countItem SILV]
			$u setItem SILV [expr {$cur_silv + $amt}]
			return 0
		}

		work {
			# earn wages
			return 0
		}

		entertain {
			# gather more income
			return 0
		}

		give {
			# give an item
			set units [dGet $ctxt Units]

			## check receiver
			set recv [lindex $op 1]
			set i 2

			if {$recv == 0} {
				set recv_obj ""
			} else {
				if {$recv eq "new"} {
					set recv "new [lindex $op 2]"
					incr i
				}
				if {![dict exists $units $recv]} {
					return [list -1 "Give with invalid receiver ('$o')"]
				}
				set recv_obj [dict get $units $recv]
			}

			set item_id [string toupper [lindex $op $i+1]]
			if {$item_id eq ""} {
				return [list -1 "Give needs item ('$o')"]
			}

			# check inventory
			set cur_ct [$u countItem $item_id]
			set ct [lindex $op $i]
			if {$ct eq "all"} {
				set ct $cur_ct
			}

			if {$cur_ct < $ct} {
				return [list -1 "Give more than they own ($cur_ct < $ct $item_id)"]
			}

			# execute
			$u setItem $item_id [expr {$cur_ct - $ct}]
			if {$recv_obj ne ""} {
				set recv_ct [$recv_obj countItem $item_id]
				$recv_obj setItem $item_id [expr {$recv_ct + $ct}]
			}

			return 0
		}

		promote {
			# change control of building
			# TODO check args
			return 0
		}

		exchange {
			# swap item
			# TODO check args
			return 0
		}

		end {
			# end of form
			return [list -1 "End without Form"]
		}

		endturn {
			# end of turn directive
			return [list -1 "Endturn without Turn"]
		}

		study {
			# improve skill
			# check arg
			set skill_name [lindex $op 1]
			set skill_cost [db onecolumn {
				SELECT cost
				FROM skills
				WHERE level = 1 and (abbr like $skill_name or name like $skill_name)
			}]
			if {$skill_cost eq ""} {
				puts "Warning: no skill cost for $skill_name"
				return 0
			}
			set men [countMen [$u cget -items]]
			set cost [expr {$skill_cost * $men}]

			set cur_silv [$u countItem SILV]
			if {$cur_silv < $cost} {
				return [list -1 "STUDY: not enough funds"]
			}

			$u setItem SILV [expr {$cur_silv - $cost}]
			return 0
		}

		teach {
			# help another unit study
			# TODO check args
			return 0
		}

		forget {
			# get ready for a new skill
			# TODO check args
			return 0
		}

		move {
			# change hex
			# TODO check args
			return 0
		}

		sail {
			# change hex over water
			# TODO check args
			return 0
		}

		enter {
			# go into a building/object
			# TODO check args
			return 0
		}

		leave {
			# leave current object
			return 0
		}

		advance {
			# move with aggression
			# TODO check args
			return 0
		}

		attack {
			# start a fight with given unit
			# TODO check args
			return 0
		}

		evict {
			# kick unit out of object
			# TODO check args
			return 0
		}

		assassinate {
			# secretly attack
			# TODO check args
			return 0
		}

		steal {
			# try to take an item
			# TODO check args
			return 0
		}

		destroy {
			# tear down an object
			# TODO check args
			return 0
		}

		tax {
			# get money from residents
			return 0
		}

		pillage {
			# degrade hex for cash
			return 0
		}

		buy {
			# get items for cash
			# TODO check args
			return 0
		}

		sell {
			# give up item for cash
			# TODO check args
			return 0
		}

		produce {
			# gather natural resources, or refine them into goods
			# TODO check args
			return 0
		}

		build {
			# construct an object
			# TODO check args
			return 0
		}
	}
	return [list -1 "Command '$c' not recognized"]
}

proc cleanOrder {o} {
	# strip whitespace
	set o [string trim $o]

	if {[string index $o 0] eq "@"} {
		# repeat
		set o [string trimleft $o "@"]

		if {[string is digit [string index $o 0]]} {
			# @n <order>
			set o [regsub {[[:digit:]]+} $o ""]
		} else {
			# it was a simple repeat
		}

		set o [string trim $o]
	}

	return $o
}

proc getUnitObjects {id} {
	set res [::db eval {
		SELECT x, y, z
		FROM detail
		WHERE id=$id
	}]
	foreach {x y z} $res {}
	set region "$x $y $z"

	set ret [list]
	set units [::db eval {
		SELECT name, orders, items, skills, flags
		FROM units
		WHERE units.regionId=$id AND units.detail='own'
	}]
	foreach {full_name orders items skills flags} $units {
		foreach {name num} [extractUnitNameNum $full_name] {}

		lappend ret [Unit #auto \
		    Name $name Num $num Items $items Orders $orders Skills $skills Flags $flags Region $region Object ""
		]
	}

	return $ret
}

itcl::body Unit::filterInstantOrders {} {
	set bool_flags {
		avoid
		hold
		behind
		noaid
		guard
		nocross
		autotax
	}
	set enum_flags {
		consume
		reveal
	}
	set enum_flag_vals {
		""
		unit
		faction
	}

	foreach {x y z} $region {}

	set new_units [list]

	set new_orders [list]
	set skip 0
	for {set i 0} {$i < [llength $orders]} {incr i} {
		set o [cleanOrder [lindex $orders $i]]
		set cmd [string tolower [lindex $o 0]]

		if {$skip} {
			if {$cmd eq "endturn"} {
				set skip 0
			}
			continue
		}

		if {$cmd eq "turn"} {
			set skip 1
			continue
		}

		# handle bool flags
		if {[lsearch $bool_flags $cmd] != -1} {
			set arg [lindex $o 1]
			if {[checkBool $arg]} {
				puts "$name ($x, $y, $z) $cmd bad argument $arg"
			}
			# TODO: apply flags
			continue
		}

		# enum flags
		if {[lsearch $enum_flags $cmd] != -1} {
			set arg [lindex $o 1]
			if {[lsearch -nocase $enum_flag_vals $arg] == -1} {
				puts "$name ($x, $y, $z) $cmd bad argument $arg"
			}
			# TODO: apply flags
			continue
		}

		# TODO spoils
		if {$cmd eq "spoils"} {
			continue
		}

		# anything but form
		if {![regexp {^form +(.*)} $o -> new_id]} {
			lappend new_orders $o
			continue
		}

		set new_o [list]
		for {incr i} {$i < [llength $orders]} {incr i} {
			set o [lindex $orders $i]
			if {[regexp { *end *$} $o]} {
				break
			}
			lappend new_o $o
		}

		set new_unit [itcl::code [Unit #auto Num "new $new_id" Orders $new_o Flags $flags Region $region Object $object]]
		lappend new_units $new_unit

		set tmp [$new_unit filterInstantOrders]
		if {$tmp ne ""} {
			puts "Error: nested FORM will not work [$u cget -name] ($x, $y, $z)"
			itcl:delete object $tmp
		}
	}

	set orders $new_orders
	return $new_units
}

proc checkOrderType {tgt_ord x y z ctxt} {
	set ret [list]
	set unit_map [dict get $ctxt Units]
	dict for {u v} $unit_map {
		set ol [$v cget -orders]
		if {$ol eq ""} continue

		set il [$v cget -items]

		set new_orders [list]

		# foreach order
		foreach o $ol {

			set o [cleanOrder $o]

			set op [split $o " "]
			set c [string tolower [lindex $op 0]]
			if {$tgt_ord ne "" && $tgt_ord ne $c} {
				lappend new_orders $o
				continue
			}

			set r [checkOrder $v $o $x $y $z $ctxt]
			set rc [lindex $r 0]
			if {$rc < 0} {
				lappend ret [format "$u ($x, $y, $z) %s" [lindex $r 1]]
			} elseif {$rc == 2} {
				# endturn without turn
				lappend ret [format "$u ($x, $y, $z) EndTurn without Turn"]
			} elseif {$rc == 1} {
				puts "$u ($x, $y, $z) Turn should have been handled"
			}
		}
		$v configure -orders $new_orders
	}
	return $ret
}

proc checkAllOrders {} {
	# pull all hexes that we have details for
	set res [::db eval {
		SELECT DISTINCT id, x, y, z
		FROM detail
		WHERE turn=$gui::currentTurn
	}]

	set ret [list]
	# foreach hex
	foreach {id x y z} $res {
		set units [getUnitObjects $id]
		if {$units eq ""} { continue }

		set new_units [list]
		foreach u $units {
			set lret [$u filterInstantOrders]
			if {$lret ne ""} {
				lappend new_units {*}$lret
			}
		}
		if {$new_units ne ""} {
			lappend units {*}$new_units
		}

		# stash unit ids
		set unit_map [dict create]
		foreach u $units {
			dict set unit_map [$u cget -num] $u
		}

		set ctxt [dict create]
		dict set ctxt Units $unit_map

		# run claim/give before other orders
		lappend ret {*}[checkOrderType "claim" $x $y $z $ctxt]
		lappend ret {*}[checkOrderType "give" $x $y $z $ctxt]
		lappend ret {*}[checkOrderType "" $x $y $z $ctxt]

		if {$units ne ""} {
			itcl::delete object {*}$units
		}
	}
	if {$ret ne ""} {
		tk_messageBox -message [join $ret "\n"]
	}
	return $ret
}

proc saveOrders {} {
	set filename [format {orders%d.txt} $gui::currentTurn]
	set ofile [tk_getSaveFile -initialfile $filename ]
	if {$ofile eq ""} { return }

	set f [open $ofile "w"]

	set pid [::db eval { SELECT player_id FROM settings }]
	set ppass [::db eval { SELECT player_pass FROM settings }]
	if {$ppass eq ""} {
		puts $f "#atlantis $pid"
	} else {
		puts $f "#atlantis $pid \"$ppass\""
	}

	set res [::db eval {
		SELECT units.name, units.orders, detail.x, detail.y, detail.z
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn AND units.detail='own'
		ORDER BY detail.z, detail.x, detail.y
	}]

	set loc ""
	foreach {u ol x y z} $res {
		if {$ol eq ""} continue

		set newLoc [list $x $y $z]
		if {$loc eq "" || $loc ne $newLoc} {
			set loc $newLoc
			puts $f ";*** $x $y $z ***"
		}

		regexp {\(([[:digit:]]+)\)} $u -> unitNum
		puts $f "unit $unitNum"
		puts $f "; $u"
		puts $f "[join $ol "\n"]\n"
	}

	puts $f "#end"
	close $f
}

proc rightCenter {} {
	recenter .t.fR.screen $gui::rightX $gui::rightY
}

proc keyCenter {w} {
	set xy [getSelectionXY]
	if {$xy eq ""} { return }

	centerHex $w [lindex $xy 0] [lindex $xy 1]
}

proc centerHex {w x y} {
	lassign [$w bbox [format "hex_%d_%d" $x $y]] x1 y1 x2 y2

	centerCanvas $w [expr {($x1+$x2)/2.}] [expr {($y1+$y2)/2.}]
}

proc recenter {w sx sy} {

	# convert screen coords to canvas coords
	set cx [$w canvasx $sx]
	set cy [$w canvasy $sy]
	centerCanvas $w $cx $cy
}

proc centerCanvas {w cx cy} {
	lassign [$w cget -scrollregion] xmin ymin xmax ymax
	lassign [$w yview] top btm
	lassign [$w xview] left right

	set xpos [expr {$cx / $xmax - ($right - $left) / 2.0}]
	set ypos [expr {$cy / $ymax - ($btm - $top)    / 2.0}]

	$w xview moveto $xpos
	$w yview moveto $ypos
}

proc showAllUnits {} {
	set units [db eval {
		SELECT detail.x, detail.y, detail.z, units.name, units.items, units.orders
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn and units.detail='own'
	}]

	# build the window
	set t .tAllUnits

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "All Units"
		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.tv -side left -expand 1 -fill both
	}

	# clear old contents
	$t.fTop.tv delete [$t.fTop.tv children {}]

	# populate it

	# configure all the columns
	set cols ""
	for {set i 1} {$i <= 6} {incr i} { lappend cols $i }
	$t.fTop.tv configure -columns $cols

	# x,y,z
	$t.fTop.tv column  1 -width 34; $t.fTop.tv column  2 -width 34; $t.fTop.tv column  3 -width 34
	$t.fTop.tv heading 1 -text "x"; $t.fTop.tv heading 2 -text "y"; $t.fTop.tv heading 3 -text "z"

	$t.fTop.tv heading 4 -text "men"
	$t.fTop.tv heading 5 -text "silv"
	$t.fTop.tv heading 6 -text "orders"

	foreach {x y z name items orders} $units {
		if {![info exists id($x,$y,$z)]} {
			set id($x,$y,$z) [$t.fTop.tv insert {} end -text "Terrain" -values [list $x $y $z "" "" ""]]
			$t.fTop.tv item $id($x,$y,$z) -open 1
		}
		$t.fTop.tv insert $id($x,$y,$z) end -text $name -values [list $x $y $z [countMen $items] [countItem $items SILV] $orders]
	}
}

proc formTaxers {regionId} {
	# get units in hex
	set rdata [db eval {
		SELECT name, orders, items, flags
		FROM units
		WHERE regionId=$regionId AND detail="own"
	}]

	# look over all units
	set totalSilver 0
	set numTaxers 0
	foreach {name orders items flags} $rdata {
		set silver [lsearch -inline $items "* silver *"]
		if {$silver ne ""} {
			incr totalSilver [lindex $silver 0]
		}
		if {[ordersMatch $orders "tax"] != -1} {
			incr numTaxers [countMen $items]
		}
	}
	if {$totalSilver == 0} { return 1 }

	set maxTax [db eval {
		SELECT tax FROM detail WHERE id=$regionId ORDER BY turn DESC LIMIT 1
	}]

	set taxersNeeded [expr {($maxTax - $numTaxers * 50) / 50}]
	if {$taxersNeeded == 0} { return 1 }

	# limit by cash on hand
	set rdata [db onecolumn {
		SELECT sells
		FROM detail
		WHERE id=$regionId AND turn=$gui::currentTurn
	}]
	set price [lindex $rdata 1]
	# TODO configure maintenance cost
	set maxBuy [expr {$totalSilver / ($price + 10)}]
	return [expr {min($taxersNeeded, $maxBuy)}]
}

proc formUnit {} {
	# pull current hex info
	set xy [getSelectionXY]
	if {$xy eq ""} { return }
	set x [lindex $xy 0]
	set y [lindex $xy 1]
	set zlevel [getZlevel]

	## get most recent details
	set rdata [db eval {
		SELECT id, turn, sells
		FROM detail
		WHERE x=$x AND y=$y AND z=$zlevel
		ORDER BY turn DESC LIMIT 1
	}]
	if {[llength $rdata] == 0} { return }

	set regionId [lindex $rdata 0]
	set turn [lindex $rdata 1]
	set sells [lindex $rdata 2]
	if {$turn != $gui::currentTurn} { return }

	set peasants [db eval {
		SELECT DISTINCT race FROM detail
	}]

	set maxRace 0
	set raceList [list]
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
	}
	if {$maxRace == 0} { return }

	# get units in hex
	set rdata [db eval {
		SELECT name, orders, flags
		FROM units
		WHERE regionId=$regionId AND detail="own"
	}]

	set maxAlias 0
	set unitList [list]
	set unitOrders [list]
	set unitFlags [list]
	foreach {name orders flags} $rdata {
		lappend unitList $name
		lappend unitOrders $orders
		lappend unitFlags $flags

		foreach o $orders {
			if {[regexp -nocase "^@?form\\M" $o]} {
				set maxAlias [expr {max($maxAlias, [lindex $o 1])}]
			}
		}
	}

	# build the window
	set t .tFormUnit

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Create New Unit"
		pack [frame $t.fTop] -side top -expand 1 -fill both

		#grid [label $t.fTop.lParent -text "Parent"] -row 0 -column 0
		#grid [ttk::combobox $t.fTop.cbParent -state readonly] -row 0 -column 1 -sticky we

		grid [label $t.fTop.lAlias -text "Alias"] -row 1 -column 0
		grid [ttk::spinbox $t.fTop.sAlias -from 1 -to 100] -row 1 -column 1 -sticky we

		grid [label $t.fTop.lName -text "Name"] -row 2 -column 0
		grid [entry $t.fTop.eName] -row 2 -column 1 -sticky we

		grid [label $t.fTop.lRace -text "Race"] -row 3 -column 0
		grid [ttk::combobox $t.fTop.cbRaces -state readonly] -row 3 -column 1 -sticky we

		grid [label $t.fTop.lCt -text "Count"] -row 4 -column 0
		grid [ttk::spinbox $t.fTop.sCt -from 0] -row 4 -column 1 -sticky we

		grid [label $t.fTop.lOrders -text "Orders"] -row 5 -columnspan 2
		grid [text $t.fTop.orders -height 24 -width 42] -row 6 -columnspan 2 -sticky nswe

		grid columnconfigure $t.fTop 1 -weight 1

		pack [frame $t.fButtons] -side bottom
		pack [button $t.fButtons.bOk -text "Ok" -command [list finishForm $t]] -side left
		pack [button $t.fButtons.bCancel -text "Cancel" -command [list destroy $t]] -side right
	}

	#$t.fTop.cbParent configure -values $unitList
	#$t.fTop.cbParent current 0
	$t.fTop.sAlias set [expr {$maxAlias + 1}]
	$t.fTop.cbRaces configure -values $raceList
	$t.fTop.cbRaces current 0
	$t.fTop.sCt configure -to $maxRace
	$t.fTop.sCt set [formTaxers $regionId]
}

proc finishForm {t} {
	.t.fL.tOrd insert end "\nform [$t.fTop.sAlias get]\n"

	set name [$t.fTop.eName get]
	if {$name ne ""} {
		.t.fL.tOrd insert end "name unit \"$name\"\n"
	}

	set ct [$t.fTop.sCt get]
	if {$ct > 0} {
		set race [$t.fTop.cbRaces get]
		regexp {\[(.+)\]} $race -> abbr
		.t.fL.tOrd insert end "buy $ct $abbr\n"
	}

	set orders [$t.fTop.orders get 1.0 end]
	if {$orders ne ""} {
		.t.fL.tOrd insert end "$orders\n"
	}
	.t.fL.tOrd insert end "end\n"

	# TODO use parent listbox
	if {$gui::prevUnit ne ""} {
		saveUnitOrders $gui::prevId .t.fL.tOrd
	}

	destroy $t
}

proc loadGlob {patt} {
	set files [glob $patt]
	foreach f $files {
		loadData $f
	}

	set gui::currentTurn [db eval {select max(turn) from detail}]
	drawDB .t.fR.screen db
}

rename exit origExit
proc exit {} {
	if {[info commands ::db] ne ""} {
		set geom [winfo geometry .t]
		set i [lsearch $::zoomLevels $::n]
		::db eval {
			UPDATE settings SET
			geom_top = $geom,
			zoom_level = $i,
			view_level = $gui::viewLevel,
			forSale_open = $gui::forSaleOpen
			WHERE id=1
		}
		::db close
	}
	origExit
}

##############################################################################
### build the GUI
toplevel .t
bind .t <Destroy> {exit}
wm title .t "True Atlanteans - <no game open>"

### top menu
menu .mTopMenu -tearoff 0
menu .mTopMenu.mFile -tearoff 0
menu .mTopMenu.mView -tearoff 0
menu .mTopMenu.mReports -tearoff 0

.mTopMenu add cascade -label "File" -menu .mTopMenu.mFile -underline 0
.mTopMenu add cascade -label "View" -menu .mTopMenu.mView -underline 0
.mTopMenu add cascade -label "Reports" -menu .mTopMenu.mReports -underline 0

# file menu
.mTopMenu.mFile add command -label "New"         -command newGame -underline 0 -accelerator "Ctrl+N"
.mTopMenu.mFile add command -label "Open"        -command doOpen  -underline 0 -accelerator "Ctrl+O"
.mTopMenu.mFile add command -label "Add Report"  -command doAdd   -underline 0
.mTopMenu.mFile add command -label "Save Orders" -command saveOrders -underline 0
.mTopMenu.mFile add separator
.mTopMenu.mFile add command -label "Exit"        -command exit    -underline 1 -accelerator "Ctrl+Q"

# view menu
.mTopMenu.mView add command -label "Mark active hexes" -command markActive -underline 0
.mTopMenu.mView add command -label "Find units" -command searchUnits -underline 0
.mTopMenu.mView add command -label "Items" -command itemView -underline 0

# reports menu
.mTopMenu.mReports add command -label "Idle Units" -command findIdleUnits -underline 0
.mTopMenu.mReports add command -label "Foreign Units" -command findForeignUnits -underline 0
.mTopMenu.mReports add command -label "Resources" -command reportResources -underline 0
.mTopMenu.mReports add command -label "Taxers" -command reportTax -underline 0

.t configure -menu .mTopMenu

### right frame
pack [frame .t.fR] -side right -fill both -expand 1

# need scrollbars to navigate
scrollbar .t.fR.canvasX -command ".t.fR.screen xview" -orient horizontal
scrollbar .t.fR.canvasY -command ".t.fR.screen yview" -orient vertical

# main canvas
set w [canvas .t.fR.screen -bg white -xscrollcommand ".t.fR.canvasX set" \
-yscrollcommand ".t.fR.canvasY set" \
-scrollregion "0 0 4000 6000"]

# right-click menu
menu .mRight -tearoff 0
.mRight add command -label "Center" -command {rightCenter}
.mRight add separator
.mRight add command -label "Down Level" -command {dnLevel}
.mRight add command -label "Up Level" -command {upLevel}
.mRight add separator
.mRight add command -label "Calc Taxers" -command {calcTaxers}

pack .t.fR.canvasX -side bottom -fill x
pack .t.fR.canvasY -side right  -fill y
pack .t.fR.screen  -side right  -fill both -expand 1

### left frame
pack [frame .t.fL] -side left -anchor nw

# top, region description
pack [text .t.fL.tDesc -width 42 -height 9] -side top

pack [label .t.fL.lProd -wraplength 300] -side top

pack [frame .t.fL.fMarket] -side top -expand 1 -fill x
pack [ttk::treeview .t.fL.fMarket.tv -show tree \
-yscrollcommand ".t.fL.fMarket.vs set"] -side left -expand 1 -fill x
pack [scrollbar .t.fL.fMarket.vs -command ".t.fL.fMarket.tv yview" \
-orient vertical] -side left -fill y

# next, unit combobox
pack [ttk::combobox .t.fL.cbMyUnits -state readonly -width 45] -side top
if {$tcl_platform(os) eq "Linux"} {
	# for some reason, the combox is much wider on Linux
	.t.fL.cbMyUnits configure -width 36
}

# next, unit items (text + scrollbar)
pack [frame .t.fL.fItems] -side top
pack [text .t.fL.fItems.t -width 40 -height 10 -state disabled \
-yscrollcommand ".t.fL.fItems.vs set"] -side left

pack [scrollbar .t.fL.fItems.vs -command ".t.fL.fItems.t yview" \
-orient vertical] -side left -fill y

# next, orders box
pack [text .t.fL.tOrd -width 42 -height 9 -undo 1] -side top

### bindings
## canvas
# canvas normally doesn't want focus
bind $w <Enter> {switchFocus %W}
bind .t.fL.tDesc <Enter> {switchFocus %W}
bind .t.fL.tOrd  <Enter> {switchFocus %W}

# bind mousewheel to vertical scrolling
bind $w <MouseWheel> {%W yview scroll [expr %D < 0 ? 1 : -1] units}

# bind click
bind $w <1> {hexClick %W %x %y}
bind $w <Double-1> {recenter %W %x %y}
bind $w <3> {
	set gui::rightX %x
	set gui::rightY %y
	.mRight post %X %Y
}

# bind zoom keys
bind $w <minus>       zoomOut
bind $w <KP_Subtract> zoomOut
bind $w <plus>        zoomIn
bind $w <KP_Add>      zoomIn

# directional movement
bind $w <KP_Up> {arrow %W up}
bind $w <Up> {arrow %W up}
bind $w <KP_Home> {arrow %W ul}
bind $w <KP_Prior> {arrow %W ur}
bind $w <Home> {arrow %W ul}
bind $w <Prior> {arrow %W ur}
bind $w <KP_Left> {arrow %W lt}
bind $w <KP_Right> {arrow %W rt}
bind $w <Left> {arrow %W lt}
bind $w <Right> {arrow %W rt}
bind $w <KP_End> {arrow %W ll}
bind $w <KP_Next> {arrow %W lr}
bind $w <End> {arrow %W ll}
bind $w <Next> {arrow %W lr}
bind $w <KP_Down> {arrow %W dn}
bind $w <Down> {arrow %W dn}

bind $w <F5> {drawDB %W db} ;# refresh

bind $w <d> {clearNotDoneCur %W}
bind $w <c> {keyCenter %W}
bind $w <n> {formUnit}

## orders
# update orders on unit dropdown change
bind .t.fL.cbMyUnits <<ComboboxSelected>> {showUnit [%W get]}

# redo should be default on Windows, but needed on Linux
bind .t.fL.tOrd <Control-y> {%W edit redo}


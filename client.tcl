#!/usr/bin/env wish
package require Tk
lappend ::auto_path [file dirname $argv0]
package require client_utils

wm withdraw .

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
	mystforest  #008000
	ocean       #000040
	plain       #ffffc0
	swamp       #a0a040
	tunnels     #704018
	tundra      #00ffff
	underforest #00c000
	wasteland   #d88040
	grotto      #d88040
	nexus       #ffffc0
	volcano     #c40c0c
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

namespace eval gui {

	set unitFlags {
		GUARD   g
		AVOID   a
		BEHIND  b
		HOLD    h
		AUTOTAX t
		NOAID   i
		NOCROSS x
		SHARE   s
	}

	set viewLevel 1
	set draw_all 1

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

proc saveWindow {db t children} {
	set settings [$db onecolumn {
		SELECT val FROM gui WHERE name="WINDOWS"
	}]

	set geom [winfo geometry $t]

	set child_vals [dict create]
	foreach {w type} $children {
		if {$type eq "TREEVIEW"} {
			set cols [$w configure -columns]
			set cols [lindex $cols end]

			set widths [list]
			lappend widths [$w column #0 -width]
			foreach c $cols {
				lappend widths [$w column $c -width]
			}
			dict set child_vals $w [dict create TYPE "TREEVIEW" VALS $widths]
		} else {
			puts "Unknown type '$type'"
		}
	}

	set values [dict create GEOM $geom CHILDREN $child_vals]
	dict set settings $t $values

	$db eval {
		UPDATE gui SET
		name = "WINDOWS",
		val = $settings
	}
	destroy $t
}

##############################################################################
### Atlantis specific utilities
# z levels can be discontiguous (e.g. 1,2,10,20,30)
proc getZlevel {} {
	set zList [::db eval {select distinct z from terrain order by cast(z as integer)}]
	if {$gui::viewLevel > [llength $zList]} {
		set gui::viewLevel 0
	}

	return [lindex $zList $gui::viewLevel]
}

proc setZlevel {new_z} {
	set zList [::db eval {select distinct z from terrain order by cast(z as integer)}]
	set max [tcl::mathfunc::max {*}$zList]
	if {$new_z > $max} {
		set new_z $max
	}
	set gui::viewLevel [lsearch $zList $new_z]
}

proc unitFlagsStr {flags} {
	set ret [list]
	foreach {f l} $gui::unitFlags {
		set v [dGet $flags $f]
		if {$v eq "1"} {
			lappend ret $l
		}
	}

	set v [dGet $flags REVEAL]
	if {$v eq "UNIT"} {
		lappend ret "r"
	} elseif {$v eq "FACTION"} {
		lappend ret "R"
	}

	set v [dGet $flags CONSUME]
	if {$v eq "UNIT"} {
		lappend ret "c"
	} elseif {$v eq "FACTION"} {
		lappend ret "C"
	}

	return $ret
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

proc drawRoad {dir x y} {
	set w .t.fR.screen

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
	lappend tags [format "hex_%d_%d" $x $y] "pos_hex"
	$obj itemconfigure $hexId -tags $tags
	return $hexId
}

# column and row are the original values
proc plot_left_neg_hex {obj col row} {
	# shift everything over half a screen
	set half_x [expr {$::max_x / 2}]
	set off [expr {$half_x * 3 - 1}]
	# come back where column == half_x would be 0,0
	set x [expr {(($col - $half_x) * 3 - $off) * $::n}]
	# y is normal
	set y [row2y $row]

	# plot the hext
	set hex_id [plot_hex_full $obj $x $y]

	# tag it with original x,y
	set tags [$obj itemcget $hex_id -tags]
	lappend tags [format "hex_%d_%d" $col $row] "neg_hex"
	$obj itemconfigure $hex_id -tags $tags
	return $hex_id
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
	if {$exitData eq ""} {
		$w configure -scrollregion [$w bbox all]
		return
	}

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

proc decorateHex {w db hexId data_row} {
	foreach {col row type city ct rid exitDirs} $data_row {}

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
	if {$ct == $::currentTurn} {
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

	# draw a stipple over unexplored hexes
	if {$ct eq ""} {
		set coords [$w coords $hexId]
		set hexOverId [$w create polygon $coords -tags [list hex [format "hex_%d_%d" $col $row]]]
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
			drawRoad $dir $x $y
		} elseif {$desc eq "Shaft"} {
			$w create text [expr $x+2.5*$::n] [expr $y+$::nrad3] -text "H" \
			  -anchor e -tags icon
		} elseif {[lsearch $::boats $desc] != -1} {
			# TODO draw ship icon
		} elseif {!$hasOtherBuild} {
			set hasOtherBuild 1
		}
	}
	if {$ct == $::currentTurn && $hasOtherBuild} {
		$w create text [expr $x+2*$::n] [expr $y] -text "B" \
		  -anchor ne -tags icon
	}
}

# draw all the regions in the db data
proc drawDB {w db} {
	$w delete all

	set zlevel [getZlevel]

	set data [$db eval {
			SELECT x, y, turn, id, exitDirs
			FROM detail a
			WHERE z=$zlevel AND turn = (
			    SELECT max(turn) FROM detail
			    WHERE x=a.x AND y=a.y AND z=a.z
			)
	}]

	if {$zlevel == 0} {
		drawNexus $w $data
		return
	}

	set half_x [expr {$::max_x / 2}]
	foreach {col row ct rid exitDirs} $data {
		set terrain_data [db eval {
			SELECT type, city FROM terrain
			WHERE x=$col AND y=$row AND z=$zlevel
		}]
		foreach {type city} $terrain_data {}

		set data_row [list $col $row $type $city $ct $rid $exitDirs]

		set hex_id [plot_hex_num $w $col $row]
		if {$ct == $::currentTurn && ![info exists decorate($col,$row)]} {
			decorateHex $w $db $hex_id $data_row
			set decorate($col,$row) ""
			set drawn($col,$row) ""

			if {$col >= $half_x} {
				set hex_id [plot_left_neg_hex $w $col $row]
				decorateHex $w $db $hex_id $data_row
			}
		}

		if {[info exists drawn($col,$row)]} {continue}
		set drawn($col,$row) ""

		decorateHex $w $db $hex_id $data_row

		if {$col >= $half_x} {
			set hex_id [plot_left_neg_hex $w $col $row]
			decorateHex $w $db $hex_id $data_row
		}
	}

	set data [$db eval {
			SELECT x, y, type, city FROM terrain
			WHERE z=$zlevel
	}]
	foreach {col row type city} $data {
		if {[info exists drawn($col,$row)]} {continue}
		set drawn($col,$row) ""

		set hex_id [plot_hex_num $w $col $row]
		set data_row [list $col $row $type $city "" "" ""]
		decorateHex $w $db $hex_id $data_row

		if {$col >= $half_x} {
			set hex_id [plot_left_neg_hex $w $col $row]
			decorateHex $w $db $hex_id $data_row
		}
	}

	if {$gui::draw_all} {
		for {set y 0} {$y < $::max_y} {incr y 2} {
			for {set x 0} {$x < $::max_x} {incr x} {
				set col $x
				set row [expr {($x & 1) ? $y + 1 : $y}]
				if {[info exists drawn($col,$row)]} {continue}
				set drawn($col,$row) ""

				set hexId [plot_hex_num $w $col $row]
			}
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
bind Treeview <Home> {%W see {}}

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
		set gui::prevUnit ""
	}

	.t.fItems.t configure -state normal
	.t.fItems.t delete 1.0 end
	.t.fItems.t configure -state disabled

	.t.tvItems delete [.t.tvItems children {}]

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

proc showBuilding {t regionId name} {
	set name [string range $name 2 end]

	set obj [db eval {
		SELECT id FROM objects
		WHERE regionId=$regionId and name=$name
	}]
	if {$obj eq ""} {
		return
	}

	set total_wt 0
	set units [list]
	db eval {
		SELECT id, uid
		FROM units
		WHERE regionId=$regionId
	} {
		set obj_id [db onecolumn {
			SELECT objectId
			FROM object_unit_map
			WHERE unitId=$id
		}]
		if {$obj_id eq $obj} {
			lappend units $uid
		}
	}

	set total_wt 0
	set total_men 0
	foreach u $units {
		set items [db onecolumn {
			SELECT items
			FROM units
			WHERE regionId=$regionId AND uid=$u
		}]
		incr total_wt [weighItems $items]
		incr total_men [countMen $items]
	}
	$t insert end "Total men: $total_men\n"
	$t insert end "Total weight: $total_wt\n"
}

# make the unit with 'name' active from the combox
proc showUnit {name} {
	orderBoxReset .t.tOrd

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
	set is_current [expr {[lindex $detail 1] == $::currentTurn}]

	set gui::prevUnit $name

	set r [extractUnitNameNum $name 1]
	set just_name [lindex $r 0]
	set uid [lindex $r 1]

	set t .t.fItems.t
	$t configure -state normal

	if {$uid eq ""} {
		# it's a building
		showBuilding $t $regionId $name
		.t.tOrd configure -state disabled
		$t configure -state disabled
		return
	}

	set data [db eval {
		SELECT orders, id, items, skills, detail, flags, faction, desc
		FROM units
		WHERE regionId=$regionId AND uid=$uid
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

	if {$is_current} {
		set data [db eval {
			SELECT type, val
			FROM events
			WHERE (type="ERROR" or type="EVENT") and dGet(val, "UNIT") = $uid
		}]
		foreach {tp v} $data {
			if {$tp eq "ERROR"} {
				$t insert end "Error: "
			}
			$t insert end "[dGet $v DESC]\n"
		}
	}

	### capacities
	set cap_types [list walking riding flying swimming]
	foreach tp $cap_types {
		set cap($tp) 0
	}

	set skip 0
	set total_wt 0
	set missing [list]
	foreach i $items {
		set ct [lindex $i 0]
		set full_abbr [lindex $i 2]
		if {[regexp {\[(.*)\]} $i -> abbr]} {
			if {$abbr eq "SILV"} {
				set wt   0
				set lcap ""
			} else {
				set data [db onecolumn {SELECT desc FROM items WHERE abbr=$abbr}]
				set wt   [dGet $data Weight]
				set lcap [dGet $data Capacity]
				if {$wt eq ""} {
					set skip 1
					lappend missing $abbr
					continue
				}
			}

			incr total_wt [expr {$ct * $wt}]

			foreach tp $cap_types {
				set v [dGet $lcap $tp]
				if {$v ne ""} {
					incr cap($tp) [expr {$v * $ct}]
				} else {
					incr cap($tp) [expr {-$ct * $wt}]
				}
			}
		} else {
			error "Item with no abbr $i"
		}
	}

	$t insert end "Total weight: $total_wt\n"

	set any_cap 0
	foreach tp {walking riding flying swimming} {
		if {$cap($tp) >= 0} {
			$t insert end "$tp capacity: $cap($tp)\n"
			set any_cap 1
		}
	}
	if {!$any_cap} {
		$t insert end "Cannot move $cap(walking)\n"
	}

	if {$skip} {
		$t insert end "Cannot retrieve weights for: [join $missing ","]\n"
	}

	$t insert end "Flags: "
	$t insert end [join [unitFlagsStr $flags] " "]
	$t insert end "\n"

	set v [dGet $flags SPOILS]
	if {$v eq "NONE"} {
		$t insert end "No spoils\n"
	} elseif {$v eq "WALK"} {
		$t insert end "Walking spoils\n"
	} elseif {$v eq "RIDE"} {
		$t insert end "Riding spoils\n"
	} elseif {$v eq "FLY"} {
		$t insert end "Flying spoils\n"
	}

	# fill the items box (stick skills in too)
	set tv .t.tvItems

	set par [$tv insert {} end -text "Skills" -open 1]
	if {$skills eq ""} {
		$tv insert $par end -text "<none>"
	} else {
		foreach s $skills {
			$tv insert $par end -text "[join $s]"
		}
	}

	if {$detail ne "own"} {
		$t insert end "Faction: '$fact'\n"
	}
	if {$desc ne ""} {
		$t insert end "'$desc'\n"
	}

	set other [db onecolumn {SELECT other FROM units WHERE id=$gui::prevId}]
	set combat_spell [dGet $other CombatSpell]
	set can_study    [dGet $other CanStudy]

	if {$combat_spell ne ""} {
		$tv insert {} end -text "Combat spell: $combat_spell"
	}

	if {$can_study ne ""} {
		set par [$tv insert {} end -text "Can Study"]
		foreach sk $can_study {
			$tv insert $par end -text $sk
		}
	}

	$t insert end "-------- [countMen $items] men\n"

	foreach i $items {
		$t insert end "[join $i]\n"
	}
	$t configure -state disabled
	# done with items

	# populate orders box
	foreach o $orders {
		.t.tOrd insert end "$o\n"
	}
	.t.tOrd edit modified 0

	# don't let people modify foreign unit orders
	if {$detail eq "own"} {
		.t.tOrd configure -state normal
	} else {
		.t.tOrd configure -state disabled
	}
}

# update the left frame with all the region details
proc displayRegion {x y nexus} {
	# clean up
	orderBoxReset .t.tOrd

	set mt .t.fMarket.tv
	set marketChildren [$mt children {}]
	if {[llength $marketChildren]} {
		set gui::forSaleOpen [$mt item [lindex $marketChildren 0] -open]
	}
	$mt delete $marketChildren

	# clear current unit, in case there is none
	.t.cbMyUnits set ""
	.t.cbMyUnits configure -values ""

	set t .t.tDesc
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
	if {$turn == $::currentTurn} {
		$t insert end "Data is current\n"
	} else {
		$t insert end "Data from turn: $turn\n"
	}
	set weather [lindex $rdata 1]
	set wages   [lindex $rdata 2]

	$t insert end "[lGet $rdata 3] peasants "
	$t insert end "([lGet $rdata 4]), \$[lGet $rdata 5].\n"
	$t insert end "------------------------------------\n"
	set cur_weather [lGet $weather 0]
	if {$cur_weather ne ""} {
		$t insert end "The weather was $cur_weather last month;\n"
		$t insert end "it will be [lGet $weather 1] next month.\n"
	}

	$t insert end "Wages: \$[lGet $wages 0] (Max: \$[lGet $wages 1]).\n"
	$t insert end "Entertainment: \$[lindex $rdata 6].\n"

	set regionId [lindex $rdata 7]

	# pull buildings
	array set objects ""
	db eval {
		SELECT id, name, desc, flags FROM objects
		WHERE regionId=$regionId
	} {
		$t insert end [format "%s - %s %s\n" $name $desc [join $flags ","]]
		set objects($id) [dict create NAME $name]
	}

	# region resources for production
	set resources [lindex $rdata 8]
	if {$resources ne ""} {
		set tvi [.t.fMarket.tv insert {} 0 -text "Resources" -open 1]
		foreach r $resources {
			.t.fMarket.tv insert $tvi end -text $r
		}
	}

	# market
	set sells [lindex $rdata 9]
	set wants [lindex $rdata 10]
	if {[llength $sells] == 0} {
		.t.fMarket.tv insert {} end -text "Nothing for sale" -open $gui::forSaleOpen
	} else {
		set tvi [.t.fMarket.tv insert {} end -text "For sale" -open $gui::forSaleOpen]
	}

	foreach {i c} $sells {
		.t.fMarket.tv insert $tvi end -text "$i @ \$$c"
	}

	if {[llength $wants] > 0 && [lindex $wants 0] ne "none"} {
		set tvi [.t.fMarket.tv insert {} end -text "Wanted"]
		foreach {i c} $wants {
			.t.fMarket.tv insert $tvi end -text "$i @ \$$c"
		}
	}

	set gate [db onecolumn {
		SELECT desc
		FROM gates
		WHERE x=$x AND y=$y AND z=$zlevel
	}]
	if {$gate ne ""} {
		$t insert end "There is a Gate here ($gate)\n"
	}

	# unit processing
	set units [db eval {
		SELECT id, name, uid, detail, faction
		FROM units
		WHERE regionId=$regionId
		ORDER BY detail DESC
	}]

	## set up units combox
	set unitList {}
	set state "start"
	foreach {id name uid detail fact} $units {
		set obj_id [db onecolumn {
			SELECT objectId
			FROM object_unit_map
			WHERE unitId=$id
		}]
		set unit_entry [format {%s (%d)} $name $uid]
		if {$obj_id ne ""} {
			dict lappend objects($obj_id) UNITS $unit_entry
			continue
		}

		if {$detail eq "own"} {
			## don't show owned units from the past
			if {$turn != $::currentTurn} { continue }
			set state "own"
		} elseif {$state ne "start"} {
			set state "start"
			lappend unitList "-----"
		}

		lappend unitList $unit_entry
	}

	set prev_k ""
	foreach {k v} [array get objects] {
		set obj_units [dGet $v UNITS]
		if {$obj_units ne "" && $k ne $prev_k} {
			lappend unitList [format {+ %s} [dGet $v NAME]]
			set prev_k $k
		}
		foreach u $obj_units {
			lappend unitList $u
		}
	}

	.t.cbMyUnits configure -values $unitList
	if {[llength $unitList] != 0} {
		.t.cbMyUnits current 0
		showUnit [.t.cbMyUnits get]
		if {[llength $unitList] == 1} {
			.t.cbMyUnits configure -state disabled
		} else {
			.t.cbMyUnits configure -state readonly
		}
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
	if {$gui::viewLevel > 0} {
		incr gui::viewLevel -1
		drawDB .t.fR.screen db
	}
}

proc zoomIn {} {
	set i [lsearch $::zoomLevels $::n]
	if {$i == -1 || $i+1 == [llength $::zoomLevels]} {return}

	set xy [getSelectionXY]
	if {$xy ne ""} {
		foreach {x y} $xy {}
	}

	# increase zoom level
	incr i
	setN [lindex $::zoomLevels $i]
	drawDB .t.fR.screen db

	if {$xy ne ""} {
		# reselect active
		selectRegion .t.fR.screen $x $y
		centerHex .t.fR.screen $x $y
	}
}

proc zoomOut {} {
	set i [lsearch $::zoomLevels $::n]
	if {$i == -1 || $i == 0} {return}

	set xy [getSelectionXY]
	if {$xy ne ""} {
		foreach {x y} $xy {}
	}

	# decrease zoom level
	incr i -1
	setN [lindex $::zoomLevels $i]
	drawDB .t.fR.screen db

	if {$xy ne ""} {
		# reselect active
		selectRegion .t.fR.screen $x $y
		centerHex .t.fR.screen $x $y
	}
}

proc saveSettings {} {
	# if no db open, done
	if {[info commands ::db] eq ""} {
		return
	}
	saveUnitOrders $gui::prevId .t.tOrd

	set top_geom [winfo geometry .t]
	set x_scroll [.t.fR.canvasX get]
	set y_scroll [.t.fR.canvasY get]

	set geom [list $top_geom $x_scroll $y_scroll]

	set i [lsearch $::zoomLevels $::n]
	::db eval {
		UPDATE settings SET
		geom_top = $geom,
		zoom_level = $i,
		view_level = $gui::viewLevel,
		forSale_open = $gui::forSaleOpen
		WHERE id=1
	}
}

proc createGuiTables {} {
	db eval {
		CREATE TABLE gui(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			name TEXT not null unique on conflict replace,
			val TEXT not null
		);
		INSERT INTO gui(name, val) VALUES(
			"WINDOWS",
			""
		)
	}
}

proc newGame {} {
	set types {
		{{Game Database} {.db}}
		{{All Files} *}
	}
	set ofile [tk_getSaveFile -filetypes $types]
	if {$ofile eq ""} { return }

	# make sure file ends in .db (Windows, I'm looking at you)
	if {[file extension $ofile] eq ""} {
		append ofile ".db"
	}

	wm title .t "True Atlanteans - [file tail $ofile]"

	saveSettings
	createDb $ofile
	createGuiTables

	.t.fR.screen delete all
	enableMenus
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

	saveSettings
	set errMsg [openDb $ofile]
	if {$errMsg ne ""} {
		tk_messageBox -message $errMsg
	}

	if {[db onecolumn {
		SELECT name FROM sqlite_master
		WHERE type = "table" AND name = "gui"
		}] eq ""} {
			createGuiTables
	}

	set gui::viewLevel 1

	wm title .t "True Atlanteans - [file tail $ofile] Turn $::currentTurn"

	# pull settings from db
	set res [db eval {
		SELECT geom_top, zoom_level, view_level, forSale_open
		FROM settings WHERE id=1
	}]

	foreach {geom zoom view forSale} $res {
		if {[llength $geom] == 1} {
			wm geometry .t $geom
		} elseif {[llength $geom] == 3} {
			wm geometry .t [lindex $geom 0]
			.t.fR.screen xview moveto [lindex $geom 1 0]
			.t.fR.screen yview moveto [lindex $geom 2 0]
		}

		setN [lindex $::zoomLevels $zoom]
		set gui::viewLevel $view
		set gui::forSaleOpen $forSale
	}

	drawDB .t.fR.screen db
	enableMenus
	selectFirstHex

	set has_battles [db onecolumn {SELECT count(val) FROM events WHERE type = "BATTLE"}]
	if {$has_battles} {
		tk_messageBox -message "You have battles this turn"
	}
}

proc doAdd {} {
	set ofiles [tk_getOpenFile -multiple 1]
	if {$ofiles eq ""} { return }

	foreach f $ofiles {
		loadData $f
	}

	set txt [wm title .t]
	wm title .t "True Atlanteans - [lindex $txt 3] Turn $::currentTurn"

	drawDB .t.fR.screen db

	set has_battles [db onecolumn {SELECT count(val) FROM events WHERE type = "BATTLE"}]
	if {$has_battles} {
		tk_messageBox -message "You have battles this turn"
	}
	selectFirstHex
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
			WHERE detail.turn=$::currentTurn AND units.detail='own'
			GROUP BY detail.x, detail.y, detail.z
	}

	.t.fR.screen dtag notdone
	drawMarkers .t.fR.screen db
}

proc clearNotDoneCur {w} {
	set xy [getSelectionXY]
	if {$xy eq ""} {return}
	set x [lindex $xy 0]
	set y [lindex $xy 1]

	clearNotDone $w $x $y
}

proc showNotDone {} {
	# build the window
	set t .tNotDone

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Not Done"

		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"
		bind $t.fTop.tv <Double-1> [list selectUnitFromView %W]

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.tv -side left -expand 1 -fill both
	} else {
		raise $t
	}

	$t.fTop.tv delete [$t.fTop.tv children {}]

	# configure all the columns
	set cols ""
	for {set i 1} {$i <= 3} {incr i} { lappend cols $i }
	$t.fTop.tv configure -columns $cols

	$t.fTop.tv heading 1 -text "x"
	$t.fTop.tv heading 2 -text "y"
	$t.fTop.tv heading 3 -text "z"

	db eval {
		SELECT x,y,z
		FROM active_markers
		WHERE done = 0
	} {
		$t.fTop.tv insert {} end -values [list $x $y $z]
	}
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

proc selectUnit {x y z name} {
	set zlevel [getZlevel]
	if {$zlevel ne $z} {
		if {$z eq ""} {set z 1}
		setZlevel $z
		drawDB .t.fR.screen db
		update idletasks
	}
	selectRegion .t.fR.screen $x $y [expr {$z == 0}]
	.t.cbMyUnits set $name
	showUnit $name
}

proc selectFirstHex {} {
	set res [db eval {
		SELECT detail.x, detail.y, detail.z,
		       units.uid, units.name, skillLevel(units.skills, "FORC") as skill
		FROM detail JOIN units ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn and units.detail='own' AND
		   (countItem(units.items, "FLEAD") > 0 OR countItem(units.items, "LEAD") > 0)
		ORDER BY skill DESC LIMIT 1
	}]

	if {$res ne ""} {
		foreach {x y z uid name sl} $res {}
		selectUnit $x $y $z [format {%s (%d)} $name $uid]
		centerHex .t.fR.screen $x $y
		return
	}

	# just grab the first unit
	set res [db eval {
		SELECT detail.x, detail.y as y, detail.z as z,
		       units.uid as uid, units.name
		FROM detail JOIN units ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn and units.detail='own'
		ORDER BY uid DESC LIMIT 1
	}]
	if {$res eq ""} {
		# no units
		return
	}
	foreach {x y z uid name} $res {}
	selectUnit $x $y $z [format {%s (%d)} $name $uid]
	centerHex .t.fR.screen $x $y
}

proc selectUnitFromList {w} {
	set i [lindex [$w curselection] 0]
	set str [$w get $i]
	regexp {^(.+) \(([[:digit:]]+),([[:digit:]]+),?([[:digit:]]*)\)$} \
	  $str -> name x y z

	selectUnit $x $y $z $name
}

proc selectUnitFromView {w} {
	set sel [lindex [$w selection] 0]; # should only be one on double click anyway
	if {$sel eq ""} { return }
	set vals [$w item $sel -values]
	set name [$w item $sel -text]

	set cols [$w cget -columns]
	set x -1
	set y -1
	set z -1
	foreach c $cols {
		set col_name [$w heading $c -text]
		set val [lindex $vals $c-1]

		if {$col_name eq "Loc"} {
			if {![regexp {\(([[:digit:]]+),([[:digit:]]+),([[:digit:]])\)} $val -> x y z]} {
				tk_messageBox -message "Unable to parse Loc column $val"
				return
			}
		} elseif {$col_name eq "x"} {
			set x $val
		} elseif {$col_name eq "y"} {
			set y $val
		} elseif {$col_name eq "z"} {
			set z $val
		} elseif {$col_name eq "Id"} {
			append name [format { (%d)} $val]
		}
	}
	if {$z == -1} {
		tk_messageBox -message "Unable to get xyz values in window $w"
		return
	}

	set sel_xy [getSelectionXY]
	set sel_x [lindex $sel_xy 0]
	set sel_y [lindex $sel_xy 1]
	set zlevel [getZlevel]

	if {$zlevel ne $z} {
		# change level
		setZlevel $z
		drawDB .t.fR.screen db
		# must select region
		selectRegion .t.fR.screen $x $y [expr {$z == 0}]
	} elseif {$sel_x != $x || $sel_y != $y} {
		# region change
		selectRegion .t.fR.screen $x $y [expr {$z == 0}]
	}

	if {$name ne ""} {
		.t.cbMyUnits set $name
		showUnit $name
	}
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
	foreach {n uid x y z} $res {
		if {$z eq ""} {
			$t.fTop.tl insert end "$n ($uid) ($x,$y)"
		} else {
			$t.fTop.tl insert end "$n ($uid) ($x,$y,$z)"
		}
	}
}

proc findForeignUnits {} {
	set res [db eval {
		SELECT units.name, units.uid, detail.x, detail.y, detail.z
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn AND units.detail<>'own'
	}]

	makeUnitListbox .tForeignUnits "Foreign Units" $res
}

proc findIdleUnits {} {
	set res [db eval {
		SELECT units.name, units.uid, detail.x, detail.y, detail.z
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn AND units.detail='own'
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
		setZlevel $z
		drawDB .t.fR.screen db
	}

	selectRegion .t.fR.screen $x $y
}

proc reportTax {} {
	set res [::db eval {
		SELECT x,y,z,curTax(id,tax) as ct,tax
		FROM detail
		WHERE turn=$::currentTurn AND ct > 0
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
		raise $t
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

proc sortAllUnits {tv col isInt} {
	if {$col == 2} {
		# just rebuild
		showAllUnits
		return
	}

	set vals [list]

	# pull top level
	set child_list [$tv children {}]
	# determine if there are multiple levels
	set first_item [lindex $child_list 0]
	if {[$tv children $first_item] eq ""} {
		foreach i $child_list {
			set col0 [$tv item $i -text]
			lappend vals [list $col0 {*}[$tv item $i -values]]
		}
	} else {
		foreach l $child_list {
			foreach i [$tv children $l] {
				set col0 [$tv item $i -text]
				lappend vals [list $col0 {*}[$tv item $i -values]]
			}
		}
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
		WHERE turn=$::currentTurn
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

proc showEvents {} {
	set t .tShowEvents

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Events"

		scrollbar $t.vs -command "$t.tv yview"
		ttk::treeview $t.tv -yscrollcommand "$t.vs set"

		pack $t.vs -side right -fill y
		pack $t.tv -side left -expand 1 -fill both

		wm protocol $t WM_DELETE_WINDOW [list saveWindow db $t [list $t.tv TREEVIEW]]
	}
	$t.tv delete [$t.tv children {}]

	# configure all the columns
	# check for existing settings
	set settings [db onecolumn {
	    SELECT val FROM gui WHERE name="WINDOWS"
	}]
	set settings [dGet $settings $t]
	if {$settings ne ""} {
		wm geometry $t [dGet $settings GEOM]
		set children [dGet $settings CHILDREN]
		foreach {tv child_settings} $children {}
		set widths [dGet $child_settings VALS]
		if {[llength $widths] != 2} {
			set widths [list 216 673]
		}
	} else {
		set widths [list 216 673]
		set tv $t.tv
	}

	set cols ""
	for {set i 1} {$i <= 1} {incr i} { lappend cols $i }
	$tv configure -columns $cols

	$tv column #0 -width [lindex $widths 0]
	for {set i 1} {$i < [llength $widths]} {incr i} {
		$tv column $i -width [lindex $widths $i]
	}

	$tv heading #0 -text "Unit"
	$tv heading 1 -text "Event"

	# populate it
	set par ""
	db eval {
		SELECT type, val
		FROM events
		WHERE type = "REWARD"
	} {
		if {$par eq ""} {
			set par [$t.tv insert {} end -text "Times Reward"]
		}
		$tv insert $par end -values [dGet $val AMT]
	}

	set par_err ""
	db eval {
		SELECT type, val
		FROM events
		WHERE type = "ERROR"
	} {
		if {$par_err eq ""} {
			set par_err [$tv insert {} end -text "Errors"]
		}
		$tv insert $par_err end -text [dGet $val UNIT] -values [list [dGet $val DESC]]
	}

	set par ""
	db eval {
		SELECT type, val
		FROM events
		WHERE type = "EVENT" OR type = "SAIL"
	} {
		if {[dGet $val SUB] eq "FORBID"} {
			$tv insert $par_err end -text [dGet $val UNIT] -values [list [dGet $val DESC]]
		} else {
			if {$par eq ""} {
				set par [$tv insert {} end -text "Events"]
			}
			$tv insert $par end -text [dGet $val UNIT] -values [list [dGet $val DESC]]
		}
	}
}

proc showBattles {} {
	set t .tShowBattles

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Battles"

		scrollbar $t.vs -command "$t.tv yview"
		ttk::treeview $t.tv -yscrollcommand "$t.vs set"
		bind $t.tv <Double-1> [list selectUnitFromView %W]

		pack $t.vs -side right -fill y
		pack $t.tv -side left -expand 1 -fill both

		wm protocol $t WM_DELETE_WINDOW [list saveWindow db $t [list $t.tv TREEVIEW]]
	}
	$t.tv delete [$t.tv children {}]

	# configure all the columns
	# check for existing settings
	set settings [db onecolumn {
	    SELECT val FROM gui WHERE name="WINDOWS"
	}]
	set settings [dGet $settings $t]
	if {$settings ne ""} {
		wm geometry $t [dGet $settings GEOM]
		set children [dGet $settings CHILDREN]
		foreach {tv child_settings} $children {}
		set widths [dGet $child_settings VALS]
		if {[llength $widths] != 2} {
			set widths {200 88}
		}
	} else {
		set widths {200 88}
		set tv $t.tv
	}

	set cols ""
	for {set i 1} {$i <= 1} {incr i} { lappend cols $i }
	$tv configure -columns $cols

	$tv column #0 -width [lindex $widths 0]
	for {set i 1} {$i < [llength $widths]} {incr i} {
		$tv column $i -width [lindex $widths $i]
	}

	$tv column 1 -stretch 0

	$tv heading 1 -text "Loc"

	# populate it
	db eval {
		SELECT val
		FROM events
		WHERE type = "BATTLE"
	} {
		set att [dGet $val Attacker]
		set id [dGet $val AttId]
		set loc [dGet $val XY]
		if {[llength $loc] == 2} {
			foreach {x y} $loc {}
			set z 1
		} else {
			foreach {x y z} $loc {}
		}
		set att_name [format {%s (%d)} $att $id]
		set loc [format {(%d,%d,%d)} $x $y $z]
		set row [$t.tv insert {} end -text $att_name -values [list $loc]]

		set def_name [format {%s (%d)} [dGet $val Defender] [dGet $val DefId]]
		$t.tv insert $row end -text "Attacks: $def_name"

		set att_row [$t.tv insert $row end -text "Attackers"]
		foreach v [dGet $val Attackers] {
			$t.tv insert $att_row end -text $v
		}
		set def_row [$t.tv insert $row end -text "Defenders"]
		foreach v [dGet $val Defenders] {
			$t.tv insert $def_row end -text $v
		}

		set tact_unit [dGet $val Tactics]
		if {$tact_unit ne ""} {
			$t.tv insert $row end -text "$tact_unit gets a free round of attacks"
		}
		foreach l [dGet $val RAW] {
			$t.tv insert $row end -text $l
		}
		set spoil_row [$t.tv insert $row end -text "Spoils" -open 1]
		foreach s [split [dGet $val "Spoils"] ","] {
			$t.tv insert $spoil_row end -text $s
		}
	}
}

proc deleteKeepOuts {w} {
	$w delete [$w selection]
}

proc saveKeepOuts {t} {
	set w $t.fTop.tv
	set keep_outs [list]

	set lines [$w children {}]
	foreach l $lines {
		set v [$w item $l -values]
		lappendU keep_outs $v
	}
	::db eval { INSERT OR REPLACE INTO notes VALUES("keep_out", $keep_outs)}

	destroy $t
}

proc showKeepOuts {} {
	# build the window
	set t .tKeepOut

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Keep Out"

		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"
		bind $t.fTop.tv <Delete> [list deleteKeepOuts %W]
		bind $t.fTop.tv <Double-1> [list selectUnitFromView %W]

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.tv -side left -expand 1 -fill both

		pack [frame $t.fBot] -side top
		pack [button $t.fBot.bOk -text "Ok" -command [list saveKeepOuts $t]] -side left
		pack [button $t.fBot.bCancel -text "Cancel" -command [list destroy $t]] -side left

		wm protocol $t WM_DELETE_WINDOW [list saveWindow db $t [list $t.fTop.tv TREEVIEW]]
	} else {
		raise $t
	}

	$t.fTop.tv delete [$t.fTop.tv children {}]

	# configure all the columns
	# check for existing settings
	set settings [db onecolumn {
	    SELECT val FROM gui WHERE name="WINDOWS"
	}]
	set settings [dGet $settings $t]
	if {$settings ne ""} {
		wm geometry $t [dGet $settings GEOM]
		set children [dGet $settings CHILDREN]
		foreach {tv child_settings} $children {}
		set widths [dGet $child_settings VALS]
		if {[llength $widths] != 2} {
			set widths {20 88 88 88}
		}
	} else {
		set widths {20 88 88 88}
		set tv $t.fTop.tv
	}

	set cols ""
	for {set i 1} {$i <= 3} {incr i} { lappend cols $i }
	$tv configure -columns $cols

	$tv column #0 -width [lindex $widths 0]
	for {set i 1} {$i < [llength $widths]} {incr i} {
		$tv column $i -width [lindex $widths $i]
	}

	$tv heading 1 -text "x"
	$tv heading 2 -text "y"
	$tv heading 3 -text "z"

	set keep_out [::db onecolumn { SELECT val FROM notes WHERE key="keep_out"}]
	foreach coords [lsort -integer -index 2 [lsort -integer -index 0 [lsort -integer -index 1 $keep_out]]] {
		set x [lindex $coords 0]
		set y [lindex $coords 1]
		set z [lindex $coords 2]
		$t.fTop.tv insert {} end -values [list $x $y $z]
	}
}

proc updateSearch {new_txt} {
	set t .tSearchUnits
	$t.fTop.tv delete [$t.fTop.tv children {}]

	# populate it
	set name [format {%%%s%%} $new_txt]
	::db eval {
		SELECT x, y, z, name, uid, orders
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn and units.detail='own' and units.name LIKE $name
	} {
		$t.fTop.tv insert {} end -text $name -values [list $uid $x $y $z [join $orders ";"]]
	}

	return 1
}

proc searchUnits {} {
	# build the window
	set t .tSearchUnits

	if {[winfo exists $t]} {
		raise $t
		# TODO refresh data
		return
	}

	toplevel $t
	wm title $t "Find Units"

	# TODO allow search by id
	pack [frame $t.fEntry] -side top -fill x
	pack [label $t.fEntry.l -text "Unit name"] -side left
	pack [entry $t.fEntry.e -validate key -validatecommand {updateSearch %P}] -side left -expand 1 -fill x

	pack [frame $t.fTop] -side top -expand 1 -fill both

	scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
	ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"
	bind $t.fTop.tv <Double-1> [list selectUnitFromView %W]

	pack $t.fTop.vs -side right -fill y
	pack $t.fTop.tv -side left -expand 1 -fill both

	wm protocol $t WM_DELETE_WINDOW [list saveWindow db $t [list $t.fTop.tv TREEVIEW]]

	# configure all the columns
	# check for existing settings
	set settings [db onecolumn {
	    SELECT val FROM gui WHERE name="WINDOWS"
	}]
	set settings [dGet $settings $t]
	if {$settings ne ""} {
		wm geometry $t [dGet $settings GEOM]
		set children [dGet $settings CHILDREN]
		foreach {tv child_settings} $children {}
		set widths [dGet $child_settings VALS]
		if {[llength $widths] < 6} {
			set widths {177 80 64 64 34 800}
		}
	} else {
		set widths {177 80 64 64 34 800}
		set tv $t.fTop.tv
	}

	set cols ""
	for {set i 1} {$i <= 5} {incr i} { lappend cols $i }
	$tv configure -columns $cols

	set headers {
		"Id" 0
		"x"  0
		"y"  0
		"z"  0
		"Orders" 1
	}

	$tv heading #0 -text "Name"
	$tv column #0 -width [lindex $widths 0] -stretch 0
	set i 1
	foreach {h s} $headers {
		$tv column $i -width [lindex $widths $i]
		$tv heading $i -text $h
		$tv column $i -stretch $s
		incr i
	}

	# allow sorting
	$t.fTop.tv heading #0 -command [list sortProdList $t.fTop.tv 0 0]
	for {set i 1} {$i <= 3} {incr i} {
		$t.fTop.tv heading $i -command [list sortProdList $t.fTop.tv $i 1]
	}

	# start with courier
	$t.fEntry.e insert 0 "Courier"
}

proc wrapText {t d1 col_width name pfx} {
	set later_pfx [lrepeat [llength $pfx] ""]

	set id ""
	set final_desc ""
	set total_w 0
	foreach desc_sent $d1 {
		foreach dw [split [string trim $desc_sent]] {
			set sent_w [font measure TkDefaultFont " $dw"]
			if {$total_w + $sent_w > $col_width} {
				if {$id ne ""} {
					$t.fTop.tv insert $id end -text "" -values [list {*}$later_pfx $final_desc]
				} else {
					append final_desc
					set id [$t.fTop.tv insert {} end -text $name -values [list {*}$pfx $final_desc]]
				}
				set final_desc $dw
				set total_w $sent_w
			} else {
				append final_desc " " $dw
				incr total_w $sent_w
			}
		}
		append final_desc "."
		incr total_w [font measure TkDefaultFont "."]
	}

	if {$id ne ""} {
		$t.fTop.tv insert $id end -text "" -values [list {*}$later_pfx $final_desc]
	} else {
		set id [$t.fTop.tv insert {} end -text $name -values [list {*}$pfx $final_desc]]
	}
	return $id
}

proc itemView {} {
	# build the window
	set t .tSearchItems

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Item Viewer"
		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		scrollbar $t.fTop.hs -command "$t.fTop.tv xview" -orient horizontal
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set" -xscrollcommand "$t.fTop.hs set"

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.hs -side bottom -fill x
		pack $t.fTop.tv -side left -expand 1 -fill both

		wm protocol $t WM_DELETE_WINDOW [list saveWindow db $t [list $t.fTop.tv TREEVIEW]]
	}
	$t.fTop.tv delete [$t.fTop.tv children {}]

	# configure all the columns
	# check for existing settings
	set settings [db onecolumn {
	    SELECT val FROM gui WHERE name="WINDOWS"
	}]
	set settings [dGet $settings $t]
	if {$settings ne ""} {
		wm geometry $t [dGet $settings GEOM]
		set children [dGet $settings CHILDREN]
		foreach {tv child_settings} $children {}
		set widths [dGet $child_settings VALS]
		if {[llength $widths] < 4} {
			set widths {200 65 79 34}
		}
	} else {
		set widths {200 65 79 34}
		set tv $t.fTop.tv
	}

	set cols ""
	for {set i 1} {$i <= 4} {incr i} { lappend cols $i }
	$tv configure -columns $cols

	$tv column #0 -width [lindex $widths 0]
	for {set i 1} {$i < [llength $widths]} {incr i} {
		$tv column $i -width [lindex $widths $i]
	}

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
		ORDER BY name
	}]
	foreach {name abbr type desc} $res {
		set wt [dGet $desc Weight]
		set d1 [dGet $desc Desc]

		set col_width [$t.fTop.tv column 4 -width]

		if {$type eq "race"} {
			set d [parseMan $d1]
			set id [$t.fTop.tv insert {} end -text $name -values [list $abbr $type $wt [join [dGet $d DESC] "."]]]
			if {[dict exists $d ALL]} {
				set lvl [dict get $d ALL]
				$t.fTop.tv insert $id end -text "" -values [list "" "" "" "This race may study all skills to level $lvl"]
			} else {
				set lvl1 [dGet $d SPEC_LVL]
				set lvl2 [dGet $d OTH_LVL]
				set skills [dGet $d SPEC]
				$t.fTop.tv insert $id end -text "" -values [list "" "" "" "This race may study $skills to level $lvl1"]
				$t.fTop.tv insert $id end -text "" -values [list "" "" "" "This race may study other skills to level $lvl2"]
			}

		} elseif {$type eq "item"} {
			set id [wrapText $t $d1 $col_width $name [list $abbr $type $wt]]
		} else {
			set id [wrapText $t $d1 $col_width $name [list $abbr $type $wt]]
		}

		set capacity [dGet $desc Capacity]
		if {$capacity ne ""} {
			set txt ""
			foreach type {walking riding swimming flying} {
				set val [dGet $capacity $type]
				if {$val eq ""} { continue }
				append txt "$type capacity $val "
			}
			$t.fTop.tv insert $id 0 -text "" -values [list "" "" "" $txt]
		}
	}

	# allow sorting
	$t.fTop.tv heading #0 -command [list sortProdList $t.fTop.tv 0 0]
	for {set i 1} {$i <= 4} {incr i} {
		$t.fTop.tv heading $i -command [list sortProdList $t.fTop.tv $i [expr {$i == 3}]]
	}
}

proc showObjectDefs {} {
	# build the window
	set t .tObjectDefs

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Object Definitions"
		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.tv -side left -expand 1 -fill both

		wm protocol $t WM_DELETE_WINDOW [list saveWindow db $t [list $t.fTop.tv TREEVIEW]]
	}
	$t.fTop.tv delete [$t.fTop.tv children {}]

	# configure all the columns
	# check for existing settings
	set settings [db onecolumn {
	    SELECT val FROM gui WHERE name="WINDOWS"
	}]
	set settings [dGet $settings $t]
	if {$settings ne ""} {
		wm geometry $t [dGet $settings GEOM]
		set children [dGet $settings CHILDREN]
		foreach {tv child_settings} $children {}
		set widths [dGet $child_settings VALS]
		if {[llength $widths] != 2} {
			set widths {100 65}
		}
	} else {
		set widths [list 100 65]
		set tv $t.fTop.tv
	}
	set cols ""
	for {set i 1} {$i <= 1} {incr i} { lappend cols $i }
	$t.fTop.tv configure -columns $cols

	$tv column #0 -width [lindex $widths 0]
	for {set i 1} {$i < [llength $widths]} {incr i} {
		$tv column $i -width [lindex $widths $i]
	}

	$t.fTop.tv heading #0 -text "Object"
	$t.fTop.tv column #0 -stretch 0
	$t.fTop.tv heading 1 -text "Type"

	# populate it
	db eval {
		SELECT desc
		FROM object_defs
		ORDER BY desc
	} {
		set col0 [lindex $desc 0]
		regexp {([^:]*): This is a (.*)} $col0 -> name type

		set row [$t.fTop.tv insert {} end -text $name -values [list $type]]
		foreach l [lrange $desc 1 end] {
			$t.fTop.tv insert $row end -values [list $l]
		}
	}
}

proc showSkills {} {
	# build the window
	set t .tShowSkills

	if {[winfo exists $t]} {
		raise $t
		return
	}

	toplevel $t
	wm title $t "Skill Viewer"
	pack [frame $t.fTop] -side top -expand 1 -fill both

	scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
	ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"

	pack $t.fTop.vs -side right -fill y
	pack $t.fTop.tv -side left -expand 1 -fill both

	wm protocol $t WM_DELETE_WINDOW [list saveWindow db $t [list $t.fTop.tv TREEVIEW]]

	$t.fTop.tv delete [$t.fTop.tv children {}]

	# configure all the columns
	# check for existing settings
	set settings [db onecolumn {
	    SELECT val FROM gui WHERE name="WINDOWS"
	}]
	set settings [dGet $settings $t]
	if {$settings ne ""} {
		wm geometry $t [dGet $settings GEOM]
		set children [dGet $settings CHILDREN]
		foreach {tv child_settings} $children {}
		set widths [dGet $child_settings VALS]
		if {[llength $widths] < 4} {
			set widths {100 65 79 34}
		}
	} else {
		set widths [list 100 65 79 34]
		set tv $t.fTop.tv
	}

	set cols ""
	for {set i 1} {$i <= 4} {incr i} { lappend cols $i }
	$t.fTop.tv configure -columns $cols

	$tv column #0 -width [lindex $widths 0]
	for {set i 1} {$i < [llength $widths]} {incr i} {
		$tv column $i -width [lindex $widths $i]
	}

	$t.fTop.tv column #0 -stretch 0
	$t.fTop.tv column 1 -stretch 0
	$t.fTop.tv column 2 -stretch 0
	$t.fTop.tv column 3 -stretch 0
	$t.fTop.tv heading 1 -text "Abbr"
	$t.fTop.tv heading 2 -text "Level"
	$t.fTop.tv heading 3 -text "Cost"
	$t.fTop.tv heading 4 -text "Desc"

	set col_width [$t.fTop.tv column 4 -width]

	::db eval {
		SELECT name, abbr, level, cost, desc
		FROM skills
		ORDER BY name
	} {
		wrapText $t $desc $col_width $name [list $abbr $level $cost]
	}
}

# return number of hexes where production is underway
proc ctProd {} {
	set res [db eval {
		SELECT units.orders, detail.x, detail.y, detail.z
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn AND units.detail='own'
	}]

	foreach {ol x y z} $res {
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

proc findSharingUnits {units} {
	set ret [list]
	foreach u $units {
		if {[lsearch [{*}$u cget -flags] SHARE] != -1} {
			lappend ret $u
		}
	}
	return $ret
}

# process one order
proc checkOrder {u o x y z ctxt} {
	if {$o eq ""} {
		return 0
	}
	# check for comments
	if {[string index $o 0] eq ";"} {
		return 0
	}

	set units [dGet $ctxt Units]
	set sharing_units [findSharingUnits [dict get $ctxt UnitObj]]

	set op $o
	set c [lindex $op 0]
	switch -nocase $c {
		declare {
			# change diplomatic stance
			# TODO use a dialog
			return 0
		}

		transport {
			# TODO
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
			set cur_silv [{*}$u countItem SILV]
			{*}$u setItem SILV [expr {$cur_silv + $amt}]
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

			## check receiver
			set recv [lindex $op 1]
			set i 2

			if {$recv == 0} {
				set recv_obj ""
			} else {
				if {[string compare -nocase $recv "new"] == 0} {
					set recv "new [lindex $op 2]"
					incr i
				}
				if {[dict exists $units $recv]} {
					set recv_obj [dict get $units $recv]
					if {$recv eq [{*}$u cget -num]} {
						return [list -1 "Give: Cannot give to self ('$o')"]
					}
				} elseif {[lsearch [dict get $ctxt Foreign] $recv] != -1 } {
					set recv_obj ""
				} else {
					return [list -1 "Give with invalid receiver ('$o')"]
				}
			}

			set item_id [string toupper [lindex $op $i+1]]
			if {$item_id eq ""} {
				return [list -1 "Give needs item ('$o')"]
			}

			# check inventory
			set cur_ct [{*}$u countItem $item_id]
			set ct [lindex $op $i]
			if {$ct eq "all"} {
				set ct $cur_ct
			}

			if {$cur_ct < $ct} {
				return [list -1 "Give more than they own ($cur_ct < $ct $item_id)"]
			}

			# execute
			{*}$u setItem $item_id [expr {$cur_ct - $ct}]
			if {$recv_obj ne ""} {
				set recv_ct [{*}$recv_obj countItem $item_id]
				{*}$recv_obj setItem $item_id [expr {$recv_ct + $ct}]
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
			set men [countMen [{*}$u cget -items]]
			set cost [expr {$skill_cost * $men}]

			set cur_silv [{*}$u countItem SILV]
			if {$cur_silv > 0} {
				if {$cur_silv >= $cost} {
					{*}$u setItem SILV [expr {$cur_silv - $cost}]
					set cost 0
				} else {
					{*}$u setItem SILV 0
					set cost [expr {$cost - $cur_silv}]
				}
			}

			if {$cost > 0} {
				# look for a sharing source
				foreach su $sharing_units {
					set s_silv [{*}$su countItem SILV]
					if {$s_silv >= $cost} {
						{*}$su setItem SILV [expr {$s_silv - $cost}]
						return 0
					}

					if {$s_silv > 0} {
						{*}$su setItem SILV 0
						set cost [expr {$cost - $s_silv}]
					}
				}
				return [list -1 "STUDY: not enough funds"]
			}

			return 0
		}

		teach {
			# help another unit study
			# TODO check args
			for {set i 1} {$i < [llength $o]} {incr i} {
				set student [lindex $o $i]
				if {[string compare -nocase $student "new"] == 0} {
					incr i
					set student "new [lindex $o $i]"
				} elseif {[string compare -nocase $student "!new"] == 0} {
					incr i
					set student "new [lindex $o $i]"
				}

				if {[dict exists $units $student]} {
					set student_obj [dict get $units $student]
				} elseif {[lsearch [dict get $ctxt Foreign] $student] != -1 } {
					continue
				} else {
					return [list -1 "Teach with invalid student ('$o')"]
				}

				set student_orders [{*}$student_obj cget -orders]
				set soi [lsearch -regexp -nocase $student_orders {^study}]
				if {$soi == -1} {
					return [list -1 "Teach: Student $student is not studying ('$o')"]
				}
			}
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
			set valid_dir {n s ne se nw sw}
			foreach d [lrange $op 1 end] {
				if {[string compare -nocase $d "in"] == 0} {
					# TODO check that object has inner location
				} elseif {[string is integer $d]} {
					# TODO check that object exists
				} else {
					if {[lsearch -nocase $valid_dir $d] == -1} {
						return [list -1 "MOVE: Invalid direction: $d"]
					}
				}
			}
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

		join {
			# TODO check target
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
			# check args
			set ct [lindex $op 1]
			set item_id [string toupper [lindex $op 2]]
			if {$item_id eq ""} {
				return [list -1 "SELL: need quantity"]
			}
			# check inventory
			set cur_ct [{*}$u countItem $item_id]
			if {$ct eq "all"} {
				set ct $cur_ct
			} elseif {$cur_ct < $ct} {
				return [list -1 "SELL: more than they own ($cur_ct < $ct $item_id)"]
			}
			# execute
			{*}$u setItem $item_id [expr {$cur_ct - $ct}]
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

# check one order, only do orders of "tgt_ord" if not null
proc checkOrderType {tgt_ord x y z ctxt} {
	set ret [list]
	set unit_map [dict get $ctxt Units]
	dict for {u v} $unit_map {
		set ol [{*}$v cget -orders]
		if {$ol eq ""} continue

		set new_orders [list]

		# foreach order
		foreach o $ol {

			set o [cleanOrder $o]

			set op [split $o " "]
			set c [string tolower [lindex $op 0]]
			if {$tgt_ord eq ""} {
				lappend new_orders $o
			} elseif {$tgt_ord ne $c} {
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
		{*}$v configure -orders $new_orders
	}
	return $ret
}

proc checkAllOrders {} {
	saveUnitOrders $gui::prevId .t.tOrd
	# pull all hexes that we have details for
	set res [::db eval {
		SELECT DISTINCT id, x, y, z
		FROM detail
		WHERE turn=$::currentTurn
	}]

	set ret [list]
	# foreach hex
	foreach {id x y z} $res {
		set units [getUnitObjects $id]
		if {$units eq ""} { continue }

		set new_units [list]
		foreach u $units {
			set lret [{*}$u filterInstantOrders]
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
			dict set unit_map [{*}$u cget -num] $u
		}

		set ctxt [dict create]
		dict set ctxt Units $unit_map
		dict set ctxt UnitObj $units

		dict set ctxt Foreign [db eval {
			SELECT uid FROM units
			WHERE regionId=$id AND detail <> 'own'
		}]

		# run claim/give before other orders
		lappend ret {*}[checkOrderType "claim" $x $y $z $ctxt]
		lappend ret {*}[checkOrderType "give" $x $y $z $ctxt]
		lappend ret {*}[checkOrderType "" $x $y $z $ctxt]

		if {$units ne ""} {
			itcl::delete object {*}$units
		}
	}

	if {$ret ne ""} {
		showErrors $ret
	} else {
		# clear old contents
		set t .tOrderErrors
		if {[winfo exists $t]} {
			$t.fTop.tv delete [$t.fTop.tv children {}]
		}

		tk_messageBox -message "No errors"
	}
	return $ret
}

proc showErrors {err_list} {
	# build the window
	set t .tOrderErrors
	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Errors in Orders"
		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.tv -side left -expand 1 -fill both

		wm protocol $t WM_DELETE_WINDOW [list saveWindow db $t [list $t.fTop.tv TREEVIEW]]
		bind $t.fTop.tv <g> [list giveToSel $t.fTop.tv]
	}

	# clear old contents
	$t.fTop.tv delete [$t.fTop.tv children {}]

	# pull previous settings
	set settings [db onecolumn {
	    SELECT val FROM gui WHERE name="WINDOWS"
	}]
	set settings [dGet $settings $t]
	if {$settings ne ""} {
		wm geometry $t [dGet $settings GEOM]
		set children [dGet $settings CHILDREN]
		foreach {tv child_settings} $children {}
	} else {
		set tv $t.fTop.tv
	}

	# populate it
	foreach err $err_list {
		$tv insert {} end -text $err
	}
}

proc saveOrders {} {
	saveUnitOrders $gui::prevId .t.tOrd
	set filename [format {orders%d.txt} $::currentTurn]
	set ofile [tk_getSaveFile -initialfile $filename ]
	if {$ofile eq ""} { return }

	writeOrders $ofile
}

proc importMap {} {
	set fname [tk_getOpenFile]
	if {$fname eq ""} { return }

	set regions [readMap $fname]
	importRegionData db $regions
	drawDB .t.fR.screen db
}

proc exportMap {} {
	set ofile [tk_getSaveFile]
	if {$ofile eq ""} { return }

	writeMap $ofile
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
	set bbox [$w bbox [format "hex_%d_%d && !neg_hex" $x $y]]
	if {$bbox eq ""} {
		set bbox [$w bbox [format "nexus_%d_%d && !neg_hex" $x $y]]
	}
	lassign $bbox x1 y1 x2 y2

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

	set xpos [expr {($cx - $xmin) / ($xmax - $xmin) - ($right - $left) / 2.0}]
	set ypos [expr {($cy - $ymin) / ($ymax - $ymin) - ($btm - $top)    / 2.0}]

	$w xview moveto $xpos
	$w yview moveto $ypos
}

proc countItemsByType {il type} {
	set count 0
	foreach i $il {
		set abbr [string trim [lindex $i 2] {[]}]
		set t [::db onecolumn {
			SELECT type FROM items WHERE abbr=$abbr
		}]
		if {$t eq $type} {
			incr count [lindex $i 0]
		}
	}
	return $count
}

proc typeMen {il} {
	set type "none?"
	foreach i $il {
		set abbr [string trim [lindex $i 2] {[]}]
		if {[lsearch $::men $abbr] != -1} {
			if {$type eq "none?"} {
				set type $abbr
			} else {
				return "MIX"
			}
		}
	}
	return $type
}

proc showAllUnits {} {
	set units [db eval {
		SELECT detail.x, detail.y, detail.z, units.name, units.uid, units.items,
		   units.orders, units.skills, units.flags
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn and units.detail='own'
		ORDER BY
		    cast(detail.z as integer),
		    cast(detail.x as integer),
		    cast(detail.y as integer)
	}]

	# build the window
	set t .tAllUnits

	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "All Units"
		pack [frame $t.fTop] -side top -expand 1 -fill both

		scrollbar $t.fTop.vs -command "$t.fTop.tv yview"
		ttk::treeview $t.fTop.tv -yscrollcommand "$t.fTop.vs set"
		bind $t.fTop.tv <Double-1> [list selectUnitFromView %W]

		pack $t.fTop.vs -side right -fill y
		pack $t.fTop.tv -side left -expand 1 -fill both

		wm protocol $t WM_DELETE_WINDOW [list saveWindow db $t [list $t.fTop.tv TREEVIEW]]
		bind $t.fTop.tv <g> [list giveToSel $t.fTop.tv]
	}

	# clear old contents
	$t.fTop.tv delete [$t.fTop.tv children {}]

	# populate it

	# configure all the columns
	set hdrs {
		"Id" "Loc" "Flags" "Skills" "Men" "Type" "Silv" "Weapons" "Armor" "Horses" "Orders"
	}
	set widths {
		126 54 50 93 87 90 81 95 78 87 94 329
	}

	set settings [db onecolumn {
	    SELECT val FROM gui WHERE name="WINDOWS"
	}]
	set settings [dGet $settings $t]
	if {$settings ne ""} {
		wm geometry $t [dGet $settings GEOM]
		set children [dGet $settings CHILDREN]
		foreach {tv child_settings} $children {}
		set new_widths [dGet $child_settings VALS]
		if {[llength $new_widths] == [llength $widths]} {
			set widths $new_widths
		}
	} else {
		set tv $t.fTop.tv
	}

	set cols ""
	for {set i 1} {$i <= [llength $hdrs]} {incr i} { lappend cols $i }
	$t.fTop.tv configure -columns $cols

	$tv heading #0 -command [list sortAllUnits $tv 0 0]

	$tv column #0 -width [lindex $widths 0]
	$tv column #0 -stretch 0
	for {set i 1} {$i < [llength $hdrs]} {incr i} {
		$tv heading $i -text [lindex $hdrs $i-1]
		$tv column $i -width [lindex $widths $i]
		$tv column $i -stretch 0
		$tv heading $i -command [list sortAllUnits $tv $i [expr {$i != 9}]]
	}

	$tv heading $i -text [lindex $hdrs $i-1]
	$tv heading $i -command [list sortAllUnits $tv $i 0]

	set total_men 0
	set total_silv 0
	foreach {x y z name uid items orders skills flags} $units {
		if {![info exists id($x,$y,$z)]} {
			set terrain_type [::db onecolumn {SELECT type FROM terrain WHERE x=$x AND y=$y AND z=$z}]
			set hdr_cols [list "" "($x,$y,$z)" "" "" "" "" 0]
			set id($x,$y,$z) [$t.fTop.tv insert {} end -text $terrain_type -values $hdr_cols]
			$t.fTop.tv item $id($x,$y,$z) -open 1
			set total_men 0
			set total_silv 0
		}

		set vals [list]
		lappend vals $uid
		lappend vals "($x,$y,$z)"
		lappend vals [join [unitFlagsStr $flags] ""]

		if {[llength $skills] == 1} {
			# only one skill - easy
			lappend vals [lindex $skills 0 3]
		} else {
			# figure out which one to show
			#
			# assume mages are FORC + FIRE
			set i [lsearch -index 1 $skills FORC]
			if {$i != -1} {
				lappend vals "mage"
			} else {
				# not a mage
				# check for soldier
				set i [lsearch -index 1 $skills COMB]
				if {$i != -1} {
					# soldier of some sort
					set comb [lindex $skills $i 3]
					set rskills [lreplace $skills $i $i]
					lappend vals [format {%d/%s} $comb [join [lmap i $rskills {lindex $i 3}] "/"]]
				} else {
					# non-combat
					lappend vals [join [lmap i $skills {lindex $i 3}] "/"]
				}
			}
		}

		set amt_men [countMen $items]
		lappend vals $amt_men
		incr total_men $amt_men

		lappend vals [typeMen $items]

		set amt_silv [countItem $items SILV]
		incr total_silv $amt_silv
		lappend vals $amt_silv

		set hdr_cols [lreplace $hdr_cols 4 4 $total_men]
		set hdr_cols [lreplace $hdr_cols 6 6 $total_silv]
		$t.fTop.tv item $id($x,$y,$z) -values $hdr_cols

		lappend vals [countItemsByType $items weapon]
		lappend vals [countItemsByType $items armor]
		lappend vals [countItemsByType $items mount]
		lappend vals [join $orders ";"]
		$t.fTop.tv insert $id($x,$y,$z) end -text $name -values $vals
	}
}

proc formTaxers {regionId} {
	# get units in hex
	set rdata [db eval {
		SELECT orders, items, flags
		FROM units
		WHERE regionId=$regionId AND detail="own"
	}]

	# look over all units
	set totalSilver 0
	set numTaxers 0
	foreach {orders items flags} $rdata {
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
		WHERE id=$regionId AND turn=$::currentTurn
	}]
	set price [lindex $rdata 1]
	# TODO configure maintenance cost
	set maxBuy [expr {$totalSilver / ($price + 10)}]
	return [expr {min($taxersNeeded, $maxBuy)}]
}

proc applyOrderTemplate {} {
	set t .tFormUnit.fTop.cbTemplate
	set v [$t get]

	set e .tFormUnit.fTop.eName
	set o .tFormUnit.fTop.orders
	switch $v {
		Guard {
			$e delete 0 end
			$e insert end $v
			$o insert end "study comb\n"
			$o insert end "turn\n"
			$o insert end "@tax\n"
			$o insert end "endturn\n"
		}
	}
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
		SELECT id, turn, sells, race
		FROM detail
		WHERE x=$x AND y=$y AND z=$zlevel
		ORDER BY turn DESC LIMIT 1
	}]
	if {[llength $rdata] == 0} { return }

	foreach {regionId turn sells peasants} $rdata {}

	if {$turn != $::currentTurn} { return }

	set ret [getBuyRace $sells $peasants]
	set max_list [lindex $ret 0]
	set raceList [lindex $ret 1]
	set maxRace [lindex $max_list 0]

	# get units in hex
	set rdata [db eval {
		SELECT name, uid, orders, flags
		FROM units
		WHERE regionId=$regionId AND detail="own"
	}]

	set maxAlias 0
	set unitList [list]
	set unitOrders [list]
	set unitFlags [list]
	foreach {name uid orders flags} $rdata {
		lappend unitList [format {%s (%d)} $name $uid]
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

		set template_values [list "Guard"]
		grid [ttk::combobox $t.fTop.cbTemplate -state readonly -values $template_values] -row 5 -column 0
		grid [button $t.fTop.bTemplate -text "Apply Template" -command applyOrderTemplate] -row 5 -column 1
		$t.fTop.cbTemplate current 0

		grid [label $t.fTop.lOrders -text "Orders"] -row 6 -columnspan 2
		grid [text $t.fTop.orders -height 24 -width 42] -row 7 -columnspan 2 -sticky nswe

		grid columnconfigure $t.fTop 1 -weight 1

		pack [frame $t.fButtons] -side bottom
		pack [button $t.fButtons.bOk -text "Ok" -command [list finishForm $t]] -side left
		pack [button $t.fButtons.bCancel -text "Cancel" -command [list destroy $t]] -side right
	}

	#$t.fTop.cbParent configure -values $unitList
	#$t.fTop.cbParent current 0
	$t.fTop.sAlias set [expr {$maxAlias + 1}]
	if {[llength $raceList] > 0} {
		$t.fTop.cbRaces configure -values $raceList
		$t.fTop.cbRaces current 0
	}
	$t.fTop.sCt configure -to $maxRace
	$t.fTop.sCt set [expr {min($maxRace, [formTaxers $regionId])}]
}

proc finishForm {t} {
	.t.tOrd insert end "\nform [$t.fTop.sAlias get]\n"

	set name [$t.fTop.eName get]
	if {$name ne ""} {
		.t.tOrd insert end "name unit \"$name\"\n"
	}

	set ct [$t.fTop.sCt get]
	if {$ct > 0} {
		set race [$t.fTop.cbRaces get]
		regexp {\[(.+)\]} $race -> abbr
		.t.tOrd insert end "buy $ct $abbr\n"
	}

	set orders [$t.fTop.orders get 1.0 end]
	if {$orders ne ""} {
		.t.tOrd insert end "$orders\n"
	}
	.t.tOrd insert end "end\n"

	# TODO use parent listbox
	if {$gui::prevUnit ne ""} {
		saveUnitOrders $gui::prevId .t.tOrd
	}

	destroy $t
}

proc splitUnit {} {
	# pull current hex info
	set xy [getSelectionXY]
	if {$xy eq ""} { return }
	set x [lindex $xy 0]
	set y [lindex $xy 1]
	set zlevel [getZlevel]
	## get most recent details
	set rdata [db eval {
		SELECT id, turn
		FROM detail
		WHERE x=$x AND y=$y AND z=$zlevel
		ORDER BY turn DESC LIMIT 1
	}]
	if {[llength $rdata] == 0} { return }
	foreach {regionId turn} $rdata {}
	if {$turn != $::currentTurn} { return }

	# get units in hex
	set rdata [db eval {
		SELECT name, uid, orders, flags, items
		FROM units
		WHERE regionId=$regionId AND detail="own"
	}]

	set maxAlias 0
	set unitList [list]
	set unitOrders [list]
	set unitFlags [list]
	set start_items ""
	set unit_name ""
	foreach {name uid orders flags items} $rdata {
		set full_name [format {%s (%d)} $name $uid]
		lappend unitList $full_name
		if {$full_name eq $gui::prevUnit} {
			set start_items $items
			set unit_name $name
		}
		lappend unitOrders $orders
		lappend unitFlags $flags

		foreach o $orders {
			if {[regexp -nocase "^@?form\\M" $o]} {
				set maxAlias [expr {max($maxAlias, [lindex $o 1])}]
			}
		}
	}

	# build the window
	set t .tSplitUnit
	if {![winfo exists $t]} {
		toplevel $t
		wm title $t "Split Unit"

		pack [frame $t.fTop] -side top -expand -1 -fill y
		grid [label $t.fTop.lAlias -text "Alias"] -row 0 -column 0
		grid [ttk::spinbox $t.fTop.sAlias -from 1 -to 100] -row 0 -column 1 -sticky we -columnspan 2

		grid [label $t.fTop.lName -text "Name"] -row 1 -column 0
		grid [entry $t.fTop.eName] -row 1 -column 1 -sticky we -columnspan 2
		$t.fTop.eName insert end $unit_name
		grid columnconfigure $t.fTop 1 -weight 1

		pack [frame $t.fMid] -side top -expand 1 -fill both
		grid [ttk::treeview $t.fMid.lbKeep -selectmode browse -columns {0 1}] -row 0 -column 0
		grid [frame $t.fMid.fMovers] -row 0 -column 1
		grid [ttk::treeview $t.fMid.lbGive -selectmode browse -columns {0 1}] -row 0 -column 2
		grid columnconfigure $t.fMid 0 -weight 1
		grid columnconfigure $t.fMid 2 -weight 1

		pack [button $t.fMid.fMovers.bAdd -text "<" -command [list splitAdd $t.fMid.lbKeep $t.fMid.lbGive 1]] -side top
		pack [button $t.fMid.fMovers.bSub -text ">" -command [list splitSub $t.fMid.lbKeep $t.fMid.lbGive 1]] -side top
		bind $t.fMid.fMovers.bAdd <Shift-1> "splitAdd $t.fMid.lbKeep $t.fMid.lbGive 10; break"
		bind $t.fMid.fMovers.bAdd <Control-1> "splitAdd $t.fMid.lbKeep $t.fMid.lbGive 100; break"
		bind $t.fMid.fMovers.bAdd <Control-Shift-1> "splitAdd $t.fMid.lbKeep $t.fMid.lbGive 1000; break"

		bind $t.fMid.fMovers.bSub <Shift-1> "splitSub $t.fMid.lbKeep $t.fMid.lbGive 10; break"
		bind $t.fMid.fMovers.bSub <Control-1> "splitSub $t.fMid.lbKeep $t.fMid.lbGive 100; break"
		bind $t.fMid.fMovers.bSub <Control-Shift-1> "splitSub $t.fMid.lbKeep $t.fMid.lbGive 1000; break"

		pack [label $t.lOrders -text "Orders"] -side top
		pack [text $t.orders -height 24 -width 42] -side top -expand 1 -fill both

		pack [frame $t.fButtons] -side top
		pack [button $t.fButtons.bOk -text "Ok" -command [list finishSplit $t]] -side left
		pack [button $t.fButtons.bCancel -text "Cancel" -command [list destroy $t]] -side left
	}

	$t.fTop.sAlias set [expr {$maxAlias + 1}]
	foreach i $start_items {
		set ct [lindex $i 0]
		set abbr [lindex $i end]
		set name [lrange $i 1 end-1]
		$t.fMid.lbKeep insert {} end -text $name -values [list $abbr $ct]
	}
}

proc splitMv {w w2 v} {
	set sel [$w selection]
	if {$sel eq ""} { return }

	set name [$w item $sel -text]
	set i [$w item $sel -values]
	set ct [lindex $i 1]
	set abbr [lindex $i 0]

	if {$v > $ct} { set v $ct }
	incr ct -$v

	if {$ct > 0} {
		$w item $sel -values [list $abbr $ct]
	} else {
		$w delete $sel
	}

	foreach item [$w2 children {}] {
		set i [$w2 item $item -values]
		set ct2 [lindex $i 1]
		set abbr2 [lindex $i 0]
		if {$abbr2 eq $abbr} {
			incr ct2 $v
			$w2 item $item -values [list $abbr $ct2]
			return
		}
	}
	$w2 insert {} end -text $name -values [list $abbr $v]
}

proc splitAdd {t1 t2 v} {
	set w2 $t1
	set w $t2
	splitMv $w $w2 $v
}

proc splitSub {t1 t2 v} {
	set w $t1
	set w2 $t2
	splitMv $w $w2 $v
}

proc finishSplit {t} {
	set alias [$t.fTop.sAlias get]
	.t.tOrd insert end "\nform $alias\n"

	set name [$t.fTop.eName get]
	if {$name ne ""} {
		.t.tOrd insert end "name unit \"$name\"\n"
	}

	set orders [$t.orders get 1.0 end]
	if {$orders ne ""} {
		.t.tOrd insert end "$orders\n"
	}
	.t.tOrd insert end "end\n"

	# give
	foreach i [$t.fMid.lbGive children {}] {
		set vals [$t.fMid.lbGive item $i -values]
		set ct [lindex $vals 1]
		set abbr [string map {[ "" ] ""} [lindex $vals 0]]
		.t.tOrd insert end "give new $alias $ct $abbr\n"
	}

	# TODO use parent listbox
	if {$gui::prevUnit ne ""} {
		saveUnitOrders $gui::prevId .t.tOrd
	}

	destroy $t
}

proc finishGive {t recv_id} {
	.t.tOrd insert end "\n"
	foreach i [$t.fMid.lbGive children {}] {
		set vals [$t.fMid.lbGive item $i -values]
		set ct [lindex $vals 1]
		set abbr [string map {[ "" ] ""} [lindex $vals 0]]
		.t.tOrd insert end "give $recv_id $ct $abbr\n"
	}
	destroy $t
}

proc giveToSel {w} {
	# we'll give from prevUnit
	if {$gui::prevUnit eq ""} {
		return
	}
	set r [extractUnitNameNum $gui::prevUnit 1]
	set give_id [lindex $r 1]

	set sel [$w selection]
	# TODO handle give to many...
	if {[llength $sel] != 1} {
		return
	}
	set vals [$w item $sel -values]

	set id ""
	set x ""
	set y ""
	set z ""
	set cols [$w cget -columns]
	foreach c $cols {
		set col_name [$w heading $c -text]
		set val [lindex $vals $c-1]

		if {$col_name eq "Loc"} {
			if {![regexp {\(([[:digit:]]+),([[:digit:]]+),([[:digit:]])\)} $val -> x y z]} {
				tk_messageBox -message "Unable to parse Loc column $val"
				return
			}
		} elseif {$col_name eq "Id"} {
			set id $val
		} elseif {$col_name eq "x"} {
			set x $val
		} elseif {$col_name eq "y"} {
			set y $val
		} elseif {$col_name eq "z"} {
			set z $val
		}
	}
	if {$id eq "" || $x eq "" || $y eq "" || $z eq ""} {
		return
	}
	set rid [db onecolumn {
		SELECT id
		FROM detail
		WHERE turn=$::currentTurn and x=$x and y=$y and z=$z
	}]
	set items [db onecolumn {
		SELECT items
		FROM units
		WHERE regionId=$rid and uid=$give_id
	}]

	# build the window
	set t .tGiveItems
	toplevel $t
	wm title $t "Give Items"

	pack [frame $t.fMid] -side top -expand 1 -fill both
	grid [ttk::treeview $t.fMid.lbKeep -selectmode browse -columns {0 1}] -row 0 -column 0
	grid [frame $t.fMid.fMovers] -row 0 -column 1
	grid [ttk::treeview $t.fMid.lbGive -selectmode browse -columns {0 1}] -row 0 -column 2
	grid columnconfigure $t.fMid 0 -weight 1
	grid columnconfigure $t.fMid 2 -weight 1

	pack [button $t.fMid.fMovers.bAdd -text "<" -command [list splitAdd $t.fMid.lbKeep $t.fMid.lbGive 1]] -side top
	pack [button $t.fMid.fMovers.bSub -text ">" -command [list splitSub $t.fMid.lbKeep $t.fMid.lbGive 1]] -side top
	bind $t.fMid.fMovers.bAdd <Shift-1> "splitAdd $t.fMid.lbKeep $t.fMid.lbGive 10; break"
	bind $t.fMid.fMovers.bAdd <Control-1> "splitAdd $t.fMid.lbKeep $t.fMid.lbGive 100; break"
	bind $t.fMid.fMovers.bAdd <Control-Shift-1> "splitAdd $t.fMid.lbKeep $t.fMid.lbGive 1000; break"

	bind $t.fMid.fMovers.bSub <Shift-1> "splitSub $t.fMid.lbKeep $t.fMid.lbGive 10; break"
	bind $t.fMid.fMovers.bSub <Control-1> "splitSub $t.fMid.lbKeep $t.fMid.lbGive 100; break"
	bind $t.fMid.fMovers.bSub <Control-Shift-1> "splitSub $t.fMid.lbKeep $t.fMid.lbGive 1000; break"

	pack [frame $t.fButtons] -side top
	pack [button $t.fButtons.bOk -text "Ok" -command [list finishGive $t $id]] -side left
	pack [button $t.fButtons.bCancel -text "Cancel" -command [list destroy $t]] -side left

	foreach i $items {
		set ct   [lindex $i 0]
		set abbr [lindex $i end]
		set name [join [lrange $i 1 end-1] " "]
		$t.fMid.lbKeep insert {} end -text $name -values [list $abbr $ct]
	}

	tkwait window $t
}

proc toggleDrawAll {} {
	drawDB .t.fR.screen db
}

proc loadGlob {patt} {
	set files [glob $patt]
	foreach f $files {
		loadData $f
	}

	drawDB .t.fR.screen db
}

rename exit origExit
proc exit {} {
	saveSettings
	origExit
}

##############################################################################
### build the GUI
toplevel .t
bind .t <Destroy> {exit}
wm title .t "True Atlanteans - <no game open>"

### top menu
menu .mTopMenu -tearoff 0

# file menu
menu .mTopMenu.mFile -tearoff 0
.mTopMenu add cascade -label "File" -menu .mTopMenu.mFile -underline 0
.mTopMenu.mFile add command -label "New"         -command newGame -underline 0 -accelerator "Ctrl+N"
.mTopMenu.mFile add command -label "Open"        -command doOpen  -underline 0 -accelerator "Ctrl+O"
.mTopMenu.mFile add command -label "Add Report"  -command doAdd   -underline 0 -state disabled
.mTopMenu.mFile add command -label "Import Map"   -command importMap -underline 0 -state disabled
.mTopMenu.mFile add command -label "Save Orders" -command saveOrders -underline 0 -state disabled
.mTopMenu.mFile add command -label "Export Map"   -command exportMap -underline 0 -state disabled
.mTopMenu.mFile add separator
.mTopMenu.mFile add command -label "Exit"        -command exit    -underline 1 -accelerator "Ctrl+Q"

proc enableMenus {} {
	.mTopMenu.mFile entryconfigure 2 -state normal
	.mTopMenu.mFile entryconfigure 3 -state normal
	.mTopMenu.mFile entryconfigure 4 -state normal
	.mTopMenu.mFile entryconfigure 5 -state normal
}

# action menu
menu .mTopMenu.mAction -tearoff 0
.mTopMenu add cascade -label "Action" -menu .mTopMenu.mAction -underline 0
.mTopMenu.mAction add command -label "Mark active hexes" -command markActive -underline 0
.mTopMenu.mAction add command -label "Form new unit" -command formUnit -underline 5
.mTopMenu.mAction add command -label "Split unit" -command splitUnit -underline 0
.mTopMenu.mAction add command -label "Check Orders" -command checkAllOrders -underline 0

# view menu
menu .mTopMenu.mView -tearoff 0
.mTopMenu add cascade -label "View" -menu .mTopMenu.mView -underline 0
.mTopMenu.mView add checkbutton -label "All Hexes" -command toggleDrawAll -underline 0 -variable gui::draw_all
.mTopMenu.mView add separator
.mTopMenu.mView add command -label "Battles" -command showBattles -underline 0
.mTopMenu.mView add command -label "Events" -command showEvents -underline 0
.mTopMenu.mView add command -label "Find units" -command searchUnits -underline 0
.mTopMenu.mView add command -label "Foreign Units" -command findForeignUnits -underline 1
.mTopMenu.mView add command -label "Idle Units" -command findIdleUnits -underline 0
.mTopMenu.mView add command -label "Keep Outs" -command showKeepOuts -underline 0
.mTopMenu.mView add command -label "My Units" -command showAllUnits -underline 3
.mTopMenu.mView add command -label "Not Done" -command showNotDone -underline 0
.mTopMenu.mView add command -label "Production Count" -command {tk_messageBox -message [ctProd]} -underline 0
.mTopMenu.mView add command -label "Taxers" -command reportTax -underline 0
.mTopMenu.mView add command -label "Resources" -command reportResources -underline 0
.mTopMenu.mView add command -label "Unclaimed" -command {
	tk_messageBox -message [format {$%d} [db onecolumn {
		SELECT val FROM notes WHERE key="unclaimed"
	}]]
} -underline 0

# game data
menu .mTopMenu.mData -tearoff 0
.mTopMenu add cascade -label "Data" -menu .mTopMenu.mData -underline 0
.mTopMenu.mData add command -label "Items" -command itemView -underline 0
.mTopMenu.mData add command -label "Objects" -command showObjectDefs -underline 2
.mTopMenu.mData add command -label "Skills" -command showSkills -underline 0

# help menu
menu .mTopMenu.mHelp -tearoff 0
.mTopMenu add cascade -label "Help" -menu .mTopMenu.mHelp -underline 0
.mTopMenu.mHelp add command -label "Version" -command {tk_messageBox -message "0.3"} -underline 0

.t configure -menu .mTopMenu

### left/right panes
pack [ttk::panedwindow .t.pwMain -orient horizontal] -expand 1 -fill both
.t.pwMain add [ttk::panedwindow .t.pwLeft -orient vertical]
.t.pwMain add [frame .t.fR]

### right frame

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

# top, region description
.t.pwLeft add [text .t.tDesc -width 42 -height 9]

.t.pwLeft add [frame .t.fMarket]
pack [ttk::treeview .t.fMarket.tv -show tree -height 7 \
-yscrollcommand ".t.fMarket.vs set"] -side left -expand 1 -fill both
pack [scrollbar .t.fMarket.vs -command ".t.fMarket.tv yview" \
-orient vertical] -side left -fill y

# next, unit combobox and orders
.t.pwLeft add [frame .t.fLunitOrders]
# make disabled text readable
ttk::style configure AtlUnitCb.TCombobox
ttk::style map AtlUnitCb.TCombobox -foreground [list disabled #4c4c4c]
pack [ttk::combobox .t.cbMyUnits -state readonly -width 45 -exportselection 0 -style AtlUnitCb.TCombobox] \
-side top -in .t.fLunitOrders -expand 1 -fill both
if {$tcl_platform(os) eq "Linux"} {
	# for some reason, the combox is much wider on Linux
	.t.cbMyUnits configure -width 36
}

.t.pwLeft add [frame .t.fSkills]
pack [ttk::treeview .t.tvItems -show tree -height 3 \
-yscrollcommand ".t.fSkills.vs set"] -in .t.fSkills -expand 1 -fill both -side left
pack [scrollbar .t.fSkills.vs -command ".t.tvItems yview" \
-orient vertical] -side left -fill y -side left

# next, unit items (text + scrollbar)
pack [frame .t.fItems] -side top -in .t.fLunitOrders -expand 1 -fill both
pack [text .t.fItems.t -width 40 -height 20 -state disabled \
-yscrollcommand ".t.fItems.vs set"] -side left -expand 1 -fill both

pack [scrollbar .t.fItems.vs -command ".t.fItems.t yview" \
-orient vertical] -side left -fill y

# next, orders box
.t.pwLeft add [text .t.tOrd -width 42 -height 9 -undo 1] -weight 1

### bindings
## canvas
# canvas normally doesn't want focus
bind $w <Enter> {switchFocus %W}
bind .t.tDesc <Enter> {switchFocus %W}
bind .t.tOrd  <Enter> {switchFocus %W}

# bind mousewheel to vertical scrolling
bind $w <MouseWheel> {%W yview scroll [expr %D < 0 ? 1 : -1] units}
bind $w <4> {%W yview scroll -1 units}
bind $w <5> {%W yview scroll 1 units}

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
if {$tcl_platform(os) eq "Linux"} {
	bind $w <KP_Up> {arrow %W up}
	bind $w <KP_Home> {arrow %W ul}
	bind $w <KP_Prior> {arrow %W ur}
	bind $w <KP_Left> {arrow %W lt}
	bind $w <KP_Right> {arrow %W rt}
	bind $w <KP_End> {arrow %W ll}
	bind $w <KP_Next> {arrow %W lr}
	bind $w <KP_Down> {arrow %W dn}
}
bind $w <Up> {arrow %W up}
bind $w <Home> {arrow %W ul}
bind $w <Prior> {arrow %W ur}
bind $w <Left> {arrow %W lt}
bind $w <Right> {arrow %W rt}
bind $w <End> {arrow %W ll}
bind $w <Next> {arrow %W lr}
bind $w <Down> {arrow %W dn}

bind $w <F5> {drawDB %W db} ;# refresh

bind $w <d> {clearNotDoneCur %W}
bind $w <c> {keyCenter %W}
bind $w <n> {formUnit}
bind $w <s> {splitUnit}

## orders
# update orders on unit dropdown change
bind .t.cbMyUnits <<ComboboxSelected>> {showUnit [%W get]}

# redo should be default on Windows, but needed on Linux
bind .t.tOrd <Control-y> {%W edit redo}


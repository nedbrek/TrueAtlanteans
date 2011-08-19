package require Tk
package require sqlite3

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
	mystforest  #004000
	ocean       #000090
	plain       #ffffc0
	swamp       #a0a040
	tunnels     #704018
	tundra      #00ffff
	underforest #00c000
	wasteland   #d88040
}

### game constants
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

set ::men {
	AMAZ
	BARB
	DDWA
	DMAN
	DRLF
	ESKI
	GBLN
	GELF
	GNOL
	GNOM
	HDWA
	HELF
	HILA
	HOBB
	IDWA
	LEAD
	LIZA
	MINO
	NOMA
	ORC
	PLAI
	SELF
	TELF
	TITA
	TMAN
	UDWA
	URUK
	VIKI
	WELF
}

set ::boats {
	Longboat
	Clipper
	Galleon
}

# map from product to skill
set ::production {
	GRAI FARM
	LIVE RANC
	FISH FISH
	WOOD LUMB
	IRON MINI
	MITH MINI
	STON QUAR
	HERB HERB
	LASS HERB
	HORS HORS
	CAME CAME
	GEM  GCUT
	PARM ARMO
	CARM ARMO
	BHAM WEAP
	MSTA WEAP
	MSWO WEAP
	SPEA WEAP
	SWOR WEAP
	XBOW WEAP
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
proc dGet {d k} {
	if {![dict exists $d $k]} { return "" }

	return [string trim [dict get $d $k]]
}

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
	set zlevel $gui::viewLevel
	if {$zlevel == 1} {set zlevel ""}

	return $zlevel
}

proc calcTurnNo {m y} {
	set mN [lsearch $::monthNames $m]

	return [expr ($y-1)*12 + $mN + 1]
}

# return the total number of men in an item list
proc countMen {il} {
	set count 0
	foreach i $il {
		set abbr [string trim [lindex $i 2] {[]}]
		if {[lsearch $::men $abbr] != -1} {
			incr count [lindex $i 0]
		}
	}
	return $count
}

# return index if current orders contain 'str' (-1 on no match)
# e.g. ordersMatch $ol "tax"
# ordersMatch $ol "produce"
# (useful for reports on keeping in faction limits)
proc ordersMatch {ol str} {
	# handle delayed orders
	set inTurn 0
	set idx -1
	foreach o $ol {
		incr idx
		# if in turn block
		if {$inTurn > 0} {
			# only endturn matters
			if {[string match -nocase "endturn" $o]} {
				incr inTurn -1
			}

			continue
		}

		# look for start of turn block
		if {[regexp -nocase {^@?turn\M} $o]} {
			incr inTurn
			continue
		}

		# check for match
		if {[regexp -nocase "^@?$str\\M" $o]} {
			return $idx
		}
	}

	# no match
	return -1
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

	foreach {col row type city ct rid exitDirs} $data {
		if {$type eq "nexus"} {continue}

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
			$w addtag unexplored withtag $hexId
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
				# draw ship icon
			} elseif {!$hasOtherBuild} {
				set hasOtherBuild 1
			}
		}
		if {$hasOtherBuild} {
			$w create text [expr $x+2*$::n] [expr $y] -text "B" \
			  -anchor ne -tags icon
		}
	}

	$w itemconfigure unexplored -stipple gray50

	drawMarkers $w $db

	$w configure -scrollregion [$w bbox all]
}

##############################################################################
### database
proc buildProductDict {maxProducts} {
	set ret ""
	foreach p $maxProducts {
		set key [string trim [lindex $p end] {[]}]
		set val [lindex $p 0]
		dict set ret $key $val
	}
	return $ret
}

# return a list of producers in hex given by 'rid'
# (capped by the maximums in maxProducts)
proc curProduce {rid maxProducts} {
	# pull all the units in the region
	set res [db eval {
		SELECT items,orders,skills
		FROM units
		WHERE regionId=$rid
	}]

	set maxProdDict [buildProductDict $maxProducts]
	set ret ""
	# foreach result
	foreach {il ol sl} $res {
		set idx [ordersMatch $ol "produce"]
		if {$idx != -1} {
			set o [lindex $ol $idx]
			set product [string toupper [lindex $o 1]]
			if {![dict exists $::production $product]} {
				puts "No product $product"
			}
			set numMen [countMen $il]
			dict set ret $product $numMen
		}
	}

	return $maxProdDict
}

# (database available function)
# return amount of tax revenue in hex given by 'rid'
# (capped by maxTax extracted from detail table)
proc curTax {rid maxTax} {
	# ocean hexes have null maxTax
	if {$maxTax eq ""} { return 0 }

	# pull all the units in the region
	set res [db eval {
		SELECT items,orders
		FROM units
		WHERE regionId=$rid
	}]

	# count number of men taxing
	set taxers 0
	foreach {il ol} $res {
		if {[ordersMatch $ol "tax"] != -1} {
			incr taxers [countMen $il]
		}
	}

	# can't tax more than maxTax
	return [expr min($taxers*50, $maxTax)]
}

proc countItem {ils item} {
	foreach il $ils {
		if {[lindex $il 2] eq [format {[%s]} $item]} {
			return [lindex $il 0]
		}
	}

	return 0
}

# helper for updateDb
# process the exits field
# returns a list of all exit directions (for wall processing)
proc doExits {db exits} {
	set dirs ""

	#foreach direction and exit info
	foreach {d e} $exits {
		lappend dirs $d ;# save the exit direction

		# pull the terrain info from the exit info
		set loc [dGet $e Location]
		set x [lindex $loc 0]
		set y [lindex $loc 1]
		set z [lindex $loc 2]

		set ttype  [dGet $e Terrain]
		set city   [dGet $e Town]
		set region [dGet $e Region]

		$db eval {
			INSERT OR REPLACE INTO terrain VALUES
			($x, $y, $z, $ttype, $city, $region);
		}
	}

	return $dirs
}

proc dbInsertUnit {db regionId u} {
	set name   [dGet $u Name]
	set desc   [dGet $u Desc]
	set detail [dGet $u Report]
	set orders [dGet $u Orders]
	set items  [dGet $u Items]
	set skills [dGet $u Skills]
	set flags  [dGet $u Flags]

	$db eval {
		INSERT INTO units
		(regionId, name, desc, detail, orders, items, skills, flags)
		VALUES(
		$regionId, $name, $desc, $detail, $orders, $items, $skills, $flags
		);
	}
	return [$db last_insert_rowid]
}

proc updateDb {db tdata} {
	set turnNo [calcTurnNo [dGet $tdata Month] [dGet $tdata Year]]

	$db eval {BEGIN TRANSACTION}

	set regions [dGet $tdata Regions]
	foreach r $regions {

		set dirs [doExits $db [dGet $r Exits]]

		set loc [dGet $r Location]
		set x [lindex $loc 0]
		set y [lindex $loc 1]
		set z [lindex $loc 2]
		set ttype [dGet $r Terrain]
		if {$ttype eq "nexus"} {continue}

		set city    [dGet $r Town]
		set region  [dGet $r Region]

		$db eval {
			INSERT OR REPLACE INTO terrain VALUES
			($x, $y, $z, $ttype, $city, $region);
		}

		set weather [list [dGet $r WeatherOld] [dGet $r WeatherNew]]
		set wages   [list [dGet $r Wage] [dGet $r MaxWage]]
		set pop     [dGet $r Population]
		set race    [dGet $r Race]
		set tax     [dGet $r MaxTax]
		set wants   [dGet $r Wants]
		set sells   [dGet $r Sells]
		set prod    [dGet $r Products]
		$db eval {
			INSERT OR REPLACE INTO detail
			(x, y, z, turn, weather, wages, pop, race, tax, wants,
			 sells, products, exitDirs)

			VALUES(
			$x, $y, $z, $turnNo, $weather, $wages, $pop, $race, $tax, $wants,
			$sells, $prod, $dirs
			);
		}

		set regionId [$db last_insert_rowid]
		set units [dGet $r Units]
		foreach u $units {
			dbInsertUnit $db $regionId $u
		}

		set objects [dGet $r Objects]
		foreach o $objects {
			set oname [dGet $o Name]
			set odesc [dGet $o ObjectName]
			$db eval {
				INSERT OR REPLACE INTO objects
				(regionId, name, desc)
				VALUES(
				$regionId, $oname, $odesc
				)
			}
			set objectId [$db last_insert_rowid]

			foreach u [dGet $o Units] {
				set unitRow [dbInsertUnit $db $regionId $u]
				$db eval {
					INSERT OR REPLACE INTO object_unit_map
					(objectId, unitId)
					VALUES($objectId, $unitRow)
				}
			}
		}
	}

	$db eval {END TRANSACTION}
}

##############################################################################
### gui
proc orderBoxReset {w} {
	if {$gui::prevUnit ne "" && [$w edit modified]} {
		set orders [split [string trimright [$w get 1.0 end]] "\n"]
		db eval {
			UPDATE units SET orders=$orders
			WHERE id=$gui::prevId
		}
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

	if {$i == -1} {return ""}

	set hexTag [lindex $tags $i]
	regexp {hex_([[:digit:]]+)_([[:digit:]]+)} $hexTag -> x y

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
	if {[lindex $detail 1] != $gui::currentTurn} { return }

	## got it, use it to retrieve the unit
	set regionId [lindex $detail 0]

	set gui::prevUnit $name

	set data [db eval {
		SELECT orders, id, items, skills, detail, flags
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
proc displayRegion {x y} {
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
		SELECT turn, weather, wages, pop, race, tax, id, products, sells, wants
		FROM detail
		WHERE x=$x AND y=$y AND z=$zlevel
		ORDER BY turn DESC LIMIT 1
	}]

	# if no detail info, done
	if {[llength $rdata] == 0} { return }

	$t insert end "Data from turn: [lGet $rdata 0]\n"
	set weather [lindex $rdata 1]
	set wages   [lindex $rdata 2]

	$t insert end "[lGet $rdata 3] peasants "
	$t insert end "([lGet $rdata 4]), \$[lGet $rdata 5].\n"
	$t insert end "------------------------------------\n"
	$t insert end "The weather was [lGet $weather 0] last month;\n"
	$t insert end "it will be [lGet $weather 1] next month.\n"

	$t insert end "Wages: \$[lGet $wages 0] (Max: \$[lGet $wages 1]).\n"

	set regionId [lindex $rdata 6]

	# pull buildings
	set objects [db eval {
		SELECT name, desc FROM objects
		WHERE regionId=$regionId
	}]
	foreach {name desc} $objects {
		$t insert end "$name - $desc\n"
	}

	# region resources for production
	.t.fL.lProd configure -text [join [lindex $rdata 7]]

	# market
	set sells [lindex $rdata 8]
	set wants [lindex $rdata 9]
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

	## don't show units from the past
	if {[lindex $rdata 0] != $gui::currentTurn} { return }

	set units [db eval {
		SELECT name, detail
		FROM units
		WHERE regionId=$regionId
		ORDER BY detail DESC
	}]

	## set up units combox
	set unitList {}
	set state "start"
	foreach {name detail} $units {
		if {$detail eq "own"} {
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
proc selectRegion {w x y} {
	# see if hex is active
	set curTags [$w gettags [format "hex_%d_%d" $x $y]]
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
	$w addtag active withtag [format "hex_%d_%d" $x $y]

	# show active
	$w itemconfigure active -outline red
	$w itemconfigure active -outlinestipple ""
	$w itemconfigure active -width 4
	$w raise active
	$w raise icon

	displayRegion $x $y
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

	selectRegion $w $x $y
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
	if {$i == -1} { return }
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
proc loadData {filename} {
	set tfile [open $filename]
	set tdata [read $tfile]
	close $tfile

	updateDb db [regsub -all {\n} $tdata " "]
}

# helper for "New Game"
proc createGame {filename} {
	wm title .t "True Atlanteans - [file tail $filename]"

	if {[info exists ::db]} {
		::db close
	}
	sqlite3 ::db $filename

	# terrain table: (x, y) -> terrain type
	::db eval {
		CREATE TABLE terrain(
		x TEXT not null,
		y TEXT not null,
		z TEXT not null,
		type not null,
		city not null,
		region not null,
		  unique(x,y,z));
	}

	# detailed table: (x, y) -> turn info gathered, wants?, sells?, weather(cur,
	# next) wage(per, max), region, city
	::db eval {
		CREATE TABLE detail (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			x TEXT not null,
			y TEXT not null, 
			z TEXT not null,
			turn not null,
			weather not null,
			wages not null,
			pop not null,
			race not null,
			tax not null,
			wants not null,
			sells not null,
			products not null,
			exitDirs not null,
			unique(x,y,z,turn)
		);
	}

	# unit table: (regionId -> name description detail (own or foreign) orders
	::db eval {
		CREATE TABLE units (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			regionId INTEGER not null,
			name not null,
			desc not null,
			detail not null,
			orders not null,
			items not null,
			skills not null,
			flags not null,
			FOREIGN KEY (regionId) REFERENCES detail(id)
			  ON DELETE CASCADE
			  ON UPDATE CASCADE
		);
	}

	::db eval {
		CREATE TABLE objects (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			regionId INTEGER not null,
			name not null,
			desc not null,
			FOREIGN KEY (regionId) REFERENCES detail(id)
				ON DELETE CASCADE
				ON UPDATE CASCADE
		);
	}

	::db eval {
		CREATE TABLE object_unit_map (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			objectId INTEGER not null,
			unitId INTEGER not null,
			FOREIGN KEY (objectId) REFERENCES objects(id)
				ON DELETE CASCADE
				ON UPDATE CASCADE,
			FOREIGN KEY (unitId) REFERENCES units(id)
				ON DELETE CASCADE
				ON UPDATE CASCADE
		);
	}

	::db eval {
		CREATE TABLE active_markers(
			x TEXT not null,
			y TEXT not null,
			z TEXT not null,
			done not null,
			unique(x,y,z)
		)
	}

	::db function curTax curTax
	::db function curProduce curProduce
	::db function countItem countItem
}

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
	set ofile [tk_getSaveFile]
	if {$ofile eq ""} { return }

	createGame $ofile
	.t.fR.screen delete all
}

proc doOpen {} {
	set ofile [tk_getOpenFile]
	if {$ofile eq ""} { return }

	wm title .t "True Atlantians - [file tail $ofile]"

	if {[info exists ::db]} {
		::db close
	}
	sqlite3 ::db $ofile
	set res [db eval {SELECT name from sqlite_master}]
	if {[lsearch $res terrain] == -1 ||
	    [lsearch $res detail]  == -1} {
		tk_messageBox -message "Error file $ofile is invalid"
		::db close
		unset ::db
	}

	::db function curTax curTax
	::db function curProduce curProduce

	set gui::currentTurn [db eval {select max(turn) from detail}]
	set gui::viewLevel 1

	drawDB .t.fR.screen db
}

proc doAdd {} {
	set ofile [tk_getOpenFile]
	if {$ofile eq ""} { return }

	loadData $ofile

	set gui::currentTurn [db eval {select max(turn) from detail}]

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
	db eval {
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
	db eval {DROP TABLE active_markers}

	# create the marker table
	db eval {
		CREATE TABLE active_markers(
			x TEXT not null,
			y TEXT not null,
			z TEXT not null,
			done not null,
			unique(x,y,z)
		)
	}

	# populate it with coordinates where we have units right now
	db eval {
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
	set maxTax [db eval {
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

proc makeUnitListbox {t res} {
	if {![winfo exists $t]} {
		toplevel $t
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

	makeUnitListbox .tForeignUnits $res
}

proc findIdleUnits {} {
	set res [db eval {
		SELECT units.name, detail.x, detail.y, detail.z
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn AND units.detail='own'
		   AND units.orders=''
	}]

	makeUnitListbox .tIdleUnits $res
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
	set res [db eval {
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
	}
	wm title $t "Tax Report ([expr [llength $res]/5] regions)"

	$t.fTop.tl delete 0 end
	foreach {x y z tx max} $res {
		set delta [expr $max - $tx]
		if {$z eq ""} {
			$t.fTop.tl insert end "($x,$y) - $tx ($max - $delta)"
		} else {
			$t.fTop.tl insert end "($x,$y,$z) - $tx ($max - $delta)"
		}
	}
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

proc sortProdList {col isInt} {
	set t .tProduceRegions
	set childList [$t.fTop.tv children {}]
	foreach i $childList {
		set terrain [$t.fTop.tv item $i -text]
		lappend vals [concat $terrain [$t.fTop.tv item $i -values]]
	}

	set command safeLsortIdxI
	if {!$isInt} {
		set command safeLsortIdxS
	}
	lappend command $col

	set vals2 [lsort -decreasing -command $command $vals]

	$t.fTop.tv delete [$t.fTop.tv children {}]
	foreach v $vals2 {
		$t.fTop.tv insert {} end -text [lindex $v 0] -values [lrange $v 1 end]
	}
}

# build the production report window
proc reportProd {} {
	# pull all production values
	set res [db eval {
		SELECT x,y,z,curProduce(id, products) as cp
		FROM detail
		WHERE turn=$gui::currentTurn
	}]

	# build the window
	set t .tProduceRegions

	if {![winfo exists $t]} {
		toplevel $t
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
		$t.fTop.tv heading $i -command [list sortProdList $i [expr $i&1]]
	}
}

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

proc saveOrders {} {
	set filename [format {orders%d.txt} $gui::currentTurn]
	set ofile [tk_getSaveFile -initialfile $filename ]
	if {$ofile eq ""} { return }

	set f [open $ofile "w"]

	set res [db eval {
		SELECT units.name, units.orders
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$gui::currentTurn AND units.detail='own'
	}]

	foreach {u ol} $res {
		if {$ol eq ""} continue

		regexp {\(([[:digit:]]+)\)} $u -> unitNum
		puts $f "unit $unitNum"
		puts $f "[join $ol "\n"]\n"
	}

	close $f
}

proc rightCenter {} {
	recenter .t.fR.screen $gui::rightX $gui::rightY
}

proc recenter {w x y} {
	set width  [winfo width $w]
	set height [winfo height $w]

	set deltaX [expr double($x)/$width]
	set deltaY [expr double($y)/$height]

	if {$deltaX > 0.9} {
		$w xview scroll 4 units
	} elseif {$deltaX > 0.7} {
		$w xview scroll 2 units
	} elseif {$deltaX > 0.5} {
		$w xview scroll 1 units
	} elseif {$deltaX < 0.1} {
		$w xview scroll -4 units
	} elseif {$deltaX < 0.3} {
		$w xview scroll -2 units
	} elseif {$deltaX < 0.5} {
		$w xview scroll -1 units
	}

	if {$deltaY > 0.9} {
		$w yview scroll 4 units
	} elseif {$deltaY > 0.7} {
		$w yview scroll 2 units
	} elseif {$deltaY > 0.5} {
		$w yview scroll 1 units
	} elseif {$deltaY < 0.1} {
		$w yview scroll -4 units
	} elseif {$deltaY < 0.3} {
		$w yview scroll -2 units
	} elseif {$deltaY < 0.5} {
		$w yview scroll -1 units
	}
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
	if {[info exists ::db]} {
		::db close
	}
	origExit
}

##############################################################################
### build the GUI
toplevel .t
#bind .t <Destroy> {exit}
wm title .t "True Atlantians - <no game open>"

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

# reports menu
.mTopMenu.mReports add command -label "Idle Units" -command findIdleUnits -underline 0
.mTopMenu.mReports add command -label "Foreign Units" -command findForeignUnits -underline 0
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
bind $w <KP_Left> {arrow %W lt}
bind $w <KP_Right> {arrow %W rt}
bind $w <Left> {arrow %W lt}
bind $w <Right> {arrow %W rt}
bind $w <KP_End> {arrow %W ll}
bind $w <KP_Next> {arrow %W lr}
bind $w <KP_Down> {arrow %W dn}
bind $w <Down> {arrow %W dn}

bind $w <F5> {drawDB %W db} ;# refresh

bind $w <d> {clearNotDoneCur %W}

## orders
# update orders on unit dropdown change
bind .t.fL.cbMyUnits <<ComboboxSelected>> {showUnit [%W get]}

# redo should be default on Windows, but needed on Linux
bind .t.fL.tOrd <Control-y> {%W edit redo}


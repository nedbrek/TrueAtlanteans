package require Tk
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
	cavern      #f0d800
	desert      #f0d800
	forest      #00c000
	jungle      #205020
	mountain    #704018
	mystforest  #004000
	ocean       #000090
	plain       #ffffc0
	swamp       #a0a040
	tunnels     #704018
	underforest #00c000
	wasteland   #d88040
}

set ::men {
	BARB
	DDWA
	GBLN
	GELF
	GNOM
	HDWA
	HELF
	HILA
	HOBB
	LEAD
	NOMA
	PLAI
	SELF
	TELF
	TMAN
	VIKI
	WELF
}

namespace eval gui {
	set currentTurn 0

	set viewLevel 1

	set prevUnit ""
	set prevId   ""

	set rightX 0
	set rightY 0
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

# return true if current orders contain 'str'
# e.g. ordersMatch $ol "tax"
# ordersMatch $ol "produce"
# (useful for reports on keeping in faction limits)
proc ordersMatch {ol str} {
	# handle delayed orders
	set inTurn 0
	foreach o $ol {
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
		if {[regexp -nocase "^$str\\M" $o]} {
			return 1
		}
	}

	# no match
	return 0
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

	set hexId [plot_hex_full $obj [expr ($x * 3 * $n) + $n] \
	           [expr $yNum * 2 * $nrad3 + $yOff]]

	set tags [$obj itemcget $hexId -tags]
	lappend tags [format "hex_%d_%d" $x $y]
	$obj itemconfigure $hexId -tags $tags
	return $hexId
}

# draw all the regions in the db data
proc drawDB {w db} {
	$w delete all

	set zlevel [getZlevel]

	set data [$db eval {
			SELECT x, y, type, city, detail.turn, detail.id
			FROM terrain left outer join detail
			USING(x,y,z)
			WHERE z=$zlevel
			GROUP BY terrain.x, terrain.y, terrain.z
			ORDER BY detail.turn
	}]

	foreach {x y type city ct rid} $data {
		if {$type eq "nexus"} {continue}

		if {[info exists drawn($x,$y)]} {continue} 
		set drawn($x,$y) ""

		set hexId [plot_hex_num $w $x $y]

		$w itemconfigure $hexId -fill [dict get $::terrainColors $type]

		set c [$w coords $hexId]
		set x [lindex $c 0]
		set y [lindex $c 1]

		# draw city icon
		if {$city ne ""} {
			$w create text [expr $x+$::n] [expr $y+$::nrad3] -text "*" -tags icon
		}

		# show unit flags
		if {$ct == $gui::currentTurn} {
			set res [db eval {
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
		}

		# tag unexplored hexes
		if {$ct eq ""} {
			$w addtag unexplored withtag $hexId
		}
	}

	$w itemconfigure unexplored -stipple gray50
}

##############################################################################
### database
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
		if {[ordersMatch $ol "tax"]} {
			incr taxers [countMen $il]
		}
	}

	# can't tax more than maxTax
	return [expr min($taxers*50, $maxTax)]
}

proc doExits {db exits} {
	foreach {d e} $exits {
		set loc [dGet $e Location]
		set x [lindex $loc 0]
		set y [lindex $loc 1]
		set z [lindex $loc 2]

		set ttype [dGet $e Terrain]
		set city [dGet $e Town]
		set region [dGet $e Region]
		$db eval {
			INSERT OR REPLACE INTO terrain VALUES
			($x, $y, $z, $ttype, $city, $region);
		}
	}
}

proc dbInsertUnit {db regionId u} {
	set name   [dGet $u Name]
	set desc   [dGet $u Desc]
	set detail [dGet $u Report]
	set orders [dGet $u Orders]
	set items  [dGet $u Items]
	set skills [dGet $u Skills]

	$db eval {
		INSERT INTO units
		(regionId, name, desc, detail, orders, items, skills)
		VALUES(
		$regionId, $name, $desc, $detail, $orders, $items, $skills
		);
	}
}

proc updateDb {db tdata} {
	set turnNo [calcTurnNo [dGet $tdata Month] [dGet $tdata Year]]

	$db eval {BEGIN TRANSACTION}

	set regions [dGet $tdata Regions]
	foreach r $regions {

		doExits $db [dGet $r Exits]

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
			 sells, products)

			VALUES(
			$x, $y, $z, $turnNo, $weather, $wages, $pop, $race, $tax, $wants,
			$sells, $prod
			);
		}

		set regionId [$db last_insert_rowid]
		set units [dGet $r Units]
		foreach u $units {
			dbInsertUnit $db $regionId $u
		}

		set objects [dGet $r Objects]
		foreach o $objects {
			foreach u [dGet $o Units] {
				dbInsertUnit $db $regionId $u
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

	$w delete 1.0 end
	$w edit reset
	$w edit modified 0
}

proc getSelectionXY {} {
	set tags [.t.fR.screen gettags active]
	set i [lsearch -regexp $tags {hex_[[:digit:]]+_[[:digit:]]}]

	if {$i == -1} {return ""}

	set hexTag [lindex $tags $i]
	regexp {hex_([[:digit:]]+)_([[:digit:]]+)} $hexTag -> x y

	return [list $x $y]
}

proc showUnit {name} {
	orderBoxReset .t.fL.tOrd

	set w .t.fR.screen

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

	set regionId [lindex $detail 0]

	set gui::prevUnit $name

	set data [db eval {
		SELECT orders, id, items, skills, detail
		FROM units
		WHERE regionId=$regionId AND name=$name
		ORDER BY id
	}]
	set orders      [lindex $data 0]
	set gui::prevId [lindex $data 1]
	set items       [lindex $data 2]
	set skills      [lindex $data 3]
	set detail      [lindex $data 4]

	set t .t.fL.fItems.t
	$t configure -state normal

	$t insert end "Skills: "
	if {$skills eq ""} {
		$t insert end "<none>\n"
	} else {
		foreach s $skills {
			$t insert end "[join $s]\n"
		}
	}
	$t insert end "--------\n"

	foreach i $items {
		$t insert end "[join $i]\n"
	}
	$t configure -state disabled

	foreach o $orders {
		.t.fL.tOrd insert end "$o\n"
	}
	.t.fL.tOrd edit modified 0

	if {$detail eq "own"} {
		.t.fL.tOrd configure -state normal
	} else {
		.t.fL.tOrd configure -state disabled
	}
}

proc unitUpdate {wcb} {
	set name [$wcb get]
	showUnit $name
}

proc displayRegion {x y} {
	# clean up
	orderBoxReset .t.fL.tOrd

	.t.fL.lProd configure -text ""

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
		SELECT turn, weather, wages, pop, race, tax, id, products, sells
		FROM detail
		WHERE x=$x AND y=$y AND z=$zlevel
		ORDER BY turn DESC LIMIT 1
	}]

	# if no detail info, done
	if {[llength $rdata] == 0} { return }

	$t insert end "Data from turn: [lGet $rdata 0]\n"

	$t insert end "[lGet $rdata 3] peasants "
	$t insert end "([lGet $rdata 4]), \$[lGet $rdata 5].\n"
	$t insert end "------------------------------------\n"
	set weather [lindex $rdata 1]
	$t insert end "The weather was [lGet $weather 0] last month;\n"
	$t insert end "it will be [lGet $weather 1] next month.\n"

	set wages [lindex $rdata 2]
	$t insert end "Wages: \$[lGet $wages 0] (Max: \$[lGet $wages 1]).\n"

	.t.fL.lProd configure -text [join [lindex $rdata 7]]

	set sells [lindex $rdata 8]

	# unit processing

	# don't show units from the past
	if {[lindex $rdata 0] != $gui::currentTurn} { return }

	set regionId [lindex $rdata 6]
	set units [db eval {
		SELECT name, detail
		FROM units
		WHERE regionId=$regionId
		ORDER BY detail DESC
	}]

	# set up units combox
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
		unitUpdate .t.fL.cbMyUnits
	}
}

proc findType {w ids type} {
	foreach i $ids {
		if {[$w type $i] eq $type} {
			return $i
		}
	}
	return ""
}

# make region x and y selected in w
proc selectRegion {w x y} {
	# see if hex is active
	set curTags [$w gettags [format "hex_%d_%d" $x $y]]
	set i [lsearch $curTags "active"]
	if {$i != -1} {return}

	# deselect current active
	$w itemconfigure active -outline black
	$w itemconfigure active -width 1

	# move active tag
	$w dtag active
	$w addtag active withtag [format "hex_%d_%d" $x $y]

	# show active
	$w itemconfigure active -outline red
	$w itemconfigure active -width 4
	$w raise active
	$w raise icon

	displayRegion $x $y
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
			FOREIGN KEY (regionId) REFERENCES detail(id)
			  ON DELETE CASCADE
			  ON UPDATE CASCADE
		);
	}

	::db function curTax curTax
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

proc reportTax {} {
	set res [db eval {
		SELECT x,y,z,curTax(id,tax) as ct
		FROM detail
		WHERE turn=$gui::currentTurn
		ORDER BY ct DESC
	}]

	foreach {x y z t} $res {
		if {$t} {
			puts "($x,$y,$z) $t"
		}
	}
}

proc saveOrders {} {
	set ofile [tk_getSaveFile]
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

.mTopMenu add cascade -label "File" -menu .mTopMenu.mFile -underline 0

.mTopMenu.mFile add command -label "New"         -command newGame -underline 0 -accelerator "Ctrl+N"
.mTopMenu.mFile add command -label "Open"        -command doOpen  -underline 0 -accelerator "Ctrl+O"
.mTopMenu.mFile add command -label "Add Report"  -command doAdd   -underline 0
.mTopMenu.mFile add command -label "Save Orders" -command saveOrders -underline 0
.mTopMenu.mFile add separator
.mTopMenu.mFile add command -label "Exit"        -command exit    -underline 1 -accelerator "Ctrl+Q"

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

pack [scrollbar .t.fL.fItems.vs -command ".t.fL.fItems.t yview" -orient vertical] -side left -fill y

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
bind .t.fR.screen <minus> zoomOut
bind .t.fR.screen <KP_Subtract> zoomOut
bind .t.fR.screen <plus> zoomIn
bind .t.fR.screen <KP_Add> zoomIn

## orders
# update orders on unit dropdown change
bind .t.fL.cbMyUnits <<ComboboxSelected>> [list unitUpdate %W]

# redo should be default on Windows, but needed on Linux
bind .t.fL.tOrd <Control-y> {%W edit redo}


package require atlantis_utils
package require sqlite3
package provide atlantis_dbtools 1.0

# men will be auto-populated from db
set ::men {
}

set ::boats {
}

proc parseShips {} {
	db eval {
		SELECT desc
		FROM object_defs
	} {
		set col0 [lindex $desc 0]
		if {[regexp {([^:]*): This is a ship} $col0 -> name]} {
			lappendU ::boats $name
		}
	}
}

proc getMaint {item_list} {
	set maint 0
	foreach i $item_list {
		set abbr [string trim [lindex $i 2] {[]}]
		set item_desc [dGet [db onecolumn {SELECT desc FROM items WHERE abbr=$abbr}] Desc]
		if {[regexp {This race takes (.*) hits to kill} $item_desc -> hits]} {
			set ct [lindex $i 0]
			incr maint [expr {$ct * $hits * 5}]
		}
	}
	return $maint
}

set ::currentTurn 0

# (database available function)
# return amount of tax revenue in hex given by 'rid'
# (capped by maxTax extracted from detail table)
proc curTax {rid maxTax} {
	# ocean hexes have null maxTax
	if {$maxTax eq ""} { return 0 }

	# pull all the units in the region
	set res [::db eval {
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

# return a list of producers in hex given by 'rid'
# (capped by the maximums in maxProducts)
proc curProduce {rid maxProducts} {
	# pull all the units in the region
	set res [::db eval {
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

proc countItem {ils item} {
	foreach il $ils {
		if {[lindex $il 2] eq [format {[%s]} $item]} {
			return [lindex $il 0]
		}
	}

	return 0
}

proc skillLevel {skill_list skill_name} {
	set i [lsearch $skill_list [format {*%s*} $skill_name]]
	if {$i == -1} { return 0 }
	if {[lindex $skill_list $i 0] ne $skill_name && [lindex $skill_list $i 1] ne $skill_name} {
		puts "Mismatch in skillLevel '$skill_list' '$skill_name'"
		exit
	}
	return [lindex $skill_list $i 2]
}

proc registerFunctions {} {
	::db function curTax curTax
	::db function curProduce curProduce
	::db function countItem countItem
	::db function countMen countMen
	::db function dGet dGet
	::db function skillLevel skillLevel
}

proc createObjDef {} {
	::db eval {
		CREATE TABLE object_defs(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			desc TEXT not null unique on conflict replace
		)
	}
}

proc createDb {filename} {
	if {[info exists ::db]} {
		::db close
	}
	sqlite3 ::db $filename

	# settings
	::db eval {
		CREATE TABLE settings(
		id INTEGER PRIMARY KEY,
		version INTEGER,
		player_id INTEGER,
		player_pass TEXT not null,
		geom_top TEXT not null,
		zoom_level INTEGER,
		view_level INTEGER,
		forSale_open INTEGER
		);
	}

	::db eval {
		INSERT INTO settings
		(id, version, player_id, player_pass, geom_top, zoom_level, view_level, forSale_open)
		VALUES(1, 3, 0, "", "", 0, 0, 0)
	}

	::db eval {
		CREATE TABLE notes(
		key TEXT PRIMARY KEY,
		val TEXT not null
		);
	}

	# terrain table: (x, y, z) -> terrain type, city, region name
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

	::db eval {
		CREATE TABLE gates(
		x TEXT not null,
		y TEXT not null,
		z TEXT not null,
		desc TEXT not null,
		  unique(x,y,z));
	}

	# detailed table: (x, y, z) -> turn info gathered, wants?, sells?, weather(cur,
	# next) wage(per, max)
	::db eval {
		CREATE TABLE detail (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			x TEXT not null,
			y TEXT not null, 
			z TEXT not null,
			turn INTEGER not null,
			weather not null,
			wages not null,
			pop not null,
			race not null,
			tax not null,
			entertainment not null,
			wants not null,
			sells not null,
			products not null,
			exitDirs not null,
			  unique(x,y,z,turn)
		);
	}

	::db eval {
		CREATE TABLE nexus_exits (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			dir not null,
			dest not null,
				unique(dir)
		);
	}

	# unit table: (regionId) -> name, description, detail (own or foreign), orders
	::db eval {
		CREATE TABLE units (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			regionId INTEGER not null,
			name not null,
			uid INTEGER not null,
			desc not null,
			faction not null,
			detail not null,
			orders not null,
			items not null,
			skills not null,
			flags not null,
			other,
			FOREIGN KEY (regionId) REFERENCES detail(id)
			  ON DELETE CASCADE
			  ON UPDATE CASCADE
		);
	}

	# object table
	::db eval {
		CREATE TABLE objects (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			regionId INTEGER not null,
			name not null,
			desc not null,
			flags not null,
			FOREIGN KEY (regionId) REFERENCES detail(id)
				ON DELETE CASCADE
				ON UPDATE CASCADE
		);
	}

	# object to unit mappings (what units are in which objects)
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

	# item descriptions (desc is a dict)
	::db eval {
		CREATE TABLE items(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			name TEXT not null,
			abbr TEXT not null unique on conflict replace,
			type TEXT not null,
			desc TEXT not null
		)
	}

	# skill descriptions (desc is a list)
	::db eval {
		CREATE TABLE skills(
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			name TEXT not null,
			abbr TEXT not null,
			level TEXT not null,
			cost TEXT not null,
			desc TEXT not null
		)
	}

	createObjDef

	# active markers
	::db eval {
		CREATE TABLE active_markers(
			x TEXT not null,
			y TEXT not null,
			z TEXT not null,
			done not null,
			  unique(x,y,z)
		)
	}

	registerFunctions
}

proc setMaxXY {} {
	set maxx [::db onecolumn {SELECT val FROM notes WHERE key = "max_x"}]
	if {$maxx eq ""} {
		set maxx [::db onecolumn { SELECT max(cast(x as integer)) FROM terrain WHERE z=1}]
		if {$maxx ne ""} {
			set ::max_x [expr {$maxx + 1}]
		}
	} else {
		set ::max_x $maxx
	}

	set maxy [::db onecolumn {SELECT val FROM notes WHERE key = "max_y"}]
	if {$maxy eq ""} {
		set maxy [::db onecolumn { SELECT max(cast(y as integer)) FROM terrain WHERE z=1}]
		if {$maxy ne ""} {
			set ::max_y [expr {$maxy + 1}]
		}
	} else {
		set ::max_y $maxy
	}
}

proc openDb {ofile} {
	if {[info exists ::db]} {
		::db close
	}
	sqlite3 ::db $ofile
	set res [db eval {SELECT name from sqlite_master}]
	if {[lsearch $res terrain] == -1 ||
	    [lsearch $res detail]  == -1} {
		::db close
		unset ::db
		return "Error file $ofile is invalid"
	}
	set version [db onecolumn {SELECT version FROM settings}]
	if {$version == 1} {
		createObjDef

		db eval {
			ALTER TABLE units
			ADD COLUMN other
		}
		db eval {
			UPDATE settings SET version = 2
		}
	}

	if {$version == 2} {
		::db eval {
			CREATE TABLE gates(
			x TEXT not null,
			y TEXT not null,
			z TEXT not null,
			desc TEXT not null,
		  	  unique(x,y,z));
			UPDATE settings SET version = 3
		}
	}

	registerFunctions

	set ::men [db eval {select abbr from items where type="race"}]
	set ::currentTurn [db eval {select max(turn) from detail}]
	parseShips

	setMaxXY

	return ""
}

proc insertItem {i} {
	set nd [dict get $i "Name"]
	regexp { *([^[]*)  *\[(.*)\]} $nd -> name abbr

	set type [dict get $i "Type"]

	set desc [dict remove $i "Name" "Type"]
	::db eval {
		INSERT INTO items
		(name, abbr, type, desc)
		VALUES($name, $abbr, $type, $desc)
	}
}

proc taxProgressDetailed {} {
	return [::db eval {
		SELECT x, y, z, turn, curTax(id, tax)
		FROM detail
		ORDER BY turn
	}]
}

proc taxProgress {} {
	return [::db eval {
		SELECT turn, sum(curTax(id, tax))
		FROM detail
		GROUP BY turn
	}]
}

proc getUnits {name} {
	return [::db eval {
		SELECT detail.x, detail.y, detail.z, units.name, units.uid
		FROM detail JOIN units
		ON detail.id=units.regionId
		WHERE detail.turn=$::currentTurn and units.detail='own' and units.name LIKE $name
	}]
}

# helper for updateDb
# process the exits field
# returns a list of all exit directions (for wall processing)
proc doExits {db exits rz} {
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

		if {$rz == 0} {
			$db eval {
				INSERT OR REPLACE INTO nexus_exits (dir, dest)
				VALUES ($d, $loc);
			}
		}
	}

	return $dirs
}

proc dbInsertUnit {db regionId u} {
	set name   [dGet $u Name]
	set desc   [dGet $u Desc]
	set fact   [dGet $u Faction]
	set detail [dGet $u Report]
	set orders [dGet $u Orders]
	set items  [dGet $u Items]
	set skills [dGet $u Skills]
	set flags  [dGet $u Flags]
	set can_study [dGet $u CanStudy]
	set combat_spell [dGet $u CombatSpell]
	set other [dict create CanStudy $can_study CombatSpell $combat_spell]

	set r [extractUnitNameNum $name]
	set n [lindex $r 0]
	set uid [lindex $r 1]

	$db eval {
		INSERT INTO units
		(regionId, name, uid, desc, faction, detail, orders, items, skills, flags, other)
		VALUES(
		$regionId, $n, $uid, $desc, $fact, $detail, $orders, $items, $skills, $flags, $other
		);
	}
	return [$db last_insert_rowid]
}

proc insertSkill {s} {
	set name [dGet $s "Name"]
	set abbr [dGet $s "Abbr"]
	set level [dGet $s "Level"]
	set cost [dGet $s "Cost"]
	set desc [dGet $s "Desc"]

	::db eval {
		INSERT INTO skills
		(name, abbr, level, cost, desc)
		VALUES($name, $abbr, $level, $cost, $desc)
	}
}

proc updateDb {db tdata} {
	set pid [dGet $tdata PlayerNum]
	set ppass [dGet $tdata PlayerPass]
	db eval {
		UPDATE settings SET
		player_id = $pid,
		player_pass = $ppass
	}
	set turnNo [calcTurnNo [dGet $tdata Month] [dGet $tdata Year]]

	$db eval {BEGIN TRANSACTION}

	set alignment [dGet $tdata Alignment]
	$db eval {INSERT OR REPLACE INTO notes VALUES("alignment", $alignment)}

	set regions [dGet $tdata Regions]
	foreach r $regions {

		set loc [dGet $r Location]
		set x [lindex $loc 0]
		set y [lindex $loc 1]
		set z [lindex $loc 2]

		set dirs [doExits $db [dGet $r Exits] $z]

		set ttype [dGet $r Terrain]

		set city    [dGet $r Town]
		set region  [dGet $r Region]

		$db eval {
			INSERT OR REPLACE INTO terrain VALUES
			($x, $y, $z, $ttype, $city, $region);
		}

		set gate    [dGet $r Gate]
		$db eval {
			INSERT OR REPLACE INTO gates VALUES
			($x, $y, $z, $gate);
		}

		set weather [list [dGet $r WeatherOld] [dGet $r WeatherNew]]
		set wages   [list [dGet $r Wage] [dGet $r MaxWage]]
		set pop     [dGet $r Population]
		set race    [dGet $r Race]
		set tax     [dGet $r MaxTax]
		set ente    [dGet $r Entertainment]
		set wants   [dGet $r Wants]
		set sells   [dGet $r Sells]
		set prod    [dGet $r Products]
		$db eval {
			INSERT OR REPLACE INTO detail
			(x, y, z, turn, weather, wages, pop, race, tax, entertainment, wants,
			 sells, products, exitDirs)

			VALUES(
			$x, $y, $z, $turnNo, $weather, $wages, $pop, $race, $tax, $ente,
			$wants, $sells, $prod, $dirs
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
			set oflags [dGet $o Flags]
			$db eval {
				INSERT OR REPLACE INTO objects
				(regionId, name, desc, flags)
				VALUES(
				$regionId, $oname, $odesc, $oflags
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

	# items are a list of dict
	set items [dGet $tdata Items]
	foreach item $items {
		insertItem $item
	}

	set skills [dGet $tdata Skills]
	foreach skill $skills {
		insertSkill $skill
	}

	foreach obj_def [dGet $tdata Objects] {
		$db eval {
			INSERT OR REPLACE INTO object_defs
			(desc)
			VALUES($obj_def)
		}
	}

	$db eval {END TRANSACTION}

	set unclaimed [dGet $tdata Unclaimed]
	$db eval {
		INSERT OR REPLACE INTO notes
		VALUES("unclaimed", $unclaimed)
	}

	set ::men [db eval {select abbr from items where type="race"}]
	set ::currentTurn [db eval {select max(turn) from detail}]
	parseShips

	# only store events for latest turn
	if {$turnNo == $::currentTurn} {
		$db eval {DROP TABLE IF EXISTS events}
		$db eval {
			CREATE TABLE events(
			id INTEGER PRIMARY KEY,
			type TEXT not null,
			val TEXT not null
			)
		}

		$db eval {BEGIN TRANSACTION}
		foreach e [dGet $tdata Events] {
			set t [dict get $e TYPE]
			set val [dict remove $e TYPE]
			$db eval {
				INSERT INTO events
				(type, val)
				VALUES($t, $val)
			}
		}
		foreach b [dGet $tdata Battles] {
			$db eval {
				INSERT INTO events
				(type, val)
				VALUES("BATTLE", $b)
			}
		}
		$db eval {END TRANSACTION}
	}
	setMaxXY
}


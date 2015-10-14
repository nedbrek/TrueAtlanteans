source atlantis_utils.tcl

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

proc createDb {filename} {
	if {[info exists ::db]} {
		::db close
	}
	sqlite3 ::db $filename

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

	# detailed table: (x, y, z) -> turn info gathered, wants?, sells?, weather(cur,
	# next) wage(per, max)
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

	# unit table: (regionId) -> name, description, detail (own or foreign), orders
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

	# object table
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

	::db function curTax curTax
	::db function curProduce curProduce
	::db function countItem countItem
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

	::db function curTax curTax
	::db function curProduce curProduce
	::db function countItem countItem
	return ""
}


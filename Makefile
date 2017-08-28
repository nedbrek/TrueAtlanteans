GAME_DIR := ../Atlantis/ceran/cp1

.PHONY: all
all: check

.PHONY: cp1
cp1:
	tclsh computer_player.tcl new cp1

	tclsh computer_player.tcl add cp1 turn1.3
	tclsh computer_player.tcl gen cp1

	tclsh computer_player.tcl add cp1 turn2.3
	tclsh computer_player.tcl gen cp1

	tclsh computer_player.tcl add cp1 turn3.3
	tclsh computer_player.tcl gen cp1

	tclsh computer_player.tcl add cp1 turn4.3

.PHONY: check
check:
	diff $(GAME_DIR)/turn1/report.3 cp1/turn1.3
	diff $(GAME_DIR)/turn2/report.3 cp1/turn2.3
	diff $(GAME_DIR)/turn3/report.3 cp1/turn3.3
	diff $(GAME_DIR)/turn4/report.3 cp1/turn4.3
	diff cp1/orders.2 $(GAME_DIR)/turn2/orders.3
	diff cp1/orders.3 $(GAME_DIR)/turn3/orders.3
	diff cp1/orders.4 $(GAME_DIR)/turn4/orders.3

.PHONY: install_orders
install_orders:
	cp cp1/orders.2 $(GAME_DIR)/turn2/orders.3
	cp cp1/orders.3 $(GAME_DIR)/turn3/orders.3
	cp cp1/orders.4 $(GAME_DIR)/turn4/orders.3

.PHONY: install_turns
install_turns:
	cp $(GAME_DIR)/turn1/report.3 cp1/turn1.3
	cp $(GAME_DIR)/turn2/report.3 cp1/turn2.3
	cp $(GAME_DIR)/turn3/report.3 cp1/turn3.3
	cp $(GAME_DIR)/turn4/report.3 cp1/turn4.3

.PHONY: clean
clean:
	rm -f cp1/game.db cp1/orders.*


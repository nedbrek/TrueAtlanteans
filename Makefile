GAME_DIR := ../Atlantis/ceran/cp1
NUM_TURNS := 5
NUMS_TURNS := $(shell seq 1 $(NUM_TURNS))

.PHONY: all
all: check

.PHONY: cp1
cp1:
	tclsh computer_player.tcl new cp1

	for n in $(NUMS_TURNS) ; do \
		tclsh computer_player.tcl add cp1 turn$$n.3 ; \
		tclsh computer_player.tcl gen cp1 ; \
	done

TURNS := $(addprefix check_turn, $(NUMS_TURNS))

NUMS_ORDERS := $(shell seq 2 $(NUM_TURNS))
ORDERS := $(addprefix check_order, $(NUMS_ORDERS))

.PHONY: check
check: $(TURNS) $(ORDERS)

.PHONY: $(TURNS)
$(TURNS): check_turn%:
	diff $(GAME_DIR)/turn$*/report.3 cp1/turn$*.3

.PHONY: $(ORDERS)
$(ORDERS): check_order%:
	diff cp1/orders.$* $(GAME_DIR)/turn$*/orders.3

IORDERS := $(addprefix install_order, $(NUMS_ORDERS))

.PHONY: install_orders
install_orders: $(IORDERS)

.PHONY: $(IORDERS)
$(IORDERS): install_order%:
	cp cp1/orders.$* $(GAME_DIR)/turn$*/orders.3

ITURNS := $(addprefix install_turn, $(NUMS_TURNS))
.PHONY: $(ITURNS)

.PHONY: install_turns
install_turns: $(ITURNS)

$(ITURNS): install_turn%:
	cp $(GAME_DIR)/turn$*/report.3 cp1/turn$*.3

.PHONY: clean
clean:
	rm -f cp1/game.db cp1/orders.*


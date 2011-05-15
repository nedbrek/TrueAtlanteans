Tcl GUI for Atlantis PBEM
Requires the turn to be in "Tcl Object Notation" (a Tcl dictionary)

For example:
Name {Tester (3)}
FactionType { (War 1, Trade 1, Magic 1) }
Month March
Year 1
VerString {4.1.0}
Rulesetname {Ceran}
Rulesetversion {2.0.4 (beta)}
Newssheet 1
Password {none}
TurnCountdown -1
Quit 0
TaxRegion 0
MaxTax 10
TradeRegion 0

(each entry is "key" <space> "value", with spaces inside the values
protected by {} - all the keys are camel case, and lack spaces)


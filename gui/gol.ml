open Cellular_automata.Two
open Gui
module GoL = MakeBoard (B3_S23)
module GoLGUI = MakeGUI (GoL)

let _ = GoLGUI.run_gui ()
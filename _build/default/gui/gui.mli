open Cellular_automata.Two
open Graphics
module GoL : Board

val grid : GoL.gameboard
(** [grid] is the initialization of a glider configuration of a gameboard. *)

val grid_width : int
(** [grid_width] is the number of cells wide of the displayed grid, equaling the
    width of the gameboard array array stored in [grid]. *)

val grid_height : int
(** [grid_width] is the number of cells high of the displayed grid, equaling the
    length of the gameboard array array stored in [grid]. *)

val cell_size : int
(** [cell_size] is constant storing the height and width of each drawn cell. *)

val window_width : int
(** [window_width] is the width of the opened window in pixels, based from
    [cell_size] and [grid_width]. *)

val window_height : int
(** [window_height] is the height of the opened window in pixels, based from
    [cell_size] and [grid_width]. *)

val button_size : int
(** [button_size] is the constant storing the length and width of the created
    button. *)

val dead_color : color
(** [dead_color] is the constant representing the color of a dead cell
    represented as a hexadecimal color value. *)

val alive_color : color
(** [alive_color] is the constant representing the color of an alive cell
    represented as a hexadecimal color value. *)

val button_text : string
(** [button_text] is the string constant representing the label of the button. *)

val button_font : string
(** [button_font] is the string constant representing the font of the button's
    text. *)

val draw_cell : int -> int -> GoL.state -> unit
(** [draw_cell x y st] is a helper function to [draw_grid g] that creates a
    rectangle at coordinates (x, y) with the color indicated by [st]. *)

val draw_grid : GoL.state array array -> unit
(** [draw_grid g] creates a grid using [draw_cell x y st] as a helper, creating
    a grid of cells of dimension [grid_width] x [grid_height]. *)

val run_gui : unit -> unit
(** [run_gui ()] is the function that opens a window, sets the title, and runs a
    loop to continuously check for state changes. *)

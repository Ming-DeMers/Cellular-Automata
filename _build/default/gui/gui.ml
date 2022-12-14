open Graphics
open Cellular_automata.Two

(* Make the board *)
module GoL = Make (B3_S23)

(** [grid] is the initialization of a glider configuration of a gameboard. *)
let grid = GoL.init_glider ()

(** [grid_width] is the number of cells wide of the displayed grid, equaling the
    width of the gameboard array array stored in [grid]. *)
let grid_width = Array.length grid.(0)

(** [grid_width] is the number of cells high of the displayed grid, equaling the
    length of the gameboard array array stored in [grid]. *)
let grid_height = Array.length grid

(** [cell_size] is constant storing the height and width of each drawn cell. *)
let cell_size = 30

(** [window_width] is the width of the opened window in pixels, based from
    [cell_size] and [grid_width]. *)
let window_width = "800"

(** [window_height] is the height of the opened window in pixels, based from
    [cell_size] and [grid_width]. *)
let window_height = "800"

(** [dead_color] is the constant representing the color of a dead cell
    represented as a hexadecimal color value. *)
let dead_color = 0xCCCCCC

(** [alive_color] is the constant representing the color of an alive cell
    represented as a hexadecimal color value. *)
let alive_color = 0xFFFF00

(** [draw_cell x y st] is a helper function to [draw_grid g] that creates a
    rectangle at coordinates (x, y) with the color indicated by [st]. *)
let draw_cell x y st =
  let color =
    match st with
    | GoL.Dead -> dead_color
    | GoL.Alive -> alive_color
  in
  set_color color;
  fill_rect (x * cell_size) (y * cell_size) cell_size cell_size

let draw_borders x y =
  set_color black;
  draw_rect (x * cell_size) (y * cell_size) cell_size cell_size

(** [draw_grid g] creates a grid using [draw_cell x y st] as a helper, creating
    a grid of cells of dimension [grid_width] x [grid_height]. *)
let draw_grid g =
  for x = 0 to grid_width - 1 do
    for y = 0 to grid_height - 1 do
      draw_cell (x + 8) (y + 8) g.(x).(y);
      draw_borders (x + 8) (y + 8)
    done
  done

(* let mouse_x = fst (mouse_pos ()) let mouse_y = snd (mouse_pos ()) let
   mouse_is_clicked = button_down () *)
let open_string = " " ^ window_width ^ "x" ^ window_height

(** [run_gui ()] is the function that opens a window, sets the title, and runs a
    loop to continuously check for state changes. *)
let run_gui () =
  open_graph open_string;
  set_window_title "Conway's Game of Life";
  display_mode true;
  set_color black;
  while true do
    draw_grid grid;
    Unix.sleep 1;
    GoL.update_board grid
    (* let key = read_key () in match key with | 'q' -> exit 0 | _ -> () *)

    (* loop_at_exit [ Key_pressed; Poll ] (fun e -> if e.button then
       GoL.update_board grid else raise Exit; draw_grid grid *)
    (* let st = wait_next_event [ Key_pressed ] in synchronize (); *)
    (* if st.keypressed then begin Unix.sleep 1; raise Exit *)
    (* end *)
    (* Unix.sleep 1; *)
    (* if st.button then GoL.update_board grid *)
    (* GoL.update_board grid *)
  done

let _ = run_gui ()
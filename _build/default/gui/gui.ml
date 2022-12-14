open Graphics
open Cellular_automata.Two

(* Make the board *)
module GoL = Make (B3_S23)

let grid = GoL.init_glider ()

(* GUI dimensions*)
let grid_width = Array.length grid.(0)
let grid_height = Array.length grid
let cell_size = 30
let window_width = "800"
let window_height = "800"
let button_size = 50

(* GUI Colors *)
let dead_color = 0xCCCCCC
let alive_color = 0xFFFF00

(* GUI Text *)
let button_text = "Next Step"
let button_font = "Sans-serif"

let draw_cell x y st =
  let color =
    match st with
    | GoL.Dead -> dead_color
    | GoL.Alive -> alive_color
  in
  set_color color;
  fill_rect (x * cell_size) (y * cell_size) cell_size cell_size

(* let create_button x y wh = failwith "unimplemented" *)

let draw_grid g =
  for x = 0 to grid_width - 1 do
    for y = 0 to grid_height - 1 do
      draw_cell (x + 8) (y + 8) g.(x).(y)
    done
  done

(* let step g = GoL.update_board g *)
let open_string = " " ^ window_width ^ "x" ^ window_height

let run_gui () =
  open_graph open_string;
  set_window_title "Conway's Game of Life";
  display_mode true;
  (* let i = ref 0 in *)
  (* while !i < 19 do *)
  while true do
    draw_grid grid;
    Unix.sleep 1;
    GoL.update_board grid (* i := !i + 1; *)
  done

(* In utop, run: #require "graphics" then you can run a file with header `open
   Graphics` *)
(* problem: open Two is not currently working. need to make new directory o*)

let _ = run_gui ()

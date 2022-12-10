open Graphics
open Two

(* Make the board *)
module GoL = Make (B3_S23)

let grid = GoL.init_glider ()

(* GUI dimensions*)
let grid_width = Array.length grid.(0)
let grid_height = Array.length grid
let cell_size = 10
let window_width = grid_width * cell_size
let window_height = grid_height * cell_size
let button_size = 20

(* GUI Colors *)
let dead_color = 0xCCCCCC
let alive_color = 0x000000

(* GUI Text *)
let button_text = "Next Step"
let button_font = "Sans-serif"

let draw_cell x y st =
  let color =
    match st with
    | GoL.Dead -> dead_color
    | GoL.Alive -> alive_color
  in
  ignore color;
  fill_rect (x * cell_size) (y * cell_size) cell_size cell_size

let draw_grid g =
  for x = 0 to grid_width - 1 do
    for y = 0 to grid_height - 1 do
      draw_cell x y g.(y).(x)
    done
  done

let run_gui () =
  open_graph "";
  set_window_title "Conway's Game of Life";
  let grid = ref grid in
  while true do
    draw_grid !grid;
    let key = read_key () in
    match key with
    | 'q' -> exit 0
    | _ -> ()
  done

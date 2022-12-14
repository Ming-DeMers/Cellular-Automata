open Graphics
open Cellular_automata.Two

module MakeGUI (B : Board) = struct
  (* GUI dimensions*)
  let grid =
    match B.born_rule with
    | [ 3 ] -> B.init_glider ()
    | [ 3; 6 ] -> B.init_replicator ()
    | [ 3; 4; 6; 7; 8 ] -> B.init_rocket ()
    | [ 2 ] -> B.init_seed ()
    | _ -> failwith "Unsupported Gamerule"

  let grid_width = Array.length grid.(0)
  let grid_height = Array.length grid
  let cell_size = 30
  let window_width = "800"
  let window_height = "800"

  (* GUI Colors *)
  let dead_color = 0xCCCCCC
  let alive_color = 0xFFFF00

  let draw_cell x y st =
    let color =
      match st with
      | B.Dead -> dead_color
      | B.Alive -> alive_color
    in
    set_color color;
    fill_rect (x * cell_size) (y * cell_size) cell_size cell_size

  let draw_borders x y =
    set_color black;
    draw_rect (x * cell_size) (y * cell_size) cell_size cell_size

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

  let run_gui () =
    open_graph open_string;
    let title =
      match B.born_rule with
      | [ 3 ] -> "Conway's Game of Life"
      | [ 3; 6 ] -> "Highlife"
      | [ 3; 4; 6; 7; 8 ] -> "Day and Night"
      | [ 2 ] -> "Seeds"
      | _ -> failwith "Unsupported Gamerule"
    in
    set_window_title title;
    display_mode true;
    set_color black;
    while true do
      draw_grid grid;
      Unix.sleep 1;
      B.update_board grid
    done
end
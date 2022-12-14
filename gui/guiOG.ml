open Graphics
open Cellular_automata.Two

(* Make the board *)
module GoL = MakeBoard (B3_S23)

let grid = GoL.init_glider ()

module MakeGUI (B : Board) = struct
  (* GUI dimensions*)
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
      | GoL.Dead -> dead_color
      | GoL.Alive -> alive_color
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

  let run_gui_glider () =
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

  let _ = run_gui_glider ()
end
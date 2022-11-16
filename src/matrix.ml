type state =
  | Dead
  | Alive

type gameboard = state array array

exception AlreadyAlive
exception AlreadyDead

let init_empty x y = Array.make_matrix x y Dead

(* [state g x y] is the state of the node at coordinate position (x,y) with the
   top left corner being (0, 0), increasing in x and y when moving right and
   down respectively *)
let get g x y = g.(y).(x)

(* [set g x y st] changes the value the node at coordinate position (x,y) to
   st *)
let set g x y st = g.(y).(x) <- st

(* Obviously could just set nodes directly, but the errors here help for
   debugging *)
let birth_node g x y =
  match get g x y with
  | Dead -> set g x y Alive
  | Alive -> raise AlreadyAlive

let kill_node g x y =
  match get g x y with
  | Dead -> raise AlreadyDead
  | Alive -> set g x y Dead

(* O(1) maybe*)
let neighbors g x y =
  let width = Array.length g.(0) in
  let height = Array.length g in
  let count = ref 0 in
  for r = x - 1 to x + 1 do
    for c = y - 1 to y + 1 do
      if
        (not (r = -1 || r = width))
        && (not (c = -1 || c = height))
        && not (r = x && c = y)
      then if get g r c = Alive then count := !count + 1 else ()
      else ()
    done
  done;
  !count

let update_node g x y n =
  match get g x y with
  | Dead -> if n = 3 then birth_node g x y else ()
  | Alive -> if n = 2 || n = 3 then () else kill_node g x y

let update_board g =
  let width = Array.length g.(0) in
  let height = Array.length g in
  let neighbors_matrix = Array.make_matrix width height 0 in
  for r = 0 to width - 1 do
    for c = 0 to height - 1 do
      set neighbors_matrix r c (neighbors g r c)
    done
  done;
  for r = 0 to width - 1 do
    for c = 0 to height - 1 do
      update_node g r c (get neighbors_matrix r c)
    done
  done

let to_list g = Array.to_list g

let rec make_row_string r =
  match r with
  | [] -> ""
  | h :: t -> begin
      match h with
      | Alive -> "◾" ^ make_row_string t
      | Dead -> "◽" ^ make_row_string t
    end

(** Creates a string of the given gameboard *)
let rec gb_list_to_string g =
  match g with
  | [] -> ""
  | h :: t -> make_row_string (to_list h) ^ "\n" ^ gb_list_to_string t

let print_board g = print_endline (g |> to_list |> gb_list_to_string)

let loop g n =
  for num = n downto 1 do
    update_board g;
    print_board g
  done

let init_glider () =
  let b = init_empty 10 10 in
  birth_node b 3 4;
  birth_node b 4 4;
  birth_node b 5 4;
  birth_node b 4 2;
  birth_node b 5 3;
  b

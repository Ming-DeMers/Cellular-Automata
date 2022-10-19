type state =
  | Dead
  | Alive

type gameboard = state list list
(** Two dimensional list of nodes representing a gameboard *)

exception PreconditionViolation of string
(** raised when precondtion is violated*)

(** Helper function for new_gamebaord. Creates a list of x default nodes that
    are dead *)
let rec ng_x lst x =
  match x with
  | 0 -> lst
  | e -> ng_x (Dead :: lst) (e - 1)

(** Helper function for new_gamebaord. Creates a list of x default nodes *)
let rec ng_y outer_lst y inner_lst =
  match y with
  | 0 -> outer_lst
  | e -> ng_y (inner_lst :: outer_lst) (e - 1) inner_lst

(** Creates a new gameboard with dimensions x by y filled with empty nodes.
    Precondition: x,y >= 1 *)
let new_gameboard x y = x |> ng_x [] |> ng_y [] y

(** [init_gameboard g_option] is the initial gameboard. The initial gameboard is
    either [g] if [g_option] is [Some g], otherwise it is some default
    gameboard.

    Right now set to create a 10x10 empty gameboard if none *)
let init_gameboard gb_op =
  match gb_op with
  | None -> new_gameboard 10 10
  | Some g -> g

(** Creates a string of the given row *)
let rec make_row_string r =
  match r with
  | [] -> ""
  | h :: t -> (
      match h with
      | Alive -> "◾" ^ make_row_string t
      | Dead -> "◽" ^ make_row_string t)

(** Creates a string of the given gameboard *)
let rec make_gb_string gb =
  match gb with
  | [] -> ""
  | h :: t -> make_row_string h ^ "\n" ^ make_gb_string t

(** Prints the given gameboard *)
let print_board gb = print_endline (make_gb_string gb)

(** Is the width of given gameboard *)
let get_gb_width gb =
  match gb with
  | [] -> 0
  | h :: t -> List.length h

(** Is the height of given gameboard *)
let get_gb_height gb = List.length gb

(** Is the node with coordinate (x,y) in the given gameboard *)
let get_node gb x y =
  let index = (get_gb_width gb * (y - 1)) + x in
  let flat = List.flatten gb in
  List.nth flat index

(** Is 0 if neighbor to the north is dead, 1 if alive *)
let check_north gb x y =
  if y = 1 then 0
  else
    let n = get_node gb x (y - 1) in
    match n with
    | Dead -> 0
    | Alive -> 1

(** Is 0 if neighbor to the south is dead, 1 if alive *)
let check_south gb x y =
  if y = get_gb_height gb then 0
  else
    let n = get_node gb x (y + 1) in
    match n with
    | Dead -> 0
    | Alive -> 1

(** Is 0 if neighbor to the east is dead, 1 if alive *)
let check_east gb x y =
  if x = get_gb_width gb then 0
  else
    let n = get_node gb (x + 1) y in
    match n with
    | Dead -> 0
    | Alive -> 1

(** Is 0 if neighbor to the west is dead, 1 if alive *)
let check_west gb x y =
  if x = 1 then 0
  else
    let n = get_node gb (x - 1) y in
    match n with
    | Dead -> 0
    | Alive -> 1

(** Is 0 if neighbor to the northwest is dead, 1 if alive *)
let check_nw gb x y =
  if y = 1 || x = 1 then 0
  else
    let n = get_node gb (x - 1) (y - 1) in
    match n with
    | Dead -> 0
    | Alive -> 1

(** Is 0 if neighbor to the northeast is dead, 1 if alive *)
let check_ne gb x y =
  if y = 1 || x = get_gb_width gb then 0
  else
    let n = get_node gb (x + 1) (y - 1) in
    match n with
    | Dead -> 0
    | Alive -> 1

(** Is 0 if neighbor to the southwest is dead, 1 if alive *)
let check_sw gb x y =
  if y = get_gb_height gb || x = 1 then 0
  else
    let n = get_node gb (x - 1) (y + 1) in
    match n with
    | Dead -> 0
    | Alive -> 1

(** Is 0 if neighbor to the southeast is dead, 1 if alive *)
let check_se gb x y =
  if y = get_gb_height gb || x = get_gb_width gb then 0
  else
    let n = get_node gb (x + 1) (y + 1) in
    match n with
    | Dead -> 0
    | Alive -> 1

(** [get_neighbors g x y] is the number of neighbors that the node located at
    position ([x], [y]) on the grid has.

    Precondition: (x,y) must be a valid position in the grid *)
let get_neighbors gb x y =
  check_north gb x y + check_south gb x y + check_east gb x y
  + check_west gb x y + check_nw gb x y + check_ne gb x y + check_sw gb x y
  + check_se gb x y

let update_node gb x y =
  let n = get_neighbors gb x y in
  match get_node gb x y with
  | Alive -> if n = 2 || n = 3 then Alive else Dead
  | Dead -> if n = 3 then Alive else Dead

(** [get_head gb] is the first element of [gb] or [\[\]] if empty. *)
let get_head gb =
  match gb with
  | [] -> []
  | h :: _ -> h

(** [get_head gb] is [gb] without the first element or [\[\]] if empty. *)
let get_tail gb =
  match gb with
  | [] -> []
  | _ :: t -> t

let rec update_board gb x y acc =
  let new_node = update_node gb x y in
  match (x, y) with
  | 1, 1 -> (new_node :: get_head acc) :: get_tail acc
  | 1, y ->
      update_board gb (get_gb_width gb) (y - 1) ([ new_node ] :: get_tail acc)
  | x, _ ->
      update_board gb (x - 1) y ((new_node :: get_head acc) :: get_tail acc)

let loop gb iterations = raise (Failure "Unimplemented")
let turn gb = raise (Failure "unimplemented")
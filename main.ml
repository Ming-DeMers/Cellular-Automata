type node = bool * int
(** bool is true if alive, false if not. int is number of neighbors *)

type gameboard = node list list
(** Two dimensional list of nodes representing a gameboard *)

exception PreconditionViolation of string
(** raised when precondtion is violated*)

(** Helper function for new_gamebaord. Creates a list of x default nodes*)
let rec ng_x lst x =
  match x with
  | 0 -> lst
  | e -> ng_x ((false, 0) :: lst) (e - 1)

(** Helper function for new_gamebaord. Creates a list of x default nodes*)
let rec ng_y outer_lst y inner_lst =
  match y with
  | 0 -> outer_lst
  | e -> ng_y (inner_lst :: outer_lst) (e - 1) inner_lst

(** Creates a new gameboard with dimensions x by y filled with empty nodes.
    Precondition: x,y >= 1*)
let new_gameboard x y = x |> ng_x [] |> ng_y [] y

(** [init_gameboard g_option] is the initial gameboard. The initial gameboard is
    either [g] if [g_option] is [Some g], otherwise it is some default
    gameboard.

    Right now set to create a 10x10 empty gameboard if none *)
let init_gameboard gb_op =
  match gb_op with
  | None -> new_gameboard 10 10
  | Some g -> g

(** Creates a string of the given row*)
let rec make_row_string r =
  match r with
  | [] -> ""
  | h :: t -> (
      match h with
      | true, _ -> "◾" ^ make_row_string t
      | false, _ -> "◽" ^ make_row_string t)

(** Creates a string of the given gameboard*)
let rec make_gb_string gb =
  match gb with
  | [] -> ""
  | h :: t -> make_row_string h ^ "\n" ^ make_gb_string t

(** Prints the given gameboard*)
let print_board gb = print_endline (make_gb_string gb)

(**◽◾\n*)
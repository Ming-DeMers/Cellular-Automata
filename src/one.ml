type state =
  | Dead
  | Alive

(* Type bit is the integer represntation of a bit: either 0 or 1 *)
type bit = int
type byte = bit list

(* The byte of a bit, represents the upto 2^8 or, 256 possible rules. *)
type rule = byte
type gameboard = state array

type game_rule = state array * rule
(** Two dimensional array of nodes representing a gameboard. Top left corner is
    (0, 0), increasing in x and y when moving right and down respectively *)

type 'a gen = Cons of 'a * 'a gen

exception AlreadyAlive
exception AlreadyDead

let rec make_n lst n = if List.length lst < n then make_n (0 :: lst) n else lst

(* [int_to_binary int] returns the but list representation of a string. For the
   purpose of this program, binarys from 0-7 are represented as a three bit
   list.*)
let int_to_binary int =
  if int = 0 then [ 0 ]
  else
    let rec int_to_binary' int acc =
      if int = 0 then acc
      else
        let remainder = int mod 2 in
        int_to_binary' (int / 2) (remainder :: acc)
    in
    int_to_binary' int []

(* [binary_to_int binary] is thje int representation of a binary*)
let binary_to_int binary =
  let rec binary_to_int' binary acc =
    match binary with
    | [] -> acc
    | h :: t -> binary_to_int' t ((acc * 2) + h)
  in
  binary_to_int' binary 0

(* [int_to_rule n] is the byte that represents a rule. For example, [int_to_rule
   90] = [0; 1; 0; 1; 1; 0; 1; 0] *)
let int_to_rule (n : int) : rule =
  let rule = int_to_binary n in
  make_n rule 8

(* [init_empty x] is a gameboard with a single alive node in the middle of the
   board. *)

let init_empty x : gameboard =
  let ary = Array.make x Dead in
  ary.(x / 2) <- Alive;
  ary

(* [gb_to_string gb] converts the row of a gameboard into a string with squares
   that represent the alive or dead state of a node.*)
let gb_to_string gb =
  Array.to_list gb
  |> List.map (fun x ->
         match x with
         | Dead -> "◾"
         | Alive -> "◽")
  |> List.fold_left ( ^ ) ""

let print_board (gb : gameboard) = print_endline (gb_to_string gb)

(** [neighbors gb x] is the number of alive neighbors that the node located at
    position ([x]). Neighbors are the left and right nodes, thus there can be
    none, one, or two neightbors Requires: ([x]) is a positive integer. *)
let neighbors gb x : int =
  if gb.(x - 1) = Alive then 1 else 0 + if gb.(x + 1) = Alive then 1 else 0

(** [update_node gb x n] updates the node at ([x]) in gameboard g with n
    neighbors in the previous generation to be dead or alive for the next
    generation, based on its number neighbors and according to the specified
    rules.

    Precondition: (x) is a positive integer. *)
let update_node gb rule x =
  let bin_rule = int_to_binary rule in
  match binary_to_int gb.(x - 1) with
  | 7 -> if List.nth bin_rule 0 = 1 then Alive else Dead
  | 6 -> if List.nth bin_rule 1 = 1 then Alive else Dead
  | 5 -> if List.nth bin_rule 2 = 1 then Alive else Dead
  | 4 -> if List.nth bin_rule 3 = 1 then Alive else Dead
  | 3 -> if List.nth bin_rule 4 = 1 then Alive else Dead
  | 2 -> if List.nth bin_rule 5 = 1 then Alive else Dead
  | 1 -> if List.nth bin_rule 6 = 1 then Alive else Dead
  | 0 -> if List.nth bin_rule 7 = 1 then Alive else Dead
  | _ -> raise (Failure "Invalid rule")

(** [update_board gb] updates gameboard gb to the next generation *)
let update_board gb =
  ignore gb;
  raise (Failure "Unimplemented")

let loop (gb : gameboard) x =
  ignore x;
  ignore gb;
  raise (Failure "Unimplemented")

(** [birth_node gb x] checks the state of the node at position [x] in gameboard
    gb. If that node is dead, it is updated to be alive. Raises AlreadyAlive if
    the node at position [x], is already alive.

    Precondition: [x] is a valid positive integer*)
let birth_node gb x =
  ignore x;
  ignore gb;
  raise (Failure "Unimplemented")

(** [kill_node gb x] checks the state of the node at grid position [x] in
    gameboard gb. If that node is alove, it is updated to be dead. Raises
    AlreadyDead if the node at position [x] is already alive.contents
    Precondition: [x] is a valid positive integer. *)
let kill_node gb x =
  ignore x;
  ignore gb;
  raise (Failure "Unimplemented")

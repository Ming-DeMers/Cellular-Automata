type state =
  | Dead
  | Alive

(* Type bit is the integer represntation of a bit: either 0 or 1 *)
type bit = int
type byte = bit list

(* The byte of a bit, represents the upto 2^8 or, 256 possible rules. *)
type rule = byte
type gameboard = state array
type gamerule = state array * rule
(* Two dimensional array of nodes representing a gameboard. Top left corner is
   (0, 0), increasing in x and y when moving right and down respectively *)

type 'a gen = Cons of 'a * 'a gen

exception AlreadyAlive
exception AlreadyDead

(* [make_n] prepends 0s to the front of a list to achieve n length. *)
let rec make_n lst n = if List.length lst < n then make_n (0 :: lst) n else lst

(* [make_end_n] appends 0s to the back of a list to achieve n length. *)
let rec make_end_n lst n =
  if List.length lst < n then make_end_n (lst @ [ 0 ]) n else lst

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

    int_to_binary' int [] |> List.rev

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

(* [gb_to_byte] converts a gameboard into its byte representation, where Dead is
   0 and Alive is 1. *)
let gb_to_byte (gb : gameboard) : bit list =
  let rec gb_to_byte' acc count =
    if count = Array.length gb then acc
    else
      match gb.(count) with
      | Alive -> gb_to_byte' (1 :: acc) (count + 1)
      | Dead -> gb_to_byte' (0 :: acc) (count + 1)
  in
  gb_to_byte' [] 0 |> List.rev

(* [init_empty x] is a gameboard with a single alive node in the middle of the
   board. *)
let init_empty x : gameboard =
  if x = 0 then raise (Failure "Invalid board size")
  else
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
let state (gb : gameboard) x = gb.(x)

(* [neighbors gb x] is the number of alive neighbors that the node located at
   position ([x]). Neighbors are the left and right nodes, thus there can be
   none, one, or two neightbors Requires: ([x]) is a positive integer. *)
let neighbors (gb : gameboard) x =
  let rec neighbors' gb x count acc =
    if count = 3 then acc
    else
      match gb.(x - 1) with
      | Alive -> neighbors' gb (x + 1) (count + 1) (acc + 1)
      | Dead -> neighbors' gb (x + 1) (count + 1) acc
  in
  neighbors' gb x 0 0

let neighborhood (gb : gameboard) x : state array =
  let array = Array.make 3 Dead in
  array.(0) <- gb.(x - 1);
  array.(1) <- gb.(x);
  array.(2) <- gb.(x + 1);
  array

(* [next_gen gb] is the next generation of gameboard [gb] according to the
   specified rules. *)

(* [birth_node gb x] checks the state of the node at position [x] in gameboard
   gb. If that node is dead, it is updated to be alive. Raises AlreadyAlive if
   the node at position [x], is already alive.

   Precondition: [x] is a valid positive integer*)
let birth_node gb x =
  if gb.(x) = Alive then raise AlreadyAlive else gb.(x) <- Alive

(* [kill_node gb x] checks the state of the node at grid position [x] in
   gameboard gb. If that node is alive, it is updated to be dead. Raises
   AlreadyDead if the node at position [x] is already alive.contents
   Precondition: [x] is a valid positive integer. *)
let kill_node gb x = if gb.(x) = Dead then raise AlreadyDead else gb.(x) <- Dead
let make_rule rule = make_end_n (int_to_binary rule) 8 |> List.rev

(* [update_node gb x n] updates the node at ([x]) in gameboard g with n
   neighbors in the previous generation to be dead or alive for the next
   generation, based on its number neighbors and according to the specified
   rules.

   Precondition: (x) is a positive integer. *)
let update_node (gb : gameboard) rule x =
  let neighborhood = neighborhood gb x in
  let rule = make_rule rule in
  match neighborhood |> gb_to_byte |> binary_to_int with
  | 7 -> if List.nth rule 0 = 1 then Alive else Dead
  | 6 -> if List.nth rule 1 = 1 then Alive else Dead
  | 5 -> if List.nth rule 2 = 1 then Alive else Dead
  | 4 -> if List.nth rule 3 = 1 then Alive else Dead
  | 3 -> if List.nth rule 4 = 1 then Alive else Dead
  | 2 -> if List.nth rule 5 = 1 then Alive else Dead
  | 1 -> if List.nth rule 6 = 1 then Alive else Dead
  | 0 -> if List.nth rule 7 = 1 then Alive else Dead
  | _ -> raise (Failure "Invalid neighborhood")

(* [update_board gb] updates gameboard gb to the next generation *)
let update_board (gb : gameboard) rule =
  let new_gb = init_empty (Array.length gb) in
  for i = 1 to Array.length gb - 2 do
    new_gb.(i) <- update_node gb rule i
  done;
  new_gb

let gb_to_string (gb : gameboard) =
  Array.to_list gb
  |> List.map (fun x ->
         match x with
         | Dead -> "◽"
         | Alive -> "◾")
  |> List.fold_left ( ^ ) ""

let print_board (gb : gameboard) = print_endline (gb_to_string gb)

let rec loop gb rule x =
  if x = 0 then gb else loop (update_board gb rule) rule (x - 1)

let rec print_loop gb rule x =
  if x = 0 then print_board (init_empty (Array.length gb))
  else
    let new_gb = update_board gb rule in
    print_board new_gb;
    print_loop new_gb rule (x - 1)
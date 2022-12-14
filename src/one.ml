type state =
  | Dead
  | Alive

type bit = int
type byte = bit list
type rule = byte
type gameboard = state array
(* type 'a gen = Cons of 'a * 'a gen *)

exception AlreadyAlive
exception AlreadyDead

(* [make_n lst n] prepends 0s to the front of a list to achieve n length.
   Requires: [lst] is a bit list of length <= n. *)
(* let rec make_n lst n = if List.length lst < n then make_n (0 :: lst) n else
   lst *)

(* [make_end_n lst n] appends 0s to the back of a list to achieve n length.
   Requires: [lst] is a bit list of length <= n. *)
let rec make_end_n lst n =
  if List.length lst < n then make_end_n (lst @ [ 0 ]) n else lst

(* [int_to_binary i] returns the byte representation of a string. For the
   purpose of this program, binary numbers 0-7 are representated as a 3-bit
   list. *)
let int_to_binary i =
  if i = 0 then [ 0 ]
  else
    let rec int_to_binary' i acc =
      if i = 0 then acc
      else
        let remainder = i mod 2 in
        int_to_binary' (i / 2) (remainder :: acc)
    in
    int_to_binary' i [] |> List.rev

(* [binary_to_int binary] is the int representation of binary, of type byte. *)
let binary_to_int binary =
  let rec binary_to_int' binary acc =
    match binary with
    | [] -> acc
    | h :: t -> binary_to_int' t ((acc * 2) + h)
  in
  binary_to_int' binary 0

(* [int_to_rule n] is the byte that represents a rule, stemming from a bit
   input. For example, [int_to_rule 90 = \[0; 1; 0; 1; 1; 0; 1; 0\]]*)
(* let int_to_rule (n : int) : rule = let rule = int_to_binary n in make_n rule
   8 *)

(* [gb_to_byte gb] converts gameboard [gb] to its byte representation, where
   Dead has a value of 0, and Alive has a value of 1. *)
let gb_to_byte (gb : gameboard) : bit list =
  let rec gb_to_byte' acc count =
    if count = Array.length gb then acc
    else
      match gb.(count) with
      | Alive -> gb_to_byte' (1 :: acc) (count + 1)
      | Dead -> gb_to_byte' (0 :: acc) (count + 1)
  in
  gb_to_byte' [] 0 |> List.rev

let init_empty x : gameboard =
  if x = 0 then raise (Failure "Invalid board size")
  else
    let ary = Array.make x Dead in
    ary.(x / 2) <- Alive;
    ary

(* [gb_to_string gb] converts the row of a gameboard into a string with squares
   that represent the alive/dead state of a node. *)
let gb_to_string gb =
  Array.to_list gb
  |> List.map (fun x ->
         match x with
         | Dead -> "◾"
         | Alive -> "◽")
  |> List.fold_left ( ^ ) ""

(* let print_board (gb : gameboard) = print_endline (gb_to_string gb) *)

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
  let rec make_nb count =
    if count = 3 then array
    else
      let st =
        if x = 0 && count = 0 then gb.(Array.length gb - 1)
        else gb.((x - 1 + count) mod Array.length gb)
      in
      array.(count) <- st;
      make_nb (count + 1)
  in
  make_nb 0

let birth_node gb x =
  if gb.(x) = Alive then raise AlreadyAlive else gb.(x) <- Alive

let kill_node gb x = if gb.(x) = Dead then raise AlreadyDead else gb.(x) <- Dead

(* [make_rule rule b] creates a type rule of the appropriate length containing
   the rule of a converted integer input. Requires: [b] is a positive
   integer. *)
let make_rule b = make_end_n (int_to_binary b) 8 |> List.rev

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

let update_board (gb : gameboard) rule =
  let new_gb = init_empty (Array.length gb) in
  for i = 0 to Array.length gb - 1 do
    new_gb.(i) <- update_node gb rule i
  done;
  new_gb
(* let gb_to_string (gb : gameboard) = Array.to_list gb |> List.map (fun x ->
   match x with | Dead -> "⬛" | Alive -> "⬜") |> List.fold_left ( ^ ) "" *)

let print_board (gb : gameboard) = print_endline (gb_to_string gb)

(* let rec loop gb rule x = if x = 0 then gb else loop (update_board gb rule)
   rule (x - 1) *)

let rec print_loop gb rule x =
  let new_gb = update_board gb rule in
  if x = 0 then print_board gb
  else (
    print_board gb;
    print_loop new_gb rule (x - 1))

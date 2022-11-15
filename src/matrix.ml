include Samples

type state =
  | Dead
  | Alive

type gameboard = state array array

exception AlreadyAlive
exception AlreadyDead

let init_gameboard x y = Array.make_matrix x y dead
let rec make_row_string r = failwith "TODO"
let gb_to_string g = failwith "TODO"
let print_board g = failwith "TODO"
let neighbors g x y = failwith "TODO"
let update_node g x y = failwith "TODO"
let update_board g = failwith "TODO"
let loop g n = failwith "TODO"

(* Obviously could abstract with a flip node function, but the errors here
   ensure we are using the right function for the right purposes*)
let birth_node g x y =
  match g.(x).(y) with
  | Dead -> g.(x).(y) <- Alive
  | Alive -> raise AlreadyAlive

let kill_node g x y =
  match g.(x).(y) with
  | Dead -> raise AlreadyDead
  | Alive -> g.(x).(y) <- Dead

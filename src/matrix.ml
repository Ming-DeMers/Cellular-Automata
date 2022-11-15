include Samples

type state =
  | Dead
  | Alive

type gameboard = state array array

exception AlreadyAlive
exception AlreadyDead

let init_gameboard x y = failwith "TODO"
let print_board g = failwith "TODO"
let neighbors g x y = failwith "TODO"
let update_node g x y = failwith "TODO"
let update_board g = failwith "TODO"
let loop g n = failwith "TODO"
let birth_node x y = failwith "TODO"
let kill_node x y = failwith "TODO"

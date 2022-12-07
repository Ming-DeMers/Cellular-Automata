type state =
  | Dead
  | Alive

type gameboard = state array array
(** Two dimensional array of nodes representing a gameboard. Top left corner is
    (0, 0), increasing in x and y when moving right and down respectively *)

exception AlreadyAlive
exception AlreadyDead

let init_empty x = failwith "Unimplemented"

(** [print_board gb] prints [gb]. *)
let print_board gb = failwith "Unimplemented"

(** [neighbors gb x] is the number of alive neighbors that the node located at
    position ([x]). Neighbors are the left and right nodes, thus there can be
    none, one, or two neightbors Requires: ([x]) is a positive integer. *)
let neighbors gb x = failwith "Unimplemented"

let update_node gb x = "Unimplemented"
let update_node gb = "Unimplemented"
let loop gb x = "Unimplemented"
let birth_node gb x = "Unimplemented"
let kill_node gb x = failwith "Unimplemented"

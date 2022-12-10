type state =
  | Dead
  | Alive

type gameboard = state array array
(** Two dimensional array of nodes representing a gameboard. Top left corner is
    (0, 0), increasing in x and y when moving right and down respectively *)

type 'a gen = Cons of 'a * 'a gen

exception AlreadyAlive
exception AlreadyDead

let init_empty x =
  ignore x;
  raise (Failure "Unimplemented")

(** [print_board gb] prints [gb]. *)
let print_board gb =
  ignore gb;
  raise (Failure "Unimplemented")

(** [neighbors gb x] is the number of alive neighbors that the node located at
    position ([x]). Neighbors are the left and right nodes, thus there can be
    none, one, or two neightbors Requires: ([x]) is a positive integer. *)
let neighbors gb x =
  ignore x;
  ignore gb;
  raise (Failure "Unimplemented")

let update_node gb x =
  ignore x;
  ignore gb;
  raise (Failure "Unimplemented")

let update_board gb =
  ignore gb;
  raise (Failure "Unimplemented")

let loop gb x =
  ignore x;
  ignore gb;
  raise (Failure "Unimplemented")

let birth_node gb x =
  ignore x;
  ignore gb;
  raise (Failure "Unimplemented")

let kill_node gb x =
  ignore x;
  ignore gb;
  raise (Failure "Unimplemented")

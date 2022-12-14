type state =
  | Dead
  | Alive  (** [state] is the dead/alive status of a node. *)

type bit = int
(** [bit] is the int representation of a bit -- either 0 or 1. *)

type byte = bit list
(** [byte] is a list representation of 8 bits, representing 2^8 = 256 possible
    rules. *)

type rule = byte
(** [rule] is a byte representation of the game's rules, typically inputted by
    the user. *)

type gameboard = state array
(** [gameboard] represents each node's state (dead or alive) on the gameboard. *)

exception AlreadyAlive
(** Raised if trying to update an alive node's state to alive. *)

exception AlreadyDead
(** Raised if updating a dead node's state to dead. *)

val init_empty : bit -> gameboard
(** [init_empty x] initializes a gameboard with a single alive node in the
    middle of the board. *)

(* val print_board : gameboard -> unit *)
(** [print_board gb] prints the gameboard [gb] to terminal, using black squares
    to represent dead and white squares to represent alive.*)

val neighbors : gameboard -> bit -> bit
(** [neighbors gb x] is the number of alive neighbors of the node located at
    position [x]. Neighbors are the left and right nodes, thus there can be 0,
    1, or 2 neighbors. Requires: [x] is a positive integer. *)

val neighborhood : gameboard -> bit -> state array
(** [neighborhood gb x] creates a state array that represents the neighbors at
    each position. *)

val birth_node : gameboard -> bit -> unit
(** [birth_node gb x] checks the state of the node at position [x] in gameboard
    [gb]. If the node is dead, it is updated to be alive. Raises AlreadyAlive if
    the node is already alive. Requires: [x] is a valid positive integer. *)

val kill_node : gameboard -> bit -> unit
(** [kill_node gb x] checks the state of the node at grid position [x] in
    gameboard [gb]. If that node is alive, it is updated to be dead. Raises
    AlreadyDead if the node is already dead. Requires: [x] is a valid positive
    integer. *)

val update_node : gameboard -> bit -> bit -> state
(** [update_node gb rule x] updates the node at [x] in gameboard [gb] with a
    specified amount of neighbors in the previous generation to be dead/alive in
    the next. It stems from the number of neighbors according to the specified
    rules from [rule]. Requires: [x] is a positive integer. *)

val update_board : gameboard -> bit -> gameboard
(** [update_board gb rule] outputs a modified gameboard [gb] of the next
    generation. *)

val gb_to_string : gameboard -> string
(** [gb_to_string gb] is the string representation of the gameboard, using black
    and white squares. *)

val print_board : gameboard -> unit
(** [print_board gb] prints the string representation of the gameboard to
    terminal, using [gb_to_string gb] as a helper function. *)

val print_loop : gameboard -> bit -> bit -> unit
(** [print_loop gb rule x] is a loop that prints the continually updated
    gameboard [gb] and its future generations. *)

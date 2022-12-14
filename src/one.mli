type state =
  | Dead
  | Alive  (** [state] is the state of a given node, either dead or alive. *)

type bit = int
(** [bit] is the int representation of a bit: either 0 or 1. *)

type byte = bit list
(** [byte] is a list representation of 8 bits, representing 2^8 = 256 possible
    rules. *)

type rule = byte
(** [rule] is a byte representation of a certain 1D rule. *)

type gameboard = state array
(** [gameboard] is the entire board or dead or alive cells in a row. *)

type gamegrid = state array array
(** [gamegrid] is a 2D representation of a [gameboard], where each row is a new
    generation. *)

exception AlreadyAlive
(** Raised if trying to update an alive node's state to alive. *)

exception AlreadyDead
(** Raised if updating a dead node's state to dead. *)

val make_n : byte -> bit -> byte
(** [make_n lst n] prepends 0s to the front of a list to achieve n length.
    Requires: [lst] is a bit list of length <= n. *)

val make_end_n : byte -> bit -> byte
(** [make_end_n lst n] appends 0s to the back of a list to achieve n length.
    Requires: [lst] is a bit list of length <= n. *)

val int_to_binary : int -> byte
(** [int_to_binary i] returns the byte representation of a string. For the
    purpose of this program, binary numbers 0-7 are representated as a 3-bit
    list. *)

val binary_to_int : byte -> int
(** [binary_to_int binary] is the int representation of binary, of type byte. *)

val int_to_rule : bit -> rule
(** [int_to_rule n] is the byte that represents a rule, stemming from a bit
    input. For example, [int_to_rule 90 = \[0; 1; 0; 1; 1; 0; 1; 0\]]*)

val gb_to_byte : gameboard -> byte
(** [gb_to_byte gb] converts gameboard [gb] to its byte representation, where
    Dead has a value of 0, and Alive has a value of 1. *)

val init_empty : bit -> gameboard
(** [init_empty x] initializes a gameboard with a single alive node in the
    middle of the board of length [x].

    Requires: [x] is a postive integer.*)

val gb_to_string : gameboard -> string
(** [gb_to_string gb] converts the row of a gameboard into a string with squares
    that represent the alive/dead state of a node. *)

(* val print_board : gameboard -> unit *)
(** [print_board gb] prints the gameboard [gb] to terminal, using black squares
    to represent dead and white squares to represent alive.*)

val neighbors : gameboard -> bit -> bit
(** [neighbors gb x] is the number of alive neighbors of the node located at
    position [x]. Neighbors are the left and right nodes, thus there can be 0,
    1, or 2 neighbors.

    Requires: [x] is a postive integer in the gameboard. *)

val neighborhood : gameboard -> bit -> state array
(** [neighborhood gb x] creates a state array that represents the neighbors at
    each position.

    Requires: [x] is a postive integer in the gameboard.*)

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
    the next according to [rule].

    Requires: [x] is a positive integer and [rule] is between 1-255, inclusive. *)

val update_board : gameboard -> bit -> gameboard
(** [update_board gb rule] outputs a modified gameboard [gb] of the next
    generation. *)

val gb_to_string : gameboard -> string
(** [gb_to_string gb] is the string representation of the gameboard, using black
    and white squares. *)

val print_grid : gameboard -> unit
(** [print_board gb] prints the string representation of the gameboard to
    terminal. *)

val print_loop : gameboard -> bit -> bit -> unit
(** [print_loop gb rule x] is a loop that prints the continually updated
    gameboard [gb] according to the [rule], [x] times. *)

(* val make_grid : gameboard -> bit -> bit -> gamegrid (** [make_grid gb rule x]
   is the 2D dimensional representation of [game] according to [rule] with [x]
   many row representing each generation. *) *)

val print_grid : gameboard array -> unit
(** [print_grid gg] prints the 2D dimensional representation of the gameboard *)

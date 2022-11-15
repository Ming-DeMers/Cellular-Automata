type state =
  | Dead
  | Alive  (** [state] is either Dead or Alive. *)

type gameboard = state array array
(** Two dimensional array of nodes representing a gameboard *)

exception AlreadyAlive
exception AlreadyDead

val init_gameboard : int -> int -> gameboard
(** [init_gameboard x y] is a gameboard with dimensions x by y with all dead
    nodes *)

val print_board : gameboard -> unit
(** [print_board g] prints [g]. *)

val neighbors : gameboard -> int -> int -> int
(** [neighbors g x y] is the number of alive neighbors that the node located at
    position ([x], [y]) on the grid has. Neighbors are located directly to
    either side, diagonally, above, and below the original node. Requires: ([x],
    [y]) must be a valid position in the grid. *)

val update_node : gameboard -> int -> int -> unit
(** [update_node gb x y] updates the node to be dead or alive for the next
    generation, based on its number neighbors and according to the specified
    rules.

    Precondition: (x,y) is a valid coordinate of a node on the gameboard. *)

val update_board : gameboard -> unit
(** [update_board gb] updates gameboard gb to the next generation *)

val loop : gameboard -> int -> unit
(** [loop g i] loops through [i] generations of the Game of Life with gameboard
    [g], printing each time the board is updated *)

val birth_node : gameboard -> int -> int -> unit
(** [birth_node g x y] checks the state of the node at grid position [x], [y] in
    gameboard g. If that node is dead, it is updated to be alive. Raises
    AlreadyAlive if the node at position [x], [y] is already alive *)

val kill_node : gameboard -> int -> int -> unit
(** [kill_node g x y] checks the state of the node at grid position [x], [y] in
    gameboard g. If that node is alice, it is updated to be dead. Raises
    AlreadyDead if the node at position [x], [y] is already alive *)

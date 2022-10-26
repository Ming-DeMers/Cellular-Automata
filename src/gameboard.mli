type state
(** [state] is either Dead or Alive. *)

type gameboard
(** Two dimensional list of nodes representing a gameboard *)

val init_gameboard : gameboard option -> gameboard
(** [init_gameboard g_option] is the initial gameboard. The initial gameboard is
    either [g] if [g_option] is [Some g], otherwise it is some default
    gameboard. *)

val print_board : gameboard -> unit
(** [print_board g] prints [g]. *)

val neighbors : gameboard -> int -> int -> int
(** [get_neighbors g x y] is the number of alive neighbors that the node located
    at position ([x], [y]) on the grid has. Neighbors are located directly to
    either side, diagonally, above, and below the original node. If a node is
    located on the border of the board, wrap-around occurs. Requires: ([x], [y])
    must be a valid position in the grid. *)

val update_node : state -> int -> int -> state
(** [update_node gb x y] updates the node to be dead or alive for the next
    generation, based on the number of neighbors and according to the specified
    rules.

    Precondition: (x,y) is a valid coordinate of a node on the gameboard. *)

val update_board : gameboard -> gameboard
(** [update_board gb] updates gameboard gb to the next generation *)

val turn : gameboard -> gameboard
(** [turn g] is the gameboard of the next generation.

    [turn g] takes in a gameboard and calculates and prints the board for the
    next generation. *)

val loop : gameboard -> int -> unit
(** [loop g i] loops through [i] generations of the Game of Life with gameboard
    [g]. Loop simply calls [loop (turn g) (i-1)] or returns () if i = 0. *)

val add_node : int -> int -> gameboard
(** [add_node x y] checks the state of the node at grid position ([x], [y]. If
    that node is alive, the original gameboard is returned. If that node is
    dead, a new gameboard is returned with that node now alive. *)

val del_node : int -> int -> gameboard
(** [del_node x y] checks the state of the node at grid position ([x], [y]. If
    that node is dead, the original gameboard is returned. If that node is
    alive, a new gameboard is returned with that node now dead. *)

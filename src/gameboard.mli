type gameboard
(** Grid with (1,1) as the top left node *)

type state
(** [state] is either Dead or Alive. *)

val init_gameboard : gameboard option -> gameboard
(** [init_gameboard g_option] is the initial gameboard. The initial gameboard is
    either [g] if [g_option] is [Some g], otherwise it is some default
    gameboard. *)

val loop : gameboard -> int -> unit
(** [loop g i] loops through [i] generations of the Game of Life with gameboard
    [g]. Loop simply calls [loop (turn g) (i-1)] or returns () if i = 0. *)

val turn : gameboard -> gameboard
(** [turn g] is the gameboard of the next generation.

    [turn g] takes in a gameboard and calculates and prints the board for the
    next generation. *)

val get_neighbors : gameboard -> int -> int -> int
(** [get_neighbors g x y] is the number of alive neighbors that the node located
    at position ([x], [y]) on the grid has. Neighbors are located directly to
    either side, diagonally, above, and below the original node. If a node is
    located on the border of the board, some of its neighbors may not exist.
    Requires: ([x], [y]) must be a valid position in the grid. *)

val update_node : state -> int -> int -> state
(** [update_node status x y] is whether or not the node is dead or alive in the
    next generation. *)

val update_board : gameboard -> int -> int -> gameboard
(** [update_board g x y acc] takes in an empty gameboard and constructs a new
    gameboard with node x y added.

    [acc] is originally an empty list [update_board 1 1 gb acc] calls
    [update_board 2 1 gb (acc with node 0, 0 updated)]
    [update_board max_x 1 gb acc] calls
    [update_board 1 2 gb (acc with node max_x 1 updated)]
    [update_board max_x max_y gb acc] is the final updated board *)

val print_board : gameboard -> unit
(** [print_board g] prints [g]. *)
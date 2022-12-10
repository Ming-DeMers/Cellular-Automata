(* two_matrix is the 2 dimensional implementation of the Game of Life. The game
   is represented in a 2D that updates upon each generation. Each node has 8
   neighbors, which affects whether it lives or dies on the next round. This
   implementation allows the user to generate preset patterns or input their own
   manually, and run the pattern for generations. *)

module type BSRules = sig
  val born : int list
  val survive : int list
end

module B3_S23 : BSRules
module B36_S23 : BSRules
module B34678_S3678 : BSRules
module B2_S : BSRules

module type Board = sig
  type state =
    | Dead
    | Alive

  type gameboard = state array array
  (** Two dimensional array of nodes representing a gameboard. Top left corner
      is (0, 0), increasing in x and y when moving right and down respectively *)

  exception AlreadyAlive
  exception AlreadyDead
  exception PreconditionViolation of string

  val get : gameboard -> int -> int -> state
  (** [get g x y] is the state of the node at coordinate position (x,y) with the
      top left corner being (0, 0), increasing in x and y when moving right and
      down respectively. Requires: ([x], [y]) must be a valid position in the
      grid *)

  val birth_node : gameboard -> int -> int -> unit
  (** [birth_node g x y] checks the state of the node at grid position [x], [y]
      in gameboard g. If that node is dead, it is updated to be alive. Raises
      AlreadyAlive if the node at position [x], [y] is already alive. Requires:
      ([x], [y]) must be a valid position in the grid.*)

  val kill_node : gameboard -> int -> int -> unit
  (** [kill_node g x y] checks the state of the node at grid position [x], [y]
      in gameboard g. If that node is alice, it is updated to be dead. Raises
      AlreadyDead if the node at position [x], [y] is already dead. Requires:
      ([x], [y]) must be a valid position in the grid.*)

  val neighbors : gameboard -> int -> int -> int
  (** [neighbors g x y] is the number of alive neighbors that the node located
      at position ([x], [y]) on the grid has. Neighbors are located directly to
      either side, diagonally, above, and below the original node. Requires:
      ([x], [y]) must be a valid position in the grid. *)

  val update_node : gameboard -> int -> int -> int -> unit
  (** [update_node gb x y n] updates the node at (x,y) in gamebaord g with n
      neighbors in the previous generation to be dead or alive for the next
      generation, based on its number neighbors and according to the rules of
      the board. Requires: (x,y) is a valid coordinate of a node on the
      gameboard. *)

  val update_board : gameboard -> unit
  (** [update_board gb] updates gameboard gb to the next generation *)

  val print_board : gameboard -> unit
  (** [print_board g] prints [g]. *)

  val loop : gameboard -> int -> unit
  (** [loop g i] loops through [i] generations of the Game of Life with
      gameboard [g], printing each time the board is updated *)

  val init_empty : int -> int -> gameboard
  (** [init_gameboard x y] is a gameboard with dimensions x by y with all dead
      nodes. Requires: x > 0 and y > 0 *)

  val init_glider : unit -> gameboard
  (** Is a new glider *)
end

module Make : functor (_ : BSRules) -> Board

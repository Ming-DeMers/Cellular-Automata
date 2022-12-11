(** One_matrix is the 1-dimendsional implementation of the game of life. The
   gameboard is a one dimensional array, which updates on each generation. It
   can be stylized as as a 2D gird, however. Due to the nature of each node only
   having three neighbors, there are 8 possible outcomes of which a rule can be
   composed. As such there are 255 rules, some more interesting than others.
   This implementation allows the user to choose their rule, and to run
   generations and see the pattern develop. *)
type state =
  | Dead
  | Alive  (** [state] is either Dead or Alive. *)

type gameboard = state array array
(** Two dimensional array of nodes representing a gameboard. Top left corner is
    (0, 0), increasing in x and y when moving right and down respectively *)

type 'a gen =
  | Cons of 'a * 'a gen
      (** A one dimensional list that represents the infinite row of each
          generation*)

exception AlreadyAlive
exception AlreadyDead

val init_empty : int -> int -> gameboard
(** [init_gameboard x] is a gameboard with the width of x and all dead nodes *)

val print_board : gameboard -> unit
(** [print_board gb] prints [gb]. *)

val neighbors : gameboard -> int -> int
(** [neighbors gb x] is the number of alive neighbors that the node located at
    position ([x]). Neighbors are the left and right nodes, thus there can be
    none, one, or two neightbors Requires: ([x]) is a positive integer. *)

val update_node : gameboard -> int -> unit
(** [update_node gb x n] updates the node at ([x]) in gameboard g with n
    neighbors in the previous generation to be dead or alive for the next
    generation, based on its number neighbors and according to the specified
    rules.

    Precondition: (x) is a positive integer. *)

val update_board : gameboard -> unit
(** [update_board gb] updates gameboard gb to the next generation *)

val loop : gameboard -> int -> unit
(** [loop gb i] loops through [i] generations of the Game of Life with gameboard
    [gb], printing each time the board is updated *)

val birth_node : gameboard -> int -> int -> unit
(** [birth_node gb x] checks the state of the node at position [x] in gameboard
    gb. If that node is dead, it is updated to be alive. Raises AlreadyAlive if
    the node at position [x], is already alive.

    Precondition: [x] is a valid positive integer*)

val kill_node : gameboard -> int -> int -> unit
(** [kill_node gb x] checks the state of the node at grid position [x] in
    gameboard gb. If that node is alove, it is updated to be dead. Raises
    AlreadyDead if the node at position [x] is already alive.contents
    Precondition: [x] is a valid positive integer. *)
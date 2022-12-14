open Two

(** [ActiveBoard] is contains the neccessary functions to observe 2D CA
    according to specific gamerules. This implementation is more time effecient
    but less space efficent than Two.Board by keeping track of the active nodes
    and updating only them *)
module type ActiveBoard = sig
  (** [state] represents the state of a node. Either [Dead] or [Alive] *)
  type state =
    | Dead
    | Alive

  type gameboard
  (** Two dimensional array of nodes representing a gameboard. Top left corner
      is (0, 0), increasing in x and y when moving right and down respectively *)

  exception AlreadyAlive
  (** [AlreadyAlive] is raised when a node containing an [Alive] node is
      attempted to be birthed *)

  exception AlreadyDead
  (** [AlreadyDead] is raised when a node containing an [Dead] node is attempted
      to be killed *)

  exception PreconditionViolation of string
  (** [PreconditionViolation s] is raised when the precondition of a funciton is
      violated. [s] tells which function raised this excpetion *)

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
  (** [update_board gb] updates gameboard [gb] to the next generation *)

  val print_board : gameboard -> unit
  (** [print_board g] prints [g]. *)

  val loop : gameboard -> int -> unit
  (** [loop g i] loops through [i] generations of the Game of Life with
      gameboard [g], printing each time the board is updated *)

  val init_empty : int -> int -> gameboard
  (** [init_gameboard x y] is a gameboard with dimensions [x] by [y] with all
      dead nodes. Requires: [x > 0] and [y > 0] *)

  val init_glider : unit -> gameboard
  (** [init_glider ()] is a glider centered on a 10x10 grid. Has interesting
      applications in Conway's Game of Life (B3_S23) *)

  val make_board : int -> int -> (int * int) list -> gameboard
  (** [make_board x y c] is a gameboard with dimensions [x] by [y] and alive
      nodes at each coordinate specified in [c] *)
end

(** [MakeActive] makes an [ActiveBoard] according to the rules specified by [BS] *)
module MakeActive (BS : BSRules) : ActiveBoard = struct
  type state =
    | Dead
    | Alive

  type gameboard = state array array * (int * int) list ref

  exception AlreadyAlive
  exception AlreadyDead
  exception PreconditionViolation of string

  let check_inbounds (g, _) x y f_name =
    let width = Array.length g.(0) in
    let height = Array.length g in
    if x >= 0 && x < width then
      if y >= 0 && y < height then ()
      else
        raise (PreconditionViolation ("y out of bounds. Error in: " ^ f_name))
    else raise (PreconditionViolation ("x out of bounds. Error in: " ^ f_name))

  let get (g, _) x y =
    check_inbounds (g, ref []) x y "get";
    g.(y).(x)

  (* [set g x y st] changes the value the node at coordinate position (x,y) to
     st. Requires: ([x], [y]) must be a valid position in the grid *)
  let set (g, _) x y n =
    check_inbounds (g, ref []) x y "set";
    g.(y).(x) <- n

  let update_neighbors (g, a) x y =
    check_inbounds (g, a) x y "update_neighbors";
    let width = Array.length g.(0) in
    let height = Array.length g in
    for r = x - 1 to x + 1 do
      for c = y - 1 to y + 1 do
        let r' = if r = -1 then width - 1 else r in
        let rf = if r' = width then 0 else r' in
        let c' = if c = -1 then height - 1 else c in
        let cf = if c' = height then 0 else c' in
        if not (rf = x && cf = y) then
          if List.exists (fun (x', y') -> x' = rf && y' = cf) !a then ()
          else a := (rf, cf) :: !a
        else ()
      done
    done

  let birth_node g x y =
    check_inbounds g x y "birth_node";
    match get g x y with
    | Dead ->
        set g x y Alive;
        update_neighbors g x y
    | Alive -> raise AlreadyAlive

  let kill_node g x y =
    check_inbounds g x y "kill_node";
    match get g x y with
    | Dead -> raise AlreadyDead
    | Alive ->
        set g x y Dead;
        update_neighbors g x y

  let neighbors (g, a) x y =
    check_inbounds (g, a) x y "neighbors";
    let width = Array.length g.(0) in
    let height = Array.length g in
    let count = ref 0 in
    for r = x - 1 to x + 1 do
      for c = y - 1 to y + 1 do
        let r' = if r = -1 then width - 1 else r in
        let rf = if r' = width then 0 else r' in
        let c' = if c = -1 then height - 1 else c in
        let cf = if c' = height then 0 else c' in
        if not (rf = x && cf = y) then
          match get (g, a) rf cf with
          | Alive -> count := !count + 1
          | Dead -> ()
        else ()
      done
    done;
    !count

  let update_node g x y n =
    check_inbounds g x y "update_node";
    match get g x y with
    | Dead -> if List.mem n BS.born then birth_node g x y else ()
    | Alive -> if List.mem n BS.survive then () else kill_node g x y

  let update_board (g, a) =
    let neighbors_list =
      let f acc (x, y) = ((x, y), neighbors (g, ()) x y) :: acc in
      List.fold_left f [] !a
    in
    let f' () ((x, y), n) = update_node (g, a) x y n in
    List.fold_left f' () neighbors_list

  let to_list g = Array.to_list g

  let rec make_row_string r =
    match r with
    | [] -> ""
    | h :: t -> begin
        match h with
        | Alive -> "◾" ^ make_row_string t
        | Dead -> "◽" ^ make_row_string t
      end

  (** Creates a string of the given gameboard *)
  let rec gb_list_to_string g =
    match g with
    | [] -> ""
    | h :: t -> make_row_string (to_list h) ^ "\n" ^ gb_list_to_string t

  let print_board (g, _) = print_endline (g |> to_list |> gb_list_to_string)

  let loop (g, a) n =
    for num = n downto 1 do
      ignore num;
      update_board (g, a);
      print_board (g, a)
    done

  let init_empty x y =
    if x < 1 || y < 1 then
      raise (PreconditionViolation "error in init_empty: x and y must be >= 1")
    else
      let a = ref [] in
      for r = 0 to x - 1 do
        for c = 0 to y - 1 do
          a := (r, c) :: !a
        done
      done;
      (Array.make_matrix x y Dead, a)

  let make_board x y c =
    let gb = init_empty x y in
    let f (g, a) (x, y) =
      birth_node (g, a) x y;
      (g, a)
    in
    List.fold_left f gb c

  let init_glider () =
    make_board 10 10 [ (3, 4); (4, 4); (5, 4); (4, 2); (5, 3) ]
end
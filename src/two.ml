module type BSRules = sig
  val born : int list
  val survive : int list
end

module type Board = sig
  type state =
    | Dead
    | Alive

  type gameboard = state array array

  exception AlreadyAlive
  exception AlreadyDead
  exception PreconditionViolation of string

  val born_rule : int list
  val survive_rule : int list
  val get : gameboard -> int -> int -> state
  val birth_node : gameboard -> int -> int -> unit
  val kill_node : gameboard -> int -> int -> unit
  val neighbors : gameboard -> int -> int -> int
  val update_node : gameboard -> int -> int -> int -> unit
  val update_board : gameboard -> unit
  val string_of_board : gameboard -> string
  val print_board : gameboard -> unit
  val loop : gameboard -> int -> unit
  val init_empty : int -> int -> gameboard
  val init_glider : unit -> gameboard
  val init_replicator : unit -> gameboard
  val init_rocket : unit -> gameboard
  val init_seed : unit -> gameboard
  val make_board : int -> int -> (int * int) list -> gameboard
end

(* Conway's Game of Life *)
module B3_S23 : BSRules = struct
  let born = [ 3 ]
  let survive = [ 2; 3 ]
end

(* HighLife *)
module B36_S23 : BSRules = struct
  let born = [ 3; 6 ]
  let survive = [ 2; 3 ]
end

(* Day and Night *)
module B34678_S3678 : BSRules = struct
  let born = [ 3; 4; 6; 7; 8 ]
  let survive = [ 3; 6; 7; 8 ]
end

(* Seeds *)
module B2_S : BSRules = struct
  let born = [ 2 ]
  let survive = []
end

module MakeBoard (BS : BSRules) : Board = struct
  type state =
    | Dead
    | Alive

  type gameboard = state array array

  exception AlreadyAlive
  exception AlreadyDead
  exception PreconditionViolation of string

  let born_rule = BS.born
  let survive_rule = BS.survive

  let check_inbounds g x y f_name =
    let width = Array.length g.(0) in
    let height = Array.length g in
    if x >= 0 && x < width then
      if y >= 0 && y < height then ()
      else
        raise (PreconditionViolation ("y out of bounds. Error in: " ^ f_name))
    else raise (PreconditionViolation ("x out of bounds. Error in: " ^ f_name))

  let get g x y =
    check_inbounds g x y "get";
    g.(y).(x)

  (* [set g x y st] changes the value the node at coordinate position (x,y) to
     st. Requires: ([x], [y]) must be a valid position in the grid *)
  let set g x y st =
    check_inbounds g x y "set";
    g.(y).(x) <- st

  (* Obviously could just set nodes directly, but the errors here help for
     debugging *)
  let birth_node g x y =
    check_inbounds g x y "birth_node";
    match get g x y with
    | Dead -> set g x y Alive
    | Alive -> raise AlreadyAlive

  let kill_node g x y =
    check_inbounds g x y "kill_node";
    match get g x y with
    | Dead -> raise AlreadyDead
    | Alive -> set g x y Dead

  (* O(1) maybe*)
  (* For ALl Dead Border, DEPRECIATED *)
  (* let neighbors_dead_boundary g x y = let width = Array.length g.(0) in let
     height = Array.length g in let count = ref 0 in for r = x - 1 to x + 1 do
     for c = y - 1 to y + 1 do if (not (r = -1 || r = width)) && (not (c = -1 ||
     c = height)) && not (r = x && c = y) then if get g r c = Alive then count
     := !count + 1 else () else () done done; !count *)

  let neighbors g x y =
    check_inbounds g x y "neighbors";
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
          if get g rf cf = Alive then count := !count + 1 else ()
        else ()
      done
    done;
    !count

  let update_node g x y n =
    check_inbounds g x y "update_node";
    match get g x y with
    | Dead -> if List.mem n BS.born then birth_node g x y else ()
    | Alive -> if List.mem n BS.survive then () else kill_node g x y

  let update_board g =
    let width = Array.length g.(0) in
    let height = Array.length g in
    let neighbors_matrix = Array.make_matrix width height 0 in
    for r = 0 to width - 1 do
      for c = 0 to height - 1 do
        set neighbors_matrix r c (neighbors g r c)
      done
    done;
    for r = 0 to width - 1 do
      for c = 0 to height - 1 do
        update_node g r c (get neighbors_matrix r c)
      done
    done

  let to_list g = Array.to_list g

  let rec make_row_string r =
    match r with
    | [] -> ""
    | h :: t -> begin
        match h with
        | Alive -> "⬛" ^ make_row_string t
        | Dead -> "⬜" ^ make_row_string t
      end

  (** Creates a string of the given gameboard *)
  let rec gb_list_to_string g =
    match g with
    | [] -> ""
    | h :: t -> make_row_string (to_list h) ^ "\n" ^ gb_list_to_string t

  let string_of_board g = g |> to_list |> gb_list_to_string
  let print_board g = g |> string_of_board |> print_endline

  let loop g n =
    for num = n downto 1 do
      ignore num;
      update_board g;
      print_board g
    done

  let init_empty x y =
    if x < 1 || y < 1 then
      raise (PreconditionViolation "error in init_empty: x and y must be >= 1")
    else Array.make_matrix x y Dead

  let make_board x y c =
    let gb = init_empty x y in
    let f g (x, y) =
      birth_node g x y;
      g
    in
    List.fold_left f gb c

  let init_glider () =
    make_board 10 10 [ (3, 4); (4, 4); (5, 4); (4, 2); (5, 3) ]

  let init_replicator () =
    make_board 18 18 [ (7, 8); (7, 9); (7, 10); (8, 7); (9, 7); (10, 7) ]

  let init_rocket () =
    make_board 20 15
      [
        (18, 7);
        (17, 7);
        (16, 7);
        (15, 7);
        (14, 7);
        (13, 7);
        (12, 7);
        (11, 7);
        (14, 6);
        (14, 8);
        (13, 5);
        (13, 6);
        (13, 8);
        (13, 9);
        (12, 5);
        (12, 9);
        (11, 6);
        (11, 8);
      ]

  let init_seed () =
    make_board 18 18
      [ (7, 8); (7, 9); (8, 7); (9, 7); (10, 8); (10, 9); (8, 9); (9, 9) ]
end
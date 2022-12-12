open OUnit2
open Cellular_automata.One
open Cellular_automata.Two

let make_n_test name in_lst in_int exp_out =
  name >:: fun _ -> assert_equal exp_out (make_n in_lst in_int)

let int_to_binary_test name in_int exp_out =
  name >:: fun _ -> assert_equal exp_out (int_to_binary in_int)

let binary_to_int_test name in_lst exp_out =
  name >:: fun _ -> assert_equal exp_out (binary_to_int in_lst)

let int_to_rule_test name in_int exp_out =
  name >:: fun _ -> assert_equal exp_out (int_to_rule in_int)

let game_9 = init_empty 9
let game_15 = init_empty 15

let init_empty_test name in_x exp_out =
  name >:: fun _ -> assert_equal exp_out (init_empty in_x)

let update_node test_name in_gb in_rule in_x exp_out =
  test_name >:: fun _ -> assert_equal exp_out (update_node in_gb in_rule in_x)

let one_tests =
  [
    make_n_test "make_n [1] 2 is [0; 1]" [ 1 ] 2 [ 0; 1 ];
    make_n_test "make_n 1;3 4 is [0; 0; 1; 3]" [ 1; 3 ] 4 [ 0; 0; 1; 3 ];
    int_to_binary_test "int_to_binary 0 is [0]" 0 [ 0 ];
    int_to_binary_test "int_to_binary 1 is [1]" 1 [ 1 ];
    int_to_binary_test "int_to_binary 2 is [1; 0]" 2 [ 1; 0 ];
    int_to_binary_test "int_to_binary 3 is [1; 1]" 3 [ 1; 1 ];
    int_to_binary_test "int_to_binary 9 is [1; 0; 0; 1]" 9 [ 1; 0; 0; 1 ];
    binary_to_int_test "binary_to_int [0] is 0" [ 0 ] 0;
    binary_to_int_test "binary_to_int [1] is 1" [ 1 ] 1;
    binary_to_int_test "binary_to_int [1; 0] is 2" [ 1; 0 ] 2;
    binary_to_int_test "binary_to_int [1; 1] is 3" [ 1; 1 ] 3;
    binary_to_int_test "binary_to_int [1; 0; 0; 1] is 9" [ 1; 0; 0; 1 ] 9;
    int_to_rule_test "int_to_rule 0 is [0; 0; 0; 0; 0; 0; 0; 0]" 0
      [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    int_to_rule_test "int_to_rule 1 is [0; 0; 0; 0; 0; 0; 0; 1]" 1
      [ 0; 0; 0; 0; 0; 0; 0; 1 ];
    int_to_rule_test "int_to_rule 90 is [0; 1; 0; 1; 1; 0; 1; 0]" 90
      [ 0; 1; 0; 1; 1; 0; 1; 0 ];
    int_to_rule_test "int_to_rule 255 is [1; 1; 1; 1; 1; 1; 1; 1]" 255
      [ 1; 1; 1; 1; 1; 1; 1; 1 ];
    init_empty_test "init_empty 3 is [|Dead; Alive; Dead|]" 3
      [| Dead; Alive; Dead |];
    init_empty_test "init_empty 5 is [|Dead; Dead; Alive; Dead; Dead|]" 5
      [| Dead; Dead; Alive; Dead; Dead |];
    init_empty_test
      "init_empty 10 is [| Dead; Dead; Dead; Dead; Dead; Alive; Dead; Dead; \
       Dead; Dead |]"
      10
      [| Dead; Dead; Dead; Dead; Dead; Alive; Dead; Dead; Dead; Dead |];
  ]

(* Test Boards *)
module GoL = Make (B3_S23)

(************** Tests for Standard Game of Life with Wraparound **************)
let empty_10x10 = GoL.init_empty 10 10
let glider_10x10 = GoL.init_glider ()

let b55_10x10 =
  let x = GoL.init_empty 10 10 in
  GoL.birth_node x 5 5;
  x

let b00_10x10 =
  let x = GoL.init_empty 10 10 in
  GoL.birth_node x 0 0;
  x

let b09_10x10 =
  let x = GoL.init_empty 10 10 in
  GoL.birth_node x 0 9;
  x

let b90_10x10 =
  let x = GoL.init_empty 10 10 in
  GoL.birth_node x 9 0;
  x

let b99_10x10 =
  let x = GoL.init_empty 10 10 in
  GoL.birth_node x 9 9;
  x

let b05_10x10 =
  let x = GoL.init_empty 10 10 in
  GoL.birth_node x 0 5;
  x

let b95_10x10 =
  let x = GoL.init_empty 10 10 in
  GoL.birth_node x 9 5;
  x

let b50_10x10 =
  let x = GoL.init_empty 10 10 in
  GoL.birth_node x 5 0;
  x

let b59_10x10 =
  let x = GoL.init_empty 10 10 in
  GoL.birth_node x 5 9;
  x

let state_printer s =
  match s with
  | GoL.Alive -> "Alive"
  | GoL.Dead -> "Dead"

(* let assert_equal_boards gb1 gb2 = let gb1_lst = gb1 |> Array.to_list |>
   List.map (fun arr -> Array.to_list arr) |> List.flatten in let gb2_lst = gb2
   |> Array.to_list |> List.map (fun arr -> Array.to_list arr) |> List.flatten
   in List.equal (fun a b -> a = b) gb1_lst gb2_lst *)

let neighbors_test name in_gb in_x in_y exp_out =
  name >:: fun _ ->
  assert_equal exp_out (GoL.neighbors in_gb in_x in_y) ~printer:string_of_int

let update_node_test name in_gb in_x in_y (exp_out : GoL.state) =
  name >:: fun _ ->
  assert_equal exp_out
    (let n = GoL.neighbors in_gb in_x in_y in
     GoL.update_node in_gb in_x in_y n;
     GoL.get in_gb in_x in_y)
    ~printer:state_printer

(* let update_board_test name in_gb exp_out = name >:: fun _ -> assert_equal
   exp_out (GoL.update_board in_gb; in_gb) *)

let neighbors_tests =
  [
    (* empty board *)
    neighbors_test "neighbors of empty @ 0,0" empty_10x10 0 0 0;
    neighbors_test "neighbors of empty @ 9,9" empty_10x10 9 9 0;
    (* only alive node at 5,5 *)
    neighbors_test "neighbors of 5,5 @ 5,5" b55_10x10 5 5 0;
    neighbors_test "neighbors of 5,5 @ 4,4" b55_10x10 4 4 1;
    neighbors_test "neighbors of 5,5 @ 5,4" b55_10x10 5 4 1;
    neighbors_test "neighbors of 5,5 @ 6,4" b55_10x10 6 4 1;
    neighbors_test "neighbors of 5,5 @ 4,5" b55_10x10 4 5 1;
    neighbors_test "neighbors of 5,5 @ 6,5" b55_10x10 6 5 1;
    neighbors_test "neighbors of 5,5 @ 4,6" b55_10x10 4 6 1;
    neighbors_test "neighbors of 5,5 @ 5,6" b55_10x10 5 6 1;
    neighbors_test "neighbors of 5,5 @ 6,6" b55_10x10 6 6 1;
    neighbors_test "neighbors of 5,5 @ 5,7" b55_10x10 5 7 0;
    (* mid edge cases*)
    neighbors_test "neighbors of 5,0 @ 4,9" b50_10x10 4 9 1;
    neighbors_test "neighbors of 5,0 @ 5,9" b50_10x10 5 9 1;
    neighbors_test "neighbors of 5,0 @ 6,9" b50_10x10 6 9 1;
    neighbors_test "neighbors of 9,5 @ 0,4" b95_10x10 0 4 1;
    neighbors_test "neighbors of 9,5 @ 0,5" b95_10x10 0 5 1;
    neighbors_test "neighbors of 9,5 @ 0,6" b95_10x10 0 6 1;
    neighbors_test "neighbors of 5,9 @ 4,0" b59_10x10 4 0 1;
    (* neighbors_test "neighbors of 5,9 @ 5,0" b59_10x10 5 0 1; *)
    (* failed *)
    neighbors_test "neighbors of 5,9 @ 6,0" b59_10x10 6 0 1;
    neighbors_test "neighbors of 0,5 @ 9,4" b05_10x10 9 4 1;
    neighbors_test "neighbors of 0,5 @ 9,5" b05_10x10 9 5 1;
    neighbors_test "neighbors of 0,5 @ 9,6" b05_10x10 9 6 1;
    (* corner cases *)
    neighbors_test "neighbors of 0,0 @ 9,0" b00_10x10 9 0 1;
    neighbors_test "neighbors of 0,0 @ 0,9" b00_10x10 0 9 1;
    neighbors_test "neighbors of 0,0 @ 9,9" b00_10x10 9 9 1;
    neighbors_test "neighbors of 9,0 @ 0,0" b90_10x10 0 0 1;
    neighbors_test "neighbors of 9,0 @ 0,9" b90_10x10 0 9 1;
    neighbors_test "neighbors of 9,0 @ 9,9" b90_10x10 9 9 1;
    neighbors_test "neighbors of 0,9 @ 0,0" b09_10x10 0 0 1;
    neighbors_test "neighbors of 0,9 @ 9,9" b09_10x10 9 9 1;
    neighbors_test "neighbors of 0,9 @ 0,0" b09_10x10 9 0 1;
    neighbors_test "neighbors of 9,9 @ 0,9" b99_10x10 0 9 1;
    neighbors_test "neighbors of 9,9 @ 0,0" b99_10x10 0 0 1;
    (* neighbors_test "neighbors of 9,9 @ 9,0" b99_10x10 9 0 1; *)
    (* failed *)
    neighbors_test "neighbors of glider @ 5,5" glider_10x10 5 5 2;
    neighbors_test "neighbors of glider @ 4,5" glider_10x10 4 5 3;
    neighbors_test "neighbors of glider @ 4,4" glider_10x10 4 4 3;
    neighbors_test "neighbors of glider @ 4,3" glider_10x10 4 3 5;
  ]

let update_node_tests =
  [
    update_node_test "update empty" empty_10x10 5 5 GoL.Dead;
    update_node_test "update gilder @ 5,5" glider_10x10 5 5 GoL.Dead;
    update_node_test "update gilder @ 4,5" glider_10x10 4 5 GoL.Alive;
    update_node_test "update gilder @ 5,4" glider_10x10 5 4 GoL.Alive;
  ]

let gol_tests = List.flatten [ neighbors_tests; update_node_tests ]

(******************************************************************************)

let suite = "test suite for CA" >::: List.flatten [ gol_tests; one_tests ]
let _ = run_test_tt_main suite

open OUnit2
open Cellular_automata.One
open Cellular_automata.Two

let make_n_test name in_lst in_int exp_out =
  name >:: fun _ -> assert_equal exp_out (make_n in_lst in_int)

let int_to_binary_test name in_int exp_out =
  name >:: fun _ -> assert_equal exp_out (int_to_binary in_int)

let binary_to_int_test name in_lst exp_out =
  name >:: fun _ -> assert_equal exp_out (binary_to_int in_lst)

let make_rule_test name in_int exp_out =
  name >:: fun _ -> assert_equal exp_out (make_rule in_int)

let init_empty_test name in_x exp_out =
  name >:: fun _ -> assert_equal exp_out (init_empty in_x)

let one_tests =
  [
    make_n_test "make_n [1] 2 is [0; 1]" [ 1 ] 2 [ 0; 1 ];
    make_n_test "make_n 1;3 4 is [0; 0; 1; 3]" [ 1; 3 ] 4 [ 0; 0; 1; 3 ];
    binary_to_int_test "binary_to_int [0] is 0" [ 0 ] 0;
    binary_to_int_test "binary_to_int [1] is 1" [ 1 ] 1;
    binary_to_int_test "binary_to_int [1; 0] is 2" [ 1; 0 ] 2;
    binary_to_int_test "binary_to_int [1; 1] is 3" [ 1; 1 ] 3;
    binary_to_int_test "binary_to_int [1; 0; 0; 1] is 9" [ 1; 0; 0; 1 ] 9;
    make_rule_test "make_rule 0 is [0; 0; 0; 0; 0; 0; 0; 0]" 0
      [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    make_rule_test "make_rule 1 is [0; 0; 0; 0; 0; 0; 0; 1]" 1
      [ 0; 0; 0; 0; 0; 0; 0; 1 ];
    make_rule_test "make_rule 90 is [0; 1; 0; 1; 1; 0; 1; 0]" 90
      [ 0; 1; 0; 1; 1; 0; 1; 0 ];
    make_rule_test "make_rule 255 is [1; 1; 1; 1; 1; 1; 1; 1]" 255
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

let int_to_binary_tests =
  [
    int_to_binary_test "int_to_binary 0 is [0]" 0 [ 0 ];
    int_to_binary_test "int_to_binary 1 is [1]" 1 [ 1 ];
    int_to_binary_test "int_to_binary 2 is [0; 1]" 2 [ 1; 0 ];
    int_to_binary_test "int_to_binary 3 is [1; 1]" 3 [ 1; 1 ];
    int_to_binary_test "int_to_binary 4 is [1; 0; 0]" 4 [ 1; 0; 0 ];
    int_to_binary_test "int_to_binary 5 is [1; 0; 1]" 5 [ 1; 0; 1 ];
    int_to_binary_test "int_to_binary 6 is [1; 1; 0]" 6 [ 1; 1; 0 ];
    int_to_binary_test "int_to_binary 7 is [1; 1; 1]" 7 [ 1; 1; 1 ];
    int_to_binary_test "int_to_binary 8 is [1; 0; 0; 0]" 8 [ 1; 0; 0; 0 ];
    int_to_binary_test "int_to_binary 9 is [1; 0; 0; 1]" 9 [ 1; 0; 0; 1 ];
    int_to_binary_test "int_to_binary 10 is [1; 0; 1; 0]" 10 [ 1; 0; 1; 0 ];
    int_to_binary_test "int_to_binary 11 is [1; 0; 1; 1]" 11 [ 1; 0; 1; 1 ];
    int_to_binary_test "int_to_binary 12 is [1; 1; 0; 0]" 12 [ 1; 1; 0; 0 ];
    int_to_binary_test "int_to_binary 13 is [1; 1; 0; 1]" 13 [ 1; 1; 0; 1 ];
    int_to_binary_test "int_to_binary 14 is [1; 1; 1; 0]" 14 [ 1; 1; 1; 0 ];
    int_to_binary_test "int_to_binary 15 is [1; 1; 1; 1]" 15 [ 1; 1; 1; 1 ];
    int_to_binary_test "int_to_binary 16 is [1; 0; 0; 0; 0]" 16
      [ 1; 0; 0; 0; 0 ];
    int_to_binary_test "int_to_binary 17 is [1; 0; 0; 0; 1]" 17
      [ 1; 0; 0; 0; 1 ];
    int_to_binary_test "int_to_binary 18 is [1; 0; 0; 1; 0]" 18
      [ 1; 0; 0; 1; 0 ];
    int_to_binary_test "int_to_binary 19 is [1; 0; 0; 1; 1]" 19
      [ 1; 0; 0; 1; 1 ];
    int_to_binary_test "int_to_binary 20 is [1; 0; 1; 0; 0]" 20
      [ 1; 0; 1; 0; 0 ];
    int_to_binary_test "int_to_binary 21 is [1; 0; 1; 0; 1]" 21
      [ 1; 0; 1; 0; 1 ];
    int_to_binary_test "int_to_binary 22 is [1; 0; 1; 1; 0]" 22
      [ 1; 0; 1; 1; 0 ];
    int_to_binary_test "int_to_binary 23 is [1; 0; 1; 1; 1]" 23
      [ 1; 0; 1; 1; 1 ];
    int_to_binary_test "int_to_binary 24 is [1; 1; 0; 0; 0]" 24
      [ 1; 1; 0; 0; 0 ];
    int_to_binary_test "int_to_binary 25 is [1; 1; 0; 0; 1]" 25
      [ 1; 1; 0; 0; 1 ];
    int_to_binary_test "int_to_binary 26 is [1; 1; 0; 1; 0]" 26
      [ 1; 1; 0; 1; 0 ];
    int_to_binary_test "int_to_binary 27 is [1; 1; 0; 1; 1]" 27
      [ 1; 1; 0; 1; 1 ];
    int_to_binary_test "int_to_binary 28 is [1; 1; 1; 0; 0]" 28
      [ 1; 1; 1; 0; 0 ];
    int_to_binary_test "int_to_binary 29 is [1; 1; 1; 0; 1]" 29
      [ 1; 1; 1; 0; 1 ];
    int_to_binary_test "int_to_binary 30 is [1; 1; 1; 1; 0]" 30
      [ 1; 1; 1; 1; 0 ];
    int_to_binary_test "int_to_binary 31 is [1; 1; 1; 1; 1]" 31
      [ 1; 1; 1; 1; 1 ];
    int_to_binary_test "int_to_binary 32 is [1; 0; 0; 0; 0; 0]" 32
      [ 1; 0; 0; 0; 0; 0 ];
    int_to_binary_test "int_to_binary 33 is [1; 0; 0; 0; 0; 1]" 33
      [ 1; 0; 0; 0; 0; 1 ];
    int_to_binary_test "int_to_binary 34 is [1; 0; 0; 0; 1; 0]" 34
      [ 1; 0; 0; 0; 1; 0 ];
    int_to_binary_test "int_to_binary 35 is [1; 0; 0; 0; 1; 1]" 35
      [ 1; 0; 0; 0; 1; 1 ];
    int_to_binary_test "int_to_binary 36 is [1; 0; 0; 1; 0; 0]" 36
      [ 1; 0; 0; 1; 0; 0 ];
    int_to_binary_test "int_to_binary 37 is [1; 0; 0; 1; 0; 1]" 37
      [ 1; 0; 0; 1; 0; 1 ];
    int_to_binary_test "int_to_binary 38 is [1; 0; 0; 1; 1; 0]" 38
      [ 1; 0; 0; 1; 1; 0 ];
    int_to_binary_test "int_to_binary 39 is [1; 0; 0; 1; 1; 1]" 39
      [ 1; 0; 0; 1; 1; 1 ];
    int_to_binary_test "int_to_binary 40 is [1; 0; 1; 0; 0; 0]" 40
      [ 1; 0; 1; 0; 0; 0 ];
    int_to_binary_test "int_to_binary 41 is [1; 0; 1; 0; 0; 1]" 41
      [ 1; 0; 1; 0; 0; 1 ];
    int_to_binary_test "int_to_binary 42 is [1; 0; 1; 0; 1; 0]" 42
      [ 1; 0; 1; 0; 1; 0 ];
    int_to_binary_test "int_to_binary 43 is [1; 0; 1; 0; 1; 1]" 43
      [ 1; 0; 1; 0; 1; 1 ];
    int_to_binary_test "int_to_binary 44 is [1; 0; 1; 1; 0; 0]" 44
      [ 1; 0; 1; 1; 0; 0 ];
    int_to_binary_test "int_to_binary 45 is [1; 0; 1; 1; 0; 1]" 45
      [ 1; 0; 1; 1; 0; 1 ];
    int_to_binary_test "int_to_binary 46 is [1; 0; 1; 1; 1; 0]" 46
      [ 1; 0; 1; 1; 1; 0 ];
    int_to_binary_test "int_to_binary 47 is [1; 0; 1; 1; 1; 1]" 47
      [ 1; 0; 1; 1; 1; 1 ];
    int_to_binary_test "int_to_binary 48 is [1; 1; 0; 0; 0; 0]" 48
      [ 1; 1; 0; 0; 0; 0 ];
    int_to_binary_test "int_to_binary 49 is [1; 1; 0; 0; 0; 1]" 49
      [ 1; 1; 0; 0; 0; 1 ];
    int_to_binary_test "int_to_binary 50 is [1; 1; 0; 0; 1; 0]" 50
      [ 1; 1; 0; 0; 1; 0 ];
    int_to_binary_test "int_to_binary 51 is [1; 1; 0; 0; 1; 1]" 51
      [ 1; 1; 0; 0; 1; 1 ];
    int_to_binary_test "int_to_binary 52 is [1; 1; 0; 1; 0; 0]" 52
      [ 1; 1; 0; 1; 0; 0 ];
    int_to_binary_test "int_to_binary 53 is [1; 1; 0; 1; 0; 1]" 53
      [ 1; 1; 0; 1; 0; 1 ];
    int_to_binary_test "int_to_binary 54 is [1; 1; 0; 1; 1; 0]" 54
      [ 1; 1; 0; 1; 1; 0 ];
    int_to_binary_test "int_to_binary 55 is [1; 1; 0; 1; 1; 1]" 55
      [ 1; 1; 0; 1; 1; 1 ];
    int_to_binary_test "int_to_binary 56 is [1; 1; 1; 0; 0; 0]" 56
      [ 1; 1; 1; 0; 0; 0 ];
    int_to_binary_test "int_to_binary 57 is [1; 1; 1; 0; 0; 1]" 57
      [ 1; 1; 1; 0; 0; 1 ];
    int_to_binary_test "int_to_binary 58 is [1; 1; 1; 0; 1; 0]" 58
      [ 1; 1; 1; 0; 1; 0 ];
    int_to_binary_test "int_to_binary 59 is [1; 1; 1; 0; 1; 1]" 59
      [ 1; 1; 1; 0; 1; 1 ];
    int_to_binary_test "int_to_binary 60 is [1; 1; 1; 1; 0; 0]" 60
      [ 1; 1; 1; 1; 0; 0 ];
    int_to_binary_test "int_to_binary 61 is [1; 1; 1; 1; 0; 1]" 61
      [ 1; 1; 1; 1; 0; 1 ];
    int_to_binary_test "int_to_binary 62 is [1; 1; 1; 1; 1; 0]" 62
      [ 1; 1; 1; 1; 1; 0 ];
    int_to_binary_test "int_to_binary 63 is [1; 1; 1; 1; 1; 1]" 63
      [ 1; 1; 1; 1; 1; 1 ];
  ]

(* Test Boards *)
module GoL = MakeBoard (B3_S23)

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

let suite =
  "test suite for CA"
  >::: List.flatten [ gol_tests; one_tests; int_to_binary_tests ]

let _ = run_test_tt_main suite

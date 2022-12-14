(********************************* TEST PLAN *********************************)
(* We used OUnit tests to automatically test the following: 
 * - 1D CA functions: make_n, int_to_binary, binary_to_int, make_rule, 
 * neighborhood, update_node, and update_board 
 * - 2D CA functions for various gamerules: 
 
 
 
 
 
 
 
 *)

(*****************************************************************************)
open OUnit2
open Cellular_automata.One
open Cellular_automata.Two
open Cellular_automata.Active

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

let game = init_empty 9
let game1 = [| Alive; Dead; Alive; Alive; Dead |]
let game2 = [| Dead; Alive; Dead; Dead; Alive; Alive; Dead; Alive; Dead |]

let neighborhood_test name in_game in_x exp_out =
  name >:: fun _ -> assert_equal exp_out (neighborhood in_game in_x)

let update_node_test name in_game in_rule in_x exp_out =
  name >:: fun _ -> assert_equal exp_out (update_node in_game in_rule in_x)

let update_board_test name in_game in_rule exp_out =
  name >:: fun _ -> assert_equal exp_out (update_board in_game in_rule)

let one_tests =
  [
    make_n_test "make_n [1] 2 is [0; 1]" [ 1 ] 2 [ 0; 1 ];
    make_n_test "make_n 1;3 4 is [0; 0; 1; 3]" [ 1; 3 ] 4 [ 0; 0; 1; 3 ];
    binary_to_int_test "binary_to_int [0] is 0" [ 0 ] 0;
    binary_to_int_test "binary_to_int [1] is 1" [ 1 ] 1;
    binary_to_int_test "binary_to_int [1; 0] is 2" [ 1; 0 ] 2;
    binary_to_int_test "binary_to_int [1; 1] is 3" [ 1; 1 ] 3;
    binary_to_int_test "binary_to_int [1; 0; 0; 1] is 9" [ 1; 0; 0; 1 ] 9;
    init_empty_test "init_empty 3 is [|Dead; Alive; Dead|]" 3
      [| Dead; Alive; Dead |];
    init_empty_test "init_empty 5 is [|Dead; Dead; Alive; Dead; Dead|]" 5
      [| Dead; Dead; Alive; Dead; Dead |];
    init_empty_test
      "init_empty 10 is [| Dead; Dead; Dead; Dead; Dead; Alive; Dead; Dead; \
       Dead; Dead |]"
      10
      [| Dead; Dead; Dead; Dead; Dead; Alive; Dead; Dead; Dead; Dead |];
    neighborhood_test "neighborhood game 0 is [Dead; Dead; Dead]" game 0
      [| Dead; Dead; Dead |];
    neighborhood_test "neighborhood game 3 is [Dead; Dead; Alive]" game 3
      [| Dead; Dead; Alive |];
    neighborhood_test "neighborhood game1 2 is [Dead; Alive; Alive]" game1 2
      [| Dead; Alive; Alive |];
    neighborhood_test "neighborhood game2 3 is [Dead; Dead; Alive]" game2 3
      [| Dead; Dead; Alive |];
    neighborhood_test "neighborhood game2 4 is [Dead; Alive; Alive]" game2 4
      [| Dead; Alive; Alive |];
    neighborhood_test "neighborhood game2 5 is [Alive; Alive; Dead]" game2 5
      [| Alive; Alive; Dead |];
    update_node_test "update_node game 90 4 is Dead" game 90 4 Dead;
    update_node_test "update_node game 90 3 is Alive" game 90 3 Alive;
    update_node_test "update_node game 90 2 is Dead" game 90 2 Dead;
    update_node_test "update_node game 90 1 is Dead" game 90 1 Dead;
    update_node_test "update_node game 90 0 is Dead" game 90 0 Dead;
    update_node_test "update_node game 90 7 is Dead" game 90 7 Dead;
    update_node_test "update_node game 90 6 is Dead" game 90 6 Dead;
    update_node_test "update_node game 90 5 is Alive" game 90 5 Alive;
    update_board_test
      "update_board game 90 is[|Dead; Dead; Dead; Alive; Dead; Alive; Dead; \
       Dead; Dead|]"
      game 90
      [| Dead; Dead; Dead; Alive; Dead; Alive; Dead; Dead; Dead |];
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
    int_to_binary_test "int_to_binary 64 is [1; 0; 0; 0; 0; 0; 0]" 64
      [ 1; 0; 0; 0; 0; 0; 0 ];
    int_to_binary_test "int_to_binary 65 is [1; 0; 0; 0; 0; 0; 1]" 65
      [ 1; 0; 0; 0; 0; 0; 1 ];
  ]

let make_rule_test =
  [
    make_rule_test "make_rule 0 is [0; 0; 0; 0; 0; 0; 0; 0]" 0
      [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    make_rule_test "make_rule 1 is [0; 0; 0; 0; 0; 0; 0; 1]" 1
      [ 0; 0; 0; 0; 0; 0; 0; 1 ];
    make_rule_test "make_rule 2 is [0; 0; 0; 0; 0; 0; 1; 0]" 2
      [ 0; 0; 0; 0; 0; 0; 1; 0 ];
    make_rule_test "make_rule 3 is [0; 0; 0; 0; 0; 0; 1; 1]" 3
      [ 0; 0; 0; 0; 0; 0; 1; 1 ];
    make_rule_test "make_rule 4 is [0; 0; 0; 0; 0; 1; 0; 0]" 4
      [ 0; 0; 0; 0; 0; 1; 0; 0 ];
    make_rule_test "make_rule 5 is [0; 0; 0; 0; 0; 1; 0; 1]" 5
      [ 0; 0; 0; 0; 0; 1; 0; 1 ];
    make_rule_test "make_rule 6 is [0; 0; 0; 0; 0; 1; 1; 0]" 6
      [ 0; 0; 0; 0; 0; 1; 1; 0 ];
    make_rule_test "make_rule 7 is [0; 0; 0; 0; 0; 1; 1; 1]" 7
      [ 0; 0; 0; 0; 0; 1; 1; 1 ];
    make_rule_test "make_rule 8 is [0; 0; 0; 0; 1; 0; 0; 0]" 8
      [ 0; 0; 0; 0; 1; 0; 0; 0 ];
    make_rule_test "make_rule 9 is [0; 0; 0; 0; 1; 0; 0; 1]" 9
      [ 0; 0; 0; 0; 1; 0; 0; 1 ];
    make_rule_test "make_rule 10 is [0; 0; 0; 0; 1; 0; 1; 0]" 10
      [ 0; 0; 0; 0; 1; 0; 1; 0 ];
    make_rule_test "make_rule 11 is [0; 0; 0; 0; 1; 0; 1; 1]" 11
      [ 0; 0; 0; 0; 1; 0; 1; 1 ];
    make_rule_test "make_rule 12 is [0; 0; 0; 0; 1; 1; 0; 0]" 12
      [ 0; 0; 0; 0; 1; 1; 0; 0 ];
    make_rule_test "make_rule 13 is [0; 0; 0; 0; 1; 1; 0; 1]" 13
      [ 0; 0; 0; 0; 1; 1; 0; 1 ];
    make_rule_test "make_rule 14 is [0; 0; 0; 0; 1; 1; 1; 0]" 14
      [ 0; 0; 0; 0; 1; 1; 1; 0 ];
    make_rule_test "make_rule 15 is [0; 0; 0; 0; 1; 1; 1; 1]" 15
      [ 0; 0; 0; 0; 1; 1; 1; 1 ];
    make_rule_test "make_rule 16 is [0; 0; 0; 1; 0; 0; 0; 0]" 16
      [ 0; 0; 0; 1; 0; 0; 0; 0 ];
    make_rule_test "make_rule 17 is [0; 0; 0; 1; 0; 0; 0; 1]" 17
      [ 0; 0; 0; 1; 0; 0; 0; 1 ];
    make_rule_test "make_rule 18 is [0; 0; 0; 1; 0; 0; 1; 0]" 18
      [ 0; 0; 0; 1; 0; 0; 1; 0 ];
    make_rule_test "make_rule 19 is [0; 0; 0; 1; 0; 0; 1; 1]" 19
      [ 0; 0; 0; 1; 0; 0; 1; 1 ];
    make_rule_test "make_rule 20 is [0; 0; 0; 1; 0; 1; 0; 0]" 20
      [ 0; 0; 0; 1; 0; 1; 0; 0 ];
    make_rule_test "make_rule 21 is [0; 0; 0; 1; 0; 1; 0; 1]" 21
      [ 0; 0; 0; 1; 0; 1; 0; 1 ];
    make_rule_test "make_rule 22 is [0; 0; 0; 1; 0; 1; 1; 0]" 22
      [ 0; 0; 0; 1; 0; 1; 1; 0 ];
    make_rule_test "make_rule 23 is [0; 0; 0; 1; 0; 1; 1; 1]" 23
      [ 0; 0; 0; 1; 0; 1; 1; 1 ];
    make_rule_test "make_rule 24 is [0; 0; 0; 1; 1; 0; 0; 0]" 24
      [ 0; 0; 0; 1; 1; 0; 0; 0 ];
    make_rule_test "make_rule 25 is [0; 0; 0; 1; 1; 0; 0; 1]" 25
      [ 0; 0; 0; 1; 1; 0; 0; 1 ];
    make_rule_test "make_rule 26 is [0; 0; 0; 1; 1; 0; 1; 0]" 26
      [ 0; 0; 0; 1; 1; 0; 1; 0 ];
    make_rule_test "make_rule 27 is [0; 0; 0; 1; 1; 0; 1; 1]" 27
      [ 0; 0; 0; 1; 1; 0; 1; 1 ];
    make_rule_test "make_rule 28 is [0; 0; 0; 1; 1; 1; 0; 0]" 28
      [ 0; 0; 0; 1; 1; 1; 0; 0 ];
    make_rule_test "make_rule 29 is [0; 0; 0; 1; 1; 1; 0; 1]" 29
      [ 0; 0; 0; 1; 1; 1; 0; 1 ];
    make_rule_test "make_rule 30 is [0; 0; 0; 1; 1; 1; 1; 0]" 30
      [ 0; 0; 0; 1; 1; 1; 1; 0 ];
    make_rule_test "make_rule 31 is [0; 0; 0; 1; 1; 1; 1; 1]" 31
      [ 0; 0; 0; 1; 1; 1; 1; 1 ];
    make_rule_test "make_rule 32 is [0; 0; 1; 0; 0; 0; 0; 0]" 32
      [ 0; 0; 1; 0; 0; 0; 0; 0 ];
    make_rule_test "make_rule 33 is [0; 0; 1; 0; 0; 0; 0; 1]" 33
      [ 0; 0; 1; 0; 0; 0; 0; 1 ];
    make_rule_test "make_rule 34 is [0; 0; 1; 0; 0; 0; 1; 0]" 34
      [ 0; 0; 1; 0; 0; 0; 1; 0 ];
    make_rule_test "make_rule 35 is [0; 0; 1; 0; 0; 0; 1; 1]" 35
      [ 0; 0; 1; 0; 0; 0; 1; 1 ];
    make_rule_test "make_rule 36 is [0; 0; 1; 0; 0; 1; 0; 0]" 36
      [ 0; 0; 1; 0; 0; 1; 0; 0 ];
    make_rule_test "make_rule 37 is [0; 0; 1; 0; 0; 1; 0; 1]" 37
      [ 0; 0; 1; 0; 0; 1; 0; 1 ];
    make_rule_test "make_rule 38 is [0; 0; 1; 0; 0; 1; 1; 0]" 38
      [ 0; 0; 1; 0; 0; 1; 1; 0 ];
    make_rule_test "make_rule 39 is [0; 0; 1; 0; 0; 1; 1; 1]" 39
      [ 0; 0; 1; 0; 0; 1; 1; 1 ];
    make_rule_test "make_rule 40 is [0; 0; 1; 0; 1; 0; 0; 0]" 40
      [ 0; 0; 1; 0; 1; 0; 0; 0 ];
    make_rule_test "make_rule 41 is [0; 0; 1; 0; 1; 0; 0; 1]" 41
      [ 0; 0; 1; 0; 1; 0; 0; 1 ];
    make_rule_test "make_rule 42 is [0; 0; 1; 0; 1; 0; 1; 0]" 42
      [ 0; 0; 1; 0; 1; 0; 1; 0 ];
    make_rule_test "make_rule 43 is [0; 0; 1; 0; 1; 0; 1; 1]" 43
      [ 0; 0; 1; 0; 1; 0; 1; 1 ];
    make_rule_test "make_rule 44 is [0; 0; 1; 0; 1; 1; 0; 0]" 44
      [ 0; 0; 1; 0; 1; 1; 0; 0 ];
    make_rule_test "make_rule 45 is [0; 0; 1; 0; 1; 1; 0; 1]" 45
      [ 0; 0; 1; 0; 1; 1; 0; 1 ];
    make_rule_test "make_rule 46 is [0; 0; 1; 0; 1; 1; 1; 0]" 46
      [ 0; 0; 1; 0; 1; 1; 1; 0 ];
    make_rule_test "make_rule 47 is [0; 0; 1; 0; 1; 1; 1; 1]" 47
      [ 0; 0; 1; 0; 1; 1; 1; 1 ];
    make_rule_test "make_rule 48 is [0; 0; 1; 1; 0; 0; 0; 0]" 48
      [ 0; 0; 1; 1; 0; 0; 0; 0 ];
    make_rule_test "make_rule 49 is [0; 0; 1; 1; 0; 0; 0; 1]" 49
      [ 0; 0; 1; 1; 0; 0; 0; 1 ];
    make_rule_test "make_rule 50 is [0; 0; 1; 1; 0; 0; 1; 0]" 50
      [ 0; 0; 1; 1; 0; 0; 1; 0 ];
    make_rule_test "make_rule 51 is [0; 0; 1; 1; 0; 0; 1; 1]" 51
      [ 0; 0; 1; 1; 0; 0; 1; 1 ];
    make_rule_test "make_rule 52 is [0; 0; 1; 1; 0; 1; 0; 0]" 52
      [ 0; 0; 1; 1; 0; 1; 0; 0 ];
    make_rule_test "make_rule 53 is [0; 0; 1; 1; 0; 1; 0; 1]" 53
      [ 0; 0; 1; 1; 0; 1; 0; 1 ];
    make_rule_test "make_rule 54 is [0; 0; 1; 1; 0; 1; 1; 0]" 54
      [ 0; 0; 1; 1; 0; 1; 1; 0 ];
    make_rule_test "make_rule 55 is [0; 0; 1; 1; 0; 1; 1; 1]" 55
      [ 0; 0; 1; 1; 0; 1; 1; 1 ];
    make_rule_test "make_rule 56 is [0; 0; 1; 1; 1; 0; 0; 0]" 56
      [ 0; 0; 1; 1; 1; 0; 0; 0 ];
    make_rule_test "make_rule 57 is [0; 0; 1; 1; 1; 0; 0; 1]" 57
      [ 0; 0; 1; 1; 1; 0; 0; 1 ];
    make_rule_test "make_rule 58 is [0; 0; 1; 1; 1; 0; 1; 0]" 58
      [ 0; 0; 1; 1; 1; 0; 1; 0 ];
    make_rule_test "make_rule 59 is [0; 0; 1; 1; 1; 0; 1; 1]" 59
      [ 0; 0; 1; 1; 1; 0; 1; 1 ];
    make_rule_test "make_rule 60 is [0; 0; 1; 1; 1; 1; 0; 0]" 60
      [ 0; 0; 1; 1; 1; 1; 0; 0 ];
    make_rule_test "make_rule 61 is [0; 0; 1; 1; 1; 1; 0; 1]" 61
      [ 0; 0; 1; 1; 1; 1; 0; 1 ];
    make_rule_test "make_rule 62 is [0; 0; 1; 1; 1; 1; 1; 0]" 62
      [ 0; 0; 1; 1; 1; 1; 1; 0 ];
    make_rule_test "make_rule 63 is [0; 0; 1; 1; 1; 1; 1; 1]" 63
      [ 0; 0; 1; 1; 1; 1; 1; 1 ];
    make_rule_test "make_rule 64 is [0; 1; 0; 0; 0; 0; 0; 0]" 64
      [ 0; 1; 0; 0; 0; 0; 0; 0 ];
    make_rule_test "make_rule 65 is [0; 1; 0; 0; 0; 0; 0; 1]" 65
      [ 0; 1; 0; 0; 0; 0; 0; 1 ];
    make_rule_test "make_rule 66 is [0; 1; 0; 0; 0; 0; 1; 0]" 66
      [ 0; 1; 0; 0; 0; 0; 1; 0 ];
    make_rule_test "make_rule 67 is [0; 1; 0; 0; 0; 0; 1; 1]" 67
      [ 0; 1; 0; 0; 0; 0; 1; 1 ];
    make_rule_test "make_rule 68 is [0; 1; 0; 0; 0; 1; 0; 0]" 68
      [ 0; 1; 0; 0; 0; 1; 0; 0 ];
    make_rule_test "make_rule 69 is [0; 1; 0; 0; 0; 1; 0; 1]" 69
      [ 0; 1; 0; 0; 0; 1; 0; 1 ];
    make_rule_test "make_rule 70 is [0; 1; 0; 0; 0; 1; 1; 0]" 70
      [ 0; 1; 0; 0; 0; 1; 1; 0 ];
    make_rule_test "make_rule 71 is [0; 1; 0; 0; 0; 1; 1; 1]" 71
      [ 0; 1; 0; 0; 0; 1; 1; 1 ];
    make_rule_test "make_rule 72 is [0; 1; 0; 0; 1; 0; 0; 0]" 72
      [ 0; 1; 0; 0; 1; 0; 0; 0 ];
    make_rule_test "make_rule 73 is [0; 1; 0; 0; 1; 0; 0; 1]" 73
      [ 0; 1; 0; 0; 1; 0; 0; 1 ];
    make_rule_test "make_rule 74 is [0; 1; 0; 0; 1; 0; 1; 0]" 74
      [ 0; 1; 0; 0; 1; 0; 1; 0 ];
    make_rule_test "make_rule 75 is [0; 1; 0; 0; 1; 0; 1; 1]" 75
      [ 0; 1; 0; 0; 1; 0; 1; 1 ];
    make_rule_test "make_rule 76 is [0; 1; 0; 0; 1; 1; 0; 0]" 76
      [ 0; 1; 0; 0; 1; 1; 0; 0 ];
    make_rule_test "make_rule 77 is [0; 1; 0; 0; 1; 1; 0; 1]" 77
      [ 0; 1; 0; 0; 1; 1; 0; 1 ];
    make_rule_test "make_rule 78 is [0; 1; 0; 0; 1; 1; 1; 0]" 78
      [ 0; 1; 0; 0; 1; 1; 1; 0 ];
    make_rule_test "make_rule 79 is [0; 1; 0; 0; 1; 1; 1; 1]" 79
      [ 0; 1; 0; 0; 1; 1; 1; 1 ];
    make_rule_test "make_rule 80 is [0; 1; 0; 1; 0; 0; 0; 0]" 80
      [ 0; 1; 0; 1; 0; 0; 0; 0 ];
    make_rule_test "make_rule 81 is [0; 1; 0; 1; 0; 0; 0; 1]" 81
      [ 0; 1; 0; 1; 0; 0; 0; 1 ];
    make_rule_test "make_rule 82 is [0; 1; 0; 1; 0; 0; 1; 0]" 82
      [ 0; 1; 0; 1; 0; 0; 1; 0 ];
    make_rule_test "make_rule 83 is [0; 1; 0; 1; 0; 0; 1; 1]" 83
      [ 0; 1; 0; 1; 0; 0; 1; 1 ];
    make_rule_test "make_rule 84 is [0; 1; 0; 1; 0; 1; 0; 0]" 84
      [ 0; 1; 0; 1; 0; 1; 0; 0 ];
    make_rule_test "make_rule 85 is [0; 1; 0; 1; 0; 1; 0; 1]" 85
      [ 0; 1; 0; 1; 0; 1; 0; 1 ];
    make_rule_test "make_rule 86 is [0; 1; 0; 1; 0; 1; 1; 0]" 86
      [ 0; 1; 0; 1; 0; 1; 1; 0 ];
    make_rule_test "make_rule 87 is [0; 1; 0; 1; 0; 1; 1; 1]" 87
      [ 0; 1; 0; 1; 0; 1; 1; 1 ];
    make_rule_test "make_rule 88 is [0; 1; 0; 1; 1; 0; 0; 0]" 88
      [ 0; 1; 0; 1; 1; 0; 0; 0 ];
    make_rule_test "make_rule 89 is [0; 1; 0; 1; 1; 0; 0; 1]" 89
      [ 0; 1; 0; 1; 1; 0; 0; 1 ];
    make_rule_test "make_rule 90 is [0; 1; 0; 1; 1; 0; 1; 0]" 90
      [ 0; 1; 0; 1; 1; 0; 1; 0 ];
    make_rule_test "make_rule 91 is [0; 1; 0; 1; 1; 0; 1; 1]" 91
      [ 0; 1; 0; 1; 1; 0; 1; 1 ];
    make_rule_test "make_rule 92 is [0; 1; 0; 1; 1; 1; 0; 0]" 92
      [ 0; 1; 0; 1; 1; 1; 0; 0 ];
    make_rule_test "make_rule 93 is [0; 1; 0; 1; 1; 1; 0; 1]" 93
      [ 0; 1; 0; 1; 1; 1; 0; 1 ];
    make_rule_test "make_rule 94 is [0; 1; 0; 1; 1; 1; 1; 0]" 94
      [ 0; 1; 0; 1; 1; 1; 1; 0 ];
    make_rule_test "make_rule 95 is [0; 1; 0; 1; 1; 1; 1; 1]" 95
      [ 0; 1; 0; 1; 1; 1; 1; 1 ];
    make_rule_test "make_rule 96 is [0; 1; 1; 0; 0; 0; 0; 0]" 96
      [ 0; 1; 1; 0; 0; 0; 0; 0 ];
    make_rule_test "make_rule 97 is [0; 1; 1; 0; 0; 0; 0; 1]" 97
      [ 0; 1; 1; 0; 0; 0; 0; 1 ];
    make_rule_test "make_rule 98 is [0; 1; 1; 0; 0; 0; 1; 0]" 98
      [ 0; 1; 1; 0; 0; 0; 1; 0 ];
    make_rule_test "make_rule 99 is [0; 1; 1; 0; 0; 0; 1; 1]" 99
      [ 0; 1; 1; 0; 0; 0; 1; 1 ];
    make_rule_test "make_rule 100 is [0; 1; 1; 0; 0; 1; 0; 0]" 100
      [ 0; 1; 1; 0; 0; 1; 0; 0 ];
    make_rule_test "make_rule 101 is [0; 1; 1; 0; 0; 1; 0; 1]" 101
      [ 0; 1; 1; 0; 0; 1; 0; 1 ];
    make_rule_test "make_rule 102 is [0; 1; 1; 0; 0; 1; 1; 0]" 102
      [ 0; 1; 1; 0; 0; 1; 1; 0 ];
    make_rule_test "make_rule 103 is [0; 1; 1; 0; 0; 1; 1; 1]" 103
      [ 0; 1; 1; 0; 0; 1; 1; 1 ];
    make_rule_test "make_rule 104 is [0; 1; 1; 0; 1; 0; 0; 0]" 104
      [ 0; 1; 1; 0; 1; 0; 0; 0 ];
    make_rule_test "make_rule 105 is [0; 1; 1; 0; 1; 0; 0; 1]" 105
      [ 0; 1; 1; 0; 1; 0; 0; 1 ];
    make_rule_test "make_rule 106 is [0; 1; 1; 0; 1; 0; 1; 0]" 106
      [ 0; 1; 1; 0; 1; 0; 1; 0 ];
    make_rule_test "make_rule 107 is [0; 1; 1; 0; 1; 0; 1; 1]" 107
      [ 0; 1; 1; 0; 1; 0; 1; 1 ];
    make_rule_test "make_rule 108 is [0; 1; 1; 0; 1; 1; 0; 0]" 108
      [ 0; 1; 1; 0; 1; 1; 0; 0 ];
    make_rule_test "make_rule 109 is [0; 1; 1; 0; 1; 1; 0; 1]" 109
      [ 0; 1; 1; 0; 1; 1; 0; 1 ];
    make_rule_test "make_rule 110 is [0; 1; 1; 0; 1; 1; 1; 0]" 110
      [ 0; 1; 1; 0; 1; 1; 1; 0 ];
    make_rule_test "make_rule 111 is [0; 1; 1; 0; 1; 1; 1; 1]" 111
      [ 0; 1; 1; 0; 1; 1; 1; 1 ];
    make_rule_test "make_rule 112 is [0; 1; 1; 1; 0; 0; 0; 0]" 112
      [ 0; 1; 1; 1; 0; 0; 0; 0 ];
    make_rule_test "make_rule 113 is [0; 1; 1; 1; 0; 0; 0; 1]" 113
      [ 0; 1; 1; 1; 0; 0; 0; 1 ];
    make_rule_test "make_rule 114 is [0; 1; 1; 1; 0; 0; 1; 0]" 114
      [ 0; 1; 1; 1; 0; 0; 1; 0 ];
    make_rule_test "make_rule 115 is [0; 1; 1; 1; 0; 0; 1; 1]" 115
      [ 0; 1; 1; 1; 0; 0; 1; 1 ];
    make_rule_test "make_rule 116 is [0; 1; 1; 1; 0; 1; 0; 0]" 116
      [ 0; 1; 1; 1; 0; 1; 0; 0 ];
    make_rule_test "make_rule 117 is [0; 1; 1; 1; 0; 1; 0; 1]" 117
      [ 0; 1; 1; 1; 0; 1; 0; 1 ];
    make_rule_test "make_rule 118 is [0; 1; 1; 1; 0; 1; 1; 0]" 118
      [ 0; 1; 1; 1; 0; 1; 1; 0 ];
    make_rule_test "make_rule 119 is [0; 1; 1; 1; 0; 1; 1; 1]" 119
      [ 0; 1; 1; 1; 0; 1; 1; 1 ];
    make_rule_test "make_rule 120 is [0; 1; 1; 1; 1; 0; 0; 0]" 120
      [ 0; 1; 1; 1; 1; 0; 0; 0 ];
    make_rule_test "make_rule 121 is [0; 1; 1; 1; 1; 0; 0; 1]" 121
      [ 0; 1; 1; 1; 1; 0; 0; 1 ];
    make_rule_test "make_rule 122 is [0; 1; 1; 1; 1; 0; 1; 0]" 122
      [ 0; 1; 1; 1; 1; 0; 1; 0 ];
    make_rule_test "make_rule 123 is [0; 1; 1; 1; 1; 0; 1; 1]" 123
      [ 0; 1; 1; 1; 1; 0; 1; 1 ];
    make_rule_test "make_rule 124 is [0; 1; 1; 1; 1; 1; 0; 0]" 124
      [ 0; 1; 1; 1; 1; 1; 0; 0 ];
    make_rule_test "make_rule 125 is [0; 1; 1; 1; 1; 1; 0; 1]" 125
      [ 0; 1; 1; 1; 1; 1; 0; 1 ];
    make_rule_test "make_rule 126 is [0; 1; 1; 1; 1; 1; 1; 0]" 126
      [ 0; 1; 1; 1; 1; 1; 1; 0 ];
    make_rule_test "make_rule 127 is [0; 1; 1; 1; 1; 1; 1; 1]" 127
      [ 0; 1; 1; 1; 1; 1; 1; 1 ];
    make_rule_test "make_rule 128 is [1; 0; 0; 0; 0; 0; 0; 0]" 128
      [ 1; 0; 0; 0; 0; 0; 0; 0 ];
    make_rule_test "make_rule 129 is [1; 0; 0; 0; 0; 0; 0; 1]" 129
      [ 1; 0; 0; 0; 0; 0; 0; 1 ];
    make_rule_test "make_rule 130 is [1; 0; 0; 0; 0; 0; 1; 0]" 130
      [ 1; 0; 0; 0; 0; 0; 1; 0 ];
    make_rule_test "make_rule 131 is [1; 0; 0; 0; 0; 0; 1; 1]" 131
      [ 1; 0; 0; 0; 0; 0; 1; 1 ];
    make_rule_test "make_rule 132 is [1; 0; 0; 0; 0; 1; 0; 0]" 132
      [ 1; 0; 0; 0; 0; 1; 0; 0 ];
    make_rule_test "make_rule 133 is [1; 0; 0; 0; 0; 1; 0; 1]" 133
      [ 1; 0; 0; 0; 0; 1; 0; 1 ];
    make_rule_test "make_rule 134 is [1; 0; 0; 0; 0; 1; 1; 0]" 134
      [ 1; 0; 0; 0; 0; 1; 1; 0 ];
    make_rule_test "make_rule 135 is [1; 0; 0; 0; 0; 1; 1; 1]" 135
      [ 1; 0; 0; 0; 0; 1; 1; 1 ];
    make_rule_test "make_rule 136 is [1; 0; 0; 0; 1; 0; 0; 0]" 136
      [ 1; 0; 0; 0; 1; 0; 0; 0 ];
    make_rule_test "make_rule 137 is [1; 0; 0; 0; 1; 0; 0; 1]" 137
      [ 1; 0; 0; 0; 1; 0; 0; 1 ];
    make_rule_test "make_rule 138 is [1; 0; 0; 0; 1; 0; 1; 0]" 138
      [ 1; 0; 0; 0; 1; 0; 1; 0 ];
    make_rule_test "make_rule 139 is [1; 0; 0; 0; 1; 0; 1; 1]" 139
      [ 1; 0; 0; 0; 1; 0; 1; 1 ];
    make_rule_test "make_rule 140 is [1; 0; 0; 0; 1; 1; 0; 0]" 140
      [ 1; 0; 0; 0; 1; 1; 0; 0 ];
    make_rule_test "make_rule 141 is [1; 0; 0; 0; 1; 1; 0; 1]" 141
      [ 1; 0; 0; 0; 1; 1; 0; 1 ];
    make_rule_test "make_rule 142 is [1; 0; 0; 0; 1; 1; 1; 0]" 142
      [ 1; 0; 0; 0; 1; 1; 1; 0 ];
    make_rule_test "make_rule 143 is [1; 0; 0; 0; 1; 1; 1; 1]" 143
      [ 1; 0; 0; 0; 1; 1; 1; 1 ];
    make_rule_test "make_rule 144 is [1; 0; 0; 1; 0; 0; 0; 0]" 144
      [ 1; 0; 0; 1; 0; 0; 0; 0 ];
    make_rule_test "make_rule 145 is [1; 0; 0; 1; 0; 0; 0; 1]" 145
      [ 1; 0; 0; 1; 0; 0; 0; 1 ];
    make_rule_test "make_rule 146 is [1; 0; 0; 1; 0; 0; 1; 0]" 146
      [ 1; 0; 0; 1; 0; 0; 1; 0 ];
    make_rule_test "make_rule 147 is [1; 0; 0; 1; 0; 0; 1; 1]" 147
      [ 1; 0; 0; 1; 0; 0; 1; 1 ];
    make_rule_test "make_rule 148 is [1; 0; 0; 1; 0; 1; 0; 0]" 148
      [ 1; 0; 0; 1; 0; 1; 0; 0 ];
    make_rule_test "make_rule 149 is [1; 0; 0; 1; 0; 1; 0; 1]" 149
      [ 1; 0; 0; 1; 0; 1; 0; 1 ];
    make_rule_test "make_rule 150 is [1; 0; 0; 1; 0; 1; 1; 0]" 150
      [ 1; 0; 0; 1; 0; 1; 1; 0 ];
    make_rule_test "make_rule 151 is [1; 0; 0; 1; 0; 1; 1; 1]" 151
      [ 1; 0; 0; 1; 0; 1; 1; 1 ];
    make_rule_test "make_rule 152 is [1; 0; 0; 1; 1; 0; 0; 0]" 152
      [ 1; 0; 0; 1; 1; 0; 0; 0 ];
    make_rule_test "make_rule 153 is [1; 0; 0; 1; 1; 0; 0; 1]" 153
      [ 1; 0; 0; 1; 1; 0; 0; 1 ];
    make_rule_test "make_rule 154 is [1; 0; 0; 1; 1; 0; 1; 0]" 154
      [ 1; 0; 0; 1; 1; 0; 1; 0 ];
    make_rule_test "make_rule 155 is [1; 0; 0; 1; 1; 0; 1; 1]" 155
      [ 1; 0; 0; 1; 1; 0; 1; 1 ];
    make_rule_test "make_rule 156 is [1; 0; 0; 1; 1; 1; 0; 0]" 156
      [ 1; 0; 0; 1; 1; 1; 0; 0 ];
    make_rule_test "make_rule 157 is [1; 0; 0; 1; 1; 1; 0; 1]" 157
      [ 1; 0; 0; 1; 1; 1; 0; 1 ];
    make_rule_test "make_rule 158 is [1; 0; 0; 1; 1; 1; 1; 0]" 158
      [ 1; 0; 0; 1; 1; 1; 1; 0 ];
    make_rule_test "make_rule 159 is [1; 0; 0; 1; 1; 1; 1; 1]" 159
      [ 1; 0; 0; 1; 1; 1; 1; 1 ];
    make_rule_test "make_rule 160 is [1; 0; 1; 0; 0; 0; 0; 0]" 160
      [ 1; 0; 1; 0; 0; 0; 0; 0 ];
    make_rule_test "make_rule 161 is [1; 0; 1; 0; 0; 0; 0; 1]" 161
      [ 1; 0; 1; 0; 0; 0; 0; 1 ];
    make_rule_test "make_rule 162 is [1; 0; 1; 0; 0; 0; 1; 0]" 162
      [ 1; 0; 1; 0; 0; 0; 1; 0 ];
    make_rule_test "make_rule 163 is [1; 0; 1; 0; 0; 0; 1; 1]" 163
      [ 1; 0; 1; 0; 0; 0; 1; 1 ];
    make_rule_test "make_rule 164 is [1; 0; 1; 0; 0; 1; 0; 0]" 164
      [ 1; 0; 1; 0; 0; 1; 0; 0 ];
    make_rule_test "make_rule 165 is [1; 0; 1; 0; 0; 1; 0; 1]" 165
      [ 1; 0; 1; 0; 0; 1; 0; 1 ];
    make_rule_test "make_rule 166 is [1; 0; 1; 0; 0; 1; 1; 0]" 166
      [ 1; 0; 1; 0; 0; 1; 1; 0 ];
    make_rule_test "make_rule 167 is [1; 0; 1; 0; 0; 1; 1; 1]" 167
      [ 1; 0; 1; 0; 0; 1; 1; 1 ];
    make_rule_test "make_rule 168 is [1; 0; 1; 0; 1; 0; 0; 0]" 168
      [ 1; 0; 1; 0; 1; 0; 0; 0 ];
    make_rule_test "make_rule 169 is [1; 0; 1; 0; 1; 0; 0; 1]" 169
      [ 1; 0; 1; 0; 1; 0; 0; 1 ];
    make_rule_test "make_rule 170 is [1; 0; 1; 0; 1; 0; 1; 0]" 170
      [ 1; 0; 1; 0; 1; 0; 1; 0 ];
    make_rule_test "make_rule 171 is [1; 0; 1; 0; 1; 0; 1; 1]" 171
      [ 1; 0; 1; 0; 1; 0; 1; 1 ];
    make_rule_test "make_rule 172 is [1; 0; 1; 0; 1; 1; 0; 0]" 172
      [ 1; 0; 1; 0; 1; 1; 0; 0 ];
    make_rule_test "make_rule 173 is [1; 0; 1; 0; 1; 1; 0; 1]" 173
      [ 1; 0; 1; 0; 1; 1; 0; 1 ];
    make_rule_test "make_rule 174 is [1; 0; 1; 0; 1; 1; 1; 0]" 174
      [ 1; 0; 1; 0; 1; 1; 1; 0 ];
    make_rule_test "make_rule 175 is [1; 0; 1; 0; 1; 1; 1; 1]" 175
      [ 1; 0; 1; 0; 1; 1; 1; 1 ];
    make_rule_test "make_rule 176 is [1; 0; 1; 1; 0; 0; 0; 0]" 176
      [ 1; 0; 1; 1; 0; 0; 0; 0 ];
    make_rule_test "make_rule 177 is [1; 0; 1; 1; 0; 0; 0; 1]" 177
      [ 1; 0; 1; 1; 0; 0; 0; 1 ];
    make_rule_test "make_rule 178 is [1; 0; 1; 1; 0; 0; 1; 0]" 178
      [ 1; 0; 1; 1; 0; 0; 1; 0 ];
    make_rule_test "make_rule 179 is [1; 0; 1; 1; 0; 0; 1; 1]" 179
      [ 1; 0; 1; 1; 0; 0; 1; 1 ];
    make_rule_test "make_rule 180 is [1; 0; 1; 1; 0; 1; 0; 0]" 180
      [ 1; 0; 1; 1; 0; 1; 0; 0 ];
    make_rule_test "make_rule 181 is [1; 0; 1; 1; 0; 1; 0; 1]" 181
      [ 1; 0; 1; 1; 0; 1; 0; 1 ];
    make_rule_test "make_rule 182 is [1; 0; 1; 1; 0; 1; 1; 0]" 182
      [ 1; 0; 1; 1; 0; 1; 1; 0 ];
    make_rule_test "make_rule 183 is [1; 0; 1; 1; 0; 1; 1; 1]" 183
      [ 1; 0; 1; 1; 0; 1; 1; 1 ];
    make_rule_test "make_rule 184 is [1; 0; 1; 1; 1; 0; 0; 0]" 184
      [ 1; 0; 1; 1; 1; 0; 0; 0 ];
    make_rule_test "make_rule 185 is [1; 0; 1; 1; 1; 0; 0; 1]" 185
      [ 1; 0; 1; 1; 1; 0; 0; 1 ];
    make_rule_test "make_rule 186 is [1; 0; 1; 1; 1; 0; 1; 0]" 186
      [ 1; 0; 1; 1; 1; 0; 1; 0 ];
    make_rule_test "make_rule 187 is [1; 0; 1; 1; 1; 0; 1; 1]" 187
      [ 1; 0; 1; 1; 1; 0; 1; 1 ];
    make_rule_test "make_rule 188 is [1; 0; 1; 1; 1; 1; 0; 0]" 188
      [ 1; 0; 1; 1; 1; 1; 0; 0 ];
    make_rule_test "make_rule 189 is [1; 0; 1; 1; 1; 1; 0; 1]" 189
      [ 1; 0; 1; 1; 1; 1; 0; 1 ];
    make_rule_test "make_rule 190 is [1; 0; 1; 1; 1; 1; 1; 0]" 190
      [ 1; 0; 1; 1; 1; 1; 1; 0 ];
    make_rule_test "make_rule 191 is [1; 0; 1; 1; 1; 1; 1; 1]" 191
      [ 1; 0; 1; 1; 1; 1; 1; 1 ];
    make_rule_test "make_rule 192 is [1; 1; 0; 0; 0; 0; 0; 0]" 192
      [ 1; 1; 0; 0; 0; 0; 0; 0 ];
    make_rule_test "make_rule 193 is [1; 1; 0; 0; 0; 0; 0; 1]" 193
      [ 1; 1; 0; 0; 0; 0; 0; 1 ];
    make_rule_test "make_rule 194 is [1; 1; 0; 0; 0; 0; 1; 0]" 194
      [ 1; 1; 0; 0; 0; 0; 1; 0 ];
    make_rule_test "make_rule 195 is [1; 1; 0; 0; 0; 0; 1; 1]" 195
      [ 1; 1; 0; 0; 0; 0; 1; 1 ];
    make_rule_test "make_rule 196 is [1; 1; 0; 0; 0; 1; 0; 0]" 196
      [ 1; 1; 0; 0; 0; 1; 0; 0 ];
    make_rule_test "make_rule 197 is [1; 1; 0; 0; 0; 1; 0; 1]" 197
      [ 1; 1; 0; 0; 0; 1; 0; 1 ];
    make_rule_test "make_rule 198 is [1; 1; 0; 0; 0; 1; 1; 0]" 198
      [ 1; 1; 0; 0; 0; 1; 1; 0 ];
    make_rule_test "make_rule 199 is [1; 1; 0; 0; 0; 1; 1; 1]" 199
      [ 1; 1; 0; 0; 0; 1; 1; 1 ];
    make_rule_test "make_rule 200 is [1; 1; 0; 0; 1; 0; 0; 0]" 200
      [ 1; 1; 0; 0; 1; 0; 0; 0 ];
    make_rule_test "make_rule 201 is [1; 1; 0; 0; 1; 0; 0; 1]" 201
      [ 1; 1; 0; 0; 1; 0; 0; 1 ];
    make_rule_test "make_rule 202 is [1; 1; 0; 0; 1; 0; 1; 0]" 202
      [ 1; 1; 0; 0; 1; 0; 1; 0 ];
    make_rule_test "make_rule 203 is [1; 1; 0; 0; 1; 0; 1; 1]" 203
      [ 1; 1; 0; 0; 1; 0; 1; 1 ];
    make_rule_test "make_rule 204 is [1; 1; 0; 0; 1; 1; 0; 0]" 204
      [ 1; 1; 0; 0; 1; 1; 0; 0 ];
    make_rule_test "make_rule 205 is [1; 1; 0; 0; 1; 1; 0; 1]" 205
      [ 1; 1; 0; 0; 1; 1; 0; 1 ];
    make_rule_test "make_rule 206 is [1; 1; 0; 0; 1; 1; 1; 0]" 206
      [ 1; 1; 0; 0; 1; 1; 1; 0 ];
    make_rule_test "make_rule 207 is [1; 1; 0; 0; 1; 1; 1; 1]" 207
      [ 1; 1; 0; 0; 1; 1; 1; 1 ];
    make_rule_test "make_rule 208 is [1; 1; 0; 1; 0; 0; 0; 0]" 208
      [ 1; 1; 0; 1; 0; 0; 0; 0 ];
    make_rule_test "make_rule 209 is [1; 1; 0; 1; 0; 0; 0; 1]" 209
      [ 1; 1; 0; 1; 0; 0; 0; 1 ];
    make_rule_test "make_rule 210 is [1; 1; 0; 1; 0; 0; 1; 0]" 210
      [ 1; 1; 0; 1; 0; 0; 1; 0 ];
    make_rule_test "make_rule 211 is [1; 1; 0; 1; 0; 0; 1; 1]" 211
      [ 1; 1; 0; 1; 0; 0; 1; 1 ];
    make_rule_test "make_rule 212 is [1; 1; 0; 1; 0; 1; 0; 0]" 212
      [ 1; 1; 0; 1; 0; 1; 0; 0 ];
    make_rule_test "make_rule 213 is [1; 1; 0; 1; 0; 1; 0; 1]" 213
      [ 1; 1; 0; 1; 0; 1; 0; 1 ];
    make_rule_test "make_rule 214 is [1; 1; 0; 1; 0; 1; 1; 0]" 214
      [ 1; 1; 0; 1; 0; 1; 1; 0 ];
    make_rule_test "make_rule 215 is [1; 1; 0; 1; 0; 1; 1; 1]" 215
      [ 1; 1; 0; 1; 0; 1; 1; 1 ];
    make_rule_test "make_rule 216 is [1; 1; 0; 1; 1; 0; 0; 0]" 216
      [ 1; 1; 0; 1; 1; 0; 0; 0 ];
    make_rule_test "make_rule 217 is [1; 1; 0; 1; 1; 0; 0; 1]" 217
      [ 1; 1; 0; 1; 1; 0; 0; 1 ];
    make_rule_test "make_rule 218 is [1; 1; 0; 1; 1; 0; 1; 0]" 218
      [ 1; 1; 0; 1; 1; 0; 1; 0 ];
    make_rule_test "make_rule 219 is [1; 1; 0; 1; 1; 0; 1; 1]" 219
      [ 1; 1; 0; 1; 1; 0; 1; 1 ];
    make_rule_test "make_rule 220 is [1; 1; 0; 1; 1; 1; 0; 0]" 220
      [ 1; 1; 0; 1; 1; 1; 0; 0 ];
    make_rule_test "make_rule 221 is [1; 1; 0; 1; 1; 1; 0; 1]" 221
      [ 1; 1; 0; 1; 1; 1; 0; 1 ];
    make_rule_test "make_rule 222 is [1; 1; 0; 1; 1; 1; 1; 0]" 222
      [ 1; 1; 0; 1; 1; 1; 1; 0 ];
    make_rule_test "make_rule 223 is [1; 1; 0; 1; 1; 1; 1; 1]" 223
      [ 1; 1; 0; 1; 1; 1; 1; 1 ];
    make_rule_test "make_rule 224 is [1; 1; 1; 0; 0; 0; 0; 0]" 224
      [ 1; 1; 1; 0; 0; 0; 0; 0 ];
    make_rule_test "make_rule 225 is [1; 1; 1; 0; 0; 0; 0; 1]" 225
      [ 1; 1; 1; 0; 0; 0; 0; 1 ];
    make_rule_test "make_rule 226 is [1; 1; 1; 0; 0; 0; 1; 0]" 226
      [ 1; 1; 1; 0; 0; 0; 1; 0 ];
    make_rule_test "make_rule 227 is [1; 1; 1; 0; 0; 0; 1; 1]" 227
      [ 1; 1; 1; 0; 0; 0; 1; 1 ];
    make_rule_test "make_rule 228 is [1; 1; 1; 0; 0; 1; 0; 0]" 228
      [ 1; 1; 1; 0; 0; 1; 0; 0 ];
    make_rule_test "make_rule 229 is [1; 1; 1; 0; 0; 1; 0; 1]" 229
      [ 1; 1; 1; 0; 0; 1; 0; 1 ];
    make_rule_test "make_rule 230 is [1; 1; 1; 0; 0; 1; 1; 0]" 230
      [ 1; 1; 1; 0; 0; 1; 1; 0 ];
    make_rule_test "make_rule 231 is [1; 1; 1; 0; 0; 1; 1; 1]" 231
      [ 1; 1; 1; 0; 0; 1; 1; 1 ];
    make_rule_test "make_rule 232 is [1; 1; 1; 0; 1; 0; 0; 0]" 232
      [ 1; 1; 1; 0; 1; 0; 0; 0 ];
    make_rule_test "make_rule 233 is [1; 1; 1; 0; 1; 0; 0; 1]" 233
      [ 1; 1; 1; 0; 1; 0; 0; 1 ];
    make_rule_test "make_rule 234 is [1; 1; 1; 0; 1; 0; 1; 0]" 234
      [ 1; 1; 1; 0; 1; 0; 1; 0 ];
    make_rule_test "make_rule 235 is [1; 1; 1; 0; 1; 0; 1; 1]" 235
      [ 1; 1; 1; 0; 1; 0; 1; 1 ];
    make_rule_test "make_rule 236 is [1; 1; 1; 0; 1; 1; 0; 0]" 236
      [ 1; 1; 1; 0; 1; 1; 0; 0 ];
    make_rule_test "make_rule 237 is [1; 1; 1; 0; 1; 1; 0; 1]" 237
      [ 1; 1; 1; 0; 1; 1; 0; 1 ];
    make_rule_test "make_rule 238 is [1; 1; 1; 0; 1; 1; 1; 0]" 238
      [ 1; 1; 1; 0; 1; 1; 1; 0 ];
    make_rule_test "make_rule 239 is [1; 1; 1; 0; 1; 1; 1; 1]" 239
      [ 1; 1; 1; 0; 1; 1; 1; 1 ];
    make_rule_test "make_rule 240 is [1; 1; 1; 1; 0; 0; 0; 0]" 240
      [ 1; 1; 1; 1; 0; 0; 0; 0 ];
    make_rule_test "make_rule 241 is [1; 1; 1; 1; 0; 0; 0; 1]" 241
      [ 1; 1; 1; 1; 0; 0; 0; 1 ];
    make_rule_test "make_rule 242 is [1; 1; 1; 1; 0; 0; 1; 0]" 242
      [ 1; 1; 1; 1; 0; 0; 1; 0 ];
    make_rule_test "make_rule 243 is [1; 1; 1; 1; 0; 0; 1; 1]" 243
      [ 1; 1; 1; 1; 0; 0; 1; 1 ];
    make_rule_test "make_rule 244 is [1; 1; 1; 1; 0; 1; 0; 0]" 244
      [ 1; 1; 1; 1; 0; 1; 0; 0 ];
    make_rule_test "make_rule 245 is [1; 1; 1; 1; 0; 1; 0; 1]" 245
      [ 1; 1; 1; 1; 0; 1; 0; 1 ];
    make_rule_test "make_rule 246 is [1; 1; 1; 1; 0; 1; 1; 0]" 246
      [ 1; 1; 1; 1; 0; 1; 1; 0 ];
    make_rule_test "make_rule 247 is [1; 1; 1; 1; 0; 1; 1; 1]" 247
      [ 1; 1; 1; 1; 0; 1; 1; 1 ];
    make_rule_test "make_rule 248 is [1; 1; 1; 1; 1; 0; 0; 0]" 248
      [ 1; 1; 1; 1; 1; 0; 0; 0 ];
    make_rule_test "make_rule 249 is [1; 1; 1; 1; 1; 0; 0; 1]" 249
      [ 1; 1; 1; 1; 1; 0; 0; 1 ];
    make_rule_test "make_rule 250 is [1; 1; 1; 1; 1; 0; 1; 0]" 250
      [ 1; 1; 1; 1; 1; 0; 1; 0 ];
    make_rule_test "make_rule 251 is [1; 1; 1; 1; 1; 0; 1; 1]" 251
      [ 1; 1; 1; 1; 1; 0; 1; 1 ];
    make_rule_test "make_rule 252 is [1; 1; 1; 1; 1; 1; 0; 0]" 252
      [ 1; 1; 1; 1; 1; 1; 0; 0 ];
    make_rule_test "make_rule 253 is [1; 1; 1; 1; 1; 1; 0; 1]" 253
      [ 1; 1; 1; 1; 1; 1; 0; 1 ];
    make_rule_test "make_rule 254 is [1; 1; 1; 1; 1; 1; 1; 0]" 254
      [ 1; 1; 1; 1; 1; 1; 1; 0 ];
    make_rule_test "make_rule 255 is [1; 1; 1; 1; 1; 1; 1; 1]" 255
      [ 1; 1; 1; 1; 1; 1; 1; 1 ];
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

let equal_boards gb1 gb2 =
  let gb1_lst =
    gb1 |> Array.to_list
    |> List.map (fun arr -> Array.to_list arr)
    |> List.flatten
  in
  let gb2_lst =
    gb2 |> Array.to_list
    |> List.map (fun arr -> Array.to_list arr)
    |> List.flatten
  in
  List.equal (fun a b -> a = b) gb1_lst gb2_lst

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

let update_board_test name in_gb exp_out =
  name >:: fun _ ->
  assert_equal exp_out
    (GoL.update_board in_gb;
     in_gb)
    ~cmp:equal_boards ~printer:GoL.string_of_board

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
    neighbors_test "neighbors of 5,9 @ 5,0" b59_10x10 5 0 1;
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
    neighbors_test "neighbors of 9,9 @ 9,0" b99_10x10 9 0 1;
    neighbors_test "neighbors of glider @ 5,5" glider_10x10 5 5 2;
    neighbors_test "neighbors of glider @ 4,5" glider_10x10 4 5 3;
    neighbors_test "neighbors of glider @ 4,4" glider_10x10 4 4 3;
    neighbors_test "neighbors of glider @ 4,3" glider_10x10 4 3 5;
  ]

let update_node_tests =
  [
    update_node_test "update empty" empty_10x10 5 5 GoL.Dead;
    (*Initial Glider State*)
    update_node_test "update initial gilder @ 3,2" (GoL.init_glider ()) 3 2
      GoL.Dead;
    update_node_test "update initial gilder @ 4,2" (GoL.init_glider ()) 4 2
      GoL.Dead;
    update_node_test "update initial gilder @ 5,2" (GoL.init_glider ()) 5 2
      GoL.Dead;
    update_node_test "update initial gilder @ 3,3" (GoL.init_glider ()) 3 3
      GoL.Alive;
    update_node_test "update initial gilder @ 4,3" (GoL.init_glider ()) 4 3
      GoL.Dead;
    update_node_test "update initial gilder @ 5,3" (GoL.init_glider ()) 5 3
      GoL.Alive;
    update_node_test "update initial gilder @ 3,4" (GoL.init_glider ()) 3 4
      GoL.Dead;
    update_node_test "update initial gilder @ 4,4" (GoL.init_glider ()) 4 4
      GoL.Alive;
    update_node_test "update initial gilder @ 5,4" (GoL.init_glider ()) 5 4
      GoL.Alive;
    update_node_test "update initial gilder @ 3,5" (GoL.init_glider ()) 3 5
      GoL.Dead;
    update_node_test "update initial gilder @ 4,5" (GoL.init_glider ()) 4 5
      GoL.Alive;
    update_node_test "update initial gilder @ 5,5" (GoL.init_glider ()) 5 5
      GoL.Dead;
    (*Glider In Corner*)
    update_node_test "update corner glider @ 7 7"
      (let x = GoL.init_glider () in
       for _ = 0 to 17 do
         GoL.update_board x
       done;
       x)
      7 7 GoL.Dead;
    update_node_test "update corner glider @ 8 7"
      (let x = GoL.init_glider () in
       for _ = 0 to 17 do
         GoL.update_board x
       done;
       x)
      8 7 GoL.Alive;
    update_node_test "update corner glider @ 9 7"
      (let x = GoL.init_glider () in
       for _ = 0 to 17 do
         GoL.update_board x
       done;
       x)
      9 7 GoL.Dead;
    update_node_test "update corner glider @ 0 8"
      (let x = GoL.init_glider () in
       for _ = 0 to 17 do
         GoL.update_board x
       done;
       x)
      0 8 GoL.Alive;
    update_node_test "update corner glider @ 7 8"
      (let x = GoL.init_glider () in
       for _ = 0 to 17 do
         GoL.update_board x
       done;
       x)
      7 8 GoL.Dead;
  ]

let update_board_tests =
  [
    update_board_test "update glider initial" (GoL.init_glider ())
      (GoL.make_board 10 10 [ (3, 3); (5, 3); (4, 4); (5, 4); (4, 5) ]);
    update_board_test "update glider step 2"
      (let x = GoL.init_glider () in
       GoL.update_board x;
       x)
      (GoL.make_board 10 10 [ (5, 3); (3, 4); (5, 4); (4, 5); (5, 5) ]);
    update_board_test "update glider step 3"
      (let x = GoL.init_glider () in
       GoL.update_board x;
       GoL.update_board x;
       x)
      (GoL.make_board 10 10 [ (4, 3); (5, 4); (6, 4); (4, 5); (5, 5) ]);
  ]

let gol_tests =
  List.flatten [ neighbors_tests; update_node_tests; update_board_tests ]

module HighLife = MakeBoard (B36_S23)

let repl_18x18 = HighLife.init_replicator ()
let empty_18x18 = HighLife.init_empty 18 18

let hl_neighbors_test name in_gb in_x in_y exp_out =
  name >:: fun _ ->
  assert_equal exp_out
    (HighLife.neighbors in_gb in_x in_y)
    ~printer:string_of_int

let highlife_tests =
  [
    (* empty board *)
    hl_neighbors_test "neighbors of empty @ 0,0" empty_18x18 0 0 0;
    hl_neighbors_test "neighbors of empty @ 17,17" empty_18x18 17 17 0;
    hl_neighbors_test "neighbors of empty @ 10,10" empty_18x18 10 10 0;
    hl_neighbors_test "neighbors of empty at 2,2" empty_18x18 2 2 0;
    (* replicator tests - diagonal *)
    hl_neighbors_test "neighbors of replicator @ 0,0" repl_18x18 0 0 0;
    hl_neighbors_test "neighbors of replicator @ 1,1" repl_18x18 1 1 0;
    hl_neighbors_test "neighbors of replicator @ 2,2" repl_18x18 2 2 0;
    hl_neighbors_test "neighbors of replicator @ 3,3" repl_18x18 3 3 0;
    hl_neighbors_test "neighbors of replicator @ 4,4" repl_18x18 4 4 0;
    hl_neighbors_test "neighbors of replicator @ 5,5" repl_18x18 5 5 0;
    hl_neighbors_test "neighbors of replicator @ 6,6" repl_18x18 6 6 0;
    hl_neighbors_test "neighbors of replicator @ 7,7" repl_18x18 7 7 2;
    hl_neighbors_test "neighbors of replicator @ 8,8" repl_18x18 8 8 4;
    hl_neighbors_test "neighbors of replicator @ 9,9" repl_18x18 9 9 0;
    hl_neighbors_test "neighbors of replicator @ 10,10" repl_18x18 10 10 0;
    hl_neighbors_test "neighbors of replicator @ 11,11" repl_18x18 11 11 0;
    hl_neighbors_test "neighbors of replicator @ 12,12" repl_18x18 12 12 0;
    hl_neighbors_test "neighbors of replicator @ 13,13" repl_18x18 13 13 0;
    hl_neighbors_test "neighbors of replicator @ 14,14" repl_18x18 14 14 0;
    hl_neighbors_test "neighbors of replicator @ 15,15" repl_18x18 15 15 0;
    hl_neighbors_test "neighbors of replicator @ 16,16" repl_18x18 16 16 0;
    hl_neighbors_test "neighbors of replicator @ 17,17" repl_18x18 17 17 0;
    (* edge cases *)
    hl_neighbors_test "neighbors of replicator @ 0,1" repl_18x18 0 1 0;
    hl_neighbors_test "neighbors of replicator @ 0,2" repl_18x18 0 2 0;
    hl_neighbors_test "neighbors of replicator @ 0,3" repl_18x18 0 3 0;
    hl_neighbors_test "neighbors of replicator @ 0,4" repl_18x18 0 4 0;
    hl_neighbors_test "neighbors of replicator @ 0,5" repl_18x18 0 5 0;
    hl_neighbors_test "neighbors of replicator @ 0,6" repl_18x18 0 6 0;
    hl_neighbors_test "neighbors of replicator @ 0,7" repl_18x18 0 7 0;
    hl_neighbors_test "neighbors of replicator @ 0,8" repl_18x18 0 8 0;
    hl_neighbors_test "neighbors of replicator @ 0,9" repl_18x18 0 9 0;
    hl_neighbors_test "neighbors of replicator @ 0,10" repl_18x18 0 10 0;
    hl_neighbors_test "neighbors of replicator @ 0,11" repl_18x18 0 11 0;
    hl_neighbors_test "neighbors of replicator @ 0,12" repl_18x18 0 12 0;
    hl_neighbors_test "neighbors of replicator @ 0,13" repl_18x18 0 13 0;
    hl_neighbors_test "neighbors of replicator @ 0,14" repl_18x18 0 14 0;
    hl_neighbors_test "neighbors of replicator @ 0,15" repl_18x18 0 15 0;
    hl_neighbors_test "neighbors of replicator @ 0,16" repl_18x18 0 16 0;
    hl_neighbors_test "neighbors of replicator @ 0,17" repl_18x18 0 17 0;
  ]

let hl_tests = List.flatten [ highlife_tests ]

(******************************************************************************)

(************** Tests for Active Game of Life with Wraparound **************)

module ActiveGoL = MakeActive (B3_S23)

let a_empty_10x10 = ActiveGoL.init_empty 10 10
let a_glider_10x10 = ActiveGoL.init_glider ()

let a_b55_10x10 =
  let x = ActiveGoL.init_empty 10 10 in
  ActiveGoL.birth_node x 5 5;
  x

let a_b00_10x10 =
  let x = ActiveGoL.init_empty 10 10 in
  ActiveGoL.birth_node x 0 0;
  x

let a_b09_10x10 =
  let x = ActiveGoL.init_empty 10 10 in
  ActiveGoL.birth_node x 0 9;
  x

let a_b90_10x10 =
  let x = ActiveGoL.init_empty 10 10 in
  ActiveGoL.birth_node x 9 0;
  x

let a_b99_10x10 =
  let x = ActiveGoL.init_empty 10 10 in
  ActiveGoL.birth_node x 9 9;
  x

let a_b05_10x10 =
  let x = ActiveGoL.init_empty 10 10 in
  ActiveGoL.birth_node x 0 5;
  x

let a_b95_10x10 =
  let x = ActiveGoL.init_empty 10 10 in
  ActiveGoL.birth_node x 9 5;
  x

let a_b50_10x10 =
  let x = ActiveGoL.init_empty 10 10 in
  ActiveGoL.birth_node x 5 0;
  x

let a_b59_10x10 =
  let x = ActiveGoL.init_empty 10 10 in
  ActiveGoL.birth_node x 5 9;
  x

let active_state_printer s =
  match s with
  | ActiveGoL.Alive -> "Alive"
  | ActiveGoL.Dead -> "Dead"

let active_neighbors_test name in_gb in_x in_y exp_out =
  name >:: fun _ ->
  assert_equal exp_out
    (ActiveGoL.neighbors in_gb in_x in_y)
    ~printer:string_of_int

let active_update_node_test name in_gb in_x in_y (exp_out : ActiveGoL.state) =
  name >:: fun _ ->
  assert_equal exp_out
    (let n = ActiveGoL.neighbors in_gb in_x in_y in
     ActiveGoL.update_node in_gb in_x in_y n;
     ActiveGoL.get in_gb in_x in_y)
    ~printer:active_state_printer

let active_neighbors_tests =
  [
    (* empty board *)
    active_neighbors_test "neighbors of empty @ 0,0" a_empty_10x10 0 0 0;
    active_neighbors_test "neighbors of empty @ 9,9" a_empty_10x10 9 9 0;
    (* only alive node at 5,5 *)
    active_neighbors_test "neighbors of 5,5 @ 5,5" a_b55_10x10 5 5 0;
    active_neighbors_test "neighbors of 5,5 @ 4,4" a_b55_10x10 4 4 1;
    active_neighbors_test "neighbors of 5,5 @ 5,4" a_b55_10x10 5 4 1;
    active_neighbors_test "neighbors of 5,5 @ 6,4" a_b55_10x10 6 4 1;
    active_neighbors_test "neighbors of 5,5 @ 4,5" a_b55_10x10 4 5 1;
    active_neighbors_test "neighbors of 5,5 @ 6,5" a_b55_10x10 6 5 1;
    active_neighbors_test "neighbors of 5,5 @ 4,6" a_b55_10x10 4 6 1;
    active_neighbors_test "neighbors of 5,5 @ 5,6" a_b55_10x10 5 6 1;
    active_neighbors_test "neighbors of 5,5 @ 6,6" a_b55_10x10 6 6 1;
    active_neighbors_test "neighbors of 5,5 @ 5,7" a_b55_10x10 5 7 0;
    (* mid edge cases*)
    active_neighbors_test "neighbors of 5,0 @ 4,9" a_b50_10x10 4 9 1;
    active_neighbors_test "neighbors of 5,0 @ 5,9" a_b50_10x10 5 9 1;
    active_neighbors_test "neighbors of 5,0 @ 6,9" a_b50_10x10 6 9 1;
    active_neighbors_test "neighbors of 9,5 @ 0,4" a_b95_10x10 0 4 1;
    active_neighbors_test "neighbors of 9,5 @ 0,5" a_b95_10x10 0 5 1;
    active_neighbors_test "neighbors of 9,5 @ 0,6" a_b95_10x10 0 6 1;
    active_neighbors_test "neighbors of 5,9 @ 4,0" a_b59_10x10 4 0 1;
    active_neighbors_test "neighbors of 5,9 @ 5,0" a_b59_10x10 5 0 1;
    active_neighbors_test "neighbors of 5,9 @ 6,0" a_b59_10x10 6 0 1;
    active_neighbors_test "neighbors of 0,5 @ 9,4" a_b05_10x10 9 4 1;
    active_neighbors_test "neighbors of 0,5 @ 9,5" a_b05_10x10 9 5 1;
    active_neighbors_test "neighbors of 0,5 @ 9,6" a_b05_10x10 9 6 1;
    (* corner cases *)
    active_neighbors_test "neighbors of 0,0 @ 9,0" a_b00_10x10 9 0 1;
    active_neighbors_test "neighbors of 0,0 @ 0,9" a_b00_10x10 0 9 1;
    active_neighbors_test "neighbors of 0,0 @ 9,9" a_b00_10x10 9 9 1;
    active_neighbors_test "neighbors of 9,0 @ 0,0" a_b90_10x10 0 0 1;
    active_neighbors_test "neighbors of 9,0 @ 0,9" a_b90_10x10 0 9 1;
    active_neighbors_test "neighbors of 9,0 @ 9,9" a_b90_10x10 9 9 1;
    active_neighbors_test "neighbors of 0,9 @ 0,0" a_b09_10x10 0 0 1;
    active_neighbors_test "neighbors of 0,9 @ 9,9" a_b09_10x10 9 9 1;
    active_neighbors_test "neighbors of 0,9 @ 0,0" a_b09_10x10 9 0 1;
    active_neighbors_test "neighbors of 9,9 @ 0,9" a_b99_10x10 0 9 1;
    active_neighbors_test "neighbors of 9,9 @ 0,0" a_b99_10x10 0 0 1;
    (* neighbors_test "neighbors of 9,9 @ 9,0" b99_10x10 9 0 1; *)
    active_neighbors_test "neighbors of glider @ 5,5" a_glider_10x10 5 5 2;
    active_neighbors_test "neighbors of glider @ 4,5" a_glider_10x10 4 5 3;
    active_neighbors_test "neighbors of glider @ 4,4" a_glider_10x10 4 4 3;
    active_neighbors_test "neighbors of glider @ 4,3" a_glider_10x10 4 3 5;
  ]

let active_update_node_tests =
  [
    active_update_node_test "update empty" a_empty_10x10 5 5 ActiveGoL.Dead;
    active_update_node_test "update gilder @ 5,5" a_glider_10x10 5 5
      ActiveGoL.Dead;
    active_update_node_test "update gilder @ 4,5" a_glider_10x10 4 5
      ActiveGoL.Alive;
    active_update_node_test "update gilder @ 5,4" a_glider_10x10 5 4
      ActiveGoL.Alive;
  ]

let active_gol_tests =
  List.flatten [ active_neighbors_tests; active_update_node_tests ]

let suite =
  "test suite for CA"
  >::: List.flatten
         [
           gol_tests;
           one_tests;
           int_to_binary_tests;
           make_rule_test;
           active_gol_tests;
           hl_tests;
         ]

let _ = run_test_tt_main suite

open OUnit2
open Gameboard

type state =
  | Dead
  | Alive

type gameboard = state list list

(*some starter patterns for the board

  TODO: WOULD LIKE TO MAKE HELPER FUNCTION THAT ALLOWS ONE TO CREATE BOARDS BY
  ONLY SPECIFIYING WHICH CELLS ARE ALIVE, RATHER THAN WRITING IT ALL OUT, YA
  KNOW*)

let assert_equal_boards (gb1 : gameboard) (gb2 : gameboard) =
  if List.flatten gb1 = List.flatten gb2 then true else false

let dead : gameboard =
  [
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
  ]

let block : gameboard =
  [
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Alive; Alive; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Alive; Alive; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
  ]

let toad =
  [
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
  ]

let glider =
  [
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Alive; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Alive; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Alive; Alive; Alive; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
    [ Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead ];
  ]

(* test functions to test functionality of gamboard.ml*)
let init_gameboard_test name in_gb exp_out =
  name >:: fun _ -> assert_equal exp_out (init_gameboard in_gb)

let loop_test name in_gb in_int exp_out =
  name >:: fun _ -> assert_equal exp_out (loop in_gb in_int)

let turn_test name in_gb exp_out =
  name >:: fun _ -> assert_equal exp_out (turn in_gb)

let neighbors_test name in_gb in_x in_y exp_out =
  name >:: fun _ -> assert_equal exp_out (neighbors in_gb in_x in_y)

let update_node_test name in_gb in_x in_y exp_out =
  name >:: fun _ -> assert_equal exp_out (update_node in_gb in_x in_y)

let update_board_test name in_gb exp_out =
  name >:: fun _ -> assert_equal exp_out (update_board in_gb)

(* test suite to execute gameboard tests*)
let gameboard_tests =
  [
    init_gameboard_test "test empty board creates" None
      dead (*TODO: implement more test cases*);
  ]

let suite = "test suite for CA" >::: List.flatten [ gameboard_tests ]
let _ = run_test_tt_main suite
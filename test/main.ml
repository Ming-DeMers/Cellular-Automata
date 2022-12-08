open OUnit2
open Cellular_automata.Two_matrix

(* Test Boards *)
module GoL = Make (B3_S23)

(************** Tests for Standard Game of Life with Wraparound **************)
let empty_10x10 = GoL.init_empty 10 10
let b1_10x10 = GoL.init_empty 10 10

let assert_equal_boards gb1 gb2 =
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
  assert_equal (List.equal (fun a b -> a = b) gb1_lst gb2_lst) true

let neighbors_test name in_gb in_x in_y exp_out =
  name >:: fun _ -> assert_equal exp_out (GoL.neighbors in_gb in_x in_y)

let update_node_test name in_gb in_x in_y exp_out =
  name >:: fun _ -> assert_equal exp_out (GoL.update_node in_gb in_x in_y)

let update_board_test name in_gb exp_out =
  name >:: fun _ -> assert_equal exp_out (GoL.update_board in_gb)

let loop_test name in_gb in_int exp_out =
  name >:: fun _ -> assert_equal exp_out (GoL.loop in_gb in_int)

let neighbors_tests =
  [ neighbors_test "neighbors of empty @ 0,0" empty_10x10 0 0 0 ]

let gol_tests = List.flatten [ neighbors_tests ]

(******************************************************************************)

let suite = "test suite for CA" >::: List.flatten [ gol_tests ]
let _ = run_test_tt_main suite
open Cellular_automata.One

module type Rule = sig
  val rule : int
end

module Make1DGame (R : Rule) = struct
  let gb_to_grid g = Array.make 1 g

  let rec loop gb x =
    let g = gb.(0) in
    let new_gb = update_board g R.rule in
    if x = 0 then print_board g
    else (
      print_board g;
      loop (gb_to_grid new_gb) (x - 1))
end
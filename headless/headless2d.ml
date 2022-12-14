open Cellular_automata.Two

module Make2DGame (B : Board) = struct
  open B

  let loop g n =
    for num = n downto 1 do
      ignore num;
      update_board g;
      print_board g
    done
end

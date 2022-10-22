# INSTRUCTIONS TO RUN `CELLULAR-AUTOMATA`

>## System Requirements
- A system running Linux Ubuntu either natively or through OPAM switches.
- OCAML compiler, standard library and UTOP

>## Installation and Running
- In the command line, move to the directory ./ms2_code/Cellular-Automata/src
- Open utop
- Input the command $ #use "gameboard.ml";;
Now to try out some various inital states
Glider:
- Input the command $ let x = init_gameboard (Some glider);;
- Input the command $ loop x 10;;
- Observe the printed gameboards
Toad:
- Input the command $ let y = init_gameboard (Some toad);;
- Input the command $ loop y 10;;
- Observe the printed gameboards
Block:
- Input the command $ let z = init_gameboard (Some block);;
- Input the command $ loop z 5;;
- Observe the printed gameboards


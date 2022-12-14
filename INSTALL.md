# INSTRUCTIONS TO RUN `CELLULAR-AUTOMATA`

>## System Requirements
- A system running Linux Ubuntu either natively or through OPAM switches.
- OCAML compiler, standard library and UTOP
- To run the GUI, must have ocaml graphics module and have XQuartz downloaded

>## Installation and Running
- In the command line, move to the directory ./Cellular-Automata
- Input the command $ dune build
- move to the directory ./Cellular-Automata/src using the command $ cd src
- Open utop

## Elementary 1D Cellular Automata
- A new game is created by defining the width of the board. The midddle most node will be *Alive* and the rest will be *Dead*. We do this by running `init_empty n` where n is the length of the board. An odd integer is recommended.
- We can print any gameboard with `print_board gb` where `gb` is the gameboard. 
- Next we can move to the next generation by using `update_board gb rule` where `gb` is the initialized gameboard, and `rule` is an integer between 1-256. `Rule` is the integer representation of a byte, which encodes the rules of each generation.
- We can print that result, too. 
- Finally, we can see many generations at once, by running `print_loop gb rule n` where `gb` is the gameboard, `rule` is the integer between 1-256, and `n` is n many generations desired.
- Example, one could do these steps to see *rule 90* print *50 times*.
1. `# let game = init_empty 49`
2. `# print_loop game 90 50`

Other interesting rules:
- 13
- 18
- 30
- 45
- 57
- 73
- 105

*and many more*

## 2D Cellualar Automata
- Input the command $ #use "two.ml";;

**Conway's Game of life:**
- Input the command $ module G = MakeBoard (B3_S23);;
- Input the command $ let g = G.init_glider ();;
- Input the command $ G.loop g 30;; (Or however many iterations you wish to see)
- Observe the printed gameboards

**Hilifelife:**
- Input the command $ module H = MakeBoard (B36_S23);;
- Input the command $ let h = H.init_replicator ();;
- Input the command $ H.loop h 20;; (Or however many iterations you wish to see)
- Observe the printed gameboards

**Day and Night:**
- Input the command $ module D = MakeBoard (B3678_S34678);;
- Input the command $ let d = D.init_rocket ();;
- Input the command $ D.loop d 30;; (Or however many iterations you wish to see)
- Observe the printed gameboards

**Seeds:**
- Input the command $ module S = MakeBoard (B2_S);;
- Input the command $ let s = S.init_seed ();;
- Input the command $ S.loop s 20;; (Or however many iterations you wish to see)
- Observe the printed gameboards

Now let's take a look at a new implimentation of 2D Cellular Automata that is optomized to run more efficiently:








Finally, lets use a GUI to observe the board updates in real time.
- First, open xterm terminal through XQuartz
- move to the directory ./Cellular-Automata
- Enter the command $ make gui
- Observe
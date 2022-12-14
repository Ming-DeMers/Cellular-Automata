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

<<<<<<< HEAD
## Elementary 1D Cellular Automata
- A new game is created by defining the width of the board. The midddle most node will be *Alive* and the rest will be *Dead*. We do this by running `init_empty n` where n is the length of the board. An odd integer is recommended.
- We can print any gameboard with `print_board gb` where `gb` is the gameboard. 
- Next we can move to the next generation by using `update_board gb rule` where `gb` is the initialized gameboard, and `rule` is an integer between 1-256. `Rule` is the integer representation of a byte, which encodes the rules of each generation.
- We can print that result, too. 
- Finally, we can see many generations at once, by running `print_loop gb rule n` where `gb` is the gameboard, `rule` is the integer between 1-256, and `n` is n many generations desired.
- Example, one could do these steps to see *rule 90* print *50 times*.
1. `# let game = init_empty 49`
2. `# print_loop game 90 50`

- A rule is a byte that determines what nodes live and what die in the next generation. For any node, there are two neighboring nodes, and including itself, we have a 3-node neighborhood. There are 8 possible neighborhoods, and depending on which it is, 0-7, we match that to the index of the rule byte, and see if it is 1 or 0, determining if it lives or dies. For example, in a 3-node gameboard, where the middle is alive and others are dead and the rule 90: 
- The rule can be represented as [01011010] the first node, which has the nieghborhood [001], which is the bit of 2, thus lives. For the second, [010]. which dies, and the third, [100], which lives.

### Other interesting rules
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
=======
We'll start by observing some 2D Cellualar Automata, both the original 
implementation as well as a more efficient implementation

Conway's Game of life:
- Original: Input the command $ G.loop g 30;; 
(Or however many iterations you wish to see)
- Efficient: Input the command $ GA.loop ga 30;;
- Observe the printed gameboards

Hilifelife:
- Original: Input the command $ H.loop h 20;;
- Efficient: Input the command $ HA.loop ha 20;;
- Observe the printed gameboards

Day and Night:
- Original: Input the command $ D.loop d 30;;
- Observe the printed gameboards

Seeds:
- Original: Input the command $ S.loop s 20;;
- Efficient: Input the command $ SA.loop sa 20;;
>>>>>>> f8a4918fbe3015b309d3a6cd61e65de9adf6839e
- Observe the printed gameboards





Finally, lets use a GUI to observe the board updates in real time.
- First, open xterm terminal through XQuartz
- move to the directory ./Cellular-Automata
- Enter the command $ make gui
- Observe
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

We'll start by observing some 2D Cellualar Automata
- Input the command $ #use "two.ml";;

Conway's Game of life:
- Input the command $ module G = MakeBoard (B3_S23);;
- Input the command $ let g = G.init_glider ();;
- Input the command $ G.loop g 30;; (Or however many iterations you wish to see)
- Observe the printed gameboards

Hilifelife:
- Input the command $ module H = MakeBoard (B36_S23);;
- Input the command $ let h = H.init_replicator ();;
- Input the command $ H.loop h 20;; (Or however many iterations you wish to see)
- Observe the printed gameboards

Day and Night:
- Input the command $ module D = MakeBoard (B3678_S34678);;
- Input the command $ let d = D.init_rocket ();;
- Input the command $ D.loop d 30;; (Or however many iterations you wish to see)
- Observe the printed gameboards

Seeds:
- Input the command $ module S = MakeBoard (B2_S);;
- Input the command $ let s = S.init_seed ();;
- Input the command $ S.loop s 20;; (Or however many iterations you wish to see)
- Observe the printed gameboards

Now let's take a look at a new implimentation of 2D Cellular Automata that is optomized to run more efficiently:





Now let's take a look at some 1D Cellular Automata




Finally, lets use a GUI to observe the board updates in real time.
- First, open xterm terminal through XQuartz
- move to the directory ./Cellular-Automata
- Enter the command $ make gui
- Observe
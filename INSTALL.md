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
- Observe the printed gameboards


Now let's take a look at some 1D Cellular Automata




Finally, lets use a GUI to observe the board updates in real time.
- First, open xterm terminal through XQuartz
- move to the directory ./Cellular-Automata
- Enter the command $ make gui
- Observe
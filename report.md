# OChamleon MS2 Report:
>## Vision:
Our current vision is to have an interactive system with cellular automata. At the very least, we hope to have users be able to create Cellular Automata patterns (CA), whether in the first or second dimension, on the CLI and observe their behavior based on a set of predetermined rules. Users will be able create their own starter patterns, choose premade patterns, or modify existing patterns. Users will also be able to determine the rules that dictate automaton behavior. We hope to extend functionality and complexity with a third dimension, adding additional roles for nodes, and adding continuous functions and models. For example, we hope to add more states beyond alive and dead (i.e. killer cells, protector cells), allow values to represent color (i.e. 255 = RGB), and possibly neural networks and machine learning for regenerative and mutating CA. A GUI may be implemented, but its necessity isn't seen yet.

>## Summary of progress:
Between the two milestones, idea became code. We agreed on a reachable goal that would provide a strong foundation for our project: implementing 2D CA, based off Conways' Game of Life. We first outline the functions in the .mli interface, demonstrating what functions should be exposed to the user. This gives the user power to interact and work with our program without needing to know the messy details (i.e. abstraction).

`gameboard.ml` contains our implementation of the 2D CA. It contains a number of helper functions to successfully implement our game in OCaml. We allow the user to select a pattern, either the default being a 10x10 dead grid or a one of a selection of premade patterns. From there, we allow the user to run a generation at a time, or run many in a loop. 

Overall progress is substantial and a good step to our goals. Implementing the grid with pattern matching was at first challenging, but became more clear as we continued writing functions. We currently have a number of duplicitous functions intended to "check neighbors," but are working on writing a tail recursive function that can do the same in fewer and non-duplicitous lines of code. Our next steps are to ensure code coverage, better implement user interaction, and to work on the 1D and 3D CA versions.

>## Activity breakdown: 
For each team member, give a bulleted list of the responsibilities that team members had, the activities in which they participated, the features they delivered, and the number of hours they spent working.

### Ming
- Assigned to setup the prepo and collaborative environment, help implement functions, and ensure the program is within application requirements.
- Set up Cornell github, as well as personal github so those not at Cornell may see the project.
- Wrote makefiles, dune files, etc.
- Helped implement functions and ensure functionality.
- Rewrote neighbors to be tr recursive - untested so far.
- Created and wrote the yaml, report, instructions.
- 8 Hours.

### Jack
- Assigned to implement in the rogram, testing, and ensure we are meeting application req's.
- Defined specifications for all functions in gameboard.mli
- Implemented update_board and its helper functions
- Implemented update_node
- Reviewed code for errors
-8 hours

### Ben
- Assigned to implement the program, communicate with the project mentor.
- Implemented most functions defined in gameboard.mli using many helper functions
- Fixed bugs to ensure all code ran as intended
- Coordinated meetings with PM and PG
- General code cleanup and organization
- Created install instructions
- Submit project
- 12 hours

### Jason
- Assigned to test functionality, ensure program correctness, and clean and organize function declarations, order, and specifications to ensure readability.
- Created gameboard.ml and gameboard.mli and helped write specifications for the essential functions in gameboard.mli
- Performed final checks on gameboard.ml and .mli functionality
- Created writeup file and made structure for content
- 5 hours

>## Productivity analysis: 

As an entire team, we were somewhat productive. We were able to accomplish what we set out to do for this sprint, although our goals were not too lofty. Our estimates of what we could do were pretty spot on, as we worked right up to the deadline in order to accomplish our goals. Now that we have established a base, it should be much easier to expand upon our ideas for future sprints. For this reason, we should be able to set our sights higher for these future sprints. Our communication stands to gain some improvement, but there is no doubt that we are all committed to this project and making it the best we possibly can.

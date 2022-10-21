# OCHamleon MS2 Report:
>## Vision:
Our current vision is to have an interactive system with cellular automata. At the very least, we hope to have users be able to create Cellular Automata patterns (CA), whether in the first or second dimension, on the CLI. Users can implement their own starter patterns or rules, but we hope to extend functionality and complexity with a third dimension, adding more roles for nodes, and adding continous functions and models. A GUI may be implemented, but its necessity isn't seen yet.

>## Summary of progress:
Between the two milestones, idea became code. We agreed on a reachable goal that would provide a strong foundation for our project: implementing 2D CA, based off Conways' Game of Life. We first outline the functions in the .mli interface, demonstrating what functions should be exposed to the user. This gives the user power to interact and work with our program without needing to know the messy detials (i.e. abstraction).

`gameboard.ml` contains our implementation of the 2D CA. It contains a number of helper functions to sucessfully implement our game in OCaml. We allow the user to create a pattern, the default being a 10x10 dead grid. From there, we allow the user to run a generation at a time, or run many in a loop. The board is implemented with wrap-around, too.

Overall progress is substantial and a good step to our goals. Implementing the grid with pattern matching was at first challenging, but became more clear as we continued writing functions. We intially had a number number of duplicitous functions intended to "check neighbors," but are working on writing a tail recursive function that can do the same in fewer and non-duplicitous lines of code. Our next steps are to ensure code coverage, better implement user interaction, and to work on the 1D and 3D CA versions.

>## Activity breakdown: 
For each team member, give a bulleted list of the responsibilities that team member had, the activities in which they participated, the features they delivered, and the number of hours they spent working.

### Ming
- Assigned to setup the repo and collaborative enviroment, help implement functions, and ensure program is within application requirements.
- Set up Cornell github, as well as personal github so those not at Cornell may see the project.
- Wrote makefiles, dune files, etc.
- Helped implement functions and ensure functionality.
- Rewrote neighbors to be tr recursive - untested so far.
- Created and wrote the yaml, report, instructions.
- 8 Hours.


### Jack
- Assigned to implement in the program, testing, and ensure we are meeting application req's.
- *What you did*

### Ben
- Assigned to implement the program, communicate with the project mentor.
- *What you did*

### Jason
- Assigned to test functionality and ensure program correctness.
- *What you did*

>## Productivity analysis: 

As an entire team, how productive were you? Did you accomplish what you planned in your sprints? Were your estimates of what you could do accurate, or far off? Write a paragraph addressing those questions. Please be honest: we want you to reflect candidly on your progress, so that you can make more accurate estimates between MS2 and MS3. Your grade is not going to be based on how positive or negative you are here.

*I will write this once y'all add your stuff*
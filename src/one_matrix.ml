type state =
  | Dead
  | Alive

type gameboard = state array array

exception AlreadyAlive
exception AlreadyDead 

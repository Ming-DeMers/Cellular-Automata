type state

module type Game = sig
  val loop : state array array -> int -> unit
end
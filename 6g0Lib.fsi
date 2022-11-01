module Solitaire 

type pos = int * int 
type value = Red | Green | Blue | Yellow | Black
type piece = value * pos 
type state = piece list 

val fromValue: v:value -> Canvas.color
val nextColor : c : value -> value
val filter : k : int -> s : state -> state
val empty : s : state -> pos list
val shiftLeft : s : state -> state
val flipLR : s : state -> state
val transpose : s : state -> state
val addRandom : c : value -> s : state -> state option

val draw: w: int -> h: int -> s: state -> unit
val react: s: state -> k: key -> state option





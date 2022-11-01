#r "nuget:DIKU.Canvas, 1.0"
#load "6g0Lib.fs"

open Canvas
open Solitaire

let height:int = 3
let width:int = 3

do runApp "2048" 600 600 draw react [(Red,(0,0));(Blue,(1,0))]

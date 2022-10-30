#r "nuget:DIKU.Canvas, 1.0"
#load "6g0Lib.fs"

open Canvas
open Solitaire
open System

let height:int = 3
let width:int = 3

let a:state = [(Red,(0,0));(Red,(1,0));(Red,(2,0));(Blue,(0,1));(Red,(1,1));(Red,(2,1));(Red,(0,2));(Red,(1,2));(Green,(2,2))]
printfn "%A" (addRandom Blue (shiftLeft a))

do runApp "2048" 600 600 draw react [(Red,(0,0));(Blue,(1,0))]
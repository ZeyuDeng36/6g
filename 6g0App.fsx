#r "nuget:DIKU.Canvas, 1.0"
#load "6g0Lib.fs"

open Canvas
open Solitaire
open System

let height:int = 3
let width:int = 3

let statet: state = [(Red,(0,0));(Red,(1,0));(Red,(2,0));(Red,(1,1));(Blue,(2,1));(Green,(2,1));(Red,(0,2));(Red,(1,2));(Red, (2,2))]
let test: state = [(Green,(0,0));(Green,(1,0));(Green,(2,0));(Green,(3,0))]
printfn "= %A" (xshiftLEFT statet)


// do runApp "2048" 600 600 draw react [(Red,(0,0));(Blue,(1,0))]

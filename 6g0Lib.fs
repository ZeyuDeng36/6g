module Solitaire

type pos = int * int 
type value = Red | Green | Blue | Yellow | Black
type piece = value * pos 
type state = piece list 
let length:int = 3

let fromValue (v:value):Canvas.color=
    match v with
        |Red -> Canvas.red
        |Green -> Canvas.green
        |Blue -> Canvas.blue
        |Yellow -> Canvas.yellow
        |Black -> Canvas.black

let nextColor (c:value):value =
    match c with
        |Red -> Green
        |Green -> Blue
        |Blue -> Yellow
        |Yellow -> Black
        |Black -> Black

let filter (k:int) (s:state):state=
    s |> List.filter (fun (_, (_, y)) -> if y = k then true else false)

let shiftLeft (s:state):state=
    let shift (k:int) (s:state): state=
        let mutable rFin:state= []
        let rec merge (r:state)=
            if r.IsEmpty <> true then
                let (headColor, (x,y)) = r.Head
                let (tailColor, _) = r.Tail.Head
                if r.Tail.IsEmpty <> true && headColor = fst(r.Tail.Head) then
                    rFin <- rFin @ [nextColor(headColor),(x,y)]
                    merge (r.Tail.Tail)
                else
                    rFin <- rFin @ [r.Head]
                    merge (r.Tail)
        merge (filter k ( List.sortBy (fun (_, (x, _)) -> x) s))
        List.mapi (fun index (v,(x,y)) -> (v,(index, y))) rFin
    (shift 0 s) @ (shift 1 s) @ (shift 2 s)
    
let flipUD (s:state):state=
    s |> List.map (fun (value ,(x,y)) -> (value ,(2 - x, y)))
    
let transpose (s:state):state=
    s |> List.map (fun (value, (x,y)) -> (value, (y,x)))

let empty (s: state) : pos list =
    let height:int = 3
    let width:int = 3
    let spos = (s |> List.map (fun (_,(x,y)) -> (x,y)))
    let lst = List.init (height * width) (fun i -> (i % width, i/height))
    lst |> List.except spos



    
module Solitaire
open Canvas
type pos = int * int 
type value = Red | Green | Blue | Yellow | Black
type piece = value * pos 
type state = piece list 
let height:int = 3
let width:int = 3

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
    List.filter (fun (_,(_,y)) -> if y=k then true else false) s

let shiftLeft (s:state):state=
    let shift (k:int) (s:state): state=
        let mutable rFin:state= []
        let rec merge (r:state)=
            if r.IsEmpty <> true then
                if r.Tail.IsEmpty <> true && fst(r.Head) = fst(r.Tail.Head) then
                    rFin <- rFin @ [nextColor(fst(r.Head)),(fst(snd(r.Head)),snd(snd(r.Head)))]
                    merge (r.Tail.Tail)
                else
                    rFin <- rFin :: [r.Head]
                    merge (r.Tail)
        merge (filter k ( List.sortBy (fun (_,(x,_)) -> x) s))
        List.mapi (fun index (v,(x,y)) -> (v,(index, y))) rFin
    (shift 0 s) :: (shift 1 s) :: (shift 2 s)
    
let flipLR (s:state):state=
    List.map (fun (v,(x,y)) -> (v,((width-1-x),y))) s
    
let transpose (s:state):state=
    List.map (fun (v,(x,y)) -> (v,(y,x))) s

let empty (s: state) : pos list =
    let spos = List.map (fun (_,(x,y)) -> (x,y)) s
    let mutable lst = List.init (height*width) (fun i -> (i % width, i/height))
    let rec filter (lst: pos list) : pos list = 
        match lst with
            | head::tail when List.contains head spos -> filter tail
            | head::tail -> head :: filter(tail)
            | [] -> []
    filter lst 

let addRandom (c:value) (s:state) :state option =
    let emptyPositions:pos list = empty s
    if emptyPositions.IsEmpty = true then
        None
    else 
        let rnd = System.Random()
        let i = rnd.Next(0,(emptyPositions.Length))
        Some (s @ [(c,(List.item i emptyPositions))])


let draw (w: int) (h: int) (s: state): canvas =
    let C:canvas = Canvas.create w h
    List.iter (fun (v,(x,y)) -> Canvas.setFillBox  C (fromValue v) ((w/width*x),(h/height*y)) ((w/width*(x+1)),(h/height*(y+1)))) s
    C

let react (s:state)(k:key):state option =
    let mutable sNew:state = []
    match getKey k with
        | LeftArrow -> sNew <- (shiftLeft s)
        | RightArrow -> sNew <-(flipLR (shiftLeft (flipLR s)))
        | UpArrow -> sNew <-(transpose (shiftLeft (transpose s)))
        | DownArrow -> sNew <-(transpose (flipLR(shiftLeft (flipLR(transpose s)))))
        | _ -> ()
    if (empty sNew) <> [] then   
        sNew <- Option.get(addRandom Red sNew)
        printfn "%A" sNew
    Some(sNew)
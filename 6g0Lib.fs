module Solitaire
open Canvas
type pos = int * int 
type value = Red | Green | Blue | Yellow | Black
type piece = value * pos 
type state = piece list 
let height: int = 3
let width: int = 3

let fromValue (v: value) : Canvas.color =
    match v with
        |Red -> Canvas.red
        |Green -> Canvas.green
        |Blue -> Canvas.blue
        |Yellow -> Canvas.yellow
        |Black -> Canvas.black

let nextColor (c: value) : value =
    match c with
        |Red -> Green
        |Green -> Blue
        |Blue -> Yellow
        |Yellow -> Black
        |Black -> Black

let filter (k: int) (s: state) : state=
    s |> List.filter (fun (_, (_, y)) -> y = k )

let empty (s: state) : pos list =
    let sPos = (s |> List.map (fun (_,pos) -> pos))
    let lst = List.init (height * width) (fun i -> (i % width, i/height))
    lst |> List.except sPos


let rec shiftLEFT (s: state) : state =
    let mergedRow: state = List.empty
    let rec merge (s: state) : state = 
        //printfn "merge - %A" (s)
        // try defining elm and rst using try function
        match s with 
            //compare Value and y-axis of head and next element in list
            | elm::rst when (List.isEmpty rst = false && fst(elm) = fst(rst.Head) && snd(snd(elm)) = snd(snd(rst.Head))) ->
                (nextColor(fst(elm)), snd(elm)) :: (merge rst.Tail)
            | elm::rst ->
                elm :: (merge rst)
            | [] ->
                mergedRow

    let mutable shiftedRow: state = List.empty
    let rec shift (s: state) : state = 
        match s with
            | elm::rst ->
                let shiftPos = ((empty shiftedRow) |> (List.filter (fun (_,y) -> y = snd(snd(elm))))).Head
                shiftedRow <- shiftedRow @ [(fst(elm), shiftPos)]
                (shift rst)
            | [] ->
                shiftedRow
                 



    shift (merge s)








let flipLR (s: state) : state =
    s |> List.map (fun (v, (x,y)) -> (v, ((width - 1 - x),y)))
    
let transpose (s: state) : state =
    s |> List.map (fun (v, (x,y)) -> (v, (y,x)))

let addRandom (c:value) (s:state) :state option =
    let emptyPositions:pos list = empty s
    if emptyPositions.IsEmpty = true then
        None
    else 
        let rnd = System.Random()
        let i = rnd.Next(0,(emptyPositions.Length))
        Some (s @ [(c,(List.item i emptyPositions))])


let draw (w: int) (h: int) (s: state): canvas =
    let C: canvas = Canvas.create w h
    List.iter (fun (v,(x,y)) -> Canvas.setFillBox  C (fromValue v) ((w/width*x),(h/height*y)) ((w/width*(x+1)),(h/height*(y+1)))) s
    C

let react (s: state)(k: key) : state option =
    let mutable sNew: state = []
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
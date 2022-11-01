module Solitaire
open Canvas
type pos = int * int 
type value = Red | Green | Blue | Yellow | Black
type piece = value * pos 
type state = piece list 
let height: int = 3
let width: int = 3

//hullu bullu

/// <summary>gets the canvas colour from a value</summary>
/// <param name = "v"> value </param>
/// <returns> coresponding value in canvas color </returns>
let fromValue (v: value) : Canvas.color =
    match v with
        |Red -> Canvas.red
        |Green -> Canvas.green
        |Blue -> Canvas.blue
        |Yellow -> Canvas.yellow
        |Black -> Canvas.black

/// <summary>gets the next value </summary>
/// <param name = "c"> value </param>
/// <returns> the next value </returns>
let nextColor (c: value) : value =
    match c with
        |Red -> Green
        |Green -> Blue
        |Blue -> Yellow
        |Yellow -> Black
        |Black -> Black

/// <summary>fliters out all pieces that are not on the designated y-axis</summary>
/// <param name = "k">a integer that defines the y-axis</param>
/// <param name = "s"> a list og pieces </param>
/// <returns> the pices that have a y value of k </returns>
let filter (k: int) (s: state) : state=
    s |> List.filter (fun (_, (_, y)) -> y = k )

/// <summary>takes a list of pices and returns all unoccupied positions</summary>
/// <param name = "s">a list of pices</param>
/// <returns>a list of positions that are unoccupied </returns>
let empty (s: state) : pos list =
    let sPos = (s |> List.map (fun (_,pos) -> pos))
    let lst = List.init (height * width) (fun i -> (i % width, i/height))
    lst |> List.except sPos


/// <summary>takes a list of pices and merges them and shift them to the right</summary>
/// <param name = "s"> a list og pieces </param>
/// <returns> a list if the merged and shifted pices</returns>
let rec shiftLeft (s: state) : state =
    let mergedRow: state = List.empty
    let rec merge (s: state) : state = 
        match (s) with 
            //merge if 
            //elm is not last elment in list s and elm and next element have the same value and y-axis
            | elm::rst when (List.isEmpty rst = false && fst(elm) = fst(rst.Head) && snd(snd(elm)) = snd(snd(rst.Head))) ->
                (nextColor(fst(elm)), snd(elm)) :: (merge rst.Tail)
            | elm::rst ->
                elm :: (merge rst)
            | [] ->
                mergedRow

    let mutable shiftedRow: state = List.empty
    let rec shift (s: state) : state = 
        match s with
            //for each element in sorted list s change its position to the first empty position with the same y-axis
            | elm::rst ->
                let (value, (x,y)) = elm
                let emptyLst = (empty shiftedRow)
                let shiftPos = (emptyLst |> (List.filter (fun (_,empy) -> empy = y))).Head
                shiftedRow <- (shiftedRow @ [(value, shiftPos)])
                (shift rst)
            | [] ->
                shiftedRow

    //sort by y then x axis
    let sSorted: state = s |> List.sortBy (fun (_, (x, y)) -> y, x)
    shift (merge sSorted)


/// <summary>the a list of pices and flips to the opposid x-axis</summary>
/// <param name = "s"> a list og pieces </param>
/// <returns> a list of pices flipped on the x axis</returns>
let flipLR (s: state) : state =
    s |> List.map (fun (v, (x,y)) -> (v, ((width - 1 - x),y)))

/// <summary>the a list of pices and Transposes them</summary>
/// <param name = "s"> a list og pieces </param>
/// <returns> a Transpose list of pices</returns>
let transpose (s: state) : state =
    s |> List.map (fun (v, (x,y)) -> (v, (y,x)))

/// <summary>adds a random </summary>
/// <param name = "s"> a list og pieces </param>
/// <returns> a Transpose list of pices</returns>
let addRandom (c:value) (s:state) :state option =
    let emptyPositions:pos list = empty s
    if emptyPositions.IsEmpty = true then
        None
    else 
        let rnd = System.Random()
        let i = rnd.Next(0,(emptyPositions.Length))
        printfn "%A" (List.item i emptyPositions)
        Some (s @ [(c,(List.item i emptyPositions))])


let draw (w: int) (h: int) (s: state): canvas =
    let C: canvas = Canvas.create w h
    s |> List.iter (fun (v,(x,y)) -> Canvas.setFillBox  C (fromValue v) ((w/width*x),(h/height*y)) ((w/width*(x+1)),(h/height*(y+1))))
    C

let react (s: state)(k: key) : state option =
    let mutable sNew: state = []
    match getKey k with
        | LeftArrow -> sNew <- (shiftLeft s)
        | RightArrow -> sNew <-(flipLR (shiftLeft (flipLR s)))
        | UpArrow -> sNew <-(transpose (shiftLeft (transpose s)))
        | DownArrow -> sNew <-(transpose (flipLR(shiftLeft (flipLR(transpose s)))))
        | _ -> ()
    match sNew with
        //add new brick if there are empty spaces sNew is not the same as s.
        | elm::rst when (empty sNew).IsEmpty = false && (List.sort s = List.sort sNew) = false ->
            sNew <- Option.get(addRandom Red sNew)
            Some(sNew)
        | _ -> 
            Some(s)

        
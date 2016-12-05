open System
open System.IO

type Move = Up='U' | Down='D' | Left='L' | Right='R'
type Action = { move:Move; shouldPress:bool }
type Position = {index:int*int; shouldPress:bool }

let charToMove c = LanguagePrimitives.EnumOfValue<char, Move>(c)

let executeMove (min:int*int) (max:int*int) (position:Position) (action:Action) =
    let xmin,ymin = min
    let xmax,ymax = max
    let x,y = position.index
    let x,y = match action.move with
              | Move.Up    -> if y < ymax then x,y+1 else x,y
              | Move.Down  -> if y > ymin then x,y-1 else x,y
              | Move.Right -> if x < xmax then x+1,y else x,y
              | Move.Left  -> if x > xmin then x-1,y else x,y
              | _ -> failwithf "Invalid move %A" action.move
    { index=x,y; shouldPress=action.shouldPress }

let rec executeMoves (min:int*int) (max:int*int) start actionList =
    match actionList with
    | [] -> []
    | head::tail ->
        let nextPos = executeMove min max start head 
        nextPos :: executeMoves min max nextPos tail 
let lineToActions (line:string) =
    let actions = [for i in 1 .. (line.Length-1) -> false] @ [true]
    let moves = [for c in line -> charToMove c]
    [for m,a in (Seq.zip moves actions) -> {move=m; shouldPress=a}]

let readInput fileName =
    seq { for l in File.ReadLines fileName -> l }
    |> (Seq.map lineToActions)
    |>  (Seq.reduce (@))

let run fileName =
    let array =  [|[|'7';'8';'9'|];
                   [|'4';'5';'6'|];
                   [|'1';'2';'3'|]|]
    let keypad = Array2D.init 3 3 (fun i j -> array.[i].[j])

    let positions = executeMoves (0,0) (2,2) { index=1,1; shouldPress=false } (readInput fileName)
    [for p in positions do if p.shouldPress then yield p.index]
    |> List.map (fun (x,y) -> keypad.[y,x])
    |> List.toArray
    |> System.String
    |> printfn "%s"

// run "test.txt"
run "input.txt"
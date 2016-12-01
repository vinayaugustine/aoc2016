open Microsoft.FSharp.Core
open System
open System.IO
open System.Net

type Direction = North=0 | East=1 | South=2 | West=3
type Turn = Left='L' | Right='R'
type Move = { Turn:Turn; NumberOfBlocks:uint32 }
type Position = { direction:Direction; x:int; y:int }

let computeDirection (direction:Direction) (turn:Turn) =
    let directions : Direction array = unbox (Enum.GetValues(typeof<Direction>))
    let i = match turn with
            | Turn.Left  -> -1
            | Turn.Right ->  1
            | _ -> failwith "invalid Turn value"

    let newIndex = match (int direction) + i with
                   | j when j < 0 -> directions.Length - 1
                   | j when j >= directions.Length -> 0
                   | j -> j
    directions.[newIndex]

let calculatePosition (position:Position) (move:Move) =
    let count = int move.NumberOfBlocks
    let direction = computeDirection position.direction move.Turn
    let x, y = position.x, position.y
    let x, y = match direction with
               | Direction.North -> x, y+count
               | Direction.East  -> x+count, y
               | Direction.South -> x, y-count
               | Direction.West  -> x-count, y
               | _  -> failwith "Invalid direction"
    { direction=direction; x=x; y=y }

let computeDistance start finish =
    Math.Abs ((finish.x - start.x) + (finish.y - start.y))

let executeMoves moves =
    let start = { direction=Direction.North; x=0; y=0 }
    let finish = List.fold calculatePosition start moves
    printfn "Easter bunny HQ is %i blocks away" (computeDistance start finish)

let charToTurn c = LanguagePrimitives.EnumOfValue<char, Turn>(c)

let parseDirection text =
    match [for c in text -> c] with
    | letter :: number ->
        {
            Turn = charToTurn letter;
            NumberOfBlocks = UInt32.Parse (String.Concat number)
        }
    | [] -> failwith "empty string not allowed"

let fetchUrl callback url =
    let request = WebRequest.Create (Uri url)

    use response = request.GetResponse()
    use stream = response.GetResponseStream()
    use reader = new IO.StreamReader(stream)
    callback reader

let readFile callback (fileName:string) =
    use reader = new IO.StreamReader(fileName) 
    callback reader

let parseInput parseElement delimiters (streamReader:IO.StreamReader) =
    let text = streamReader.ReadToEnd()
    Array.toList (text.Split delimiters)
    |> List.map (fun x -> parseElement (x.Trim()))
let splitOnComma reader = parseInput parseDirection [|','|] reader 



executeMoves (List.map parseDirection ["R2";"L3"])
executeMoves (List.map parseDirection ["R2";"R2";"R2"])
executeMoves (List.map parseDirection ["R5";"L5";"R5";"R3"])
// executeMoves (fetchUrl splitOnComma "http://adventofcode.com/2016/day/1/input")
executeMoves (readFile splitOnComma @"input.txt")
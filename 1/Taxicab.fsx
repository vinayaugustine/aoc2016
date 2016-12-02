open Microsoft.FSharp.Core
open System
open System.IO
open System.Net

type Direction = North=0 | East=1 | South=2 | West=3
type Turn = Left='L' | Right='R'
type Move = { Turn:Turn; NumberOfBlocks:uint32 }
type Position = { x:int; y:int }
type PosAndDirection = { direction:Direction; position:Position }

let computeDirection (direction:Direction) (turn:Turn) =
    let directions : Direction array = unbox (Enum.GetValues(typeof<Direction>))
    let i = match turn with
            | Turn.Left  -> -1
            | Turn.Right ->  1
            | _ -> failwith "invalid Turn value"

    let newIndex = match (int direction) + i with
                   | j when j < 0 -> directions.Length - 1
                   | j -> j % directions.Length
    directions.[newIndex]

let calculatePosition (pad:PosAndDirection) (move:Move) =
    let count = int move.NumberOfBlocks
    let direction = computeDirection pad.direction move.Turn
    let x, y = pad.position.x, pad.position.y
    let positions = [for i in 1 .. count -> match direction with
                                            | Direction.North -> x, y+i
                                            | Direction.South -> x, y-i
                                            | Direction.East  -> x+i, y
                                            | Direction.West  -> x-i, y
                                            | _  -> failwith "Invalid direction"]
    [for x,y in positions -> {direction=direction; position={x=x;y=y } }]
let computeDistance start finish =
    Math.Abs ((finish.x - start.x) + (finish.y - start.y))

let rec move start moves =
    match moves with
    | [] -> []
    | head :: tail ->
        let positions = calculatePosition start head
        positions @ (move (positions.Item (positions.Length-1)) tail)
let findShortestPathLength moves =
    let start = { direction=Direction.North; position = { x=0; y=0 } }
    let positions = move start moves
    let finish = positions.Item (positions.Length-1)
    printfn "[%A -> %A] Easter bunny HQ is %i blocks away" start.position finish.position (computeDistance start.position finish.position)
let findFirstDuplicate moves =
    let start = { direction=Direction.North; position = { x=0; y=0 } }
    let positions = [for p in move start moves -> p.position]
    
    let rec checkIfVisited (positionSet:Set<Position>) (positionList:List<Position>) =
        match positionList with
        | [] -> None
        | head :: tail -> if positionSet.Contains head then Some(head) else checkIfVisited (positionSet.Add head) tail
    
    let firstDuplicate = checkIfVisited Set.empty positions
    match firstDuplicate with
    | None -> printfn "No first duplicate"
    | Some(p) -> printfn "%A is first duplicate (%i blocks away)" p (computeDistance start.position p) 

// parsing code
let charToTurn c = LanguagePrimitives.EnumOfValue<char, Turn>(c)
let parseDirection text =
    match [for c in text -> c] with
    | letter :: number ->
        {
            Turn = charToTurn letter;
            NumberOfBlocks = UInt32.Parse (String.Concat number)
        }
    | [] -> failwith "empty string not allowed"

// Input code
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


// execute!
findShortestPathLength (List.map parseDirection ["R2";"L3"])
findShortestPathLength (List.map parseDirection ["R2";"R2";"R2"])
findShortestPathLength (List.map parseDirection ["R5";"L5";"R5";"R3"])
// executeMoves (fetchUrl splitOnComma "http://adventofcode.com/2016/day/1/input")
findShortestPathLength (readFile splitOnComma @"input.txt")

findFirstDuplicate (List.map parseDirection ["R8"; "R4"; "R4"; "R8"])
findFirstDuplicate (readFile splitOnComma @"input.txt")
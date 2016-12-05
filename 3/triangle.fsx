open System
open System.IO

let testTriangle l =
    match List.sort l with
    | [x;y;z] -> x + y > z
    | _ -> failwith "the triangle does not have three sides"

let lineToList (line:string) =
    line.Split [|' '|]
    |> Array.toList
    |> List.filter (fun v -> v.Length > 0)
    |> List.map Int32.Parse

let readInput fileName =
    seq { for l in File.ReadLines fileName -> l }
    |> Seq.map lineToList

let input = readInput @"3\input.txt"

input
|> Seq.filter testTriangle
|> Seq.length
|> printfn "Valid triangles: %A"

input
|> Seq.chunkBySize 3
|> Seq.map (fun (l:int list []) -> List.zip3 l.[0] l.[1] l.[2])
|> Seq.reduce (@)
|> Seq.map (fun (t:int*int*int) ->
            let x,y,z = t
            [x;y;z])
|> Seq.filter testTriangle
|> Seq.length
|> printfn "Valid triangles (vertically): %A"
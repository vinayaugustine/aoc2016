open System
open System.IO

let testTriangle l =
    match l with
    | [x;y;z] -> x + y > z
    | _ -> failwith "the triangle does not have three sides"

let lineToList (line:string) =
    line.Split [|' '|]
    |> Array.toList
    |> List.filter (fun v -> v.Length > 0)
    |> List.map Int32.Parse
    |> List.sort

let readInput fileName =
    seq { for l in File.ReadLines fileName -> l }
    |> Seq.map lineToList

readInput @"3\input.txt"
|> Seq.filter testTriangle
|> Seq.length
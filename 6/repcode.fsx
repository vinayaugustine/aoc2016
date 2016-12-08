open System
open System.IO

let foo sortFunc charSeq =
    charSeq
    |> Seq.groupBy id
    |> Seq.map (fun (letter, sq) -> letter, Seq.length sq)
    |> sortFunc snd
    |> Seq.map fst
    |> Seq.head
let mostCommon = foo Seq.sortByDescending 
let leastCommon = foo Seq.sortBy
let pivotInput lines =
    lines
    |> Seq.collect (fun s -> s |> Seq.mapi(fun i e -> (i, e)))
    |> Seq.groupBy fst
    |> Seq.map(fun (i, s) -> s |> Seq.map snd)

// let input = seq { for l in File.ReadLines @"6\test.txt" -> l }
let input = seq { for l in File.ReadLines @"6\input.txt" -> l }

pivotInput input
|> Seq.map mostCommon
|> (Array.ofSeq >> System.String)

pivotInput input
|> Seq.map leastCommon
|> (Array.ofSeq >> System.String)
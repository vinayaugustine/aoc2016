open System
open System.IO

type Ipv7 = { parts:list<string>; hypernetParts:list<string> }

let tests = [
    ("abba[mnop]qrst", true);
    ("abcd[bddb]xyyx", false);
    ("aaaa[qwer]tyui", false);
    ("ioxxoj[asdfgh]zxcvbn", true)
]

let lineToAddress (line:string) =
    let pieces = line.Split [|'[';']'|]
                 |> Seq.mapi (fun i e -> i,e)
                 |> Seq.groupBy (fun (i,e) -> i % 2 = 0)
                 |> Seq.map (fun (i,e) -> i, e |> (Seq.map snd))
                 |> Map.ofSeq
    {
        parts = pieces.[true] |> List.ofSeq;
        hypernetParts = pieces.[false] |> List.ofSeq
    }

let containsAbba sequencelength part =
    let testWindow w =
        let start  = w |> Seq.take sequencelength |> Array.ofSeq |> System.String
        let finish = w |> Seq.skip sequencelength |> List.ofSeq |> List.rev |> Array.ofList |> System.String
        (start = finish) && (start |> Seq.distinct |> Seq.length > 1)
    part
    |> Seq.windowed (sequencelength * 2)
    |> Seq.exists testWindow

let containsAbba2 = containsAbba 2
let supportsTls abbaTest (address:Ipv7) =
    (address.parts |> List.exists abbaTest) && not (address.hypernetParts |> List.exists abbaTest)

tests
|> List.map (fun (a,t) -> lineToAddress a,t)
|> List.filter (fun (a,t) -> not (supportsTls containsAbba2 a) = t)

seq { for l in File.ReadLines @"input.txt" -> l }
|> Seq.filter (lineToAddress >> (supportsTls containsAbba2))
|> Seq.length
|> printfn "%i"
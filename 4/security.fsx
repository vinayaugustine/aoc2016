open System
open System.IO
open System.Text.RegularExpressions

type SecurityId = { Id:string; SectorId:int; Checksum:string }

let idPattern = @"(?<id>([a-z]+-)+)(?<sectorId>\d+)\[(?<checkSum>[a-z]+)\]"

let (|LineMatch|_|) pattern input =
    let m = Regex.Match(input, pattern) in
    if m.Success then
        Some {
               Id=m.Groups.["id"].Value;
               SectorId=Int32.Parse m.Groups.["sectorId"].Value;
               Checksum=m.Groups.["checkSum"].Value
             }
        else None

let parseLine pattern line =
    match line with
    | LineMatch pattern id -> id
    | _ -> failwithf "Invalid id %s" line 

let parseLineDefault = parseLine idPattern
let idIsValid (checkSumLength:int) (secId:SecurityId) =
    secId.Id
    |> Seq.filter Char.IsLetter
    |> Seq.groupBy id
    |> Seq.map (fun (letter, sq) -> letter, Seq.length sq)
    |> Seq.sortBy (fun (letter, count) -> -count, letter)
    |> Seq.take checkSumLength
    |> Seq.map (fun (letter, count) -> letter)
    |> Array.ofSeq
    |> System.String = secId.Checksum

let idIsValid5 = idIsValid 5
let lines = [ "aaaaa-bbb-z-y-x-123[abxyz]";
              "a-b-c-d-e-f-g-h-987[abcde]";
              "not-a-real-room-404[oarel]";
              "totally-real-room-200[decoy]"]
// let input = Seq.ofList lines
let input = seq { for l in File.ReadLines "input.txt" -> l }

input |> Seq.map parseLineDefault
      |> Seq.filter idIsValid5
      |> Seq.sumBy (fun id -> id.SectorId)
      |> printfn "sector sum = %i"
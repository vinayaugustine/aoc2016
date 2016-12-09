open System
open System.Security.Cryptography
open System.Text

let input = "uqwqemis"

let hashInputs = seq { (uint64 0) .. System.UInt64.MaxValue }

let generateInputs (hashInputs:seq<uint64>) (input:string) =
    hashInputs |> Seq.map (sprintf "%s%i" input)

let hash (hasher:MD5) (input:string) =
    let byteArray = Encoding.ASCII.GetBytes(input)
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(byteArray))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let md5seq (input:seq<string>) =
    use md5 = MD5.Create()
    input |> Seq.map (hash md5)
let hashIsValid checkLength hash =
    hash |> Seq.take checkLength
         |> Seq.filter (fun c -> c = '0')
         |> Seq.length = checkLength

// let hash = md5 "abc3231929"
// hashIsValid 5 (md5 "abc3231929")
// hash |> (Seq.item 5)
 
// part 1
generateInputs hashInputs input
|> md5seq
|> Seq.filter (hashIsValid 5)
|> Seq.map (Seq.item 5)
|> Seq.take 8
|> Array.ofSeq
|> System.String
|> printfn "%s"

// part 2
generateInputs hashInputs input
|> md5seq
|> Seq.filter (hashIsValid 5)
|> Seq.map (fun h -> (h |> Seq.item 5), (h |> Seq.item 6))
|> Seq.filter (fun p -> '0' <= (fst p) && '7' >= (fst p))
|> Seq.groupBy fst
|> Seq.sortBy fst
|> Seq.map (snd >> List.ofSeq >> List.head)
|> Seq.take 8
|> Array.ofSeq
|> string
|> printfn "%s"
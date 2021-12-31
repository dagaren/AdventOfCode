module Utils

open System.IO

let printBanner day =
    printfn "--------------------------------"
    printfn "    Advent Of Code day %d" day
    printfn "--------------------------------"
    printfn ""

let printFirstStarResult result = 
    printfn " - Star 1. %s" result

let printSecondStarResult result = 
    printfn " - Star 2. %s" result


let readFileLines (filePath:string) = File.ReadAllLines(filePath)

let readInputFile name = readFileLines <| sprintf "./Inputs/%s" name

let split char (str:string) = str.Split [| char |]

let isNotEmptyString (str:string) = System.String.IsNullOrWhiteSpace str |> not
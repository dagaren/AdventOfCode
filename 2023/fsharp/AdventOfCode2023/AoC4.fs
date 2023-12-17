module AoC4

open Utils

module Parsing =
    open FParsec

    let pCard = pstring "Card" >>. spaces1 >>. pint32 .>> pstring ":"
    let pNumberList = many1 (spaces >>? pint32)
    let pOwnedNumbers = spaces >>. pchar '|' >>. spaces >>. pNumberList

    let pLine = tuple3 pCard pNumberList pOwnedNumbers

    let pFile = sepBy pLine (pchar '\n')

    let parseInput inputText = 
        let pResult = run pFile inputText
        
        match pResult with
        | Success(cards, _, _) -> Result.Ok(cards)
        | Failure(errorMsg, _, _) -> Result.Error (sprintf "Failed to parse input: %s" errorMsg)

let getIntersectionPoints (y, z) = 
    let intersection = Set.intersect (Set.ofList y) (Set.ofList z)

    intersection |> Set.toList |> List.length

let doubleOne (times: int) =
    let rec doubleHelper current result =
        if current = 0 then
            result
        else
        doubleHelper (current - 1) (result * 2)

    doubleHelper (times-1) 1

let incrementOrInitialize increment x =
    match x with
    | Some s -> Some(s + increment)
    | None -> Some(increment)

let incrementTimes times copies index = 
    copies |> Map.change index (incrementOrInitialize times)

let foldCards copies (cardIndex, winningNumbers, numbers) = 
    let newCopies = copies |> Map.change cardIndex (incrementOrInitialize 1)

    let currentCopies = Map.find cardIndex newCopies

    let numWonNumbers = getIntersectionPoints (winningNumbers, numbers)
    
    [ (cardIndex + 1) .. (cardIndex + numWonNumbers) ]
    |> List.fold (incrementTimes currentCopies) newCopies

let aoc4 () =
    printBanner 4

    let inputText = readInputFileText "AoC4.txt" 

    let result = Parsing.parseInput inputText

    match result with
    | Ok cards ->
        let total =
            cards
            |> List.map (fun (x, y,z) -> (y, z))
            |> List.map getIntersectionPoints
            |> List.filter (fun x -> x > 0)
            |> List.map doubleOne
            |> List.sum

        printFirstStarResult (sprintf "Result %d" total)

        let total2 = 
            cards 
            |> List.fold foldCards Map.empty<int,int> 
            |> Map.values
            |> Seq.sum

        printSecondStarResult (sprintf "Result %d" total2)
    | Error errorMsg ->
        printfn "Failed to parse: %s" errorMsg
﻿module AoC10

open Utils

type Token = 
 | OpeningToken of char
 | ClosingToken of char

let aoc10 () =
    printBanner 10

    let input = readInputFile "AoC10.txt"

    let getTokenPoints closingToken = 
        match closingToken with
        | ClosingToken ')' -> 3
        | ClosingToken ']' -> 57
        | ClosingToken '}' -> 1197
        | ClosingToken '>' -> 25137
        | _ -> failwith "Invalid token"

    let toToken char = 
        match char with
        | '[' | '{' | '(' | '<' -> OpeningToken char
        | ']' | '}' | ')' | '>' -> ClosingToken char
        | _ -> failwith "Unexpected token"

    let tokensMatch openingToken closingToken = 
        match openingToken, closingToken with
        | OpeningToken '[', ClosingToken ']'
        | OpeningToken '{', ClosingToken '}'
        | OpeningToken '<', ClosingToken '>'
        | OpeningToken '(', ClosingToken ')' -> true
        | _ -> false
          
    let rec pline pending queue =
        match pending with 
        | [] -> None
        | first :: remaining ->
            match first with
            | OpeningToken char ->
                let nextQueue = first :: queue
                pline remaining nextQueue
            | ClosingToken char ->
                match queue with
                | [] -> None
                | firstQueue :: remainingQueue ->
                    if (tokensMatch firstQueue first) then
                        pline remaining remainingQueue
                    else
                        Some first
                
    let processLine line = 
        let chars = Seq.toList line |> Seq.map toToken |> List.ofSeq

        let result = pline chars []

        match result with 
        | None -> 
            0
        | Some (ClosingToken char) -> 
            getTokenPoints (ClosingToken char)

    let score = input |> Array.map processLine |> Array.sum

    let result1 = sprintf "Total score: %d" score
    printFirstStarResult result1

    // Part 2
    let getTokenScore closingToken = 
        match closingToken with
        | ClosingToken ')' -> 1L
        | ClosingToken ']' -> 2L
        | ClosingToken '}' -> 3L
        | ClosingToken '>' -> 4L
        | _ -> failwith "Invalid token"

    let addScore current nextToken = (5L * current) + (getTokenScore nextToken)

    let calculateScore missingTokens = 
        missingTokens |> List.fold addScore 0

    let rec plineComplete pending queue =
           match pending with 
           | [] -> Some queue
           | first :: remaining ->
               match first with
               | OpeningToken char ->
                   let nextQueue = first :: queue
                   plineComplete remaining nextQueue
               | ClosingToken char ->
                   match queue with
                   | [] -> None
                   | firstQueue :: remainingQueue ->
                       if (tokensMatch firstQueue first) then
                           plineComplete remaining remainingQueue
                       else
                           None

    let getTokenInverse openingToken = 
        match openingToken with
        | OpeningToken '(' -> ClosingToken ')'
        | OpeningToken '[' -> ClosingToken ']'
        | OpeningToken '{' -> ClosingToken '}'
        | OpeningToken '<' -> ClosingToken '>'
        | _ -> failwith "Invalid token"

    let processLineAutocomplete line = 
        let chars = Seq.toList line |> Seq.map toToken |> List.ofSeq

        let result = plineComplete chars []

        match result with 
        | None -> 
            0L
        | Some pendingChars ->
            let ptokens = pendingChars |> List.map getTokenInverse
            let score  = calculateScore ptokens
            //printfn "Pending chars: %A, score: %d" ptokens score
            score 


    let scores = input |> Array.map processLineAutocomplete |> Array.filter (fun x -> x <> 0L) |> Array.sort

    let numElements = scores |> Array.length
    let middleIndex = numElements / 2 
    printfn "NumScores: %d, Middle: %d, Scores: %A" numElements middleIndex scores
    let middleScore = Array.get scores middleIndex
    
    let result2 = sprintf "Score: %A" middleScore
    printSecondStarResult result2
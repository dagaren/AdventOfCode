module AoC2

open Utils

type Turn = 
| Rock
| Paper
| Scissors

type GameResult = 
| Win
| Loss
| Draw

let aoc2 () =
    printBanner 2

    let inputLines = readInputFile "AoC2.txt" |> Array.toList

    let mapOpponentTurn x = 
        match x with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> failwith "Invalid input for opponent turn"

    let mapOwnTurn x = 
        match x with
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissors
        | _ -> failwith "Invalid input for own turn"

    let mapExpectedResult x = 
        match x with
        | "X" -> Loss
        | "Y" -> Draw
        | "Z" -> Win
        | _ -> failwith "Invalid input for expected result"

    let shapeScore x =
        match x with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    
    let resultScore x = 
        match x with
        | Win -> 6
        | Loss -> 0
        | Draw -> 3

    let game mine opponent =
        match mine with
        | Rock ->
            match opponent with
            | Rock -> Draw
            | Paper -> Loss
            | Scissors -> Win
        | Paper ->
            match opponent with
            | Rock -> Win
            | Paper -> Draw
            | Scissors -> Loss
        | Scissors ->
            match opponent with
            | Rock -> Loss
            | Paper -> Win
            | Scissors -> Draw

    let turnForResult opponent expectedResult = 
        match opponent with
        | Rock ->
            match expectedResult with
            | Win -> Paper
            | Draw -> Rock
            | Loss -> Scissors
        | Paper ->
            match expectedResult with
            | Win -> Scissors
            | Draw -> Paper
            | Loss -> Rock
        | Scissors ->
            match expectedResult with
            | Win -> Rock
            | Draw -> Scissors
            | Loss -> Paper

    let result1 =
        inputLines
        |> List.map (split ' ')
        |> List.map (fun [| x; y |] -> (x |> mapOpponentTurn, y |> mapOwnTurn))
        |> List.map (fun (opponent, mine) -> (shapeScore mine) + (resultScore (game mine opponent)))
        |> List.sum
        |> string

    printFirstStarResult result1

    let result2 =
        inputLines
        |> List.map (split ' ')
        |> List.map (fun [| x; y |] -> (x |> mapOpponentTurn, y |> mapExpectedResult))
        |> List.map (fun (opponent, expectedResult) -> (opponent, (turnForResult opponent expectedResult), expectedResult))
        |> List.map (fun (opponent, mine, expectedResult) -> (shapeScore mine) + (resultScore expectedResult))
        |> List.sum
        |> string

    printSecondStarResult result2
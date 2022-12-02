module AoC1

open Utils

let aoc1 () =
    printBanner 1

    let inputLines = readInputFile "AoC1.txt" |> Array.toList

    let mapElement el =
        match el with
        | "" -> None
        | _ -> Some (el |> int)

    let input = inputLines |> List.map mapElement

    let reduceInput lists element =
        match element with
        | None -> [] :: lists
        | Some number ->
            match lists with
            | [] -> [[ number ]]
            | first :: remaining -> (number :: first) :: remaining

    let numbers = 
        input
        |> List.fold reduceInput []
        |> List.map List.sum

    let result1 =
        numbers
        |> List.max
        |> string

    printFirstStarResult result1

    let result2 = 
        numbers
        |> List.sortDescending
        |> List.take 3
        |> List.sum
        |> string
    
    printFirstStarResult result2

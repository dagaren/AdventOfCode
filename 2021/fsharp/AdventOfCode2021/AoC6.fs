module AoC6

open Utils

let aoc6 () =
    printBanner 6

    let input = [|  1;3;1;5;5;1;1;1;5;1;1;1;3;1;1;4;3;1;1;2;2;4;2;1;3;3;2;4;4;4;1;3;1;1;4;3;1;5;5;1;1;3;4;2;1;5;3;4;5;5;2;5;5;1;5;5;2;1;5;1;1;2;1;1;1;4;4;1;3;3;1;5;4;4;3;4;3;3;1;1;3;4;1;5;5;2;5;2;2;4;1;2;5;2;1;2;5;4;1;1;1;1;1;4;1;1;3;1;5;2;5;1;3;1;5;3;3;2;2;1;5;1;1;1;2;1;1;2;1;1;2;1;5;3;5;2;5;2;2;2;1;1;1;5;5;2;2;1;1;3;4;1;1;3;1;3;5;1;4;1;4;1;3;1;4;1;1;1;1;2;1;4;5;4;5;5;2;1;3;1;4;2;5;1;1;3;5;2;1;2;2;5;1;2;2;4;5;2;1;1;1;1;2;2;3;1;5;5;5;3;2;4;2;4;1;5;3;1;4;4;2;4;2;2;4;4;4;4;1;3;4;3;2;1;3;5;3;1;5;5;4;1;5;1;2;4;2;5;4;1;3;3;1;4;1;3;3;3;1;3;1;1;1;1;4;1;2;3;1;3;3;5;2;3;1;1;1;5;5;4;1;2;3;1;3;1;1;4;1;3;2;2;1;1;1;3;4;3;1;3 |]

    let transformKey (key, value) = 
        if key = 0 then
            [(6, value); (8, value)]
        else
            [(key - 1, value)]
    
    let step (currentFishes:Map<int,int64>) index = 
        currentFishes 
            |> Map.toList 
            |> List.collect (fun (key, value) -> transformKey (key, value))
            |> List.groupBy (fun (key, value) -> key)
            |> List.map (fun (key, elements) -> (key, (elements |> List.map (fun (key, value) -> value) |> List.sum)))
            |> Map.ofList

    let getNumFishes (initialFishes:int[]) days = 
        let fishesMap = initialFishes |> Array.countBy id |> Array.map (fun (el, count) -> (el, int64(count))) |> Map.ofArray

        [ for i in 1..days -> i]
         |> List.fold step fishesMap
         |> Map.toList
         |> List.map(fun (key, num) -> num)
         |> List.sum

    let numSteps = 80

    let numFishes = getNumFishes input numSteps

    let result1 = sprintf "Num fishes after %d days: %d" numSteps numFishes
    printFirstStarResult result1

    let numSteps2 = 256

    let numFishes2 = getNumFishes input numSteps2

    let result2 = sprintf "Result. Num fishes after %d days: %d" numSteps2 numFishes2
    printSecondStarResult result2
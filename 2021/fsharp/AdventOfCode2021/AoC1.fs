module AoC1

open Utils

let aoc1 () =
    printBanner 1

    let inputLines = readInputFile "AoC1.txt"

    let input = inputLines |> Seq.toArray |> Array.map int
    
    printfn "Input Size: %d"  (input |> Array.length)

    let increasingMeasurements = 
        input
        |> Array.pairwise
        |> Array.filter (fun (prev, current) -> current > prev)
        |> Array.length
    
    let result1 = sprintf "Numer of increasing measurments: %d" increasingMeasurements
    printFirstStarResult result1

    // Part 2

    let increasingWindows = 
        input
        |> Array.windowed 3
        |> Array.map (fun x -> x |> Array.sum)
        |> Array.pairwise
        |> Array.filter (fun (prev, current) -> current > prev)
        |> Array.length

    let result2 = sprintf "Numer of increasing windows: %d" increasingWindows
    printSecondStarResult result2
    
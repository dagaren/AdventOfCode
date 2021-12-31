module AoC7

open Utils

let aoc7 () =
    printBanner 7

    let input = readInputFile "AoC7.txt" |> Array.map (split ',') |> Array.head |> Array.map int

    let min = input |> Array.fold min System.Int32.MaxValue
    let max = input |> Array.fold max 0

    let range =  [| min .. max |]

    let getConstantFuel origin destination = abs (destination - origin)

    let calculatePositionFuel input position fuelCalculator = input |> Array.map (fuelCalculator position) |> Array.sum

    let (bestPosition, fuel) = range 
                                |> Array.map (fun position -> (position, (calculatePositionFuel input position getConstantFuel)))
                                |> Array.sortBy (fun (position, fuel) -> fuel)
                                |> Array.head

    printfn "Min: %d, Max: %d " min max
    let result1 = sprintf "Best position: %d, fuel: %d" bestPosition fuel
    printFirstStarResult result1


    let getProgresiveFuel origin destination = 
        let distance = abs (destination - origin)

        int(distance * (distance + 1) / 2)

    
    let (bestPositionProgresive, fuelProgresive) = range 
                                                    |> Array.map (fun position -> (position, (calculatePositionFuel input position getProgresiveFuel)))
                                                    |> Array.sortBy (fun (position, fuel) -> fuel)
                                                    |> Array.head

    let result2 = sprintf "Best position progressive: %d, fuel progressive: %d" bestPositionProgresive fuelProgresive
    printSecondStarResult result2


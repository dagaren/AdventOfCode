module AoC3

open Utils

let aoc3 () =
        printBanner 3

        let input = readInputFile "AoC3.txt"
        
        printfn "Input Size: %d" (input |> Array.length)

        let numBits = input |> Array.head |> Seq.length
        
        printfn "Num bits: %d" numBits

        let getBitStats index (numbers:string[]) = 
            let (ones, zeroes) = numbers
                                 |> Array.map (fun x -> x[index..index])
                                 |> Array.partition (fun x -> x = "1")
            (ones |> Array.length, zeroes |> Array.length) 

        let bitStats = [for x in 0..(numBits - 1) -> x] |> List.map (fun x -> getBitStats x input)

        let getGamma bitStats = 
            let bstring = bitStats 
                             |> List.map (fun (ones, zeroes) -> if ones > zeroes then "1" else "0")
                             |> List.fold (+) ""

            System.Convert.ToInt32(bstring, 2);

        let getEpsilon bitStats =
            let bstring = bitStats 
                            |> List.map (fun (ones, zeroes) -> if ones > zeroes then "0" else "1")
                            |> List.fold (+) ""

            System.Convert.ToInt32(bstring, 2);

        let gamma = getGamma bitStats
        let epsilon = getEpsilon bitStats
        let powerConsumption = gamma * epsilon

        let result1 = sprintf "Gamma: %d, Epsilon: %d, Power Consumption: %d" gamma epsilon powerConsumption
        printFirstStarResult result1

        // Part 2

        let rec getOxygene numbers index =
            match numbers with
            | [|n|] -> System.Convert.ToInt32(n, 2)
            | [||] -> failwith "Invalid Empty number list"
            | _ ->
                let remaining = match (getBitStats index numbers) with
                                | (ones, zeroes) when ones >= zeroes -> 
                                    numbers |> Array.filter (fun x -> x[index..index] = "1") 
                                | _ -> 
                                    numbers |> Array.filter (fun x -> x[index..index] = "0")
                getOxygene remaining (index + 1)

        let rec getCO2 numbers index =
            match numbers with
            | [|n|] -> System.Convert.ToInt32(n, 2)
            | [||] -> failwith "Invalid Empty number list"
            | _ ->
                let remaining = match (getBitStats index numbers) with
                                | (ones, zeroes) when zeroes <= ones -> 
                                    numbers |> Array.filter (fun x -> x[index..index] = "0") 
                                | _ -> 
                                    numbers |> Array.filter (fun x -> x[index..index] = "1")
                getCO2 remaining (index + 1)
                
                

        let oxygenGeneratorRating = getOxygene input 0
        let co2ScrubberRating = getCO2 input 0
        let lifeSupportRating = oxygenGeneratorRating * co2ScrubberRating

        let result2 = sprintf "Oxygen generator rating: %d, CO2 Scrubber Rating: %d, Life Support rating: %d" oxygenGeneratorRating  co2ScrubberRating lifeSupportRating
        printSecondStarResult result2
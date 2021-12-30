module AoC11

open Utils

type octopusmap = {
    map: int[,];
    rows: int;
    columns: int
    numFlashed: int
}

let aoc11 () =
    printBanner 11

    let input = [|
        "4341347643";
        "5477728451";
        "2322733878";
        "5453762556";
        "2718123421";
        "4237886115";
        "5631617114";
        "2217667227";
        "4236581255";
        "4482627641"
    |]

    let tInput = input
                  |> Array.map (fun x -> Seq.toList x) 
                  |> Array.map (fun x -> x |> List.map (fun y -> string(y)) |> List.map int)
    let tmap = array2D tInput

    let octopusmap = {
        map = tmap
        rows = tmap.GetLength(0)
        columns = tmap.GetLength(1)
        numFlashed = 0
    }

    let isValidPosition rows columns (x, y) = x >= 0 && y >= 0 && x < rows && y < columns
    
    let getAdjacentPoints map (x, y) =
        [ for i in (x-1)..(x+1) do for j in (y-1)..(y+1) -> (i, j) ]
            |> List.filter (isValidPosition map.rows map.columns)

    let getIterator map = 
        [ for i in 0..(map.rows - 1) do for j in 0..(map.columns - 1) -> (i, j)]

    let iterateMap map f = 
        getIterator map
          |> List.iter (fun (x, y) -> f map (x, y))

    let increasePos map (x, y) =
        if map.map[x, y] > -1 then
            map.map[x, y] <- map.map[x, y] + 1

    let resetFlashed map (x, y) =
        map.map[x,y] <- if map.map[x,y] = -1 then 0 else map.map[x,y]

    let rec flashOctopuses map =
        let flashedPos = getIterator map
                          |> List.tryFind (fun (x, y) -> map.map[x, y] > 9)

        match flashedPos with
         | None -> 0
         | Some (x, y) ->
            map.map[x,y] <- -1
            getAdjacentPoints map (x, y) |> List.iter (fun point -> increasePos map point)
            1 + (flashOctopuses map)

    let step map =
        // Increase octopus level by 1
        iterateMap map increasePos

        // Flash octopuses
        let flashed = flashOctopuses map
        
        // Reset Flashed octopuses
        iterateMap map resetFlashed

        flashed

    let numFlashed = [for i in 1..100 -> i]
                        |> List.map (fun x ->
                            let numFlashed = step octopusmap
                            //printfn "ITeration: %d, num flases: %d" x numFlashed
                            numFlashed)
                        |> List.sum

    let result1 = sprintf "Num flashed after 100 steps: %d" numFlashed
    printFirstStarResult result1
    
    let tmap2 = array2D tInput
    let octopusmap2 = {
        map = tmap2
        rows = tmap2.GetLength(0)
        columns = tmap2.GetLength(1)
        numFlashed = 0
    }

    let totalPoints = octopusmap2.rows * octopusmap2.columns

    let numFlashed = seq {for i in 1..1000 -> i}
                        |> Seq.tryFind  (fun x -> 
                            let numFlashed = (step octopusmap2) 
                            //printfn "ITeration: %d, num flases: %d" x numFlashed
                            numFlashed = totalPoints)

    let result2 = 
        match numFlashed with
        | None -> "Part 2. No step flashing all"
        | Some i -> sprintf "Part 2. First step flashing all: %d" i
    printSecondStarResult result2

    

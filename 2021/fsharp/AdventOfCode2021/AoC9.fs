module AoC9

open Utils

type heightmap = {
    map: int[,];
    rows: int;
    columns: int
}

let aoc9 () =
    printBanner 9

    let input = readInputFile "AoC9.txt"

    let tInput = input 
               |> Array.map (fun x -> Seq.toList x) 
               |> Array.map (fun x -> x |> List.map (fun y -> string(y)) |> List.map int)

    let tmap = array2D tInput
    
    let heightmap = {
        map = tmap
        rows = tmap.GetLength(0)
        columns = tmap.GetLength(1)
    }

    let isValidPosition rows columns (x, y) = x >= 0 && y >= 0 && x < rows && y < columns

    let getAdjacentPoints heightmap (x, y) =
        [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
        |> List.filter (isValidPosition heightmap.rows heightmap.columns)

    let getWithAdjacentPoints heightmap (x, y) =
       let adjacentHeights =  getAdjacentPoints heightmap (x, y)
                            |> List.map (fun (x, y) -> heightmap.map[x,y])
       ((x, y), heightmap.map[x, y], adjacentHeights)

    let isLowPoint ((x, y), value,adjacents) = adjacents |> List.forall (fun y -> y > value)

    let heightmapRisk  = [ for i in 0..(heightmap.rows-1) do for j in 0..(heightmap.columns-1) -> (i, j)]
                            |> List.map (getWithAdjacentPoints heightmap)
                            |> List.filter isLowPoint
                            |> List.map (fun ((x, y), value, adjacents) -> value + 1)
                            |> List.sum

    let result1 = sprintf "Sum of the risk level of all low points: %d" heightmapRisk
    printFirstStarResult result1

    // Part 2

    let getDownwardsAdjacents heightmap (x, y) =
        getAdjacentPoints heightmap (x, y)
            |> List.filter (fun (x, y) -> heightmap.map[x, y] < 9)
        
    let rec getBasinSize pending visited size = 
        match pending with
        | [] -> size
        | first :: rest ->
            let downwardsAdjacents = getDownwardsAdjacents heightmap first
                                        |> List.except visited
                                        |> List.except pending

            let nextSize = size + 1
            let nextVisited = first :: visited
            let nextPending = rest @ downwardsAdjacents
            
            getBasinSize nextPending nextVisited nextSize

    let lowPoints  = [ for i in 0..(heightmap.rows-1) do for j in 0..(heightmap.columns-1) -> (i, j)]
                        |> List.map (getWithAdjacentPoints heightmap)
                        |> List.filter isLowPoint
                        |> List.map (fun ((x, y), value, adjacents) -> (x, y))

    let higherBasins = lowPoints
                        |> List.map (fun (x, y) -> ((x, y), (getBasinSize [(x,y)] [] 0)))
                        |> List.sortByDescending (fun (point, size) -> size)
                        |> List.take 3

    printfn "Higher basins: "
    higherBasins
        |> List.iter (fun (point, size) -> printfn "LowPoint: %A, basinSize = %d" point size)

    let result = higherBasins 
                    |> List.map (fun (point, size) -> size)
                    |> List.fold (*) 1

    let result2 = sprintf "Result: %d" result
    printSecondStarResult result2


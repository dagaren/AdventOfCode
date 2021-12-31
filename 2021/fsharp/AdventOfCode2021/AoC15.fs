module AoC15

open Utils

type RiskMap = {
    map: Map<(int*int),int>
    rows: int
    columns: int
}

let calculatePathRisk path (map:Map<(int*int),int>) = path |> List.map(fun point -> map[point]) |> List.sum

let isValidPosition rows columns (x, y) = x >= 0 && y >= 0 && x < rows && y < columns

let getAdjacents (x, y) rows columns =
    [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
        |> List.filter (isValidPosition rows columns)

let selectBestPath map path1 path2 =
    match (path1, path2) with
    | (None, None) -> None
    | (None, Some x) -> Some x
    | (Some x, None) -> Some x
    | (Some x, Some y) ->
        let risk1 = calculatePathRisk x map
        let risk2 = calculatePathRisk y map

        if risk1 < risk2 then
            Some x
        else 
            Some y

let manhattenDistance (x1, y1) (x2, y2) =
    abs(x1 - x2) + abs(y1 - y2)
       
let rec getBestPath map currentPath =
    match currentPath with
    | [] -> failwith "Empty path"
    | (x, y) :: remaining when (x = (map.rows - 1) && y = (map.columns - 1)) -> 
        Some currentPath
    | (x, y) :: _ ->
        let adjacents = getAdjacents (x, y) map.rows map.columns |> List.except currentPath |> List.sortByDescending (fun (x,y) -> manhattenDistance (x,y) (map.rows - 1, map.columns - 1))

        adjacents 
            |> List.map (fun (x, y) -> getBestPath map ((x,y) :: currentPath)) 
            |> List.fold (selectBestPath map.map) None

let printPath path map = 
    for i = 0 to map.rows-1 do
        for j = 0 to map.columns-1 do
            if (path |> List.contains (i, j)) then
                 printf "#"
            else
                 printf "."
       
        printfn ""

let rec getBestPath2 map pendingPaths bestPath =
    match pendingPaths with
    | [] -> bestPath
    | currentPath :: remainingPaths ->
        match currentPath with
            | [] -> bestPath
            | (x, y) :: remaining when (x = (map.rows - 1) && y = (map.columns - 1)) -> 
                
                let newbestPath = selectBestPath map.map bestPath (Some currentPath)

                if newbestPath <> bestPath then
                    match newbestPath with
                    | Some s -> 
                        printPath s map
                        printfn  "New best path found. Risk: %d" (calculatePathRisk s map.map)
                    | _ -> ()
                    

                getBestPath2 map remainingPaths newbestPath
            | (x, y) :: _ ->
                let currentRisk = calculatePathRisk currentPath map.map

                match bestPath with
                | Some path when ((calculatePathRisk path map.map) < currentRisk) ->
                    getBestPath2 map remainingPaths bestPath
                | _ ->
                    let adjacents = 
                        getAdjacents (x, y) map.rows map.columns
                        |> List.except currentPath
                        |> List.sortBy (fun (x,y) -> manhattenDistance (x,y) (map.rows - 1, map.columns - 1) + map.map[(x,y)] )

                    let newPendingPaths = (adjacents |> List.map (fun (x, y) -> (x, y) :: currentPath)) @ remainingPaths

                    getBestPath2 map newPendingPaths bestPath

let updatePointEntry newVal (x:Option<int>) = 
    match x with
    | Some currentVal -> if newVal < currentVal then Some newVal else Some currentVal
    | None -> Some newVal

let rec shortestPath caveMap (alreadySet:List<(int*int)>) (distances:Map<(int*int),int>) =
    let numAlreadySet = alreadySet |> List.length

    if numAlreadySet = (caveMap.columns * caveMap.rows) then
        distances
    else
        let (point, distance) = distances |> Map.toList |> List.where (fun (point, distance) -> not <| List.contains point alreadySet) |> List.sortBy (fun (x, y) -> y) |> Seq.head

        let newAlreadySet = point :: alreadySet

        let adjacents = getAdjacents point caveMap.rows caveMap.columns

        let newDistances = 
            adjacents 
            |> List.map (fun adjacent -> (adjacent, distances[point] + caveMap.map[adjacent] )) 
            |> List.fold (fun currentDistances (point, value) -> currentDistances |> Map.change point (updatePointEntry value)) distances

        shortestPath caveMap newAlreadySet newDistances

open FSharpx.Collections
open System.Collections.Generic

let toMap dictionary = 
    (dictionary :> seq<_>)
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

let shortestPath2 caveMap startPoint =
    let mutable he = Heap.empty<int*(int * int)> false |> Heap.insert (0, startPoint)
    let distances = new Dictionary<(int*int),int>()
    distances[startPoint] <- 0

    while he.IsEmpty = false do
        let (distance, point) = he.Head
        he <- he.Tail ()

        let adjacents = getAdjacents point caveMap.rows caveMap.columns

        adjacents 
          |> List.map (fun adjacent -> (adjacent, distances[point] + caveMap.map[adjacent] ))
          |> List.iter (fun (point, distance) -> if distances.ContainsKey(point) = false || distances[point] > distance then distances[point] <- distance; he <- he.Insert (distance, point))

    distances |> toMap

let getCaveMap input = 
    let map = 
        input 
         |> List.mapi (fun indexX value -> 
                         value |> Seq.mapi(fun indexY x -> ((indexX, indexY), int(string(x)))) |> List.ofSeq)
         |> List.collect id
         |> Map.ofList
    let rows = (map |> Map.toList |> List.map (fun ((x, y), value) -> x) |> List.max) + 1
    let columns =  (map |> Map.toList |> List.map (fun ((x, y), value) -> y) |> List.max) + 1
    {
        map = map
        columns = columns
        rows = rows
    }

let aoc15 () =
    printBanner 15

    let input = readInputFile "Aoc15.txt" |> Array.toList
    
    printfn "Loading map"
    let caveMap = getCaveMap input
    printfn "Map loaded. Columns: %d. Rows: %d" caveMap.columns caveMap.rows

    let distances = shortestPath2 caveMap (0,0)
    let result1 = sprintf " Distance to end point: %d" distances[(caveMap.rows-1, caveMap.columns-1)]
    printFirstStarResult result1


    // Part 2
    let incrementLine (line:string) = 
        line 
            |> Seq.map (fun x -> int(string(x)))
            |> Seq.map (fun x -> if x = 9 then 1 else x + 1) 
            |> Seq.map (fun x -> string(x))
            |> Seq.fold (+) ""
    
    let repeatList list = 
        list |> List.map (fun x -> incrementLine x)
   

    let repeatRightTimes list times = 
        [1..times] 
            |> List.fold (fun state _ -> 
                            let last = state |> List.last
                            state @ [(repeatList last)]) [ list ]
            |> List.fold (fun state next -> List.map2 (fun x y -> x + y) state next) (List.replicate list.Length "")


    let repeatDownTimes list times = 
        [1..times] 
        |> List.fold (fun state _ -> 
                        let last = state |> List.last
                        state @ [(repeatList last)]) [ list ]
        |> List.concat

    let bigInput = repeatDownTimes (repeatRightTimes input 4) 4

    let bigCaveMap = getCaveMap bigInput

    let distances2 = shortestPath2 bigCaveMap (0,0)
    let result2 = sprintf " Distance to end point: %d" distances2[(bigCaveMap.rows-1, bigCaveMap.columns-1)]
    printFirstStarResult result2
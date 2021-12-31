module AoC25

open Utils

type SeaCucumber = 
| HorizontalSeaCucumber
| VerticalSeaCucumber

type SeaMap = {
    map: Map<(int*int),SeaCucumber>
    rows: int
    columns: int
}

let aoc25 () = 
    printBanner 25

    let input = readInputFile "AoC25.txt" |> Array.toList 

    let getSeaCucumber char = 
        match char with 
        | '>' -> Some HorizontalSeaCucumber
        | 'v' -> Some VerticalSeaCucumber
        | _ -> None

    let getNextPoint cucumber (x, y) map =
        match cucumber with
        | VerticalSeaCucumber -> 
            let nextY = if y = map.rows - 1 then 0 else y + 1
            (x, nextY)
        | HorizontalSeaCucumber -> 
            let nextX = if x = map.columns - 1 then 0 else x + 1
            (nextX, y)

    let getMap (input:string list) = 

        let initialMap = input 
                      |> List.mapi (fun lineIndex line -> (lineIndex, line))
                      |> List.collect (fun (lineIndex, line) -> line |> Seq.toList |> List.mapi(fun columnIndex char -> ((columnIndex, lineIndex), (getSeaCucumber char))))
                      |> List.filter (fun (point, cucumberOption) -> cucumberOption.IsSome)
                      |> List.map (fun (point, cucumberOption) -> (point, cucumberOption.Value))
                      |> Map.ofList

        {
            map = initialMap
            rows = input.Length
            columns = input[0].Length
        }

    let moveCucumber map (x, y) cucumber =
        let nextPoint = getNextPoint cucumber (x, y) map

        if map.map.ContainsKey nextPoint then
            ((x, y), cucumber)
        else
            (nextPoint, cucumber)

    let isHorizontalCucumber cucumber = 
        match cucumber with
        | VerticalSeaCucumber -> false
        | HorizontalSeaCucumber -> true

    let isVerticalCucumber cucumber = 
        match cucumber with
        | VerticalSeaCucumber -> true
        | HorizontalSeaCucumber -> false

    let step seaMap =
        let middleMapPoints = 
            seaMap.map 
            |> Map.toList
            |> List.map (fun ((x, y), cucumber) -> 
                            if isHorizontalCucumber cucumber then
                                moveCucumber seaMap (x, y) cucumber
                            else
                                ((x, y), cucumber))
            |> Map.ofList

        let middleMap = { seaMap with map = middleMapPoints }

        let finalMapPoints = 
            middleMap.map
            |> Map.toList
            |> List.map (fun ((x, y), cucumber) -> 
               if isVerticalCucumber cucumber then
                   moveCucumber middleMap (x, y) cucumber
               else
                   ((x, y), cucumber))
            |> Map.ofList

        let finalMap = { seaMap with map = finalMapPoints }
        finalMap

    let rec runSteps map steps =
        let nextMap = step map

        if nextMap = map then
            (map, steps + 1)
        else
            runSteps nextMap (steps + 1)

    let initialMap = getMap input

    let finalMap, steps = runSteps initialMap 0

    let result1 = sprintf "Num steps until no changes: %d" steps
    printFirstStarResult result1

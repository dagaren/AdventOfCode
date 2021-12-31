module AoC5

open Utils

let aoc5 () =
    printBanner 5

    let input = readInputFile "AoC5.txt" |> Array.toList 

    let parsePoint (pstring:string) =
        let r = pstring.Split([|","|], System.StringSplitOptions.RemoveEmptyEntries)
        (int(r[0]), int(r[1]))

    let isHvLine ((x1, y1), (x2, y2)) = (x1 = x2) || (y1 = y2)

    let getLinePoints ((x1, y1), (x2, y2)) =
        let difx = abs (x1 - x2)
        let dify = abs (y1 - y2)
        let stepx = if x1 > x2 then -1 else 1
        let stepy = if y1 > y2 then -1 else 1
        if x1 = x2 then
            let min = if y1 > y2 then y2 else y1
            let max = if y1 > y2 then y1 else y2
            [ for i in min..max -> (x1, i) ]
        elif y1 = y2 then
            let min = if x1 > x2 then x2 else x1
            let max = if x1 > x2 then x1 else x2
            [ for i in min..max -> (i, y1) ]
        elif difx = dify then
            [for i in 0..difx -> (x1 + (stepx * i), y1 + (stepy * i))]
        else
            failwith "Invalid line"

    let points = input
                 |> List.map (fun x -> 
                                let r = x.Split([|" -> "|], System.StringSplitOptions.RemoveEmptyEntries)
                                (r[0], r[1]))
                 |> List.map (fun (p1, p2) -> ((parsePoint p1), parsePoint p2))

    let numOverlappingPoints = points
                                |> List.filter isHvLine
                                |> List.collect getLinePoints
                                |> List.countBy id
                                |> List.filter (fun (point, num) -> num >= 2)
                                |> List.length

    let result1 = sprintf "Result: %d" numOverlappingPoints
    printFirstStarResult result1

    let numOverlappingPoints2 = points
                                |> List.collect getLinePoints
                                |> List.countBy id
                                |> List.filter (fun (point, num) -> num >= 2)
                                |> List.length

    let result2 = sprintf "Result: %d " numOverlappingPoints2
    printSecondStarResult result2
module AoC13

open Utils

type Fold =
    | FoldX of int
    | FoldY of int

let aoc13 () = 
    printBanner 13

    let input = readInputFile "AoC13.txt"

    let pointLines = input |> Array.takeWhile isNotEmptyString
    let instructionLines = input |> Array.skipWhile isNotEmptyString |> Array.skip 1 |> Array.toList

    let foldInstructions =
        instructionLines
        |> List.map (fun x -> x.Substring("fold along ".Length))
        |> List.map (fun x -> let r = x.Split '=' in (r[0],int(r[1])))
        |> List.map (fun (direction, value) -> 
                            match direction with
                            | "x" -> FoldX value
                            | "y" -> FoldY value
                            | _ -> failwith "Invalid direction")

    let points = pointLines |> Array.toList |> List.map (fun x -> let r = x.Split ',' in (int(r[0]), int(r[1])))


    let foldPointVertical (x, y) max = (max - x - 1, y)
    let foldPointHorizontal (x, y) max = (x, max - y - 1)

    let foldVertical points position = 
        let numElements = position * 2 + 1
        let (leftFold, rightFold) = points |> List.partition (fun (x, y) -> x < position)
        
        let rightFoldInversed = 
            rightFold
             |> List.map (fun (x, y) -> foldPointVertical (x, y) numElements)

        leftFold @ rightFoldInversed |> Set.ofList |> Set.toList

    let foldHorizontal points position = 
        let numElements = position * 2 + 1
        let (upFold, downFold) = points |> List.partition (fun (x, y) -> y < position)
        
        let downFoldInversed = 
            downFold
             |> List.map (fun (x, y) -> foldPointHorizontal (x, y) numElements)

        upFold @ downFoldInversed |> Set.ofList |> Set.toList
    
    let fold points instruction = 
        match instruction with
        | FoldX value -> foldVertical points value
        | FoldY value -> foldHorizontal points value
    
    let firstInstruction = foldInstructions |> List.head

    let finalPoints = fold points firstInstruction

    let result1 = sprintf "Num of points: %d" (finalPoints |> List.length)
    printFirstStarResult result1

    let finalPoints2 = List.fold fold points foldInstructions

    let result2 = sprintf "Result 2 Num of points: %d" (finalPoints2 |> List.length)
    printSecondStarResult result2

    let maxX = finalPoints2 |> List.map (fun (x, y) -> x) |> List.max
    let maxY = finalPoints2 |> List.map (fun (x, y) -> y) |> List.max

    for i = 0 to maxY do
      for j = 0 to maxX do
        if (finalPoints2 |> List.contains (j, i)) then
            printf "#"
        else
            printf "."
      
      printfn ""
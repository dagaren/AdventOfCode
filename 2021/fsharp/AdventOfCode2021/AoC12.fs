module AoC12

open Utils

let aoc12 () =
    printBanner 12

    let input = readInputFile "AoC12.txt" |> Array.toList

    let updateKey newVal oldVal = 
        match oldVal with
           | None -> []
           | Some x -> newVal :: x

    let r = input 
            |> List.map (fun x -> let r = x.Split '-' in (r[0],r[1]))
    let s = (r @ (r |> List.map(fun (x, y) -> (y, x))))

    let graph = 
        s
         |> List.groupBy (fun (x, y) -> x)
         |> List.map (fun (x , y) -> (x, (y |> List.map (fun (i, j) -> j))))
         |> Map.ofList

    let isUpper (str:string) = System.Char.IsUpper str[0] 

    let isLower (str:string) = System.Char.IsLower str[0] 
    
    let canBeVisited (path:string list) node = 
        match (isUpper node) with
        | true -> true
        | false -> not <| List.contains node path
    
    let rec findPath canBeVisited (graph:Map<string,string list>) (visited:string list) : string list list =
        match visited with
        | [] -> failwith "Empty array"
        | first :: remaining when first = "end" -> [ visited ]
        | first :: remaining ->
            graph[first]
             |> List.filter (canBeVisited visited)
             |> List.map (fun x -> findPath canBeVisited graph (x :: visited))
             |> List.concat

    let result = findPath canBeVisited graph [ "start" ]

    let result1 = sprintf "Num paths: %d" (result |> List.length)
    printFirstStarResult result1

    let canBeVisited2 (path:string list) node = 
        match (isUpper node) with
        | true -> true
        | false -> 
            match node with
            | "start" -> false
            | n when (not <| List.contains node path) -> true
            | n when (path |> List.filter isLower |> List.countBy id |> List.filter (fun (x, y) -> y > 1) |> List.isEmpty) -> true
            | _ -> false
                

    let paths = findPath canBeVisited2 graph [ "start" ]

    let result2 = sprintf "Result 2. Num paths: %d" (paths |> List.length)
    printSecondStarResult result2
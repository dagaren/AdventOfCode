module AoC2

open Utils

type Command =
    | Forward of int
    | Down of int
    | Up of int

let aoc2 () =
        printBanner 2

        let input = readInputFile "AoC2.txt"

        printfn "Input size: %d commands" (input |> Array.length)

        let commands = 
            input 
            |> Array.map (fun x ->
                let sp =  x.Split [|' '|]
                (sp[0], int(sp[1])))
            |> Array.map (fun (command, value) ->
                match command with 
                 | "forward" -> Forward value
                 | "down" -> Down value
                 | "up" -> Up value)

        let applyCommand (x, y) command = 
            match command with
             | Forward v -> (x + v, y) 
             | Down v -> (x, y + v)
             | Up v -> (x, y - v)

        let (fPos, fDepth) =  commands |> Array.fold applyCommand (0, 0)
                
        let result1 = sprintf "Final position %d, final depth: %d, multiplication: %d" fPos fDepth (fPos * fDepth)
        printFirstStarResult result1
        
        // Part2

        let applyExtendedCommand (x, y, z) command = 
                   match command with
                    | Forward v -> (x + v, y + (z * v), z) 
                    | Down v -> (x, y, z + v)
                    | Up v -> (x, y, z - v)

        let (fEPos, fEDepth, fEAim) = commands |> Array.fold applyExtendedCommand (0, 0, 0)
                       
        let result2 = sprintf "Final position %d, final depth: %d, final aim: %d, multiplication: %d" fEPos fEDepth fEAim (fEPos * fEDepth)
        printSecondStarResult result2

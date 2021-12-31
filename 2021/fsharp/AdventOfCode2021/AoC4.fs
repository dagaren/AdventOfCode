module AoC4

open Utils
open FParsec

type InputLine = 
| EmptyLine
| BoardLine of int list

let aoc4 () =
        printBanner 4

        let inputLines = readInputFile "AoC4.txt"

        let numbers = inputLines |> Seq.take 1 |> Seq.map (split ',') |> Seq.head |> Array.map int |> Array.toList
        
        let boardLines = inputLines |> Seq.skip 2 |> Seq.toList

        let pEmptyLine = eof >>% EmptyLine
        let pBoardLine = spaces >>. sepBy pint32 spaces1 |>> BoardLine

        let pLine = choice [
            (attempt pEmptyLine)
            (attempt pBoardLine)
        ]

        let parseLine str = 
            match run  pLine str with
            | Success(result, _, _)   -> Some result
            | Failure(errorMsg, _, _) -> 
                 printfn "Invalid line: %s" errorMsg 
                 None

        let tryParseLine str : InputLine =
            match parseLine str with
            | Some command -> command
            | None -> failwith "Invalid line"


        let parseBoardLines lines = 
            let processLine (boardIndex, boards) line = 
                match line with 
                | EmptyLine -> (boardIndex + 1, boards)
                | BoardLine numbers ->
                    let currentBoard = boards |> Map.tryFind boardIndex |> Option.defaultValue []
                    let newBoard = currentBoard @ numbers
                    let newBoards = boards |> Map.add boardIndex newBoard

                    (boardIndex, newBoards)
                    

            let initialState = (0, Map.empty<int,int list>)

            let (_, boards) =
                lines
                |> List.map tryParseLine
                |> List.fold processLine initialState

            boards |> Map.toList |> List.map snd

        let boards = parseBoardLines boardLines

        let reverseBoard board = board 
                                    |> List.mapi (fun i a -> (i, a)) 
                                    |> List.groupBy (fun (i,a) -> i % 5) 
                                    |> List.map (fun (c, elements) -> (elements |> List.map( fun (i, e) -> e)))
                                    |> List.concat

        let checkBoardRowSolved board givenNumbers = 
            board |> List.chunkBySize 5 |> List.exists (fun x -> 0 = (x |> List.except givenNumbers |> List.length) )

        let checkBoardSolved board givenNumbers =
            (checkBoardRowSolved board givenNumbers) || (checkBoardRowSolved (reverseBoard board) givenNumbers)

        let rec solveBoard board pendingNumbers givenNumbers = 
            match pendingNumbers with 
            | [] -> failwith "Board not solved with given numbers"
            | currentNumber :: nextPendingNumbers ->
                let nextGivenNumbers = currentNumber :: givenNumbers

                let solved = checkBoardSolved board nextGivenNumbers

                if solved = false then
                    solveBoard board nextPendingNumbers nextGivenNumbers
                else
                    let numPendingNumbers = nextPendingNumbers |> List.length
                    let notMarkedNumbers = board |> List.except nextGivenNumbers
                   
                    (numPendingNumbers, currentNumber, notMarkedNumbers)

        let (numPendingNumbers, winningNumber, notMarkedNumbers) = 
            boards
                |> List.map (fun x -> solveBoard x numbers [])
                |> List.sortByDescending (fun (x, y, z) -> x)
                |> List.head

        let resultNumber = winningNumber * (notMarkedNumbers |> List.sum)

        let result1 = sprintf "Num pending numbers %d, WinningNumber: %d, PendingNumbers: %A, Result: %d " numPendingNumbers  winningNumber notMarkedNumbers resultNumber
        printFirstStarResult result1

        // Part 2

        let (numPendingNumbers2, losingNumber, notMarkedNumbers2) = 
            boards
                |> List.map (fun x -> solveBoard x numbers [])
                |> List.sortBy (fun (x, y, z) -> x)
                |> List.head

        let resultNumber2 = losingNumber * (notMarkedNumbers2 |> List.sum)

        let result2 = sprintf "Num pending numbers %d, LosingNumber: %d, PendingNumbers: %A, Result: %d " numPendingNumbers2  losingNumber notMarkedNumbers2 resultNumber2
        printSecondStarResult result2
        


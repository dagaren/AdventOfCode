module AoC2

open FParsec
open Utils

type Colors = Green | Red  | Blue

type Cube = {
    color: Colors
    number: int
}

let pGreen = pstring "green" >>% Green
let pRed = pstring "red" >>% Red
let pBlue = pstring "blue" >>% Blue

let pColor = choice [
    attempt pGreen
    attempt pRed
    attempt pBlue
    ]

let createCube number color = {
    number = number
    color = color
}

let pCube = pipe2 (spaces1 >>.pint32) (spaces1 >>. pColor) createCube
let pSubset = sepBy1 pCube (pchar ',')
let pGame = sepBy1 pSubset (pchar ';')
let pLine = pstring "Game " >>. pint32 .>> pstring ":" .>>. pGame

let parseLine text =
    let r = run pLine text
    match r with 
    | Success (result, _, _) -> Result.Ok result
    | _ -> Result.Error "Not valid line"

let isSubsetValid x = 
    x |> List.forall (fun cube -> 
                            match cube.color with
                            | Green -> cube.number <= 13
                            | Red -> cube.number <= 12
                            | Blue -> cube.number <= 14)

let parseLines inputLines = 
    let result = 
        inputLines
        |> List.map (fun x -> parseLine x)

    match List.exists Result.isError result with
    | true -> Result.Error "One or mole lines bad formatted"
    | false -> Result.Ok (result |> List.map (Result.defaultValue (0, [])))

let orderSubset x =
    let red = x |> List.tryFind (fun x -> x.color = Red)
    let green = x |> List.tryFind (fun x -> x.color = Green)
    let blue = x |> List.tryFind( fun x -> x.color = Blue)
    let numberRed = match red with
                    | Some x -> x.number
                    | None -> 0
    let numberGreen = match green with
                        | Some x -> x.number
                        | None -> 0
    let numberBlue = match blue with 
                        | Some x -> x.number
                        | None -> 0
    (numberRed, numberGreen, numberBlue)

let maintainMax (x:int, y:int , z:int) (a:int, b:int , c:int) =
    ((if x > a then x else a), (if y > b then y else b), (if z > c then z else c))

let calculateFirstStar lines = 
    lines
    |> List.filter(fun (_, y)  -> y |> List.forall isSubsetValid )
    |> List.map( fun (x, _) -> x)
    |> List.sum

let calculateSecondStar lines = 
    lines
    |> List.map (fun (x, y) -> y)
    |> List.map ( fun x -> x |> List.map orderSubset)
    |> List.map (fun x -> x |> List.reduce maintainMax)
    |> List.map (fun (x, y, z) -> x*y*z)
    |> List.sum

let aoc2 () =
    printBanner 2

    let linesResult = 
        readInputFile "AoC2.txt"
        |> Array.toList
        |> parseLines

    match linesResult with
    | Result.Ok lines -> 
        printFirstStarResult (calculateFirstStar lines |> string)
        printSecondStarResult (calculateSecondStar lines |> string)
    | Result.Error msg -> printfn "Error parsing input: %s" msg

    ()
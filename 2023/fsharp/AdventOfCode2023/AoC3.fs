module AoC3

open Utils
open FParsec

type PositionedNumber = { Value: int; Position: Position }

type Element = 
        | Number of uint
        | Symbol of char
        | Period
        | NewLine

module parsing = 
    open FParsec

    let notDigitOrPeriodOrNewLine = fun c -> not (System.Char.IsDigit(c) || c = '.' || c = '\n')
    let pPeriod = (many1 (pchar '.')) >>% Period
    let pNewLine = pchar '\n' >>% NewLine
    let pNumber = puint32 |>> Number
    let pSymbol = satisfy notDigitOrPeriodOrNewLine |>> Symbol
    let pElement = pNumber <|> pSymbol <|> pNewLine <|> pPeriod
    let pPositionedElement = getPosition >>= fun pos -> (pElement |>> (fun value -> (pos, value)))
    let pInput = many pPositionedElement

    let parseInput text =
        let r = run pInput text
        match r with 
        | Success (result, _, _) -> Result.Ok result
        | _ -> Result.Error "Not valid input"

let isValid (lines:int64) (columns:int64) (line:int64, column:int64) = 
    line > 0 && column > 0 && line <= lines && column <= columns

let getAdjacents (columns:int64)  (lines: int64) (line: int64, column: int64) =
    [ (line + 1L, column);
      (line - 1L, column);
      (line, column + 1L);
      (line, column - 1L);
      (line - 1L, column - 1L);
      (line + 1L, column + 1L);
      (line + 1L, column - 1L);
      (line - 1L, column + 1L)] |> List.filter (isValid lines columns)

let numberTouchesPositions positions ((line:int64, column:int64), number) =
    let numberLength = number |> string |> String.length

    let numberPositions = [ for i in 0 .. (numberLength - 1) do (line, column + (int64 i)) ]

    numberPositions |> List.exists (fun (x, y) -> positions |> List.contains (x, y))


let numberTouchesGear (lines, columns) (gearLine: int64, gearColumn:int64) ((numberLine: int64, numberColumn: int64), number: uint) : bool=
    let numberLength = number |> string |> String.length

    let numberPositions = [ for i in 0 .. (numberLength - 1) do (numberLine, numberColumn + (int64 i)) ]

    let gearPositions = getAdjacents columns lines (gearLine, gearColumn)

    Set.intersect (Set.ofList numberPositions) (Set.ofList gearPositions)
        |> Set.isEmpty
        |> not

let getGearNumbers (lines, columns) numbers (line, column) =
    numbers 
    |> List.filter (fun positionedNumber -> (numberTouchesGear (lines, columns) (line, column) positionedNumber))
    |> List.map (fun ((x, y), number) -> number)

let chooseNumber (position, element) = 
    match element with 
    | Number number-> Some(position, number)
    | _ -> None

let chooseSymbolPosition (position, element) =
    match element with
    | Symbol _ -> Some(position)
    | _ -> None

let chooseGearPosition (position, element) =
    match element with
    | Symbol character when character = '*' -> Some(position)
    | _ -> None

let aoc3 () =
    printBanner 3

    let inputText = readInputFileText "AoC3.txt" 

    let result = parsing.parseInput inputText

    match result with
    | Result.Ok positionedNumbers -> 
        let columns = (positionedNumbers |> List.map (fun (x, _) -> x.Column) |> List.max) - 1L
        let lines = positionedNumbers |> List.map (fun (x, _) -> x.Line) |> List.max

        let elements = 
            positionedNumbers 
            |> List.map (fun (pos, element) -> ((pos.Line, pos.Column), element))
        let symbolPositions = elements |> List.choose chooseSymbolPosition
        let numbers = elements |> List.choose chooseNumber

        let symbolAffectedPositions = 
            symbolPositions 
            |> List.collect (fun (line, column) -> getAdjacents columns lines (line, column))

        let total = 
            numbers
                |> List.filter (numberTouchesPositions symbolAffectedPositions)
                |> List.map(fun ((_, _), number) -> number)
                |> List.sum

        printFirstStarResult (sprintf "Result %d" total)

        let gears = elements |> List.choose chooseGearPosition

        let total2 =
            gears 
                |> List.map (getGearNumbers (lines, columns) numbers)
                |> List.filter (fun x -> x|> List.length = 2)
                |> List.map(fun x -> x |> List.reduce (fun x y -> x * y))
                |> List.sum

        printSecondStarResult (sprintf "Result %d" total2)

        ()
    | Result.Error msg -> printfn "Error parsing input: %s" msg

    ()
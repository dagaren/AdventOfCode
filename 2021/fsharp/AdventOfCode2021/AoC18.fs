module AoC18

open FParsec
open Utils

type SimpleNumber = int32

type SnailfishNumber = SnailfishElement * SnailfishElement
and SnailfishElement =
    | Simple of SimpleNumber
    | Recursive of SnailfishNumber

type ReductionResult = 
| Reduced of SnailfishElement * (int option * int option)
| NonReduced

let aoc18 () = 
    printBanner 18

    let input = readInputFile "Aoc18.txt" |> Array.toList

    let pSnailfishElement, pSnailfishElementRef = createParserForwardedToRef<SnailfishElement, unit>()

    let pSnailfishNumber = pstring "[" >>. pSnailfishElement .>> pstring "," .>>. pSnailfishElement .>> pstring "]" |>> SnailfishNumber

    let pSimple = pint32 |>> Simple
    let pRecursive = pSnailfishNumber |>> Recursive

    do pSnailfishElementRef := choice [ (attempt pSimple); (attempt pRecursive) ]

    let parseSnailfishNumber str = 
        match run  pSnailfishNumber str with
        | Success(result, _, _)   -> Some result
        | Failure(errorMsg, _, _) -> 
             printfn "Invalid number: %s" errorMsg 
             None

    let tryParseSnailfishNumber str =
        match parseSnailfishNumber str with
        | Some number -> number
        | None -> failwith "Invalid number found"

    let rec elementToText element = 
        match element with
        | Simple n -> sprintf "%d" n
        | Recursive r -> numberToText r
    and numberToText ((left, right): SnailfishNumber) = sprintf "[%s,%s]" (elementToText left) (elementToText right)

    let add sfn1 sfn2 = SnailfishNumber (Recursive sfn1, Recursive sfn2)

    let splitSimple (s:SimpleNumber) =
        let half = s / 2 
        let remainder = s % 2

        let left = half
        let right = if remainder = 0 then half else half + 1

        SnailfishNumber (Simple left, Simple right)

    let rec reduceSplitElement element = 
        match element with
        | Simple n -> 
            if n >= 10 then 
                Recursive(splitSimple n) 
            else Simple n 
        | Recursive r -> Recursive (reduceSplitNumber r)
    and reduceSplitNumber ((left, right): SnailfishNumber) = 
        let reducedLeft = reduceSplitElement left

        if reducedLeft = left then
            let reducedRight = reduceSplitElement right

            SnailfishNumber (left, reducedRight)
        else
            SnailfishNumber (reducedLeft, right)
 
    let rec incrementRightValue element incValue =
        match element with
        | Simple n -> Simple (n + incValue)
        | Recursive (left, right) ->
            Recursive (left, (incrementRightValue right incValue))

    let rec incrementLeftValue element incValue = 
        match element with
        | Simple n -> Simple (n + incValue)
        | Recursive (left, right) ->
            Recursive ((incrementLeftValue left incValue), right)

    let rec reduceExplodeElement element level = 
        match element with
        | Simple n -> 
            NonReduced
        | Recursive (left, right) ->
            match left, right, level with
            | Simple numberLeft, Simple numberRight, 4 -> 
                Reduced ((Simple 0), (Some numberLeft, Some numberRight))
            | left, right, _ ->
                let reducedLeft = reduceExplodeElement left (level + 1)

                match reducedLeft with
                | Reduced (lElement, (eWaveLeft, eWaveRight)) -> 
                    let newRight = match eWaveRight with
                                    | None -> right
                                    | Some incValue -> incrementLeftValue right incValue

                    Reduced (Recursive (lElement, newRight), (eWaveLeft, None))
                | NonReduced ->
                    let reducedRight = reduceExplodeElement right (level + 1)
                    match reducedRight with
                    | NonReduced -> NonReduced
                    | Reduced (rElement, (eWaveLeft, eWaveRight)) -> 
                        let newLeft = match eWaveLeft with
                                        | None -> left
                                        | Some incValue -> incrementRightValue left incValue
                        Reduced (Recursive (newLeft, rElement), (None, eWaveRight))
                

    let reduceExplodeNumber (number:SnailfishNumber) = 
        let reduced = reduceExplodeElement (Recursive number) 0

        match reduced with
        | NonReduced -> number
        | Reduced (rElement, eWave) ->
            match rElement with
            | Simple s -> failwith "Reduced to single element"
            | Recursive number -> number

    let rec reduceNumber (number:SnailfishNumber) =
        let explodeReduction = reduceExplodeNumber number

        if explodeReduction = number then
            let splitReduction = reduceSplitNumber number

            if splitReduction = number then
                number
            else
                reduceNumber splitReduction
        else
            reduceNumber explodeReduction

    let addReduce sfn1 sfn2 =  add sfn1 sfn2 |> reduceNumber
    
    let reduceNumbers numbers = numbers |> List.reduce addReduce

    let rec getMagnitude ((left, right): SnailfishNumber) = 3 * (getElementMagnitude left) + 2 * (getElementMagnitude right)
    and getElementMagnitude element = 
        match element with
               | Simple n -> n
               | Recursive r -> getMagnitude r
       

    let reduced = reduceNumbers (input |> List.map tryParseSnailfishNumber) 
    let result1 = sprintf "Magnitude: %d" (getMagnitude reduced)
    printFirstStarResult result1

    // Part2.

    let rec getMaxMagnitude numbers currentMax = 
        match numbers with
        | [ element ] -> currentMax
        | first :: remaining ->
            let newMax = remaining 
                            |> List.collect (fun number -> [ (getMagnitude (addReduce first number)); (getMagnitude (addReduce number first)) ])
                            |> List.max

            let nextMax = if newMax > currentMax then newMax else currentMax

            getMaxMagnitude remaining nextMax

    let maxMagnitude = getMaxMagnitude (input |> List.map tryParseSnailfishNumber) 0
    let result2 = sprintf "Max Magnitude: %d" maxMagnitude
    printSecondStarResult result2

        


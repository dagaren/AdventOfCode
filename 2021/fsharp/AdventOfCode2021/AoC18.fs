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

    let input = [
        "[5,[7,[8,4]]]";
        "[[[4,1],[6,[9,3]]],[[7,4],[5,[7,0]]]]";
        "[[6,2],[[[8,6],[5,5]],0]]";
        "[[[5,9],[3,[4,2]]],[[[1,2],0],2]]";
        "[[[[4,3],2],0],[[[1,7],[1,2]],[[8,2],[6,7]]]]";
        "[[[[0,1],9],3],[[4,7],[7,8]]]";
        "[[[[8,7],4],[5,[9,2]]],[[8,[9,6]],[1,8]]]";
        "[[[2,3],[[9,9],[7,0]]],[6,7]]";
        "[8,[[9,9],[8,6]]]";
        "[[[[5,7],[7,1]],[3,[7,6]]],[2,[[5,5],[8,3]]]]";
        "[[[7,0],2],[[[2,2],7],[6,[2,9]]]]";
        "[[6,2],[[0,8],8]]";
        "[[[[2,9],4],9],[1,[[6,9],[7,5]]]]";
        "[[[9,3],[[5,7],[3,1]]],[5,[6,[7,8]]]]";
        "[0,[[8,9],1]]";
        "[[4,[[4,3],4]],[7,[[4,0],0]]]";
        "[[0,[[1,9],[6,1]]],[[[7,0],[5,2]],[[3,8],[0,4]]]]";
        "[[[2,7],[7,[1,6]]],[6,[[8,7],[8,5]]]]";
        "[[9,5],[[1,[2,5]],[8,[2,0]]]]";
        "[6,[[8,[9,4]],[9,8]]]";
        "[[[[2,0],[4,6]],3],[[8,0],4]]";
        "[[[8,8],[[5,7],[5,6]]],5]";
        "[[5,[[7,9],9]],[1,6]]";
        "[[[[5,2],[4,9]],[[1,9],[2,9]]],[[[6,8],[7,5]],[[0,2],4]]]";
        "[1,[5,[[5,5],[1,2]]]]";
        "[[[1,4],[[0,3],7]],[[[9,1],9],[[2,3],7]]]";
        "[[[[6,4],[4,0]],[[3,4],[7,0]]],[[8,7],[5,[0,6]]]]";
        "[[3,[8,[2,8]]],[9,[0,[5,2]]]]";
        "[[7,[[1,8],1]],[6,[6,6]]]";
        "[[[3,[9,4]],[[3,2],[5,2]]],8]";
        "[3,[[4,[4,3]],[5,[9,2]]]]";
        "[[[1,8],[2,[7,5]]],[[0,[8,1]],[2,0]]]";
        "[1,3]";
        "[7,[[[9,6],[8,4]],9]]";
        "[6,4]";
        "[[[8,9],[[3,7],2]],[4,[[5,0],8]]]";
        "[[[[1,8],[7,9]],0],[[[4,4],3],[4,[1,7]]]]";
        "[[[[2,2],[0,9]],[1,2]],[[[9,1],[0,0]],[[1,6],4]]]";
        "[[[[8,1],6],[[3,3],[6,7]]],[[2,3],5]]";
        "[[[[9,0],7],6],[[[3,6],[6,7]],3]]";
        "[[[[1,0],6],[5,[0,0]]],[[[9,7],7],5]]";
        "[[[[5,1],4],[[7,7],[6,2]]],[[0,[6,0]],2]]";
        "[[[[8,3],[0,4]],[[9,9],[3,7]]],[[[2,7],[2,9]],[[2,0],[4,7]]]]";
        "[6,[[[4,8],0],8]]";
        "[[[6,[5,9]],[[0,3],9]],[[[2,5],[9,5]],0]]";
        "[[1,4],[6,[0,[6,2]]]]";
        "[9,[[[3,7],1],7]]";
        "[[[2,3],[[1,2],1]],[[[2,6],[0,1]],[0,[4,1]]]]";
        "[[[0,1],[[0,3],[7,3]]],[[8,7],3]]";
        "[[0,[[1,5],[5,3]]],4]";
        "[[[5,3],[[5,8],6]],[[[6,0],3],[4,1]]]";
        "[8,3]";
        "[[[[5,5],[3,0]],6],[[7,5],[2,[9,4]]]]";
        "[[[3,[3,3]],[[4,7],4]],[[2,0],1]]";
        "[[[0,[2,8]],[4,[7,9]]],[[[5,4],2],2]]";
        "[[3,[7,[1,8]]],[5,[[8,2],0]]]";
        "[[1,9],[[6,[5,9]],8]]";
        "[[5,[5,2]],5]";
        "[[[1,1],[4,3]],1]";
        "[[[[6,9],[4,1]],0],[[[3,0],6],7]]";
        "[[9,[[7,3],6]],[[[7,2],0],[9,9]]]";
        "[[5,4],[[[6,0],[5,1]],7]]";
        "[[[4,0],0],[[[2,6],[4,4]],[[6,8],2]]]";
        "[[[9,6],8],[[0,[9,5]],9]]";
        "[[6,[2,5]],[[[1,8],[9,0]],[[4,0],[5,7]]]]";
        "[5,[[8,[9,9]],[5,[6,8]]]]";
        "[[[7,[9,0]],5],6]";
        "[[9,[[3,7],[3,0]]],[[[7,2],[5,7]],[[0,5],[7,4]]]]";
        "[[7,3],[[6,5],[9,4]]]";
        "[[4,[4,3]],[9,[[2,6],0]]]";
        "[[[6,[0,1]],9],[[7,[3,2]],[[0,1],[5,2]]]]";
        "[[5,[0,[3,1]]],[[[1,1],[8,9]],[[6,3],[0,9]]]]";
        "[[[[2,8],0],[[8,7],4]],[[[9,6],3],[[7,8],[2,3]]]]";
        "[[[[1,0],1],4],[4,9]]";
        "[[[7,8],5],[[[3,7],[5,7]],6]]";
        "[[[8,[7,4]],[[1,6],[6,7]]],[2,4]]";
        "[[7,8],3]";
        "[[0,[4,[3,8]]],[[[1,0],1],6]]";
        "[[[[6,3],7],2],[[4,5],6]]";
        "[[[5,9],[[1,8],1]],[[[1,8],8],[[6,4],0]]]";
        "[[3,[8,[2,8]]],[[[2,8],[4,4]],9]]";
        "[7,[5,[[3,3],3]]]";
        "[3,[1,[0,[3,0]]]]";
        "[[[1,2],4],[9,[[7,1],[5,4]]]]";
        "[[[5,8],[7,[0,7]]],[0,[[2,9],8]]]";
        "[[[7,[2,0]],[1,[4,3]]],[0,[[1,1],[2,0]]]]";
        "[[[2,[2,5]],[4,1]],[0,[6,0]]]";
        "[[[8,3],9],[[[4,3],[5,8]],[[7,0],9]]]";
        "[2,[1,4]]";
        "[[[3,[2,6]],6],[[[3,2],[0,8]],[[3,5],[6,4]]]]";
        "[[[1,[3,3]],[[0,8],[1,3]]],[8,[[3,8],[0,8]]]]";
        "[[[[1,5],[0,1]],3],[[6,[1,7]],[4,7]]]";
        "[[4,[5,7]],[6,[[6,2],7]]]";
        "[[[[7,4],[3,1]],[5,6]],[0,[6,5]]]";
        "[[[7,[0,0]],6],[5,[[0,0],[3,5]]]]";
        "[[[[8,7],[5,8]],[8,[9,3]]],[[7,0],[[7,2],0]]]";
        "[[[7,[4,2]],0],[[[4,0],1],3]]";
        "[[[6,3],[9,[2,2]]],[[0,8],[1,2]]]";
        "[3,[[3,1],[[7,1],1]]]";
        "[[3,[[4,0],7]],[[[4,6],[2,3]],[[0,2],[1,8]]]]"
    ]

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

        


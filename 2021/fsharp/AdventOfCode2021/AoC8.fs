module AoC8

open Utils

let aoc8 () =
    printBanner 8

    let input = readInputFile "AoC8.txt" |> Array.toList

    let entries = 
        input 
            |> List.map (fun x -> let r = x.Split " | " in (r[0],r[1]))
            |> List.map (fun (x, y) -> (x.Split ' ' |> List.ofArray, y.Split ' ' |> List.ofArray))

    let numberSegments = Map.ofList [
        (1, 2);
        (4, 4);
        (7, 3);
        (8, 7)
    ]

    let filterElement (element:string) = 
        numberSegments
        |> Map.toList
        |> List.map (fun (x,y) -> y)
        |> List.contains element.Length

    let numExpectedDigits =
        entries
            |> List.map (fun (x, y) -> y)
            |> List.collect (fun elements -> elements |> List.where filterElement)
            |> List.length

    let result1 = sprintf "Num of digits 1, 4, 7 and 8: %d" numExpectedDigits
    printFirstStarResult result1

    // Part 2

    let evaluateLength pattern expectedLength number = 
        match pattern |> Set.count with
        | l when l = expectedLength -> Some number
        | _ -> None 

    let evaluateIs1 pattern (knownPatterns:Map<int, Set<char>>) = evaluateLength pattern 2 1 

    let evaluateIs4 pattern (knownPatterns:Map<int, Set<char>>) = evaluateLength pattern 4 4

    let evaluateIs7 pattern (knownPatterns:Map<int, Set<char>>) = evaluateLength pattern 3 7 

    let evaluateIs8 pattern (knownPatterns:Map<int, Set<char>>) = evaluateLength pattern 7 8

    let evaluateIs3 (pattern:Set<char>) (knownPatterns:Map<int, Set<char>>)  =
        let onePatternOption = knownPatterns.TryFind 1
        match onePatternOption with
        | None -> 
            None // Rule cannot apply yet
        | Some onePattern -> 
            if pattern.Count = 5 && (Set.intersect pattern onePattern).Count = 2 then
                Some 3
            else
                None

    let evaluateIs9 (pattern:Set<char>) (knownPatterns:Map<int, Set<char>>)  =
        let threePatternOption = knownPatterns.TryFind 3
        match threePatternOption with
        | None -> None // Rule cannot apply yet
        | Some threePattern -> 
            match pattern.Count with
            | count when count = 6 ->
                if (Set.intersect pattern threePattern).Count = 5 then
                    Some 9
                else
                    None
            | _ -> None

    let evaluateIs0 (pattern:Set<char>) (knownPatterns:Map<int, Set<char>>) =
        let ninePatternOption = knownPatterns.TryFind 9
        let onePatternOption = knownPatterns.TryFind 1

        match (ninePatternOption, onePatternOption, pattern.Count) with
        | (Some _, Some onePattern, num) when num = 6 ->
            if (Set.intersect pattern onePattern).Count = 2 then
                Some 0
            else
                None
        | _ -> None 

    let evaluateIs6 (pattern:Set<char>) (knownPatterns:Map<int, Set<char>>) =
        let ninePatternOption = knownPatterns.TryFind 9
        let onePatternOption = knownPatterns.TryFind 1

        match (ninePatternOption, onePatternOption, pattern.Count) with
        | (Some _, Some onePattern, count) when count = 6 ->
            if (Set.intersect pattern onePattern).Count <> 2 then
                Some 6
            else
                None
        | _ -> None 
            
    
    let evaluateIs2 (pattern:Set<char>) (knownPatterns:Map<int, Set<char>>) =
        let sixPatternOption = knownPatterns.TryFind 6
        match (sixPatternOption, pattern.Count) with
        | (Some sixPattern, count) when count = 5 -> 
            if (Set.intersect sixPattern pattern).Count <> 5 then
                Some 2
            else
                None
        | _ -> None

    let evaluateIs5 (pattern:Set<char>) (knownPatterns:Map<int, Set<char>>) =
        let sixPatternOption = knownPatterns.TryFind 6
        match (sixPatternOption, pattern.Count) with
        | (Some sixPattern, count) when count = 5 -> 
            if (Set.intersect sixPattern pattern).Count = 5 then
                Some 5
            else
                None
        | _ -> None
    
    let rules = [
        evaluateIs0;
        evaluateIs1;
        evaluateIs2;
        evaluateIs3;
        evaluateIs4;
        evaluateIs5;
        evaluateIs6;
        evaluateIs7;
        evaluateIs8;
        evaluateIs9
    ]

    let rec evaluateRules (patterns:List<Set<char>>) rules (knownPatterns:Map<int,Set<char>>) =
        if patterns.Length = 0 then
            knownPatterns
        else
            let postKnown = 
              patterns
                |> List.fold (fun currentKnownPatterns pattern ->
                                rules |> List.fold (fun ckp rule -> 
                                                        let r = rule pattern ckp
                                                        match r with 
                                                        | Some number -> ckp |> Map.add number pattern
                                                        | None -> ckp
                                                        ) currentKnownPatterns) knownPatterns

            if postKnown.Count <> knownPatterns.Count then
                let pendingPatterns = patterns |> List.filter (fun x -> postKnown |> Map.toList |> List.map (fun (key, value) -> value) |> (List.contains x >> not) )
                evaluateRules pendingPatterns rules postKnown
            else
                knownPatterns

    
    let stringToCharSet str = str |> Seq.toList |> Set.ofList

    let calculateNumber patterns output = 
           let digitPatterns = patterns |> List.map stringToCharSet
           let outputDigits = output |> List.map stringToCharSet
           
           let mapping = evaluateRules digitPatterns rules (Map.empty<int,Set<char>>)

           let inverseMapping = mapping |> Map.toList |> List.map(fun (x, y) -> (y, x)) |> Map.ofList

           let numberText = outputDigits |> List.map (fun x -> string(inverseMapping[x])) |> List.fold (+) ""

           int(numberText)

    let number = 
        entries 
            |> List.map (fun (pattern, outputDigits) -> calculateNumber pattern outputDigits)
            |> List.sum

    let result2 = sprintf "Sum of all numbers: %d" number
    printSecondStarResult result2
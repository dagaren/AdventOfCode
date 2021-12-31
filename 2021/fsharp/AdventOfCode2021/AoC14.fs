module AoC14

open Utils

let aoc14 () =
    printBanner 14

    let input = readInputFile "AoC14.txt"

    let template = input |> Array.head
    let transformationLines = input |> Array.skipWhile isNotEmptyString |> Array.skip 1 |> Array.toList

    let insertionRules = 
        transformationLines
            |> List.map (fun x -> 
                           let r = x.Split([|" -> "|], System.StringSplitOptions.RemoveEmptyEntries)
                           (r[0], r[1]))
            |> Map.ofList

    let initialTemplate =
        template
            |> Seq.map string
            |> List.ofSeq
     
    let step (polymer:List<string>) (rules:Map<string,string>) =
        (polymer
         |> List.pairwise
         |> List.collect (fun (x, y) -> [ x; rules[(x + y)] ]))
        @ [ polymer|> List.last ]

    let run numSteps = 
        let finalPolymer = 
            [for i in 1..numSteps  -> i]
                |> List.fold (fun polymer iteration -> 
                                let r = step polymer insertionRules
                                r) initialTemplate
    
        let quantities = finalPolymer |> List.countBy id |> List.map (fun (x, y) -> (x, int64(y))) |> Map.ofList
        quantities  

    let incrementMap (x:Option<int64>) = 
        match x with
        | Some s -> Some (s + 1L)
        | None -> Some (1L)
    
    let rec run2 (rules:Map<string,string>) pending (state:Map<string,int64>) steps = 
        //printfn "-------- Penging: %A" pending
        match pending with
        | [] -> state
        | [ ((first, "$"), _) ] -> state |> Map.change first incrementMap
        | ((first, second), level) :: remaining ->
            match level with
            | l when l = steps -> 
                let nextState = state |> Map.change first incrementMap
                //printfn "State: %A, nextState: %A" state nextState
                run2 rules remaining nextState steps
            | _ ->
                //if level = 0 then
                    //printfn " - Processing element in first level (%s, %s)" first second
                let succesor1 = ((first,  rules[(first + second)]), level + 1)
                let succesor2 = ((rules[(first + second)],  second), level + 1)
                let nextPending = [ succesor1; succesor2 ] @ remaining     

                run2 rules nextPending state steps

    let execute rules numSteps template = 
        let initialPending = template @ ["$"] |> List.pairwise |> List.map (fun (x, y) -> ((x, y), 0))
        //printfn "=== Num elements in initial Template: %d. Num Steps: %d" (initialPending |> List.length) numSteps
        run2 rules initialPending Map.empty<string,int64> numSteps

    let executeAlt rules numSteps template = 
        let initialPending = template|> List.pairwise |> List.map (fun (x, y) -> ((x, y), 0))
        //printfn "=== Num elements in initial Template: %d. Num Steps: %d" (initialPending |> List.length) numSteps
        run2 rules initialPending Map.empty<string,int64> numSteps

    
    let generateCache (rules:Map<string,string>) steps = 
        let cacheKeys = rules |> Map.toList |> List.map (fun (x, y) -> x)
        
        let result = cacheKeys |> List.map (fun x -> ((x, steps), (executeAlt rules steps [string(x[0]); string(x[1])]))) |> Map.ofList
        result

    let quantities = execute insertionRules 10 initialTemplate
    let q = quantities |> Map.toList |> List.map snd
    let max2 = q |> List.max
    let min2 = q |> List.min
    let result1 = sprintf "Min: %d, Max: %d, Substraction: %d" min2 max2 (max2-min2)
    printFirstStarResult result1

    let mergeMapValues (value:int64) (x:Option<int64>) = 
        match x with
        | Some s -> Some (s + value)
        | None -> Some (value)

    let mergeMaps (m1: Map<string,int64>) (m2:Map<string,int64>) =
        Map.fold (fun state key value -> Map.change key (mergeMapValues value) state) m1 m2

    let rec run3 (cache:Map<(string*int),Map<string,int64>>) (rules:Map<string,string>) pending (state:Map<string,int64>) steps = 
           match pending with
           | [] -> state
           | [ ((first, "$"), _) ] -> state |> Map.change first incrementMap
           | ((first, second), level) :: remaining ->
               if cache.ContainsKey ((first+second),steps - level) then
                   let newState = mergeMaps state cache[((first+second),steps - level)]
                   run3 cache rules remaining newState steps
               else
                   match level with
                   | l when l = steps -> 
                       let nextState = state |> Map.change first incrementMap
                       run3 cache rules remaining nextState steps
                   | _ ->
                       if level = 0 then
                           printfn " - Processing element in first level (%s, %s)" first second
                       let succesor1 = ((first,  rules[(first + second)]), level + 1)
                       let succesor2 = ((rules[(first + second)],  second), level + 1)
                       let nextPending = [ succesor1; succesor2 ] @ remaining     

                       run3 cache rules nextPending state steps
        

    printfn "Generating cache..."
    let cache = generateCache insertionRules 20
    printfn "Done."

    let execute2 cache rules numSteps template = 
        let initialPending = template @ ["$"] |> List.pairwise |> List.map (fun (x, y) -> ((x, y), 0))
        run3 cache rules initialPending Map.empty<string,int64> numSteps


    let quantities2 = execute2 cache insertionRules 40 initialTemplate

    let q2 = quantities2 |> Map.toList |> List.map (fun (x, y) -> y)
    let max2 = q2 |> List.max
    let min2 = q2 |> List.min

    let result2 = sprintf "Min: %d, Max: %d, Substraction: %d" min2 max2 (max2-min2)
    printSecondStarResult result2
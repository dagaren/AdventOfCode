module AoC19

open FParsec
open Utils

type InputLine = 
| EmptyLine
| ScannerLine of int
| PointLine of (int * int * int)

type Point = (int*int*int)

type Scanner = {
    name: string
    beacons: Point list
    scanners: Point list
}

let aoc19 () = 
    printBanner 19

    let inputList = readInputFile "Aoc19.txt" |> List.ofArray

    let pEmptyLine = eof >>% EmptyLine
    let pScannerLine = pstring "--- scanner " >>. pint32 |>> ScannerLine
    let pPointLine = pipe3 (pint32 .>> pstring ",") (pint32 .>> pstring ",") (pint32) (fun x y z -> PointLine (x, y, z))

    let pLine = choice [
        (attempt pEmptyLine);
        (attempt pScannerLine);
        (attempt pPointLine)
    ]

    let parseLine str = 
        match run  pLine str with
        | Success(result, _, _)   -> Some result
        | Failure(errorMsg, _, _) -> 
             printfn "Invalid instruction: %s" errorMsg 
             None

    let tryParseLine str : InputLine =
        match parseLine str with
        | Some command -> command
        | None -> failwith "Invalid instruction"

    let emptyScanner name = {
        name = name
        beacons = []
        scanners = [ (0, 0, 0) ]
    }

    let parseInput inputLines = 
        let processLine ((currentScanner:string option), scannersMap) line = 
            match line with 
            | EmptyLine -> (None, scannersMap)
            | ScannerLine number -> (Some ("[" + (string number) + "]"), scannersMap)
            | PointLine (x, y, z) ->
                match currentScanner with
                | None -> failwith "Invalid situation no scanner set"
                | Some scannerName ->
                    let currentScanner = scannersMap |> Map.tryFind scannerName |> Option.defaultValue (emptyScanner scannerName)

                    let newScanner = { currentScanner with beacons = currentScanner.beacons |> List.append [(x, y, z)] }
                    let newMap = scannersMap |> Map.add scannerName newScanner
                    (Some scannerName, newMap)

        let initialState = (None, Map.empty<string,Scanner>)
        
        let (_, scannersMap) = 
            inputLines 
            |> List.map tryParseLine
            |> List.fold processLine initialState

        scannersMap

    let inversePoint (x, y, z) = (-x, -y , -z) 

    let addPoint (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

    let permutationFunctions = 
        [
            (fun (x, y, z) -> (x, y, z));
            (fun (x, y, z) -> (x, z, y));
            (fun (x, y, z) -> (z, x, y));
            (fun (x, y, z) -> (y, x, z));
            (fun (x, y, z) -> (z, y, x));
            (fun (x, y, z) -> (y, z, x));
        ]

    let directionFunctions =
        [
            (fun (x, y, z) -> (x, y, z));
            (fun (x, y, z) -> (x, -y, z));
            (fun (x, y, z) -> (x, y, -z));
            (fun (x, y, z) -> (x, -y, -z));
            (fun (x, y, z) -> (-x, y, z));
            (fun (x, y, z) -> (-x, -y, z));
            (fun (x, y, z) -> (-x, y, -z));
            (fun (x, y, z) -> (-x, -y, -z));
        ]

    let orientationFunctions =
        [for f1 in permutationFunctions do for f2 in directionFunctions -> f1 >> f2]

    let tryMergeScanners scanner1 scanner2 =

        let relativeScannerPoint =
            List.allPairs scanner1.beacons scanner2.beacons 
            |> List.map (fun (beacon1, beacon2) -> (addPoint beacon1 (inversePoint beacon2)))
            |> List.countBy id
            |> List.where (fun (point, numTimes) -> numTimes >= 12)
            |> List.map fst
            |> List.tryHead
             
        match relativeScannerPoint with
        | None -> None
        | Some (x, y, z) ->
            let newBeacons = scanner1.beacons @ (scanner2.beacons |> List.map (addPoint (x, y, z))) |> List.distinct
            let newScanners = scanner1.scanners @ (scanner2.scanners |> List.map (addPoint (x, y, z))) |> List.distinct

            let newScanner = {
                name = scanner1.name + "-" + scanner2.name
                beacons = newBeacons
                scanners = newScanners
            }
            Some newScanner

    let getAlternativeScanner scanner orientationFunction = 
        { scanner with 
            beacons = (scanner.beacons |> List.map orientationFunction)
            scanners = (scanner.scanners |> List.map orientationFunction)}

    let getAlternativeScanners scanner = 
        orientationFunctions |> List.map (getAlternativeScanner scanner)

    let mergeScanners scanner1 scanner2 =
        let scanner2Alternatives = getAlternativeScanners scanner2

        scanner2Alternatives |> List.tryPick (tryMergeScanners scanner1)

    let rec pairCombinationsRec pending combinations = 
        match pending with 
        | [] -> combinations
        | first :: remaining ->
            let firstCombinations = remaining |> List.map (fun x -> (first, x))
            let nextCombinations = combinations @ firstCombinations

            pairCombinationsRec remaining nextCombinations

    let pairCombinations list = pairCombinationsRec list []

    let reduceScannersStep (scannersMap:Map<string,Scanner>) = 
        let names = scannersMap |> Map.toList |> List.map fst

        let combinations = pairCombinations names

        let merged = combinations |> List.tryPick (fun (x, y) -> mergeScanners scannersMap[x] scannersMap[y])

        match merged with 
        | None -> scannersMap
        | Some mergedScanner -> 
            let newMap = 
                scannersMap 
                |> Map.toList 
                |> List.filter (fun (x, _) -> (mergedScanner.name.Contains(x)) |> not) 
                |> List.append [ (mergedScanner.name, mergedScanner) ]
                |> Map.ofList
            newMap

    let rec reduceScanners (scannersMap:Map<string,Scanner>) =
        let reducedMap = reduceScannersStep scannersMap

        if reducedMap = scannersMap  then
            reducedMap
        else reduceScanners reducedMap

    let manhattanDistance (x1, y1, z1) (x2, y2, z2) = (abs (x1 - x2)) + (abs (y1 - y2)) + (abs (z1 - z2))
        
    let initialScanners = parseInput inputList
    
    let result = reduceScanners initialScanners

    if result.Count > 1 then
        printfn "Number of elements after reduction is not 1"
    else
        let finalScanner = result |> Map.toList |> List.head |> snd
        let numBeacons = finalScanner.beacons.Length

        let result1 = sprintf "Number of beacons: %d" numBeacons
        printFirstStarResult result1

        let combinations = pairCombinations finalScanner.scanners

        let distances = combinations |> List.map (fun (x, y) -> manhattanDistance x y)

        let maxDistance = distances |> List.max

        let result2 = sprintf "Max distance between two beacons = %d" maxDistance
        printSecondStarResult result2
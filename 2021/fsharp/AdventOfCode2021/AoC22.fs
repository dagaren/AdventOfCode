module AoC22

open FParsec
open Utils

type Range = int * int

type Axis = {
    name: string
    range: Range
}

type Cube = {
    rangex: Range
    rangey: Range
    rangez: Range
}
type Command = 
| SwitchOn of Cube
| SwitchOff of Cube

type State =
| On
| Off

let aoc22 () =
    printBanner 22

    let input = readInputFile "AoC22.txt" |> Array.toList

    let createCube xAxis yAxis zAxis = { rangex = xAxis; rangey = yAxis; rangez = zAxis}

    let pRange = pint32 .>> pstring ".." .>>. pint32 |>> Range

    let pAxis char = pchar char >>. pchar '=' >>. pRange 

    let pipeCubeAxes = pipe3 (pAxis 'x' .>> pstring ",")  (pAxis 'y' .>> pstring ",") (pAxis 'z') createCube

    let pCommandOn = pstring "on " >>. pipeCubeAxes |>> SwitchOn
    let pCommandOff = pstring "off " >>. pipeCubeAxes |>> SwitchOff

    let pCommand = choice [ (attempt pCommandOn); (attempt pCommandOff) ]

    let parseCommand str = 
        match run  pCommand str with
        | Success(result, _, _)   -> Some result
        | Failure(errorMsg, _, _) -> 
             printfn "Invalid command: %s" errorMsg 
             None

    let tryParseCommand str : Command =
        match parseCommand str with
        | Some command -> command
        | None -> failwith "Invalid command"

    let min x y = if x < y then x else y

    let axesIntersection (axe1:Range) (axe2:Range) =
        match axe1, axe2 with
        | (min1, max1), (min2, max2) when min1 >= min2 && min1 <= max2 -> Some (Range (min1, (min max1 max2)))
        | (min1, max1), (min2, max2) when min2 >= min1 && min2 <= max1 -> Some (Range (min2, (min max1 max2)))
        | _ -> None
    
    let getIntersection cube1 cube2 =
        let xintersection = axesIntersection cube1.rangex cube2.rangex
        let yintersection = axesIntersection cube1.rangey cube2.rangey
        let zintersection = axesIntersection cube1.rangez cube2.rangez
        match (xintersection, yintersection, zintersection) with
        | (Some rangex, Some rangey, Some rangez) -> 
            let intersectionCube = createCube rangex rangey rangez
            Some intersectionCube
        | _ -> None

    let initialCubeRegion = createCube (-50, 50) (-50, 50) (-50, 50)

    let commands = input |> List.map tryParseCommand

    let extract cube1 cube2 = 
        // Removes cube2 from cube1. returning cubes
        let intersection = getIntersection cube1 cube2
        match intersection with
        | None -> [ cube1 ]
        | Some intersectionCube ->
            // Y Axis cubes
            let (ymin1, ymax1) = cube1.rangey
            let (ymin2, ymax2) = intersectionCube.rangey 
        
            let cubeDown = 
                if (ymin2 - 1) >= ymin1 then
                    Some (createCube cube1.rangex (ymin1, ymin2 - 1) cube1.rangez)
                else
                    None

            let cubeUp = 
                if (ymax2 + 1) <= ymax1 then
                    Some (createCube cube1.rangex (ymax2 + 1, ymax1) cube1.rangez)
                else
                    None

            // X Axis cubes
            let (xmin1, xmax1) = cube1.rangex
            let (xmin2, xmax2) = intersectionCube.rangex
        
            let cubeLeft =
                if (xmin2 - 1) >= xmin1 then
                    Some (createCube (xmin1, xmin2 - 1) intersectionCube.rangey cube1.rangez)
                else
                    None
            let cubeRight = 
                if (xmax2 + 1) <= xmax1 then
                    Some (createCube (xmax2 + 1, xmax1) intersectionCube.rangey cube1.rangez)
                else
                    None

            // Z Axis cubes
            let (zmin1, zmax1) = cube1.rangez
            let (zmin2, zmax2) = intersectionCube.rangez
        
            let cubeFront = 
                if (zmin2 - 1) >= zmin1 then
                    Some (createCube intersectionCube.rangex intersectionCube.rangey (zmin1, zmin2 - 1))
                else
                    None
            let cubeBack =
                if (zmax2 + 1) <= zmax1 then
                    Some (createCube intersectionCube.rangex intersectionCube.rangey (zmax2 + 1, zmax1))
                else
                    None

            let cubes = [ cubeUp; cubeDown; cubeLeft; cubeRight; cubeFront; cubeBack] |> List.choose id

            cubes

    let getNumPoints cube =
        let (xmin, xmax) = cube.rangex
        let (ymin, ymax) = cube.rangey
        let (zmin, zmax) = cube.rangez

        int64(xmax - xmin + 1) * int64(ymax - ymin + 1) * int64(zmax - zmin + 1)

    let applyCommandCube currentCubes (command:Command) =
        match command with
        | SwitchOn cube ->
            match currentCubes with
            | [] -> [ cube ]
            | _ -> currentCubes |> List.collect (fun x -> extract x cube) |> List.append [ cube ]
        | SwitchOff cube ->
            currentCubes |> List.collect (fun x -> extract x cube)

    let applyCommands commands =      
        commands |> List.fold applyCommandCube []

    let finalCubes = commands |> applyCommands

    let intersected = finalCubes |> List.map (getIntersection initialCubeRegion) |> List.choose id

    let numPointsIntersected = intersected |> List.map getNumPoints |> List.sum
    let result1 = sprintf "Number of points inside initial region: %d" numPointsIntersected
    printFirstStarResult result1

    let numPoints = finalCubes |> List.map getNumPoints |> List.sum
    let result2 = sprintf "Number of points: %d" numPoints
    printSecondStarResult result2
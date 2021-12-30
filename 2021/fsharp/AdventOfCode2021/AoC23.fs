module AoC23

open Utils

type AmphipodType = 
| Amber
| Bronze
| Copper
| Desert

type Hallway = {
    elements: Option<AmphipodType> list
}

type Diagram = {
    roomSize: int
    rooms: Map<AmphipodType,AmphipodType list>
    hallway: Hallway
}

type Position =
| HallwayIndex of int
| Room of AmphipodType

type Move = {
    amphipodType: AmphipodType
    source: Position
    destination: Position
    energy: int
}

let aoc23 () =
    printBanner 23    

    let forbiddenHallwayIndexes = [2; 4; 6; 8]
    
    let stepEnergies = Map.ofList [
        (Amber, 1);
        (Bronze, 10);
        (Copper, 100);
        (Desert, 1000)
    ]
    
    let getHallayIndexForRoom kind = 
        match kind with
        | Amber -> 2
        | Bronze -> 4
        | Copper -> 6
        | Desert -> 8

    let getRoomFromEntryIndex index = 
           match index with
           | 2 -> Some Amber
           | 4 -> Some Bronze
           | 6 -> Some Copper
           | 8 -> Some Desert
           | _ -> None

    let emptyDiagram size =
        let empty = List.empty<AmphipodType>
        let rooms = [ (Amber, empty); (Bronze, empty); (Copper, empty); (Desert, empty) ] |> Map.ofList

        let hallway = { elements = (List.replicate<AmphipodType option> 11 None) }

        {
            roomSize = size
            rooms = rooms
            hallway = hallway
        }

    let addAmphipod kind position diagram =
        match position with
        | Room roomKind ->
            if diagram.rooms[roomKind].Length = diagram.roomSize then
                failwith "Adding amphipod to full room"
            
            let newRoom = kind :: diagram.rooms[roomKind]
           
            { diagram with rooms = diagram.rooms |> Map.add roomKind newRoom}

        | HallwayIndex index ->
            let newHallwayElements = diagram.hallway.elements |> List.updateAt index (Some kind)

            { diagram with hallway = { diagram.hallway with elements = newHallwayElements} }

    let removeAmphipod position diagram = 
        match position with
        | Room roomKind ->
            match diagram.rooms[roomKind] with
            | [] -> failwith "Removing amphipod from empty room"
            | _ :: newRoom ->
                { diagram with rooms = diagram.rooms |> Map.add roomKind newRoom}
        | HallwayIndex index ->
            let newHallwayElements = diagram.hallway.elements |> List.updateAt index None
            
            { diagram with hallway = { diagram.hallway with elements = newHallwayElements }}

    let getHallwayConnectedPositions index diagram =
        let leftIndexes = [for i in index-1..-1..0 -> i]
        let rightIndexes = [for i in index+1..10 -> i]

        let leftEmptyPositions =
            leftIndexes 
              |> List.takeWhile (fun i -> diagram.hallway.elements[i] = None)
              |> List.map (fun index -> HallwayIndex index)
        let rightEmptyPositions = 
            rightIndexes
              |> List.takeWhile (fun i -> diagram.hallway.elements[i] = None)
              |> List.map (fun index -> HallwayIndex index)

        let hallwayEmptyPositions = leftEmptyPositions @ rightEmptyPositions

        hallwayEmptyPositions @ (hallwayEmptyPositions 
            |> List.map (fun (HallwayIndex index) -> index )
            |> List.choose (fun index -> getRoomFromEntryIndex index)
            |> List.map (fun rkind -> Room rkind))
        

    let getConnectedPositions position diagram =
        match position with 
        | Room roomKind ->
            let roomIndex = getHallayIndexForRoom roomKind

            getHallwayConnectedPositions roomIndex diagram
                |> List.except [Room roomKind]
        | HallwayIndex index ->
            getHallwayConnectedPositions index diagram

    let positionIsRoom position  = 
        match position with
        | Room _ -> true
        | HallwayIndex _ -> false

    let positionIsRoomOfKind kind position = 
        match position with
        | Room roomKind when roomKind = kind -> true
        | _ -> false

    let positionIsHallwayOrRoom kind position = 
        match position with
        | Room roomKind when roomKind = kind -> true
        | HallwayIndex _ -> true
        | _ -> false

    let canEnterAmphipodInRoom kind diagram =
        diagram.rooms[kind].Length <= diagram.roomSize &&
            (diagram.rooms[kind] 
                |> List.filter (fun x -> x <> kind)
                |> List.length) = 0

    let roomHasExternalElements (room:AmphipodType list) (kind:AmphipodType) = room |> List.exists (fun x -> x <> kind)

    let isHallwayIndexAllowed index = forbiddenHallwayIndexes |> List.contains index |> not

    let getEnergy origin destination kind diagram =
        let originExtraSteps, originIndex = 
            match origin with 
            | Room roomKind -> (diagram.roomSize - diagram.rooms[roomKind].Length + 1, getHallayIndexForRoom roomKind)
            | HallwayIndex index -> (0, index)

        let destinationExtraSteps, destinationIndex = 
            match destination with 
            | Room roomKind -> (diagram.roomSize - diagram.rooms[roomKind].Length, getHallayIndexForRoom roomKind)
            | HallwayIndex index -> (0, index)

        let hallwaySteps = abs(originIndex - destinationIndex)
        let finalSteps = hallwaySteps + originExtraSteps + destinationExtraSteps

        finalSteps * stepEnergies[kind]

    let getMovesFromRoom diagram kind =
        match diagram.rooms[kind] with
        | [] -> []
        | firstKind :: _ ->
            let needsToBeMoved = firstKind <> kind || (roomHasExternalElements diagram.rooms[kind] kind)

            match needsToBeMoved with
            | false -> []
            | true ->
                let connectedPositions = 
                    getConnectedPositions (Room kind) diagram
                        |> List.filter (positionIsHallwayOrRoom firstKind)
                if ((connectedPositions |> List.contains (Room firstKind)) && (canEnterAmphipodInRoom firstKind diagram)) then
                    // return move to target room
                    [ { amphipodType = firstKind; source = Room kind; destination = Room firstKind ; energy = (getEnergy (Room kind) (Room firstKind) firstKind diagram) } ]
                else
                    // return moves to hallway
                    connectedPositions
                        |> List.filter (positionIsRoom >> not)
                        |> List.filter (fun (HallwayIndex i) -> isHallwayIndexAllowed i)
                        |> List.map (fun destination -> { amphipodType = firstKind; source = Room kind; destination = destination ; energy = (getEnergy (Room kind) destination firstKind diagram) } )

    let getMoveFromHallwayIndex diagram index =
        let amphipodOption = diagram.hallway.elements[index]
        match amphipodOption with
        | None -> failwith "Trying to get moves from empty hallway index"
        | Some amphipod ->
            let canBeMoved = getConnectedPositions (HallwayIndex index) diagram |> List.contains (Room amphipod)
            if canBeMoved && (canEnterAmphipodInRoom amphipod diagram) then
                Some { amphipodType = amphipod; source = HallwayIndex index; destination = Room amphipod ; energy = (getEnergy (HallwayIndex index) (Room amphipod) amphipod diagram) }
            else
                None
                

    let getMoves diagram = 
        let hallwayMoves = 
            diagram.hallway.elements
            |> List.mapi (fun index element -> (index, element))
            |> List.where (fun (index, element) -> Option.isSome element)
            |> List.map (fun (index, _) -> getMoveFromHallwayIndex diagram index)
            |> List.choose id

        let roomMoves = 
            [ Amber; Bronze; Copper; Desert]
            |> List.collect (fun kind -> getMovesFromRoom diagram kind)

        hallwayMoves @ roomMoves

    let applyMove diagram move = 
        diagram |> removeAmphipod move.source |> addAmphipod move.amphipodType move.destination

    let isSolved diagram =
        [ Amber; Bronze; Copper; Desert] 
            |> List.forall(fun roomKind -> 
                                diagram.rooms[roomKind].Length = diagram.roomSize && 
                                (diagram.rooms[roomKind] |> List.forall (fun kind -> roomKind = kind)) &&
                                (diagram.hallway.elements |> List.choose id |> List.length) = 0)

    let getPathEngergy path = 
        path |> List.map (fun move -> move.energy) |> List.sum

    let minimumEnergyPending diagram = 
        let minimumEnergyFromRooms = 
            [ Amber; Bronze; Copper; Desert ] 
            |> List.collect (fun roomKind -> diagram.rooms[roomKind] |> List.map (fun kind -> (roomKind, kind)))
            |> List.where (fun (roomKind, kind) -> roomKind <> kind)
            |> List.map (fun (roomKind, kind) -> 
                            let distance = abs((getHallayIndexForRoom roomKind) - (getHallayIndexForRoom kind))
                            stepEnergies[kind] * (distance + 2))
            |> List.sum

        let minimumEnergyFromHallway =  
            diagram.hallway.elements
            |> List.mapi (fun index element -> (index, element))
            |> List.where (fun (index, element) -> Option.isSome element)
            |> List.map (fun (index, elemento) -> (index, Option.get elemento))
            |> List.map (fun (index, kind) -> 
                             let distance = abs(index - (getHallayIndexForRoom kind))
                             stepEnergies[kind] * (distance + 1))
            |> List.sum

        minimumEnergyFromHallway + minimumEnergyFromRooms

     
    let rec solve diagram currentPath bestPathEnergy = 
        match (isSolved diagram, bestPathEnergy) with
        | true, _ -> Some currentPath
        | false, bpe when bpe <= ((getPathEngergy currentPath) + (minimumEnergyPending diagram)) ->
            None
        | _ ->
            let moves = getMoves diagram

            let bestPath, pathEnergy = 
                moves
                |> List.sortBy (fun move ->
                                    match move.destination with 
                                    | Room x -> 0
                                    | HallwayIndex _ -> move.energy)
                |> List.fold (fun (currentBestPath, bestEnergy) move -> 
                                    let newDiagram = applyMove diagram move
                                    let newPath = move :: currentPath
                                    let moveBestPathResult = solve newDiagram newPath bestEnergy
                                    match moveBestPathResult with
                                    | None -> (currentBestPath, bestEnergy)
                                    | Some moveBestPath ->
                                        let moveBestPathEnergy = getPathEngergy moveBestPath
                                        if moveBestPathEnergy < bestEnergy then
                                            (Some moveBestPath, moveBestPathEnergy)
                                        else
                                             (currentBestPath, bestEnergy)
                                    ) (None, bestPathEnergy)
            bestPath

    let solvePosition position = 
        solve position [] System.Int32.MaxValue

    let getKindChar kindOption = 
        match kindOption with
        | None -> "."
        | Some amphipod ->
             match amphipod with
             | Amber -> "A"
             | Bronze -> "B"
             | Copper -> "C"
             | Desert -> "D"

    let getAmphipodInRoomPosition diagram kind index =
        if index > diagram.roomSize then
            failwith "Getting element out of bound indexes"

        if index > (diagram.rooms[kind].Length - 1) then
            None
        else
           Some (diagram.rooms[kind][diagram.rooms[kind].Length - 1 - index])

    let printDiagram diagram = 
        printfn "#############"
        printfn "#%s#" ([for i in 0..10 -> i] |> List.map (fun x -> getKindChar (diagram.hallway.elements[x])) |> List.fold (+) "")

        let gc = getAmphipodInRoomPosition diagram

        for i in (diagram.roomSize - 1)..(-1)..0 do
            printfn "  #%s#%s#%s#%s#" (getKindChar (gc Amber i))  (getKindChar (gc Bronze i))  (getKindChar (gc Copper i))  (getKindChar (gc Desert i))

        printfn "  #########"

    ////

    let initialDiagram = 
        emptyDiagram 2
            |> addAmphipod Copper (Room Amber)
            |> addAmphipod Desert (Room Amber)
            |> addAmphipod Copper (Room Bronze)
            |> addAmphipod Amber (Room Bronze)
            |> addAmphipod Bronze (Room Copper)
            |> addAmphipod Amber (Room Copper)
            |> addAmphipod Bronze (Room Desert)
            |> addAmphipod Desert (Room Desert)

    printfn "First input"
    printDiagram initialDiagram

    let pathResult = solvePosition initialDiagram

    let result1 = 
        match pathResult with 
        | None -> "No solution found"
        | Some path ->
            let energy = getPathEngergy path
            sprintf "Path with minimum energy: %d" energy
    printFirstStarResult result1


    let initialDiagram2 =
        emptyDiagram 4
            |> addAmphipod Copper (Room Amber)
            |> addAmphipod Desert (Room Amber)
            |> addAmphipod Desert (Room Amber)
            |> addAmphipod Desert (Room Amber)
            |> addAmphipod Copper (Room Bronze)
            |> addAmphipod Bronze (Room Bronze)
            |> addAmphipod Copper (Room Bronze)
            |> addAmphipod Amber (Room Bronze)
            |> addAmphipod Bronze (Room Copper)
            |> addAmphipod Amber (Room Copper)
            |> addAmphipod Bronze (Room Copper)
            |> addAmphipod Amber (Room Copper)
            |> addAmphipod Bronze (Room Desert)
            |> addAmphipod Copper (Room Desert)
            |> addAmphipod Amber (Room Desert)
            |> addAmphipod Desert (Room Desert)

    printfn "Second input"
    printDiagram initialDiagram2

    let pathResult2 = solvePosition initialDiagram2
    
    let result2 =
        match pathResult2 with 
        | None -> "No solution found"
        | Some path ->
            let energy = getPathEngergy path
            sprintf "Path with minimum energy: %d" energy
    printSecondStarResult result2
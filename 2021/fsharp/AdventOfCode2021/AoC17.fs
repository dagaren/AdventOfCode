module AoC17

open Utils

type state = {
    x: int
    y: int
    vx: int
    vy: int
}

type targetZone = {
    points: (int*int) list
    minX: int
    maxX: int
    minY: int
    maxY: int
}

let aoc17 () = 
    printBanner 17

    let originX = 0
    let originY = 0

    let targetXRange = [192..251]
    let targetYRange = [-89..-59]

    let targetPoints = [ for i in targetXRange do for j in targetYRange -> (i, j)]

    let target = {
        points = targetPoints
        minX = targetXRange |> List.min
        minY = targetYRange |> List.min
        maxX = targetXRange |> List.max
        maxY = targetYRange |> List.max
    }

    let getMaxY path = path |> List.map (fun (x, y) -> y) |> List.max

    let pointMatchesTarget target (x, y) =
        x >= target.minX && x <= target.maxX && y >= target.minY && y <= target.maxY

    let pathMatchesTarget target path = 
        path |> List.exists (fun (x, y) -> pointMatchesTarget target (x, y))

    let step state =
        let nx = state.x + state.vx
        let ny = state.y + state.vy

        let nvx = match state.vx with
                    | 0 -> 0
                    | negative when negative < 0 -> state.vx + 1
                    | positive when positive > 0 -> state.vx - 1

        let nvy = state.vy - 1

        {
            x = nx
            y = ny
            vx = nvx
            vy = nvy
        }

    let findFirstMaxXPoint path =
        let (point, _) = path 
                        |> List.pairwise
                        |> List.find (fun ((x0, _), (x1, _)) -> x0 = x1)

        point

    let rec sim target (path:(int*int)list) state (previous:(int*int) option) = 
        let nextPath = path @ [ (state.x, state.y) ]

        if state.x > target.maxX || state.y < target.maxY then
            match previous with 
            | Some (px, _) when px = state.x-> 
                nextPath
            | _ ->
                let nextState = step state
                sim target nextPath nextState (Some ((state.x, state.y)))
        else
            let nextState = step state

            sim target nextPath nextState (Some ((state.x, state.y)))

    let rec simFixedVX target paths state = 
        if state.vy > ((target.minY * -1) + 2) then
            paths 
        else
            let startPath = List.empty<(int*int)>
            let currentPath = sim target startPath state None

            let (maxX, y) = findFirstMaxXPoint currentPath
            let newPaths = currentPath :: paths
            let newState = { state with vy = state.vy + 1 }
            simFixedVX target newPaths newState
            
    let maxHeight = 
        [ for vx in 1 .. target.maxX -> vx]
            |> List.map (fun vx -> { vx = vx; vy = target.minY; x = originX; y = originY })
            |> List.collect (fun state -> simFixedVX target (List.empty<(int * int) list>) state)
            |> List.filter (fun path -> path |> pathMatchesTarget target)
            |> List.map getMaxY
            |> List.sortByDescending id
            |> List.head

    let result1 = sprintf "Max height: %d" maxHeight
    printFirstStarResult result1

    let numInitialVelocitiesTargeting = 
        [ for vx in 1 .. target.maxX -> vx]
            |> List.map (fun vx -> { vx = vx; vy = target.minY; x = originX; y = originY })
            |> List.collect (fun state -> simFixedVX target (List.empty<(int * int) list>) state)
            |> List.filter (fun path -> path |> pathMatchesTarget target)
            |> List.length

    let result2 = sprintf "Num initial velocities targeting: %d" numInitialVelocitiesTargeting
    printSecondStarResult result2


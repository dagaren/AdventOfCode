module AoC21

open Utils

type player = {
    score: int
    position : int
}

type turn = 
| Player1
| Player2

type multiverseResult = {
    player1wins: int64
    player2wins: int64
}

let aoc21 () =
    printBanner 21

    let getDeterministiceDice numSides =
        let mutable sequence = Seq.initInfinite (fun index -> (index % numSides) + 1)

        let dice () =
            let head = sequence |> Seq.head
            let remaining = sequence |> Seq.skip 1

            sequence <- remaining
            head

        dice

    let initPlayer position = { score = 0; position = position } 

    let calculateNewPosition position movesForward = 
        let tmp = (position + movesForward) % 10
        if tmp = 0 then 10 else tmp

    let executeTurn dice player =
        let roll1 = dice ()
        let roll2 = dice ()
        let roll3 = dice ()
        let movesForward = roll1 + roll2 + roll3

        let newPosition =  calculateNewPosition player.position movesForward

        {
            position = newPosition
            score = player.score + newPosition
        }

    let rec executeGame limit dice player1 player2 turn numRolls = 
        if player1.score >= limit || player2.score >= limit then
            (player1, player2, numRolls)
        else
            match turn with 
            | Player1 ->
                let newPlayer1 = executeTurn dice player1
                executeGame limit dice newPlayer1 player2 Player2 (numRolls + 3)
            | Player2 ->
                let newPlayer2 = executeTurn dice player2
                executeGame limit dice player1 newPlayer2 Player1 (numRolls + 3)


    let dice = getDeterministiceDice 100

    let player1 = initPlayer 7
    let player2 = initPlayer 1

    let player1Final, player2Final, numRolls = executeGame 1000 dice player1 player2 Player1 0

    let losingScore = if player1Final.score < player2Final.score then player1Final.score else player2Final.score

    let result = losingScore * numRolls

    let result1 = sprintf "Result: %d" result
    printFirstStarResult result1
    
    let rec calculateOutcomesRec numDices numSides currentOutcomes = 
        if numDices = 0 then
            currentOutcomes
        else
            let newOutcomes = 
                match currentOutcomes with
                | [] -> [ for i in 1..numSides -> i ]
                | _ -> currentOutcomes |> List.collect (fun x -> [ for i in 1..numSides -> x + i] )

            calculateOutcomesRec (numDices - 1) numSides newOutcomes


    let calculateOutcomes numDices numSides = calculateOutcomesRec numDices numSides [] |> List.countBy id |> Map.ofList


    let out1 = calculateOutcomes 3 3

    printfn "Num outcomes: %A" out1

    let executeDeterministicTurn movesForward player =

        let newPosition =  calculateNewPosition player.position movesForward

        {
            position = newPosition
            score = player.score + newPosition
        }

    let rec executeGameMultiverse (outcomes:Map<int,int>) limit player1 player2 turn = 
        if player1.score >= limit || player2.score >= limit then
            {
                player1wins = if player1.score >= limit then 1L else 0L
                player2wins = if player2.score >= limit then 1L else 0L
            }

        else
            outcomes 
                |> Map.toList 
                |> List.map (fun (value, numTimes) -> 
                    match turn with
                    | Player1 ->
                        let newPlayer1 = executeDeterministicTurn value player1
                        let result = executeGameMultiverse outcomes limit newPlayer1 player2 Player2

                        { 
                            player1wins = result.player1wins * int64(numTimes)
                            player2wins = result.player2wins * int64(numTimes)
                        }

                    | Player2 -> 
                        let newPlayer2 = executeDeterministicTurn value player2 
                        let result = executeGameMultiverse outcomes limit player1 newPlayer2 Player1

                        { 
                            player1wins = result.player1wins * int64(numTimes)
                            player2wins = result.player2wins * int64(numTimes)
                        })
                |> List.fold (fun acc el -> { player1wins = acc.player1wins + el.player1wins; player2wins = acc.player2wins + el.player2wins }) { player1wins = 0; player2wins = 0}

    let diceOutcomes = calculateOutcomes 3 3

    let result2 = executeGameMultiverse diceOutcomes 21 player1 player2 Player1

    let result2 = sprintf "Numplayer1wins: %d NumPlayer2wins: %d, Max value: %d" result2.player1wins result2.player2wins (if result2.player1wins > result2.player2wins then result2.player1wins else result2.player2wins)
    printSecondStarResult result2

module AoC1

open Utils
open FParsec

let aoc1 () =
    printBanner 1

    let inputLines = readInputFile "AoC1.txt" |> Array.toList

    let calculateResult x = 
        x
        |> List.map (fun x -> (x |> Seq.find System.Char.IsDigit |> string, x |> Seq.findBack System.Char.IsDigit |> string))
        |> List.map (fun (x,y) -> x + y)
        |> List.map (fun x -> x |> int)
        |> List.sum

    let total1 = inputLines  |> calculateResult

    let result1 = sprintf "Result %d" total1
    printFirstStarResult result1

    let numberParser : Parser<char, unit> =
        choice [
            attempt (pchar 'o' .>> (lookAhead (pstring "ne")) >>% '1')
            attempt (pchar 't' .>> (lookAhead (pstring "wo"))  >>% '2')
            attempt (pchar 't' .>> (lookAhead (pstring "hree")) >>% '3')
            attempt (pchar 'f' .>> (lookAhead (pstring "our"))  >>% '4')
            attempt (pchar 'f' .>> (lookAhead (pstring "ive"))  >>% '5')
            attempt (pchar 's' .>> (lookAhead (pstring "ix"))   >>% '6')
            attempt (pchar 's' .>> (lookAhead (pstring "even")) >>% '7')
            attempt (pchar 'e' .>> (lookAhead (pstring "ight")) >>% '8')
            attempt (pchar 'n' .>> (lookAhead (pstring "ine"))  >>% '9')
        ]

    let charParser = choice [
        attempt numberParser;
        attempt anyChar;
    ]
    let fParser = many charParser

    let parseChars text =
        let r = run fParser text
        match r with 
        | Success (result, _, _) -> result
        | _ -> raise (new System.Exception("Not valid value"))

    let total2 = 
        inputLines 
        |> List.map(fun x -> (parseChars x |> Seq.ofList))
        |> calculateResult

    let result2 = sprintf "Result %d" total2
    printSecondStarResult result2
    ()
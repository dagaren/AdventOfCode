open Commands
open Parsing

let readInput () =
    printf "\n<: Type day number to execute or 'exit' to quit: "
    System.Console.ReadLine()

let readCommandSeq = seq {
    while true do
        yield (readInput ())
}

let filterValidCommand parseResult = 
    match parseResult with
     | Ok command -> Some command
     | Error _ -> printfn  "Invalid command"; None

readCommandSeq
    |> Seq.map parseCommand
    |> Seq.choose filterValidCommand
    |> Seq.takeWhile (not << isExitCommand)
    |> Seq.iter executeCommand

printfn "Normal termination of the program"
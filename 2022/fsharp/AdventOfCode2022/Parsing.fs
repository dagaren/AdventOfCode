module Parsing

open FParsec
open Commands

let pExecuteDayCommand = pint32 |>> ExecuteDay 
let pExit = stringCIReturn "exit"  Exit

let pCommand = choice [
    attempt pExecuteDayCommand
    attempt pExit
]

let parseCommand text =
    let r = run pCommand text
    match r with 
     | Success (result, _, _) -> Result.Ok result
     | _ -> Result.Error "Invalid command"


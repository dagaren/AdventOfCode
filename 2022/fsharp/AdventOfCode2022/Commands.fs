module Commands

open AoC1
open AoC2
open AoC3
open AoC4
open AoC5
open AoC6
open AoC7
open AoC8
open AoC9
open AoC10
open AoC11
open AoC12
open AoC13  
open AoC14
open AoC15
open AoC16
open AoC17
open AoC18
open AoC19
open AoC20
open AoC21
open AoC22
open AoC23
open AoC24
open AoC25

type Command = 
| ExecuteDay of int
| Exit

let days = [
    (1, aoc1);
    (2, aoc2);
    (3, aoc3);
    (4, aoc4);
    (5, aoc5);
    (6, aoc6);
    (7, aoc7);
    (8, aoc8);
    (9, aoc9);
    (10, aoc10);
    (11, aoc11);
    (12, aoc12);
    (13, aoc13);
    (14, aoc14);
    (15, aoc15);
    (16, aoc16);
    (17, aoc17);
    (18, aoc18);
    (19, aoc19);
    (20, aoc20);
    (21, aoc21);
    (22, aoc22);
    (23, aoc23);
    (24, aoc24);
    (25, aoc25); ] |> Map.ofList

let isExitCommand command = 
    match command with 
     | Exit -> true
     | _ -> false

let executeCommand command = 
    match command with
    | Exit -> ()
    | ExecuteDay dayNumber ->
        let actionO = days |> Map.tryFind dayNumber
        match actionO with 
        | Some action -> action ()
        | None -> printfn "Invalid day"
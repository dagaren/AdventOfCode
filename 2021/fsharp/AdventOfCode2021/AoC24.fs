module AoC24

open FParsec
open Utils

type variable = 
| W
| X
| Y
| Z

type alu = {
    variables: Map<variable, int>
    input: seq<int>
}

type operand = 
| Variable of variable
| Number of int

type binaryOperation = int -> int -> int

type binaryInstruction = binaryOperation * variable * operand

type unaryInstruction = variable

type instruction = 
| BinaryInstruction of binaryInstruction
| UnaryInstruction of unaryInstruction


let aoc24 () = 
    printBanner 24

    let input = [
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 1";
        "add x 15";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 15";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 1";
        "add x 12";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 5";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 1";
        "add x 13";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 6";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 26";
        "add x -14";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 7";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 1";
        "add x 15";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 9";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 26";
        "add x -7";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 6";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 1";
        "add x 14";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 14";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 1";
        "add x 15";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 3";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 1";
        "add x 15";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 1";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 26";
        "add x -7";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 3";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 26";
        "add x -8";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 4";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 26";
        "add x -7";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 6";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 26";
        "add x -5";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 7";
        "mul y x";
        "add z y";
        "inp w";
        "mul x 0";
        "add x z";
        "mod x 26";
        "div z 26";
        "add x -10";
        "eql x w";
        "eql x 0";
        "mul y 0";
        "add y 25";
        "mul y x";
        "add y 1";
        "mul z y";
        "mul y 0";
        "add y w";
        "add y 1";
        "mul y x";
        "add z y"
    ]
    
    let add (x:int) (y:int) = x + y
    let mul (x:int) (y:int) = x * y
    let div (x:int) (y:int) = x / y
    let modu (x:int) (y:int) = x % y
    let eql (x:int) (y:int) = if x = y then 1 else 0

    let pAdd = stringReturn "add" add
    let pMul = stringReturn "mul" mul
    let pDiv = stringReturn "div" div
    let pMod = stringReturn "mod" modu
    let pEql = stringReturn "eql" eql

    let pBinaryOperation = choice [
        (attempt pAdd);
        (attempt pMul);
        (attempt pDiv);
        (attempt pMod);
        (attempt pEql)
    ]

    let pVarX = stringReturn "x" X
    let pVarY = stringReturn "y" Y
    let pVarZ = stringReturn "z" Z
    let pVarW = stringReturn "w" W

    let pVar = choice [ 
        (attempt pVarX); 
        (attempt pVarY);
        (attempt pVarZ); 
        (attempt pVarW) ]

    let pOperand = choice [ 
        (attempt (pVar |>> Variable)); 
        (attempt (pint32 |>> Number))
    ]

    let ptempt = pBinaryOperation .>> pstring " "

    let pBinaryInstruction = pipe3 (pBinaryOperation .>> pstring " ") (pVar .>> pstring " ") (pOperand) (fun operation varDest operand -> BinaryInstruction (operation, varDest, operand))

    let pUnaryInstruction = pstring "inp " >>. pVar |>> UnaryInstruction

    let pInstruction = choice [
        (attempt pUnaryInstruction);
        (attempt pBinaryInstruction)
    ]

    let parseInstruction str = 
        match run  pInstruction str with
        | Success(result, _, _)   -> Some result
        | Failure(errorMsg, _, _) -> 
             printfn "Invalid instruction: %s" errorMsg 
             None

    let tryParseInstruction str : instruction =
        match parseInstruction str with
        | Some command -> command
        | None -> failwith "Invalid instruction"

    let nonZeroDigitSeq () = seq { for i in 9..-1..1 -> string(i) }
    
    let reduceSeqs seq1 seq2 =
        Seq.allPairs seq1 seq2 |> Seq.map (fun (x, y) -> x + y)

    let getNumbers size =
        [for i in 1..size -> nonZeroDigitSeq ()] |> List.reduce reduceSeqs

    let initAlu (inputNumber:string) =
        {
            variables = [ (W, 0); (X, 0); (Y, 0); (Z, 0) ] |> Map.ofList
            input = inputNumber |> Seq.toList |> Seq.map string |> Seq.map int
        }

    let execBinaryInstruction alu (binaryOperation, variableDest, operand) = 
        let x = alu.variables[variableDest]
        let y = match operand with
                | Variable var -> alu.variables[var]
                | Number num -> num

        let result = binaryOperation x y

        { alu with variables = alu.variables |> Map.add variableDest result }

    let execInputInstruction alu variableDest = 

        let first = alu.input |> Seq.head
        let remainingInput = alu.input |> Seq.skip 1
        
        { alu with variables = alu.variables |> Map.add variableDest first 
                   input = remainingInput }

    let executeInstruction alu instruction =
        match instruction with
        | BinaryInstruction inst -> execBinaryInstruction alu inst 
        | UnaryInstruction var -> execInputInstruction alu var


    let tryNumber number instructions =
        //printfn "Trying number %s..." number
        let initialAlu = initAlu number

        let finalAlu = instructions |> List.fold executeInstruction initialAlu

        finalAlu.variables[Z] = 0
    
    let numbersSequence = getNumbers 14

    // let r = numbersSequence |> Seq.fold (fun st el->
    //                                       st) 0

    let block c1 c2 c3 w zP = 
        let x1 = (zP % 26) + c2
        let z1 = zP / c1
        
        let x2 = if x1 <> w then 1 else 0

        let y1 = (25 * x2) + 1
        let z2 = z1 * y1

        let y3 = (w + c3) * x2

        let z3 = z2 + y3

        z3

    let blockConstants = [
        (1, 15, 15);
        (1, 12, 5);
        (1, 13, 6);
        (26, -14, 7);
        (1, 15, 9);
        (26, -7, 6);
        (1, 14, 14);
        (1, 15, 3);
        (1, 15, 1);
        (26, -7, 3);
        (26, -8, 4);
        (26, -7, 6);
        (26, -5, 7);
        (26, -10, 1)
    ]

    let blocks = blockConstants |> List.map (fun (x, y , z) -> block x y z)

    let initialState = (0, [ ([], 0) ])

    let ascendingDigits = [for i in 1..9 -> i]
    let descendingDigits = [for i in 9..-1..1 -> i]
    
    let applyBlock digits (iteration, state) block = 
        printfn "Applying block %d...Nun inputs: %d" iteration (state |> List.length)

        let r = [for i in digits do for (number, j) in state -> (i :: number, block i j)]
                    |> List.groupBy (fun (number, value) -> value)
                    |> List.map (fun (value, elements) -> (elements.Head))             

        (iteration + 1, r)
    
    let (iterations, largestResults) = blocks |> List.fold (applyBlock descendingDigits) initialState

    let largestNumber = 
        largestResults
        |> List.where (fun (numbers, value) -> value = 0) 
        |> List.map (fun (numbers, value) -> numbers |> List.map (fun x -> string(x)) |> List.rev |> List.fold (+) "")
        |> List.head

    let result1 = sprintf "Greater number valid: %s" largestNumber
    printFirstStarResult result1

    let (iterations, smallestResults) = blocks |> List.fold (applyBlock ascendingDigits) initialState
    
    let smallestNumber = 
        smallestResults
        |> List.where (fun (number, value) -> value = 0) 
        |> List.map (fun (numbers, value) -> numbers |> List.map (fun x -> string(x)) |> List.rev |> List.fold (+) "")
        |> List.head

    let result2 = sprintf "Smallest number valid: %s" smallestNumber
    printSecondStarResult result2


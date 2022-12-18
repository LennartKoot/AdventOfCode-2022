open SupplyStacks.Input
open SupplyStacks.Instructions
open SupplyStacks.Stacks

type State = { Stacks: Stacks; Instructions: Instruction list }
type Crane =
    | CraneMover9000 // Part One
    | CraneMover9001 // Part Two

let exampleInput = loadInput "data/example.txt"
let realInput = loadInput "data/input.txt"

let applyInstruction (stacks: Stacks) (instruction: Instruction) (crane: Crane): Stacks =
    let ((movingCrates: Crate list), (sourceStack: Stack)) =
        stacks[instruction.Source - 1] |> List.splitAt (instruction.Amount)
    
    stacks
    |> List.mapi (fun i stack ->
        match i with
        | n when (n = instruction.Source - 1) -> 
            sourceStack
        | n when (n = instruction.Destination - 1) ->
            match crane with
            | CraneMover9000 -> (movingCrates |> List.rev)@stack
            | CraneMover9001 -> movingCrates@stack
        | _ ->
            stack)

let rec executeInstructions (state: State) (crane: Crane) : State =
    match state.Instructions with
    | [] -> state
    | x::xs -> executeInstructions { Stacks = applyInstruction state.Stacks x crane; Instructions = xs} crane

let startState input = 
    let (stacks, remainingInput) = parseStacks input
    let instructions = parseInstructions remainingInput
    { Stacks = stacks; Instructions = instructions }

let determineTopCrates (stacks: Stacks) =
    stacks |> List.map List.head

let printCrates (crates: Crate list) = crates |> Array.ofList |> System.String

let exampleStartState = startState exampleInput
let exampleEndState = executeInstructions exampleStartState
let realStartState = startState realInput
let realEndState = executeInstructions realStartState

printfn "Part one"
printfn "Example Data: %s" (determineTopCrates (exampleEndState CraneMover9000).Stacks |> printCrates)
printfn "Real Data: %A" (determineTopCrates (realEndState CraneMover9000).Stacks |> printCrates)
printfn ""

printfn "Part two"
printfn "Example Data: %s" (determineTopCrates (exampleEndState CraneMover9001).Stacks |> printCrates)
printfn "Real Data: %A" (determineTopCrates (realEndState CraneMover9001).Stacks |> printCrates)
printfn ""
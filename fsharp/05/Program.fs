open SupplyStacks.Input
open SupplyStacks.Instructions
open SupplyStacks.Stacks

type State = { Stacks: Stacks; Instructions: Instruction list }

let exampleInput = loadInput "data/example.txt"
let realInput = loadInput "data/input.txt"

let applyInstruction (stacks: Stacks) (instruction: Instruction): Stacks =
    let ((movingCrates: Crate list), (sourceStack: Stack)) =
        stacks[instruction.Source - 1] |> List.splitAt (instruction.Amount)
    
    stacks
    |> List.mapi (fun i stack ->
        match i with
        | n when (n = instruction.Source - 1) -> 
            sourceStack
        | n when (n = instruction.Destination - 1) ->
            (movingCrates |> List.rev)@stack
        | _ ->
            stack)

let rec executeInstructions (state: State) : State =
    match state.Instructions with
    | [] -> state
    | x::xs -> executeInstructions { Stacks = applyInstruction state.Stacks x; Instructions = xs}

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
printfn "Example Data: %s" (determineTopCrates exampleEndState.Stacks |> printCrates)
printfn "Real Data: %A" (determineTopCrates realEndState.Stacks |> printCrates)
printfn ""

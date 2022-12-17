module SupplyStacks.Stacks

type Crate = char
type Stack = Crate list
type Stacks = Stack list

let parseStacks lines =
    let rec takeStackLines remainingLines =
        match remainingLines with
        | [] ->
            ([], remainingLines)
        | (""::xs) ->
            ([], xs)
        | (x::xs) -> 
            let (stackLines, nextRemainingLines) = takeStackLines xs
            (x::stackLines, nextRemainingLines)

    let parseStackLine (line: string): Crate option list =
        line
        |> List.ofSeq
        |> List.chunkBySize 4
        |> List.map (function
            | '[' :: c :: ']' :: _ -> Some c
            | _ -> None)

    let (stackLines, remainingLines) = lines |> takeStackLines

    let stacks: Stacks =
        stackLines
        |> List.map parseStackLine
        |> List.transpose
        |> List.map (List.choose id)
    
    (stacks, remainingLines)

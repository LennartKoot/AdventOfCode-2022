open System
open RucksackReorganization.Input
open RucksackReorganization.Rucksack

let calculateItemPriority c =
    match c with
    | c when Char.IsLower(c) -> int c - int 'a' + 1
    | _ -> int c - int 'A' + 27

let foldIntoSum f = Seq.fold (fun sum item -> sum + f item) 0
let calculatePrioritiesSum (duplicates: Set<char> array): int =
    duplicates
    |> Array.fold (fun sum items -> sum + (foldIntoSum calculateItemPriority items)) 0

let exampleInput = loadInput "data/example.txt"
let exampleDuplicateItems = determineDuplicateItems exampleInput
let exampleSum = calculatePrioritiesSum exampleDuplicateItems

let realInput = loadInput "data/input.txt"
let realDuplicateItems = determineDuplicateItems realInput
let realSum = calculatePrioritiesSum realDuplicateItems

printfn "Part One"
printfn "Example Data: %A" exampleSum
printfn "Real Data: %A" realSum

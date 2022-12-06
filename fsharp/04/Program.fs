open CampCleanup.Input
open CampCleanup.Pair

let exampleInput = loadInput "data/example.txt"
let realInput = loadInput "data/input.txt"

let examplePairs = exampleInput |> Seq.map parsePair
let realPairs = realInput |> Seq.map parsePair

let assignmentFullyContainsOther (pair:Pair) =
    pair.left.fullyContains pair.right ||
    pair.right.fullyContains pair.left

let countContainedRanges pairs =
    pairs |> Seq.filter assignmentFullyContainsOther |> Seq.length

let exampleCountContainedRanges = countContainedRanges examplePairs
let realCountContainedRanges = countContainedRanges realPairs

printfn "Part one"
printfn "Example Data: %A" exampleCountContainedRanges
printfn "Real Data: %A" realCountContainedRanges
printfn ""

let assignmentOverlapsOther (pair:Pair) =
    pair.left.overlaps pair.right

let countOverlappingRanges pairs =
    pairs |> Seq.filter assignmentOverlapsOther |> Seq.length

let exampleCountOverlappingRanges = countOverlappingRanges examplePairs
let realCountOverlappingRanges = countOverlappingRanges realPairs

printfn "Part two"
printfn "Example Data: %A" exampleCountOverlappingRanges
printfn "Real Data: %A" realCountOverlappingRanges
printfn ""

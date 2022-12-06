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

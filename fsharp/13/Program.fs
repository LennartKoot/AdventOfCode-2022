open DistressSignal.Signal
open DistressSignal.Input

let example = loadInput "data/example.txt"
let real = loadInput "data/input.txt"

let signal = parse

printfn "Example: %A" <| signal example
printfn "Real: %A" <| signal real

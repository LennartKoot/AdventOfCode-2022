open CalorieCounting.Input
open CalorieCounting.Parsing

let exampleInput = loadInput "data/example.txt"
let realInput = loadInput "data/input.txt"

printfn "Part one"
printfn "Example Data: %A" (getMaxCalories exampleInput)
printfn "Real Data: %A" (getMaxCalories realInput)
printfn ""

printfn "Part two"
printfn "Example Data: %A" (getSumOfTopCalories exampleInput 3)
printfn "Real Data: %A" (getSumOfTopCalories realInput 3)

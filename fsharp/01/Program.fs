open CalorieCounting.Input
open CalorieCounting.Parsing

let exampleInput = loadInput "data/example.txt"
let realInput = loadInput "data/input.txt"

printfn "Max Calories Example Data: %A" (getMaxCalories exampleInput)
printfn "Max Calories Real Data: %A" (getMaxCalories realInput)

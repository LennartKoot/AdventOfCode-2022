open NoSpaceLeftOnDevice.Input

let exampleInput = loadInput "data/example.txt"
let filesystem = NoSpaceLeftOnDevice.FileSystem.create exampleInput

printfn "FileSystem: %A" filesystem

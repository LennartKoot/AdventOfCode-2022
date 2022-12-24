open NoSpaceLeftOnDevice.Input
open NoSpaceLeftOnDevice.FileSystemFoldAlgebra

let exampleInput = loadInput "data/example.txt"
let filesystem = NoSpaceLeftOnDevice.FileSystem.create exampleInput

// Simple fold to test fold algebra
let totalFileSystemSize =
    let fFile ((_, size)) = size
    let fDirectory ((_, subSizes)) = List.sum subSizes
    let fFileSystem rootSizes = List.sum rootSizes
    fold fFile fDirectory fFileSystem

printfn "Total sum: %A | Expected: 48381165" (filesystem |> totalFileSystemSize)

printfn "%A" filesystem

open NoSpaceLeftOnDevice.Input
open NoSpaceLeftOnDevice.FileSystem
open NoSpaceLeftOnDevice.FileSystemFoldAlgebra

let exampleInput = loadInput "data/example.txt"
let filesystem = NoSpaceLeftOnDevice.FileSystem.create exampleInput

let totalSumAlgebra = {
    file = (fun f -> f.Size);
    dir = (fun _ subItems -> List.sumBy (fun item -> match item with | File f -> f | Directory d -> d) subItems)
}

printfn "Total sum: %A" (fold totalSumAlgebra filesystem)

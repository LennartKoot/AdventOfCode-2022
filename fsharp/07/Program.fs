open NoSpaceLeftOnDevice.Input
open NoSpaceLeftOnDevice.FileSystem
open NoSpaceLeftOnDevice.FileSystemFoldAlgebra

let filesystem = create << loadInput

// Part one
type ItemTreeWithDirectorySize =
    | File of File
    | Directory of Directory * int * ItemTreeWithDirectorySize list

let determineDirectorySizes =
    let fFile = File
    let fDirectory (dir, subTrees) = 
        let sumBy = function | File (_,size) -> size | Directory (_,size,_) -> size
        Directory (dir, subTrees |> List.sumBy sumBy, subTrees)
    foldUpToRoots fFile fDirectory

printfn "FileSystem with directory sizes: %A" <| (filesystem "data/example.txt" |> determineDirectorySizes)

module NoSpaceLeftOnDevice.FileSystemFoldAlgebra

open NoSpaceLeftOnDevice.FileSystem

let fold fFile fDirectory fFileSystem (fileSystem: FileSystem) : 'r =
    let rec foldItemTree tree =
        let recurse = List.map foldItemTree
        match tree with
        | File file ->
            fFile file
        | Directory (directory, subTrees) ->
            fDirectory (directory, recurse subTrees)

    fileSystem |> List.map foldItemTree |> fFileSystem

let foldUpToRoots fFile fDirectory =
    let fFileSystem results = results
    fold fFile fDirectory fFileSystem

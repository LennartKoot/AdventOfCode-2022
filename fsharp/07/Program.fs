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

let flatten fFile fDirectory (trees: ItemTreeWithDirectorySize list) =
    let rec flattenTree acc tree =
        match tree with
        | File file -> 
            fFile file 
            |> function 
                | Some f -> f :: acc
                | None -> acc
        | Directory (dir,size,subTress) ->
            let subFlattened = subTress |> List.map (flattenTree []) |> List.collect id
            fDirectory (dir, size)
            |> function
                | Some d -> d :: subFlattened
                | None -> subFlattened
    trees |> List.map (flattenTree []) |> List.collect id

let flattenAndFilterDirectories sizeFilter =
    let fFile f = None
    let fDirectory (d,size) =
        match size with
        | s when sizeFilter size -> Some s
        | _ -> None
    flatten fFile fDirectory

let partOne data =
    filesystem data 
    |> determineDirectorySizes
    |> flattenAndFilterDirectories (fun s -> s < 100000)
    |> List.sum

printfn "Part one (example): %A" <| partOne "data/example.txt"
printfn "Part one (real): %A" <| partOne "data/input.txt"

// Part two
let determineTotalSize =
    let fFile (_, size) = size
    let fDirectory (_, subSizes) = List.sum subSizes
    let fFileSystem roots = List.sum roots
    fold fFile fDirectory fFileSystem

let partTwo data =
    let totalDiskSpace = 70000000
    let requiredFreeDiskSpace = 30000000
    let filesystem = filesystem data
    let minDeleteSize =
        filesystem
        |> determineTotalSize
        |> (-) totalDiskSpace
        |> (-) requiredFreeDiskSpace
    filesystem
    |> determineDirectorySizes
    |> flattenAndFilterDirectories (fun s -> s >= minDeleteSize)
    |> List.min

printfn "Part two (example): %A" <| partTwo "data/example.txt"
printfn "Part two (real): %A" <| partTwo "data/input.txt"

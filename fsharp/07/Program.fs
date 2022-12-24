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

let flattenAndFilterDirectories maxSize =
    let fFile f = None
    let fDirectory (d,size) =
        match size with
        | s when size < maxSize -> Some s
        | _ -> None
    flatten fFile fDirectory

printfn "Part one (example): %A" <| ( 
    filesystem "data/example.txt" 
    |> determineDirectorySizes
    |> flattenAndFilterDirectories 100000
    |> List.sum
)

// printfn "Part one (real): %A" <| ( 
//     filesystem "data/input.txt" 
//     |> determineDirectorySizes
//     |> flattenAndFilterDirectories 100000
//     |> List.sum
// )
module NoSpaceLeftOnDevice.FileSystem

type Directory = string
type File = string * int
type ItemTree = 
    | File of File
    | Directory of (Directory * ItemTree list)
type FileSystem = ItemTree list

type private Line =
    | CdParentDirectory
    | CdDirectory of string
    | Ls
    | File of string * int
    | Dir of string

let private parseLine (line: string) =
    let parts = line.Split(' ') |> List.ofArray
    match parts with
    | [ "$";"ls" ] -> Ls
    | [ "$";"cd";".."] -> CdParentDirectory
    | [ "$";"cd";dir] -> CdDirectory dir
    | [ "dir";dir] -> Dir dir
    | [ size;file ] -> File (file, int size)
    | _ -> failwith $"Unrecognized line '{line}'"

let create (input: string list): FileSystem =
    let rec createItemTree (state: ItemTree) (input: Line list): ItemTree * Line list =
        match state with
        | ItemTree.File _ -> (state, input)
        | ItemTree.Directory dir ->
            let (dirName, subTrees) = dir
            match input with
            | [] -> (state, input)
            | x::xs ->
                let noop = createItemTree state xs
                match x with
                | CdParentDirectory -> (state, xs)
                | Ls -> noop
                | Dir _ -> noop
                | File (name, size) -> createItemTree (Directory (dirName, (ItemTree.File (name, size) :: subTrees))) xs
                | CdDirectory name ->
                    let (itemTree, remainingInput) = createItemTree (ItemTree.Directory (name, [])) xs
                    createItemTree (Directory (dirName, itemTree :: subTrees)) remainingInput

    let rec createFileSystem (state: FileSystem) (input: Line list) =
        let addItemTree startState xs =
            let (ItemTree, remainingInput) = createItemTree startState xs
            createFileSystem (ItemTree :: state) remainingInput

        match input with
        | [] -> state
        | x::xs ->
            let noop = createFileSystem state xs
            match x with
            | Ls -> noop
            | Dir _ -> noop
            | CdParentDirectory -> noop
            | File (name, size) -> addItemTree (ItemTree.File (name, size)) xs
            | CdDirectory name -> addItemTree (ItemTree.Directory (name, [])) xs

    createFileSystem [] (input |> List.map parseLine)
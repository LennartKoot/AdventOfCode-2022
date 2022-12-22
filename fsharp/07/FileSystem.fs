module NoSpaceLeftOnDevice.FileSystem

type Item =
    | Directory of Directory
    | File of File

and Directory = { Name: string; SubItems: Item list }
and File = { Name: string; Size: int }

type FileSystem = FileSystem of Directory

type private LineType =
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
    let rec createInternal input currentDirectory : Directory =
        match input with
        | [] -> currentDirectory
        | x::xs ->
            let noop = createInternal xs currentDirectory
            let file name size = Item.File { Name = name; Size = size }
            let dir name xs = Directory (createInternal xs { Name = name; SubItems = [] })
            // CdParentDirectory marks the end of exploring a directory
            let skipSubdirLines xs = xs |> List.takeWhile (fun x -> match (parseLine x) with CdParentDirectory -> true | _ -> false) 
            match parseLine x with
            | CdParentDirectory -> noop
            | Ls -> noop
            | Dir _ -> noop
            // Add file to current list of subitems and continue with remaining input
            | File (name,size) -> createInternal xs { currentDirectory with SubItems = (file name size)::currentDirectory.SubItems }
            // Recursively create and add a directory to current list of subitems and continue with remaining input, skipping lines already hit via recursion
            | CdDirectory d -> createInternal (skipSubdirLines xs) { currentDirectory with SubItems = (dir d xs)::currentDirectory.SubItems }

    let rootDir = { Name = "/"; SubItems = [] }
    FileSystem (createInternal (List.skip 1 input) rootDir)

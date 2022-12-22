module NoSpaceLeftOnDevice.FileSystemFoldAlgebra

open NoSpaceLeftOnDevice.FileSystem

type FoldItemResult<'file,'dir> =
    | File of 'file
    | Directory of 'dir
type FileSystemFoldAlgebra<'file,'dir> = { file: File -> 'file; dir: Directory -> FoldItemResult<'file,'dir> list -> 'dir }

let fold (alg: FileSystemFoldAlgebra<_,_>) (fs: FileSystem) =
    let { file = fFile; dir = fDir } = alg
    
    let rec foldDir dir = fDir dir (List.map fItem dir.SubItems)
    and fItem item =
        match item with
        | Item.File f -> FoldItemResult.File (fFile f)
        | Item.Directory d -> FoldItemResult.Directory (foldDir d)

    let (FileSystem rootDir) = fs
    foldDir rootDir

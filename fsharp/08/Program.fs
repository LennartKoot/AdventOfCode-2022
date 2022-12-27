open TreetopTreeHouse.Input

let forestMatrix dataLocation =
    loadInput dataLocation
    |> Seq.map (Seq.map (string >> int))
    |> array2D

let mapDirections fDirection fCombine edgeValue row column (matrix: int[,]) =
    let treeHeight = matrix[row,column]
    let maxColumn = matrix[row,*].Length - 1
    let maxRow = matrix[*,column].Length - 1
    
    match row, column with
    | 0, _                          -> edgeValue
    | _, 0                          -> edgeValue
    | r,_ when r = maxRow  -> edgeValue
    | _,c when c = maxColumn -> edgeValue
    | _ ->
        let left (matrix:int[,]) =
            matrix[row,0 .. column - 1] |> Array.rev |> fDirection treeHeight
        let right (matrix:int[,]) =
            matrix[row,column + 1 .. maxColumn] |> fDirection treeHeight
        let top (matrix:int[,]) =
            matrix[0 .. row - 1, column] |> Array.rev |> fDirection treeHeight
        let bottom (matrix:int[,]) = 
            matrix[row + 1 .. maxRow, column] |> fDirection treeHeight
        
        fCombine (left matrix) (right matrix) (top matrix) (bottom matrix)

let fold (folder: 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    seq {
        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                yield (array.[x, y])
    }
    |> Seq.fold (fun acc e -> folder acc e) state

let isVisible =
    let fDirection treeHeight trees = trees |> Array.max |> (>) treeHeight
    let fCombine left right top bottom = left || right || top || bottom
    mapDirections fDirection fCombine true

let partOne dataLocation =
    let matrix = forestMatrix dataLocation
    matrix
    |> Array2D.mapi (fun row column _ -> isVisible row column matrix)
    |> fold (fun count visible -> 
        match visible with
        | true -> count + 1
        | false -> count)
        0

printfn "Part one (example): %A" <| partOne "data/example.txt"
printfn "Part one (real): %A" <| partOne "data/input.txt"

let calculateScenicScore =
    let fDirection treeHeight trees = 
        let visibleTrees = 
            trees
            |> Array.takeWhile ((>) treeHeight)
            |> Array.length
        if visibleTrees <> trees.Length
        then visibleTrees + 1
        else visibleTrees
    let fCombine left right top bottom = left * right * top * bottom
    mapDirections fDirection fCombine 0

let partTwo dataLocation =
    let matrix = forestMatrix dataLocation
    matrix
    |> Array2D.mapi (fun row column _ -> calculateScenicScore row column matrix)
    |> fold (fun acc score -> score :: acc) []
    |> Seq.max

printfn "Part two (example): %A" <| partTwo "data/example.txt"
printfn "Part two (real): %A" <| partTwo "data/input.txt"

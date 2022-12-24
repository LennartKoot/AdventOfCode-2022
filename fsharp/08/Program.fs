open TreetopTreeHouse.Input

let forestMatrix dataLocation =
    loadInput dataLocation
    |> Seq.map (Seq.map (string >> int))
    |> array2D

let isVisible row column (matrix: int[,]) =
    let treeHeight = matrix[row,column]
    let maxColumn = matrix[row,*].Length - 1
    let maxRow = matrix[*,column].Length - 1
    
    match row, column with
    | 0, _                          -> true
    | _, 0                          -> true
    | r,_ when r = maxRow  -> true
    | _,c when c = maxColumn -> true
    | _ ->
        let checkSlice slice =
            slice
            |> Array.max 
            |> (>) treeHeight

        let visibleFromLeft (matrix:int[,]) =
            matrix[row,0 .. column - 1] |> checkSlice
        let visibleFromRight (matrix:int[,]) =
            matrix[row,column + 1 .. maxColumn] |> checkSlice
        let visibleFromTop (matrix:int[,]) =
            matrix[0 .. row - 1, column] |> checkSlice
        let visibleFromBottom (matrix:int[,]) = 
            matrix[row + 1 .. maxRow, column] |> checkSlice
        
        visibleFromLeft matrix ||
        visibleFromRight matrix ||
        visibleFromTop matrix ||
        visibleFromBottom matrix

let fold (folder: 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    seq {
        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                yield (array.[x, y])
    }
    |> Seq.fold (fun acc e -> folder acc e) state

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
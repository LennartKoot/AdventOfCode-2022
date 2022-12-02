module CalorieCounting.Parsing

let private foldIntoSums (sums:int list) (line:string) : int list =
    match line with
    | "" -> 0 :: sums
    | _ -> 
        match sums with
        | [] -> [int line]
        | x::xs -> (x + int line) :: xs

let private determineSums (lines:string list): int list =
    List.fold foldIntoSums [] lines

let getMaxCalories (lines:string list): int =
    (determineSums lines) |> List.max

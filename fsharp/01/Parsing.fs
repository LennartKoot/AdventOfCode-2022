module CalorieCounting.Parsing

let private foldIntoSumPerElf (sums:int list) (line:string) : int list =
    match line with
    | "" -> 0 :: sums // Empty line starts a new elf
    | _ -> 
        match sums with
        | [] -> [int line]
        | currentSum::finishedSums -> (currentSum + int line) :: finishedSums

let private determineSumPerElf (lines:string list): int list =
    List.fold foldIntoSumPerElf [] lines

// Part One
let getMaxCalories (lines:string list): int =
    (determineSumPerElf lines) |> List.max

// Part Two
let getSumOfTopCalories (lines:string list) (take:int): int =
    (determineSumPerElf lines)
    |> List.sortDescending
    |> List.take take
    |> List.sum

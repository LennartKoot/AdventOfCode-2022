module CampCleanup.Utility

exception Exception of string

let splitIntoTwoAndApply (s: string) (c: char) f =
    s.Split c
    |> Array.toList
    |> fun xs -> 
        match xs with
        | x::y::[] -> f x y
        | _ -> raise (Exception $"Expected exactly two items after splitting on '{c}'")

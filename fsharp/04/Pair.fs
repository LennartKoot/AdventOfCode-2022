module CampCleanup.Pair

open Assigment
open Utility

type Pair = { left: Assigment; right: Assigment }

let parsePair (s:string) =
    splitIntoTwoAndApply s ',' (fun x y -> parseAssignment x, parseAssignment y)
    |> (fun (x, y) -> { left = x; right = y })

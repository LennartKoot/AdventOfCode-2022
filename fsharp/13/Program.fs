open DistressSignal.Signal
open DistressSignal.Input

let example = loadInput "data/example.txt"
let real = loadInput "data/input.txt"

let signal = parse

let rec compare (left, right) =
    match left, right with
    | PNumber lhs, PNumber rhs
        -> lhs.CompareTo rhs
    | PNumber _, PList _
        -> compare ((PList [left]), right)
    | PList _, PNumber _
        -> compare (left, (PList [right]))
    | PList lhs, PList rhs
        -> Seq.zip lhs rhs
        |> Seq.map compare
        |> Seq.skipWhile ((=) 0)
        |> Seq.tryHead
        |> function
            | Some h -> h
            | None -> (Seq.length lhs).CompareTo (Seq.length rhs)

let partOne input =
    signal input
    |> List.mapi (fun i (l, r) -> (i + 1), (compare (l, r)))
    |> List.filter (snd >> (=) -1)
    |> List.sumBy fst

printfn "Example: %A" <| partOne example
printfn "Real: %A" <| partOne real

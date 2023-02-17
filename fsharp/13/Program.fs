open DistressSignal.Signal
open DistressSignal.Input

let example = loadInput "data/example.txt"
let real = loadInput "data/input.txt"

let signal = parse

let uncurry f (x,y) = f x y

let rec compare left right =
    match left, right with
    | PNumber lhs, PNumber rhs
        -> lhs.CompareTo rhs
    | PNumber _, PList _
        -> compare (PList [left]) right
    | PList _, PNumber _
        -> compare left (PList [right])
    | PList lhs, PList rhs
        -> Seq.zip lhs rhs
        |> Seq.map (uncurry compare)
        |> Seq.skipWhile ((=) 0)
        |> Seq.tryHead
        |> function
            | Some h -> h
            | None -> (Seq.length lhs).CompareTo (Seq.length rhs)

let partOne input =
    signal input
    |> List.mapi (fun i (l, r) -> (i + 1), (compare l r))
    |> List.filter (snd >> (=) -1)
    |> List.sumBy fst

printfn "Part One"
printfn "Example: %A" <| partOne example
printfn "Real: %A" <| partOne real
printfn ""

let packets = parsePackets

let dividerPacket n = PList [(PList [PNumber n])]

let partTwo input =
    packets input
    |> List.map (fun p -> false, p)
    |> List.append [ (true, dividerPacket 2); (true, dividerPacket 6)]
    |> List.sortWith (fun (_, a) (_, b) -> compare a b)
    |> List.mapi (fun i p ->
        match p with
        | (true, _) -> i + 1
        | _ -> 1)
    |> List.reduce (*)

printfn "Part Two"
printfn "Example: %A" <| partTwo example
printfn "Real: %A" <| partTwo real

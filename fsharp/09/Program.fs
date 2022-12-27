open RopeBridge.Bridge
open RopeBridge.Input

let parseMotions (input: string array) =
    input
    |> Seq.map List.ofSeq
    |> Seq.map (fun line ->
        let createMotion m n = 
            n |> List.map string |> List.reduce (+) |> int |> m
        match line with
        | 'U'::' '::n -> createMotion Up n
        | 'D'::' '::n -> createMotion Down n
        | 'L'::' '::n -> createMotion Left n
        | 'R'::' '::n -> createMotion Right n
        | l -> failwith $"Unrecognized line '{l}'"
    )

let partOne dataLocation =
    loadInput dataLocation
    |> parseMotions
    |> Seq.fold executeMotion (RopeBridge.Bridge.create 2)
    |> (fun bridge -> bridge.Visited.Count)

printfn "Part one (example): %A" <| partOne "data/example.txt"
printfn "Part one (real): %A" <| partOne "data/input.txt"

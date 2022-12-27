open CathodeRayTube.Input

type Instruction =
    | Noop
    | Addx of int

let parseInstruction (line: string) =
    match line.Split(' ') with
    | [| "noop" |] -> Noop
    | [| "addx"; n |] -> Addx (int n)
    | _ -> failwith $"Unrecognized line {line}"

let executeInstruction (cycles: int list) instruction =
    let current = cycles.Head
    match instruction with
    | Noop -> current :: cycles
    | Addx n -> current + n :: current :: cycles // Addx takes two cycles, so for an extra cycle the value stays the same

let instructions dataLocation =
    loadInput dataLocation
    |> Seq.map parseInstruction

let partOne dataLocation =
    let specialCycles = [20 .. 40 .. 220]
    let cycleValues =
        instructions dataLocation
        |> Seq.fold executeInstruction [1]
        |> List.rev

    specialCycles
    |> Seq.map (fun c -> cycleValues[c - 1] * c)
    |> Seq.sum

printfn "Part one (example): %A" <| partOne "data/example.txt"
printfn "Part one (real): %A" <| partOne "data/input.txt"

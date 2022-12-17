module SupplyStacks.Instructions
open System.Text.RegularExpressions

type Instruction = {
    Amount: int;
    Source: int;
    Destination: int;
}

let private regex = Regex("move (\d+) from (\d+) to (\d+)", RegexOptions.Compiled)

let parseInstructions lines =
    lines
    |> List.map regex.Match
    |> List.map (fun m -> {
        Amount = m.Groups[1].Value |> int
        Source = m.Groups[2].Value |> int
        Destination = m.Groups[3].Value |> int})

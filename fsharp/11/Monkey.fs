module MonkeyInTheMiddle.Monkey

open Expression

type WorryLevel = int
type Item = Item of WorryLevel

type Test = {
    DivisibleBy: int;
    ThrowToOnTrue: int;
    ThrowToOnFalse: int;
}

type Monkey = {
    Id: int;
    Items: Item list;
    Test: Test;
    Operation: Expression;
    Inspections: int;
}

let parseMonkeys (lines: string array) =
    let parseStartingItems (line: string) =
        let prefix = "  Starting items: "
        line[prefix.Length..].Split(',')
        |> Array.map (fun s -> s.Trim() |> int)
        |> Array.map Item
        |> List.ofArray

    let parseInt (prefix: string) (line: string) =
        line[prefix.Length..] |> int

    lines
    |> Array.chunkBySize 7
    |> Array.mapi (fun idx lines -> 
        let test = {
            DivisibleBy = parseInt "  Test: divisible by " lines[3];
            ThrowToOnTrue = parseInt "    If true: throw to monkey " lines[4];
            ThrowToOnFalse = parseInt "    If false: throw to monkey " lines[5];
        }
        {
            Id = idx;
            Items = parseStartingItems lines[1];
            Operation = parseExpression (lines[2][13..]);
            Test = test;
            Inspections = 0;
        })

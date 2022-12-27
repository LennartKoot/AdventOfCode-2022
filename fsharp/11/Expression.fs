module MonkeyInTheMiddle.Expression

type Value = 
    | Old
    | Number of int64

type Expression =
    | Add of Value * Value
    | Multiply of Value * Value
        member e.Evaluate old =
            let value v =
                match v with
                | Number n -> n
                | Old -> old
            match e with
            | Add (a, b) -> value a + value b
            | Multiply (a, b) -> value a * value b

let parseExpression (line: string) =
    let value s =
        match s with
        | "old" -> Old
        | n -> Number (int n)
    match (line.Split(' ') |> Array.skip 2) with
    | [| a; "+"; b |] -> Add (value a, value b)
    | [| a; "*"; b |] -> Multiply (value a, value b)
    | _ -> failwith $"Unrecognized line {line}"

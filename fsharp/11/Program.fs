open MonkeyInTheMiddle.Input
open MonkeyInTheMiddle.Monkey

let monkeys dataLocation =
    loadInput dataLocation
    |> parseMonkeys

let throwItem item target (monkeys: Monkey array) =
    let monkey = monkeys[target]

    monkeys
    |> Array.updateAt target { monkey with Items = monkey.Items @ [item] }

let processItem relieve monkey monkeys item =
    let allModulo = monkeys |> Array.map (fun m -> int64 m.Test.DivisibleBy) |> Array.reduce (*)
    let (Item itemValue) = item
    let inspectedItemValue = monkey.Operation.Evaluate (itemValue % allModulo)
    let relievedItemValue = if relieve then inspectedItemValue / 3L else inspectedItemValue
    match relievedItemValue % int64 monkey.Test.DivisibleBy with
    | 0L -> throwItem (Item relievedItemValue) monkey.Test.ThrowToOnTrue monkeys
    | _ -> throwItem (Item relievedItemValue) monkey.Test.ThrowToOnFalse monkeys

let playTurn relieve (monkeys: Monkey array) (id: int) =
        let monkey = monkeys[id]
        let monkeysCurrentCleared =
            monkeys
            |> Array.updateAt monkey.Id { 
                monkey with 
                    Inspections = monkey.Inspections + monkey.Items.Length;
                    Items = []
                }
        
        monkey.Items
        |> List.fold (processItem relieve monkey) monkeysCurrentCleared

let playRound relieve monkeys =
    monkeys
    |> Array.map (fun m -> m.Id)
    |> Array.fold (playTurn relieve) monkeys

let rec playRounds n relieve monkeys =
    match n with
    | 0 -> monkeys
    | _ -> playRounds (n - 1) relieve (playRound relieve monkeys)

let partOne dataLocation =
    let monkeys = 
        monkeys dataLocation
        |> playRounds 20 true
        |> Array.sortByDescending (fun m -> m.Inspections)

    monkeys[0].Inspections * monkeys[1].Inspections

printfn "Part one (example): %A" <| partOne "data/example.txt"
printfn "Part one (real): %A" <| partOne "data/input.txt"

let partTwo dataLocation =
    let monkeys = 
        monkeys dataLocation
        |> playRounds 10000 false
        |> Array.sortByDescending (fun m -> m.Inspections)

    int64 monkeys[0].Inspections * int64 monkeys[1].Inspections

printfn "Part two (example): %A" <| partTwo "data/example.txt"
printfn "Part two (real): %A" <| partTwo "data/input.txt"
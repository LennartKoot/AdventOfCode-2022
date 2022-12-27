open MonkeyInTheMiddle.Input
open MonkeyInTheMiddle.Monkey

let monkeys dataLocation =
    loadInput dataLocation
    |> parseMonkeys

let throwItem item target (monkeys: Monkey array) =
    let monkey = monkeys[target]

    monkeys
    |> Array.updateAt target { monkey with Items = monkey.Items @ [item] }

let processItem monkey monkeys item =
    let (Item itemValue) = item
    let inspectedItemValue = monkey.Operation.Evaluate itemValue
    let relievedItemValue = inspectedItemValue / 3
    match relievedItemValue % monkey.Test.DivisibleBy with
    | 0 -> throwItem (Item relievedItemValue) monkey.Test.ThrowToOnTrue monkeys
    | _ -> throwItem (Item relievedItemValue) monkey.Test.ThrowToOnFalse monkeys

let playTurn (monkeys: Monkey array) (id: int) =
        let monkey = monkeys[id]
        let monkeysCurrentCleared =
            monkeys
            |> Array.updateAt monkey.Id { 
                monkey with 
                    Inspections = monkey.Inspections + monkey.Items.Length;
                    Items = []
                }
        
        monkey.Items
        |> List.fold (processItem monkey) monkeysCurrentCleared

let playRound monkeys =
    monkeys
    |> Array.map (fun m -> m.Id)
    |> Array.fold playTurn monkeys

let rec playRounds n monkeys =
    match n with
    | 0 -> monkeys
    | _ -> playRounds (n - 1) (playRound monkeys)

let partOne dataLocation =
    let monkeys = 
        monkeys dataLocation
        |> playRounds 20
        |> Array.sortByDescending (fun m -> m.Inspections)
    
    monkeys[0].Inspections * monkeys[1].Inspections

printfn "Part one (example): %A" <| partOne "data/example.txt"
printfn "Part one (real): %A" <| partOne "data/input.txt"

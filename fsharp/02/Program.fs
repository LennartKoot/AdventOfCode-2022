// For more information see https://aka.ms/fsharp-console-apps

open RockPaperScissors.Input
open RockPaperScissors.Round
open RockPaperScissors.Scores

let foldRoundToTotalScore (score: int) (round: Round) : int =
    let outcome = determineRoundOutcome round
    score
    + (getOutcomeScore outcome |> Score.unwrap)
    + (getShapeScore round.Player |> Score.unwrap)

let determineTotalScore (rounds: Round seq) =
    Seq.fold foldRoundToTotalScore 0 rounds

let exampleInput = loadInput "data/example.txt"
let exampleRounds = convertInputToRounds exampleInput

let realInput = loadInput "data/input.txt"
let realRounds = convertInputToRounds realInput

printfn "Part One"
printfn "Example Data: %A" (determineTotalScore (exampleRounds SecondColumn.PlayerShape))
printfn "Real Data: %A" (determineTotalScore (realRounds SecondColumn.PlayerShape))

printfn "Part Two"
printfn "Example Data: %A" (determineTotalScore (exampleRounds SecondColumn.RoundOutcome))
printfn "Real Data: %A" (determineTotalScore (realRounds SecondColumn.RoundOutcome))

module RockPaperScissors.Scores

open RockPaperScissors.Shapes
open RockPaperScissors.Outcomes

type Score = Score of int
    with 
    static member (+) (a,b) =
        match (a,b) with
        | Score x, Score y -> Score (x + y)
    static member unwrap (Score v) = v

let getShapeScore (shape:Shape) = 
    match shape with
    | Rock ->  Score 1
    | Paper ->  Score 2
    | Scissor ->  Score 3

let getOutcomeScore (outcome: Outcome) =
    match outcome with
    | Win -> Score 6
    | Loss -> Score 0
    | Draw -> Score 3

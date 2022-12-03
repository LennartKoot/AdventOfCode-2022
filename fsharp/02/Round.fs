module RockPaperScissors.Round

open RockPaperScissors.Shapes
open RockPaperScissors.Outcomes

exception InputError of string

type Round = { Opponent:Shape; Player:Shape }

let private determineOpponentShape (column:string) =
    match column with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissor
    | _ -> raise (InputError $"Unknown shape value '{column}' encounterd")

let private determinePlayerShape (column:string) =
    match column with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissor
    | _ -> raise (InputError $"Unknown shape value '{column}' encounterd")

let private foldLineToRounds (rounds:Round list) (line: string) : Round list =
    let columns = line.Split ' '
    let opponent = columns[0]
    let player = columns[1]
    let opponentShape = determineOpponentShape opponent
    let playerShape = determinePlayerShape player
    { Opponent = opponentShape; Player = playerShape } :: rounds


let convertInputToRounds (lines:string array) =
    Array.fold foldLineToRounds [] lines

let determineRoundOutcome (round: Round) =
    match round with
    | { Opponent = Rock; Player = Rock } -> Draw
    | { Opponent = Rock; Player = Paper } -> Win
    | { Opponent = Rock; Player = Scissor } -> Loss
    | { Opponent = Paper; Player = Rock } -> Loss
    | { Opponent = Paper; Player = Paper } -> Draw
    | { Opponent = Paper; Player = Scissor } -> Win
    | { Opponent = Scissor; Player = Rock } -> Win
    | { Opponent = Scissor; Player = Paper } -> Loss
    | { Opponent = Scissor; Player = Scissor } -> Draw

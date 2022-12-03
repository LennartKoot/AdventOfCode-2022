module RockPaperScissors.Round

open RockPaperScissors.Shapes
open RockPaperScissors.Outcomes

exception InputError of string

type Round = { Opponent:Shape; Player:Shape }

// Enum to switch between Part One and Part Two
type SecondColumn =
    | PlayerShape = 1
    | RoundOutcome = 2

let private determineOpponentShape (column:string) =
    match column with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissor
    | _ -> raise (InputError $"Unknown shape value '{column}' encounterd")

let private determinePlayerShapePartOne (column:string) =
    match column with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissor
    | _ -> raise (InputError $"Unknown shape value '{column}' encounterd")

let private determinePlayerShapeByOutcome (opponent: Shape) (outcome: Outcome) =
    match (opponent, outcome) with
    | (Rock, Win) -> Paper
    | (Rock, Loss) -> Scissor
    | (Rock, Draw) -> Rock
    | (Paper, Win) -> Scissor
    | (Paper, Loss) -> Rock
    | (Paper, Draw) -> Paper
    | (Scissor, Win) -> Rock
    | (Scissor, Loss) -> Paper
    | (Scissor, Draw) -> Scissor

let private determinePlayerShapePartTwo (column:string) (opponent:Shape) =
    let f = determinePlayerShapeByOutcome opponent
    match column with
    | "X" -> f Loss
    | "Y" -> f Draw
    | "Z" -> f Win
    | _ -> raise (InputError $"Unknown shape value '{column}' encounterd")

let private foldLineToRounds (secondColumn: SecondColumn) (rounds:Round list) (line: string) : Round list =
    let columns = line.Split ' '
    let opponentShape = determineOpponentShape columns[0]
    let playerShape =
        match secondColumn with
        | SecondColumn.PlayerShape -> determinePlayerShapePartOne columns[1]
        | SecondColumn.RoundOutcome -> determinePlayerShapePartTwo columns[1] opponentShape
        | _ -> raise (InputError $"Unknown value {secondColumn} for secondColumnMeaning")
    { Opponent = opponentShape; Player = playerShape } :: rounds

let convertInputToRounds (lines:string array) (secondColumn: SecondColumn) =
    Array.fold (foldLineToRounds secondColumn) [] lines

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

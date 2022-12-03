module RockPaperScissors.Input

open System.IO

let loadInput (relativePath: string): string array =
    let filename: string = Path.Combine(__SOURCE_DIRECTORY__, relativePath)
    File.ReadAllLines(filename)

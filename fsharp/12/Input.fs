module HillClimbing.Input

open System.IO

let loadInput relativePath =
    let filename: string = Path.Combine(__SOURCE_DIRECTORY__, relativePath)
    File.ReadAllLines(filename)

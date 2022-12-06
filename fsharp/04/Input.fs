module CampCleanup.Input

open System.IO

let loadInput (relativePath: string): string list =
    let filename: string = Path.Combine(__SOURCE_DIRECTORY__, relativePath)
    File.ReadAllLines(filename) |> Array.toList
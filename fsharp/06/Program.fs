open System.IO

// Solution

let readInput (relativePath: string): string =
    let filename: string = Path.Combine(__SOURCE_DIRECTORY__, relativePath)
    File.ReadAllText(filename)

let isMarker s =
    s |> Set.ofSeq |> Set.count = 4

let findMarkerIndex input =
    input
    |> Seq.windowed 4
    |> Seq.findIndex isMarker
    |> (+) 4

// Running and output

let readExampleInput n = readInput $"data/example{n}.txt"

let example = readExampleInput 1 |> findMarkerIndex
let real = readInput "data/input.txt" |> findMarkerIndex

printfn "Part one"
printfn "Example Data: %A" example
printfn "Real Data: %A" real
printfn ""
open System.IO

// Solution

let readInput (relativePath: string): string =
    let filename: string = Path.Combine(__SOURCE_DIRECTORY__, relativePath)
    File.ReadAllText(filename)

let isMarker n s =
    s |> Set.ofSeq |> Set.count = n

let findMarkerIndex n input =
    input
    |> Seq.windowed n
    |> Seq.findIndex (isMarker n)
    |> (+) n

let findStartOfPacketMarker = findMarkerIndex 4
let findStartOfMessageMarker = findMarkerIndex 14

// Running and output

let readExampleInput n = readInput $"data/example{n}.txt"

let example1 = readExampleInput 1 |> findStartOfPacketMarker
let real1 = readInput "data/input.txt" |> findStartOfPacketMarker

printfn "Part one"
printfn "Example Data: %A" example1
printfn "Real Data: %A" real1
printfn ""

let example2 = readExampleInput 1 |> findStartOfMessageMarker
let real2 = readInput "data/input.txt" |> findStartOfMessageMarker

printfn "Part two"
printfn "Example Data: %A" example2
printfn "Real Data: %A" real2
printfn ""

open HillClimbing.Dijkstra
open HillClimbing.Dijkstra.Graph
open HillClimbing.Input

let find2D elem (arr: 'a array array) =
    let rec go x y =
        if   y >= arr.Length then None
        elif x >= arr[y].Length    then go 0 (y + 1)
        elif arr[y][x] = elem   then Some (x, y)
        else go (x+1) y
    go 0 0

let createNode (location as (x, y)) (chars: char array array) =
    let vertex = Vertex location
    let char = chars[y][x]

    let canTravelTo (x, y) =
        let determineCharLevel c =
            match c with
            | 'S' -> 'a'
            | 'E' -> 'z'
            | _ -> c
        if x < 0 || y < 0 || y >= chars.Length || x >= chars[y].Length then
            false
        else
            (int (determineCharLevel char) - int (determineCharLevel (chars[y][x]))) >= -1
    
    let edges = 
        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]
        |> List.fold (fun acc neigbor ->
            if canTravelTo neigbor then
                Edge (neigbor, Weight 1) :: acc
            else acc
            ) []
    Node (vertex, edges)

let createNodes chars =
    chars
    |> Array.mapi (fun y xs -> Array.mapi (fun x _ -> createNode (x,y) chars) xs)
    |> Array.reduce Array.append

let graph dataLocation =
    let chars = 
        loadInput dataLocation
        |> Array.map Array.ofSeq
    let graph =
        chars
        |> createNodes
        |> Array.fold (fun graph node -> graph |> addNode node) Graph.empty
    let startPosition = match find2D 'S' chars with | None -> failwith "S not found" | Some pos -> pos
    let endPosition = match find2D 'E' chars with | None -> failwith "E not found" | Some pos -> pos
    (graph, startPosition, endPosition)

let partOne dataLocation =
    let (graph, start, target) = graph dataLocation
    let (_, prev) = Algorithm.dijkstra start graph
    Algorithm.findPath prev start target
    |> List.length

printfn "Part one (example): %A" <| partOne "data/example.txt"
printfn "Part one (real): %A" <| partOne "data/input.txt"
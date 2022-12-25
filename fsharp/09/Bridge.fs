module RopeBridge.Bridge

type Position = int * int
type Bridge = {
    Head: Position
    Tail: Position
    Visited: Set<Position>
}

type Motion =
    | Up of int
    | Down of int
    | Left of int
    | Right of int

let create = {
    Head = (0, 0);
    Tail = (0, 0);
    Visited = Set.empty |> Set.add (0, 0)
}

let rec executeMotion bridge motion =
    let newHeadPosition =
        let (hx, hy) = bridge.Head
        match motion with
        | Up _ -> (hx, hy + 1)
        | Down _ -> (hx, hy - 1)
        | Left _ -> (hx - 1, hy)
        | Right _ -> (hx + 1, hy)
    
    let headTailTouch (head: Position) (tail: Position) =
        let (hx, hy) = head
        let (tx, ty) = tail
        head = tail || // Same position
        (hx = tx && abs (hy - ty) = 1) || // Up or down
        (hy = ty && abs (hx - tx) = 1) || // Left or right
        (abs (hx - tx) = 1 && abs (ty - hy) = 1) // Diagonal

    let newTailPosition =
        if headTailTouch newHeadPosition bridge.Tail
        then bridge.Tail
        else
            let (hx, hy) = newHeadPosition
            match motion with
            | Up _ -> (hx, hy - 1)
            | Down _ -> (hx, hy + 1)
            | Left _ -> (hx + 1, hy)
            | Right _ -> (hx - 1, hy)

    let newBridge = { 
        bridge with
            Head = newHeadPosition
            Tail = newTailPosition
            Visited = bridge.Visited |> Set.add newTailPosition
    }

    let newMotion = 
        let newOption s m : Motion option = if s = 1 then None else Some (m (s - 1))
        match motion with
        | Up m -> newOption m Up
        | Down m -> newOption m Down
        | Left m -> newOption m Left
        | Right m -> newOption m Right

    match newMotion with
    | None -> newBridge
    | Some m -> executeMotion newBridge m

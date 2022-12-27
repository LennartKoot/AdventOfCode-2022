module RopeBridge.Bridge

type Position = int * int
type Bridge = {
    Knots: Position list
    Visited: Position Set
}

type Motion =
    | Up of int
    | Down of int
    | Left of int
    | Right of int

let create knots = {
    Knots = List.replicate knots (0,0)
    Visited = Set.empty |> Set.add (0, 0)
}

type private DisconnectedDirection =
    | Top | Bottom | Left | Right
    | TopLeft | TopRight | BottomLeft | BottomRight

let rec executeMotion (bridge: Bridge) motion =
    let moveHead head =
        let (hx, hy) = head
        match motion with
        | Motion.Up _ -> (hx, hy + 1)
        | Motion.Down _ -> (hx, hy - 1)
        | Motion.Left _ -> (hx - 1, hy)
        | Motion.Right _ -> (hx + 1, hy)
    
    let doKnotsTouch (a: Position) (b: Position) =
        let (ax, ay) = a
        let (bx, by) = b
        a = b || // Same position
        (ax = bx && abs (ay - by) = 1) || // Up or down
        (ay = by && abs (ax - bx) = 1) || // Left or right
        (abs (ax - bx) = 1 && abs (ay - by) = 1) // Diagonal

    let determineDisconnectedDirection head tail =
        let (hx,hy),(tx,ty) = head,tail
        let top = hy - ty = -2
        let bottom = hy - ty = 2
        let left = hx - tx = 2
        let right = hx - tx = -2
        match top, bottom, left, right with
        | true, _,    true, _     -> TopLeft
        | true, _,    _,    true  -> TopRight
        | _,    true, true, _     -> BottomLeft
        | _,    true, _,    true  -> BottomRight
        | true, _,    _,    _     -> Top
        | _,    true, _,    _     -> Bottom
        | _,    _,    true, _     -> Left
        | _,    _,    _,    true  -> Right
        | _ -> failwith $"Unexpected position {head}, {tail}"
    let foldKnotsToNewPositions acc knot =
        match acc with
        | [] -> moveHead knot :: acc
        | head::_ -> 
            if doKnotsTouch head knot
            then knot :: acc
            else
                let (hx, hy) = head
                let newTailPosition =
                    let top = hy + 1
                    let bottom = hy - 1
                    let left = hx - 1
                    let right = hx + 1 
                    match determineDisconnectedDirection head knot with
                    | Bottom -> (hx, bottom)
                    | Top -> (hx, top)
                    | Right -> (right, hy)
                    | Left -> (left, hy)
                    | TopLeft -> (left, top)
                    | TopRight -> (right, top)
                    | BottomLeft -> (left, bottom)
                    | BottomRight -> (right, bottom)
                newTailPosition :: acc

    let newBridge =
        let newKnotsPositions = List.fold foldKnotsToNewPositions [] bridge.Knots 
        { 
            bridge with
                Knots = newKnotsPositions |> List.rev
                Visited = bridge.Visited |> Set.add (newKnotsPositions |> List.head)
        }

    let newMotion = 
        let newOption s m : Motion option = if s = 1 then None else Some (m (s - 1))
        match motion with
        | Motion.Up m -> newOption m Motion.Up
        | Motion.Down m -> newOption m Motion.Down
        | Motion.Left m -> newOption m Motion.Left
        | Motion.Right m -> newOption m Motion.Right

    match newMotion with
    | None -> newBridge
    | Some m -> executeMotion newBridge m

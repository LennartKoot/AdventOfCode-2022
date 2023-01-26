namespace HillClimbing.Dijkstra
    module Graph =
        type Weight = Weight of int
        type Vertex<'TId> = Vertex of 'TId
        type Edge<'TId> = Edge of 'TId * Weight
        type Node<'TId> = Node of Vertex<'TId> * Edge<'TId> list

        type Graph<'TId when 'TId : comparison> =
            private T of Map<'TId,Node<'TId>>

        let empty: Graph<'TId> = T Map.empty
        let addNode (node as (Node (Vertex id, _))) (T graph)  =
            graph |> Map.add id node |> T
        let getNode id (T graph)=
            graph |> Map.find id
        let getAllVertices (T graph) = graph |> Map.fold (fun acc _ (Node (vertex, _)) -> vertex :: acc) []

        let reverse (T graph) = 
            graph
            |> Map.fold (fun (acc: Map<'TId,Node<'TId>>) _ (Node ((Vertex vertexId), neigbors)) ->
                 neigbors
                 |> List.fold (fun acc (Edge (id, weight)) ->
                    let newVertex = Vertex id
                    let newEdge = Edge (vertexId, weight)
                    match Map.tryFind id acc with
                    | None -> Map.add id (Node (newVertex, [newEdge])) acc
                    | Some (Node (_, edges)) -> Map.add id (Node (newVertex, (newEdge :: edges))) acc
                 ) acc
            ) Map.empty
            |> T

    module Algorithm =
        open Graph
        open HillClimbing.MinPriorityQueue
        open System

        [<CustomComparison;CustomEquality>]
        type private QueueElem<'TId when 'TId : comparison> =
            | E of 'TId * int

            override x.Equals(yobj) =
                let equalsInternal (E (xId,xPrio)) (E (yId,yPrio)) = xId = yId && xPrio = yPrio
                match yobj with
                | :? QueueElem<'TId> as y -> equalsInternal x y
                | _ -> false
            override x.GetHashCode() = hash x
            interface IComparable with
                member x.CompareTo yobj =
                    let compareInternal (E (_,xPrio)) (E (_,yPrio)) =
                        compare xPrio yPrio
                    
                    match yobj with
                    | :? QueueElem<'TId> as y -> compareInternal x y
                    | _ -> invalidArg "yobj" "Cannot compare value of different types"

        let dijkstra (source: 'TId) (graph: Graph<'TId>) =
            let rec dijkstraInternal (graph: Graph<'TId>) (dist: Map<'TId,int>) (prev: Map<'TId,'TId option>) (inQ: Set<'TId>) Q =
                if BinomialQueue.isEmpty Q then
                    dist, prev
                else
                    let (E (u,_)) = BinomialQueue.findMin Q
                    let distU = dist |> Map.find u
                    let (Node (_,neigbors)) = getNode u graph
                    let (newDist, newPrev, newInQ, newQ) =
                        neigbors
                        |> List.fold (
                            fun (dist, prev, inQ, q) (Edge (v,Weight weight)) ->
                            let alt = distU + weight
                            let isAltSmaller = alt < (dist |> Map.find v)
                            let isVInQueue = Set.contains v inQ
                            if isAltSmaller && not isVInQueue then
                                // Map.add replaces existing values
                                dist |> Map.add v alt,
                                prev |> Map.add v (Some u),
                                Set.add v inQ,
                                q |> BinomialQueue.insert (E (v,alt))
                            else if isAltSmaller then
                                // Map.add replaces existing values
                                dist |> Map.add v alt,
                                prev |> Map.add v (Some u),
                                inQ,
                                q
                            else dist, prev, inQ, q
                        ) (dist, prev, (Set.remove u inQ), BinomialQueue.deleteMin Q)
                    dijkstraInternal graph newDist newPrev newInQ newQ

            let allVertices = getAllVertices graph
            let (dist, prev) : Map<'TId, int> * Map<'TId, 'TId option> =
                allVertices
                |> List.fold (fun (dist, prev) (Vertex v) ->
                    if v <> source then 
                        (dist |> Map.add v Int32.MaxValue, prev |> Map.add v None)
                    else
                        (dist |> Map.add v 0, prev |> Map.add v None)
                    ) (Map.empty, Map.empty)

            let Q =
                BinomialQueue.empty<QueueElem<'TId>>
                |> BinomialQueue.insert (E (source, dist |> Map.find source))

            dijkstraInternal graph dist prev (Set.empty |> Set.add source) Q

        let findPath (prev: Map<'TId,'TId option>) (source:'TId) (target:'TId) =
            let rec findPathInternal prev source target S =
                let u = prev |> Map.find target
                match u with
                | Some uId ->
                    if (uId = source) then 
                        S
                    else
                        findPathInternal prev source uId (uId :: S)
                | _ -> failwith $"No path found from {source} to {target}"
            findPathInternal prev source target [target]

namespace HillClimbing.MinPriorityQueue

    (*
        Implemented according to 'Optimal Purely Functional Priority Queues'
        by Gerth Stølting Brodal and Chris Okasaki
        from BRICS Report Series RS-96-37 (https://www.brics.dk/RS/96/37/BRICS-RS-96-37.pdf)
    *)
    module BinomialQueue =
        type BinomialQueue<'E when 'E : comparison> = private T of Tree<'E> list
        and private Rank = int
        and private Tree<'E> = Node of 'E * Rank * Tree<'E> list

        let private unwrap (T ts) = ts
        let private root (Node (x,_,_)) = x
        let private rank (Node (_,r,_)) = r
        let private link (t1 as Node (x1,r1,c1)) (t2 as Node (x2,r2,c2)) =
            if x1 <= x2 then Node (x1, r1 + 1, t2 :: c1) else Node (x2, r2 + 1, t1 :: c2)
        let rec private ins (t: Tree<'a>) (trees: Tree<'a> list) =
            match trees with
            | [] -> [t]
            | t'::ts -> if rank t < rank t' then t :: trees else ins (link t t') ts

        let empty<'E when 'E : comparison> = T List.empty<Tree<'E>>
        
        let isEmpty (T ts) = ts = []
        
        let insert x (T ts) = T <| ins (Node (x,0,[])) ts

        let rec meld (T ts1 as q1) (T ts2 as q2) =
            match ts1,ts2 with
            | [], _ -> q2
            | _, [] -> q1
            | (t1::ts1), (t2::ts2) ->
                if rank t1 < rank t2 then T (t1 :: (unwrap <| meld (T ts1) q2))
                else if rank t2 < rank t1 then T (t2 :: (unwrap <| meld q1 (T ts2)))
                else T (ins (link t1 t2) (unwrap <| meld (T ts1) (T ts2)))

        exception Empty

        let rec findMin (T q) =
            match q with
            | [] -> raise Empty
            | t::[] -> root t
            | t::ts ->
                let x = findMin (T ts)
                if root t <= x then root t else x

        let deleteMin (T q) =
            match q with
            | [] -> raise Empty
            | ts ->
                let rec getMin ts =
                    match ts with
                    | t'::[] -> (t', [])
                    | t'::ts' ->
                        let (t'', ts'') = getMin ts
                        if root t' < root t'' then (t', ts') else (t'', t'::ts'')
                    | _ -> failwith "Can't happen"
                let (Node (x,r,c), ts) = getMin ts
                meld (T (c |> List.rev)) (T ts)

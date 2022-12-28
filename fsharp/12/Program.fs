open HillClimbing.MinPriorityQueue.BinomialQueue

empty<int>
|> insert 5
|> insert 10
|> insert 3
|> insert 8
|> insert 2
|> insert 7
|> insert 8
|> insert 9
|> findMin
|> printfn "%A"


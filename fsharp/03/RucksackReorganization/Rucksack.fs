module RucksackReorganization.Rucksack

let private compartmentHalfIndex r = (String.length r) / 2

let private splitRucksackIntoCompartments r =
    let half = compartmentHalfIndex r
    r[..(half - 1)], r[half..]

let private mapTuple f (a, b) = (f a, f b)

let determineDuplicateItems (rucksacks: string array) =
    rucksacks
    |> Array.map splitRucksackIntoCompartments
    |> Array.map (mapTuple Seq.toList)
    |> Array.map (mapTuple Set.ofList)
    |> Array.map ((<||) Set.intersect)

type Item = string * float * int // name,weight,utility

/// determine overall utility of knspsack contents x
let totUtility (x:Item List) = List.sumBy (function | (_, _, util) -> util) x

/// determine overall weight of knapsack contents x
let totWeight (x:Item List) = List.sumBy (function | (_, weight,_) -> weight) x
let optimise maxWeight (sets: Item List Set)  =
    sets
    |> Set.map (fun x -> totWeight x, x)
    |> Set.filter (fun (w,_) -> w < maxWeight)
    |> Set.map (fun (w,x) -> totUtility x, w, x)
    |> Seq.maxBy (fun (u,_,_) -> u)

/// generate solutions using recursive subfunction search'
let search1 maxWeight items =
    let searchFolder lists nextItem = Set.map (fun lst ->  nextItem :: lst) lists + lists
    Set.fold searchFolder (set [[]]) items
    |> optimise maxWeight

/// generate solutions using fold
let search2 maxWeight items =
    let addPossItemToLists sets item = Set.map (fun lst -> Set [lst ; item :: lst]) sets |> Set.unionMany
    Set.fold addPossItemToLists  (set [[]]) items  
    |> optimise maxWeight

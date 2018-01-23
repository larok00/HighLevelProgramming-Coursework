module Q37



type Item = string * float * int // name,weight,utility

/// determine overall utility of knspsack contents x
let totUtility (x:Item list) = List.sumBy (function | (_, _, util) -> util) x

/// determine overall weight of knspsack contents x
let totWeight (x:Item list) = List.sumBy (function | (_, weight,_) -> weight) x



/// return the optimum solution from those with weight less than or equal to maxW
let optimise (maxW:float) (solutions: Item list list) = 
    let pickOptimum i j = 
        if totUtility i>totUtility j then i  else 
            if totWeight j <= maxW then j else i

    List.reduce pickOptimum solutions

/// Given items return optimum solution for knapsack with these
/// This function generates the set of all 2^N possible solutions
/// and then uses optimise
let search (maxW:float) (items: Item list) = 
    let reducer (listOfListsOfCombos: Item list list) (nextItem: Item) = 
        listOfListsOfCombos @ List.map (fun combo -> combo @ [nextItem]) listOfListsOfCombos

    items 
        |> List.fold reducer [[]] 
        |> optimise maxW

#load "Q37.fs"

open Q37


let rnd = System.Random()

/// Generate a list of numitems items with random weights and utilities for testing
/// Utilities are in the range 1..10
/// Weights are in the range 0.0 - 1.0
let genTestItems numItems =
    [1..numItems] |> List.map (fun i -> (sprintf "Item%d" i), rnd.NextDouble(), rnd.Next(1,10) )

let testItems = genTestItems 5
let maxW = (totWeight testItems) * rnd.NextDouble()

let result = search (maxW:float) (testItems: Item list)

printfn "%s %A of total weight allowed." 
    "%" (maxW/totWeight testItems * 100.0)
printfn "%s %A of total utility achieved." 
    "%" (float(totUtility result)/float(totUtility testItems) * 100.0)
printfn "%s %A of weight allowance used." 
    "%" (totWeight result/maxW)

result

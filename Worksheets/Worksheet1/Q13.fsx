#load "Q12.fsx"

open Q12

let deleteBadPairs x = List.filter ( fun (a,b) -> a<b ) x
let nonredundantMultiList = twoTuples |> deleteBadPairs |> multiList
let printTriple x = List.map printMultiList x
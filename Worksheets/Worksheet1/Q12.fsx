module Q12

#load "Q1.fs"
#load "Q7.fs"


open Q1
open Q7

let intList = [1..5]
let twoTuples = pairList intList
let multiList x = List.map (fun p -> p, makeTriple p) x

let printMultiList x = printfn "%A" x

module Q11

// takes no parameters, lst, a0 and b are all abstracted
let makePairs1 = fun lst -> fun a0 -> List.map (fun b -> (a0,b)) lst

// makePairs is partially applied, with lst set and a0 undefined
let pairList lst = List.collect (makePairs1 lst) lst

#load "Q1.fs"
open Q1

let intList = [1..5]
let tripleList = List.map makeTriple (pairList intList)

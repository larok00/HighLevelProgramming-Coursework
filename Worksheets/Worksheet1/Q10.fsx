module Q10

#load "Q1.fs"
#load "Q7.fs"


open Q1
open Q7

let intList = [1..5]
let tripleList = List.map makeTriple (pairList intList)

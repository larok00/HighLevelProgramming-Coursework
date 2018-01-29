#load "Q6.fsx"
#load "Q2.fs"
#load "Q5.fsx"

open Q2
open Q6
open Q5

let c16 f = cTimes c4 c4 f

#load "Q8.fsx"

open Q8

printfn "%A" (eval (cPred c16))
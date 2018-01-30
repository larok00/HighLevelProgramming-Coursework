module Q9

#load "Q8.fsx"

open Q8

let cSubtract cM cN f x = cM (cPred) (cN f x)
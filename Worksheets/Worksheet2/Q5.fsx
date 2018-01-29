module Q5

#load "Q2.fs"

open Q2

let cPlus cM cN f x = (cM f) (cN f x)

let cTimes cM cN f x = cM (cN f) x
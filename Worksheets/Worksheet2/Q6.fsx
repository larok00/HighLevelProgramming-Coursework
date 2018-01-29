module Q6

#load "Q2.fs"

open Q2

let eval ch = ch (fun x ->x+1) 0
let show ch = ch (fun x -> "1+"+x) "0"
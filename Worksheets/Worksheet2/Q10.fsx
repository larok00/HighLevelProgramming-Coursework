#load "Q2.fs"
#load "Q6.fsx"
#load "Q9.fsx"

open Q2
open Q6
open Q9

let s00 f  = cSubtract c0 c0 f  // works, f needed to prevent value restriction

let s10 f  = cSubtract c1 c0 f   // works, f  needed as above

//let s22 f = cSubtract c2 c2 // does not type check! the inferred type for s22 is too specific. 


eval s00 // works!
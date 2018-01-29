#load "Q5.fsx"

open Q5

let c8 f = cPlus c4 c4 f
let c16 f = cTimes c4 c4 f


//let c8' = cPlus c4 c4
// doesn't work because uncertain that F# has inferred the type correctly.
// Value restriction is an interesting and subtle aspect of the F# type system.
// It is introduced because F# allows assignment as well as pure functions.

printfn "%A" (c8 (fun x -> 2*x) 1)

printfn "%A" (c16 (fun x -> 2*x) 1)
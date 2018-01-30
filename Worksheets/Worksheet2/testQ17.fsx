#load "Q17.fs"

open Q17

// check that iPred i3 has the correct function
printfn "%A" <| (iPred i3).Apply (fun n -> n+1) 0 

printfn "%A" <| iShow (iPred i3) // check that iShow works

printfn "%A" <| iEval (iPred i3) // check that iEval works
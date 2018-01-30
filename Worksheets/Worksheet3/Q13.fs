type RListNodeBad = {Head: int ; Tail: RListBad}
and RListBad = | RNode of RListNodeBad | RNil

let x = (RNode {Head=1 ; Tail=RNode {Head=2 ; Tail=RNode {Head=3 ; Tail=RNil}}})

printfn "%A" x
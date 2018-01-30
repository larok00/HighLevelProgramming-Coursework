let cFalse: 'T -> 'T -> 'T = fun a b -> b
let cTrue: 'T -> 'T -> 'T  = fun a b -> a

let testBoolean () =
    let b : 'T -> 'T -> 'T = cTrue // b is either cTrue or cFalse
    let s1 = b "aa" "bb"
    let s2 = b 0 1
    (s1,s2)

/// b is either cTrue or cFalse
let testBoolean1 b =
    let s1 = b "aa" "bb"
    let s2 = b 0 1
    (s1,s2)
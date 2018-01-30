module Q14

type IBool =
    abstract If : 'T -> 'T -> 'T

let iTrue = 
    { new IBool with
        member __.If(tp: 'a) (ep: 'a): 'a =  tp
    }

let iFalse = 
    { new IBool with
        member __.If(tp: 'a) (ep: 'a): 'a = ep
    }

let testBoolean1 (b:IBool) =
    let s1 = b.If "aa" "aaa"
    let s2 = b.If 0 1
    (s1,s2)
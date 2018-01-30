module Q11

type IChurchNumber =
    abstract Apply : ('a -> 'a) -> 'a -> 'a

let iEval (n : IChurchNumber) =
    n.Apply ( (+) 1) 0

let iShow (n : IChurchNumber) = 
    n.Apply ( (+) "1+") "0"
    |> printf "%s"


let i0 = 
    { new IChurchNumber with
        member __.Apply(_: 'a -> 'a) (z: 'a): 'a =  z
    }

let iSucc (n : IChurchNumber) =
    { new IChurchNumber with
        member __.Apply(f: 'a -> 'a) (x: 'a): 'a =  
              f (n.Apply f x)
    }


let i1 = iSucc i0
let i2 = iSucc i1
let i3 = iSucc i2
// etc


// try to create i4 as (i2 i2) analogous to c4 = c2 c2
// let i4Bad = i2.Apply i2
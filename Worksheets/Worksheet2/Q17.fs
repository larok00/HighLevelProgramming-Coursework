module Q17

// code needed to implement Church numerals with F# interface

/// Interface for Church numerals
type IChurchNumber =
    /// method which implements applying the numeral
    abstract Apply : ('a -> 'a) -> 'a -> 'a

let iEval (n : IChurchNumber) =
    n.Apply ( (+) 1) 0

let iShow (n : IChurchNumber) = 
    n.Apply ( (+) "1+") "0"
    |> printf "%s"

/// Church numeral for 0
let i0 = 
    { new IChurchNumber with
        member __.Apply(_: 'a -> 'a) (z: 'a): 'a =  z
    }

/// Returns Church numeral 1 more than n
let iSucc (n : IChurchNumber) =
    { new IChurchNumber with
        member __.Apply(s: 'a -> 'a) (z: 'a): 'a =  s (n.Apply s z)
    }

let i1 : IChurchNumber = iSucc i0
let i2 : IChurchNumber = iSucc i1
let i3 : IChurchNumber = iSucc i2

/// Returns Church numeral 1 less than n
let iPred (n : IChurchNumber) =
    { new IChurchNumber with
        member __.Apply(f: 'a -> 'a) (x: 'a): 'a =
            let f1 g h = h (g f)
            let f2 _ = x
            n.Apply f1 f2 id
    }

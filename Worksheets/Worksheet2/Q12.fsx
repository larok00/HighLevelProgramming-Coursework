module Q12

#load "Q11.fs"

open Q11

let iPred'  f (b,x) = 
    match b with
    | true -> (true, f x)
    | false -> (true, x)

let iPred (n: IChurchNumber) = 
    { new IChurchNumber with
        member __.Apply(f: 'a -> 'a) (x: 'a): 'a =  snd (n.Apply (iPred' f) (false,x))
    }


let iSubtract (m: IChurchNumber) (n: IChurchNumber) = m.Apply iPred n
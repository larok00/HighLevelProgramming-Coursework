module Q8

#load "Q2.fs"

open Q2

let pair a b = (a,b)
let fst (a,b) = a
let snd (a,b) = b

/// The first application of (cSucc' f) should be on a tuple (false, x0)
/// The second and onwards applications of (cSucc' f) will have b = true
/// Applying (cSucc f) n times has the effect of applying f (n-1) times to x0.
let cSucc' f (b,x) = if b then (b, f x) else (true, x)

let cPred cN f x = snd (cN (cSucc' f) (false,x))
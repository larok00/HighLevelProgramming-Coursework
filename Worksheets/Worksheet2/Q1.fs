module Q1

let c0 f x = x
let c1 f x = f x
let c2 f x = f (f x)
let c3 f x = f (f (f x))
let c4 f x = f (f (f (f x)))

// increment or successor function
let cSucc cn f x = f (cn f x) 
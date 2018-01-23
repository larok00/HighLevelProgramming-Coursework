module Q33

let tester = 
    let n = 17
    let makeIncrementer x =
        let n = 2*x
        let incr i = i+n
        incr // incr is a closure with bound n
    let f1 = makeIncrementer 10
    let f2 = makeIncrementer 20
    printfn "n1 = %d" n
    let n = 3
    printfn "f1 1 = %d" (f1 1)
    printfn "f2 3 = %d" (f2 3)
    printfn "n2 = %d" n

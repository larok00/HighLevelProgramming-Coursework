module Q7

let makePairs lst a0 = List.map (fun b -> (a0,b)) lst // note that makePairs has two parameters

// makePairs is partially applied, with lst set and a0 undefined
let pairList lst = List.collect (makePairs lst) lst
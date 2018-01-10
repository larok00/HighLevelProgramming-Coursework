(*
let pairList = List.collect (fun a -> List.map (fun b -> (a,b)) [1..5]) [1..5]
let makeTriple (a,b) =
    let sq x = x*x
    (sq a - sq b, 2*a*b, sq a + sq b)
let tripleList = List.map makeTriple pairList
printfn "Triples are:%A" tripleList
*)


let sq x = x*x

let solution = [1..5] 
            |> List.collect ( fun a -> ([1..5] |> List.map (fun b -> (a,b))) ) 
            |> List.map ( fun (a,b) -> ( ((a |> sq)-(b |> sq)), ((2*a*b)), ((a |> sq)+(b |> sq))) ) 
            |> printfn "Triples are:%A"
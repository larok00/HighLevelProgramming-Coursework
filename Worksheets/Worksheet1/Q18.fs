module Q18

let sequences a b = 
    [a..b] |> List.allPairs [a..b] |> List.collect (fun (i, j) -> if i <= j then [[i..j]] else [])

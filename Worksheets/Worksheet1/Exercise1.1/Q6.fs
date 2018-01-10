module Q6

let retainPositive lst = List.collect (fun a -> if a > 0 then [a] else []) lst

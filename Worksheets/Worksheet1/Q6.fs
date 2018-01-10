module Q6

// filters the list to only positive numbers
let retainPositive lst = List.collect (fun a -> if a > 0 then [a] else []) lst

module Q20

let insertElement1 (lst: 'a list) x n = lst.[0..n-1] @ [x] @ lst.[n..List.length lst-1]

let insertList1 (lst: 'a list) xLst n = lst.[0..n-1] @ xLst @ lst.[n..List.length lst-1]
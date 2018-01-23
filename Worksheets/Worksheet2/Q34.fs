module Q34

let length1 lst =
    List.fold (fun n _ -> n+1) 0 lst

module Q36

let rev2 lis = 
    List.fold (fun lst h -> h :: lst) [] lis
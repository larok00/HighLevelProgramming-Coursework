module Q29

let map2b f lis =
   let rec map2b' ret lis =
      match lis with
      | [] -> ret
      | h :: tl -> map2b' (ret @ [f h]) tl
   map2b' [] lis

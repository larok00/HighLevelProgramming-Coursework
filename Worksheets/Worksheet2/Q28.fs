module Q28

let map2a f lis =
   let rec map2a' ret lis =
      match lis with
      | [] -> ret
      | h :: tl -> map2a' (f h :: ret) tl
   map2a' [] lis

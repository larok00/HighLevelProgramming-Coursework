module Q32

let mapTailRecRev f lis =
   let rec map2c' ret lis =
      match lis with
      | [] -> List.rev ret
      | h :: tl -> map2c' (f h :: ret) tl
   map2c' [] lis

let mapTailRecAppend f lis =
   let rec map2a' ret lis =
      match lis with
      | [] -> ret
      | h :: tl -> 
          map2a' (List.append ret [f h]) tl
   map2a' [] lis

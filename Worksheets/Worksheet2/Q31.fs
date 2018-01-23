module Q31

let rev1 lis =
   let rec rev1' ret lis =
      match lis with
      | [] -> ret
      | h :: tl -> rev1' (h :: ret) tl
   rev1' [] lis

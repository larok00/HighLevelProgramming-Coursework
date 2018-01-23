module Q25

let rec map1 f lis =
   match lis with
   | h :: tl -> (f h) :: map1 f tl

#load "Q20.fs"

open Q20

printfn "%A" (insertElement1 [0; 1; 3; 4] 2 2)
System.Console.ReadLine() // prevent the program from terminating

printfn "%A" (insertList1 [0; 2; 4; 16] [8] 3)
System.Console.ReadLine() // prevent the program from terminating

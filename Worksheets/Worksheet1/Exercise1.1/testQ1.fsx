#load "Q1.fs"

// test code
open Q1

pythagTriples
printfn "Look at this Pythagorean triple: %A" (makeTriple(3,4))
System.Console.ReadLine() // prevent the program from terminating

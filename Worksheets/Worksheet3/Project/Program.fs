open System
open FsCheck
open Expecto

//Q27
let ``de Morgan's Theorem Fails`` a b = 
    // fails, = binds more tightly than && so that this reads as a && (b = not (not a) || (not b))
    //a && b = not ((not a) || (not b))

    (a && b) = not ((not a) || (not b))

//Q29
let associativity (x:int) (f:int->float,g:float->char,h:char->int) =
    // true for all functions, since function composition is associative.
    ((f >> g) >> h) x = (f >> (g >> h)) x

//Q30
let revOfRevIsOrig (x: int list) = List.rev (List.rev x) = x

let listsAreSmall (x:int list) = List.length x < 10 // this test should fail

//Q31
// Tests are written as individual Expecto Test values, each tagged with [<Tests>]
// The code demonstrates different ways to run Expecto and FsCheck tests

[<Tests>]
let simpleExpectoTest =
    testCase "A simple test" <| fun () ->
    let expected = 4
    Expect.equal (2+2) expected "2+2 = 4"

[<Tests>]
let expectoFsCheckTest1 = 
    testProperty "Reverse of reverse of a list is the original list" <|
        fun (xs:list<int>) -> List.rev (List.rev xs) = xs
        // NB this is revOfRevIsOrig written out as anonymous function

[<Tests>]
let expectoFsCheckTest2 = 
    testProperty "Lists are small" <|
        listsAreSmall

let fsCheckConfig = { FsCheckConfig.defaultConfig with maxTest = 10000 }

[<Tests>]
let expectoCheckTestFsCheckWithConfig1 =
    // you can also override the FsCheck config
    testPropertyWithConfig fsCheckConfig "Product is distributive over addition" <|
        fun a b c ->
        a * (b + c) = a * b + a * c

let testsWithoutExpecto() =
    Check.Quick revOfRevIsOrig
    Check.Quick listsAreSmall
    Check.Quick (4 = 2+2)

let testListWithExpecto =
    testList "A test group" [
        simpleExpectoTest
        expectoFsCheckTest1
        expectoFsCheckTest2
        expectoCheckTestFsCheckWithConfig1
    ]

let testsWithExpecto() =
    runTests defaultConfig testListWithExpecto |> ignore

//Q32
let allTestsWithExpecto() =
    runTestsInAssembly defaultConfig [||]

//Q33
// lst must be annotated with its type, 
// because the method does not uniquely define the type of the object it is used on. 
// This is the main disadvantage of using OOP style.
let getEndsOf (lst: string list) =
    match lst.Length with
    | n when n < 2 -> ""
    | _ -> lst.[0] + lst.[lst.Length - 1]


[<EntryPoint>]
let main argv =
    //Check.Quick ``de Morgan's Theorem Fails``
    //Check.Quick associativity
    //printfn "Testing with FSCheck and Expecto!"
    //testsWithoutExpecto() |> ignore
    //testsWithExpecto() |> ignore
    //allTestsWithExpecto() |> ignore

    let x = [1;2;3]
    printfn "%s" (x.ToString())
    Console.ReadKey() |> ignore
    0 // return an integer exit code

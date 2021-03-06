// Learn more about F# at http://fsharp.org

module Program

open System
open EEExtensions
open Tokenize
open Parse
open BracketAbstract
open Reduce
open Expecto.ExpectoFsCheck
open Expecto

//Allowed data output types for Tests
type OUT = | TL of Token list
           | AST of Exp //Non Bracket Reduced
           | ASTBR of Exp // Bracket Reduced
           | RS of string

//Composite Functions
let T = tokenize
let TP = tokenize >> parse
let TPB = tokenize >> parse >> removeLambdas
let TPB' = tokenize >> parse' >> Result.map removeLambdas

//Last stage includes the default params for reduce function
let TPBR =  tokenize >> parse >> removeLambdas >> (fun exp -> (reduce 1000 exp []))
let TPBR' =  tokenize >> parse' >> Result.map removeLambdas >> Result.map (fun exp -> (reduce 1000 exp []))

/// Convert lambda AST string into combinator & list of args form
/// wrapped in Reduced: as generated by reduce
let TPBmap txt =
    let rec TPBmap' argL = function
        | Apply(f, arg)  -> TPBmap' (arg :: argL) f
        | co -> Reduced(co, argL)
    TPBmap' [] (TPB txt)


    

//Main higher order test function
let makeTest (name : string) (inp : string) (output : OUT) =
    testCase name <| fun () ->
    match output with
        | OUT.TL(tl)        -> Expect.equal (T(inp)) tl (sprintf "Tokenize '%s'" inp)
        | OUT.AST(exp)      -> Expect.equal (TP(inp)) exp (sprintf "Parse '%s'" inp)
        | OUT.ASTBR(expBr)  -> Expect.equal (TPB'(inp)) (Result.Ok(expBr)) (sprintf "Bracket Abstract '%s'" inp)
        | OUT.RS(rs)        -> Expect.equal (TPBR'(inp)) (Result.Ok(TPBmap rs)) (sprintf "Reduce '%s'" inp)


[<Tests>]
let t1 =
    Expecto.Tests.testList "Tokenizer Tests"
            [
                makeTest "Token2" "" (OUT.TL [END])
                makeTest "Token1" "x" (OUT.TL [SYM ("x",0);END])
                makeTest "Token3" @"\x.()" (OUT.TL [LAMBDA 0 ; SYM("x",1); DOT 2; LBRA 3; RBRA 4; END])
            ]
            

[<Tests>]
let t2 =
    Expecto.Tests.testList "Parser tests"
        [
            makeTest "Parse1" "f g h" (OUT.AST (Apply(Apply(Var "f",Var "g"),Var "h")))
        ]

[<Tests>]
let t3 =
    Expecto.Tests.testList "Parser tests"
        [
            makeTest "BA1" "f g h" (OUT.ASTBR (Apply3 (Var "f") (Var "g") (Var "h")))
            makeTest "BA2" @"\x.x" (OUT.ASTBR (Comb "I"))
            makeTest "BA3" @"\x.y" (OUT.ASTBR (Apply2 (Comb "K") (Var "y")))
            makeTest "BA4" @"\x.x x" (OUT.ASTBR (Apply3 (Comb "S") (Comb "I") (Comb "I")))
        ]

[<Tests>]
let t4 =
    let makeReduceTest name inp outp =
        testCase name <| fun () ->
            let toks = tokenize inp
            let ast = parse toks
            let ba = removeLambdas ast
            let red = reduce 1000 ba []
            Expect.equal red outp (sprintf "Reduce '%s'" inp)
    Expecto.Tests.testList "Reduce tests"
        [
            makeTest "Reduce1" "S f g x" (OUT.RS "f x (g x)")
        ]



//Repl recursive function (ctrl-c to quit)
let rec repl() =
    printf("REPL>")
    let line = Console.ReadLine()
    match TPBR'(line) with
    | Result.Ok(res) -> printfn "%A" res
    | Result.Error(err) -> printfn "An error occured during evaluation:\n%A" err
    repl()

[<EntryPoint>]
let main argv =
    printfn "Please enter 't' to start tests or any other key for REPL"
    match Console.ReadKey().KeyChar with
    | 'T' | 't' -> printfn "" ; Tests.runTestsInAssembly Tests.defaultConfig [||] |> ignore
    | _         -> printfn "" ; repl() |> ignore
    0 // return an integer exit code
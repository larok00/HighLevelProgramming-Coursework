module Reduce
open EEExtensions
open Tokenize
open Parse


type ReduceState = Break of Exp * Exp list | Reduced of Exp * Exp list


/// Returns a,r
/// a is number of arguments for combinator
/// r is function that implemente reduction rule
/// r args = reduced expression
let getRule func =
    match func with
    | "S" -> 3, fun (args: Exp list) -> Apply3 args.[0] args.[2] (Apply2 args.[1] args.[2])
    | "K" -> 2, fun (args: Exp list) -> args.[0]
    | "I" -> 1, fun (args: Exp list) -> args.[0]
    | "P" -> 3, fun (args: Exp list) -> Apply3 args.[2] args.[0] args.[1]
    | "Y" -> 1, fun (args: Exp list) -> Apply2 args.[0] (Apply2 (Comb "Y") args.[0])
    | s ->  failwithf "%A has no reduction rule" s


/// apply exp to args and perform reduction rule
/// repeat this via Normal Order reduction until
/// no further reduction is possible.
/// args is list of arguments in order
let rec reduce count exp args =
    let numArgs = List.length args
    match exp with
    | _ when count = 0 -> Break(exp, args)
    | Apply ( func, arg) -> reduce count func (arg :: args) // extract arguments from func
    | Comb c -> printfn "Reducing %A with %A" exp args
                let arity, rule = getRule c
                if arity > numArgs 
                then Reduced(exp, args)
                else reduce (count-1) (rule args.[0..arity-1]) args.[arity..args.Length-1]
    | Var v -> Reduced(exp, args)
    | _ -> failwithf "Unexpected construct during reduction: %A" exp
        
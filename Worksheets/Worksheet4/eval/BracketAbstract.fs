module BracketAbstract
open Parse


let Apply3 a b c = Apply( Apply(a,b) , c)
let Apply2 a b = Apply(a,b)

let combinatorList = ["S";"K";"I";"P";"Y"]


/// Return [v] exp
/// Where ([v] exp) v = exp
/// exp must have no lambda bindings in it
let rec bracketAbstract v exp =
    match exp with
    | Apply(e1, e2) -> Apply3 (Comb "S") (bracketAbstract v e1) (bracketAbstract v e2)
    | Lambda _ -> 
        failwithf "What? bracketAbstract cannot be applied directlty to lambdas"
    | Var x when x = v -> Comb "I"
    | exp -> Apply(Comb "K", exp)

/// Remove Lambda bindings from exp
/// using bracket abstraction
/// \x.exp = [x] exp
let rec removeLambdas exp =
    match exp with
    | Lambda( bv, body) -> bracketAbstract bv (removeLambdas body)
    | Apply( e1, e2) -> Apply( removeLambdas e1, removeLambdas e2)
    | Var x when List.contains x combinatorList -> Comb x
    | exp -> exp
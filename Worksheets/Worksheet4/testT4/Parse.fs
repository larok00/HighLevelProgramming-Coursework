module Parse
open EEExtensions
open Tokenize


type Exp = | Apply of func: Exp * arg: Exp
           | Var of string
           | Lambda of bound: string * body: Exp
           | Comb of string


let Apply3 a b c = Apply( Apply(a,b) , c)
let Apply2 a b = Apply(a,b)
let toListExp itemList = List.reduce Apply2 itemList


/// Parse the next single (possibly bracketed) item
/// Return the item parse tree paired with the list of unused tokens
/// If no item is available to parse then fail
let rec parseItem toks =
    match toks with
    | RBRA _ :: _ | END _ :: _ -> failwithf "Parse Error - no item found"
    | LAMBDA _ :: SYM (bv,_) :: DOT _ :: body -> 
        let pa, toks = parseItemList body
        Lambda(bv, toListExp pa), toks
    | LBRA _ :: toks' -> 
        let bracketedExp, after = parseItemList toks'
        match after with
        | RBRA _ :: toks'' -> toListExp bracketedExp,  toks''
        | _ -> failwithf "Parse error: right bracket expected but not found at token"
    | SYM (sym,_) :: toks -> Var sym, toks
    | [] -> failwithf "What? Unexpected end of tokens encounterd during parse"
    | _ -> failwithf "Unexpected token encountered duting parse: %A" toks

/// Parse a (possibly empty) sequence of individual items
/// Return the list of parsed items paired with remaining tokens
/// If a parse error occurs fail
and parseItemList toks =
    let item, toks' = parseItem toks
    match toks' with
    | END _ :: _ | RBRA _ :: _ -> [item], toks'
    | _ -> 
        let items, toks'' = parseItemList toks'
        item :: items, toks''

let parse toks =
    let items, toks = parseItemList toks
    if toks <> [END] then 
        failwithf "Parse error token left should be '[END]' but are\n%A" toks
    List.reduce Apply2 items


//------------------------------------------------------------------
//------------------------------------------------------------------
// Monad version of parse
//------------------------------------------------------------------
//------------------------------------------------------------------


/// Parse the next single (possibly bracketed) item
/// Return the item parse tree paired with the list of unused tokens
/// Return failure as monadic value
let rec parseItem' toks =
    match toks with
    | RBRA _ :: _ | END _ :: _ -> Error "Parse Error - no item found"
    | LAMBDA _ :: SYM (bv,_) :: DOT _ :: body -> 
        parseItemList' body
        |> Result.map (fun (pa,toks) -> Lambda(bv, toListExp pa), toks)
    | LBRA _ :: toks' -> 
        parseItemList' toks'
        |> Result.bind (fun (bracketedExp, after) ->
            match after with
            | RBRA _ :: toks'' -> (toListExp bracketedExp,  toks'') |> Ok
            | _ -> Error "Parse error: right bracket expected but not found at token")
    | SYM(sym,_) :: toks -> (Var sym, toks) |> Ok
    | [] ->  "What? Unexpected end of tokens encountered during parse" |> Error
    | _ -> sprintf "Unexpected token encountered during parse: %A" toks |> Error

/// Parse a (possibly empty) sequence of individual items
/// Return the list of parsed items paired with remaining tokens
/// Return failure as monadic value
and parseItemList' toks =
    parseItem' toks
    |> Result.bind (fun (item,toks') ->
            match toks' with
            | END _ :: _ | RBRA _ :: _ -> ([item], toks') |> Ok
            | _ -> 
                parseItemList' toks'
                |> Result.map (fun (items,toks'') -> 
                    item :: items, toks''))

// Return failure as monadic value
let parse' toks =
    parseItemList' toks
    |> Result.bind (fun (items,toks) ->
        if toks <> [END]
        then Error (sprintf "Parse error token left should be '[END]' but are\n%A" toks)
        else List.reduce Apply2 items |> Ok)

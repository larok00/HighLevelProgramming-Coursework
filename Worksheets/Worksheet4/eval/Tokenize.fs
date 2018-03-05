module Tokenize

open EEExtensions

type Token = SYM of string * int | DOT of int | LBRA of int| RBRA of int 
             | LAMBDA of int | END
type LexerState = Normal | InComment // add cases as needed to perform lexical analysis`
type LexData = { Txt: string; State: LexerState; Numb: int}

/// Active Pattern for Lexer
/// debug = true => print out full info about matches
/// regex is the regex to match - must be a literal in the pattern
/// returns (m,st) where m is string matched and st is state with
/// Txt adjusted as appropriate given the match
let (|LexMatchD|_|) debug regex state =
    match String.regexMatch regex state.Txt with
    | None -> if debug 
              then printfn "Match of '%s' with '%s' failed." state.Txt regex; 
              None
    | Some (mStr, _) -> 
        let mChars = String.length mStr
        if mChars = 0 then 
            failwithf "What? Unexpected 0 character match in LexMatch '%s'" regex
        if debug then
            printfn "Match of '%s' with '%s' OK: match is '%s" state.Txt regex mStr; 
        let state' = {state with Txt = state.Txt.[mChars..]}
        Some (mStr,state')
/// change parameter to alter debugging printout
/// true => print out every match
/// false => print out nothing
let (|LexMatch|_|) = (|LexMatchD|_|) false


/// Returns next token Option, and new state, given state lData
/// If it returns None for token, with a changed state
/// It will be called again with new state
/// It must never be called with Txt=""
let nextToken lData =
    let incr st = {st with Numb = st.Numb+1}
    let retTag tag ld = Some(tag ld.Numb), incr ld
    match lData.State with
    | InComment ->
        match lData with
        | _ -> None, {lData with Txt=""}
    | Normal ->
        match lData with 
        | LexMatch "//" _ -> None, {lData with State=InComment}
        | LexMatch @"^\." (_,sta) -> retTag DOT sta
        | LexMatch @"^[ \t\r\f]+" (_, sta) -> None, incr sta
        | LexMatch @"^\\" (_, sta) -> retTag LAMBDA sta
        | LexMatch "^[a-zA-Z]+" (sym, sta) -> Some(SYM(sym, sta.Numb)), incr sta
        | LexMatch @"^\)" (_, sta) -> retTag RBRA sta
        | LexMatch @"^\(" (_,sta) -> retTag LBRA sta
        | _ -> failwithf "Matching failed in lexer at: '%s'" lData.Txt

/// Repeatedly calls nextToken
/// to do lexical analysis
let tokenize str =
    let rec tokenize' st =
        match st.Txt with
        | "" -> [END]
        | _ -> let nt,st' = nextToken st
               match nt with
               | None -> tokenize' st'
               | Some tok -> tok :: tokenize' st'
    tokenize' {Txt=str;State=Normal;Numb=0}


//----------------------------------------------------------------------------



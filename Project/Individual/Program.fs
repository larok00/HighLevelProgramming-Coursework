module MemoryStack

//open System

open CommonDataAndLex.CommonData
open CommonDataAndLex.CommonLex
open EEExtensions

module Memory = 
    open EEExtensions

    // change these types as required
    type Root = LDM | STGMEDIUM

    type Mode = EA | ED | FA | FD

    type UpdatePointer = bool

    /// instruction (dummy: must change)
    type Instr =  {
        Root: string;
        Mode: string;
        Cond: Condition;
        StackPointer: RName*UpdatePointer;
        OpRegs: string //RName list
    }

    /// parse error (dummy, but will do)
    type ErrInstr = string

    let memSpec = {
        InstrC = MEM
        Roots = ["LDM";"STM"]
        Suffixes = [""]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand memSpec

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =
            // this does the real work of parsing
            // dummy return for now
            let firstComma = String.indexOf "," ls.Operands
            match firstComma with
                | Some i -> 
                    let regString, updatePointer = if String.endsWith "!" ls.Operands.[0..i-1] then ls.Operands.[0..i-2],true else ls.Operands.[0..i-1],false
                    let pointer = Map.tryFind regString regNames
                    match pointer with 
                        | Some ptr -> 
                            let opRegs = ls.Operands.[i..]
                            Ok { PInstr={Root=root; Mode=suffix; Cond=pCond; StackPointer=ptr,updatePointer; OpRegs=opRegs}; PLabel = None ; PSize = 4u; PCond = pCond }
                        | None -> Error "Invalid stack pointer."
                    //let opRegs = ls.Operands.[i..] |> Array.ofSeq |> 
                | None -> Error "No comma in operands string."
                
            //Ok { Root=root; Mode=suffix; Cond=pCond; }
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'



    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse

////////////////////////////////////////////////////////////////////////////////////
//      Code defined at top level after the instruction processing modules
////////////////////////////////////////////////////////////////////////////////////
module CommonTop =

    /// allows different modules to return different instruction types
    type Instr =
        | IMEM of Memory.Instr
    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIMEM of Memory.ErrInstr
        | ERRTOPLEVEL of string

    /// Note that Instr in Mem and DP modules is NOT same as Instr in this module
    /// Instr here is all possible isntruction values combines with a D.U.
    /// that tags the Instruction class
    /// Similarly ErrInstr
    /// Similarly IMatch here is combination of module IMatches
    let IMatch (ld: LineData) : Result<Parse<Instr>,ErrInstr> option =
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | Memory.IMatch pa -> pConv IMEM ERRIMEM pa
        | _ -> None
    
    

    type CondInstr = Condition * Instr

    let parseLine (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string) =
        /// put parameters into a LineData record
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat "" operands
            Label=None
            LoadAddr = loadAddr
            SymTab = symtab
        }
        /// remove comments from string
        let removeComment (txt:string) =
            txt.Split(';')
            |> function 
                | [|x|] -> x 
                | [||] -> "" 
                | lineWithComment -> lineWithComment.[0]
        /// split line on whitespace into an array
        let splitIntoWords ( line:string ) =
            line.Split( ([||] : char array), 
                System.StringSplitOptions.RemoveEmptyEntries)
        /// try to parse 1st word, or 2nd word, as opcode
        /// If 2nd word is opcode 1st word must be label
        let matchLine words =
            let pNoLabel =
                match words with
                | opc :: operands -> 
                    makeLineData opc operands 
                    |> IMatch
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa
            | None, label :: opc :: operands -> 
                match { makeLineData opc operands 
                        with Label=Some label} 
                      |> IMatch with
                | None -> 
                    Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %s" opc))
                | Some pa -> pa
            | _ -> Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %A" words))
        asmLine
        |> removeComment
        |> splitIntoWords
        |> Array.toList
        |> matchLine
    
open Tests

[<EntryPoint>]
let main argv = 
    printf "%A" (CommonTop.parseLine None (WA 0u) "LDM R 1 !, {R0 R0}")
    //Check.Quick XXX
    //tests() |> ignore
    allTests() |> ignore
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

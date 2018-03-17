//////////////////////////////////////////////////////////////////////////////////////////
//                                  INITPROJECTLEXER                                    //
//////////////////////////////////////////////////////////////////////////////////////////

(*
    Change Log
    v1.00 initial version
    v1.01 deleted duplicate "" suffix in condmap
*)

#load "CommonData.fs"
open System.Runtime.InteropServices.ComTypes
#load "CommonLex.fsx"

open CommonData
open CommonLex

module Memory =
    // change these types as required
    type Root = LDM | STGMEDIUM

    type Mode = EA | ED | FA | FD

    /// instruction (dummy: must change)
    type Instr =  {Root: Root;
        Mode: Mode;
        Cond: Condition;
        //StackPointer: RName; would 
        OpRegs: RName list
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
            Ok { Root=root; Mode=suffix; Cond=pCond; }
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
    

open CommonTop
/// test the initProjectLexer code
printf "%A" (parseLine None (WA 0u) "ADD R0, R0, R0")




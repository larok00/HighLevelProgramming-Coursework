module MemoryStack

//open System

open CommonDataAndLex.CommonData
open CommonDataAndLex.CommonLex
open EEExtensions
open System

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
        OpRegs: RName list //RName list
    }

    /// parse error (dummy, but will do)
    type ErrInstr = string

    let memSpec = {
        InstrC = MEM
        Roots = ["LDM";"STM"]
        Suffixes = [""; "IA"; "IB"; "DA"; "DB"]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand memSpec

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,string> option =
        // this does the real work of parsing
        let opCodeValid (instrC, (root,suffix,pCond)) = 
            let operands = ls.Operands
            
            let firstComma = String.indexOf "," operands
            match firstComma with
                | Some i -> 
                    let  stackPtrExists = 
                        let regString, updatePointer = if String.endsWith "!" operands.[0..i-1] then operands.[0..i-2],true else operands.[0..i-1],false
                        let pointer = Map.tryFind regString regNames
                        match pointer with 
                        | Some ptr when ptr=R15 -> Error "Base register must not be R15."
                        | Some ptr -> Ok (ptr,updatePointer)
                        | None -> Error (sprintf "Invalid base register %A." regString)
                    
                    let operandListExists = 
                        if (String.startsWith "{" operands.[i+1..]) && (String.endsWith "}" operands.[i+1..]) 
                        then 
                            
                            let makeOpRegsList lst reg = 
                                let rName = Map.tryFind reg regNames
                                match lst, rName with
                                | Error first, _ -> Error first
                                | _, None -> Error (sprintf "Invalid operand %A in register list." reg)
                                | Ok lst, Some rName -> Ok (rName::lst) //register list is reversed in order

                            if String.IsNullOrEmpty operands.[i+2..(String.length operands)-2] 
                            then Error "Empty register list."
                            else 
                                let opStringList = String.splitString [|","|] operands.[i+2..(String.length operands)-2]
                                match (Array.fold makeOpRegsList (Ok []) opStringList) with 
                                    | Ok lst -> Ok (List.rev lst)
                                    | Error str -> Error str
                        else Error (sprintf "Invalid register list %A, not surrounded by curly brackets." operands.[i+1..])
                    
                    match stackPtrExists, operandListExists with
                    | Error first, _ -> Error first
                    | _, Error second -> Error second
                    | Ok stackPtr, Ok regList -> Ok { PInstr={Root=root; Mode=suffix; Cond=pCond; StackPointer=stackPtr; OpRegs=regList}; PLabel = None ; PSize = 4u; PCond = pCond }
                | None -> Error (sprintf "No comma in operands string %A." operands)
        
        Map.tryFind ls.OpCode opCodes
        |> Option.map opCodeValid



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

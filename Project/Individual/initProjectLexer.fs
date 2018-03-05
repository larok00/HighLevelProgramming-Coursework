//////////////////////////////////////////////////////////////////////////////////////////
//                                  INITPROJECTLEXER                                    //
//////////////////////////////////////////////////////////////////////////////////////////

(*
    Change Log
    v1.00 initial version
    v1.01 deleted duplicate "" suffix in condmap
*)

module CommonData =
    //////////////////////////////////////////////////////////////////////////////////////
    //                   Common types and code used by all modules
    //////////////////////////////////////////////////////////////////////////////////////

    /// ARM Status bits
    type Flags = { N: bool; C:bool; Z: bool; V:bool}


    ////////////////////////ARM register names and operations/////////////////////////////
   

    /// ARM register names
    /// NB R15 is the program counter as read
    [<Struct>]
    type RName = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 
                 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

    /// ARM state as values of all registers and status bits
    /// NB PC can be found as R15 - 8. (Pipelining)
    type DataPath = {Fl: Flags; Regs:Map<RName,uint32>}

    /// Map used to convert strings into RName values, 
    /// includes register aliasses PC, LR, SP
    let regNames = 
        Map.ofList [ 
            "R0",R0 ; "R1",R1 ; "R2",R2 ; "R3",R3 ; "R4",R4 ; "R5",R5
            "R6",R6 ; "R7",R7 ; "R8",R8 ; "R9", R9 ; "R10",R10 ; "R11",R11 ; 
            "R12",R12 ; "R13",R13 ; "R14",R14 ; "R15",R15 ; 
            "PC",R15 ; "LR",R14 ; "SP",R13 
        ] 

    // various functions used to convert between string, RName, and register number

    /// Inverse of regNames, used to convert RName values to strings
    /// NB The string chosen will always be the register (not alias)
    let regStrings = 
        regNames
        |> Map.toList
        |> List.map (fun (s,rn)-> (rn,s)) 
        |> List.filter (fun (_,s:string) -> s.StartsWith "R")
        |> Map.ofList

    /// Map converts RName into register number (no aliasses)
    let regNums = Map.map (fun _ (s:string) -> int (s.[1..])) regStrings

    /// Map converts register number into RName (no aliasses)
    let inverseRegNums = 
        regNums |> Map.toList 
        |> List.map (fun (rn,n)->(n,rn)) |> Map.ofList

    /// Property on RName to return register number, for convenience
    /// Aliasses not included, since they are not RNames
    type RName with
        /// Return the number of a register as an integer
        member r.RegNum = regNums.[r]
    
    /// Return a register name from an integer
    let register n = 
        if 0 <= n && n < 16 
        then inverseRegNums.[n] 
        else (failwithf "Register %d does not exist!" n)

    /// Type to represent the contents of one memory location
    /// 'INS is a parameter set to the type of an instruction
    /// needed because instruction type is only defined
    /// at top level.
    type MemLoc<'INS> =
        | DataLoc of uint32
        | Code of 'INS

    /// type to represent a (word) address
    /// there is some ambiguity. Does this contain the real address
    /// which is always divisible by 4
    /// or does it contain the word number (real address dvided by 4)
    /// either way multiply/divide by 4 will cause problems!
    /// document this well and be consistent.
    type WAddr = | WA of uint32

    /// type to represent memory
    type MachineMemory<'INS> = Map<WAddr,MemLoc<'INS>>


////////////////////////////////////////////////////////////////////////////////////////////////////
//                     Common code for Instruction Definition and Parsing
////////////////////////////////////////////////////////////////////////////////////////////////////

module CommonLex =

    open CommonData
    
    /// ARM execution conditions
    type Condition =

        | Ceq
        | Cne
        | Cmi
        | Cpl
        | Chi
        | Chs
        | Clo
        | Cls
        | Cge
        | Cgt
        | Cle
        | Clt
        | Cvs
        | Cvc
        | Cnv // the "never executed" condition NV - not often used!
        | Cal // the "always executed condition "AL". Used by default on no condition

    /// classes of instructions (example, add/change this is needed)
    type InstrClass = | DP | MEM

    /// specification of set of instructions
    type OpSpec = {
        InstrC: InstrClass
        Roots: string list
        Suffixes: string list
    }

    type SymbolTable = Map<string,uint32>

    /// result returned from instruction-specific module parsing
    /// an instruction class. If symbol definitions are found in a 
    /// symbol table then a complete parse will be output
    /// otherwise some fields will be None
    type Parse<'INS> = {
            /// value representing instruction. NB type varies with instruction class
            PInstr: 'INS 
            /// name and value of label defined on this line, if one is.
            PLabel: (string * uint32) option 
            /// number of bytes in memory taken up by this instruction
            PSize: uint32 
            /// execution condition for instruction
            PCond: Condition
        }

    /// data given to instruction-specific parse function
    type LineData = {
        /// memory address this instruction is loaded. Must be word address
        LoadAddr: WAddr 
        /// name of label defined on this line, if one exists
        Label: string option 
        /// table of symbols with defined values. 
        /// if this is given we are phase 2 and all symbols should be defined
        /// if this is not given we are phase 1 and no symbols are defined
        SymTab: SymbolTable option
        /// opcode string
        OpCode: string
        /// string of all the operands
        Operands: string
    }


    /// Strings with corresponding execution condition
    /// Note some conditions have multiple strings
    /// Note "" is a valid condition string (always execute condition)
    let condMap = [ "EQ",Ceq ; "NE",Cne ; "MI",Cmi ; "PL",Cpl ; "HI", Chi ; "HS",Chs ; "LO",Clo ; 
                    "LS",Cls ; "GE",Cge ; "GT", Cgt ; "LE", Cle ; "LT", Clt ; "VS",Cvs ; 
                    "VC",Cvc ; "NV",Cnv ; "AL",Cal ; "",Cal] |> Map.ofList

    /// list of all strings representing execution conditions
    /// includes ""
    let condStrings = 
        condMap
        |> Map.toList
        |> List.map fst
        |> List.distinct    

    /// generate all possible opcode strings for given specification
    /// each string is paired with info about instruction
    /// and the three parts of the opcode
    let opCodeExpand (spec: OpSpec) 
        //    opcode    class        root    suffix   instr cond
        : Map<string, InstrClass * (string * string * Condition)> =
        spec.Roots
        |> List.collect (fun r -> 
            spec.Suffixes
            |> List.collect (fun s -> 
                condStrings
                |> List.map (fun c -> r+s+c, (spec.InstrC,(r,s, condMap.[c])))))
                |> Map.ofList

    /// function used to change PInstr field of a Result<Parse<'INS>,'E>
    /// the output has this field mapped with fMap
    /// or if Error has this value chnaged by fMapE
    let pResultInstrMap fMap fMapE paRes =
        match paRes with
        | Ok ({PInstr=ins} as pr) -> 
            // Note subtle point. {pr with Pinst = ...} will not work here
            // That is because applying fmap changes the type of PInstr
            // and therefore the type of the record.
            Ok {
            PInstr = fMap ins 
            PLabel = pr.PLabel
            PCond = pr.PCond
            PSize = pr.PSize
            }
        | Error e -> Error (fMapE e)

//////////////////////////////////////////////////////////////////////////////////////////
//                   Sample (skeleton) instruction implementation modules
//////////////////////////////////////////////////////////////////////////////////////////

module DP =

    open CommonLex

    // change these types as required

    /// instruction (dummy: must change)
    type Instr =  {DPDummy: Unit}

    /// parse error (dummy, but will do)
    type ErrInstr = string

    /// sample specification for set of instructions
    /// very incomplete!
    let dPSpec = {
        InstrC = DP
        Roots = ["ADD";"SUB"]
        Suffixes = [""; "S"]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand dPSpec

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =
            // this does the real work of parsing
            // dummy return for now
            Ok { PInstr={DPDummy=()}; PLabel = None ; PSize = 4u; PCond = pCond }
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse


module Memory =

    open CommonLex

    /// sample specification for set of instructions

    // change these types as required

    /// instruction (dummy: must change)
    type Instr =  {MemDummy: Unit}

    /// parse error (dummy, but will do)
    type ErrInstr = string

    let memSpec = {
        InstrC = MEM
        Roots = ["LDR";"STR"]
        Suffixes = [""; "B"]
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
            Ok { PInstr={MemDummy=()}; PLabel = None ; PSize = 4u; PCond = pCond }
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'



    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse

////////////////////////////////////////////////////////////////////////////////////
//      Code defined at top level after the instruction processing modules
////////////////////////////////////////////////////////////////////////////////////
module CommonTop =

    open CommonLex
    open CommonData

    /// allows different modules to return different instruction types
    type Instr =
        | IMEM of Memory.Instr
        | IDP of DP.Instr
    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIMEM of Memory.ErrInstr
        | ERRIDP of DP.ErrInstr
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
        | DP.IMatch pa -> pConv IDP ERRIDP pa
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
open CommonData
/// test the initProjectLexer code
let test = parseLine None (WA 0u)




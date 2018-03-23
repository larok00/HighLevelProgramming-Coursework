module MemoryStack

open System

open CommonDataAndLex.CommonData
open CommonDataAndLex.CommonLex
open EEExtensions

module Memory = 

    // change these types as required
    type Root = LDM | STGMEDIUM

    type Mode = EA | ED | FA | FD

    type UpdatePointer = bool

    /// instruction (dummy: must change)
    type Instr =  {
        Root: string;
        Mode: string;
        StackPointer: RName*UpdatePointer;
        OpRegs: RName list
    }

    /// parse error (dummy, but will do)
    type ErrInstr = string

    let memSpec = {
        InstrC = MEM
        Roots = 
            [
                        "STM";  "LDM";
            ]
                                        //first column equivalent to second column for STM
                                        //first column equivalent to second column flipped for LDM
        Suffixes = 
            [
                "IA";   "EA";   ""; 
                "IB";   "FA"; 
                "DA";   "ED"; 
                "DB";   "FD";
            ]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand memSpec



    type WAddrType = | One | Four

    //used for indrementing or decrementing memory location
    type Addition = {Before:int; After:int}

    let execute (machState:DataPath) (memoryState: MachineMemory<InstrClass>) (parseInstr: Parse<Instr>) = 
        let wAddrType=Four
        let addrMultiplier=if wAddrType=One then 1u else 4u
        let minLoc=0x10000u
        let maxLoc=0xFFFFFFFCu

        let instr = parseInstr.PInstr
        let ptr,updatePointer = instr.StackPointer
        let initialAddr = Map.find ptr machState.Regs
        match initialAddr with 
        | initAddr when initAddr%(addrMultiplier)<>0u -> 
            Error (sprintf "Base register must be divisible by %i for a valid address." addrMultiplier)
        | initAddr when initAddr<minLoc -> 
            Error (sprintf "Base register must be bigger than 0x%x, this space is reserved as Instruction Memory." (minLoc-1u))
        | stAddr when stAddr>maxLoc -> 
            //useless at the moment, 
            //since addrMultiplier=4 and 
            //maxBaseReg=0xFFFFFFFCu
            Error (sprintf "Base register is too big, biggest address available is 0x%x." maxLoc)
        | _ -> 

            let deltaToFinal, addition = 
                let afterAdvantage = if updatePointer then 0 else 1
                match instr.Root, instr.Mode with 
                |_,"IB" |"STM","FA" |"LDM","ED" -> 
                    //IB
                    int64 addrMultiplier*int64 (List.length instr.OpRegs),
                    {Before=1*int addrMultiplier; After=0}
                |_,"DA" |"STM","ED" |"LDM","FA" -> 
                    //DA
                    int64 addrMultiplier*int64 (afterAdvantage-(List.length instr.OpRegs)),
                    {Before=0; After=(-1)*int addrMultiplier}
                |_,"DB" |"STM","FD" |"LDM","EA" -> 
                    //DB
                    int64 addrMultiplier*int64 (-(List.length instr.OpRegs)),
                    {Before=(-1)*int addrMultiplier; After=0}
                |_,"IA" |"STM","EA" |"LDM","FD" |_,""   |_  -> 
                    //IA
                    int64 addrMultiplier*int64 ((List.length instr.OpRegs)-afterAdvantage),
                    {Before=0; After=1*int addrMultiplier}
            
            match deltaToFinal with 
            | delta when delta<int64 minLoc-int64 initialAddr -> 
                Error (sprintf "Stack takes up space below smallest available address 0x%x." minLoc)
            | delta when delta>int64 maxLoc-int64 initialAddr -> 
                Error (sprintf "Stack takes up space above biggest available address 0x%x." maxLoc)
            | _ -> 
                let readWrite (theThing: Result<Map<RName,uint32>*MachineMemory<InstrClass>*uint32,String>) reg = 
                    match theThing with 
                    |Error err -> Error err
                    | Ok (regStates,memState,addr) -> 
                        match instr.Root with 
                        | "STM" -> Ok (regStates, Map.add (WA addr) (DataLoc (Map.find reg machState.Regs)) memState, addr)
                        | "LDM" -> 
                                    match Map.tryFind (WA addr) memState with 
                                    | Some value -> 
                                        match value with 
                                        | DataLoc x -> Ok (Map.add reg x regStates, memState, addr)
                                        | Code _ -> Error (sprintf "Memory location 0x%x contains an instruction value." addr)
                                    | None -> Ok (Map.add reg 0u regStates, memState, addr)
                        | _ -> Error "Instruction not recognised."
                let newState (theThing) reg: Result<Map<RName,uint32>*MachineMemory<InstrClass>*uint32,String> = 
                    match theThing with 
                    |Error err -> Error err
                    | Ok (regStates,memState,addr) -> 
                        let calculation = readWrite (Ok (regStates,memState,addr+uint32 addition.Before)) reg
                        match calculation with 
                        | Error err -> Error err
                        | Ok (nRegStates,nMemState,nAddr) -> Ok (nRegStates, nMemState, nAddr+uint32 addition.After)
                let result = List.fold newState (Ok (machState.Regs,memoryState,initialAddr)) instr.OpRegs
                match result with 
                | Error err -> Error err
                | Ok (newRegStates,newMemState,newAddr) ->
                    if updatePointer 
                    then Ok ((Map.add ptr newAddr newRegStates), newMemState) 
                    else Ok (newRegStates, newMemState)

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,ErrInstr> option =
        //printfn "%A" ls |> ignore

        // this does the real work of parsing
        let opCodeValid (instrC, (root,suffix,pCond)) = 
            instrC |> ignore

            let operands = ls.Operands
            
            let firstComma = String.indexOf "," operands
            match firstComma with
                | Some i -> 
                    let  stackPtr = 
                        let regString, updatePointer = 
                            if String.endsWith "!" operands.[0..i-1] 
                            then operands.[0..i-2],true 
                            else operands.[0..i-1],false
                        
                        let pointer = Map.tryFind regString regNames

                        match pointer with 
                        //| Some ptr when ptr=R15 -> Error "Base register must not be R15."
                        | Some ptr -> Ok (ptr,updatePointer)
                        | None -> Error (sprintf "Invalid base register %A." regString)


                    let operandList = 
                        if not ( (String.startsWith "{" operands.[i+1..]) && (String.endsWith "}" operands.[i+1..]) ) 
                        then Error (sprintf "Invalid register list %A, not surrounded by curly brackets." operands.[i+1..])
                        else 
                            let makeOpRegsList lst regOrRange = 
                                let makeOpRegsList' lst reg = 
                                    let rName = Map.tryFind reg regNames
                                    match lst, rName with
                                    | Error first, _ -> Error first
                                    | _, None -> Error (sprintf "Invalid operand %A in register list." reg)
                                    | Ok lst, Some rName -> Ok (rName::lst) //register list is reversed in order
                                
                                if not (String.contains "-" regOrRange) 
                                then makeOpRegsList' lst regOrRange 
                                else 
                                    let range = String.split [|'-'|] regOrRange

                                    if range.Length<>2 
                                    then Error (sprintf "Invalid range %A in register list." regOrRange) 
                                    else 
                                        match (Array.fold makeOpRegsList' (Ok []) range) with 
                                            | Ok ls -> 
                                                //dummy variable so that range can be constructed later
                                                let rangeConfirmedInt = 
                                                    ls 
                                                    |> List.rev 
                                                    |> List.map (fun key -> int (Map.find key regStrings).[1..])
                                                //range from a to b with a>b gives empty list, which is desired
                                                [rangeConfirmedInt.[0]..rangeConfirmedInt.[1]] 
                                                |> List.map (sprintf "R%i")
                                                |> List.fold makeOpRegsList' lst 

                                            | Error str -> Error str
                            
                            if String.IsNullOrEmpty operands.[i+2..(String.length operands)-2] 
                            then Error "Empty register list."
                            else 
                                let opStringList = String.splitString [|","|] operands.[i+2..(String.length operands)-2]
                                match (Array.fold makeOpRegsList (Ok []) opStringList) with 
                                    | Ok ls -> if List.isEmpty ls then Error "Empty register list." else Ok (List.rev ls)
                                    | Error str -> Error str


                    match stackPtr, operandList with
                    | Error ptr, _ -> Error ptr
                    | _, Error lst -> Error lst
                    | Ok ptr, Ok opList -> 
                        Ok { PInstr={Root=root; Mode=suffix; StackPointer=ptr; OpRegs=opList}; PLabel = None ; PSize = 4u; PCond = pCond }
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

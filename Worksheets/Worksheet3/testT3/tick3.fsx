/////////////////////////////////////////////////////////////
/// ARM Status bits
type Flags = { N: bool; C:bool; Z: bool; V:bool}

/// ARM register names
[<Struct>]
type RName = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
                | R9 | R10 | R11 | R12 | R13 | R14 | R15

/// ARM state as values of all registers and status bits
type DataPath = {Fl: Flags; Regs:Map<RName,uint32>}

/// Map used to convert strings into RName values, 
/// includes register aliasses PC, LR, SP
let regNames = Map.ofList [ 
                        "R0",R0 ; "R1",R1 ; "R2",R2 ; "R3",R3 ; "R4",R4 ; 
                        "R5",R5 ; "R6",R6 ; "R7",R7 ; "R8",R8 ;
                        "R9", R9 ; "R10",R10 ; "R11",R11 ; "R12",R12 ; 
                        "R13",R13 ; "R14",R14 ; "R15",R15 ; 
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
    regNums 
    |> Map.toList 
    |> List.map (fun (rn,n)->(n,rn)) 
    |> Map.ofList

/// Property on RName to return register number, for convenience
/// Aliasses not included, since they are not RNames
type RName with
    /// Return the number of a register as an integer
    member r.RegNum = regNums.[r]

/// Return a register name from an integer
let register n = if 0 <= n && n < 16 
                    then inverseRegNums.[n] 
                    else (failwithf "Register %d does not exist!" n)
/////////////////////////////////////////////////////////////////////////////



/// literal value = (K % 256) rotated right by (R &&& 0xF)*2. 
type Literal = {K: uint32; R: int} // best practice, see later
type LiteralData = uint32
type Reg = RName
type SVal = Nms of Literal | Rg of Reg
type Shift = 
    | LSL 
    | ASR 
    | LSR 
    | ROR 

type Op2 = 
    | Nm of Literal
    | Ro of Reg
    | Shifted of Reg * Shift * SVal
    | RRX of Reg

let allowedLiterals = 
    [0..2..30] //why only multiples of 2? Why can't I shift by one?
    |> List.allPairs [0u..255u] 
    |> List.map (fun (lit,n) -> (lit >>> n) + (lit <<< 32-n), {K=lit; R=n/2})
    |> Map.ofList

let makeLiteral (lit: uint32) = Map.tryFind lit allowedLiterals

let doROR a n = (a >>> n) ||| (a <<< (32-n))

//converts literals in K and R format into simple uint32
let litValue {K=k ; R=r} = doROR (k % uint32(256)) ((r &&& 0xF)*2)

let flexOp2 (op2:Op2) (cpuData:DataPath) = 
    match op2 with
    | Nm(lit)-> litValue lit
    | Ro(r)-> cpuData.Regs.[r]
    | Shifted(reg, shift, sVal)-> 
        let regValue = cpuData.Regs.[reg]

        let sFunction =
            match shift with
            | LSL -> (<<<)
            | ASR -> (fun a b -> (int a) >>> b |> uint32)
            | LSR -> (>>>)
            | ROR -> doROR
        
        sFunction regValue <|
            match sVal with
            | Nms(lit) -> 
                ((litValue lit) <<< 27) >>> 27 
                |> int
            | Rg(register) ->
                (cpuData.Regs.[register] <<< 27) >>> 27 
                |> int
    | RRX(reg)-> 
        let shifted = (cpuData.Regs.[reg] >>> 1)
        match cpuData.Fl with 
        | {C=true} -> uint32(0x80) ||| shifted
        | _ -> shifted

let makeShift (rOp2: RName) (rShift: RName option) (s: int option) (shift: Shift) = 
    let box = Shifted >> Some
    match (rShift, s) with 
    | (Some (x: RName),None) -> (rOp2, shift, Rg x) |> box
    | (None, Some (y: int)) -> (rOp2, shift, Nms {K=(uint32 y); R=0}) |> box
    | _ -> None



(* couldn't get it to work due to type mismatches
let flexOp2' op2 = flexOp2 op2 dummyCpuData

[<Tests>]
let makeLiteralTest = 
    testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 10000 } 
        "Test consistency of makeLiteral and flexOp2" <| // this <| means we can omit brackets round fun
                fun lit ->
                    Expect.equal lit (lit |> flexOp2' |> makeLiteral)
                        "flexOp2` and makeLiteral are self-inverse"
*)
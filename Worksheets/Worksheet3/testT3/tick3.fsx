#load "header.fs"

/// literal value = (K % 256) rotated right by (R &&& 0xF)*2. 
type Literal = {K: uint32; R: int} // best practice, see later
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
    [0..2..30] 
    |> List.allPairs [0u..255u] 
    |> List.map (fun (lit,n) -> (lit >>> n) + (lit <<< 32-n), {K=lit; R=n/2})
    |> Map.ofList

let makeLiteral (lit: uint32) = 
    Map.tryFind lit allowedLiterals

let flexOp2 (op2:Op2) (cpuData:DataPath) = 
    match op2 with
    | Nm(literalData)-> literalData.K >>> (literalData.R &&& 0xF)*2
    | Ro(register)-> 
        let regs = cpuData.Regs
        regs.[register]
    | Shifted(register*shift*sValue)-> 
        let regs = cpuData.Regs
        let reg = shift.Reg
        let regValue = regs.[reg]
        match shift.Value with
        | NumericValue(value) -> regValue<<<(int(value.Value))
        | RValue(value) ->
            let shiftRegValue = (regs.[value.Value]<<< 27)>>> 27
            regValue <<< int(shiftRegValue)
    | ASR(shift)->
        let regs = cpuData.Regs
        let reg = shift.Reg
        let regValue = regs.[reg]
        match shift.Value with
        | NumericValue(value) ->
            let shifted = int regValue>>>(int(value.Value))
            uint32 shifted
        | RValue(value) ->
            let shiftRegValue = (regs.[value.Value]<<< 27)>>> 27
            let shifted = int regValue>>>(int(shiftRegValue))
            uint32 shifted

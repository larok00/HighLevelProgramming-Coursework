module Program

open CommonDataAndLex.CommonData
open CommonDataAndLex.CommonLex
open MemoryStack

[<EntryPoint>]
let main argv = 
    argv |> ignore
    printfn "%A" (CommonTop.parseLine None (WA 0u) "LDM R1!, {R3-R5}") |> ignore
    let customRegs = 
        [(R0, 0u); (R1, 1u); (R2, 2u); (R3, 3u); (R4, 0xFFFFCu); (R5, 5u); (R6, 6u); (R7, 7u); 
        (R8, 8u); (R9, 0u); (R10, 1u); (R11, 1u); (R12, 2u); (R13, 3u); (R14, 4u); (R15, 5u);]
        |> Map.ofList
    let machState:DataPath = { Fl={N=false; C=false; Z=false; V=false}; Regs=customRegs}
    let memState:MachineMemory<InstrClass> = Map.ofList [WA 0x10000u, DataLoc 1u]
    let lData = 
        {
            LoadAddr = WA 0u;
            Label = None;
            SymTab = None;
            OpCode = "STM";
            Operands = "R4!,{R3-R5}";
        }
    let parsed = Memory.parse lData
    let result = 
        match parsed with
        | Some (Ok parsed) -> Memory.execute machState memState parsed
        | Some (Error err) -> Error err
        |None -> Error "Unimplemented instruction."
    printfn "%A" result |> ignore
    //Check.Quick XXX
    //ParseTests.runParseTests() |> ignore
    //ExecutionTests.runExecutionTests() |> ignore
    Expecto.Tests.runTestsInAssembly Expecto.Tests.defaultConfig [||] |> ignore
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

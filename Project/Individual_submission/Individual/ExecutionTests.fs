module ExecutionTests

open EEExtensions
open CommonDataAndLex.CommonData
open CommonDataAndLex.CommonLex
open MemoryStack

open Expecto
open FsCheck
open Mono.Cecil.Cil


let checkState error regs memState instr tester = 
    let machState = { Fl={N=false; C=false; Z=false; V=false}; Regs=regs}
    let parseInstr = 
        { PInstr=instr; PLabel = None ; PSize = 4u; PCond = Cal }
    let result = Memory.execute machState memState parseInstr
    
    Expect.equal result (Error error) (sprintf "Tester %A failed." tester)

let unitTest unitErrStr instrErr machState memState instr tester = 
    testCase unitErrStr <| fun() -> 
    checkState instrErr machState memState instr tester

let propertyTest propErrStr instrErr machState memState instr = 
    testProperty propErrStr <| 
    fun (randNum: int) -> 
        let tester = randNum
        checkState instrErr machState memState instr tester

let exhaustiveTest exhaustErrStr instrErr machState memState instr exhaustiveList = 
    exhaustiveList
    |> List.map 
        (fun tester -> 
        testCase (sprintf "Testing %A" tester) <| fun () -> 
        checkState instrErr machState memState instr tester
        )

    |> testList exhaustErrStr

(*
let regs = 
    regNums 
    |> Map.toList 
    |> List.map (fun (a,b) -> a,(uint32 b) )
    |> Map.ofList
let lData = 
    {
        LoadAddr = WA 0u;
        Label = None;
        SymTab = None;
        OpCode = "LDM";
        Operands = "R0,{R3-R5}";
    }
let parsed = Memory.parse lData
let result = 
    match parsed with
    | Some (Ok parsed) -> Memory.execute machState memState parsed
    | Some (Error err) -> Error err
    |None -> Error "Unimplemented instruction."
*)


[<Tests>]
let HelloWorld =
  test "A simple execution test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "Just give up."
  }

[<Tests>]
let nonDivisiblePtrProperty = 
    testProperty "Stack pointers that aren't divisible by 4 should be invalid." <|
    fun (randNum: uint32) -> 
        if randNum%4u=0u then Expect.equal 0 0 "number%4=0" else
            let regs = 
                [(R0, randNum); (R1, 0u); (R2, 0u); (R3, 0u); (R4, 0u); (R5, 0u); (R6, 0u); (R7, 0u); 
                (R8, 0u); (R9, 0u); (R10, 0u); (R11, 0u); (R12, 0u); (R13, 0u); (R14, 0u); (R15, 0u);]
                |> Map.ofList
            let memState:MachineMemory<InstrClass> = Map.ofList [WA 0x10000u, DataLoc 1u]
            let instr = 
                {
                    Memory.Root="LDM";
                    Memory.Mode="";
                    Memory.StackPointer= R0,false;
                    Memory.OpRegs= [R3; R4; R5]
                }
            checkState 
                "Base register must be divisible by 4 for a valid address." 
                regs 
                memState 
                instr 
                randNum

[<Tests>]
let smallPtrProperty = 
    testProperty "Stack pointers smaller than 0x10000 should be invalid." <|
    fun (randNum: uint32) -> 
        let regs = 
            [(R0, randNum*4u%0xFFFFu); (R1, 1u); (R2, 2u); (R3, 3u); (R4, 4u); (R5, 5u); (R6, 0u); (R7, 0u); 
            (R8, 0u); (R9, 0u); (R10, 0u); (R11, 0u); (R12, 0u); (R13, 0u); (R14, 0u); (R15, 0u);]
            |> Map.ofList
        let memState:MachineMemory<InstrClass> = Map.ofList [WA 0x10000u, DataLoc 1u]
        let instr = 
            {
                Memory.Root="LDM";
                Memory.Mode="";
                Memory.StackPointer= R0,false;
                Memory.OpRegs= [R3; R4; R5]
            }
        checkState 
            "Base register must be bigger than 0xffff, this space is reserved as Instruction Memory." 
            regs 
            memState 
            instr 
            randNum

[<Tests>]
let underMinLocProperty = 
    let testFn root mode updatePointer offset  = 
        testProperty (String.concat "," [root; mode; string updatePointer]) <|
        fun (randNum: uint32) -> 
            let regs = 
                [(R0, 0x10008u); (R1, 1u); (R2, 2u); (R3, 3u); (R4, 4u); (R5, 5u); (R6, 0u); (R7, 0u); 
                (R8, 0u); (R9, 0u); (R10, 0u); (R11, 0u); (R12, 0u); (R13, 0u); (R14, 0u); (R15, 0u);]
                |> Map.ofList
            let memState:MachineMemory<InstrClass> = Map.ofList [WA 0x10000u, DataLoc 1u]
            let instr = 
                {
                    Memory.Root=root;
                    Memory.Mode=mode;
                    Memory.StackPointer= R0,updatePointer;
                    Memory.OpRegs= List.map (fun _ -> R5) [1u..(offset+randNum)]
                }
            checkState 
                "Stack takes up space below smallest available address 0x10000." 
                regs 
                memState 
                instr 
                randNum
    
    testList "Stacks that take up space below 0x10000 should be invalid." <| 
        [
            testFn "STM" "DA" false 4u;
            testFn "STM" "DA" true  3u;
            testFn "STM" "DB" false 3u;
            testFn "STM" "DB" true  3u;
            testFn "LDM" "DA" false 4u;
            testFn "LDM" "DA" true  3u;
            testFn "LDM" "DB" false 3u;
            testFn "LDM" "DB" true  3u;
        ]

[<Tests>]
let overMaxLocProperty = 
    let testFn root mode updatePointer offset  = 
        testProperty (String.concat "," [root; mode; string updatePointer]) <|
        fun (randNum: uint32) -> 
            let regs = 
                [(R0, 0xFFFFFFF0u); (R1, 1u); (R2, 2u); (R3, 3u); (R4, 4u); (R5, 5u); (R6, 0u); (R7, 0u); 
                (R8, 0u); (R9, 0u); (R10, 0u); (R11, 0u); (R12, 0u); (R13, 0u); (R14, 0u); (R15, 0u);]
                |> Map.ofList
            let memState:MachineMemory<InstrClass> = Map.ofList [WA 0x10000u, DataLoc 1u]
            let instr = 
                {
                    Memory.Root=root;
                    Memory.Mode=mode;
                    Memory.StackPointer= R0,updatePointer;
                    Memory.OpRegs= List.map (fun _ -> R5) [1u..(offset+randNum)]
                }
            checkState 
                "Stack takes up space above biggest available address 0xfffffffc." 
                regs 
                memState 
                instr 
                randNum
    
    testList "Stacks that take up space above 0xFFFFFFFC should be invalid." <| 
        [
            testFn "STM" "IA" false 5u;
            testFn "STM" "IA" true  4u;
            testFn "STM" "IB" false 4u;
            testFn "STM" "IB" true  4u;
            testFn "LDM" "IA" false 5u;
            testFn "LDM" "IA" true  4u;
            testFn "LDM" "IB" false 4u;
            testFn "LDM" "IB" true 4u;
        ]

[<Tests>]
let memLocIsInstrUnit = 
    let testFn root = 
        testCase root <| fun () -> 
        let regs = 
            [(R0, 0x10000u); (R1, 0u); (R2, 0u); (R3, 0u); (R4, 0u); (R5, 0u); (R6, 0u); (R7, 0u); 
            (R8, 0u); (R9, 0u); (R10, 0u); (R11, 0u); (R12, 0u); (R13, 0u); (R14, 0u); (R15, 0u);]
            |> Map.ofList
        let instr = 
            {
                Memory.Root=root;
                Memory.Mode="";
                Memory.StackPointer= R0,false;
                Memory.OpRegs= [R3; R4; R5]
            }
        let memState:MachineMemory<InstrClass> = 
            Map.ofList [WA 0x10000u, CommonDataAndLex.CommonData.Code MEM]
        checkState 
            "Memory location 0x10000 contains an instruction value." 
            regs 
            memState 
            instr 
            ({
                Memory.Root="LDM";
                Memory.Mode="";
                Memory.StackPointer= R0,false;
                Memory.OpRegs= [R3; R4; R5]
            }, 
            [WA 0x10000u, CommonDataAndLex.CommonData.Code MEM])
    
    testList "Memory locations containing instruction values should be invalid inputs." <| 
        [
            testFn "STM";
            testFn "LDM";
        ]



let executionTestList =
    testList "Execution test group" <| 
    [
        HelloWorld              ;
        nonDivisiblePtrProperty ;
        smallPtrProperty        ;
        overMaxLocProperty      ;
        underMinLocProperty     ;
    ]

let runExecutionTests() =
    runTests defaultConfig executionTestList |> ignore
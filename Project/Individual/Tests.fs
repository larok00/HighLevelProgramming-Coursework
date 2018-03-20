module Tests

open EEExtensions
open CommonDataAndLex
open MemoryStack
open Expecto

let checkInstr error placeholder1 tester placeholder2 = 
    //testCase str <| fun () ->
    let result = 
        ([ placeholder1; tester; placeholder2] 
        |> List.toSeq 
        |> (String.concat "") 
        |> (CommonTop.parseLine None (CommonData.WA 0u)))
    
    Expect.equal result (Error error) (sprintf "Tester %A failed." tester)

type InstrErr = Err of CommonTop.ErrInstr | ErrWithArg of (string -> string)

let randStr (chars: string) intList = 
    let intToStr lst i = 
        let index = i%chars.Length |> abs
        (String.ofChar chars.[index])::lst
    intList |> List.fold intToStr [] |> String.concat ""

let unitTest unitErrStr instrErr placeholder1 tester placeholder2 = 
    testCase unitErrStr <| fun() -> 
    checkInstr instrErr placeholder1 tester placeholder2

let propertyTest propErrStr basisSet instrErr placeholder1 placeholder2 = 
    testProperty propErrStr <| 
    fun (randNums: int list) -> 
        let tester = randStr basisSet randNums

        if tester.Length=0 then Expect.equal 0 0 "String is empty." else 
        match instrErr with 
        | Err error -> checkInstr error placeholder1 tester placeholder2
        | ErrWithArg error -> checkInstr (CommonTop.ERRIMEM (error tester)) placeholder1 tester placeholder2

let exhaustiveTest exhaustErrStr instrErr placeholder1 placeholder2 exhaustiveList = 
    exhaustiveList
    |> List.map 
        (fun tester -> 
        testCase (sprintf "Testing %A" tester) <| fun () -> 
        match instrErr with 
        | Err error -> checkInstr error placeholder1 tester placeholder2
        | ErrWithArg error -> checkInstr (CommonTop.ERRIMEM (error tester)) placeholder1 tester placeholder2
        )

    |> testList exhaustErrStr



[<Tests>]
let helloWorldTest =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }

[<Tests>]
let invalidOpCodesProperty = 
    propertyTest 
        "Invalid opCodes should return top level error." 
        "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
        (Err (CommonTop.ERRTOPLEVEL "Unimplemented instruction <placeholder>")) 
        "" 
        " <placeholder>"

[<Tests>]
let ValidOpCodesExhaustive = 
    Memory.opCodes 
    |> Map.keys 
    |> Array.toList 
    |> exhaustiveTest 
        "Valid opCodes should be recognised." 
        (Err (CommonTop.ERRIMEM "No comma in operands string \"<placeholder>\".")) 
        "" 
        " <placeholder>"


[<Tests>]
let noCommaInOperandsProperty = 
    propertyTest 
        "Operands list without a comma should not be valid." 
        "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
        (ErrWithArg (sprintf "No comma in operands string %A." )) 
        "LDM " 
        ""

[<Tests>]
let invalidStackPtrProperty = 
    propertyTest 
        "Invalid opCodes should not be accepted." 
        "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
        (ErrWithArg (sprintf "Invalid base register %A.")) 
        "LDM " 
        ", <placeholder>"

[<Tests>]
let ValidStackPtrExhaustive = 
    CommonData.regNames 
    |> Map.keys 
    |> Array.toList 
    |> exhaustiveTest 
        "Valid base registers should be recognised." 
        (Err (CommonTop.ERRIMEM "Invalid register list \"<placeholder>\", not surrounded by curly brackets.")) 
        "LDM " 
        ", <placeholder>"

[<Tests>]
let invalidRegListProperty = 
    propertyTest 
        "Invalid register lists should not be accepted." 
        "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
        (ErrWithArg (sprintf "Invalid register list %A, not surrounded by curly brackets.")) 
        "LDM R0, " 
        ""

[<Tests>]
let emptyRegListUnit = 
    testList "Empty register lists should not be accepted." [
        unitTest 
            "without empty range" 
            (CommonTop.ERRIMEM "Empty register list.") 
            "LDM R0, {" 
            "" 
            "}";
        unitTest 
            "with empty range" 
            (CommonTop.ERRIMEM "Empty register list.") 
            "LDM R0, {" 
            "R5-R3" 
            "}"
    ]

[<Tests>]
let invalidRegListOpProperty = 
    propertyTest 
        "Invalid register list operands should not be accepted." 
        "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
        (ErrWithArg (sprintf "Invalid operand %A in register list.")) 
        "LDM R0, {" 
        "}"

[<Tests>]
let invalidRegRangeUnit = 
    testList "Invalid ranges should not be accepted." [
        unitTest 
            "R0-R0-R0" 
            (CommonTop.ERRIMEM "Invalid range \"R0-R0-R0\" in register list.")
            "LDM R0, {" 
            "R0-R0-R0" 
            "}";
        unitTest 
            "R15-R14-R13" 
            (CommonTop.ERRIMEM "Invalid range \"R15-R14-R13\" in register list.")
            "LDM R0, {" 
            "R15-R14-R13" 
            "}";
        unitTest 
            "R1-R0-" 
            (CommonTop.ERRIMEM "Invalid range \"R1-R0-\" in register list.")
            "LDM R0, {" 
            "R1-R0-" 
            "}";
        unitTest 
            "-R1-R0" 
            (CommonTop.ERRIMEM "Invalid range \"-R1-R0\" in register list.")
            "LDM R0, {" 
            "-R1-R0" 
            "}";
        unitTest 
            "R0--R1" 
            (CommonTop.ERRIMEM "Invalid range \"R0--R1\" in register list.")
            "LDM R0, {" 
            "R0--R1" 
            "}";
        unitTest 
            "asdf-1 2 3 4-, R0" 
            (CommonTop.ERRIMEM "Invalid range \"asdf-1234-\" in register list.")
            "LDM R0, {" 
            "asdf-1 2 3 4-, R0" 
            "}";
        unitTest 
            "R0 ---R12-, - -3" 
            (CommonTop.ERRIMEM "Invalid range \"--3\" in register list.")
            "LDM R0, {" 
            "R0 ---R12-, - -3" 
            "}";
    ]



let exampleTestList =
    testList "A test group" [
        ValidStackPtrExhaustive
    ]

let tests() =
    runTests defaultConfig exampleTestList |> ignore

//Q32
let allTests() =
    runTestsInAssembly defaultConfig [||] |> ignore
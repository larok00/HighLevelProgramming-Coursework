module Tests

open EEExtensions
open CommonDataAndLex
open MemoryStack
open Expecto

let checkInstr error placeholder1 placeholder2 tester = 
    //testCase str <| fun () ->
    let result = 
        ([ placeholder1; tester; placeholder2] 
        |> List.toSeq 
        |> (String.concat "") 
        |> (CommonTop.parseLine None (CommonData.WA 0u)))
    
    Expect.equal result (Error error) (sprintf "Tester %A failed." tester)

let randStr x = 
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
    let intToStr lst i = 
        let index = i%36 |> abs
        (String.ofChar chars.[index])::lst
    x |> List.fold intToStr [] |> String.concat ""


[<Tests>]
let helloWorldTest =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }

[<Tests>]
let invalidOpCodesProperty = 
    testProperty "Invalid opCodes should return top level error." <| 
    fun (randNums: int list) -> 
        let tester = randStr randNums

        if tester.Length=0 then Expect.equal 0 0 "String is empty." else 
        checkInstr (CommonTop.ERRTOPLEVEL "Unimplemented instruction <placeholder>") "" " <placeholder>" tester

[<Tests>]
let ValidOpCodesExhaustive = 
    Memory.opCodes 
    |> Map.keys 
    |> Array.toList 
    |> List.map 
        (fun tester -> 
        testCase (sprintf "Testing %A" tester) <| fun () -> 
        checkInstr (CommonTop.ERRIMEM "No comma in operands string \"<placeholder>\".") "" " <placeholder>" tester
        )

    |> testList "Valid opCodes should be recognised."


[<Tests>]
let noCommaInOperandsProperty = 
    testProperty "Operands list without a comma should not be valid." <| 
    fun (randNums: int list) -> 
        let tester = randStr randNums

        if tester.Length=0 then Expect.equal 0 0 "String is empty." else 
        checkInstr (CommonTop.ERRIMEM (sprintf "No comma in operands string %A." tester)) "LDM " "" tester

[<Tests>]
let invalidStackPtrProperty = 
    testProperty "Invalid opCodes should not be accepted." <| 
    fun (randNums: int list) -> 
        let tester = randStr randNums

        if tester.Length=0 then Expect.equal 0 0 "String is empty." else 
        checkInstr (CommonTop.ERRIMEM (sprintf "Invalid base register %A." tester)) "LDM " ", <placeholder>" tester

[<Tests>]
let ValidStackPtrExhaustive = 
    CommonData.regNames 
    |> Map.keys 
    |> Array.toList 
    |> List.map 
        (fun tester -> 
        testCase (sprintf "Testing %A" tester) <| fun () -> 
        if tester="R15" || tester="PC" then checkInstr (CommonTop.ERRIMEM "Base register must not be R15.") "LDM " ", <placeholder>" tester else 
        checkInstr (CommonTop.ERRIMEM "Invalid register list \"<placeholder>\", not surrounded by curly brackets.") "LDM " ", <placeholder>" tester
        )

    |> testList "Valid base registers should be recognised."

[<Tests>]
let invalidRegListProperty = 
    testProperty "Invalid register lists should not be accepted." <| 
    fun (randNums: int list) -> 
        let tester = randStr randNums

        if tester.Length=0 then Expect.equal 0 0 "String is empty." else 
        checkInstr (CommonTop.ERRIMEM (sprintf "Invalid register list %A, not surrounded by curly brackets." tester)) "LDM R0, " "" tester

let emptyRegListProperty = 
    testProperty "Invalid registers should not be acceoted." <| 
    fun (randNums: int list) -> 
        let tester = randStr randNums

        if tester.Length=0 then checkInstr (CommonTop.ERRIMEM "Empty register list.") "LDM R0, {" "}" tester else 
        checkInstr (CommonTop.ERRIMEM (sprintf "Invalid operand %A in register list." tester)) "LDM R0, {" "}" tester

let exampleTestList =
    testList "A test group" [
        ValidStackPtrExhaustive
    ]

let tests() =
    runTests defaultConfig exampleTestList |> ignore

//Q32
let allTests() =
    runTestsInAssembly defaultConfig [||] |> ignore
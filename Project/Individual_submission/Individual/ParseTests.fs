module ParseTests

open EEExtensions
open CommonDataAndLex
open MemoryStack

open Expecto
open FsCheck


let checkInstr error placeholder1 tester placeholder2 = 
    let result = 
        ([ placeholder1; tester; placeholder2] 
        |> List.toSeq 
        |> (String.concat "") 
        |> (CommonTop.parseLine None (CommonData.WA 0u)))
    
    Expect.equal result (Error error) (sprintf "Tester %A failed." tester)

type InstrErr = Err of CommonTop.ErrInstr | ErrWithArg of (string -> string)


let generator n = 
    Arb.generate<string> 
    |> Gen.sample n 3
    |> List.filter (isNull >> not)
    |> List.map (String.filter (fun c -> not (System.Char.IsWhiteSpace c) || Seq.exists ( (=)c ) [' '; '\n'; '\r'; ',']))
    |> List.collect (String.splitStringRemoveEmptyEntries [|" "|] >> Array.toList)

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
let parseHelloWorld =
  test "A simple parse test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "Just give up."
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
        "Operands list without a comma should be invalid." 
        "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789{}-"
        (ErrWithArg (sprintf "No comma in operands string %A." )) 
        "LDM " 
        ""

[<Tests>]
let invalidStackPtrProperty = 
    propertyTest 
        "Invalid opCodes should be rejected." 
        "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789{}-"
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
        "Invalid register lists should be rejected." 
        "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789-"
        (ErrWithArg (sprintf "Invalid register list %A, not surrounded by curly brackets.")) 
        "LDM R0, " 
        ""

[<Tests>]
let emptyRegListUnit = 
    testList "Empty register lists should be rejected." <| 
    [
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
        "Invalid register list operands should be rejected." 
        "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789{}"
        (ErrWithArg (sprintf "Invalid operand %A in register list.")) 
        "LDM R0, {" 
        "}"

[<Tests>]
let invalidRegRangeUnit = 
    let testIt tester errStr = 
        unitTest 
            tester 
            (CommonTop.ERRIMEM (sprintf "Invalid range %A in register list." errStr))
            "LDM R0, {" 
            tester 
            "}"
    
    testList "Invalid ranges should be rejected." <| 
    [
        testIt "R0-R0-R0" "R0-R0-R0"            ; 
        testIt "R15-R14-R13" "R15-R14-R13"      ; 
        testIt "R1-R0-" "R1-R0-"                ; 
        testIt "-R1-R0" "-R1-R0"                ; 
        testIt "R0--R1" "R0--R1"                ; 
        testIt "asdf-1 2 3 4-, R0" "asdf-1234-" ; 
        testIt "R0 ---R12-, - -3" "--3"         ;
    ]





let parseTestList =
    testList "Parse test group" <| 
    [
        parseHelloWorld             ;
        invalidOpCodesProperty      ;
        ValidOpCodesExhaustive      ;
        noCommaInOperandsProperty   ;
        invalidStackPtrProperty     ;
        ValidStackPtrExhaustive     ;
        invalidRegListProperty      ;
        emptyRegListUnit            ;
        invalidRegListOpProperty    ;
        invalidRegRangeUnit         ;
    ]

let runParseTests() =
    runTests defaultConfig parseTestList |> ignore
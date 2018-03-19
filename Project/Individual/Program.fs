module Program

open CommonDataAndLex
open MemoryStack
open Tests

[<EntryPoint>]
let main argv = 
    printfn "%A" (CommonTop.parseLine None (CommonData.WA 0u) "LDM R1!, {R0, R0}")
    //let tmp = ([ "LDM"; " <placeholder>"] |> List.toSeq |> (String.concat "") |> (CommonTop.parseLine None (CommonData.WA 0u)))
    //printfn "%A" tmp
    //Check.Quick XXX
    //tests() |> ignore
    allTests() |> ignore
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

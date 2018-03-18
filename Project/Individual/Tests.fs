module Tests

open Expecto

let helloWorldTests =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }

[<Tests>]
let exampleTestList =
    testList "A test group" [
        helloWorldTests
    ]

let tests() =
    runTests defaultConfig exampleTestList |> ignore

//Q32
let allTests() =
    runTestsInAssembly defaultConfig [||] |> ignore
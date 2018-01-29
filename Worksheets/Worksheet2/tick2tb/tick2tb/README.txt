# Testbench for Tick2

Files:
`tick2header.fsx`: functions and types in Figure 5 used by your code
`tick2.fsx`: your codetick2tb.fsx: testbench for your code
`test.fsx`: convenience top-level file that loads and runs teh testbench.

Write your code in tick2.fsx. Use test.fsx to test with my testbench data.

### Note about modules. 

When used with FSI code in a file `fname.fsx` is assigned to module `Fname`.
In order to access this value `myFunc` from another file you need
  * `open Fname` at the top of the file in which `myFunc` is written
  * to use the qualified name: `Fname.myfunc.fsx`
`
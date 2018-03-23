# Group Contribution
My code will not be used in any groups however I still tried to make the interface as minimalist as possible.

----------

`let  parse (ls: LineData) : Result<Parse<Instr>,ErrInstr\> option`
My parse function is reached by the active pattern used by top-level code. 

----------

`let  execute (machState:DataPath) (memoryState: MachineMemory<InstrClass>) (parseInstr: Parse<Instr>)`
My execute function requires 3 inputs. The parseInstr is can be easily fed in from the output of parse function. The machState and memoryState are needed for accessing/updating the values stored in registers and accessing/updating the values stored in memory respectively.

The output of execute is;
`Result<(DataPath * MachineMemory<InstrClass>),string>`
Again, it will be very easy to take this output and use it to update the DataPath and MachineMemory, with a match statement for errors.

----------

`type  ErrInstr = string`
I have been as specific and granular as possible with my error strings, so it should be very easy to understand what exactly about the input is considered invalid.
# Worthwhile Specifications
The instructions "LDM" and "STM" have been implemented. In terms of suffixes, all stack-oriented suffixes and equivalent addressing mode suffixes are supported. The addressing mode suffixes are translated back to stack-oriented ones inside the execute function.
| Stack-oriented suffix | For store or push instructions | For store or push instructions |
|--|--|--|
| FD (Full Descending stack) | DB (Decrement Before) | IA (Increment After)s |
| FA (Full Ascending stack) | IB (Increment Before) | DA (Decrement After) |
| ED (Empty Descending stack) | DA (Decrement After) | IB (Increment Before)s |
| EA (Empty Ascending stack) | IA (Increment After) | DB (Decrement Before) |
All root suffix pairs have been through exhaustive testing. Conditional execution was not implemented at a module level since this is not a module specific functionality and can be implemented at top-level.

----------
All alphabetic characters used are case-sensitive, they must be upper-case. The program is very insensitive to spaces since these are removed at top-level code. Even space between *R* and *register number* is allowed, only the space between the instruction and first operand is non-trivial, it must exist.

----------
VisUAL (doesn't throw a warning but completely) crashes if a register list contains a range from a higher number register to a lower number register. My emulation just takes it as a list of 0 registers.

----------
VisUAL seems to emulate the restricted subset of functionality called Thumb State, in that it imposes some limitations on register access. My program emulates the unrestricted ARM State. (http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0040d/CACCIDAH.html)

----------
VisUAL does not accept empty register lists. Neither does my program.

----------
My program uses the same initial memory map as VisUAL. (https://salmanarif.bitbucket.io/visual/memory_map.html)

However, VisUAL dynamically allocates memory space for instructions, I do not.

----------
Extensive property testing has been done to make sure that all monadic errors catch exceptions, have the intended priority, and get through to the output perfectly.

----------
The actual functionality of the instructions is fairly straightforward to test with unit functions when all input parameters are valid and no errors need to be thrown, so valid cases were unit tested, property testing was only applied erronous edge cases.

----------
| Tests | Completion
|--|--|
invalidOpCodes | Property tested
ValidOpCodes | Exhaustive tested
noCommaInOperands | Property tested
invalidStackPtr | Property tested
ValidStackPtr | Exhaustive tested
invalidRegList | Property tested
emptyRegList | Unit tested
invalidRegListOp | Property tested
invalidRegRange | Unit tested
nonDivisiblePtr | Property tested
smallPtr | Property tested
overMaxLocation | Property tested
underMinLocation | Property tested
Valid inputs | Unit tested by hand, <br>except at edge cases. <br>Highly predictable and uninteresting.

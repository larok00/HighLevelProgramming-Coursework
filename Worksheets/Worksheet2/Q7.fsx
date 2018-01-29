#load "Q6.fsx"

open Q6

let testChurch cN =
    show cN
    eval cN

(* 
    The function testChurch above shows a limitation of F# types. 
    The type of cN in eval cannot be determined because its first parameter f must match both: int->int and string->string. 
    The only possibility using Hindly Milner types is 'T->'T, This is however too general, because for example float->float matches this but will not work.

    This is a fundamental issue in programming languages. 
    All static type systems that are computable (so a compiler can type-check) restrict the range of programs that can be written. 
    See the end of this worksheet for one example of a type system that is much more expressive than F# (or indeed Haskell, Scala, and any Hindley-Milner based system) and will allow this example.

    testChurch could run perfectly well in F#, the only thing that prevents it being written is the type system. 
    In that sense type systems limit expressivity by prohibiting some correct programs as well as a lot of incorrect ones. 
    The ideal type system would prevent all incorrect programs, and allow all correct ones. 
    This is impossible (and can be proven so) because working out whether a program is correct or not is in general equivalent to the halting problem and not possible.

    There is therefore a trade-off. 
    The Hindley-Milner type system used by F# can be extended to a more complex system that will allow testChurch (though not more complex examples) at the cost of requiring the programmer to provide more type information, because type inference is no longer possible. 
    Haskell type classes are one solution to this beyond the scope of this module. 

    In order to experiment with complex Church Numeral operations in F# we will use a different mechanism to get round the type system.
*)
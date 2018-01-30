// type 'a option = | Some of 'a | None // defined in F# core language

let betterSqrt x = if x < 0.0 then None else Some (sqrt x)

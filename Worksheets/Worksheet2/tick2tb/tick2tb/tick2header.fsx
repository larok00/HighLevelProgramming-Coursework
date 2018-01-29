[<AutoOpen>]
module Temp
//>>>>>>>>>>>>>>> Standard header files for Tick 2 >>>>>>>>>>>>
//>>>>>>>>>>>>>>>>>>>> Start of Figure 5 code >>>>>>>>>>>>>>>>>
type Item = { Name: string; Weight: float; Utility: float}
type Partial = {Done: (Item*bool) list ; ToDo: Item list}
type Stats = {BestUtility: float; Count: int; BestPartial:Partial}
let maxWeight = 100.0
let totUtility { Done = itl} = 
    List.sumBy (function 
        |item, true -> item.Utility 
        | _ -> 0.0) itl
let totWeight p = 
    List.sumBy (function 
        | (item,true) -> item.Weight 
        | _ -> 0.0) p.Done
let branchesOf orderBy x =
    match x with 
    | {ToDo=[]} -> []
    | {ToDo= item :: others} -> 
        let first = orderBy item
        [ 
          {Done= (item, first) :: x.Done; ToDo=others}
          {Done= (item, not first) :: x.Done; ToDo=others}
        ]
//>>>>>>>>>>>>>>>>>>>> End of Figure 5 code >>>>>>>>>>>>>>>>>>>>>

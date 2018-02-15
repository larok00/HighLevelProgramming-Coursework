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

let bound (p: Partial) = 
    let doneInK = List.collect (function | (item,true) -> [item] | _ -> []) p.Done
    let maxW lst = List.sumBy (fun item -> item.Weight) lst
    let maxU lst = List.sumBy (fun item -> item.Utility) lst
    let ordToDo = List.sortByDescending (fun item -> item.Utility/item.Weight) p.ToDo
    let K = doneInK
    if maxW K > maxWeight then -1.0 else 
        let rec bound' K ordToDo =
            match ordToDo with
            | [] -> (K, [])
            | lst when maxWeight - maxW K < (List.head lst).Weight -> (K, lst)
            | h :: tl -> bound' (h :: K) tl
        let (newK, newOrdToDo) = bound' K ordToDo
        let maxGoodness = if List.isEmpty newOrdToDo then 0.0 else
                            (List.head newOrdToDo).Utility / (List.head newOrdToDo).Weight
        maxU newK + (maxWeight - maxW newK)*maxGoodness

let rec bAndB' ob st p = 
    if List.isEmpty p.ToDo then 
        if totWeight p > maxWeight || totUtility p < st.BestUtility then {BestUtility=st.BestUtility; Count=st.Count+1; BestPartial=st.BestPartial}
        else {BestUtility=totUtility p ; Count=st.Count+1 ; BestPartial=p }
    else
        if bound p <= st.BestUtility then {BestUtility=st.BestUtility; Count=st.Count+1; BestPartial=st.BestPartial}
        else 
            let theBest = (branchesOf ob p) 
                        |> List.fold (bAndB' ob) st
            {BestUtility=theBest.BestUtility; Count=theBest.Count+1; BestPartial=theBest.BestPartial}

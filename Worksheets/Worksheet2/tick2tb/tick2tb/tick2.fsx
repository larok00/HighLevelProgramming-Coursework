#load "tick2header.fsx"
//>> Skeleton code for your bound and bAndB functions.
module Tick2

let bound (p: Partial) = 
    let doneInK = List.collect (function | (item,true) -> [item] | _ -> []) p.Done
    let maxW lst = List.sumBy (fun item -> item.Weight) lst
    let maxU lst = List.sumBy (fun item -> item.Utility) lst
    let ordToDo = List.sortByDescending (fun item -> item.Utility/item.Weight) p.ToDo
    if maxW doneInK > maxWeight then -1.0 else 
        let rec bound' did toDo =
            match toDo with
            | [] -> (did, [])
            | lst when maxWeight - maxW did < (List.head lst).Weight -> (did, lst)
            | h :: tl -> bound' (h :: did) tl
        let (newK, newOrdToDo) = bound' doneInK ordToDo
        let maxGoodness = if List.isEmpty newOrdToDo then 0.0 else
                            (List.head newOrdToDo).Utility / (List.head newOrdToDo).Weight
        maxU newK + (maxWeight - maxW newK)*maxGoodness


let bAndB (ob: Item -> bool) (p: Partial) = 
    let rec bAndB' ob st (p: Partial) = 
        if totWeight p > maxWeight then {BestUtility=st.BestUtility; Count=st.Count+1; BestPartial=st.BestPartial}
            else if List.isEmpty p.ToDo then if totUtility p < st.BestUtility then {BestUtility=st.BestUtility; Count=st.Count+1; BestPartial=st.BestPartial}
                                                else if totUtility p >= st.BestUtility then {BestUtility=totUtility p ; Count=st.Count+1 ; BestPartial=p }
                                                    else if bound p <= st.BestUtility then {BestUtility=st.BestUtility; Count=st.Count+1; BestPartial=st.BestPartial}
                                                        else
                                                            let pBest = (branchesOf ob p) 
                                                                        |> List.map (bAndB' ob st) 
                                                                        |> List.maxBy (fun x -> x.BestUtility)
                                                            let count = pBest.Count+1
                                                            let theBest = if pBest.BestUtility>st.BestUtility then pBest else st
                                                            {BestUtility=theBest.BestUtility; Count=count; BestPartial=theBest.BestPartial}
    bAndB' ob {BestUtility=0.0; Count=0; BestPartial=p}

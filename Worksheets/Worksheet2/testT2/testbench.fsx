
//>> Skeleton code for your bound and bAndB functions.

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

//////////////////////////////////////////////////////////////////
let testData =
    { Done = [
                 {Name = "Item1";
                  Weight = 24.9;
                  Utility = 1.11;}, true;
                 {Name = "Item2";
                  Weight = 46.7;
                  Utility = 7.71;}, true;
                 {Name = "Item3";
                  Weight = 131.4;
                  Utility = 4.33;}, false
             ];

      ToDo = [
                {Name = "Item4";
                 Weight = 7.08;
                 Utility = 9.43;};
                {Name = "Item5";
                 Weight = 2.04;
                 Utility = 6.42;};
                {Name = "Item6";
                 Weight = 0.58;
                 Utility = 2.48;};
                {Name = "Item7";
                 Weight = 6.4;
                 Utility = 9.89;};
                {Name = "Item8";
                 Weight = 13.64;
                 Utility = 6.55;};
                {Name = "Item9";
                 Weight = 5.66;
                 Utility = 6.15;};
                {Name = "Item10";
                 Weight = 14.08;
                 Utility = 7.02;};
                {Name = "Item11";
                 Weight = 18.98;
                 Utility = 0.94;};
                {Name = "Item12";
                 Weight = 3.22;
                 Utility = 3.82;};
                {Name = "Item13";
                 Weight = 15.96;
                 Utility = 1.7;}
        ];
    }

let estimate = 48.715142
let exU, exW, its = 47.0,96.6,"Item13 Item12* Item11 Item10 Item9* Item8 Item7* Item6* Item5* Item4* Item1* Item2* Item3"
let bbResT = {
    BestUtility = 47.01;
    Count = 21;
    BestPartial = {
                    Done =
                        [({Name = "Item13";
                           Weight = 15.96;
                           Utility = 1.7;}, false); ({Name = "Item12";
                                                      Weight = 3.22;
                                                      Utility = 3.82;}, true);
                         ({Name = "Item11";
                           Weight = 18.98;
                           Utility = 0.94;}, false); ({Name = "Item10";
                                                       Weight = 14.08;
                                                       Utility = 7.02;}, false);
                         ({Name = "Item9";
                           Weight = 5.66;
                           Utility = 6.15;}, true); ({Name = "Item8";
                                                      Weight = 13.64;
                                                      Utility = 6.55;}, false);
                         ({Name = "Item7";
                           Weight = 6.4;
                           Utility = 9.89;}, true); ({Name = "Item6";
                                                      Weight = 0.58;
                                                      Utility = 2.48;}, true);
                         ({Name = "Item5";
                           Weight = 2.04;
                           Utility = 6.42;}, true); ({Name = "Item4";
                                                      Weight = 7.08;
                                                      Utility = 9.43;}, true);
                         ({Name = "Item1";
                           Weight = 24.9;
                           Utility = 1.11;}, true); ({Name = "Item2";
                                                      Weight = 46.7;
                                                      Utility = 7.71;}, true);
                         ({Name = "Item3";
                           Weight = 131.4;
                           Utility = 4.33;}, false)];
                    ToDo = [];};
                }   
let bbResF = {BestUtility = 47.01;
    Count = 263;
    BestPartial =
    { Done =
        [({Name = "Item13";
           Weight = 15.96;
           Utility = 1.7;}, false); ({Name = "Item12";
                                      Weight = 3.22;
                                      Utility = 3.82;}, true);
         ({Name = "Item11";
           Weight = 18.98;
           Utility = 0.94;}, false); ({Name = "Item10";
                                       Weight = 14.08;
                                       Utility = 7.02;}, false);
         ({Name = "Item9";
           Weight = 5.66;
           Utility = 6.15;}, true); ({Name = "Item8";
                                      Weight = 13.64;
                                      Utility = 6.55;}, false);
         ({Name = "Item7";
           Weight = 6.4;
           Utility = 9.89;}, true); ({Name = "Item6";
                                      Weight = 0.58;
                                      Utility = 2.48;}, true);
         ({Name = "Item5";
           Weight = 2.04;
           Utility = 6.42;}, true); ({Name = "Item4";
                                      Weight = 7.08;
                                      Utility = 9.43;}, true);
         ({Name = "Item1";
           Weight = 24.9;
           Utility = 1.11;}, true); ({Name = "Item2";
                                      Weight = 46.7;
                                      Utility = 7.71;}, true);
         ({Name = "Item3";
           Weight = 131.4;
           Utility = 4.33;}, false)];
      ToDo = []
    ;};}



let pList items = 
    let recs = List.map (fun i -> sprintf "%A" i) items |> String.concat ";\n"
    sprintf "[\n%s\n];" recs

let pItems x = 
    String.concat " " (x.Done |> List.map (fun (i,b) -> 
                                  i.Name+(if b then "*" else "")))

let pWeight x =  
    x.Done |> List.sumBy (fun (i,b) -> if b then i.Weight else 0.0)

/// test bAndB function
/// usage: TB2 bAndB' 
let TB2 fbAndB =
    let getNames p = 
        p.Done 
        |> List.filter (fun (_,b) -> b)
        |> List.map (fun (i,b) -> i.Name)
        |> List.sort
        |> String.concat " "
    printfn "Testing 'bAndB' maxWeight=%.1f. Best U=%.1f Best W=%.1f" maxWeight exU exW
    printfn "Best items=%s" its
    let p = testData
    [true ; false]
    |> List.map (fun b ->        
        let tbRes = if b then bbResT else bbResF
        let res = fbAndB (fun _ -> b) { BestUtility= -1.0; Count=0; BestPartial=p} p
        match res with 
        | {Count=n;BestUtility=u;BestPartial=pOut} ->
            let items = getNames pOut
            let rn = tbRes.Count
            let rItems = getNames tbRes.BestPartial
            printfn "Testing: 'fbAndB (fun _ -> %A) testData'" b
            printfn "BandB count=%d, Utility=%.1f" n u
            printfn "Best solution (* in knapsack):%s, Weight=%.1f" (pItems  pOut) (pWeight pOut)
            if n = rn && items = rItems
            then printfn "Passed!"
            else 
                printfn "Count was %d and should be %d" n rn
                printfn "Item list (* means in knapsack) was:\n %s\nand should be: \n%s" items rItems)
        



/// test bound function
/// usage 'TB1 bound'
let TB1 fBound =
    printfn "Testing 'bound' maxWeight=%.1f. Best U=%.1f Best W=%.1f" maxWeight exU exW
    printfn "Best items (* in knapsack)=%s" its
    let res = fBound testData
    if abs(res - estimate) < 1e-6 
    then 
        printfn "Passed! bound=%.1f" res
    else 
        printfn "Result was %f but should be %f" res estimate


///////////////////////////////////////////////////////////////////////

//TB1 bound // comment out when not testing
TB2 bAndB' // comment out when not testing
#load "tick2.fsx"
open Tick2
//>>>>>>>>>>>>>>>>>Testbench code>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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



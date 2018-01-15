module T1

let fRes0 n = 
    List.replicate

let fRes n rMin = 
    let e6values = [10; 15; 22; 33; 47; 68]
    let multipliers = [1; 10; 100; 1000; 10000]
    let stockResistorValues = 
        List.collect (fun multiplier -> List.map (fun e6value -> multiplier*e6value) e6values) multipliers
    
    let biggerThanMin = (List.collect (fun R -> if R<rMin then [] else [[[R]]]) stockResistorValues)
    
    printfn "%A"biggerThanMin

    let rLst = List.replicate (n+1) [[]]

    let filterAscending newCombos = 
        List.filter (fun newCombo -> ((List.reduce (fun a b -> if a<=b then b else 700000) newCombo)<690000)) newCombos
    let reducer listOfListsOfCombos _ = 
        filterAscending (List.collect (fun listOfCombo -> List.map (fun [listOfNextElement] -> listOfCombo @ listOfNextElement) biggerThanMin) listOfListsOfCombos)
    
    List.reduce reducer rLst







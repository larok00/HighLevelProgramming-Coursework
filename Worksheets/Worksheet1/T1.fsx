module T1

let fRes0 n = 
    List.replicate

let fRes (n, rMin) = 
    let stockResistorValues = 
        let e6valuesMultipliedBy multiplier= 
            let e6values = [10; 15; 22; 33; 47; 68]
            List.map (fun e6value -> multiplier*e6value) e6values
        
        let multipliers = [1; 10; 100; 1000; 10000]

        List.collect e6valuesMultipliedBy multipliers
    

    let biggerThanMin = List.collect (fun R -> if R<rMin then [] else [[[R]]]) stockResistorValues


    let iterator = List.replicate (n+1) [[]]


    let ascensionHoldsTrueForPositiveNumberList lst = 
        List.reduce (fun a b -> if a <= b then (if a = -1 then a else b) else -1) lst <> -1
    let filteredThroughIfAscending lst = 
        if (ascensionHoldsTrueForPositiveNumberList lst) then [lst] else []
    let newListOfAscendingComboFrom currListOfCombo = 
        List.collect (fun [listOfNextElement] -> filteredThroughIfAscending (currListOfCombo @ listOfNextElement)) biggerThanMin
    let reducer listOfListsOfCombos _ = 
        List.collect newListOfAscendingComboFrom listOfListsOfCombos
    
    List.reduce reducer iterator
    






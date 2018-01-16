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


    let reducer listOfListsOfCombos _ = 
        let newListOfAscendingComboFrom currListOfCombo = 

            let concatenatedIfConcatenationAscends ascendingList2 = 

                let concatenationAscends = 
                    if List.isEmpty currListOfCombo then 0<1 else 
                        if List.isEmpty ascendingList2 then 1<0 else 
                            currListOfCombo.[((List.length currListOfCombo)-1)] <= ascendingList2.[0]
                
                if (concatenationAscends) then [currListOfCombo @ ascendingList2] else []

            List.collect (fun [listOfNextElement] -> concatenatedIfConcatenationAscends (listOfNextElement)) biggerThanMin

        List.collect newListOfAscendingComboFrom listOfListsOfCombos
    

    
    List.reduce reducer iterator
    






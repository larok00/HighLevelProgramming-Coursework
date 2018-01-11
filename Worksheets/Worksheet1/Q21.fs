module Q21

let subLists x = 
    let rLst = [[]] :: (List.map (fun element -> [[element]]) x)
    let reducer listOflistsOfCombos [listOfNextElement] = 
        listOflistsOfCombos @ ( List.map (fun listOfNewCombo -> listOfNewCombo @ listOfNextElement) listOflistsOfCombos)
    

    List.reduce reducer rLst

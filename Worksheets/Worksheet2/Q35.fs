module Q35

let subLists x = 

    let reducer listOfListsOfCombos nextElement = 
        listOfListsOfCombos @ List.map (fun combo -> combo @ [nextElement]) listOfListsOfCombos

    List.fold reducer [[]] x
module Q21

let subLists x = 
    let rLst = [[]] :: (List.map (fun element -> [[element]]) x)
    let listOfNextElement listOfListOfNextElement = List.collect (id) listOfListOfNextElement
    let listOfNewCombo listOfNext listOfCombo = listOfCombo @ listOfNext
    let reducer listOflistsOfCombos listOfListOfNextElement = listOflistsOfCombos @ ( List.map (listOfNewCombo (listOfNextElement listOfListOfNextElement)) listOflistsOfCombos ) 

    List.reduce reducer rLst

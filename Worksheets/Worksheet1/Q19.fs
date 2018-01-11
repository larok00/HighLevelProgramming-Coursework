module Q19

let insertElement lst x n = 
    let (a, b) = List.splitAt n lst
    List.collect (id) [a; [x]; b]

let insertList lstOfLst xLst n = 
    let (a, b) = List.splitAt n lstOfLst
    List.collect (id) [a; xLst; b]
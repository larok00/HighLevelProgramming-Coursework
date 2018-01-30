type TUGYear = One | Two | Three | Four
let incrementUGYear = function
    | One -> Some Two
    | Two -> Some Three
    | Three -> Some Four
    | Four -> None

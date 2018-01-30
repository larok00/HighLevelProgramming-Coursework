type RList = | RNode of Head: int * Tail: RList | RNil

let RHead = function (RNode(x,_)) -> x | _ -> failwithf "RHead function applied to RNil"
let RTail = function (RNode(_,y)) -> y | _ -> failwithf "RHead function applied to RNil"

let inline (^*^) x lst = RNode(Head = x , Tail = lst)
// inline is used for overloaded operators that can apply to different types, 
// for example numeric '+' etc which can apply to int, float and other numeric types. 
// This overloading will work only if the operator type is statically resolved, 
// as will be the case for an inline operator definition.

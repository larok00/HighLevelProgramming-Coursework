module Q1

let pairs = [ (1,2) ; (1,3) ; (2,3) ; (3,4) ; (4,7) ] //list of 2-tuples for input
let makeTriple (a,b) = //generates Pythagorean triples using Euclid's formula
    let square x = x*x 
    let firstNumber = square ((square a) - (square b))
    let secondNumber = square (2*a*b)
    let thirdNumber = firstNumber + secondNumber
    (firstNumber, secondNumber, thirdNumber) //output is a 3-tuple
let pythagTriples = List.map makeTriple pairs //using the List module we map the function makeTriple over our input list

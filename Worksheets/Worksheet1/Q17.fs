module Q17

let stats1 x = 
    let theAverage = List.average x

    let n = float (List.length x)
    let variationTimesN = List.sumBy (fun i -> (i - theAverage)**2.0 ) x
    let theVariance = variationTimesN / n

    (theAverage, theVariance, x)

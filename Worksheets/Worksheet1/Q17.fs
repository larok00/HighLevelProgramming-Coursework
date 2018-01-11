module Q17

let stats1 lst = 
    let theAverage = List.average lst

    let n = float (List.length lst)
    let variationTimesN = List.sumBy (fun i -> (i - theAverage)**2.0 ) lst
    let theVariance = variationTimesN / n

    (theAverage, theVariance, lst)

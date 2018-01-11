module Q16

let stats x = 
    let theAverage = List.average x

    let n = float (List.length x)
    let variationTimesN = x |> List.map (fun i -> (i - theAverage)**2.0) |> List.reduce (+)
    let theVariance = variationTimesN / n

    (theAverage, theVariance, x)

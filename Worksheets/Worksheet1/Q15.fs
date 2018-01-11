module Q15

let raggedList n = [1.0..(float n)] |> List.map (fun i -> [1.0..(float i)])
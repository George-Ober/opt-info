let rec benout (a :int) (b :int) =
    if b == 0 
        then 1 , 0 
    else 
        let u, v = euclide b (a mod b) in 
        v, u - (a / b) * v

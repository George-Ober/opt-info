let rec bezout (a :int) (b :int) =
    if b == 0 
        then 1 , 0 
    else 
        let u, v = bezout b (a mod b) in 
        v, u - (a / b) * v

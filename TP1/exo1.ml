let rec factorielle (n :int) =
    if n = 0 then 1
    else
    n * factorielle (n - 1)

let rec puissance (a: float) (n: int) = 
    if n = 0 then 1.
    else 
    a *. puissance a (n - 1)

let rec puissanceb (a: float) (n: int) = 
    if n = 0 then 1.
    else
    	if n mod 2 = 0 then
            (puissanceb a (n / 2)) ** 2.
        else
            a *. (puissanceb a (n / 2)) ** 2.

let rec quotient (a: int) (b: int) =
    if a < b then 0 
    else
        1 + quotient (a - b) b

let rec reste (a: int) (b: int) =
    if a < b then a 
    else
        reste (a - b) b

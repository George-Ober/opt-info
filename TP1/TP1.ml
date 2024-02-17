let triple (n :int) :int = 3 * n
let double (n :int) :int = 2 * n

let fois_6 (n :int) :int =
    triple (double n)

(* Exercice 1 *)

let abs (n :int) :int =
    if n < 0 then -n else n

let test_abs :unit =
    assert (abs(30) = 30);
    assert (abs(-2) = 2);
    assert (abs(-190287109872) = 190287109872);
    print_string "on a une fonction qui marche \n"



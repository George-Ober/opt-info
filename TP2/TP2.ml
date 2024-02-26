(* George ober MPSI 1 231*)

let a (n :int) :int =
    if n < 0 then failwith "n < 0" else
    let rec a_aux (k :int) (acc :int) :int =
        if k = 0 then (8 * acc)
        else
        a_aux (k - 1) (3 * acc)
    in a_aux n 1

let b (n :int) :int =
    if n < 0 then failwith "n < 0" else
    let rec b_aux (k :int) (acc :int) :int =
        if k = 0 then 8 + acc
        else b_aux (k - 1) (6 + acc)
    in b_aux n 0



let rec c (n :int) :int =
    if n < 0 then failwith "n < 0" else
    if n = 0 then 8
    else 3 * (c(n - 1)) + 6

let a_rt = a
let b_rt = b

(* Question 4 *)

let c_rt (n :int) :int =
    let rec c_aux (k :int) (acc_somme :int) (acc_prod :int) :int =
        if k = 0 then 8 * acc_prod + acc_somme
        else c_aux (k - 1) (3 * acc_somme + 6) (acc_prod * 3)
    in c_aux n 0 1

let rec u (n :int) :int =
    if n = 0 then 5
    else if n mod 2 = 1 then
        3 * u (n - 1) + 6
    else 4 * u (n - 1) + 5

let u_rt (n :int) :int =
    let rec u_aux (k :int) (acc_somme :int) (acc_prod :int) :int =
        if k = 0 then acc_prod * 5 + acc_somme
        else if k mod 2 = 1 then
            u_aux (k - 1) (acc_prod * 6 + acc_somme) (acc_prod * 3)
        else
            u_aux (k - 1) (acc_prod * 5 + acc_somme) (acc_prod * 4)
    in u_aux n 0 1

let rec test_suite_u (n :int) :unit =
    if n = 0 then
        assert(u_rt (0) = u (0))
    else begin
        print_int (n);
        assert(u_rt (n) = u (n));
        test_suite_u (n - 1)
    end

(*
 * Exercice 2
 * Doit être mis dans un fichier `exo4.ml` ?
*)

let rec v (a :int) (b :int) (n :int) =
    if n = 0 then a
    else let j = v a b (n - 1) in b * j * j

(* let v_rt (a :int) (b :int) (n :int) =
    let v_aux (k :int) (acc :int) =
        if k = 0 then a * acc
        else v_aux (k - 1) (b *) 
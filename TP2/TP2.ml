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

let v_rt (a :int) (b :int) (n :int) =
    let a = float_of_int a in
    let b = float_of_int b in
    let n = float_of_int n in
    let rec v_aux (k :float) (expb :float) (expa :float) =
        if k = 0. then (a ** expa) *. (b ** expb)
        else v_aux (k +. -1.) (expb +. expa) (expa *. 2.)

    in int_of_float(v_aux n 0. 1.)

let rec test_suite_v (a :int) (b :int) (n :int) :unit =
    if n = 0 then
        assert(v_rt a b (0) = v a b (0))
    else begin
        print_int (n);
        assert(v_rt a b n = v a b n);
        test_suite_v a b (n - 1)
    end

(* Cette implementation est neanmoins particulierement mauvaise du point de vue de l'explosion combinatoire *)
let rec w (a :int) (b :int) (c :int) (d :int) (n :int) =
    if n = 0 then a
    else if n = 1 then b
    else
        c * (w a b c d (n - 1)) + d * (w a b c d (n - 2))

let rec pq (n :int) =
    if n = 0 then 3, 4
    else
        let (b, a) = pq(n - 1) in
            (2 * a, 7 * b + 6)

let rec p (n :int) =
    if n = 0 then 3
    else 2 * q (n - 1)
and q (n :int) =
    if n = 0 then 4
        else 7 * (p (n - 1)) + 6


let id (x :'a) :'a = x

let diago (x :'a) :'a * 'a = (x, x)

let couple (x :'a) (y :'b) :'a * 'b = (x, y)

let comp1 (c :'a*'b) :'a = let (i, j) = c in i
let comp2 (c :'a*'b) :'b = let (i, j) = c in j

let appli (f :'a -> 'b) (x :'a) :'b = f x

let compose (f :'a -> 'b) (g :'c -> 'a) :('c -> 'b) = fun x -> f (g(x))

let carre (f :'a -> 'a) :'a -> 'a = compose f f

let rec itere (f :'a -> 'a) (n :int) :'a -> 'a =
    if n = 0 then id
    else compose f (itere f (n - 1))

let f_ou_id (f :'a -> 'a) (b :bool) :'a -> 'a =
    if b then fun x -> f(x)
    else fun x -> x


let curry (f :'a * 'b -> 'c) :'a -> 'b -> 'c = fun x y -> f (x, y)

let uncurry (f :'a -> 'b -> 'c) :('a * 'b) -> 'c = fun (x,y) -> f x y

let somme1 (a :int) (b :int) :int = a - b

let somme2 (a, b :int * int) :int = a - b


let transfo (h :'a * 'b -> 'c -> 'd) :'a -> 'b * 'c -> 'd = fun x (y, z) -> h (x, y) z

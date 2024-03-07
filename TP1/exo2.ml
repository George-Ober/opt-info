(* George OBER MP1 2023-2024, Option Informatique *)
(* Exercice 2 *)

let rec puiss (a :int) (n :int) :int =
    (* Calcule la puissance entière et positive d'un entier de maniere naïve *)
    if n < 0 then failwith "Puissance negative" else
    if n = 0 then 1 else a * (puiss a (n - 1))

let puiss_rt (a :int) (n :int) :int =
    (* Version recursive terminale de `puiss` *)
    if n < 0 then failwith "Puissance negative" else
    let rec aux (m :int) (p :int) =
        if m = 0 then p
        else
            aux (m - 1) (p * a)
    in aux n 1

let rec puiss_rap (a: int) (n: int) :int = 
    (* Calcul de la puissance rapide (divisions par deux) *)
    if n < 0 then failwith "Puissance negative" else
    if n = 0 then 1
    else
    	if n mod 2 = 0 then
            let f = puiss_rap a (n / 2) in f * f
        else
            a * (let f = puiss_rap a (n / 2) in f * f)

let puiss_rap_rt (a: int) (b: int) :int = 
    (* Puissance rapide recursive terminale *)
    if b < 0 then failwith "Puissance negative" else
    let rec aux (acc :int) (p :int) (m :int) =
        if m = 0 then acc 
        else
            if m mod 2 = 0 then
                aux acc (p * p) (m / 2) 
            else
                aux (acc * p) (p * p) (m / 2) 
    in aux 1 a b

(* Tests des quatre fonctions précédentes *)

let test_puissance_f (f :(int -> int -> int)) :unit =
    assert (f 3   0  = 1   ); 
    assert (f 3   1  = 3   );
    assert (f 3   4  = 81  );
    assert (f 19  3  = 6859);
    assert (f 2   10 = 1024)

let test_puissance_4 :unit = 
    test_puissance_f puiss;
    test_puissance_f puiss_rt;
    test_puissance_f puiss_rap;
    test_puissance_f puiss_rap_rt


let rec pgcd (a :int) (b :int) :int =
    (* Calcul du PGCD avec l'algorithme d'Euclide *)
    if b = 0 then a
    else
        let r = a mod b
        in pgcd b r

(* L'algorithme trouvé ci dessus est en fait récursif terminal *)
let pgcd_rt = pgcd


let div_pos_rt (a :int) (b :int) =
    (* Calcule le couple quotient reste d'un naturel par un naturel non nul de façon récursive terminale *)
    if a < 0 then failwith "Diviseur négatif" else
    if b <= 0 then failwith "Dividende nul ou négatif" else
    let rec aux (n :int) (q :int) (acc :int) =
        if n - q < 0 then
            acc, n
        else
            aux (n - q) q (acc + 1)
    in aux a b 0

let div (a :int) (b :int) =
    (* Calcule le couple quotient reste d'un entier par un entier non nul de façon récursive terminale *)
    if b = 0 then failwith "Dividende nul" else
    let rec aux (n :int) (q :int) (acc :int) =
        let abs_q = if q < 0 then -q else q in
        if n < abs_q && n >= 0 then
            acc, n
        else
            if n * q >= 0 then
                aux (n - q) q (acc + 1)
            else 
                aux (n + q) q (acc - 1)
    in aux a b 0

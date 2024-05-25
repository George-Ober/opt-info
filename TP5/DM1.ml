(* George Ober MPSI 1 231 *)
let boucle_bouclee :unit =
    let x = ref 12 in
    while !x >= 1 do
        print_string "La boucle est bouclee\n";
        x := !x - 1
    done
    
let boucle_pour_bouclee :unit =
    for i = 1 to 12 do
        print_string "La boucle est bouclee\n"
    done


let montrer_entiers :unit =
    for i = 1 to 12 do
        print_int i;
        print_newline ();
    done;
    for i = 12 downto 1 do
        print_int i;
        print_newline ();
    done

let tab30 :int array =
    let m :int array = Array.make 30 1 in
    for i = 0 to 29 do
        m.(i) <- (i + 1)
    done;
    m

(* Autre méthode *)
(* let tab30 = Array.init 30 (fun i -> i + 1) *)


let copie_tab (a :int array) :int array =
    let l = Array.length a in
    let m :int array = Array.make l 0 in
    for i = 0 to (l - 1) do
        m.(i) <- a.(i)
    done;
    m

let somme_tab (a :int array) :int =
    let v = ref 0 in
    let l = Array.length a in
    for i = 0 to (l - 1) do
        v := !v + a.(i)
    done; !v

let indice_seuil (l :int array) (s :int) :int =
    let v = ref (-1) in
    let somme = ref 0 in
    let n = ref 0 in
    while (!v = -1 && (!n <= (Array.length l) - 1)) do
        (if (!somme >= s) then
            v := !n 
        else ());
        somme := !somme + l.(!n);
        n := !n + 1
    done; !v

(* Exercice 2 *)

type permut = int array

(* Question 1 *)

(* Il faut, bien entendu que `\sigma` soit une bijection (existence et unicité pour chaque antécédent)
$$
\forall i \in [ \! [ 0, n-1] \!], \exists ! k \in [ \! [ 0, n-1 ] \!]: \sigma (k) = i
$$
*)

(* Question 2 *)

let est_valide (p :permut) :bool =
    (* Teste si la permutation p est valide *)
    let l = Array.length p in
    let c :int array = Array.make l 0 in
    let t = ref true in
    for i = 0 to (l - 1) do
        let v = p.(i) in
        if v < 0 || v >= l then
            t := false
        else
            c.(v) <- c.(v) + 1
    done;
    for i = 0 to (l - 1) do
        if c.(i) = 0 || c.(i) >= 2 then
        t := false else () done; 
    !t

let test_est_valide :unit =
    assert(est_valide [| 1; 2; 3; 4; 5; 0|]); (* Tout est bon *)
    assert(not (est_valide tab30)); (* Le 30 pose probleme *)
    assert(not (est_valide [| -1; -2; 3; -1; 0|])) (* Valeurs négatives *)
    
(* Question 3 *)

let id (n :int) :permut =
    (* Renvoie la permutation identité de 0..(n - 1) *)
    let l = Array.make n 0 in
    for i = 0 to (n - 1) do
        l.(i) <- i
    done; l

(* Question 4 *)

let est_pt_fixe (p :permut) (n :int) :bool =
    (* Teste si n est un point fixe de la permutation p *)
    (p.(n) = n)

let test_est_fixe :unit =
    assert(est_pt_fixe (id 10) 7);
    assert(est_pt_fixe [| 1; 0; 2 |] 2)

(* Question 5 *)

let support (p :permut) :int =
    (* Renvoie le cardinal du support de p, i.e. le nombre de non points fixes *)
    let l = Array.length p in
    let c = ref 0 in
    for i = 0 to l - 1 do
        if p.(i) <> i then
            c := !c + 1
        else ()
    done;
    !c

let test_support :unit =
    assert(support (id 10) = 0);
    assert(support [|1; 0; 2|] = 2)

(* Question 6 *)
let sigma :permut = [|3; 4; 5; 6; 0; 1; 2|]
let tau :permut = [|0; 2; 1; 3; 4; 5; 6|]
let tau_sigma :permut = [|3; 4; 5; 6; 0; 2; 1|]
let sigma_tau :permut = [|3; 5; 4; 6; 0; 1; 2|]

let sigma_copie :permut = copie_tab sigma

(* Question 7 *)

let compose_d_transpo (p :permut) (i :int) (j :int) :unit =
    (* Compose la transposition (i, j) à droite de la permutation p *)
    (* On suppose que i et j sont deux entiers entre 0 et n-1 où n est le cardinal de l'ensemble où est appliquée la permutation p *)
    let l = Array.length p in
    let c = Array.make l 0 in
    for k = 0 to l - 1 do
        let n = (if k = i then j
        else if k = j then i else k) in
        c.(k) <- p.(n)
    done; 
    for k = 0 to l - 1 do
        p.(k) <- c.(k)
    done

let test_compose_d :unit =
    compose_d_transpo sigma 1 2;
    assert(sigma = sigma_tau);
    let k = [|2; 0; 4; 1; 3|] in
    compose_d_transpo k 1 3;
    assert(k = [|2; 1; 4; 0; 3|]);
    let u = [|9;0;8;7;6;5;4;3;2;1|] in
    compose_d_transpo u 3 7;
    assert(u = [|9;0;8;3;6;5;4;7;2;1|])

(* Question 8 *)

let compose_g_transpo (p :permut) (i :int) (j :int) :unit =
    (* Compose la transposition (i, j) à gauche de la permutation p *)
    (* On suppose que i et j sont deux entiers entre 0 et n-1 où n est le cardinal de l'ensemble où est appliquée la permutation p *)
    let l = Array.length p in
    let c = Array.make l 0 in
    for k = 0 to l - 1 do
        let u = p.(k) in
        let n = (if u = i then j
        else if u = j then i else u) in
        c.(k) <- n
    done; 
    for k = 0 to l - 1 do
        p.(k) <- c.(k)
    done

let test_compose_g :unit =
    compose_g_transpo sigma_copie 1 2;
    assert(sigma_copie = tau_sigma);
    let u = [|9;0;8;7;6;5;4;3;2;1|] in
    compose_d_transpo u 3 7;
    assert(u = [|9;0;8;3;6;5;4;7;2;1|])

(* Question 9 *)

let reciproque (p :permut) :permut =
    (* Calcule la bijection réciproque de la permutation p qui est supposée valide *)
    let l = Array.length p in
    let c = Array.make l 0 in
    for i = 0 to l - 1 do
        let v = p.(i) in
        c.(v) <- i
    done;
    c

(* Pour les tests, considérons les la permutation suivante
- perm1:
Départ                Arrivée 
     0 --------------> 9
     1 --------------> 0
     2 --------------> 8
     3 --------------> 3
     4 --------------> 6
     5 --------------> 5
     6 --------------> 4
     7 --------------> 7
     8 --------------> 2
     9 --------------> 1
Sa bijection réciproque :
     0 --------------> 1
     1 --------------> 9
     2 --------------> 8
     3 --------------> 3
     4 --------------> 6
     5 --------------> 5
     6 --------------> 4
     7 --------------> 7
     8 --------------> 2
     9 --------------> 0

- perm2
Départ                Arrivée
     0 --------------> 4
     1 --------------> 6
     2 --------------> 2
     3 --------------> 9
     4 --------------> 1
     5 --------------> 0
     6 --------------> 8
     7 --------------> 7
     8 --------------> 5
     9 --------------> 3
Sa bijection réciproque
     0 --------------> 5
     1 --------------> 4
     2 --------------> 2
     3 --------------> 9
     4 --------------> 0
     5 --------------> 8
     6 --------------> 1
     7 --------------> 7
     8 --------------> 6
     9 --------------> 3
*)

let test_reciproque :unit =
    let perm1 = [|9;0;8;3;6;5;4;7;2;1|] in
    assert(reciproque perm1 = [|1;9;8;3;6;5;4;7;2;0|]);
    let perm2 = [|4;6;2;9;1;0;8;7;5;3|] in
    assert(reciproque perm2 = [|5;4;2;9;0;8;1;7;6;3|])

(* Question 10 *)

let compose_permut (f :permut) (g :permut) :permut =
    (* Prend en entrée deux permutations f et g sur le __même ensemble de départ__ et calcule leur composition f \circ g *)
    let l = Array.length f in (* C'est donc la même pour les deux *)
    let c = Array.make l 0 in
    for i = 0 to l - 1 do
        c.(i) <- f.(g.(i))
    done; c
(* pi_2 et pi_1
 Départ     π₂             π₁ 
    0       ->     4       ->     9
    1       ->     9       ->     6
    2       ->     1       ->     5
    3       ->     5       ->     1
    4       ->     2       ->     7
    5       ->     0       ->     3
    6       ->     3       ->     0
    7       ->     7       ->     2
    8       ->     6       ->     8
    9       ->     8       ->     4

 Départ     π₁             π₂ 
    0       ->     3       ->     5
    1       ->     5       ->     0
    2       ->     7       ->     7
    3       ->     0       ->     4
    4       ->     9       ->     8
    5       ->     1       ->     9
    6       ->     8       ->     6
    7       ->     2       ->     1
    8       ->     4       ->     2
    9       ->     6       ->     3
On voit que ces deux permutations ne commutent pas

pi_3 et pi_4

 Départ     π₄             π₃
    0       ->     7       ->     2
    1       ->     4       ->     4
    2       ->     1       ->     3
    3       ->     3       ->     1
    4       ->     8       ->     5
    5       ->     2       ->     9
    6       ->     0       ->     6
    7       ->     6       ->     0
    8       ->     9       ->     7
    9       ->     5       ->     8

 Départ     π₃             π₄
    0       ->     6       ->     0
    1       ->     3       ->     3
    2       ->     9       ->     5
    3       ->     1       ->     4
    4       ->     4       ->     8
    5       ->     8       ->     9
    6       ->     0       ->     7
    7       ->     2       ->     1
    8       ->     5       ->     2
    9       ->     7       ->     6

*)
let test_compose_permut :unit =
    let pi_1 = [|3;5;7;0;9;1;8;2;4;6|] in
    let pi_2 = [|4;9;1;5;2;0;3;7;6;8|] in
    assert((compose_permut pi_1 pi_2) = [|9;6;5;1;7;3;0;2;8;4|]);
    assert((compose_permut pi_2 pi_1) = [|5;0;7;4;8;9;6;1;2;3|]);
    let pi_3 = [|6;3;9;1;4;8;0;2;5;7|] in
    let pi_4 = [|7;4;1;3;8;2;0;6;9;5|] in
    assert((compose_permut pi_3 pi_4) = [|2;4;3;1;5;9;6;0;7;8|]);
    assert((compose_permut pi_4 pi_3) = [|0;3;5;4;8;9;7;1;2;6|])


(* Exercice 3 *)

(* Question 1 *)

(*
σ₃:
* 0 -> 1
* 2 -> 4 -> 5
* 3 ↺
* 6 -> 7

σ₄:
* 0 ↺
* 1 -> 2
* 3 -> 6 -> 5 -> 4
* 7 ↺
*)

(* Question 2 *)

let affiche_cycles (p :permut) :unit =
    let l = Array.length p in
    let marques = Array.make l false in
    let n = ref 0 in
    for i = 0 to l - 1 do
        if marques.(i) then () else (
        n := !n + 1;
        Printf.printf "Cycle n°%d : " !n;
        Printf.printf "%d " i;
        let t = ref p.(i) in
        (while !t <> i do
            Printf.printf "%d " !t;
            marques.(!t) <- true;
            t := p.(!t)
        done);
        print_newline ()
        )
    done

let test_affiche_cycles : unit =
    print_string("- σ₂ \n");
    let sigma_2 = [|0;2;4;5;1;3;6|] in
    affiche_cycles sigma_2;
    print_string("- σ₃ \n");
    let sigma_3 = [|1;0;4;3;5;2;7;6|] in
    affiche_cycles sigma_3;
    print_string("- σ₄ \n");
    let sigma_4 = [|0; 2; 1; 6; 3; 4; 5; 7|] in
    affiche_cycles sigma_4

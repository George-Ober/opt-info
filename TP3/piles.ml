(* OBER George MPSI1 231 Option informatique *)

type 'a pile =
    | PV
    | PNV of 'a * 'a pile

let p1 :int pile = PNV(3, PNV(2, PNV( 4, PV))) 

let rec somme (p :int pile) :int = 
    (* calcule la somme des elements entiers d'une pile d'entiers *)
    match p with
    | PV -> 0
    | PNV (k,l) -> k + somme l
    
let somme_rt (p :int pile) :int =
    (* Calcule la somme des éléments entiers d'une pile d'entiers *)
    let rec aux (p :int pile) (k :int) :int =
	(* Corps récursif terminal de la fonction `somme_rt`, qui accumule les valeurs dans le second argument *)
	match p with
	    | PV -> k
	    | PNV (l, m) -> aux m (l + k)
    in aux p 0

let produit_rt (p :int pile) :int =
    (* Calcule le produit des éléments d'une pile d'entiers *)
    let rec aux (p :int pile) (k :int) :int =
	(* Corps récursif terminal de la fonction `somme_rt`, qui accumule les valeurs dans le second argument *)
	match p with
	    | PV -> k
	    | PNV (l, m) -> aux m (l * k)
    in aux p 1
    
(* Exercice 4 *)

(* 
 - Creer pile vide
 - Empiler
 - Dépiler
 - Sommet
 - Est pile vide
*)

let creer_pile_vide () : 'a pile = PV

let empiler (p :'a pile) (el :'a) :'a pile =
    (* Ajoute un élément en haut de la pile *)
    PNV(el, p)

let depiler (p :'a pile) :'a pile =
    (* Enleve un element du sommet de la pile si la pile n'est pas vide*)
    match p with
    | PV -> failwith "Pas possible de dépiler une pile vide"
    | PNV (_, a) -> a

let sommet (p :'a pile) :'a =
    (* Renvoie le sommet de la pile p si la pile n'est pas vide*)
    match p with
    | PV -> failwith "Pas de sommet pour une pile vide"
    | PNV (s, _) -> s

let est_pile_vide (p :'a pile) :bool =
    (* Teste si la pile p est une pile vide *)
    p = PV

let rec renverse_dans (p1 :'a pile) (p2 :'a pile) :'a pile =
    (* Renverse la pile p1 dans la pile p2 et ne renvoie que la deuxième pile *)
    match p1 with
	| PV -> p2
	| PNV (a, b) -> renverse_dans b (PNV(a, p2))
    
let test_renverse_dans :unit =
    assert( renverse_dans (PNV(1, PNV(2, PNV(3, PV)))) (PNV(4, PNV(5, PNV(6, PNV(7, PV))))) = PNV(3, PNV(2, PNV(1, PNV(4, PNV(5, PNV(6, PNV(7, PV))))))))

let rec est_croissante (p :'a pile) :bool =
    (* Teste si les éléments de la pile p sont rangés dans l'ordre croissant *)
    match p with
    | PV -> true
    | PNV (_, PV) -> true
    | PNV (a, b) -> a <= sommet(b) && est_croissante(b)


let test_est_croissante :unit =
    assert(est_croissante PNV(1, PNV(2, PNV(3, PNV(34, PV)))));
    assert(est_croissante PNV(1, PNV(2, PNV(3, PNV(-34, PV)))))


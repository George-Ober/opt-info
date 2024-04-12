let longueur (l :'a list) :int =
    (* Renvoie la longueur de la liste *)
    let rec aux (ell :'a list) (n :int) :int =
        (* Corps récursif terminal qui incrémente le compteur jusqu'au moment où il ne reste plus que la liste vide *)
        match ell with
            | [] -> n
            | a :: b -> aux b (n + 1)
    in aux l 0

let test_longueur :unit =
    assert(longueur [1 ; 2 ; 3] = 3);
    assert(longueur ["Trucmuche"] = 1);
    assert(longueur [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] = 1) (* Attention, il s'agit bien ici d'un tuple, donc un seul élément! *)

let est_present (l :'a list) (objet :'a) :bool =
    (* Teste si un élément `objet` est bel et bien présent dans la liste `l`. *)
    let rec aux (ell :'a list) :bool =
        (* Corps récursif terminal de la fonction qui va simplement décomposer progressivement la liste ell *)
        match ell with
        | [] -> false
        | a :: b -> if a = objet then true else aux b
    in aux l

let test_present :unit =
    assert(est_present [1; 3; 5; 7; 9] 2 = false);
    assert(est_present [-2; 7; 5; 7; 9] 7 = true);
    assert(est_present ["chien"; "chat"; "hamster"; "cochon d'inde"; "cochon-dinde"] "lémurien" = false);
    assert(est_present ["chien"; "chat"; "hamster"; "cochon d'inde"; "cochon-dinde"] "chat" = true)

let rec renverse_dans (l1 :'a list) (l2 :'a list) :'a list =
    match l1 with
    | [] -> l2
    | a :: ell1 -> renverse_dans (ell1) (a :: l2)

let renverse_dans_test :unit =
    assert(renverse_dans [1 ; 2 ; 3] [4 ; 5 ; 6] = [3 ; 2 ; 1 ; 4 ; 5 ; 6]);
    assert(renverse_dans ["chat"; "chien" ; "poisson"] ["taupe"; "ver de terre"; "hérisson"] = ["poisson"; "chien"; "chat"; "taupe"; "ver de terre"; "hérisson"])

let miroir (l :'a list) :'a list=
    (* De façon récursive, cette fonction va simplement calculer la liste des éléments de l dans l'ordre inverse *)
    renverse_dans l []

let miroir_test :unit = 
    assert(miroir [10; 9; 8; 7 ; 6] = [6; 7 ; 8; 9; 10]);
    assert(miroir [1] = [1])

(* List.rev
List.length
Je ne sais pas laquelle renverse *)

let est_present_triche (l :'a list) (b :'a) :bool =
    List.exists (fun x -> (x = b)) l


let somme_entiers (l :int list) :int =
    (* Renvoie la somme des entiers présents dans l *)
    let rec aux (ell :int list) (count :int) :int =
        (* Corps récursif terminal avec le compteur count*)
        match ell with
        | [] -> count
        | a :: b -> aux b (count + a)
    in aux l 0

let test_somme_entiers :unit =
    assert(somme_entiers [10] = 10);
    assert(somme_entiers [1; 2; -3] = 0);
    assert(somme_entiers [10; 30; -5] = 35)

let somme_float (l :float list) :float =
    (* Renvoie la somme des flottants de la liste l *)
    let rec aux (ell :float list) (count :float) :float =
        (* Corps récursif terminal avec le compteur count*)
        match ell with
        | [] -> count
        | a :: b -> aux b (count +. a)
    in aux l 0.

let test_somme_flottants :unit =
    assert(somme_float [10.] = 10.);
    assert(somme_float [1.; 2.; -3.] = 0.);
    assert(somme_float [10.; 30.; -5.] = 35.)

let rec conjonction (l :bool list) :bool =
    (* Réalise la conjonction (pas de coordination) des elements de la liste *)
    match l with
    |[] -> true
    | a :: b -> if a then conjonction b else false

let test_conjonction :unit =
    assert(conjonction [true; true; true; true; true]);
    assert(not (conjonction [true; true; false; true; true]))

let forme_mot (l :char list) :string =
    (* Concatene les char de la liste l dans un string a l'envers *)
    let rec aux (ell :char list) (s :string) :string =
        match ell with
        | [] -> s
        | a :: b -> aux b ((String.make 1 a) ^ s)
    in aux l ""

let test_forme_mot :unit =
    assert(forme_mot [')'; '8'; '7'; '-'; 'N'; 'V'; 'C'; '('; ' '; 'd'; 'r'; 'o'; 'F'; ' ';
 'S'; 'S'; 'U'] = "USS Ford (CVN-78)")

let compose_fonctions (f :'a -> 'b) (g: 'b -> 'c) :'a -> 'c =
    fun x -> f(g(x))

(* George Ober MPSI1 231 *)

type 'a proto_ab =
    | F0 of 'a
    | Nd0 of ('a proto_ab) * ('a proto_ab)

type ab_int =
    | F1 of int
    | Nd1 of int * ab_int * ab_int

type 'a ab =
    | F of 'a
    | Nd of 'a * ('a ab) * ('a ab)

type ('a, 'b) ab_bis =
    | F2 of 'a
    | Nd2 of 'b * (('a,'b) ab_bis) * (('a, 'b) ab_bis)

let a0 = F ('z')
let a1 = Nd( 'o', a0, F 'o')

let a2 = Nd( 1, F 0, Nd(4, Nd(2, F 1, F 3), F 12))
let a3 = Nd(36, Nd(12, F 4, F 8), Nd(24, F 8, F 16))

(* Exercice 2 *)

let rec hauteur (arbre :'a ab) :int =
    (* Calcule la hauteur d'un arbre binaire non vide *)
    match arbre with
    | F (_) -> 0
    | Nd (_, g, d) -> 1 + (max (hauteur g) (hauteur d))

let test_hauteur :unit =
    assert(hauteur a0 = 0);
    assert(hauteur a1 = 1);
    assert(hauteur a2 = 3);
    assert(hauteur a3 = 2)

let rec taille (arbre :'a ab) :int =
    (* Calcule la taille d'un arbre binaire non vide (i.e. noeuds internes + feuilles) *)
    match arbre with
    | F _ -> 1
    | Nd (s, g, d) -> 1 + taille(g) + taille(d)

let test_taille :unit =
    assert(taille a0 = 1);
    assert(taille a1 = 3);
    assert(taille a2 = 7);
    assert(taille a3 = 7)

let rec nb_feuilles (arbre :'a ab) :int =
    (* Calcule le nombre de feuilles uniquement *)
    match arbre with
    | F _ -> 1
    | Nd(_,g,d) -> nb_feuilles(g) + nb_feuilles(d)

let test_nb_feuilles :unit =
    assert(nb_feuilles a0 = 1);
    assert(nb_feuilles a1 = 2);
    assert(nb_feuilles a2 = 4);
    assert(nb_feuilles a3 = 4)

(* TODO: Ex2 Q4, Ex3 Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9*)

(* Exercice 3 *)
let rec affiche_prefixe (arbre: 'a ab) (trans :'a -> string) :unit =
    (* Réalise l'affichage d'un arbre etiquetté avec le type 'a (nécessite une fonction `trans` faisant la conversion 'a -> string) dans l'ordre prefixe *)
    match arbre with
    | F e -> print_string(trans e ^ ", ")
    | Nd(e, g, d) -> print_string(trans e ^ ", "); affiche_prefixe g trans; affiche_prefixe d trans

let test_affiche_prefixe :unit =
    print_string("TEST AFFICHE_PREFIXE \n");
    affiche_prefixe a3 string_of_int;
    print_string("Référence (36, 12, 4, 8, 24, 8, 16) \n ");
    affiche_prefixe a2 string_of_int;
    print_string("Référence (1, 0, 4, 2, 1, 3, 12) \n ")
    (* J'aurais fait les test pour a1 et a0 mais pas de fonction string_of_char? *)

let rec affiche_infixe (arbre: 'a ab) (trans :'a -> string) :unit =
    (* Réalise l'affichage d'un arbre etiquetté avec le type 'a (nécessite une fonction `trans` faisant la conversion 'a -> string) dans l'ordre infixe *)
    match arbre with
    | F e -> print_string(trans e ^ ", ")
    | Nd(e, g, d) ->  affiche_infixe g trans; print_string(trans e ^ ", "); affiche_infixe d trans

let test_affiche_infixe :unit =
    print_string("TEST AFFICHE INFIXE \n");
    affiche_infixe a3 string_of_int;
    print_string("Référence (4, 12, 8, 36, 8, 24, 16) \n ");
    affiche_infixe a2 string_of_int;
    print_string("Référence (0, 1, 1, 2, 3, 4, 12)\n ")

let rec affiche_postfixe (arbre: 'a ab) (trans :'a -> string) :unit =
    (* Réalise l'affichage d'un arbre etiquetté avec le type 'a (nécessite une fonction `trans` faisant la conversion 'a -> string) dans l'ordre postfixe *)
    match arbre with
    | F e -> print_string(trans e ^ ", ")
    | Nd(e, g, d) ->  affiche_postfixe g trans; affiche_postfixe d trans; print_string(trans e ^ ", ")

let test_affiche_postfixe :unit =
    print_string("TEST AFFICHE POSTFIXE \n");
    affiche_postfixe a3 string_of_int;
    print_string("Référence (4, 8, 12, 8, 16, 24, 36) \n ");
    affiche_postfixe a2 string_of_int;
    print_string("Référence (0, 1, 3, 2, 12, 4, 1)\n ")
    
let affiche_prefixe_rt (arbre :'a ab) (trans :'a -> string) :unit =
    (* Réalise l'affichage en ordre prefixe de l'arbre binaire arbre à l'aide d'une fonction de conversion 'a -> string *)
    let rec vider_liste (liste :('a ab) list) :unit =
        (* Affiche la racine du premier arbre élément de liste, et ajoute le sous arbre gauche et droit à la liste avant de recommencer *)
        match liste with
        | [] -> ()
        | (F c) :: l -> print_string(trans c ^ ", "); (vider_liste l)
        | (Nd(e, g, d)) :: l -> print_string(trans e ^ ", "); vider_liste(g :: d :: l)
    in vider_liste [arbre]

let test_affiche_prefixe_rt :unit =
    print_string("TEST AFFICHE_PREFIXE RT \n");
    affiche_prefixe_rt a3 string_of_int;
    print_string("Référence (36, 12, 4, 8, 24, 8, 16) \n ");
    affiche_prefixe_rt a2 string_of_int;
    print_string("Référence (1, 0, 4, 2, 1, 3, 12) \n ")

(* TODO: Q8 EX3 *)

let rec enfiler (file :'a list) (element :'a) :'a list =
    match file with
    | [] -> [element]
    | a :: l -> a :: enfiler l element

let affiche_par_niveaux (arbre :'a ab) (trans :'a -> string) :unit =
    let rec vider_file (file :('a ab) list) :unit =
        match file with
        | [] -> ()
        | (F c) :: l -> print_string(trans c ^ ", "); (vider_file l)
        | (Nd(e, g, d)) :: l -> print_string(trans e ^ ", "); vider_file (enfiler (enfiler l g) d)
    in vider_file [arbre]

type 'a abb =
    | V
    | N of 'a * 'a abb * 'a abb

let t0 = V
let t1 = N(true , V, V)
let t2 = N(5 , N(6, V, N(7, V, V)), V)
let t3 = N('k', N('a', N('a', V, V), N('k', N('e', V , V), V)), N('r', N('a', V, V), V))


let est_feuille (arbre_binaire :'a abb) :bool =
    match arbre_binaire with
    | N (_, g, d) when ( g = V || d = V)-> true
    | _ -> false

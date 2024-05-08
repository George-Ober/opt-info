(* George Ober MPSI1 231 *)
(* Contient exercices 1..5 *)

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

let rec prof_moy (arbre : 'a ab) : float * float =
  match arbre with
  | F _-> (1.0,0.)
  | Nd(_,g,d) -> (
        let (gn,gp) = prof_moy(g) in 
        let (dn,dp) = prof_moy(d) in 
        (gn +. (dn +. 1.), (gn *. (gp +. 1.) +. dn *. (dp +. 1.))
                                /. (gn +. dn +. 1.)) 
    )

let prof_moyenne (arbre :'a ab) :float =
    (* Calcule la profondeur moyenne des noeuds d'un arbre binaire `arbre` non vide *)
    let rec parcours (a :'a ab) (p :int) :(int * int) =
        (* Va renvoyer le nombre de noeuds, et la somme des profondeurs en faisant la somme de nombres et profondeurs des arbres sous-jacents *)
        match a with
        | F _ -> (1, p)
        | Nd(_,g,d) -> (
            let ng, sg = parcours g (p + 1) in
            let nd, sd = parcours d (p + 1) in 
            (ng + nd + 1, p + sg + sd)
        )
    in let q, r = parcours arbre 0
    in (float_of_int r) /. (float_of_int q)

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
    
(* Seulement marqué récursif dans l'énoncé de la Q7 mais puisque c'est déjà fait, j'ai fait récursif terminal *)

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

let affiche_infixe_rt (arbre :'a ab) (trans :'a -> string) :unit =
    (* Réalise l'affichage en ordre infixe de l'arbre binaire arbre de façon récursive terminale à l'aide d'une fonction de conversion 'a -> string *)
    let rec vider_liste (liste :('a ab) list) :unit =
        (* Va -entre autres-, afficher le sous arbre de droite, et ajouter la racine comme un arbre constitué d'une seule feuille pour pouvoir être traité au tour suivant *)
        match liste with
        | [] -> ()
        | (F c) :: l -> print_string(trans c ^ ", "); (vider_liste l)
        | (Nd(e, g, d)) :: l -> vider_liste(g :: (F e) :: d :: l)
    in vider_liste [arbre]

let test_affiche_infixe_rt :unit =
    print_string("TEST AFFICHE INFIXE RT \n");
    affiche_infixe_rt a3 string_of_int;
    print_string("Référence (4, 12, 8, 36, 8, 24, 16) \n ");
    affiche_infixe_rt a2 string_of_int;
    print_string("Référence (0, 1, 1, 2, 3, 4, 12)\n ")

let affiche_postfixe_rt (arbre :'a ab) (trans :'a -> string) :unit =
    (* Réalise l'affichage en ordre postfixe de l'arbre binaire arbre de façon récursive terminale à l'aide d'une fonction de conversion 'a -> string *)
    let rec vider_liste (liste :('a ab) list) :unit =
        (* Va -entre autres-, afficher le sous arbre de droite, et ajouter la racine comme un arbre constitué d'une seule feuille (mais cette fois ci après les deux sous arbres) pour pouvoir être traité au tour suivant *)
        match liste with
        | [] -> ()
        | (F c) :: l -> print_string(trans c ^ ", "); (vider_liste l)
        | (Nd(e, g, d)) :: l -> vider_liste(g :: d :: (F e) :: l)
    in vider_liste [arbre]

let test_affiche_postfixe_rt :unit =
    print_string("TEST AFFICHE POSTFIXE RT \n");
    affiche_postfixe_rt a3 string_of_int;
    print_string("Référence (4, 8, 12, 8, 16, 24, 36) \n ");
    affiche_postfixe_rt a2 string_of_int;
    print_string("Référence (0, 1, 3, 2, 12, 4, 1)\n ")

let rec enfiler (file :'a list) (element :'a) :'a list =
    match file with
    | [] -> [element]
    | a :: l -> a :: enfiler l element

let affiche_par_niveaux (arbre :'a ab) (trans :'a -> string) :unit =
    (* Réalise l'affichage de l'arbre binaire par niveaux, en revenant à la ligne après chaque profondeur *)
    let rec vider_file (file :('a ab) list) (fils :('a ab) list) :unit =
        (* La fonction garde un accumulateur pour les fils, qui sera échangé à chaque fois que la ligne sera terminée. *)
        match file with
        | [] -> if not (fils = []) then (print_newline(); vider_file fils []) else ()
        | (F c) :: l -> print_string(trans c ^ ", "); (vider_file l fils)
        | (Nd(e, g, d)) :: l -> print_string(trans e ^ ", "); vider_file l (enfiler (enfiler fils g) d)
    in vider_file [arbre] []


(* Exercice 4 *)

type 'a abb =
    | V
    | N of 'a * 'a abb * 'a abb

let t0 = V
let t1 = N(true , V, V)
let t2 = N(5 , N(6, V, N(7, V, V)), V)
let t3 = N('k', N('a', N('a', V, V), N('k', N('e', V , V), V)), N('r', N('a', V, V), V))


let est_feuille (arbre_binaire :'a abb) :bool =
    match arbre_binaire with
    | N(_, V, V) -> true
    | _ -> false

(* Exercice 5 *)
(* Pour cet exercice, je n'utilise pas le type `:'a abr` mais je continue avec :'a ab *)

let rec tous_inf (arbre_rech :'a ab) (valeur :'a) :bool =
    (* Teste si tous les noeuds de l'arbre ont une étiquette inférieure ou égale à `val` *)
    match arbre_rech with
    | F c -> c <= valeur
    | Nd(e, g, d) -> e <= valeur && tous_inf g valeur && tous_inf d valeur

let rec test_tous_inf :unit =
    assert(tous_inf a3 40);(* Cela me permet par exemple d'utiliser les arbres deja definis pour les tests *)
    assert(not (tous_inf a2 10))

let rec tous_sup (arbre_rech :'a ab) (valeur :'a) :bool =
    (* Teste si tous les noeuds de l'arbre ont une étiquette supérieure ou égale à `val` *)
    match arbre_rech with
    | F c -> c >= valeur
    | Nd(e, g, d) -> e >= valeur && tous_sup g valeur && tous_sup d valeur
 
let rec test_tous_sup :unit =
    assert(tous_sup a3 (-1));
    assert(not (tous_sup a2 (30)))

let est_de_recherche (arbre :'a ab) :bool =
    (* Teste si l'arbre est de recherche (i.e. pour tout noeud, l'arbre de gauche vérifie `tous inf` avec la valeur du noeud, et l'arbre de droite `tous_sup`)*)
    let rec parcourir (a :'a ab) :bool =
        match a with
        | F c -> true
        | Nd(e, g, d) -> tous_inf g e && tous_sup d e && parcourir g && parcourir d (* Horreur combinatoire *)
    in parcourir arbre

let ar1 = Nd(50, Nd(25, F 12, F 27), Nd(75, F 70, F 80))
let ar2 = Nd(50, Nd(25, F 12, F 27), Nd(75, F 49, F 80))

let test_est_de_recherche :unit =
    assert(est_de_recherche ar1);
    assert(not (est_de_recherche ar2))


(* Je croyais faire le malin avec un truc mieux écrit *)
(*                          |                         *)
(*                          |                         *)
(*                          |                         *)
(*                          V                         *)

(* let est_de_recherche (arbre :'a ab) :bool =
    (* Teste si l'arbre est de recherche (i.e. pour tout noeud, l'arbre de gauche vérifie `tous inf` avec la valeur du noeud, et l'arbre de droite `tous_sup`)*)
    let rec parcourir_g (a :'a ab) (i :'a) :bool =
        (* Regarde si la racine de l'arbre étudié est majorée par l'argument, puis appelle parcourir_g et parcourir_d respectivement pour le sous arbre de gauche et de droite *)
        match a with
        | F c -> c <= i
        | Nd(e, g, d) -> e <= i && parcourir_g g e && parcourir_d d e
    and parcourir_d (a :'a ab) (i :'a) :bool =
        (* Regarde si la racine de l'arbre étudié est minorée par l'argument, puis appelle parcourir_g et parcourir_d respectivement pour le sous arbre de gauche et de droite *)
        match a with
        | F c -> c >= i
        | Nd(e, g, d) -> e >= i && parcourir_g g e && parcourir_d d e
    in match arbre with
        | F _ -> true
        | Nd(e, g, d) -> parcourir_g g e && parcourir_d d e *)

let rec test_est_present (arbre :'a ab) (valeur :'a) :bool =
    (* Teste si la valeur valeur est prise par l'un des noeuds de l'arbre (supposé de recherche pour pouvoir appliquer l'algorithme de dichotomie) *)
    match arbre with 
    | F n -> n = valeur
    | Nd(e, _, _) when e = valeur -> true
    | Nd(e, g, _) when valeur < e -> test_est_present g valeur
    | Nd(e, _, d) when valeur > e -> test_est_present d valeur
    | Nd(_, _, _) -> false

let test_test_est_present :unit =
    assert(test_est_present ar1 50);
    assert(not (test_est_present ar2 139)); (* On a ici le bon résultat par chance, car ar2 n'est pas un arbre de recherche *)
    assert(not (test_est_present ar2 49)); (* Et effectivement, nos peurs sont confirmées ici puisque le résultat ici testé n'est pas le bon, puisque ar2 n'est pas un arbre de recherche*)
    assert(test_est_present ar1 12)

let rec minimum (arbre :'a abb) :'a =
    (* Renvoie la valeur minimale (donc celle la plus a gauche puisque c'est un arbre de recherche *)
    (* Note: la consigne demande de renvoyer une erreur si l'arbre est vide, mais puisque je ne fais pas l'usage de la structure `abr`, je ne renverrai que la valeur de la racine *)
    match arbre with
    | V -> failwith "eeee"
    | N(e, V, V) -> e 
    | N(_, g, _) -> minimum g

(* let test_minimum :unit =
    assert(minimum ar1 = 12) *)

let rec maximum (arbre :'a abb) :'a =
    (* Renvoie la valeur maximale (donc celle la plus a droite puisque c'est un arbre de recherche *)
    (* Note: on pourrait détecter que `arbre` n'est pas un arbre de recherche simplement en remarquant des pb de max *)
    match arbre with
    | V -> failwith "eeee"
    | N(e, V, V) -> e 
    | N(_, _, d) -> maximum d

(* let test_maximum :unit =
    assert(maximum ar1 = 80) *)

let rec insere_f (a :'a abb) (elem :'a) :'a abb =
    (* Calcule l'arbre de recherche obtenu en insérant elem en tant que feuille d'un arbre de recherche (ne remplacera jamais un noeud interne) *)
    match a with
    | N(e, V, V) when (elem <= e) -> N(e, N(elem, V, V), V)
    | N(e, V, V) when (elem > e) -> N(e, V, N(elem, V, V))
    | N(e, g, d) when (elem <= e) -> N(e, insere_f g elem, d)
    | N(e, g, d) when (elem > e) -> N(e, g, insere_f d elem)
    | _ -> N(elem, V, V)

let test_insere :unit =
    assert((insere_f (V) 1) = N(1, V, V)); (* Seulement une feuille *)
    assert((insere_f (N (1, N (0, N (0, V, V), V), N (4, N (2, V, V), V))) 2) = N (1, N (0, N (0, V, V), V), N (4, N (2, N (2, V, V), V), V))); (* On ajoute dans un arbre déjà populé par 2 (doublons) *)
    assert((insere_f (N(9, V, V)) 7) = N (9, N (7, V, V), V))

let rec supprime (a :'a abb) (elem :'a) :'a abb =
    (* De façon récursive, supprime de l'arbre binaire elem s'il existe, ou leve une exception sinon *)
    match a with
    | N(e, V, V) when (elem = e) -> V
    | N(e, N(rg, gg, gd), V) when elem = e -> N(rg, (supprime (N(rg, gg, gd)) rg), V)
    | N(e, V, N(rd, dg, dd)) when elem = e -> N(rd, V, (supprime (N(rd, dd, dd)) rd))
    | N(e, N(rg, gg, gd), d) when elem = e -> N(rg, (supprime (N(rg, gg, gd)) rg), d)
    | N(e, g, d) when elem < e -> N(e, (supprime g elem), d)
    | N(e, g, d) when elem > e -> N(e, g, (supprime d elem))
    | _ -> failwith "N'est pas dans l'arbreuh"

let rec supprime_qui_marche (t :'a abb) (elem :'a) :'a abb =
    (* Hyp : A est un ABR et contient lelement *)
    (* Calcule l'abr obtenu a partir de t en supprimant elem *)
    match t with
    | V -> failwith "Elem N'est pas de l'arbre vide"
    | N(x, g, d) -> (if elem = x then (
            match (g, d) with
            | (V, V) -> V
            | (V, d) -> d
            | (g, V) -> g
            | (g, d) -> (let m = maximum g in N(x, supprime_qui_marche g m, d)) )
        else if elem < x then N(x, supprime_qui_marche g elem, d) 
        else N(x, g, supprime d elem)
    )

let rec su (arbre :'a abb) (t :'a) :'a abb =
    match arbre with
    | V -> failwith "n'y est pas"
    | N(x, g, d) -> (
        if t = x then
            match (g, d) with
            | (V, V) -> V
            | (g, V) -> g
            | (V, d) -> d
            | (g, d) -> (let m = maximum g in su g m)
        else if t < x then N(x, su g t, d)
            else N(x, g, su d t)
    )


let rec is_bst_util (tree :'a abb) (min_val :'a) (max_val :'a) :bool =
    match tree with
    | V -> true
    | N (x, left, right) ->
        if x < min_val || x > max_val then false
        else
        is_bst_util left min_val (x - 1) && is_bst_util right (x + 1) max_val

let is_bst tree =
    is_bst_util tree (minimum tree) (maximum tree)

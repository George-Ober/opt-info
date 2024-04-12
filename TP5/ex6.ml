(* George Ober MPSI1 231 *)

type 'a arbre_g =
    | Nd of 'a * ('a arbre_g) list

let a1 :int arbre_g = Nd (1, [])
let a2 :int arbre_g = Nd (1, [Nd(11, []) ; Nd(15, [Nd(16, []) ; Nd(17, []) ; Nd(18, [])])])
let a3 :int arbre_g = Nd (1, [Nd(2, [Nd(3, [Nd(30, []); Nd(31, []); Nd(32, [])])])])
let a4 = Nd('A',[Nd('B',[Nd('E',[]);Nd('F',[])]); Nd('C',[Nd('G',[])]); Nd('D',[])])
let a5 = Nd('A',[Nd('B',[Nd('D',[]);Nd('E',[Nd('G',[]);Nd('H',[])]);Nd('F',[Nd('I',[])])]) ; Nd('C',[]) ])

let est_feuille (arbre :'a arbre_g) :bool =
    match arbre with
        | Nd (_, b) when b = [] -> true
        | _ -> false

let test_est_feuille :unit =
    assert(     est_feuille a1);
    assert(not (est_feuille a2));
    assert(not (est_feuille a3));
    assert(not (est_feuille a4))

let rec hauteur (arbre :'a arbre_g) :int =
    (* Calcule la hauteur de a (la hauteur d'une feuille est 0) *)
    match arbre with
    | Nd(_, []) -> 0
    | Nd(_, l) -> 1 + hauteur_l(l)
and hauteur_l (l :('a arbre_g) list) :int =
    (* Calcule le max des hauteurs des arbres de l *)
    match l with
    |[] -> 0
    |a::q -> max (hauteur a) (hauteur_l q)

let test_hauteur :unit =
    assert(hauteur a1 = 0); (* Arbre constitué seulement d'une feuille *)
    assert(hauteur a2 = 2);
    assert(hauteur a3 = 3);
    assert(hauteur a4 = 2);
    assert(hauteur a5 = 3) (* Cf. schéma sur le PDF *)

let rec nb_noeuds (arbre :'a arbre_g) :int =
    (* Calcule le nombre de neuds d'un arbre général en descendant feuille par feuille et faisant appel à la fonction associée *)
    match arbre with
    | Nd(_, []) -> 1
    | Nd(_, l) -> 1 + longueur_l l
and longueur_l (l :('a arbre_g) list) :int =
    (* Renvoie la somme des tailles des arbres dans la liste l *)
    match l with
    | [] -> 0
    | a :: q -> (nb_noeuds a) + (longueur_l q)

let test_nombre_noeuds :unit =
    assert(nb_noeuds a1 = 1); 
    assert(nb_noeuds a2 = 6);
    assert(nb_noeuds a3 = 6);
    assert(nb_noeuds a4 = 7);
    assert(nb_noeuds a5 = 9) 

let rec nb_feuilles (arbre :'a arbre_g) :int =
    (* Calcule le nombre de feuiles d'un arbre général en faisant la somme des nombres de feuilles des arbres sous-jacents *)
    match arbre with
    | Nd(_, []) -> 1
    | Nd(_, l) -> somme_feuilles l
and somme_feuilles (l :('a arbre_g) list) :int =
    (* Renvoie la somme des nombres de feuilles des arbres de la liste nb_feuille *)
    match l with
    | [] -> 0
    | a :: q -> (nb_feuilles a) + (somme_feuilles q)
    
let test_nombre_feuilles :unit =
    assert(nb_feuilles a1 = 1); 
    assert(nb_feuilles a2 = 4);
    assert(nb_feuilles a3 = 3);
    assert(nb_feuilles a4 = 4);
    assert(nb_feuilles a5 = 5) 

(* Exercice 8 *)
let rec est_present (arbre :'a arbre_g) (element :'a) :bool =
    (* Teste si la valeur `element` est prise par une des étiquettes des feuilles ou noeuds *)
    match arbre with
    | Nd(a, []) when a = element -> true
    | Nd(_, []) -> false
    | Nd(a, l) -> (a = element) || (est_en_dessous l element)
and est_en_dessous (l :('a arbre_g) list) (element :'a) :bool =
    (* Teste si la valeur `element` est presente dans l'un des arbres de la liste l *)
    match l with
    | [] -> false
    | a :: q -> (est_present a element) || (est_en_dessous q element)

let test_est_present :unit =
    assert(est_present a1 1);(* Racine toute seule *)
    assert(not (est_present a1 8));(* Element absent *)
    assert(est_present a2 16);(* Feuille *)
    assert(est_present a2 15);(* Noeud interne *)
    assert(est_present a2 1); (* Racine *)
    assert(est_present a5 'C'); (* Noeud interne *)
    assert(not(est_present a5 'Z')) (* Absent *)

let rec est_en_feuille (arbre :'a arbre_g) (element :'a) :bool =
    (* Teste si la valeur `element` est prise par une des étiquettes des feuilles UNIQUEMENT *)
    match arbre with
    | Nd(a, []) when a = element -> true
    | Nd(_, []) -> false
    | Nd(_, l) -> (est_en_dessous_f l element)
and est_en_dessous_f (l :('a arbre_g) list) (element :'a) :bool =
    (* Teste si la valeur `element` est presente en tant qu'etiquette de feuille dans les arbres de la liste l*)
    match l with
    | [] -> false
    | a :: q -> (est_en_feuille a element) || (est_en_dessous_f q element)

let test_est_en_feuille :unit =
    assert(est_en_feuille a1 1);            (* Feuille toute seule *)
    assert(not (est_en_feuille a1 3));      (* On sait jamais *)
    assert(not (est_en_feuille a2 15));     (* C'est un noeud interne *)
    assert(est_en_feuille a2 16);
    assert(not (est_en_feuille a3 3));      (* C'est un noeud interne *)
    assert(est_en_feuille a5 'H');          (* C'est bien une feuille *)
    assert(not (est_en_feuille a5 'F'));    (* C'est un noeud interne *)
    assert(not (est_en_feuille a5 'Z'));    (* N'existe meme pas *)
    assert(est_en_feuille a5 'C')           (* Une autre feuille pour le test *)


let rec tout_montrer (arbre :'a arbre_g) (affichage :'a -> unit) :unit =
    match arbre with
    | Nd(a, []) -> affichage a
    | Nd(a, l) -> (tout_liste l affichage); affichage a; print_newline()
and tout_liste (l_arbre :('a arbre_g) list) (affichage :'a -> unit) :unit =
    match l_arbre with
    | [] -> ()
    | a :: q -> tout_liste q affichage; tout_montrer a affichage

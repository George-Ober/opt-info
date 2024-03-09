(* OBER George MPSI1 231 Option informatique *)

type valeur =
 | Brele of int
 | Tete of string

type couleur =
 | Pique
 | Coeur
 | Trefle
 | Carreau

(* Note: il serait intéressant de définir les constructeurs avec des contraintes sur les valeurs possibles pour Brele et Tete *)

type carte = couleur * valeur
let dame_de_coeur :carte = (Coeur, Tete("Dame"))
let trois_de_pique :carte = (Pique, Brele(3))
let valet_de_pique :carte = (Pique, Tete("Valet"))
let six_de_trefle :carte = (Trefle, Brele(6))

let est_rouge (c :carte) : bool =
    (* Teste si c est rouge *)
    let (clr, _) = c in
    clr = Coeur || clr = Carreau

let est_rouge_2 (c :carte) :bool = 
    (* Teste si c est rouge, variante avec le filtrage *)
    let (clr, _) = c in
    match clr with
	| Coeur | Carreau -> true
	| _ -> false

let test_est_rouge (f :carte -> bool): unit =
    (* Factorisation des tests pour les deux fonctions précédentes *)
    assert(f dame_de_coeur);
    assert(f (Carreau, Brele(4)));
    assert(f (Coeur, Brele(8)));
    assert(not(f trois_de_pique));
    assert(not(f valet_de_pique));
    assert(not(f six_de_trefle))

let nom_carte (c :carte) :string =
    (* Calcule le nom complet d'une carte sous forme d'une chaine de caractère *)
    let (clr, vlr) = c in
    let chaine_valeur :string =
	match vlr with
	| Brele nom -> string_of_int nom
	| Tete num -> num
    and chaine_couleur :string =
	match clr with 
	| Pique -> "Pique"
	| Coeur -> "Coeur"
	| Trefle -> "Trèfle"
	| Carreau -> "Carreau"
    in chaine_valeur ^ " de " ^ chaine_couleur	

let test_nom :unit =
    assert(nom_carte valet_de_pique = "Valet de Pique");
    assert(nom_carte dame_de_coeur = "Dame de Coeur");
    assert(nom_carte six_de_trefle = "6 de Trèfle");
    assert(nom_carte (Carreau, Brele(3)) = "3 de Carreau");
    assert(nom_carte (Pique, Tete("As")) = "As de Pique")

let affiche_carte (c :carte) :unit = print_string(nom_carte(c) ^ "\n")

let valeur_entier (v :valeur) :int = 
    (* Renvoie une valeur entière 2..14 d'un objet v de type :valeur, ou une erreur si la valeur est aberrante, avec la convention qu'un As a pour valeur `Tete "As"`*)
    match v with
    (* | Brele k -> if k >= 2 && k <= 10 then k else failwith "Probleme de valeur" *)
    | Brele k when (k >= 2 && k <= 10) -> k
    | Brele _ -> failwith "Probleme de Valeur"
    | Tete nom -> let k = 
	match nom with
	| "Valet" -> 11
	| "Dame" -> 12
	| "Roi" -> 13
	| "As" -> 14
	| _ -> failwith "Probleme de tete"
	in k

let test_valeur_entier :unit =
    assert(valeur_entier (Brele(6)) = 6);
    assert(valeur_entier (Brele(5)) = 5);
    assert(valeur_entier (Tete("As")) = 14);
    assert(valeur_entier (Tete("Roi")) = 13);
    assert(valeur_entier (Tete("Dame")) = 12);
    assert(valeur_entier (Tete("Valet")) = 11)

let bataille (a :carte) (b :carte) :bool =
    (* Teste si la carte a l'emporte sur la carte b en convertissant la valeur de la carte avec la fonction `valeur_entier` qui définit l'ordre 2..10 < Valet < Dame < Roi < As
    Les cartes sont donc supposées exister avec la convention qu'un As de n'importe quelle couleur a pour valeur `Tete "As"` *)
    let (_, val_a) = a in
    let (_, val_b) = b in
    let inta :int = valeur_entier val_a in
    let intb :int = valeur_entier val_b in
    if inta = intb then
	failwith "Erreur de valeur identique"
	(* Il pourrait aussi être pertinent d'introduire un ordre des couleurs e.g. : Pique > Coeur > Trefle > Carreau *)
    else inta > intb

let test_bataille :unit =
    assert(bataille valet_de_pique dame_de_coeur = false);
    assert(bataille valet_de_pique six_de_trefle = true);
    (*assert(bataille (Trefle, Brele(9)) (Carreau, Brele(9)) = false) le test avec l'erreur *)
    assert(bataille (Pique, Tete("As")) (Coeur, Brele(7)) = true);
    assert(bataille (Pique, Tete("Roi")) (Coeur, Tete("As")) = false);
    assert(bataille (Carreau, Brele(2)) (Coeur, Brele(7)) = false)

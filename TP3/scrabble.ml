(* OBER George MPSI1 231 Option informatique *)

type piece =
    | Blanc
    | Lettre of char

type effet =
    | Aucun
    | Double_lettre
    | Double_mot
    | Triple_lettre
    | Triple_mot 

(* Autre Syntaxe
type vise = 
    | Lettre
    | Mot

type effet =
    | Aucun
    | Double of vise
    | Triple of vise *)

type case = 
    | Vide of effet
    | Couvert of piece

(* let points_lettre (p :piece) (e :effet) :int =
J'ai du mal a comprendre s'il nous faut trouver une liste des valeurs des lettres pour pouvoir implémenter cette fonction *)

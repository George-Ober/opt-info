type 'a arbre = Noeud of 'a * ('a arbre list);;

let rec nb_feuilles (a :'a arbre) :int =
    match a with
    | Noeud (_, []) -> 1
    | Noeud (_, l) -> vider_pile l
and vider_pile (p :'a arbre list) :int =
    match p with
    | [] -> 0
    | t :: q -> (nb_feuilles t) + (vider_pile q)

type arbre = Nil | Noeud of arbre * arbre

let rec genere_complet (n :int) :arbre =
    match n with
    | 0 -> Nil
    | v -> let a = genere_complet (v - 1) in Noeud(a, a)

let cheminement (a :arbre) :int =
    let rec aux (ab :arbre) (profondeur :int) :int = 
        match ab with
        | Nil -> 0
        | Noeud(Nil, Nil) -> profondeur
        | Noeud(g, d) -> (aux g (profondeur + 1)) + (aux d (profondeur + 1))
    in aux a 0

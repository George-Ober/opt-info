type 'a arbre =
    | Feuille of 'a
    | Noeud of 'a * 'a arbre * 'a arbre

let attribution_numeros (ab :'a arbre) :(int * 'a) arbre =
    let rec aux (a :'a arbre) (n :int) :(int * 'a) arbre =
        match a with
        | Feuille v -> Feuille(n, v)    
        | Noeud(e, g, d) -> Noeud((n, e), (aux g (2*n)), (aux d (2*n + 1)))
    in aux ab 1

type arbre =
    | Nil
    | Noeud of int * arbre * arbre

let rec profondeur a n =
    match a with
    | Nil -> []
    | Noeud (k, _, _) when n = 0 -> [k]
    | Noeud (_, fg, fd) -> let lg = profondeur fg (n - 1)
                            and ld = profondeur fd (n - 1)
                            in lg @ ld

let meilleur a n =
    let rec aux acc n ab =
        match ab with
        | Nil -> acc
        | Noeud (k,_,_) when n = 0 -> k :: acc
        | Noeud (_,fg, fd) ->   let acc1 = aux acc (n-1) fd in aux acc1 (n-1) fg
    in aux [] n a

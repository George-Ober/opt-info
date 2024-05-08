(* pour la première partie sur les arbres binaires non vides *)
let a0 = F ('z')
let a1 = Nd( 'o', a0, F 'o')

let a2 = Nd( 1, F 0, Nd(4, Nd(2, F 1, F 3), F 12))
let a3 = Nd(36, Nd(12, F 4, F 8), Nd(24, F 8, F 16))




(* pour la 2e partie sur les arbres binaires éventuellement vides *)
let t0 = V
let t1 = N(true , V, V)
let t2 = N(5 , N(6, V, N(7, V, V)), V)
let t3 = N('k', N('a', N('a', V, V), N('k', N('e', V , V), V)), N('r', N('a', V, V), V))

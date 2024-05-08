(*pour un meilleur affichage, régler la largeur des tabulations à 2*)

type 'a arbre_g =
    | Nd of 'a * ('a arbre_g) list

let a4 = Nd('A',[Nd('B',[Nd('E',[]);Nd('F',[])]); 
								 Nd('C',[Nd('G',[Vide])]); Nd('D',[Vide])])
let a5 = Nd('A',[Nd('B',[Nd('D',[Vide]);
												 Nd('E',[Nd('G',[Vide]);Nd('H',[Vide])]);
												 Nd('F',[Nd('I',[Vide])])]) ; 
								 Nd('C',[Vide]) ])



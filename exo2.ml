(* Question 2.1 *)

let f1 ((x, y) : 'a * 'a) (z : bool) : 'a -> bool =
  let g (a : 'a) (b : 'a) : bool = a < b in
  if z then g x else g y

(*
[z] est un booleen puisqu'il est utilise dans une condition.

[g] renvoie un booleen puique [<] est de type ['a -> 'a -> bool].
Donc [f1], en tant qu'application partielle de [g],
renvoie une fonction à un argument de type de retour [bool].

Enfin, [x] et [y] doivent avoir le meme type
comme on a l'appel [g x] et l'appel [g y],
type qu'attend [f1 (x, y) z] en entree.
*)

(* Question 2.2 *)

let list_sum (p : 'a -> bool) : 'a list -> int =
  List.fold_left (fun x y -> if p y then x + 1 else x) 0

(* [list_sum p] renvoie une fonction qui compte le nombre d'elements d'une liste
   qui satisfont le predicat p *)

let list_or : bool list -> bool = List.fold_left (fun x y -> x || y) false

(* [list_or] determine si une liste de booleens contient au moins une fois [true] *)

(*
ligne	type de a
4	'a list
6	bool list
8	'a list ref
10	bool list ref
*)

(* [list_sum] attend un predicat en premier argument
   donc les lignes 5 et 9 sont mal typees *)
   
   (* Question 2.3 *)

type 'a arbre_binaire =
  | Feuille
  | Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire

let rec map_arbre (arbre : 'a arbre_binaire) (f : 'a -> 'b) : 'b arbre_binaire =
  match arbre with
  | Feuille -> Feuille
  | Noeud (a, gauche, droite) ->
      Noeud (f a, map_arbre gauche f, map_arbre droite f)

let rec forall_arbre (arbre : 'a arbre_binaire) (p : 'a -> bool) : bool =
  match arbre with
  | Feuille -> true
  | Noeud (a, gauche, droite) ->
      p a && forall_arbre gauche p && forall_arbre droite p

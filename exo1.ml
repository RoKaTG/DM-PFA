(* Question 1.1 *)
(*Premiere methode :*)

let f a b = a + b
let ajout_deux g = if g = 0 then f 2 0 else f (2 + 1) (g - 1)

(*Deuxieme methode :*)

let f a b = a + b
let ajout_deux g = if g = 0 then f 2 0 else f (2 + 1) g - 1


(* Question 1.2 *)

let rec (* et pas [let] *) somme_derniers l1 l2 =
  match l1 with
  | [] -> (
      (* parenthèse pour grouper *)
      match l2 with
      | [] -> 0
      | x2 :: [] -> x2
      | hd2 :: tl2 -> somme_derniers [] tl2)
  | x1 :: [] -> (
      (* parenthèse pour grouper *)
      match l2 with
      | [] -> x1
      | x2 :: [] -> x1 + x2
      | hd2 :: tl2 (* et pas [hd2 @ tl2] *) -> somme_derniers [ x1 ] tl2)
  | hd1 :: tl1 -> somme_derniers tl1 l2


(* Question 1.3 *)

let x a b = a + (* et pas [.+] *) b

let f y z =
  let v = y + 5 in
  (* [let in] et pas [let] *)
  if z > y then (
    (* parenthèse pour grouper *)
    Printf.printf "%d" z;
    z)
  else x v 0


(* Question 1.4 *)

(*
ligne	  lieur de u	  lieur de z	  lieur de y
1	          -		          -		        ligne 1
2	        ligne 2		    ligne 2		      -
3	          -		        ligne 2		    ligne 1
4	          -		          -		        ligne 4
5	          -		          -		        ligne 5
6	          -	          	-		        ligne 4
7	        ligne 2		    ligne 7		    ligne 5
8	          -		        ligne 7		    ligne 4
9	        ligne 9		      -		          -
10	      ligne 9		      -		        ligne 1
*)

(* Question 4.1 *)

let rec fibo n =
  if n < 0 then failwith "undefined"
  else if n < 2 then n
  else fibo (n - 1) + fibo (n - 2)

(* Question 4.2 *)

let count : int ref = ref 0

let rec fibocount n =
  incr count;
  if n < 0 then failwith "undefined"
  else if n < 2 then n
  else fibocount (n - 1) + fibocount (n - 2)

(* La croissance de [count] est exponentielle en [n]. *)

(* Question 4.3 *)

let f : int array = Array.make 1024 (-1)

let rec memofibocount n =
  incr count;
  if n < 0 then failwith "undefined"
  else if n < 2 then n
  else fibocheck (n - 1) + fibocheck (n - 2)

and fibocheck n =
  if f.(n) = -1 then f.(n) <- memofibocount n;
  f.(n)

(* Question 4.4 *)

(* Cette fois la croissance de [count] est lineaire en [n]. *)
(* En 4.1 le meme calcul etait effectue a de nombreuses reprises
   alors qu'ici on ne calcule une valeur qu'une fois avant de la stocker *)

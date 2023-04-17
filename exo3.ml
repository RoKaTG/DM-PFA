type grid = int list list

(* Question 3.1 *)

let g_example : grid = [ [ -2; 0; 1; 4 ]; [ 7; 2; -3; -4 ]; [ 6; -1; 3; 5 ] ]

(* Question 3.2 *)

let height g =
  match g with 
    [] -> failwith "undefined height" 
  | col :: cols -> List.length col

(* Question 3.3 *)

let rec forall l  p =
  match l with 
    [] -> true 
  | x :: xs -> p x && forall xs p

let wf_grid_exn g  =
  let h = height g in
  assert (forall g (fun col -> List.length col = h))

(* Question 3.4 *)

(* Le chemin 4 -> 7 -> 6 a pour somme 17. *)

(* Question 3.5 *)

let rotate_up l =
  match l with 
    [] -> [] 
  | x :: xs -> xs @ [ x ]

(* Question 3.6 *)

let rec get_last l =
  match l with
  | [] -> failwith "undefined get_last"
  | [ x ] -> (x, [])
  | x :: xs ->
      let y, ys = get_last xs in
      (y, x :: ys)

let rotate_down l =
  let x, xs = get_last l in
  x :: xs

(* Question 3.7 *)

(* Question 3.8 *)

    (*let rec sums g = 
       match g with
       | [] -> []
       | [ col ] -> col
       | col :: cols -> list.map (best_option col) (sums cols) ( + )*)

    
(* Question 3.9 *)

let rec max_list l =
  match l with
  | [] -> failwith "undefined max_list"
  | [ x ] -> x
  | x :: xs -> max x (max_list xs)

(* Question 3.10 *)

                 (*let solve g = 
                    max_list (sums g)
                  let () = print_int (solve g_example)*)
      
      

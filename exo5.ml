type command = Up | Down | Left | Right | Seq of command list

(* Question 5.1 *)

let rec evalposfrom (x, y) cmd =
  match cmd with
  | Up -> (x, y + 1)
  | Down -> (x, y - 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)
  | Seq [] -> (x, y)
  | Seq (c :: cs) ->
      let x', y' = evalposfrom (x, y) c in
      evalposfrom (x', y') (Seq cs)

let evalpos (c : command) : int * int =
  let x, y = evalposfrom (0, 0) c in
  if x < -100 || x > 100 || y < -100 || y > 100 then failwith "out of bounds"
  else (x, y)

(* Question 5.2 *)

type command2 =
  | Up of int
  | Down of int
  | Left of int
  | Right of int
  | Seq of command2 list

let rec evalposfrom2 (x, y)  cmd =
  match cmd with
  | Up n -> (x, y + n)
  | Down n -> (x, y - n)
  | Left n -> (x - n, y)
  | Right n -> (x + n, y)
  | Seq [] -> (x, y)
  | Seq (c :: cs) ->
      let x', y' = evalposfrom2 (x, y) c in
      evalposfrom2 (x', y') (Seq cs)

let evalpos2 c  =
  let x, y = evalposfrom2 (0, 0) c in
  if x < -100 || x > 100 || y < -100 || y > 100 then failwith "out of bounds"
  else (x, y)

(* Question 5.3 *)

type amount = Number of int | Infinity

type command3 =
  | Up of amount
  | Down of amount
  | Left of amount
  | Right of amount
  | Seq of command3 list

let rec has_infinity cmd =
  match cmd with
  | Up Infinity | Down Infinity | Left Infinity | Right Infinity -> true
  | Up _ | Down _ | Left _ | Right _ -> false
  | Seq (c :: cs) -> has_infinity c || has_infinity (Seq cs)

let rec evalposfrom3 (x, y) cmd =
  match cmd with
  | Up Infinity | Down Infinity | Left Infinity | Right Infinity ->
      failwith "infinite repetition"
  | Up (Number n) -> (x, y + n)
  | Down (Number n) -> (x, y - n)
  | Left (Number n) -> (x - n, y)
  | Right (Number n) -> (x + n, y)
  | Seq [] -> (x, y)
  | Seq (c :: cs) ->
      let x', y' = evalposfrom3 (x, y) c in
      evalposfrom3 (x', y') (Seq cs)

let safety cmd =
  if has_infinity cmd then false
  else
    let x, y = evalposfrom3 (0, 0) cmd in
    not (x < -100 || x > 100 || y < -100 || y > 100)

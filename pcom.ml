(* basic parser combinator lib *)

type 'a result =
  | Succ of ('a * string)
  | Fail of string * string

let succ v = Succ v
let fail l e = Fail (l, e)

type 'a parser = {
  p : string -> 'a result;
  label : string
}

let run_p p s = p.p s

let p_of f = { p = f; label = "<unknown>" }
let update_label p l = {
  p = (fun s ->
  match run_p p s with
  | Succ (v, s') -> succ (v, s')
  | Fail (_, e) -> fail l e
  );
  label = l
}

let ( <?> ) = update_label

let rest_of_string s =
  let n = String.length s in
  String.sub s 1 (n - 1)

let pchar c : char parser = p_of @@ fun s ->
  if s = "" then fail "pchar" "the string is empty!" else
  if s.[0] = c then succ (c, rest_of_string s)
  else fail "pchar" @@ Printf.sprintf "Expected char %c, but instead got %c" c s.[0]

let a_then_b a b = p_of @@ fun s ->
  match run_p a s with
  | Succ (res_a, s') -> (
    match run_p b s' with
    | Succ (res_b, s'') -> succ ((res_a, res_b), s'')
    | Fail (a, b) -> fail a b
  )
  | Fail (a, b) -> fail a b 

let ( *>>* ) = a_then_b

let a_or_b a b = p_of @@ fun s ->
  match run_p a s with
  | Succ (res, s') -> succ (res, s')
  | Fail _ -> run_p b s

let ( *|* ) = a_or_b

let returnP x = p_of @@ fun s ->
  succ (x, s)

let choice l : 'a parser =
  match l with
  | [] -> failwith "called choice on empty selection!"
  | x :: rest -> List.fold_left (fun acc x -> acc *|* x) x rest

let any_of (chars: char list) : char parser =
  chars
  |> List.map pchar
  |> choice

let parse_lowercase =
  any_of @@ (List.init 26 (fun x -> x + Char.code 'a') |> List.map Char.chr)

let parse_digit =
  any_of @@ (List.init 10 (fun x -> x + Char.code '0') |> List.map Char.chr)

let reduce f l =
  match l with
  | [] -> failwith "reduce can't work on empty lists!"
  | x :: rest -> List.fold_left f x rest

let mapP f p = p_of @@ fun s ->
  let res = run_p p s in
  match res with
  | Succ (res', s') -> succ (f res', s')
  | Fail (a, b) -> fail a b

let ( <!> ) = mapP
let ( |>> ) a f = mapP f a

let applyP fP xP =
  ( fP *>>* xP )
  |>> (fun (f, x) -> f x)

let ( <*> ) = applyP

let lift2 fp ap bp =
  fp <!> ap <*> bp

let rec sequence parserList =
  let cons h t = h :: t in
  let consP = lift2 cons in
  match parserList with
  | [] -> returnP []
  | head :: tail ->
    consP head (sequence tail)

let char_list_to_string l =
  l
  |> List.to_seq
  |> String.of_seq

let pstring s =
  s
  |> String.to_seq
  |> List.of_seq
  |> List.map pchar
  |> sequence
  |> mapP char_list_to_string

let rec parse_zero_or_more_times p s =
  let first = run_p p s in
  match first with
  | Fail (_, _) -> ([], s)
  | Succ (v, s') ->
    let (rest, s'') = parse_zero_or_more_times p s' in
    (v :: rest, s'')

let many p = p_of @@ fun s ->
  succ @@ parse_zero_or_more_times p s

let whitespace_char = any_of [' '; '\n'; '\t']
let whitespace = many whitespace_char

let many1 p = p_of @@ fun s ->
  match run_p p s with
  | Fail (a, b) -> fail a b
  | Succ (v, s') -> (
    let (rest, s'') = parse_zero_or_more_times p s' in
    succ @@ (v :: rest, s'')
  )

let opt p =
  let some = p |>> (fun x -> Some x) in
  let none = returnP None in
  some *|* none

let ( *>> ) p1 p2 = 
  p1 *>>* p2
  |>> fst

let ( >>* ) p1 p2 = 
  p1 *>>* p2
  |>> snd

let between p1 p2 p3 = 
  p1 >>* p2 *>> p3

let bindP f p = p_of @@ fun s ->
  let res1 = run_p p s in
  match res1 with
  | Succ (v, s') ->
    let p2 = f v in
    p2 s'
  | Fail (a, b) -> fail a b

let ( >>= ) p f = bindP f p

let except c = p_of @@ fun s ->
  if s == "" then
    fail "except" "String is empty!"
  else
    let c' = s.[0] in
    if c <> c' then
      succ (c', rest_of_string s)
    else
      fail "except" (Printf.sprintf "The char %c matches!" c)

let exceptp p = p_of @@ fun s ->
  match run_p p s with
  | Succ(_, _) -> fail "exceptp" "Expected parser to fail!"
  | Fail(_, _) -> succ ("", s)

let eof = p_of @@ fun s ->
  if s = "" then
    succ ("", "")
  else
    fail "eof" "Not EOF yet!"

let should_fail a b = p_of @@ fun s ->
  match run_p a s with
  | Succ(_, _) -> fail "should_fail" "Expected parser to fail!"
  | Fail(_, _) -> run_p b s

let any = p_of @@ fun s ->
  if s = "" then
    fail "any" "String is empty!"
  else
    succ (s.[0], rest_of_string s)

let until p o =
  many ((exceptp p) *>>* o)

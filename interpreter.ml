open Ast

type funcstore = (string * ((string * Types.tt) list * Typecheck.TypedAst.stmt list)) list 
type varstoret = (string * (Typecheck.TypedAst.expr)) list

let impossible s =
  failwith @@ Printf.sprintf "TODO: %s: Impossible case!" s

let todo s =
  failwith @@ Printf.sprintf "TODO: %s" s

let unsafe_bool v = 
  let open Typecheck.TypedAst in
  match v with
  | Bool b -> b
  | _ -> failwith "Panic! Failed safe extraction of bool!"

let unsafe_char v =
  let open Typecheck.TypedAst in
  match v with
  | Char c -> c
  | _ -> failwith "Panic! Failed safe extraction of char!"

let unsafe_int v =
  let open Typecheck.TypedAst in
  match v with
  | Int i -> i
  | _ -> failwith "Panic! Failed safe extraction of int!"

let unsafe_float v =
  let open Typecheck.TypedAst in
  match v with
  | Float f -> f
  | _ -> failwith "Panic! Failed safe extraction of float!"

let unsafe_string v =
  let open Typecheck.TypedAst in
  match v with
  | String s -> s
  | _ -> failwith "Panic! Failed safe extraction of string!"

let unsafe_struct v =
  let open Typecheck.TypedAst in
  match v with
  | Struct(_, members) -> members
  | _ -> failwith "Panic! Failed safe extraction of struct!"

let unsafe_array v = 
  let open Typecheck.TypedAst in
  match v with
  | Array(_, a) -> a
  | _ -> failwith "Panic! Failed safe extraction of array!"

let rec evaluate_expr funcs varstore e =
  print_endline @@ Printf.sprintf "DEBUG: %s" (Typecheck.TypedAst.string_of_expr e);
  let ev e = evaluate_expr funcs varstore e in
  let open Typecheck.TypedAst in
  let is_builtin s = 
    match s with
    | "print"
    | "input" -> true
    | _ -> false
  in
  let eval_builtin fname args =
    match fname with
    | "print" -> (
      match args with
      | [x] ->
        let x' = evaluate_expr funcs varstore x in
        (match x' with
        | String(s) -> print_endline s; Unit
        | _ -> failwith "Something went wrong, tried to print a non-string"
        )
      | _ -> failwith "Something went wrong, expected only one argument to print"
    )
    | "input" -> let s = read_line () in String(s)
    | _ -> failwith @@ Printf.sprintf "Not a builtin function! %s" fname
  in
  let modulo a b =
    let res = a mod b in
    if res >= 0 then res
    else -res
  in
  let (%) = modulo in
  match e with
  | Unit
  | Bool(_)
  | Char(_)
  | Int(_)
  | Float(_)
  | String(_) -> e
  | Ident(_, s) -> 
    let v = List.assoc s varstore in
    evaluate_expr funcs varstore v
  | FuncCall(_, name, args) -> 
    if is_builtin name then eval_builtin name args else
    let (arg_names_and_types, body) = List.assoc name funcs in
    let (arg_names) = List.map fst arg_names_and_types in
    let evalled_args = List.map (evaluate_expr funcs varstore) args in
    let new_varstore = (List.combine arg_names evalled_args) @ varstore in
    let (retval, _, _) = run_statement_list funcs new_varstore body in
    retval
  | IndexAccess(_, a, i) ->
    let a' = unsafe_array @@ ev a in
    let i' = unsafe_int @@ ev i in
    (match List.nth_opt a' i' with
    | Some v -> v
    | None -> failwith @@ Printf.sprintf "IndexAccess: %d is out of bounds!" i'
    )
  | MemberAccess(_, s, member) ->
    let s' = unsafe_struct @@ ev s in
    List.assoc member s'
  | Struct(t, s) -> 
    Struct(t, (List.map (fun (name, v) -> (name, ev v)) s))
  | Array(t, l) -> Array(t, List.map ev l)
  | CastTo(t, e') ->
    let e'' = evaluate_expr funcs varstore e' in
    (match t, (Typecheck.TypedAst.type_of e'') with
    | Bool, Char ->
      let c = unsafe_char e'' in
      if c = "\x00" then Bool false else Bool true
    | Bool, Int ->
      let v = unsafe_int e'' in
      if v = 0 then Bool false else Bool true
    | Bool, Float ->
      let v = unsafe_float e'' in
      let eps = 0.000001 in
      if v < eps then Bool false else Bool true
    | Bool, String ->
      let v = unsafe_string e'' in
      if v = "" then Bool false else Bool true
    | Char, Int ->
      let v = unsafe_int e'' in
      let buf = BatUTF8.Buf.create 0 in
      let c = BatUChar.of_int v in
      BatUTF8.Buf.add_char buf c;
      let s = BatUTF8.Buf.contents buf in
      Char(s)
    | Int, Bool ->
      let v = unsafe_bool e'' in
      if v = true then Int 1 else Int 0
    | Int, Char ->
      let v = unsafe_char e'' in
      assert (BatUTF8.length v = 1);
      let c = BatUTF8.get v 0 in
      let c' = BatUChar.int_of c in
      Int (c')
    | Int, String ->
      let v = unsafe_string e'' in
      Int (int_of_string v)
    | Int, Float ->
      let v = unsafe_float e'' in
      Int (int_of_float v)
    | Float, String ->
      let v = unsafe_string e'' in
      Float (float_of_string v)
    | Float, Int ->
      let v = unsafe_int e'' in
      Float (float_of_int v)
    | String, Bool ->
      let v = unsafe_bool e'' in
      (match v with
      | true -> String "шын"
      | false -> String "жалған"
      )
    | String, Char ->
      let v = unsafe_char e'' in
      String (v)
    | String, Int ->
      let v = unsafe_int e'' in
      String (string_of_int v)
    | String, Float ->
      let v = unsafe_float e'' in
      String (string_of_float v)
    | _, _ -> impossible "evaluate_expr: CastTo")
  | IntAdd(a, b) ->
    let a' = unsafe_int @@ ev a in
    let b' = unsafe_int @@ ev b in
    Int(a' + b')
  | IntSub(a, b) ->
    let a' = unsafe_int @@ ev a in
    let b' = unsafe_int @@ ev b in
    Int(a' - b')
  | IntMul(a, b) ->
    let a' = unsafe_int @@ ev a in
    let b' = unsafe_int @@ ev b in
    Int(a' * b')
  | IntDiv(a, b) ->
    let a' = unsafe_int @@ ev a in
    let b' = unsafe_int @@ ev b in
    Int(a' / b')
  | IntMod(a, b) -> 
    let a' = unsafe_int @@ ev a in
    let b' = unsafe_int @@ ev b in
    Int(a' % b')
  | FloatAdd(a, b) ->
    let a' = unsafe_float @@ ev a in
    let b' = unsafe_float @@ ev b in
    Float(a' +. b')
  | FloatSub(a, b) ->
    let a' = unsafe_float @@ ev a in
    let b' = unsafe_float @@ ev b in
    Float(a' -. b')
  | FloatMul(a, b) ->
    let a' = unsafe_float @@ ev a in
    let b' = unsafe_float @@ ev b in
    Float(a' *. b')
  | FloatDiv(a, b) ->
    let a' = unsafe_float @@ ev a in
    let b' = unsafe_float @@ ev b in
    Float(a' /. b')
  | StringAdd(a, b) -> 
    let a' = unsafe_string @@ ev a in
    let b' = unsafe_string @@ ev b in
    String(a' ^ b')
  | Eq(a, b) -> 
    let wrap x = Bool x in
    let a' = ev a in
    let b' = ev b in
    (match a', b' with
    | Bool(a''), Bool(b'') -> wrap (a'' = b'')
    | Char(a''), Char(b'') -> wrap (a'' = b'')
    | Int(a''), Int(b'') -> wrap (a'' = b'')
    | Float(a''), Float(b'') -> wrap (a'' = b'')
    | String(a''), String(b'') -> wrap (a'' = b'')
    | Array(_, a''), Array(_, b'') ->
      List.combine a'' b''
      |> List.map (fun (a''', b''') -> ev (Eq(a''', b''')))
      |> List.fold_left (fun acc x -> let x' = unsafe_bool x in (acc && x')) true
      |> wrap
    | Struct(_, a''), Struct(_, b'') ->
      List.combine a'' b''
      |> List.map (fun ((_, a'''), (_, b''')) -> ev (Eq(a''', b''')))
      |> List.fold_left (fun acc x -> let x' = unsafe_bool x in (acc && x')) true
      |> wrap
    | _, _ -> failwith "Something went wrong when checking for equality!"
    )
  | Neq(a, b) -> 
    let v = unsafe_bool @@ ev (Eq(a, b)) in
    (match v with
    | true -> Bool false
    | false -> Bool true
    )
  | CharLt(a, b) ->
    let a' = unsafe_char @@ ev a in
    let b' = unsafe_char @@ ev b in
    Bool(a' < b')
  | CharLe(a, b) ->
    let a' = unsafe_char @@ ev a in
    let b' = unsafe_char @@ ev b in
    Bool(a' <= b')
  | CharGt(a, b) ->
    let a' = unsafe_char @@ ev a in
    let b' = unsafe_char @@ ev b in
    Bool(a' > b')
  | CharGe(a, b) ->
    let a' = unsafe_char @@ ev a in
    let b' = unsafe_char @@ ev b in
    Bool(a' >= b')
  | IntLt(a, b) ->
    let a' = unsafe_int @@ ev a in
    let b' = unsafe_int @@ ev b in
    Bool(a' < b')
  | IntLe(a, b) ->
    let a' = unsafe_int @@ ev a in
    let b' = unsafe_int @@ ev b in
    Bool(a' <= b')
  | IntGt(a, b) ->
    let a' = unsafe_int @@ ev a in
    let b' = unsafe_int @@ ev b in
    Bool(a' > b')
  | IntGe(a, b) ->
    let a' = unsafe_int @@ ev a in
    let b' = unsafe_int @@ ev b in
    Bool(a' >= b')
  | FloatLt(a, b) ->
    let a' = unsafe_float @@ ev a in
    let b' = unsafe_float @@ ev b in
    Bool(a' < b')
  | FloatLe(a, b) ->
    let a' = unsafe_float @@ ev a in
    let b' = unsafe_float @@ ev b in
    Bool(a' <= b')
  | FloatGt(a, b) ->
    let a' = unsafe_float @@ ev a in
    let b' = unsafe_float @@ ev b in
    Bool(a' > b')
  | FloatGe(a, b) ->
    let a' = unsafe_float @@ ev a in
    let b' = unsafe_float @@ ev b in
    Bool(a' >= b')
  | StringLt(a, b) ->
    let a' = unsafe_string @@ ev a in
    let b' = unsafe_string @@ ev b in
    Bool(a' < b')
  | StringLe(a, b) ->
    let a' = unsafe_string @@ ev a in
    let b' = unsafe_string @@ ev b in
    Bool(a' <= b')
  | StringGt(a, b) ->
    let a' = unsafe_string @@ ev a in
    let b' = unsafe_string @@ ev b in
    Bool(a' > b')
  | StringGe(a, b) ->
    let a' = unsafe_string @@ ev a in
    let b' = unsafe_string @@ ev b in
    Bool(a' >= b')
  | LAnd(a, b) -> 
    let a' = unsafe_bool @@ ev a in
    (match a' with
    | true -> ev b
    | false -> Bool(false)
    )
  | LOr(a, b) ->
    let a' = unsafe_bool @@ ev a in
    (match a' with
    | true -> Bool(true)
    | false -> ev b
    )

and execute_stmt funcs varstore s : ('a * (string * Typecheck.TypedAst.expr) list) =
  let open Typecheck.TypedAst in
  match s with
  | Nop -> failwith "Impossible case"
  | Expr (_, e) ->
    evaluate_expr funcs varstore e |> ignore;
    (funcs, varstore)
  | Assign(lval, rval) ->
    let replace_value_in_assoc name v l =
      let rec loop l =
        match l with
        | [] -> []
        | (name', x) :: rest ->
          if name = name' then (name', v) :: rest
          else (name', x) :: (loop rest)
      in
      loop l
    in
    let rec orig_value v =
      match v with
      | Ident(_, name) -> (name, List.assoc name varstore)
      | MemberAccess(_, v', _) -> orig_value v'
      | IndexAccess(_, v', _) -> orig_value v'
      | _ -> impossible "assign, orig_value"
    in
    let rec aug_value l r (v : Typecheck.TypedAst.expr) =
      match l with
      | Ident(_, _) -> (fun () -> v)
      | MemberAccess(_, l', member) ->
        let f = aug_value l r v in
        let members = unsafe_struct v in
        let res = (fun () ->
          let v = f () in
          Struct(Types.Unit, replace_value_in_assoc member v members) (* placeholder type *)
        ) in
        res
      | IndexAccess(_, a', index) ->
          let enum l =
            let id x = x in
            let n = List.length l in
            let idx = List.init n id in
            List.combine idx l
          in
          let f = aug_value l r v in
          let arr = unsafe_array a' in
          let res = (fun () ->
            let v = f () in
            let new_arr = enum arr in
            let i = unsafe_int index in
            let strip l = List.map snd l in
            Array(Types.Unit, strip @@ (replace_value_in_assoc i v new_arr))
          ) in
          res
      | _ -> impossible "aug_value"
    in
    let rec assign_to l r =
      let (name, v) = orig_value l in
      let f = aug_value l r v in
      let v' = f () in
      let new_varstore = replace_value_in_assoc name v' varstore in
      new_varstore
    in
    let rval' = evaluate_expr funcs varstore rval in
    let new_varstore = assign_to lval rval' in
    (funcs, new_varstore)
  | If(c, t, f) -> 
    let c' = c |> evaluate_expr funcs varstore |> unsafe_bool in
    (match c' with
    | true -> execute_stmt funcs varstore t
    | false -> execute_stmt funcs varstore f)
  | VarDef(_, name, v) -> 
    let v' = evaluate_expr funcs varstore v in
    let entry = (name, v') in
    let new_varstore = entry :: varstore in
    (funcs, new_varstore)
  | While(c, body) ->
    let rec loop2 funcs varstore s =
      let c' = c |> evaluate_expr funcs varstore |> unsafe_bool in
      if c' = true then
        let (new_funcs, new_varstore) = execute_stmt funcs varstore s in
        loop2 new_funcs new_varstore s
      else
        (varstore)
    in
    let new_varstore = loop2 funcs varstore body in
    (funcs, new_varstore)
  | For(init, cond, step, body) -> 
    let (new_funcs, new_varstore) = execute_stmt funcs varstore init in
    let rec loop2 funcs varstore s =
      let c' = cond |> evaluate_expr funcs varstore |> unsafe_bool in
      if c' = true then
        let (new_funcs, new_varstore) = execute_stmt funcs varstore s in
        let (new_funcs, new_varstore) = execute_stmt new_funcs new_varstore step in
        loop2 new_funcs new_varstore s
      else
        (varstore)
    in
    let new_varstore = loop2 funcs new_varstore body in
    (funcs, new_varstore)
  | Block(b) -> 
    let (_, new_funcs, new_varstore) = run_statement_list funcs varstore b in
    (new_funcs, new_varstore)
  | FuncDef(_, name, args, b) ->
    let new_funcs = (name, (args, b)) :: funcs in
    (new_funcs, varstore)
  | StructDef(_, _) -> (funcs, varstore)
  | EmptyReturn -> impossible "execute_stmt: empty return"
  | Return (_, v) -> impossible "execute_stmt: return"

and run_statement_list funcs varstore l : (Typecheck.TypedAst.expr * funcstore * varstoret) =
  let open Typecheck.TypedAst in
  let rec loop funcs varstore l : (Typecheck.TypedAst.expr * funcstore * varstoret) =
    match l with
    | [] -> (Unit, funcs, varstore)
    | EmptyReturn :: _ -> (Unit, funcs, varstore)
    | Return (_, v) :: _ -> ((evaluate_expr funcs varstore v), funcs, varstore)
    | x :: rest ->
      let (new_funcs, new_varstore) = execute_stmt funcs varstore x in
      run_statement_list new_funcs new_varstore rest
  in
  loop funcs varstore l

let interpret p =
  let open Typecheck.TypedAst in
  let varstore = [] in
  let funcs = [] in
  let (retval, _, _) = run_statement_list funcs varstore p in
  ignore retval

module Result = struct

type 'a res =
  | Ok of 'a
  | Err of string list

let bind f a =
  match a with
  | Ok v -> f v
  | Err l -> Err l

let ok x = Ok x
let err (x : string list) = Err x

let (>>=) a f = bind f a

end

module TypedAst = struct
  type expr =
    | Unit
    | Bool of bool
    | Char of string
    | Int of int
    | Float of float
    | String of string
    | Ident of Types.tt * string
    | FuncCall of Types.tt * string * (expr list)
    | IndexAccess of Types.tt * expr * expr
    | MemberAccess of Types.tt * expr * string
    | CastTo of Types.tt * expr
    | Array of Types.tt * expr list
    | Struct of Types.tt * (string * expr) list
    | IntAdd of expr * expr
    | IntSub of expr * expr
    | IntMul of expr * expr
    | IntDiv of expr * expr
    | IntMod of expr * expr
    | FloatAdd of expr * expr
    | FloatSub of expr * expr
    | FloatMul of expr * expr
    | FloatDiv of expr * expr
    | StringAdd of expr * expr
    | Eq of expr * expr
    | Neq of expr * expr
    | CharLt of expr * expr
    | CharLe of expr * expr
    | CharGt of expr * expr
    | CharGe of expr * expr
    | IntLt of expr * expr
    | IntLe of expr * expr
    | IntGt of expr * expr
    | IntGe of expr * expr
    | FloatLt of expr * expr
    | FloatLe of expr * expr
    | FloatGt of expr * expr
    | FloatGe of expr * expr
    | StringLt of expr * expr
    | StringLe of expr * expr
    | StringGt of expr * expr
    | StringGe of expr * expr
    | LAnd of expr * expr
    | LOr of expr * expr

  type stmt =
    | Nop
    | EmptyReturn
    | Return of Types.tt * expr
    | Expr of Types.tt * expr
    | Assign of expr * expr
    | If of expr * stmt * stmt
    | VarDef of Types.tt * string * expr
    | While of expr * stmt
    | For of stmt * expr * stmt * stmt
    | Block of stmt list
    | FuncDef of Types.tt * string * (string * Types.tt) list * (stmt list)
    | StructDef of string * (string * Types.tt) list

  let type_of (e : expr) : Types.tt =
    let open Types in
    match e with
    | Unit -> Unit
    | Bool _ -> Bool
    | Char _ -> Char
    | Int _ -> Int
    | Float _ -> Float
    | String _ -> String
    | Ident(t, _) -> t
    | FuncCall(t, _, _) -> t
    | IndexAccess(t, _, _) -> t
    | MemberAccess(t, _, _) -> t
    | CastTo(t, _) -> t
    | Array(t, _) -> t
    | Struct(t, _) -> t
    | IntAdd(_, _) -> Int
    | IntSub(_, _) -> Int
    | IntMul(_, _) -> Int
    | IntDiv(_, _) -> Int
    | IntMod(_, _) -> Int
    | FloatAdd(_, _) -> Float
    | FloatSub(_, _) -> Float
    | FloatMul(_, _) -> Float
    | FloatDiv(_, _) -> Float
    | StringAdd(_, _) -> String
    | Eq(_, _) -> Bool
    | Neq(_, _) -> Bool
    | CharLt(_, _) -> Bool
    | CharLe(_, _) -> Bool
    | CharGt(_, _) -> Bool
    | CharGe(_, _) -> Bool
    | IntLt(_, _) -> Bool
    | IntLe(_, _) -> Bool
    | IntGt(_, _) -> Bool
    | IntGe(_, _) -> Bool
    | FloatLt(_, _) -> Bool
    | FloatLe(_, _) -> Bool
    | FloatGt(_, _) -> Bool
    | FloatGe(_, _) -> Bool
    | StringLt(_, _) -> Bool
    | StringLe(_, _) -> Bool
    | StringGt(_, _) -> Bool
    | StringGe(_, _) -> Bool
    | LAnd(_, _) -> Bool
    | LOr(_, _) -> Bool
end

let rec traverse_list l =
  let open Result in
  match l with
  | [] -> Ok []
  | x :: rest ->
    let r = traverse_list rest in
    (match x, r with
    | Ok x', Ok r' -> ok (x' :: r')
    | Ok _, Err r' -> err r'
    | Err x', Ok _ -> err x'
    | Err x', Err r' -> err (x' @ r')
    )

let rec typecheck_expr funcs structstore ctx e =
  let open Types in
  let open Ast in
  let open TypedAst in
  let open Result in
  let err s = Err [s] in
  let ok v = Ok v in
  let (>>=) = Result.(>>=) in
  let check e = typecheck_expr funcs structstore ctx e in
  match (e : Ast.expr) with
  | Unit -> ok (Unit)
  | Bool b -> ok (Bool b)
  | Char c -> ok (Char c)
  | Int i -> ok (Int i)
  | Float f -> ok (Float f)
  | String s -> ok (String s)
  | Ident i -> 
    (match List.assoc_opt i ctx with
    | Some(t) -> ok (Ident(t, i))
    | None -> Err ([Printf.sprintf "The variable %s is not defined!" i])
    )
  | New name ->
    (match List.assoc_opt name structstore with
    | Some(members) -> ok (Struct(CustomType(name), List.map (fun (x, _) -> (x, TypedAst.Unit)) members))
    | None -> Err ([Printf.sprintf "Can't instantiate a new struct %s!" name])
    )
  | Array l -> 
    let typechecked_values = l |> List.map (check) |> traverse_list in
    (match typechecked_values with
    | Ok l' ->
      let s = 
        l'
        |> List.map (type_of)
        |> SetType.of_list
        |> SetType.to_seq
        |> List.of_seq
      in
      (match s with
      | [] -> (Ok(Array(Unknown, l')))
      | [t] ->(Ok(Array(t,       l')))
      | _ -> err "Can't guess the type of the array!")
    | Err errs -> Err errs)
  | Add(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Types.Int, Types.Int -> ok (IntAdd(a', b'))
    | Float, Float -> ok (FloatAdd(a', b'))
    | String, String -> ok (StringAdd(a', b'))
    | _, _ -> Err [Printf.sprintf 
        "The add operation is not supported for %s and %s"
        (Types.string_of t_a)
        (Types.string_of t_b)]
    )
  | Sub(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Int, Int -> ok (IntSub(a', b'))
    | Float, Float -> ok (FloatSub(a', b'))
    | _, _ -> Err [Printf.sprintf 
        "The sub operation is not supported for %s and %s"
        (Types.string_of t_a)
        (Types.string_of t_b)]
    )
  | Mul(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Int, Int -> ok (IntMul(a', b'))
    | Float, Float -> ok (FloatMul(a', b'))
    | _, _ -> Err [Printf.sprintf 
        "The mul operation is not supported for %s and %s"
        (Types.string_of t_a)
        (Types.string_of t_b)]
    )
  | Div(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Int, Int -> ok (IntDiv(a', b'))
    | Float, Float -> ok (FloatDiv(a', b'))
    | _, _ -> Err [Printf.sprintf 
        "The div operation is not supported for %s and %s"
        (Types.string_of t_a)
        (Types.string_of t_b)]
    )
  | Mod(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Int, Int -> ok (IntMod(a', b'))
    | _, _ -> Err [Printf.sprintf 
        "The add operation is not supported for %s and %s"
        (Types.string_of t_a)
        (Types.string_of t_b)]
    )
  | FuncCall(name, args) -> 
    (match List.assoc_opt name funcs with
    | Some(rettype, expected_arg_types) ->
      let typechecked_args = 
        args
        |> List.map (check)
        |> traverse_list
      in
      (match typechecked_args with
      | Err l -> Err l
      | Ok l ->
        let e_n = List.length expected_arg_types in
        let l_n = List.length l in
        if l_n = e_n then begin
          let types = List.map type_of l in
          let n = List.combine expected_arg_types types
          |> List.filter (fun ((_, a), b) -> a = b)
          |> List.length
          in
        if n = List.length expected_arg_types then begin
          ok (FuncCall(rettype, name, l))
        end else begin
          Err ([Printf.sprintf "Some type arguments to %s don't match!" name])
        end
        end else
        Err [Printf.sprintf "Expected %d arguments to %s, but instead got %d"
          e_n
          name
          l_n
        ]
      )
    | None -> Err [Printf.sprintf "The function %s is not defined!" name])
  | IndexAccess(e, v) -> 
    check e >>= fun e' ->
    check v >>= fun v' ->
    let t_e = type_of e' in
    let t_v = type_of v' in
    (match t_e, t_v with
    | Array(t'), Int -> Ok(IndexAccess(t', e', v'))
    | _, _ -> Err [Printf.sprintf "Tried to perform index access on the following types: %s, %s"
        (Types.string_of t_e)
        (Types.string_of t_v)
      ]
    )
  | MemberAccess(s, member) -> 
    check s >>= fun s' ->
    let t_s = type_of s' in
    (match t_s with
    | CustomType(structname) ->
      (match List.assoc_opt structname structstore with
      | Some(members) ->
        (match List.assoc_opt member members with
        | Some(t') -> Ok(MemberAccess(t', s', member))
        | None -> Err [Printf.sprintf "Member %s does not exist in %s!" member structname]
        )
      | None -> Err [Printf.sprintf "Struct %s is not defined!" structname]
      )
    | _ -> Err [Printf.sprintf "Can only do member access on structs, instead got: %s"
        (Types.string_of t_s)
      ]
    )
  | Eq(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    if t_a = t_b then
      ok (Eq(a', b'))
    else
      Err [
        Printf.sprintf 
          "Equality operator expects same types, however got this: %s and %s"
          (Types.string_of t_a)
          (Types.string_of t_b)
      ]
  | Neq(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    if t_a = t_b then
      ok (Neq(a', b'))
    else
      Err [
        Printf.sprintf 
          "Non-equality operator expects same types, however got this: %s and %s"
          (Types.string_of t_a)
          (Types.string_of t_b)
      ]
  | Lt(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Types.Char, Types.Char -> ok (CharLt(a', b'))
    | Types.Int, Types.Int -> ok (IntLt(a', b'))
    | Types.Float, Types.Float -> ok (FloatLt(a', b'))
    | _, _ -> Err [ Printf.sprintf 
      "Less-than operator doesn't support the following operands: %s and %s"
      (Types.string_of t_a)
      (Types.string_of t_b)])
  | Le(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Types.Char, Types.Char -> ok (CharLe(a', b'))
    | Types.Int, Types.Int -> ok (IntLe(a', b'))
    | Types.Float, Types.Float -> ok (FloatLe(a', b'))
    | _, _ -> Err [ Printf.sprintf 
      "Less-or-equall operator doesn't support the following operands: %s and %s"
      (Types.string_of t_a)
      (Types.string_of t_b)])
  | Gt(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Types.Char, Types.Char -> ok (CharGt(a', b'))
    | Types.Int, Types.Int -> ok (IntGt(a', b'))
    | Types.Float, Types.Float -> ok (FloatGt(a', b'))
    | _, _ -> Err [ Printf.sprintf 
      "Greater-than operator doesn't support the following operands: %s and %s"
      (Types.string_of t_a)
      (Types.string_of t_b)])
  | Ge(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Types.Char, Types.Char -> ok (CharGe(a', b'))
    | Types.Int, Types.Int -> ok (IntGe(a', b'))
    | Types.Float, Types.Float -> ok (FloatGe(a', b'))
    | _, _ -> Err [ Printf.sprintf 
      "Greater-or-equal operator doesn't support the following operands: %s and %s"
      (Types.string_of t_a)
      (Types.string_of t_b)])
  | LAnd(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Types.Bool, Types.Bool -> ok (LAnd(a', b'))
    | _, _ -> Err [ Printf.sprintf 
      "Logical AND expects booleans, however got this: %s and %s"
      (Types.string_of t_a)
      (Types.string_of t_b)])
  | LOr(a, b) ->
    check a >>= fun a' ->
    check b >>= fun b' ->
    let t_a = type_of a' in
    let t_b = type_of b' in
    (match t_a, t_b with
    | Types.Bool, Types.Bool -> ok (LOr(a', b'))
    | _, _ -> Err [ Printf.sprintf 
      "Logical OR expects booleans, however got this: %s and %s"
      (Types.string_of t_a)
      (Types.string_of t_b)])
  | CastTo(t, e) ->
    check e >>= fun e' ->
    let t_e = type_of e' in
    if t = t_e then ok (CastTo(t, e')) else
    match t, t_e with
    | Unit, _ -> Err [Printf.sprintf "Can't cast %s to Unit" (Types.string_of t_e)]
    | _, Unit -> Err [Printf.sprintf "Unit can't be casted to %s" (Types.string_of t)]
    | Bool, Char
    | Bool, Int
    | Bool, Float
    | Bool, String -> ok (CastTo(Bool, e'))
    | Char, Int -> ok (CastTo(Char, e'))
    | Int, Bool
    | Int, Char
    | Int, String
    | Int, Float -> ok (CastTo(Int, e'))
    | Float, String
    | Float, Int -> ok (CastTo(Float, e'))
    | String, Bool
    | String, Char
    | String, Int
    | String, Float -> ok (CastTo(String, e'))
    | _, _ -> Err [Printf.sprintf "Unsupported cast from %s to %s" (Types.string_of t_e) (Types.string_of t)]

let list_typechecking_problems e =
  List.iter (fun s -> print_endline s) e

let rec typecheck_stmt rettype funcs structstore (ctx: (string * Types.tt) list) s : (TypedAst.stmt * 'a * 'b * 'c) Result.res =
  let open Result in
  let open TypedAst in
  match s with
  | Ast.Nop -> Ok (Nop, funcs, structstore, ctx)
  | EmptyReturn -> 
    (match rettype with
    | Types.Unit -> Ok(EmptyReturn, funcs, structstore, ctx)
    | _ -> Err [Printf.sprintf "The return types don't match! Expected %s but got Unit"
        (Types.string_of rettype)
      ]
    )
  | Return e -> 
    typecheck_expr funcs structstore ctx e >>= fun e' ->
    let t_e = type_of e' in
    if rettype = t_e then
      (Ok (Return (rettype, e'), funcs, structstore, ctx))
    else
      (Err [Printf.sprintf "The return types don't match! Expected %s but got %s"
        (Types.string_of rettype)
        (Types.string_of t_e)
      ])
  | If(c, t, f) ->
    typecheck_expr funcs structstore ctx c >>= fun c' ->
    let t_c = type_of c' in
    if t_c = Bool then
      (
        typecheck_stmt rettype funcs  structstore  ctx  t >>= fun (t', funcs', structstore', ctx') ->
        typecheck_stmt rettype funcs' structstore' ctx' f >>= fun (f', funcs', structstore', ctx') ->
        Ok (If(c', t', f'), funcs', structstore', ctx')
      )
    else
      Err [Printf.sprintf "Expected Bool for if conditional, instead got %s" (Types.string_of t_c)]
  | While(c, s) ->
    typecheck_expr funcs structstore ctx c >>= fun c' ->
    let t_c = type_of c' in
    if t_c = Bool then
      (
        typecheck_stmt rettype funcs structstore ctx s >>= fun (s', funcs', structstore', ctx') ->
        Ok (While(c', s'), funcs', structstore', ctx')
      )
    else
      Err [Printf.sprintf "Expected Bool for while conditional, instead got %s" (Types.string_of t_c)]
  | For(init, cond, step, body) ->
    typecheck_stmt rettype funcs structstore ctx init >>= fun (init', funcs', structstore', ctx') ->
    typecheck_expr funcs' structstore' ctx' cond >>= fun c' ->
    let t_c = type_of c' in
    (match t_c with
    | Bool ->
      (
        typecheck_stmt rettype funcs' structstore' ctx' step >>= fun (step', funcs'', structstore'', ctx'') ->
        typecheck_stmt rettype funcs'' structstore'' ctx'' body >>= fun (body', _, _, _) ->
        Ok (For(init', c', step', body'), funcs, structstore, ctx)
      )
    | _ -> Err [Printf.sprintf "Expected Bool for 'for' conditional, instead got %s" (Types.string_of t_c)]
    )
  | FuncDef(rettype_f, name, args, body) ->
    let new_funcs = (name, (rettype_f, args)) :: funcs in
    let new_ctx = args @ ctx in
    let (t, _, _, _) = typecheck_stmt_list rettype_f new_funcs structstore new_ctx body in
    (match t with
    | Ok(body) -> Ok(FuncDef(rettype_f, name, args, body), new_funcs, structstore, ctx)
    | Err l -> Err l
    )
  | StructDef (name, members) ->
    (match List.assoc_opt name structstore with
    | Some(_) -> Err [Printf.sprintf "Struct %s has been already defined!" name]
    | None ->
      let module S = Set.Make(String) in
      let s = S.of_list (List.map fst members) in
      if S.cardinal s = List.length members then
      (
        let f t =
          match t with
          | Types.CustomType(name) ->
            (match List.assoc_opt name structstore with
            | Some(_) -> Ok(Types.CustomType(name))
            | None ->
              Err [Printf.sprintf "Type %s is not defined!" name]
            )
          | _ -> Ok(t)
        in
        let typechecked_members = members |> List.map snd |> List.map f |> traverse_list in
        (match typechecked_members with
        | Ok(tm) ->
          let new_structstore = (name, members) :: structstore in
          Ok(StructDef(name, members), funcs, new_structstore, ctx)
        | Err l -> Err l
        )
      )
      else
      (
        Err [Printf.sprintf "Some field names of %s are repeated!" name]
      )
    )
  | Ast.Expr e ->
    (match typecheck_expr funcs structstore ctx e with
    | Ok t -> Ok (Expr((type_of t), t), funcs, structstore, ctx)
    | Err l -> Err l
    )
  | VarDef(t, name, e) ->
    typecheck_expr funcs structstore ctx e >>= fun t' ->
    (match t with
    | Unknown -> 
      let new_ctx = (name, type_of t') :: ctx in
      Ok(VarDef(t, name, t'), funcs, structstore, new_ctx)
    | _ -> match t <> type_of t' with
      | true -> 
        Err [Printf.sprintf 
          "The types %s and %s are not equal" 
          (Types.string_of t) 
          (Types.string_of @@ type_of t')]
      | false ->
        let new_ctx = (name, type_of t') :: ctx in
        Ok((VarDef(t, name, t')), funcs, structstore, new_ctx)
    )
  | Assign(a, b) ->
    let rec is_lval x =
      let open Ast in
      match x with
      | Ident _ -> true
      | IndexAccess(e, _) -> is_lval e
      | MemberAccess(e, _) -> is_lval e
      | _ -> false
    in
    let b' = typecheck_expr funcs structstore ctx b in
    if (is_lval a) = false then
      match b' with
      | Ok (_) -> Err ["Trying to assign to a non-lvalue"]
      | Err(l) -> Err ("Trying to assign to a non-lvalue" :: l)
    else
      let a' = typecheck_expr funcs structstore ctx a in
      (match a', b' with
      | Ok(t), Ok(t') ->
          if t <> t' then
            Err [Printf.sprintf 
              "Trying to assign value of type %s to value of type %s" 
              (Types.string_of @@ type_of t')
              (Types.string_of @@ type_of t)]
          else
            Ok(Assign(t, t'), funcs, structstore, ctx)
      | Ok(_), (Err l) -> Err l
      | (Err l), Ok(_) -> Err l
      | (Err l'), Err(l'') -> Err (l' @ l'')
      )
  | Block l -> 
    let (t, _, _, _) = typecheck_stmt_list rettype funcs structstore ctx l in
    (match t with
    | Ok(code) -> Ok(Block(code), funcs, structstore, ctx)
    | Err(l')-> Err(l')
    )

and typecheck_func rettype funcs structstore ctx p =
  let (t, _, _, _) = typecheck_stmt_list rettype funcs structstore ctx p in
  t

and typecheck_stmt_list t funcs structstore ctx p =
  let open Result in
  let f (cur, funcs, structstore, ctx) x =
    let res = typecheck_stmt t funcs structstore ctx x in
    match cur, res with
    | Ok(code), Ok (v, new_funcs, new_structstore, new_ctx) -> 
        (Ok(code @ [v]), new_funcs, new_structstore, new_ctx)
    | Ok _, Err x -> (Err x, funcs, structstore, ctx)
    | Err l, Ok _ -> (Err l, funcs, structstore, ctx)
    | Err l, Err x -> (Err(l @ x), funcs, structstore, ctx)
  in
  List.fold_left f (Ok([]), funcs, structstore, ctx) p

let typecheck p =
  let ctx = [] in
  let funcs = [] in
  let structstore = [] in
  let (t, _, _, _) = typecheck_stmt_list (Types.Unit) funcs structstore ctx p in
  match t with
  | Ok v -> v
  | Err e -> list_typechecking_problems e; failwith "Failed due to typechecking errors"

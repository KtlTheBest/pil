type expr =
  | Unit
  | Bool of bool
  | Char of string
  | Int of int
  | Float of float
  | String of string
  | Ident of string
  | CastTo of Types.tt * expr
  | FuncCall of string * (expr list)
  | IndexAccess of expr * expr
  | MemberAccess of expr * string
  | New of string
  | Array of expr list
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Lt of expr * expr
  | Le of expr * expr
  | Gt of expr * expr
  | Ge of expr * expr
  | LAnd of expr * expr
  | LOr of expr * expr

type stmt =
  | Nop
  | EmptyReturn
  | Return of expr
  | Expr of expr
  | Assign of expr * expr
  | If of expr * stmt * stmt
  | VarDef of Types.tt * string * expr
  | While of expr * stmt
  | For of stmt * expr * stmt * stmt
  | Block of stmt list
  | FuncDef of Types.tt * string * (string * Types.tt) list * (stmt list)
  | StructDef of string * (string * Types.tt) list

let rec string_of_expr = function
  | Unit -> "Unit"
  | Bool(true) -> "Bool(true)"
  | Bool(false) -> "Bool(false)"
  | Char(s) -> Printf.sprintf "Char(%s)" s
  | Int(i) -> Printf.sprintf "Int(%d)" i
  | Float(f) -> Printf.sprintf "Float(%f)" f
  | String(s) -> Printf.sprintf "String(%s)" s
  | Ident(s) -> Printf.sprintf "Ident(%s)" s
  | CastTo(t, e) -> Printf.sprintf "CastTo(%s, %s)" (Types.string_of t) (string_of_expr e)
  | FuncCall(name, args) ->
    let args' = String.concat ", " @@ List.map string_of_expr args in
    Printf.sprintf "FuncCall(%s, (%s))" name args'
  | IndexAccess(a, i) -> Printf.sprintf "IndexAccess(%s, %s)" (string_of_expr a) (string_of_expr i)
  | MemberAccess(m, s) -> Printf.sprintf "MemberAccess(%s, %s)" (string_of_expr m) s
  | New(s) -> Printf.sprintf "New(%s)" s
  | Array(l) -> Printf.sprintf "[%s]" @@ String.concat ", " @@ List.map string_of_expr l
  | Add(a, b) -> Printf.sprintf "Add(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Sub(a, b) -> Printf.sprintf "Sub(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Mul(a, b) -> Printf.sprintf "Mul(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Div(a, b) -> Printf.sprintf "Div(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Mod(a, b) -> Printf.sprintf "Mod(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Eq(a, b) -> Printf.sprintf "Eq(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Neq(a, b) -> Printf.sprintf "Neq(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Lt(a, b) -> Printf.sprintf "Lt(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Le(a, b) -> Printf.sprintf "Le(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Gt(a, b) -> Printf.sprintf "Gt(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Ge(a, b) -> Printf.sprintf "Ge(%s, %s)" (string_of_expr a) (string_of_expr b)
  | LAnd(a, b) -> Printf.sprintf "LAnd(%s, %s)" (string_of_expr a) (string_of_expr b)
  | LOr(a, b) -> Printf.sprintf "LOr(%s, %s)" (string_of_expr a) (string_of_expr b)

let rec string_of_stmt = function
  | Nop -> "Nop"
  | EmptyReturn -> "EmptyReturn"
  | Return(e) -> Printf.sprintf "Return(%s)" (string_of_expr e)
  | Expr(e) -> Printf.sprintf "Expr(%s)" (string_of_expr e)
  | Assign(a, b) -> Printf.sprintf "Assign(%s, %s)" (string_of_expr a) (string_of_expr b)
  | If(c, t, f) -> 
    Printf.sprintf "If(%s, %s, %s)"
      (string_of_expr c)
      (string_of_stmt t)
      (string_of_stmt f)
  | VarDef(t, name, v) ->
    Printf.sprintf "VarDef(%s, %s, %s)"
      (Types.string_of t)
      name
      (string_of_expr v)
  | While(c, s) -> Printf.sprintf "While(%s, %s)" (string_of_expr c) (string_of_stmt s)
  | For(i, c, s, b) ->
    Printf.sprintf "For(%s, %s, %s, %s)"
      (string_of_stmt i)
      (string_of_expr c)
      (string_of_stmt s)
      (string_of_stmt b)
  | Block(l) -> Printf.sprintf "Block([%s])" @@ String.concat "; " @@ List.map string_of_stmt l
  | FuncDef(t, name, args, s) ->
    let args' =
      args
      |> List.map (fun (s, t) -> (s, Types.string_of t))
      |> List.map (fun (a, b) -> Printf.sprintf "(%s, %s)" a b)
      |> String.concat "; "
    in
    let s' = String.concat "; " @@ List.map string_of_stmt s in
    Printf.sprintf "FuncDef(%s, %s, [%s], [%s])"
      (Types.string_of t)
      name
      args'
      s'
  | StructDef(name, members) ->
    let members' =
      members
      |> List.map (fun (s, t) -> (s, Types.string_of t))
      |> List.map (fun (a, b) -> Printf.sprintf "(%s, %s)" a b)
      |> String.concat "; "
    in
    Printf.sprintf "StructDef(%s, [%s])"
      name
      members'

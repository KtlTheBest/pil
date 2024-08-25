type expr =
  | Unit
  | Bool of bool
  | Char of char
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

%token <bool> BOOL_LIT
%token <char> CHAR_LIT
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token <string> IDENT

%token BOOL_T
%token CHAR_T
%token INT_T
%token FLOAT_T
%token STRING_T

%token IF
%token THEN
%token ELIF
%token ELSE
%token ENDIF

%token FUNCTION_K
%token VARDEF_K
%token STRUCTDEF_K
%token NEW
%token RETURN
%token FOR

%token BEGIN
%token END
%token EOF
%token WS

%token ASSIGN

%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token L_AND
%token L_OR
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE
%token EXCLAIM

%token DOT
%token COMMA
%token LPAR
%token RPAR
%token LSQR
%token RSQR

%type <Ast.expr list> func_arg_list
%type <Ast.expr list> non_empty_func_arg_list
%type <(string * Types.tt) list> argdef
%type <(string * Types.tt)> arg
%type <Ast.stmt> toplevel_stmt
%type <Ast.stmt list> toplevel_stmt_list
%type <unit> ws
%type <Types.tt> vardef_type
%type <Types.tt> vardef_type_basic
%type <Ast.stmt list> stmt_list
%type <Ast.stmt> stmt
%type <Ast.stmt> else_branch
%type <Ast.expr> expr
%type <Ast.expr> expr_and
%type <Ast.expr> expr_eq
%type <Ast.expr> expr_add
%type <Ast.expr> expr_mul
%type <Ast.expr> expr_mem
%type <Ast.expr> arr_acc
%type <Ast.expr> lit_or_ident

%start <Ast.stmt list> prog
%%

prog:
  | EOF { [] }
  | s = toplevel_stmt_list EOF { s }
  ;

toplevel_stmt_list:
  | s = toplevel_stmt WS l = toplevel_stmt_list { s :: l }
  | s = toplevel_stmt { [s] }
  ;

stmt_list:
  | s = stmt WS l = stmt_list { s :: l }
  | s = toplevel_stmt { [s] }
  ;

ws:
  | { () }
  | WS ws { () }

toplevel_stmt:
  | ws s = stmt { s }
  | ws t = vardef_type ws i = IDENT ws LPAR args = argdef RPAR FUNCTION_K WS BEGIN body = stmt_list END 
  { Ast.FuncDef(t, i, args, body) }
  ;

stmt:
  | e = expr { Ast.Expr(e) }
  | a = expr ASSIGN b = expr { Ast.Assign(a, b) }
  | ws IF c = expr THEN ws t_l = stmt_list ws ENDIF { Ast.If(c, Ast.Block(t_l), (Ast.Block([]))) }
  | ws IF c = expr THEN ws t_l = stmt_list ws f = elif_chain { Ast.If(c, Ast.Block(t_l), f) }
  | RETURN { Ast.EmptyReturn }
  | e = expr RETURN { Ast.Return(e) }
  | t = vardef_type VARDEF_K i = IDENT ASSIGN v = expr { Ast.VarDef(t, i, v) }
  | t = vardef_type ws i = IDENT ws LPAR args = argdef RPAR WS BEGIN body = stmt_list END 
  { Ast.FuncDef(t, i, args, body) }
  | BEGIN WS sl = stmt_list END { Ast.Block(sl) }
  ;

elif_chain:
  | ws ELIF c = expr THEN ws t_l = stmt_list f = elif_chain { Ast.If(c, Ast.Block(t_l), f) }
  | ws ELSE ws f_l = stmt_list ENDIF { Ast.Block(f_l) }
  | ws ENDIF { Ast.Block([]) }
  ;

argdef:
  | { [] }
  | l = nonempty_argdef { l }
  ;

nonempty_argdef:
  | x = arg { [ x ] }
  | x = arg COMMA rest = nonempty_argdef { x :: rest }
  ;

arg:
  | t = vardef_type i = IDENT { (i, t) }
  ;

vardef_type:
  | LSQR RSQR t = vardef_type { Types.Array(t) }
  | t = vardef_type_basic { t }
  ;

vardef_type_basic:
  | BOOL_T { Types.Bool }
  | CHAR_T { Types.Char }
  | INT_T { Types.Int }
  | FLOAT_T { Types.Float }
  | STRING_T { Types.String }
  | i = IDENT { Types.CustomType(i) }
  ;

else_branch:
  | ELIF c = expr THEN t = stmt f = else_branch { Ast.If(c, t, f) }
  | ELSE f = stmt { f }
  | { Nop }
  ;

expr:
  | a = expr L_OR b = expr_and { Ast.LOr(a, b) }
  | e = expr_and { e }
  ;

expr_and:
  | a = expr_and L_AND b = expr_eq { Ast.LAnd(a, b) }
  | e = expr_eq { e }
  ;

expr_eq:
  | a = expr_add EQ b = expr_add { Ast.Eq(a, b) }
  | a = expr_add NEQ b = expr_add { Ast.Neq(a, b) }
  | a = expr_add LT b = expr_add { Ast.Lt(a, b) }
  | a = expr_add LE b = expr_add { Ast.Le(a, b) }
  | a = expr_add GT b = expr_add { Ast.Gt(a, b) }
  | a = expr_add GE b = expr_add { Ast.Ge(a, b) }
  | e = expr_add { e }
  ;

expr_add:
  | a = expr_add ADD b = expr_mul { Ast.Add(a, b) }
  | a = expr_add SUB b = expr_mul { Ast.Sub(a, b) }
  | e = expr_mul { e }
  ;

expr_mul:
  | a = expr_mul MUL b = expr_mem { Ast.Mul(a, b) }
  | a = expr_mul DIV b = expr_mem { Ast.Div(a, b) }
  | a = expr_mul MOD b = expr_mem { Ast.Mod(a, b) }
  | e = expr_mem { e }
  ;

expr_mem:
  | s = expr_mem DOT m = IDENT { Ast.MemberAccess(s, m) }
  | e = arr_acc { e }
  ;

arr_acc:
  | a = arr_acc LSQR e = expr RSQR { Ast.IndexAccess(a, e) }
  | e = lit_or_ident { e }
  ;

lit_or_ident:
  | b = BOOL_LIT { Ast.Bool b }
  | c = CHAR_LIT { Ast.Char c }
  | i = INT_LIT { Ast.Int i }
  | f = FLOAT_LIT { Ast.Float f }
  | s = STRING_LIT { Ast.String s }
  | i = IDENT { Ast.Ident i }
  | LPAR args = func_arg_list RPAR i = IDENT { Ast.FuncCall(i, args) }
  | LPAR e = expr RPAR { e }
  ;

func_arg_list:
  | { [] }
  | l = non_empty_func_arg_list { l }
  ;

non_empty_func_arg_list:
  | e = expr { [e] }
  | e = expr COMMA rest = non_empty_func_arg_list { e :: rest }
  ;

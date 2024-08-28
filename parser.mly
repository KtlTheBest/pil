%token <bool> BOOL_LIT
%token <string> CHAR_LIT
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
%token FROM
%token TO
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

%type <(string * Types.tt) list> member_list
%type <(string * Types.tt) list> nonempty_argdef
%type <Ast.stmt> elif_chain
%type <Ast.expr list> non_empty_func_arg_list
%type <(string * Types.tt) list> argdef
%type <(string * Types.tt)> arg
%type <unit> ws
%type <Types.tt> vardef_type
%type <Types.tt> vardef_type_basic
%type <Ast.stmt list> stmt_list
%type <Ast.stmt> stmt
%type <Ast.expr> expr
%type <Ast.expr> expr_and
%type <Ast.expr> expr_eq
%type <Ast.expr> expr_add
%type <Ast.expr> expr_mul
%type <Ast.expr> expr_mem
%type <Ast.expr> arr_acc
%type <Ast.expr> lit_or_ident
%type <Ast.expr> funccall_or_lit_ident

%start <Ast.stmt list> prog
%%

prog:
  | ws EOF { [] }
  | ws s = stmt_list ws EOF { s }
  ;

stmt_list:
  | l = stmt_list WS s = stmt { l @ [s] }
  | s = stmt { [s] }
  ;

ws:
  | { () }
  | WS { () }

stmt:
  | ws e = expr { Ast.Expr(e) }
  | ws a = expr ASSIGN b = expr { Ast.Assign(a, b) }
  | ws IF c = expr THEN ws t_l = stmt_list f = elif_chain { Ast.If(c, Ast.Block(t_l), f) }
  | ws RETURN { Ast.EmptyReturn }
  | ws e = expr RETURN { Ast.Return(e) }
  | ws                 VARDEF_K i = IDENT ASSIGN v = expr { Ast.VarDef(Types.Unknown, i, v) }
  | ws t = vardef_type VARDEF_K i = IDENT ASSIGN v = expr { Ast.VarDef(t, i, v) }
  | ws                 VARDEF_K i = IDENT ASSIGN v = expr FROM e = expr TO FOR ws BEGIN ws body = stmt_list ws END 
  { Ast.For(Ast.VarDef(Types.Unknown, i, v), Ast.Lt(Ast.Ident(i), e), Ast.Assign(Ast.Ident(i), Ast.Add(Ast.Ident(i), Ast.Int(1))), Ast.Block(body)) }
  | BEGIN ws sl = stmt_list ws END { Ast.Block(sl) }
  | ws t = vardef_type i = IDENT LPAR args = argdef RPAR FUNCTION_K ws BEGIN ws body = stmt_list ws END 
  { Ast.FuncDef(t, i, args, body) }
  | ws i = IDENT LPAR args = argdef RPAR FUNCTION_K ws BEGIN ws body = stmt_list ws END
  { Ast.FuncDef(Types.Unit, i, args, body) }
  | ws i = IDENT STRUCTDEF_K ws BEGIN ws END { Ast.StructDef(i, []) }
  | ws i = IDENT STRUCTDEF_K ws BEGIN ws l = member_list ws END { Ast.StructDef(i, l) }
  ;

member_list:
  | l = member_list ws x = arg { l @ [x] }
  | x = arg { [ x ] }
  ;

elif_chain:
  | ws ENDIF { Ast.Block([]) }
  | ws ELSE ws f_l = stmt_list ws ENDIF { Ast.Block(f_l) }
  | ws ELIF c = expr THEN ws t_l = stmt_list f = elif_chain { Ast.If(c, Ast.Block(t_l), f) }
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
  | t = vardef_type LSQR RSQR { Types.Array(t) }
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

expr:
  | NEW i = IDENT { Ast.New(i) }
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
  | LSQR e = expr RSQR a = arr_acc { Ast.IndexAccess(a, e) }
  | e = funccall_or_lit_ident { e }
  ;

funccall_or_lit_ident:
  | LPAR RPAR i = IDENT { Ast.FuncCall(i, []) }
  | LPAR e = expr RPAR i = IDENT { Ast.FuncCall(i, [e]) }
  | LPAR args = non_empty_func_arg_list RPAR i = IDENT { Ast.FuncCall(i, args) }
  | LPAR e = expr RPAR { e }
  | e = lit_or_ident { e }
  ;

lit_or_ident:
  | LSQR RSQR { Ast.Array( [] ) }
  | LSQR l = non_empty_func_arg_list RSQR { Ast.Array( l ) }
  | b = BOOL_LIT { Ast.Bool b }
  | c = CHAR_LIT { Ast.Char c }
  | i = INT_LIT { Ast.Int i }
  | f = FLOAT_LIT { Ast.Float f }
  | s = STRING_LIT { Ast.String s }
  | i = IDENT { Ast.Ident i }
  ;

non_empty_func_arg_list:
  | e = expr { [e] }
  | e = expr COMMA rest = non_empty_func_arg_list { e :: rest }
  ;

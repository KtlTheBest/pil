open Angstrom
open Parser

let add = char '+' *> return ADD
let sub = char '-' *> return SUB
let mul = char '*' *> return MUL
let div = char '/' *> return DIV
let _mod = char '%' *> return MOD
let lsqr = char '[' *> return LSQR
let rsqr = char ']' *> return RSQR
let lpar = char '(' *> return LPAR
let rpar = char ')' *> return RPAR
let dot = char '.' *> return DOT
let comma = char ',' *> return COMMA

let _new = string "айнымалы" *> return NEW
let _true = string "шын" *> return (BOOL_LIT(true))
let _false = string "жалған" *> return (BOOL_LIT(false))
let _struct = string "құрылымы" *> return STRUCTDEF_K
let _function = string "функциясы" *> return FUNCTION_K
let _return = string "қайтар" *> return RETURN
let _begin = string "басы" *> return BEGIN
let _end = string "аяғы" *> return END
let _if = string "егер" *> return IF
let _then = string "болса" *> return THEN
let _elif = string "немесе" *> return ELIF
let _else = string "әйтпесе" *> return ELSE
let _endif = string "істе" *> return ENDIF
let _for = string "үшін" *> return FOR
let _eq = string "тең" *> return EQ
let _neq = string "теңЕмес" *> return NEQ
let _lt = string "кішірек" *> return LT
let _le = string "кішіНеТең" *> return LE
let _gt = string "көбірек" *> return GT
let _ge = string "көпНеТең" *> return GE
let _bool_t = string "логикалық" *> return BOOL_T
let _char_t = string "әріп" *> return CHAR_T
let _int_t = string "сан" *> return INT_T
let _string_t = string "сөз" *> return STRING_T

let reduce f default l =
  match l with
  | [] -> default
  | x :: rest -> List.fold_left f x rest

let reduce_unsafe f l =
  match l with
  | [] -> failwith "reduce_unsafe failed!"
  | x :: rest -> List.fold_left f x rest

let combine l =
  reduce (<|>) (return EOF) l

let whitespace = 
  char ' ' <|> char '\n' <|> char '\t'

let white =
  whitespace *> many whitespace *> return WS


let is_digit = function '0' .. '9' -> true | _ -> false
let digits = take_while1 is_digit

let special_chars =
  combine
  [ add
  ; sub
  ; mul
  ; div
  ; _mod
  ; lsqr
  ; rsqr
  ; lpar
  ; rpar
  ; dot
  ; comma
  ]

let keyword =
  combine
  [ _new
  ; _true
  ; _false
  ; _struct
  ; _function
  ; _return
  ; _begin
  ; _end
  ; _if
  ; _then
  ; _elif
  ; _else
  ; _endif
  ; _for
  ; _eq
  ; _neq
  ; _lt
  ; _le
  ; _gt
  ; _ge
  ; _bool_t
  ; _char_t
  ; _int_t
  ; _string_t
  ]

let lowercase_kazakh_char = 
  let l = ["а";"ә";"б";"в";"г";"ғ";"д";"е";"ё";"ж";"з";"и";"й";"к";"қ";"л";"м";"н";"ң";"о";"ө";"п";"р";"с";"т";"у";"ұ";"ү";"ф";"х";"һ";"ц";"ч";"ш";"щ";"ъ";"ы";"і";"ь";"э";"ю";"я"] in
  l |> List.map string |> (reduce (<|>) (return ""))

let uppercase_kazakh_char = 
  let l = ["А";"Ә";"Б";"В";"Г";"Ғ";"Д";"Е";"Ё";"Ж";"З";"И";"Й";"К";"Қ";"Л";"М";"Н";"Ң";"О";"Ө";"П";"Р";"С";"Т";"У";"Ұ";"Ү";"Ф";"Х";"Һ";"Ц";"Ч";"Ш";"Щ";"Ъ";"Ы";"І";"Ь";"Э";"Ю";"Я"] in
  l |> List.map string |> (reduce (<|>) (return ""))

let kazakh_char = lowercase_kazakh_char <|> uppercase_kazakh_char

let digit = (['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'] |> List.map char |> reduce_unsafe (<|>))
let c_to_s p = p >>| (fun c -> String.init 1 (fun _ -> c))

let ident =
  let start = kazakh_char in
  let ident_char = kazakh_char <|> c_to_s digit <|> c_to_s (char '_') in
  (both start (many ident_char)) >>| (fun (s, rest) -> IDENT (List.fold_left (^) "" (s :: rest)))

let escaped_char = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let valid_str_char =
  (char '\\' *> any_char >>| escaped_char) <|> not_char '"'
  
let valid_chr_char =
  (char '\\' *> any_char >>| escaped_char) <|> not_char '\''

let str =
  char '"' *> (many valid_str_char) <* char '"' >>| (fun x -> STRING_LIT (x |> List.map (fun c -> String.init 1 (fun _ -> c)) |> List.fold_left (^) ""))

let chr =
  char '\'' *> (valid_chr_char) <* char '\'' >>| (fun x -> CHAR_LIT x)

let token =
  white <|> special_chars <|> keyword <|> str <|> chr <|> ident

let tokenize_p = many token

let lexer s = 
  let cur_s = ref s in
  fun () ->
    match parse_string ~consume:Prefix token !cur_s with
    | Ok(t) -> (t, Lexing.dummy_pos, Lexing.dummy_pos)
    | _ -> failwith "Something went wrong when lexing!"

let tokenize s = parse_string ~consume:All token s

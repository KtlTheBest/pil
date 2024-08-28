{
open Lexing
open Parser

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let debug s =
  print_endline @@ Printf.sprintf "Currently Read: '%s'" (Lexing.lexeme s)
}

let digit = ['0'-'9']
let int = digit digit*

let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

let whitespace = ['\n' '\t']
let white = whitespace whitespace*
let comment = "//" [^ '\n' ]* '\n'

(*let lowercase_kazakh_char = ['а' 'ә' 'б' 'в' 'г' 'ғ' 'д' 'е' 'ё' 'ж' 'з' 'и' 'й' 'к' 'қ' 'л' 'м' 'н' 'ң' 'о' 'ө' 'п' 'р' 'с' 'т' 'у' 'ұ' 'ү' 'ф' 'х' 'һ' 'ц' 'ч' 'ш' 'щ' 'ъ' 'ы' 'і' 'ь' 'э' 'ю' 'я']
let uppercase_kazakh_char = ['А' 'Ә' 'Б' 'В' 'Г' 'Ғ' 'Д' 'Е' 'Ё' 'Ж' 'З' 'И' 'Й' 'К' 'Қ' 'Л' 'М' 'Н' 'Ң' 'О' 'Ө' 'П' 'Р' 'С' 'Т' 'У' 'Ұ' 'Ү' 'Ф' 'Х' 'Һ' 'Ц' 'Ч' 'Ш' 'Щ' 'Ъ' 'Ы' 'І' 'Ь' 'Э' 'Ю' 'Я']*)

(*
let lowercase_kazakh_char =
  [ '\u0430'-'\u044F' (* cyrillic *)
    '\u04D9'          (* ә *)
    '\u0493'          (* ғ *)
    '\u049B'          (* қ *)
    '\u04A3'          (* ң *)
    '\u04E9'          (* ө *)
    '\u04B1'          (* ұ *)
    '\u04AF'          (* ү *)
    '\u04BB'          (* һ *)
    '\u0456'          (* і *)
  ]

let uppercase_kazakh_char =
  [ '\u0410'-'\u042F' (* cyrillic *)
    '\u04D8'          (* Ә *)
    '\u0492'          (* Ғ *)
    '\u049A'          (* Қ *)
    '\u04A2'          (* Ң *)
    '\u04E8'          (* Ө *)
    '\u04B0'          (* Ұ *)
    '\u04AF'          (* Ү *)
    '\u04BA'          (* Һ *)
    '\u0406'          (* І *)
  ]
let kazakh_char = lowercase_kazakh_char | uppercase_kazakh_char

let identstart =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'] | kazakh_char
let identbody =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9'] | kazakh_char
let ident = identstart identbody*
*)

let lowercase_kazakh_char = '\xd0' '\xb0' | '\xd3' '\x99' | '\xd0' '\xb1' | '\xd0' '\xb2' | '\xd0' '\xb3' | '\xd2' '\x93' | '\xd0' '\xb4' | '\xd0' '\xb5' | '\xd1' '\x91' | '\xd0' '\xb6' | '\xd0' '\xb7' | '\xd0' '\xb8' | '\xd0' '\xb9' | '\xd0' '\xba' | '\xd2' '\x9b' | '\xd0' '\xbb' | '\xd0' '\xbc' | '\xd0' '\xbd' | '\xd2' '\xa3' | '\xd0' '\xbe' | '\xd3' '\xa9' | '\xd0' '\xbf' | '\xd1' '\x80' | '\xd1' '\x81' | '\xd1' '\x82' | '\xd1' '\x83' | '\xd2' '\xb1' | '\xd2' '\xaf' | '\xd1' '\x84' | '\xd1' '\x85' | '\xd2' '\xbb' | '\xd1' '\x86' | '\xd1' '\x87' | '\xd1' '\x88' | '\xd1' '\x89' | '\xd1' '\x8a' | '\xd1' '\x8b' | '\xd1' '\x96' | '\xd1' '\x8c' | '\xd1' '\x8d' | '\xd1' '\x8e' | '\xd1' '\x8f'

let uppercase_kazakh_char = '\xd0' '\x90' | '\xd3' '\x98' | '\xd0' '\x91' | '\xd0' '\x92' | '\xd0' '\x93' | '\xd2' '\x92' | '\xd0' '\x94' | '\xd0' '\x95' | '\xd0' '\x81' | '\xd0' '\x96' | '\xd0' '\x97' | '\xd0' '\x98' | '\xd0' '\x99' | '\xd0' '\x9a' | '\xd2' '\x9a' | '\xd0' '\x9b' | '\xd0' '\x9c' | '\xd0' '\x9d' | '\xd2' '\xa2' | '\xd0' '\x9e' | '\xd3' '\xa8' | '\xd0' '\x9f' | '\xd0' '\xa0' | '\xd0' '\xa1' | '\xd0' '\xa2' | '\xd0' '\xa3' | '\xd2' '\xb0' | '\xd2' '\xae' | '\xd0' '\xa4' | '\xd0' '\xa5' | '\xd2' '\xba' | '\xd0' '\xa6' | '\xd0' '\xa7' | '\xd0' '\xa8' | '\xd0' '\xa9' | '\xd0' '\xaa' | '\xd0' '\xab' | '\xd0' '\x86' | '\xd0' '\xac' | '\xd0' '\xad' | '\xd0' '\xae' | '\xd0' '\xaf'
let kazakh_char = lowercase_kazakh_char | uppercase_kazakh_char

let identstart =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'] | kazakh_char
let identbody =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] | kazakh_char
let ident = identstart identbody*

rule read =
  parse
  | comment { debug lexbuf; read lexbuf }
  | "жаңа" { debug lexbuf; NEW }
  | "бастап" { debug lexbuf; FROM }
  | "дейін" { debug lexbuf; TO }
  | "айнымалы" { debug lexbuf; VARDEF_K }
  | "шын" { debug lexbuf; BOOL_LIT(true) }
  | "жалған" { debug lexbuf; BOOL_LIT(false) }
  | "айнымалы" { debug lexbuf; VARDEF_K }
  | "құрылымы" { debug lexbuf; STRUCTDEF_K }
  | "функциясы" { debug lexbuf; FUNCTION_K }
  | "қайтар" { debug lexbuf; RETURN }
  | "басы" { debug lexbuf; BEGIN }
  | "аяғы" { debug lexbuf; END }
  | "егер" { debug lexbuf; IF }
  | "болса" { debug lexbuf; THEN }
  | "немесе" { debug lexbuf; ELIF }
  | "әйтпесе" { debug lexbuf; ELSE }
  | "істе" { debug lexbuf; ENDIF }
  | "үшін" { debug lexbuf; FOR }
  | "тең" { debug lexbuf; EQ }
  | "теңЕмес" { debug lexbuf; NEQ }
  | "кішірек" { debug lexbuf; LT }
  | "кішіНеТең" { debug lexbuf; LE }
  | "көбірек" { debug lexbuf; GT }
  | "көпНеТең" { debug lexbuf; GE }
  | "логикалық" { debug lexbuf; BOOL_T }
  | "әріп" { debug lexbuf; CHAR_T }
  | "сан" { debug lexbuf; INT_T }
  | "сөз" { debug lexbuf; STRING_T }
  | "=" { debug lexbuf; ASSIGN }
  | white { debug lexbuf; WS }
  | '+' { debug lexbuf; ADD }
  | '-' { debug lexbuf; SUB }
  | '*' { debug lexbuf; MUL }
  | '/' { debug lexbuf; DIV }
  | '%' { debug lexbuf; MOD }
  | '[' { debug lexbuf; LSQR }
  | ']' { debug lexbuf; RSQR }
  | '(' { debug lexbuf; LPAR }
  | ')' { debug lexbuf; RPAR }
  | '.' { debug lexbuf; DOT }
  | ',' { debug lexbuf; COMMA }
  | '"' { string (Buffer.create 0) lexbuf }
  | '\'' { char lexbuf }
  | ' ' { read lexbuf }
  | int { debug lexbuf; INT_LIT(int_of_string (Lexing.lexeme lexbuf)) }
  | ident { debug lexbuf; IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }

and string buf = parse
  | '"' { STRING_LIT (Buffer.contents buf) }
  | '\\' (backslash_escapes as c)
  { Buffer.add_char buf (char_for_backslash c);
    string buf lexbuf }
  | _ as c 
  { Buffer.add_char buf c;
    string buf lexbuf }

and char = parse
  | '\\' (backslash_escapes as c) '\''
  { CHAR_LIT ( String.init 1 (fun _ -> char_for_backslash c) ) }
  | (identbody as c) '\'' { CHAR_LIT c }

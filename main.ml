let program1 = "123"
let program2 =
  In_channel.with_open_text "мысал.тіл" In_channel.input_all

let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let compile_with_error lexbuf =
  (*
  let revised_lexer = Anglexer.lexer s in
  let revised_parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.prog
  in
  revised_parser revised_lexer
  *)
  try
    let v = Parser.prog Lexer.read lexbuf in
    print_endline "parsed input!";
    v
  with
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax_error" print_position lexbuf;
    []

let lexbuf_of_str s =
  let lexbuf = Lexing.from_string s in
  lexbuf

let compile p =
  p
  |> lexbuf_of_str
  |> compile_with_error
  |> Typecheck.typecheck
  |> Interpreter.interpret

let _ = compile program2

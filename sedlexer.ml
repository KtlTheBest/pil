open Parser

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let whitespace = [%sedlex.regexp? ' ' | '\n' | '\t']
let white = [%sedlex.regexp whitespace Star whitespace]

let rec main buf =
  match%sedlex buf with
  | eof -> EOF

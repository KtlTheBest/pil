ocamlopt -c types.ml
ocamlopt -c ast.ml
ocamlopt -c typecheck.ml
ocamlfind ocamlopt -c -package batteries interpreter.ml
menhir parser.mly
ocamlopt -c parser.ml
ocamlopt -c parser.mli
ocamllex lexer.mll -ml
ocamlfind ocamlopt -c -package batteries lexer.ml
ocamlopt -c main.ml
ocamlfind ocamlopt -o main.native -linkpkg -package batteries -package threads \
  types.cmx \
  ast.cmx \
  typecheck.cmx \
  interpreter.cmx \
  parser.cmx \
  lexer.cmx \
  main.cmx

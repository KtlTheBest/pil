OCB_FLAGS = -use-menhir -use-ocamlfind -lexflag -ml -package batteries
OCB = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

sanity:
	which menhir

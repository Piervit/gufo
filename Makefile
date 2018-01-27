all:
	ocamlbuild -tag debug -cflag -g -ocamlc "ocamlc -g" -ocamlopt "ocamlopt -g" -use-menhir -menhir "menhir --dump --explain" -tag thread -use-ocamlfind -tag annot -dont-catch-errors -package lambda-term -lib str  guforun.native 

clean:
	rm -rf _build

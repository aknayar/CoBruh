.PHONY: all
all: ast.ml scanner.mll parser.mly
	ocamlc -c ast.ml
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -o ast.cmo scanner.cmo parser.cmo

.PHONY: clean
clean:
	rm *.cmi *.cmo *.mli scanner.ml parser.ml
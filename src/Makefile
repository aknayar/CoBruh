.PHONY: all
all: ast.ml scanner.mll parser.mly test.ml
	ocamlc -c ast.ml
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c test.ml
	ocamlc -o test ast.cmo scanner.cmo parser.cmo test.cmo

.PHONY: clean
clean:
	rm *.cmi *.cmo *.mli scanner.ml parser.ml test
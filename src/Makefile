.PHONY: all
all: allparser allsemant

.PHONY: allsemant
allsemant: sast.ml semant.ml
	ocamlc -c sast.ml

.PHONY: allparser
allparser: ast.ml scanner.mll parser.mly
	ocamlc -c ast.ml
	ocamllex scanner.mll
	ocamlyacc -v parser.mly
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml

.PHONY: parsertest
parsertest: allparser parsertest.ml
	ocamlc -c parsertest.ml
	ocamlc -o parsertest ast.cmo scanner.cmo parser.cmo parsertest.cmo
	./parsertest

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.mli *.output test_src/*.txt scanner.ml parser.ml
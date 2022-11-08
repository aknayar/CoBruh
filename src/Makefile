.PHONY: all
all: allparser allsemant

.PHONY: allsemant
allsemant: sast.ml semant.ml
	ocamlc -c sast.ml
	ocamlc -c semant.ml

.PHONY: semanttest
semanttest: allparser allsemant semanttest.ml
	ocamlc -c semanttest.ml
	ocamlc -o semanttest scanner.cmo parser.cmo semant.cmo semanttest.cmo
	./semanttest < ./test_src/semantinput.bruh > ./test_src/semantoutput.txt

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
	./parsertest < ./test_src/parserinput.bruh > ./test_src/parseroutput.txt
	
.PHONY: whitespacetest
whitespacetest: allparser parsertest.ml
	ocamlc -c parsertest.ml
	ocamlc -o parsertest ast.cmo scanner.cmo parser.cmo parsertest.cmo
	./parsertest < ./test_src/sigwhitespaceinput.bruh > ./test_src/sigwhitespaceoutput.txt

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.mli *.output test_src/*.txt scanner.ml parser.ml parsertest semanttest
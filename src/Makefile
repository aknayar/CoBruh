.PHONY: all
all: parser semant

.PHONY: semant
semant: sast.ml semant.ml
	ocamlc -c sast.ml
	ocamlc -c semant.ml

.PHONY: semanttest
semanttest: parser semant semanttest.ml
	ocamlc -c semanttest.ml
	ocamlc -o semanttest scanner.cmo parser.cmo semant.cmo semanttest.cmo
	./semanttest < ./test_src/semantinput.bruh > ./test_src/semantoutput.txt

.PHONY: parser
parser: ast.ml scanner.mll parser.mly
	ocamlc -c ast.ml
	ocamllex scanner.mll
	ocamlyacc -v parser.mly
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml

.PHONY: parsertest
parsertest: parser parsertest.ml
	ocamlc -c parsertest.ml
	ocamlc -o parsertest ast.cmo scanner.cmo parser.cmo parsertest.cmo
	./parsertest < ./test_src/parserinput.bruh > ./test_src/parseroutput.txt
	
.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.mli *.output test_src/*.txt scanner.ml parser.ml parsertest semanttest
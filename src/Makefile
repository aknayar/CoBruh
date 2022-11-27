.PHONY: all
all:
	ocamlc -c ast.ml
	ocamlc -c sast.ml
	ocamlc -c semant.ml

.PHONY: semant
semant:
	ocamlbuild semanttest.native
	./semanttest.native < ./test_src/semantinput.bruh > ./test_src/semantoutput.txt

.PHONY: parser
parser:
	ocamlbuild parsertest.native
	./parsertest.native < ./test_src/parserinput.bruh > ./test_src/parseroutput.txt
	
.PHONY: clean
clean:
	rm -rf *.cmi *.cmo _build/ *.native test_src/*.txt

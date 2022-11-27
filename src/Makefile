.PHONY: all
all: semant

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
	rm -rf _build/ *.native test_src/*.txt

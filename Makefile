MAININPUT=test/input.bruh

.PHONY: all
all:
	dune build

.PHONY: parse
parse:
	@dune exec -- CoBruh -a $(MAININPUT)

.PHONY: semant
semant:
	@dune exec -- CoBruh -s $(MAININPUT)

.PHONY: test
test:
	dune test

.PHONY: clean
clean:
	rm -rf _build/
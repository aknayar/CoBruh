.PHONY: all
all:
	dune build

.PHONY: test
test:
	dune test
	@echo "Output in _build/default/test"

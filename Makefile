build:
	ocamlbuild -use-ocamlfind src/main.native

run:
		./main.native

.PHONY: build

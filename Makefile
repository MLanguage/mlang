SOURCE_DIR=calculette-impots-m-source-code/sources-latin1/sourcesm2015m_4_6

build:
	ocamlbuild -use-ocamlfind src/main.native

test: build
		./main.native --debug test.m

parse_all: build
		./main.native $(wildcard $(SOURCE_DIR)/*.m)

.PHONY: build

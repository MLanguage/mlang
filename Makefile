build:
	ocamlbuild -use-ocamlfind src/main.native

run: build
		./main.native -f calculette-impots-m-source-code/sources-latin1/sourcesm2015m_4_6/chap-3.m

.PHONY: build

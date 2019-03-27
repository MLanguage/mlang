SOURCE_DIR=calculette-impots-m-source-code/sources-latin1/sourcesm2015m_4_6

build:
	ocamlbuild -use-ocamlfind src/main.native

test: build
		./main.native --debug test.m

parse_all: build
		./main.native $(wildcard $(SOURCE_DIR)/*.m) --debug

OCAMLDOC_FILES = src/**/*.ml src/*.ml
DOC_FOLDER = doc
OCAML_INCLUDES = \
	-I _build/src \
	-I _build/src/parsing \
	-I _build/src/cfg \
	-I $(OPAM_SWITCH_PREFIX)/lib/ANSITerminal

doc:
	mkdir -p $(DOC_FOLDER)
	ocamldoc \
		$(OCAML_INCLUDES) \
		-html -keep-code -m p -sort \
		-colorize-code -d $(DOC_FOLDER) \
		-t "Verifisc M compiler" \
		$(OCAMLDOC_FILES)

.PHONY: build doc

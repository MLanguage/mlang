# Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2018)
#
# This software is a computer program whose purpose is to compile and analyze
# programs written in the M langage, created by thge DGFiP.
#
# This software is governed by the CeCILL-C license under French law and
# abiding by the rules of distribution of free software.  You can  use,
# modify and/ or redistribute the software under the terms of the CeCILL-C
# license as circulated by CEA, CNRS and INRIA at the following URL
# http://www.cecill.info.
#
# As a counterpart to the access to the source code and  rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty  and the software's author,  the holder of the
# economic rights,  and the successive licensors  have only  limited
# liability.
#
# In this respect, the user's attention is drawn to the risks associated
# with loading,  using,  modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean  that it is complicated to manipulate,  and  that  also
# therefore means  that it is reserved for developers  and  experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and,  more generally, to use and operate it in the
# same conditions as regards security.
#
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL-C license and that you accept its terms.

SOURCE_DIR_2015=ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=ir-calcul/sources2017m_6_10/

SOURCE_FILES=$(shell find $(SOURCE_DIR_2015) -name "*.m")

CMT_FILES = $(shell find _build -name "*.cmt")
CMTI_FILES = $(shell find _build -name "*.cmti")
ODOC_FILES = $(CMT_FILES:.cmt=.odoc) $(CMTI_FILES:.cmti=.odoc)
DOC_FOLDER = doc
ANSI_FOLDER = $(shell ocamlfind query ANSITerminal)
GRAPH_FOLDER = $(shell ocamlfind query ocamlgraph)
Z3_FOLDER = $(shell ocamlfind query z3)
OCAML_INCLUDES = \
	-I _build/src \
	-I _build/src/parsing \
	-I _build/src/mvg \
	-I _build/src/analysis \
	-I _build/src/optimization \
	-I _build/src/z3 \
	-I $(ANSI_FOLDER) \
	-I $(GRAPH_FOLDER) \
	-I $(Z3_FOLDER)


export LD_LIBRARY_PATH=$(Z3_FOLDER)

deps:
	opam install ppx_deriving ANSITerminal str ocamlgraph z3

build:
	ocamlbuild -cflag -g -use-ocamlfind src/main.native

test: build
		./main.native --debug test.m

batch: build
		./main.native $(SOURCE_FILES) ir.m --application batch --debug

iliad: build
		./main.native $(SOURCE_FILES) ir.m --application iliad --debug --no_cycles_check

bareme: build
		./main.native $(SOURCE_FILES) ir.m --application bareme --debug

doc-depend:

%.odoc: %.cmt
	odoc compile $< -r --package verifisc $(OCAML_INCLUDES)
	odoc html $@ --output-dir $(DOC_FOLDER) $(OCAML_INCLUDES)

%.odoc: %.cmti
	odoc compile $< -r --package verifisc $(OCAML_INCLUDES)
	odoc html $@ --output-dir $(DOC_FOLDER) $(OCAML_INCLUDES)

%.odoc: %.mld
	odoc compile $< -r --package verifisc $(OCAML_INCLUDES)
	odoc html $@ --output-dir $(DOC_FOLDER) $(OCAML_INCLUDES)

src/page-index.odoc: src/index.mld
	odoc compile $< -r --package verifisc $(OCAML_INCLUDES)
	odoc html $@ --output-dir $(DOC_FOLDER) $(OCAML_INCLUDES)

doc: build
	$(MAKE) doc_

THEME_DIR = $(shell ocamlfind query odig)/../../share/odig/odoc-theme/gruvbox.dark

doc_: $(CMT_FILES) $(CMTI_FILES) $(ODOC_FILES) src/page-index.odoc
	odoc support-files --output-dir $(DOC_FOLDER)
	cp -r $(THEME_DIR)/. $(DOC_FOLDER)
	md2mld README.md > README.mld
	odoc compile README.mld --package verifisc $(OCAML_INCLUDES)
	odoc html page-README.odoc --output-dir $(DOC_FOLDER) $(OCAML_INCLUDES)
	mv $(DOC_FOLDER)/verifisc/README.html $(DOC_FOLDER)/index.html
	sed -i "s|\.\.\/odoc.css|odoc.css|" $(DOC_FOLDER)/index.html
	sed -i "s|\.\.\/highlight.pack.js|highlight.pack.js|" $(DOC_FOLDER)/index.html

.PHONY: build doc

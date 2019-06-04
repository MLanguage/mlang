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

SOURCE_FILES=chap-1.m chap-2.m chap-3.m chap-4.m chap-51.m chap-52.m chap-6.m \
 	chap-7.m chap-81.m chap-82.m chap-83.m chap-84.m chap-85.m chap-86.m chap-87.m \
	chap-88.m chap-aff.m chap-cinr.m chap-cmajo.m chap-cor.m chap-ctl.m chap-ini.m \
	chap-inr.m chap-isf.m chap-majo.m chap-perp.m chap-plaf.m chap-taux.m chap-teff.m \
	chap-thr.m chap-tl.m coc1.m coc2.m coc3.m coc4.m coc5.m coc7.m coi1.m coi2.m \
	coi3.m errB.m errI.m horizoc.m horizoi.m res-ser1.m res-ser2.m tgvB.m tgvI.m

OCAMLDOC_FILES = src/**/*.ml src/*.ml
DOC_FOLDER = doc
ANSI_FOLDER = $(shell ocamlfind query ANSITerminal)
GRAPH_FOLDER = $(shell ocamlfind query ocamlgraph)
Z3_FOLDER = $(shell ocamlfind query z3)
OCAML_INCLUDES = \
	-I _build/src \
	-I _build/src/parsing \
	-I _build/src/cfg \
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

parse_all: build
		./main.native $(addprefix "../ir-calcul/sources2017m_6_10/", $(SOURCE_FILES)) --application bareme --debug

doc:
	mkdir -p $(DOC_FOLDER)
	opam config env
	ocamldoc \
		$(OCAML_INCLUDES) \
		-html -keep-code -m p -sort \
		-colorize-code -d $(DOC_FOLDER) \
		-t "Verifisc M compiler" \
		$(OCAMLDOC_FILES)

.PHONY: build doc

# Copyright 2018 Denis Merigoux and INRIA
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


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

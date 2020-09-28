##################################################
# Variables
##################################################

SOURCE_DIR_2015=$(PWD)/ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=$(PWD)/ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=$(PWD)/ir-calcul/sources2017m_6_10/
SOURCE_DIR_2018=$(PWD)/ir-calcul/sources2018m_6_7/

SOURCE_FILES?=$(shell find $(SOURCE_DIR_2018) -name "*.m")

ifeq ($(OPTIMIZE), 1)
    OPTIMIZE_FLAG=-O
else
    OPTIMIZE_FLAG=
endif

MLANG_BIN=dune exec --no-print-director src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(PWD)/2018.mpp \
	--mpp_function=compute_double_liquidation_pvro

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

default: build

##################################################
# Building the compiler
##################################################

deps:
	opam install ppx_deriving ANSITerminal re ocamlgraph dune menhir \
	cmdliner dune-build-info visitors parmap num ocamlformat
	git submodule update --init --recursive

format:
	dune build @fmt --auto-promote | true

build: #format
	dune build

##################################################
# Testing the compiler
##################################################

# use: TEST_FILE=bla make test
test: build
	$(MLANG) --run_test=$(TEST_FILE) $(SOURCE_FILES)

# use: TESTS_DIR=bla make test
tests: build
	$(MLANG) --run_all_tests=$(TESTS_DIR) $(SOURCE_FILES)

quick_test:
	$(MLANG) --backend interpreter --function_spec specs/complex_case_with_ins_outs_2018.m_spec $(SOURCE_FILES)

##################################################
# Doc and examples
##################################################

doc:
	dune build @doc
	ln -s _build/default/_doc/_html/index.html doc.html

examples: FORCE
	$(MAKE) -C examples/python

FORCE:

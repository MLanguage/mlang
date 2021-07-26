##################################################
# Variables
##################################################

SOURCE_DIR_2015=$(wildcard $(PWD)/ir-calcul/sources2015m_4_6/*.m)
SOURCE_DIR_2016=$(wildcard $(PWD)/ir-calcul/sources2016m_4_5/*.m)
SOURCE_DIR_2017=$(wildcard $(PWD)/ir-calcul/sources2017m_6_10/*.m)
SOURCE_DIR_2018=$(wildcard $(PWD)/ir-calcul/sources2018m_6_7/*.m)
SOURCE_DIR_2019=$(wildcard $(PWD)/ir-calcul/sources2019m_8_0/*.m)

YEAR?=2019

ifeq ($(YEAR), 2018)
	SOURCE_FILES?=$(SOURCE_DIR_2018)
	MPP_FILE?=$(PWD)/mpp_specs/2018_6_7.mpp
	TESTS_DIR?=tests/2018/fuzzing/
	M_SPEC_FILE?=m_specs/complex_case_with_ins_outs_2018.m_spec
else ifeq ($(YEAR), 2019)
	SOURCE_FILES?=$(SOURCE_DIR_2019)
	MPP_FILE?=$(PWD)/mpp_specs/2019_8_0.mpp
	TESTS_DIR?=tests/2019/fuzzing/
	M_SPEC_FILE?=m_specs/complex_case_with_ins_outs_2019.m_spec
else
    $(error Unsupported year: $(YEAR))
endif

ifeq ($(OPTIMIZE), 1)
    OPTIMIZE_FLAG=-O --fast-math
else
    OPTIMIZE_FLAG=
endif

ifeq ($(CODE_COVERAGE), 1)
    CODE_COVERAGE_FLAG=--code_coverage
else
    CODE_COVERAGE_FLAG=
endif

MLANG_BIN=dune exec --no-print-director src/main.exe --

MPP_FUNCTION?=compute_double_liquidation_pvro

PRECISION?=double

TEST_ERROR_MARGIN?=0.

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--precision $(PRECISION) \
	--mpp_file=$(MPP_FILE) \
	--test_error_margin=$(TEST_ERROR_MARGIN) \
	--mpp_function=$(MPP_FUNCTION)

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG) $(CODE_COVERAGE_FLAG)

default: build

##################################################
# Building the compiler
##################################################

deps:
	opam install ppx_deriving ANSITerminal re ocamlgraph dune menhir \
		cmdliner dune-build-info visitors parmap num ocamlformat mlgmpidl \
		ocamlformat
	git submodule update --init --recursive

format:
	dune build @fmt --auto-promote | true

build: format
	dune build

# Run only in an opam switch with musl and static options activated
build-static: format
	dune build --profile=static

##################################################
# Testing the compiler
##################################################

# use: TEST_FILE=bla make test
test: build
	$(MLANG) --run_test=$(TEST_FILE) $(SOURCE_FILES)

# use: TESTS_DIR=bla make test
tests: build
	$(MLANG) --run_all_tests=$(TESTS_DIR) $(SOURCE_FILES)

test_python_backend:
	$(MAKE) -C examples/python/backend_tests all_tests

test_c_backend_perf:
	$(MAKE) -C examples/c/backend_tests run_perf

test_c_backend:
	$(MAKE) -C examples/c/backend_tests run_tests

quick_test:
	$(MLANG) --backend interpreter --function_spec $(M_SPEC_FILE) $(SOURCE_FILES)

all: tests test_python_backend test_c_backend_perf \
	test_c_backend quick_test

##################################################
# Doc
##################################################

doc: FORCE
	dune build @doc
	ln -s $(shell pwd)/_build/default/_doc/_html/index.html doc/doc.html

clean:
	$(MAKE) -C examples/c clean
	$(MAKE) -C examples/python clean
	dune clean

FORCE:

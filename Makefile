# Check Makefile.config.template if you want to override some of the flags
# in this Makefile.

include Makefile.include

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

deps-without-ocaml:
	opam install . --deps-only
	git submodule update --init --recursive

deps:
	opam switch create . --deps-only
	git submodule update --init --recursive

format:
	dune build @fmt --auto-promote | true

build: format dune

dune:
	dune build $(DUNE_OPTIONS)

# Run only in an opam switch with musl and static options activated
build-static: DUNE_OPTIONS+=--profile=static 
build-static: build

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

test_java_backend:
ifeq ($(OPTIMIZE), 0)
	@echo "\033[0;31mWarning, non-optimized Java files cannot be executed for now (too many constants for the JVM)\033[0m"
else
endif
	$(MAKE) -C examples/java/ run_tests

test_dgfip_c_backend:
	$(MAKE) -C examples/dgfip_c/backend_tests run_tests

quick_test:
	$(MLANG) --backend interpreter --function_spec $(M_SPEC_FILE) $(SOURCE_FILES)

all: tests test_python_backend test_c_backend_perf \
	test_c_backend test_java_backend test_dgfip_c_backend quick_test

##################################################
# Doc
##################################################

doc: FORCE
	dune build @doc
	ln -fs $(shell pwd)/_build/default/_doc/_html/index.html doc/doc.html

clean:
	$(MAKE) -C examples/c clean
	$(MAKE) -C examples/python clean
	$(MAKE) -C examples/java clean
	dune clean

FORCE:

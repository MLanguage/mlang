# Check Makefile.config.template if you want to override some of the flags
# in this Makefile.

include Makefile.include

ifeq ($(CODE_COVERAGE), 1)
    CODE_COVERAGE_FLAG=--code_coverage
else
    CODE_COVERAGE_FLAG=
endif

ifeq ($(TEST_FILTER), 1)
    TEST_FILTER_FLAG=--dgfip_test_filter
else
    TEST_FILTER_FLAG=
endif

MLANG_INTERPRETER_OPTS=\
	--mpp_file=$(MPP_FILE) \
	--mpp_function=$(MPP_FUNCTION)

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(MLANG_INTERPRETER_OPTS) $(CODE_COVERAGE_FLAG)

default: build

##################################################
# Initializing the project
##################################################

# Workaround for Opam 2.0 bug. Empty switch creation then installation could be a one line
# "opam switch create . --deps-only" otherwise
create-switch:
	opam switch create . --empty

init-without-switch:
	opam install . --deps-only
	git submodule update --init

init: create-switch init-without-switch

deps:
	opam switch reinstall --deps-only
	git submodule update

##################################################
# Building the compiler
##################################################

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
	$(MLANG) $(MLANGOPTS) --run_all_tests=$(TESTS_DIR) $(TEST_FILTER_FLAG) $(SOURCE_FILES)

test_java_backend: build
ifeq ($(OPTIMIZE), 0)
	@echo "\033[0;31mWarning, non-optimized Java files cannot be executed for now (too many constants for the JVM)\033[0m"
else
endif
	$(MAKE) -C examples/java/ run_tests

test_dgfip_c_backend: build
	$(MAKE) -C examples/dgfip_c/ml_primitif backend_tests

quick_test: build
	$(MLANG) --backend interpreter --function_spec $(M_SPEC_FILE) $(SOURCE_FILES)

all: tests test_java_backend test_dgfip_c_backend quick_test

##################################################
# Doc
##################################################

doc: FORCE build
	dune build @doc
	ln -fs $(shell pwd)/_build/default/_doc/_html/index.html doc/doc.html

clean:
	$(MAKE) -C examples/dgfip_c/ml_primitif cleanall
	$(MAKE) -C examples/java clean
	rm -f doc/doc.html
	dune clean

FORCE:

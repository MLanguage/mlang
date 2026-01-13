###############################################
#        Init project & build compiler        #
###############################################

##################################################
# Initializing the project
##################################################

# Workaround for Opam 2.0 bug. Empty switch creation then installation could be a one line
# "opam switch create . --deps-only" otherwise
create-switch: FORCE
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	opam switch create . --empty
endif

init-without-switch: FORCE
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	opam install . --deps-only
	git submodule init ir-calcul
	git submodule update ir-calcul
endif

init: FORCE create-switch init-without-switch

deps: FORCE
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	opam switch reinstall --deps-only
	git submodule update ir-calcul
endif

remise_a_zero_versionnage: FORCE
	sed -i 's/(version .*)/(version %%VERSION%%)/' dune-project
	git checkout -- *.opam

##################################################
# Building the compiler
##################################################

format: FORCE
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	dune build @fmt --auto-promote | true
endif

dune: FORCE
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	echo $(shell pwd)
	sed -i 's/(version %%VERSION%%)/(version ${shell git describe --always --dirty --tag})/' dune-project
	LINKING_MODE=$(LINKING_MODE) dune build $(DUNE_OPTIONS)
	$(call make_in_raw,,remise_a_zero_versionnage)
endif

build: FORCE | format dune

build-dev: DUNE_OPTIONS=--profile dev
build-dev: FORCE | format dune

build-ci: DUNE_OPTIONS=--profile ci
build-ci: FORCE | dune

build-release: DUNE_OPTIONS=--profile release
build-release: FORCE | dune

build-static: LINKING_MODE=static
build-static: FORCE build-release

build-doc: DUNE_OPTIONS=@doc
build-doc: FORCE | dune

##################################################
# Testing the compiler
##################################################

# use: TEST_FILE=bla make test
test: FORCE build-dev
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	OCAMLRUNPARAM=b $(MLANG_TEST) --run_test=$(TEST_FILE) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif

# use: TESTS_DIR=bla make test
tests: FORCE build
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	$(MLANG_TEST) $(MLANGOPTS) --run_all_tests=$(TESTS_DIR)/ $(TEST_FILTER_FLAG) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif

test_one: FORCE build-dev
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	OCAMLRUNPARAM=b $(MLANG_TEST) --run_test=$(TESTS_DIR)/$(TEST_ONE) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif

test_file: FORCE build-dev
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	OCAMLRUNPARAM=b $(MLANG_TEST) --run_test=$(TEST_FILE) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif

test_irj: FORCE build-dev
	@for dir in $(IRJ_TESTS_DIRS); do \
		OCAMLRUNPARAM=b dune exec -- irj_checker $$dir -mhuman; \
		if [ $$? -ne 0 ]; \
		then \
			echo "Failed test $$dir"; \
			exit 1; \
		fi; \
	done;

##################################################
# Doc
##################################################

TARGET_DIR_SPHINX_DOC_SRC:= _build/default/sphinx-doc-src
TARGET_DIR_DOC_BUILD:= _build/default/full-doc

doc-deps: FORCE
	python3 -m venv .venv
	.venv/bin/pip install sphinx myst-parser

sphinx-doc: FORCE build dev-doc
	@command -v .venv/bin/sphinx-build >/dev/null 2>&1 || \
	{ echo "Pour construire la documentation, vous avez besoin de sphinx-build avec \
		l'extension 'myst-parser'. Lancez `make doc-deps`."; exit 1; }
	rm -rf $(TARGET_DIR_SPHINX_DOC_SRC)/*
	cp -r doc $(TARGET_DIR_SPHINX_DOC_SRC)
	mkdir -p $(TARGET_DIR_SPHINX_DOC_SRC)/_static/dev
	cp -r $(shell pwd)/_build/default/_doc/_html/* $(TARGET_DIR_SPHINX_DOC_SRC)/_static/dev
	.venv/bin/sphinx-build -M html $(TARGET_DIR_SPHINX_DOC_SRC) $(TARGET_DIR_DOC_BUILD)
	.venv/bin/sphinx-build -M latexpdf $(TARGET_DIR_SPHINX_DOC_SRC) $(TARGET_DIR_DOC_BUILD)

dev-doc: FORCE build
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	dune build @doc
endif

doc: FORCE build dev-doc sphinx-doc
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	rm -rf examples/doc
	mkdir -p examples/doc
	cp -r $(TARGET_DIR_DOC_BUILD)/* examples/doc
endif

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
test: FORCE build
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	$(MLANG_TEST) --run_test=$(TEST_FILE) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif

# use: TESTS_DIR=bla make test
tests: FORCE build
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	$(MLANG_TEST) $(MLANGOPTS) --run_all_tests=$(TESTS_DIR)/ $(TEST_FILTER_FLAG) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif

test_one: FORCE build
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	$(MLANG_TEST) --run_test=$(TESTS_DIR)/$(TEST_ONE) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif


test_file: FORCE build
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	$(MLANG_TEST) --run_test=$(TEST_FILE) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif


##################################################
# Doc
##################################################

doc: FORCE build
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	dune build @doc
	ln -fs $(shell pwd)/_build/default/_doc/_html/index.html doc/doc.html
endif


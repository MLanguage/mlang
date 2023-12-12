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
	git submodule update --init
endif

init: FORCE create-switch init-without-switch

deps: FORCE
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	opam switch reinstall --deps-only
	git submodule update
endif

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
	dune build $(DUNE_OPTIONS)
endif

build: FORCE | format dune

# Run only in an opam switch with musl and static options activated
build-static: DUNE_OPTIONS+=--profile=static 
build-static: FORCE build

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
	$(MLANG_TEST) $(MLANGOPTS) --run_all_tests=$(TESTS_DIR) $(TEST_FILTER_FLAG) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif

quick_test: FORCE build
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	$(MLANG_TEST) --backend interpreter --function_spec $(M_SPEC_FILE) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
endif

test_one: FORCE build
ifeq ($(call is_in,),)
	$(call make_in,,$@)
else
	$(MLANG_TEST) --backend interpreter --run_test=$(TEST_ONE) $(SOURCE_FILES) $(SOURCE_EXT_FILES)
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


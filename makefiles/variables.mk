###################################
#            Variables            #
###################################

##################################################
# Generic variables
##################################################

GCC=gcc
MUSL_HOME?=/usr/local/musl

##################################################
# Tax computation configuration
##################################################

MPP_FUNCTION_BACKEND?=enchainement_primitif
MPP_FUNCTION?=enchainement_primitif_interpreteur
SOURCE_EXT_FILES?=$(call source_dir_ext,$(ROOT_DIR)/m_ext/$(YEAR)/)
# Add a TESTS_DIR for 2023 when available
ifeq ($(YEAR), 2023)
	#$(warning WARNING: the source M files and fuzzer tests have not yet been published for year: $(YEAR). Should you choose to provide your own source files, you can create a directory ir-calcul/M_SVN/$(YEAR) and put them in there)
	SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/M_SVN/$(YEAR)/)
	TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)/fuzzing.ko
else ifeq ($(filter $(YEAR), 2019 2020 2021 2022), $(YEAR))
	SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/sources$(YEAR)*/)
	TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)/fuzzing
else
    $(warning WARNING: there is no default configuration for year: $(YEAR))
    $(warning WARNING: example specification files and fuzzer tests are not included for year: $(YEAR))
endif

##################################################
# Mlang configuration
##################################################

MLANG_BIN=dune exec $(ROOT_DIR)/_build/default/src/main.exe --

PRECISION?=double
MLANG_DEFAULT_OPTS=\
 -A iliad\
 --display_time --debug\
 --precision $(PRECISION)

##################################################
# C backend configuration
##################################################

# CC is a GNU make default variable defined to CC
# It so can't be overriden by conditional operator ?=
# We check the origin of CC value to not override CL argument or explicit environment.
ifeq ($(origin CC),default)
  CC=clang
endif

# Options pour le compilateur C
# Attention, très long à compiler avec GCC en O2/O3
COMMON_CFLAGS?=-std=c89 -pedantic
ifeq ($(CC), clang)
  COMPILER_SPECIFIC_CFLAGS=-O2
#  COMPILER_SPECIFIC_CFLAGS=
else ifeq ($(CC), gcc)
  COMPILER_SPECIFIC_CFLAGS=-O1
endif
BACKEND_CFLAGS?=$(COMMON_CFLAGS) $(COMPILER_SPECIFIC_CFLAGS)

# Directory of the driver sources for tax calculator
DRIVER_DIR?=ml_driver
# Driver sources for tax calculator (must be manually ordered for OCaml compiler)
DRIVER_FILES?=irdata.c stubs.c common.ml m.ml read_test.ml main.ml

# Flag to disable binary dump comparison
NO_BINARY_COMPARE?=1

##################################################
# Etc.
##################################################

ifeq ($(CODE_COVERAGE), 1)
  CODE_COVERAGE_FLAG=--code_coverage
else
  CODE_COVERAGE_FLAG=
endif

ifeq ($(TEST_FILTER), 1)
  TEST_FILTER_FLAG=--dgfip_test_filter
  TEST_FILES=$(TESTS_DIR)/[A-Z]*
else
  TEST_FILTER_FLAG=
  TEST_FILES=$(TESTS_DIR)/*
endif

# Précision des comparaisons entre flottants pendant les calculs
COMPARISON_ERROR_MARGIN?=0.000001

MLANG_INTERPRETER_OPTS=\
  --income-year=$(YEAR) \
  --comparison_error_margin=$(COMPARISON_ERROR_MARGIN) \
  --mpp_function=$(MPP_FUNCTION)

MLANG_TEST=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(MLANG_INTERPRETER_OPTS) $(CODE_COVERAGE_FLAG)

DGFIP_DIR?=examples/dgfip_c/ml_primitif

MAKE_DGFIP=$(MAKE) --no-print-directory -f $(ROOT_DIR)/Makefile -C $(ROOT_DIR)/$(DGFIP_DIR) ROOT_DIR="$(ROOT_DIR)"

MAKE_DGFIP_CALC=$(MAKE) --no-print-directory -f $(ROOT_DIR)/Makefile -C $(ROOT_DIR)/$(DGFIP_DIR)/calc ROOT_DIR="$(ROOT_DIR)"


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

ifeq ($(YEAR), 2022)
	SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/sources2022m_6_1/)
	SOURCE_EXT_FILES?=$(call source_dir_ext,$(ROOT_DIR)/m_ext/2022/)
	TESTS_DIR?=$(ROOT_DIR)/tests/2022/fuzzing
	M_SPEC_FILE?=$(ROOT_DIR)/m_specs/complex_case_with_ins_outs_2020.m_spec
	MPP_FUNCTION_BACKEND?=enchainement_primitif
	MPP_FUNCTION?=enchainement_primitif_interpreteur
else ifeq ($(YEAR), 2021)
	SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/sources2021m_20_6/)
	SOURCE_EXT_FILES?=$(call source_dir_ext,$(ROOT_DIR)/m_ext/2021/)
	TESTS_DIR?=$(ROOT_DIR)/tests/2021/fuzzing
	M_SPEC_FILE?=$(ROOT_DIR)/m_specs/complex_case_with_ins_outs_2020.m_spec
	MPP_FUNCTION_BACKEND?=enchainement_primitif
	MPP_FUNCTION?=enchainement_primitif_interpreteur
else ifeq ($(YEAR), 2020)
	SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/sources2020m_6_5/)
	SOURCE_EXT_FILES?=$(call source_dir_ext,$(ROOT_DIR)/m_ext/2020/)
	TESTS_DIR?=$(ROOT_DIR)/tests/2020/fuzzing
	M_SPEC_FILE?=$(ROOT_DIR)/m_specs/complex_case_with_ins_outs_2020.m_spec
	MPP_FUNCTION_BACKEND?=enchainement_primitif
	MPP_FUNCTION?=enchainement_primitif_interpreteur
else ifeq ($(YEAR), 2019)
	SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/sources2019m_8_0/)
	SOURCE_EXT_FILES?=$(call source_dir_ext,$(ROOT_DIR)/m_ext/2019/)
	TESTS_DIR?=$(ROOT_DIR)/tests/2019/fuzzing
	M_SPEC_FILE?=m_specs/complex_case_with_ins_outs_2019.m_spec
	MPP_FUNCTION_BACKEND?=enchainement_primitif
	MPP_FUNCTION?=enchainement_primitif_interpreteur
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


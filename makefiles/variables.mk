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
SOURCE_EXT_DIR=$(ROOT_DIR)/m_ext/$(YEAR)
# Add a TESTS_DIR for 2024 when available
ifeq ($(filter $(YEAR), 2024 2025), $(YEAR))
	SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/M_SVN/$(YEAR)/code_m/)
	SOURCE_EXT_FILES?=$(call source_dir_ext,$(ROOT_DIR)/m_ext/$(YEAR)/)
	TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)/fuzzing
else ifeq ($(filter $(YEAR), 2022), $(YEAR))
	SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/sources$(YEAR)*/)
	SOURCE_EXT_FILES?=\
		$(SOURCE_EXT_DIR)/cibles.m \
		$(SOURCE_EXT_DIR)/codes_1731.m \
		$(SOURCE_EXT_DIR)/commence_par_5.m \
		$(SOURCE_EXT_DIR)/commence_par_7.m \
		$(SOURCE_EXT_DIR)/commence_par_H.m \
		$(SOURCE_EXT_DIR)/correctif.m \
		$(SOURCE_EXT_DIR)/main.m
	TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)/fuzzing
else ifeq ($(filter $(YEAR), 2018 2019 2020 2023), $(YEAR))
	SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/sources$(YEAR)*/)
	SOURCE_EXT_FILES?=$(call source_dir_ext,$(ROOT_DIR)/m_ext/$(YEAR)/)
	TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)/fuzzing
else ifeq ($(filter $(YEAR), 0), $(YEAR))
	SOURCE_FILES?=#$(call source_dir,$(ROOT_DIR)/m_ext/$(YEAR)/src/)
	SOURCE_EXT_FILES?=$(call source_dir_ext,$(ROOT_DIR)/m_ext/$(YEAR)/)
	TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)
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
DRIVER_DIR?=c_driver
# Driver sources for tax calculator
DRIVER_H_FILES?=aide.h chaine.h commun.h fichiers.h format.h ida.h irj.h liste.h mem.h options.h traitement.h utils.h
DRIVER_C_FILES?=aide.c chaine.c commun.c fichiers.c format.c ida.c irdata.c irj.c liste.c main.c mem.c options.c traitement.c utils.c
DRIVER_FILES?=$(DRIVER_H_FILES) $(DRIVER_C_FILES)

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


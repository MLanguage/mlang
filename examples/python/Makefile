##################################################
# Variables
##################################################

SOURCE_DIR_2015=../../ir-calcul/sources2015m_4_6/*.m
SOURCE_DIR_2016=../../ir-calcul/sources2016m_4_5/*.m
SOURCE_DIR_2017=../../ir-calcul/sources2017m_6_10/*.m
SOURCE_DIR_2018=../../ir-calcul/sources2018m_6_7/*.m
SOURCE_DIR_2019=../../ir-calcul/sources2019m_8_0/*.m

YEAR?=2019

ifeq ($(YEAR), 2018)
	SOURCE_FILES?=$(SOURCE_DIR_2018)
	MPP_FILE?=../../mpp_specs/2018_6_7.mpp
	TESTS_DIR?=../../tests/2018/fuzzing/
else ifeq ($(YEAR), 2019)
	SOURCE_FILES?=$(SOURCE_DIR_2019)
	MPP_FILE?=../../mpp_specs/2019_8_0.mpp
	TESTS_DIR?=../../tests/2019/fuzzing/
else
    $(error Unsupported year: $(YEAR))
endif

ifeq ($(OPTIMIZE), 1)
    OPTIMIZE_FLAG=-O
else
    OPTIMIZE_FLAG=
endif

MLANG_BIN=dune exec --no-print-director ../../src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(MPP_FILE) \
	--mpp_function=compute_double_liquidation_pvro

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

all: backend_tests $(shell find . -name "run_*.py")

##################################################
# Generating and running Python files from Mlang
##################################################

ir_%.py: ../../m_specs/%.m_spec
	$(MLANG) \
		--backend python --output $@ \
		--function_spec $^ \
		$(SOURCE_FILES)

run_%.py: ir_%.py
	python3 $@

##################################################
# Running the tests
##################################################


backend_tests: FORCE
	$(MAKE) -C backend_tests


FORCE:

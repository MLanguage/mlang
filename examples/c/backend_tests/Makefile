##################################################
# Variables
##################################################

SOURCE_DIR_2015=../../../ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=../../../ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=../../../ir-calcul/sources2017m_6_10/
SOURCE_DIR_2018=../../../ir-calcul/sources2018m_6_7/

SOURCE_FILES?=$(shell find $(SOURCE_DIR_2018) -name "*.m")
MPP_FILE?=../../../mpp_specs/2018_6_7.mpp

OPTIMIZE_FLAG=-O

MLANG_BIN=dune exec --no-print-director ../../../src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(MPP_FILE) \
	--mpp_function=compute_double_liquidation_pvro

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

##################################################
# Generating the tests.m_spec
##################################################

TESTS_DIR?=../../../random_tests

tests.m_spec: gen_m_spec.py 
	python3.7 $< $(TESTS_DIR) $@ 

##################################################
# Generating the C code
##################################################

ir_%.c: %.m_spec $(SOURCE_FILES)
	$(MLANG) \
		--backend c --output $@ \
		--function_spec $< \
		$(SOURCE_FILES)

.SECONDARY: ir_%.c ir_%.h
.PRECIOUS: ir_%.c ir_%.h

##################################################
# Compiling the generated C
##################################################

C_COMPILER=clang

ir_%.o: ir_%.c
	$(C_COMPILER) -I ../ -fbracket-depth=2048 $(C_OPT) -c $< ../m_value.c

%.o: %.c
	$(C_COMPILER) -I ../ -c $<

##################################################
# Building the test harness
##################################################

test_harness.exe: m_value.o ir_tests.o parse_and_run_tests.o
	$(C_COMPILER) -lm -o $@ $^


FORCE:
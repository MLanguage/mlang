##################################################
# Variables
##################################################

SOURCE_DIR_2015=../../ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=../../ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=../../ir-calcul/sources2017m_6_10/
SOURCE_DIR_2018=../../ir-calcul/sources2018m_6_7/

SOURCE_FILES?=$(shell find $(SOURCE_DIR_2018) -name "*.m")

OPTIMIZE_FLAG=-O

MLANG_BIN=dune exec --no-print-director ../../src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=../../mpp_specs/2018_6_7.mpp \
	--mpp_function=compute_double_liquidation_pvro

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

##################################################
# Generating C files from Mlang
##################################################

ir_%.c: ../../m_specs/%.m_spec $(SOURCE_FILES)
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
	$(C_COMPILER) -fbracket-depth=2048 $(C_OPT) -c $< m_value.c

%.o: %.c
	$(C_COMPILER) -c $<

##################################################
# Final targets
##################################################

# To call this target, use "make run_<name of file in m_spec/ without extension>.exe"
run_%.exe: ir_%.o run_%.o m_value.o
	$(C_COMPILER) -lm -o $@ $^

##################################################
# Running the tests
##################################################

backend_tests: FORCE
	$(MAKE) -C backend_tests run_tests

FORCE:

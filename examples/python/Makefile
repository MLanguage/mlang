##################################################
# Variables
##################################################

SOURCE_DIR_2015=$(PWD)/ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=$(PWD)/ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=$(PWD)/ir-calcul/sources2017m_6_10/
SOURCE_DIR_2018=$(PWD)/ir-calcul/sources2018m_6_7/

SOURCE_FILES?=$(shell find $(SOURCE_DIR_2018) -name "*.m")

ifeq ($(OPTIMIZE), 1)
    OPTIMIZE_FLAG=-O
else
    OPTIMIZE_FLAG=
endif

MLANG_BIN=dune exec --no-print-director ../../src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(PWD)/2018.mpp \
	--mpp_function=compute_double_liquidation_pvro

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

all: $(shell find . -name "run_*.py")

##################################################
# Generating Python files from Mlang
##################################################

ir_%.py: FORCE 
	$(MLANG) \
		--backend python --output $@ \
		--function_spec ../../specs/$*.m_spec \
		$(SOURCE_FILES)

##################################################
# Running the tests
##################################################

run_%.py: ir_%.py	
	python3.7 $@

FORCE:

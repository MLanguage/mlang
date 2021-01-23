##################################################
# Variables
##################################################

SOURCE_DIR_2015=$(wildcard ../../ir-calcul/sources2015m_4_6/*.m)
SOURCE_DIR_2016=$(wildcard ../../ir-calcul/sources2016m_4_5/*.m)
SOURCE_DIR_2017=$(wildcard ../../ir-calcul/sources2017m_6_10/*.m)
SOURCE_DIR_2018=$(wildcard ../../ir-calcul/sources2018m_6_7/*.m)
SOURCE_DIR_2019=$(wildcard ../../ir-calcul/sources2019m_8_0/*.m)

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

clean: 
	rm -rf *.java

##################################################
# Generating and running Java files from Mlang
##################################################

Ir_%.java: ../../m_specs/%.m_spec 
	$(MLANG) \
		--backend java --output $@ \
		--function_spec $^ \
		--optimize \
		$(SOURCE_FILES)
	sed -i '' -e 's/CalculImpot/$(shell echo $@ | cut -d "." -f 1)/g' $@



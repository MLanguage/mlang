##################################################
# Variables
##################################################

SOURCE_DIR_2015=$(wildcard ../../ir-calcul/sources2015m_4_6/*.m)
SOURCE_DIR_2016=$(wildcard ../../ir-calcul/sources2016m_4_5/*.m)
SOURCE_DIR_2017=$(wildcard ../../ir-calcul/sources2017m_6_10/*.m)
SOURCE_DIR_2018=$(wildcard ../../ir-calcul/sources2018m_6_7/*.m)
SOURCE_DIR_2019=$(wildcard ../../ir-calcul/sources2019m_8_0/*.m)
SOURCE_DIR_2020=$(wildcard ../../ir-calcul/sources2020m_6_5/*.m)

YEAR?=2020

ifeq ($(YEAR), 2018)
	SOURCE_FILES?=$(SOURCE_DIR_2018)
	MPP_FILE?=../../mpp_specs/2018_6_7.mpp
	TESTS_DIR?=../../tests/2018/fuzzing/
else ifeq ($(YEAR), 2019)
	SOURCE_FILES?=$(SOURCE_DIR_2019)
	MPP_FILE?=../../mpp_specs/2019_8_0.mpp
	TESTS_DIR?=../../tests/2019/fuzzing/
else ifeq ($(YEAR), 2020)
	SOURCE_FILES?=$(SOURCE_DIR_2020)
	MPP_FILE?=../../mpp_specs/2020_6_5.mpp
	TESTS_DIR?=../../tests/2020/fuzzing/
else
    $(error Unsupported year: $(YEAR))
endif

OPTIMIZE?=1

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
	rm -f src/com/mlang/Ir_*.java target/com/mlang/*.class backend_tests/target/com/mlang/*.class

##################################################
# Generating and running Java files from Mlang
##################################################

.PRECIOUS: src/com/mlang/Ir_%.java
src/com/mlang/Ir_%.java: ../../m_specs/%.m_spec
	$(MLANG) \
		--backend java --output src/com/mlang/Ir_tests_2020.java \
		--function_spec $^ \
		$(SOURCE_FILES)

target/com/mlang/Ir_%.class:  src/com/mlang/Ir_%.java
	javac -target 1.7 -source 1.7 -J-Xss10m -J-Xmx4096m -d target -cp src src/com/mlang/*.java 

backend_tests/target/TestHarness.class: target/com/mlang/Ir_tests_2020.class
	javac -J-Xss10m -cp "backend_tests/src/:target" -d backend_tests/target backend_tests/src/com/mlang/TestHarness.java 
	
run_tests: backend_tests/target/TestHarness.class
	java -cp "target:backend_tests/target" com.mlang.TestHarness $(TESTS_DIR)

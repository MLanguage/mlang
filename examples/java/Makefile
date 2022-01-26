include ../../Makefile.include

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
		--backend java --output $@ \
		--function_spec $^ \
		$(SOURCE_FILES)

target/com/mlang/Ir_%.class:  src/com/mlang/Ir_%.java
	javac  -J-Xss10m -J-Xmx4096m -target 1.7 -source 1.7 -d target -cp src src/com/mlang/*.java 

backend_tests/target/TestHarness.class: target/com/mlang/Ir_tests_2020.class
	javac -cp "backend_tests/src/:target" -d backend_tests/target backend_tests/src/com/mlang/TestHarness.java 
	
run_tests: backend_tests/target/TestHarness.class
	java -cp "target:backend_tests/target" com.mlang.TestHarness $(TESTS_DIR)

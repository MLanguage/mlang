##################################################
# Variables
##################################################

SOURCE_DIR_2015=$(wildcard ../../../ir-calcul/sources2015m_4_6/*.m)
SOURCE_DIR_2016=$(wildcard ../../../ir-calcul/sources2016m_4_5/*.m)
SOURCE_DIR_2017=$(wildcard ../../../ir-calcul/sources2017m_6_10/*.m)
SOURCE_DIR_2018=$(wildcard ../../../ir-calcul/sources2018m_6_7/*.m)
SOURCE_DIR_2019=$(wildcard ../../../ir-calcul/sources2019m_8_0/*.m)

YEAR?=2019

ifeq ($(YEAR), 2018)
	SOURCE_FILES?=$(SOURCE_DIR_2018)
	MPP_FILE?=../../../mpp_specs/2018_6_7.mpp
	TESTS_DIR?=../../../tests/2018/fuzzing/
	ONE_TEST_FILE?=../../../tests/2018/fuzzing/fuzzer_14200.m_test
else ifeq ($(YEAR), 2019)
	SOURCE_FILES?=$(SOURCE_DIR_2019)
	MPP_FILE?=../../../mpp_specs/2019_8_0.mpp
	TESTS_DIR?=../../../tests/2019/fuzzing/
	ONE_TEST_FILE?=../../../tests/2019/fuzzing/fuzzer_20741.m_test
else
    $(error Unsupported year: $(YEAR))
endif

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

tests.m_spec: gen_m_spec.py
	python3 $< $(TESTS_DIR) $@

##################################################
# Generating the C code
##################################################

ir_%.c: ../../../m_specs/%.m_spec $(SOURCE_FILES)
	$(MLANG) \
		--backend c --output $@ \
		--function_spec $< \
		$(SOURCE_FILES)
ir_%.c: %.m_spec $(SOURCE_FILES)
	$(MLANG) \
		--backend c --output $@ \
		--function_spec $< \
		$(SOURCE_FILES)

ir_%.nan.c: %.m_spec $(SOURCE_FILES)
	$(MLANG) \
		--backend c --c-nan-opt --output $@ \
		--function_spec $< \
		$(SOURCE_FILES)

.SECONDARY: ir_%.c ir_%.h ir_%.nan.c ir_%.nan.h
.PRECIOUS: ir_%.c ir_%.h ir_%.nan.c ir_%.nan.h

##################################################
# Compiling the generated C
##################################################

C_COMPILER?=clang
CC=$(C_COMPILER)

ifeq ($(C_COMPILER), clang)
    F_BRACKET_OPT=-fbracket-depth=2048
else
    F_BRACKET_OPT=
endif

ir_%.o: export AFL_DONT_OPTIMIZE=1
ir_%.o: ir_%.c
	$(CC) -I ../ $(F_BRACKET_OPT) $(C_OPT) -c -o $@ $<

%.o: %.c
	$(CC) -I ../ -O3 -c -o $@ $<

##################################################
# Building and running the test harness
##################################################

test_harness.o: test_harness.c ir_tests.c

test_harness.exe: ir_tests.o test_harness.o ../m_value.o
	$(CC) -fPIE -lm -o $@ $^

run_tests: test_harness.exe FORCE
	ulimit -s 32768; \
	./$< $(TESTS_DIR)

test_harness.nan.o: test_harness.c ir_tests.nan.c
	$(CC) -DUSE_NAN_OPTIM -I ../ -O3 -c -o $@ $<

test_nan_harness.exe: ir_tests.nan.o test_harness.nan.o ../m_nan_value.o
	$(CC) -fPIE -lm -o $@ $^

run_nan_tests: test_nan_harness.exe FORCE
	ulimit -s 32768; \
	./$< $(TESTS_DIR)

perf_harness.exe: ir_tests.o perf_harness.o ../m_value.o
	$(CC) -fPIE -lm -o $@ $^

run_perf: perf_harness.exe FORCE
	ulimit -s 32768; \
	time ./$< $(ONE_TEST_FILE)

perf_nan_harness.exe: ir_tests.nan.o perf_nan_harness.o ../m_nan_value.o
	$(CC) -fPIE -lm -o $@ $^

run_nan_perf: perf_nan_harness.exe FORCE
	ulimit -s 32768; \
	time ./$< $(ONE_TEST_FILE)

##################################################
# Building and running the fuzzing harness
##################################################

FUZZ_M_SPEC=tests

fuzz_harness.exe: CC=afl-$(C_COMPILER)
fuzz_harness.exe: ir_$(FUZZ_M_SPEC).o fuzz_harness.o ../m_value.o
	$(CC) -fPIE -lm -o $@ $^

AFL_FUZZ=afl-fuzz

ifeq ($(JOB_NO), 0)
	AFL_JOB_FLAG=-M fuzzer$(JOB_NO)
else
	AFL_JOB_FLAG=-S fuzzer$(JOB_NO)
endif

# Usage: NO_JOB=<0,1,2...> make launch_fuzz
launch_fuzz: fuzz_harness.exe
	ulimit -s 32768; \
	$(AFL_FUZZ) -i fuzz_inputs -o fuzz_findings \
		-m 500 -t 1000 $(AFL_JOB_FLAG) \
		-- ./fuzz_harness.exe @@

FUZZER_CRASHES=$(shell find fuzz_tests/ -name "*.m_crash" 2> /dev/null)

sanitize_crash_names:
	./sanitize_crash_names.sh

%.m_test: %.m_crash FORCE
	-stdbuf -oL bash -c "./fuzz_harness.exe $< > $@"

transform_crashes_into_tests: $(patsubst %.m_crash,%.m_test,$(FUZZER_CRASHES))

clean_fuzz_findings:
	rm -rf fuzz_findings/*
clean_fuzz_tests:
	rm -rf fuzz_tests/*.m_crash

clean:
	rm -f ir_tests.* ../m_value.o *.o tests.m_spec *.exe *.tmp

FORCE:

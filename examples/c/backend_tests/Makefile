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

.SECONDARY: ir_%.c ir_%.h
.PRECIOUS: ir_%.c ir_%.h

##################################################
# Compiling the generated C
##################################################

C_COMPILER?=clang

ifeq ($(C_COMPILER), clang)
    F_BRACKET_OPT=-fbracket-depth=2048
else
    F_BRACKET_OPT= 
endif

ir_%.o: export AFL_DONT_OPTIMIZE=1
ir_%.o: ir_%.c
	$(C_COMPILER) -I ../ $(F_BRACKET_OPT) $(C_OPT) -c -o $@ $<

%.o: %.c
	$(C_COMPILER) -I ../ -c -o $@ $<

##################################################
# Building and running the test harness
##################################################

test_harness.exe: ir_tests.o test_harness.o ../m_value.o
	$(C_COMPILER) -lm -o $@ $^

run_tests: test_harness.exe FORCE
	ulimit -s 65536; \
	./$< $(TESTS_DIR)

##################################################
# Building and running the fuzzing harness
##################################################

FULL_INPUTS_FUZZ?=0

ifeq ($(FULL_INPUTS_FUZZ), 1)
    FUZZ_M_SPEC=all_ins_no_outs_2018
else
    FUZZ_M_SPEC=simulateur_simplifie_2018
endif

fuzz_harness.exe: C_COMPILER=afl-clang
fuzz_harness.exe: ir_$(FUZZ_M_SPEC).o fuzz_harness.o ../m_value.o
	$(C_COMPILER) -lm -o $@ $^

AFL_FUZZ=afl-fuzz

ifeq ($(JOB_NO), 0)
	AFL_JOB_FLAG=-M fuzzer$(JOB_NO)
else 
	AFL_JOB_FLAG=-S fuzzer$(JOB_NO)
endif

# Usage: NO_JOB=<0,1,2...> make launch_fuzz
launch_fuzz: fuzz_harness.exe
	ulimit -s 65536; \
	$(AFL_FUZZ) -i fuzz_inputs -o fuzz_findings \
		-m 500 -t 1000 $(AFL_JOB_FLAG) \
		-- ./fuzz_harness.exe @@

clean:
	rm -rf fuzz_findings/*

##################################################
# Building and running the KLEE harness
##################################################

%.bc: C_COMPILER=clang
%.bc: %.c
	$(C_COMPILER) -I ../ -fbracket-depth=2048 -I $(KLEE_DIR)/include \
		 -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone -o $@ $<

run_klee: ../m_value.bc ir_$(FUZZ_M_SPEC).bc klee_harness.bc
	$(KLEE_DIR)/build/bin/klee \
		-link-llvm-lib=ir_$(FUZZ_M_SPEC).bc \
		-link-llvm-lib=../m_value.bc \
		klee_harness.bc


FORCE:

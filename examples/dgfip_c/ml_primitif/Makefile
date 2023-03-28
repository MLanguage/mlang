ifeq ($(MAKELEVEL), 0)
    include ../../../Makefile.include
endif

ifeq ($(TEST_FILTER), 1)
    TEST_FILES=$(TESTS_DIR)[A-Z]*
else
    TEST_FILES=$(TESTS_DIR)*
endif

# DGFiP C backend works only with these specifications at the moment.
MPP_FILE=../../../mpp_specs/dgfip_base.mpp
MPP_FUNCTION=dgfip_calculation

MLANG_DGFIP_C_OPTS=\
	--mpp_file=$(MPP_FILE) \
	--mpp_function=$(MPP_FUNCTION)

# Options supplémentaires pour le backend Mlang/DGFiP
# Note: les flags -Ailiad, -m, -g et -k sont déjà définis
DGFIPFLAGS=-X,-O  # = environnement DGFiP

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(MLANG_DGFIP_C_OPTS)
QUIET=>/dev/null # Uncomment to suppress output

# Options pour le compilateur C
# Attention, très long à compiler avec GCC en O2/O3
# Le flag sur la profondeur des blocs ne semble plus nécessaire.
COMMON_CFLAGS=-std=c89 -pedantic
ifeq ($(CC), clang)
    COMPILER_SPECIFIC_CFLAGS=-O2 -fbracket-depth=2048
else ifeq ($(CC), gcc)
    COMPILER_SPECIFIC_CFLAGS=-O1
endif
CFLAGS=$(COMMON_CFLAGS) $(COMPILER_SPECIFIC_CFLAGS)

# Options pour le compilateur OCaml
OCAMLFLAGS=
#OCAMLFLAGS="-g -inline 0"
# Pour instrumenter la couverture de code, il est nécessaire d'installer le paquet OCaml bisect_ppx
# 1 pour activer la couverture de code
WITH_BISECT?=0
ifeq ($(WITH_BISECT), 1)
    BISECT_PATH:=$(shell ocamlfind query bisect_ppx)
    ifeq ($(BISECT_PATH),)
      $(error $(BISECT_PATH) \
	  Pour instrumenter la couverture de code, il est nécessaire d'installer le paquet OCaml bisect_ppx)
    endif
endif

.DEFAULT_GOAL := cal

##################################################
# Generating the tests.m_spec
##################################################

# Obsolète ?
tests.m_spec: gen_m_spec.py
	python3 $< $(TESTS_DIR) $@

##################################################
# Generating C files from M sources with Mlang
##################################################

calc/var_static.c.inc calc/enchain_static.c.inc calc/desc_static.h.inc calc/const.h calc/dbg.h calc/irdata.c calc/irdata.h &:
	mkdir -p calc
	@echo "Copie des fichiers C statiques"
	cp static/* calc/

# Ml_primitif (current main build)
# -----------------------------
# Note: compilation avec -g et -kN (N=1..4) OBLIGATOIREMENT
calc/enchain.c: $(SOURCE_FILES)
	mkdir -p calc
	@echo "Compilation des fichiers M avec Mlang (YEAR=$(YEAR), MPP_FUNCTION=$(MPP_FUNCTION), DGFIPFLAGS=$(DGFIPFLAGS))"
	@$(MLANG) \
		--dgfip_options=-g,-k4,-Ailiad,-m$(YEAR),$(DGFIPFLAGS)\
		--backend dgfip_c --output $@ \
		$(SOURCE_FILES) $(QUIET)
	@echo "Compilation terminée"

# Ancien makefile racine du backend
# -----------------------------
.SECONDARY: calc/ir_%.c calc/ir_%.h
.PRECIOUS: calc/ir_%.c calc/ir_%.h

calc/old_ir_%.c: $(SOURCE_FILES)
	$(MLANG) \
		--dgfip_options=-rXMk4,-b0,-Ailiad,-m$(YEAR)\
		--backend dgfip_c --output $@ \
		$(SOURCE_FILES)

# Ancien makefile backend_tests
# -----------------------------
# branche morte : il n'y a pas de quoi construire un exécutable qui corresponde dans le makefile.
calc/ir_%.c: ../../../m_specs/%.m_spec $(SOURCE_FILES)
	$(MLANG) \
		--dgfip_options=-rXMk4,-b0,-Ailiad,-m$(YEAR)\
		--backend dgfip_c --output $@ \
		--function_spec $< \
		$(SOURCE_FILES)
calc/ir_%.c: %.m_spec $(SOURCE_FILES)
	$(MLANG) \
		--dgfip_options=-rXMk1,-b0,-Ailiad,-m$(YEAR)\
		--backend dgfip_c --output $@ \
		--function_spec $< \
		$(SOURCE_FILES)

#################################################
# Compiling the generated C
##################################################

# Ml_primitif (current main build)
# -----------------------------
calc/enchain.o: calc/var_static.c.inc calc/enchain.c
	@echo "Compilation des fichiers C issus des fichiers M (CC=$(CC), CFLAGS=$(CFLAGS))"
	cd calc && $(CC) $(CFLAGS) -c \
	irdata.c enchain.c var.c contexte.c famille.c revenu.c revcor.c penalite.c variatio.c tableg??.c restitue.c \
	chap-*.c res-ser*.c coc*.c coi*.c horiz*.c

# Ancien makefile racine du backend
# -----------------------------
### list existing C file from M equivalent
M_C_FILES=$(filter-out tgv%.c err%.c,$(notdir $(SOURCE_FILES:.m=.c)))

calc/old_ir_%.o: calc/old_ir_%.c calc/var_static.c.inc
	cd calc && $(CC) -c $(<F) \
	$(M_C_FILES) \
	contexte.c famille.c penalite.c restitue.c revcor.c \
	revenu.c variatio.c var.c irdata.c

# Ancien makefile backend_tests
# -----------------------------
calc/ir_%.o: export AFL_DONT_OPTIMIZE=1
calc/ir_%.o: calc/ir_%.c calc/var_static.c.inc
	cd calc && $(CC) -c $(<F) \
	$(M_C_FILES) \
	contexte.c famille.c penalite.c restitue.c revcor.c \
	revenu.c variatio.c var.c irdata.c

calc/test_harness.o: calc/ir_tests.c
	$(CC) -O3 -c -o $@ test_harness.c

#################################################
# Final targets
##################################################

# Ml_primitif (current main build)
# -----------------------------
cal: calc/enchain.o
	@echo "Compilation de la calculette primitive (OCAMLFLAGS=$(OCAMLFLAGS), WITH_BISECT=$(WITH_BISECT))"
ifeq ($(WITH_BISECT), 1)
	ocamlopt -cc $(CC) -ccopt -std=c99 -ccopt -fno-common \
	-I $(BISECT_PATH)/common -I $(BISECT_PATH)/runtime \
	-ppx "$(BISECT_PATH)/ppx.exe --as-ppx" \
	unix.cmxa bisect_common.cmxa bisect.cmxa ./calc/*.o stubs.c common.ml m.ml read_test.ml main.ml -o cal
else
	ocamlopt -cc $(CC) $(OCAMLFLAGS) -ccopt -std=c99 -ccopt -fno-common \
	unix.cmxa ./calc/*.o stubs.c common.ml m.ml read_test.ml main.ml -o cal
endif

# Ancien makefile racine du backend
# -----------------------------
# Même en réutilisant le fichier .c pour les tests 2018 présent dans le dossier parent
# ça ne fonctinne plus depuis le commit initial de ce backend, et probablement depuis plus de deux ans sur
# l'ancien backend C non plus, à cause de l'absence de header correspondant.
# To call this target, use "make run_<name of file in m_spec/ without extension>.exe"
run_%.exe: calc/old_ir_%.o run_%.o
	cd calc && $(CC) -lm -o $@ $(^F) \
	$(M_C_FILES:.c=.o) \
	contexte.o famille.o penalite.o restitue.o revcor.o \
	revenu.o variatio.o var.o irdata.o

# Ancien makefile backend_tests
# -----------------------------
test_harness.exe: calc/ir_tests.o calc/test_harness.o
	cd calc && $(CC) -fPIE -lm -o ../$@ $(^F) \
	$(M_C_FILES:.c=.o) \
	contexte.o famille.o penalite.o restitue.o revcor.o \
	revenu.o variatio.o var.o irdata.o

# Ne passe plus les tests 2020 en l'état (mais c'était déjà le cas avant la réorganisation.)
run_test_harness: test_harness.exe FORCE
	ulimit -s 32768; \
	./$< $(TESTS_DIR)

##################################################
# Running the tests
##################################################

# Ml_primitif (current main build)
# -----------------------------
run_tests: cal
	 ./cal ${TEST_FILES}

backend_tests: FORCE run_tests

##################################################
# Cleaners
##################################################

clean:
	@echo "Nettoyage des fichiers binaires intermédiaires"
	rm -f calc/*.o
	rm -f *.o
	rm -f *.cm*

# To keep in mind
# rm -f $(M_C_FILES) $(M_C_FILES:.c=.o) \
	contexte.* famille.* penalite.* restitue.* revcor.* \
	revenu.* tableg*.* tablev.* variatio.* var.* \
	conf.h annee.h desc.h desc_inv.h
cleanc:
	@echo "Nettoyage des sources C"
	rm -f calc/*.[ch]
	rm -f calc/*.inc

cleanexe:
	@echo "Nettoyage des exécutables"
	rm -f cal
	rm -f *.exe

cleantmp:
	@echo "Nettoyage des fichiers temporaires"
	rm -f *.tmp

cleanall: clean cleanc cleanexe
	rm -f vars.txt
	rm -f tests.m_spec


##################################################
# Utilities
##################################################
# Really, we can hope to be cutting edge enough for .PHONY
FORCE:

#########################################################################
# Broken legacy from old C backend (used for perf and fuzzing)
#########################################################################
# N'existe plus depuis la suppression de l'ancien backend C
perf_harness.exe: ir_tests.o perf_harness.o ../m_value.o
	$(CC) -fPIE -lm -o $@ $^ \
	$(M_C_FILES:.c=.o) \
	contexte.o famille.o penalite.o restitue.o revcor.o \
	revenu.o variatio.o var.o irdata.o

run_perf: perf_harness.exe FORCE
	ulimit -s 32768; \
	time ./$< $(ONE_TEST_FILE)


FUZZ_M_SPEC=tests

fuzz_harness.exe: CC=afl-$(CC)
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
include ../../../Makefile.include

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
ifeq ($(C_COMPILER), clang)
	COMPILER_SPECIFIC_CFLAGS=-O2 -fbracket-depth=2048
else ifeq ($(C_COMPILER), gcc)
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

##################################################
# Generating C files from M sources with Mlang
##################################################

calc/var_static.c.inc:
	mkdir -p calc
	@echo "Copie des fichiers C statiques"
	cp static/* calc/

# Ml_primitif (current main build)
# -----------------------------
# Note: compilation avec -g et -k1 OBLIGATOIREMENT
calc/enchain.c: $(SOURCE_FILES)
	mkdir -p calc
	@echo "Compilation des fichiers M avec Mlang (YEAR=$(YEAR) MPP_FUNCTION=$(MPP_FUNCTION))"
	@$(MLANG) \
		--dgfip_options=-g,-k1,-Ailiad,-m$(YEAR),$(DGFIPFLAGS)\
		--backend dgfip_c --output $@ \
		$(SOURCE_FILES) $(QUIET)
	@echo "Compilation terminée"

##################################################
# Compiling the generated C
##################################################

# Ml_primitif (current main build)
# -----------------------------
calc/enchain.o: calc/var_static.c.inc calc/enchain.c
	@echo "Compilation des fichiers C issus des fichiers M (CC=$(C_COMPILER), CFLAGS=$(CFLAGS))"
	cd calc && $(C_COMPILER) $(CFLAGS) -c \
	irdata.c enchain.c var.c contexte.c famille.c revenu.c revcor.c penalite.c variatio.c tableg01.c restitue.c \
	chap-*.c res-ser*.c coc*.c coi*.c horiz*.c


#################################################
# Final targets
##################################################

# Ml_primitif (current main build)
# -----------------------------
cal: calc/enchain.o
	@echo "Compilation de la calculette primitive (OCAMLFLAGS=$(OCAMLFLAGS), WITH_BISECT=$(WITH_BISECT))"
ifeq ($(WITH_BISECT), 1)
	ocamlopt -cc $(C_COMPILER) -ccopt -std=c99 -ccopt -fno-common \
	-I $(BISECT_PATH)/common -I $(BISECT_PATH)/runtime \
	-ppx "$(BISECT_PATH)/ppx.exe --as-ppx" \
	unix.cmxa bisect_common.cmxa bisect.cmxa ./calc/*.o stubs.c common.ml m.ml read_test.ml main.ml -o cal
else
	ocamlopt -cc $(C_COMPILER) $(OCAMLFLAGS) -ccopt -std=c99 -ccopt -fno-common \
	unix.cmxa ./calc/*.o stubs.c common.ml m.ml read_test.ml main.ml -o cal
endif

##################################################
# Running the tests
##################################################

# Ml_primitif (current main build)
# -----------------------------
run_tests: cal
	 ./cal ${TEST_FILES}

##################################################
# Cleaners
##################################################

clean:
	@echo "Nettoyage des fichiers binaires intermédiaires"
	rm -f calc/*.o
	rm -f *.o
	rm -f *.cm*

cleanc:
	@echo "Nettoyage des sources C"
	rm -f calc/*.[ch]
	rm -f calc/*.inc

cleanexe:
	@echo "Nettoyage des exécutables"
	rm -f cal

cleanall: clean cleanc cleanexe

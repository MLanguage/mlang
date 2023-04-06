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
COMMON_CFLAGS=-std=c89 -pedantic
ifeq ($(CC), clang)
    COMPILER_SPECIFIC_CFLAGS=-O2
else ifeq ($(CC), gcc)
    COMPILER_SPECIFIC_CFLAGS=-O1
endif
CFLAGS=$(COMMON_CFLAGS) $(COMPILER_SPECIFIC_CFLAGS)

# Options pour le compilateur OCaml
OCAMLFLAGS=
#OCAMLFLAGS="-g -inline 0"
# Pour instrumenter la couverture de code, il est nécessaire d'installer le paquet OCaml bisect_ppx
# Utiliser l'indicateur WITH_BISECT=1 pour activer l'instrumentation nécessaire à l'analyse de la couverture de code
# lors des étapes de compilation.
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
# Generating C files from M sources with Mlang
##################################################

calc/var_static.c.inc calc/enchain_static.c.inc calc/desc_static.h.inc calc/const.h calc/dbg.h calc/irdata.c calc/irdata.h &:
	mkdir -p calc
	@echo "Copie des fichiers C statiques depuis $(STATIC_DIR)"
	cp $(STATIC_DIR)* calc/

# Ml_primitif (current main build)
# -----------------------------
# Note : compilation avec -g et -kN (N=1..4) OBLIGATOIREMENT
# Note : la compilation mainframe en 2022 a pu se faire avec -rXMk4,-b0,-Ailiad ?
calc/enchain.c: $(SOURCE_FILES)
	mkdir -p calc
	@echo "Compilation des fichiers M avec Mlang (YEAR=$(YEAR), MPP_FUNCTION=$(MPP_FUNCTION), DGFIPFLAGS=$(DGFIPFLAGS))"
	@$(MLANG) \
		--dgfip_options=-g,-k4,-Ailiad,-m$(YEAR),$(DGFIPFLAGS)\
		--backend dgfip_c --output $@ \
		$(SOURCE_FILES) $(QUIET)
	@echo "Compilation terminée"

#################################################
# Compiling the generated C
##################################################

# list existing C file from M equivalent (Useful idea to improve upon)
M_C_FILES=$(filter-out tgv%.c err%.c,$(notdir $(SOURCE_FILES:.m=.c)))

# Ml_primitif (current main build)
# -----------------------------
calc/enchain.o: calc/var_static.c.inc calc/enchain.c
	@echo "Compilation des fichiers C issus des fichiers M (CC=$(CC), CFLAGS=$(CFLAGS))"
	cd calc && $(CC) $(CFLAGS) -c \
	irdata.c enchain.c var.c contexte.c famille.c revenu.c revcor.c penalite.c variatio.c tableg??.c restitue.c \
	chap-*.c res-ser*.c coc*.c coi*.c horiz*.c

#################################################
# Final targets
##################################################

# TODO: use &: when upgraded to GNU Make 4.3+
calc/stubs.c calc/common.ml calc/m.ml calc/main.ml calc/read_test.ml :
	echo "Copie des sources du pilote depuis $(DRIVER_DIR)"
	cp $(DRIVER_DIR)* calc/

# Ml_primitif (current main build)
# -----------------------------
cal: calc/enchain.o calc/stubs.c
	@echo "Compilation de la calculette primitive (OCAMLFLAGS=$(OCAMLFLAGS), WITH_BISECT=$(WITH_BISECT))"
ifeq ($(WITH_BISECT), 1)
	cd calc && ocamlopt -cc $(CC) -ccopt -std=c99 -ccopt -fno-common \
	-I $(BISECT_PATH)/common -I $(BISECT_PATH)/runtime \
	-ppx "$(BISECT_PATH)/ppx.exe --as-ppx" \
	unix.cmxa bisect_common.cmxa bisect.cmxa *.o stubs.c common.ml m.ml read_test.ml main.ml -o cal
else
	cd calc && ocamlopt -cc $(CC) $(OCAMLFLAGS) -ccopt -std=c99 -ccopt -fno-common \
	unix.cmxa *.o stubs.c common.ml m.ml read_test.ml main.ml -o ../cal
endif

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
	@echo "Nettoyage des sources"
	rm -f calc/*.[ch]
	rm -f calc/*.inc
	rm -f calc/version.*
	rm -f calc/*.ml

cleanexe:
	@echo "Nettoyage des exécutables"
	rm -f cal
	rm -f *.exe

cleantmp:
	@echo "Nettoyage des fichiers temporaires"
	rm -f *.tmp

cleanres:
	@echo "Nettoyage des résultats de test"
	rm -f *.output/*.tgv

cleanall: clean cleanc cleanexe cleanres
	rm -f vars.txt
	rm -f tests.m_spec

##################################################
# Utilities
##################################################
# Really, we can hope to be cutting edge enough for .PHONY
FORCE:

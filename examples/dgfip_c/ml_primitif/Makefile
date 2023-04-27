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

# Options supplémentaires pour le backend Mlang/DGFiP spécifiques à la cible
# Par défaut ici pour l'autotest Mlang et l'intégration continue.
# Note : compilation avec -g (informations pour tests et débogage) OBLIGATOIREMENT
# Note : compilation avec -O (optimisation par inlining)
DGFIP_TARGET_FLAGS?=-g,-O
# Options supplémentaires pour le backend Mlang/DGFiP communes à tous les build :
# Note: les flags -Ailiad, -g et -k sont déjà définis
# -Ailiad : ensemble de règles sélectionné ("Application ciblée")
# -m : millésime de calculette compilé (année des revenus taxés)
# -kN : nombre de segment pour la table de débogage.
#        N=1 à 4 pour le pilotage Mlang ml_primitif
#        N=4 pour utiliser le pilotage DGFiP.
# -X : génération de fonctions d'extraction globale dans l'interface,
#      bouclant sur la table des variables restituables (IN_init_extraction).
DGFIP_COMMON_FLAGS=-Ailiad,-m$(YEAR),-k4,-X

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(MLANG_DGFIP_C_OPTS)
QUIET=>/dev/null # Uncomment to suppress output

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

# TODO: use &: when upgraded to GNU Make 4.3+
calc/var_static.c.inc calc/enchain_static.c.inc calc/desc_static.h.inc calc/const.h calc/dbg.h calc/irdata.c calc/irdata.h :
	mkdir -p calc
	@echo "Copie des fichiers C statiques depuis $(STATIC_DIR)"
	cp $(STATIC_DIR)* calc/

calc/enchain.c: $(SOURCE_FILES)
	mkdir -p calc
	@echo "Compilation des fichiers M avec Mlang (MPP_FUNCTION=$(MPP_FUNCTION), DGFIP_TARGET_FLAGS=$(DGFIP_TARGET_FLAGS), DGFIP_COMMON_FLAGS=$(DGFIP_COMMON_FLAGS))"
	@$(MLANG) \
		--dgfip_options=$(DGFIP_TARGET_FLAGS),$(DGFIP_COMMON_FLAGS)\
		--backend dgfip_c --output $@ \
		$(SOURCE_FILES) $(QUIET)
	@echo "Compilation terminée"

#################################################
# Compiling the generated C
##################################################

# list existing C file from M equivalent (Useful idea to improve upon)
M_C_FILES=$(filter-out tgv%.c err%.c,$(notdir $(SOURCE_FILES:.m=.c)))

C_SOURCES_TARGETS?=calc/var_static.c.inc calc/enchain.c

calc/enchain.o: $(C_SOURCES_TARGETS)
	@echo "Compilation des fichiers C issus des fichiers M (CC=$(CC), CFLAGS=$(CFLAGS))"
	cd calc && $(CC) $(CFLAGS) -c \
	*.c
#	irdata.c enchain.c var.c contexte.c famille.c revenu.c revcor.c penalite.c variatio.c tableg??.c restitue.c \
	chap-*.c res-ser*.c coc*.c coi*.c horiz*.c

#################################################
# Final targets
##################################################

DRIVER_TARGETS:=$(foreach file,$(DRIVER_FILES),calc/$(file))
# TODO: use &: when upgraded to GNU Make 4.3+
$(DRIVER_TARGETS) :
	echo "Copie des sources du pilote depuis $(DRIVER_DIR)"
	cp $(DRIVER_DIR)* calc/

# Ml_primitif (current main build)
# -----------------------------
cal: calc/enchain.o $(DRIVER_TARGETS)
	@echo "Compilation de la calculette primitive (OCAMLFLAGS=$(OCAMLFLAGS), WITH_BISECT=$(WITH_BISECT))"
ifeq ($(WITH_BISECT), 1)
	cd calc && ocamlopt -cc $(CC) -ccopt -std=c99 -ccopt -fno-common \
	-I $(BISECT_PATH)/common -I $(BISECT_PATH)/runtime \
	-ppx "$(BISECT_PATH)/ppx.exe --as-ppx" \
	unix.cmxa bisect_common.cmxa bisect.cmxa *.o $(DRIVER_FILES) -o cal
else
	cd calc && ocamlopt -cc $(CC) $(OCAMLFLAGS) -ccopt -std=c99 -ccopt -fno-common \
	unix.cmxa *.o $(DRIVER_FILES) -o ../cal
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
	rm -f calc/*.cm*

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

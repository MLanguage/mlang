include ../../../Makefile.include

ifeq ($(TEST_FILTER), 1)
    TEST_FILES=$(TESTS_DIR)[A-Z]*
else
    TEST_FILES=$(TESTS_DIR)*
endif

MLANG_BIN=dune exec --no-print-director ../../../src/main.exe --
PRECISION?=double
TEST_ERROR_MARGIN?=0.
#MPP_FUNCTION?=compute_double_liquidation_pvro

MPP_FILE=../../../mpp_specs/dgfip_base.mpp
MPP_FUNCTION=dgfip_calculation

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(MPP_FILE) \
	--mpp_function=$(MPP_FUNCTION)

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

C_OPTIONS="-fbracket-depth=2048"
##################################################
# Initialisation
##################################################

calc:
	mkdir $@

##################################################
# C sources generation
##################################################

calc/var_static.c.inc: calc
	@echo "Copie des fichiers C statiques"
	cp static/* calc/

calc/enchain.c: calc/var_static.c.inc
	@echo "Compilation des fichiers M avec Mlang (YEAR=$(YEAR) $(MPP_FUNCTION)"
	$(MLANG) \
		--dgfip_options=-X,-O,-g,-k1,-b0,-Ailiad,-m$(YEAR)\
		--backend dgfip_c --output $@ \
		$(SOURCE_FILES)

##################################################
# Building "La Calculette"
##################################################

calc/enchain.o: calc/enchain.c
	cd calc && $(C_COMPILER) -std=c89 -pedantic -O2 -c \
	irdata.c enchain.c var.c contexte.c famille.c revenu.c revcor.c penalite.c variatio.c tableg01.c restitue.c \
	chap-*.c res-ser*.c coc*.c coi*.c horiz*.c

prim: calc/enchain.o
	ocamlopt -cc $(C_COMPILER) -ccopt -std=c99 -ccopt -fno-common unix.cmxa ./calc/*.o stubs.c common.ml m.ml read_test.ml main.ml -o prim

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
	rm -f prim

cleanall: clean cleanc cleanexe

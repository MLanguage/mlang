###############################################
#                  C backend                  #
###############################################

MLANG_DGFIP_C_OPTS=--mpp_function=$(MPP_FUNCTION_BACKEND)

# Options supplémentaires pour le backend Mlang/DGFiP spécifiques à la cible
# Par défaut ici pour l'autotest Mlang et l'intégration continue.
# Note : compilation avec -g et -k OBLIGATOIREMENT
#  -g  : informations pour tests et débogage
#  -kN : nombre de segment pour la table de débogage.
#        N=1 à 4 pour le pilotage Mlang ml_primitif
#        N=4 pour utiliser le pilotage DGFiP.
# Note : compilation avec -O (optimisation par inlining)
DGFIP_TARGET_FLAGS?=-g,-O,-k4
# Options supplémentaires pour le backend Mlang/DGFiP communes à tous les build :
# Note: les flags -Ailiad, -g et -k sont déjà définis
# -Ailiad : ensemble de règles sélectionné ("Application ciblée")
# -m : millésime de calculette compilé (année des revenus taxés)
# -X : génération de fonctions d'extraction globale dans l'interface,
#      bouclant sur la table des variables restituables (IN_init_extraction).
DGFIP_COMMON_FLAGS=-m$(YEAR),-X

MLANG_DGFIP=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(MLANG_DGFIP_C_OPTS)

QUIET=>/dev/null # Uncomment to suppress output

# Options pour le compilateur OCaml
OCAMLFLAGS=
#OCAMLFLAGS="-g -inline 0"
# Pour instrumenter la couverture de code, il est nécessaire d'installer
# le paquet OCaml bisect_ppx
# Utiliser l'indicateur WITH_BISECT=1 pour activer l'instrumentation nécessaire
# à l'analyse de la couverture de code lors des étapes de compilation.
WITH_BISECT?=0
ifeq ($(WITH_BISECT), 1)
  BISECT_PATH:=$(shell ocamlfind query bisect_ppx)
  ifeq ($(BISECT_PATH),)
    $(error $(BISECT_PATH) Pour instrumenter la couverture de code, il faut \
      installer le paquet OCaml bisect_ppx)
  endif
endif

##################################################
# Building the backend
##################################################

ifeq ($(call is_in,$(DGFIP_DIR)),1)
calc_dir: FORCE
	mkdir -p calc
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
$(SOURCE_FILES) $(SOURCE_EXT_FILES): FORCE
endif

ifeq ($(call is_in,$(DGFIP_DIR)/calc),1)
%.o: %.c
	$(CC) $(BACKEND_CFLAGS) -c $< -o $@
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
calc/mlang.h: $(SOURCE_FILES) $(SOURCE_EXT_FILES) | calc_dir
	$(call check_in,$(DGFIP_DIR),$@)
	@echo "Compilation des fichiers M avec Mlang:"
	@echo "  MPP_FUNCTION=$(MPP_FUNCTION_BACKEND)"
	@echo "  DGFIP_TARGET_FLAGS=$(DGFIP_TARGET_FLAGS)"
	@echo "  DGFIP_COMMON_FLAGS=$(DGFIP_COMMON_FLAGS)"
	@$(MLANG_DGFIP) \
	  --income-year=$(YEAR) \
	  --comparison_error_margin=$(COMPARISON_ERROR_MARGIN) \
	  --dgfip_options=$(DGFIP_TARGET_FLAGS),$(DGFIP_COMMON_FLAGS) \
	  --backend dgfip_c \
	  --output calc/enchain.c \
	  $(SOURCE_FILES) $(SOURCE_EXT_FILES) $(QUIET)
	cd calc && rm -f $(DRIVER_FILES)
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
info_c:
	$(call check_in,$(DGFIP_DIR),$@)
	@echo "Compilation des fichiers C issus des fichiers M:"
	@echo "  CC=$(CC)"
	@echo "  BACKEND_CFLAGS=$(BACKEND_CFLAGS)"
# If calc/ directory was not cleaned between builds, some driver C files are
# present that mustn't be compiled at this stage.
# We use find to scan the directory and keep every .c except files which are in
# DRIVER_FILES list.
# To exclude files string1, string2 and string3 from the match, the syntax is
# '-not \( -name "string1" -o -name "string2" -o -name "string3" \)'  so we use
# string substitution to replace the space separator by the '" -o -name "'
# between the file names.
# $() is an empty variable, canonical way to force make to take into account
# the space as the string to be replaced.
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
calc_o:
	$(eval override C_FILES:=$(shell cd calc && ls -1 *.c) $(ADDITIONAL_C_SOURCES_TARGETS))
	@for I in $(C_FILES:.c=.o) ; \
	do \
	  $(MAKE_DGFIP_CALC) $$I || exit; \
	done
endif

# Build derivative file lists
DRIVER_TARGETS:=$(foreach file,$(DRIVER_FILES),calc/$(file))
DRIVER_TEMP:=$(DRIVER_FILES:.ml=.o)
DRIVER_OBJECT_FILES:=$(DRIVER_TEMP:.c=.o)
# TODO: use &: when upgraded to GNU Make 4.3+

ifeq ($(call is_in,$(DGFIP_DIR)),1)
$(DRIVER_TARGETS):
	@echo "Copie des sources du pilote depuis $(DRIVER_DIR)"
	cp $(DRIVER_DIR)/* calc/
endif

# Ml_primitif (current main build)
# -----------------------------
ifeq ($(call is_in,$(DGFIP_DIR)),1)
cal: $(DRIVER_TARGETS)
	@echo "Compilation de la calculette primitive:"
	@echo "  OCAMLFLAGS=$(OCAMLFLAGS)"
	@echo "  WITH_BISECT=$(WITH_BISECT)"
	cd calc && rm -f $(DRIVER_OBJECT_FILES)
ifeq ($(WITH_BISECT), 1)
	cd calc && ocamlopt -cc $(CC) -ccopt -std=c99 -ccopt -fno-common \
	-I $(BISECT_PATH)/common -I $(BISECT_PATH)/runtime \
	-ppx "$(BISECT_PATH)/ppx.exe --as-ppx" \
	unix.cmxa bisect_common.cmxa bisect.cmxa *.o $(DRIVER_FILES) -o cal
else
	cd calc && ocamlopt -cc $(CC) $(OCAMLFLAGS) -ccopt -std=c99 -ccopt -fno-common \
	unix.cmxa *.o $(DRIVER_FILES) -o ../cal
endif
	@echo "Compilation terminée"
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
dgfip_c_backend: FORCE | build calc/mlang.h
else
dgfip_c_backend: FORCE
	$(call make_in,$(DGFIP_DIR),$@)
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
compile_dgfip_c_backend: FORCE | dgfip_c_backend info_c calc_o cal
else
compile_dgfip_c_backend: FORCE
	$(call make_in,$(DGFIP_DIR),$@)
endif

##################################################
# Testing the backend
##################################################

ifeq ($(call is_in,$(DGFIP_DIR)),1)
backend_tests: compile_dgfip_c_backend
	./cal ${TEST_FILES}
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
test_dgfip_c_backend: FORCE backend_tests
else
test_dgfip_c_backend: FORCE
	$(call make_in,$(DGFIP_DIR),$@)
endif


##################################################
# Cleaners
##################################################

ifeq ($(call is_in,$(DGFIP_DIR)),1)
clean_backend: FORCE
	@echo "Nettoyage des fichiers binaires intermédiaires"
	rm -f calc/*.o
	rm -f calc/*.cm*
else
clean_backend: FORCE
	$(call make_in,$(DGFIP_DIR),$@)
endif

# To keep in mind
# rm -f $(M_C_FILES) $(M_C_FILES:.c=.o) \
	contexte.* famille.* penalite.* restitue.* revcor.* \
	revenu.* tableg*.* tablev.* variatio.* var.* \
	conf.h annee.h desc.h desc_inv.h
ifeq ($(call is_in,$(DGFIP_DIR)),1)
clean_backend_c: FORCE
	@echo "Nettoyage des sources"
	rm -f calc/*.[ch]
	rm -f calc/*.inc
	rm -f calc/version.*
	rm -f calc/*.ml
	if [ -d calc/zos ] ; \
	then \
	  rm -f calc/zos/* ; \
	  rmdir calc/zos ; \
	fi
else
clean_backend_c: FORCE
	$(call make_in,$(DGFIP_DIR),$@)
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
clean_backend_exe: FORCE
	@echo "Nettoyage des exécutables"
	rm -f cal
	rm -f *.exe
	rm -f *.so
	rm -f *.tar
	rm -f *.tar.gz
else
clean_backend_exe: FORCE
	$(call make_in,$(DGFIP_DIR),$@)
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
clean_backend_tmp: FORCE
	@echo "Nettoyage des fichiers temporaires"
	rm -f *.tmp
else
clean_backend_tmp: FORCE
	$(call make_in,$(DGFIP_DIR),$@)
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
clean_backend_res: FORCE
	@echo "Nettoyage des résultats de test"
	rm -f *.output/*.tgv
else
clean_backend_res: FORCE
	$(call make_in,$(DGFIP_DIR),$@)
endif

ifeq ($(call is_in,$(DGFIP_DIR)),1)
clean_backend_all: FORCE clean_backend clean_backend_c clean_backend_exe clean_backend_res
	rm -f vars.txt
	rm -f tests.m_spec
else
clean_backend_all: FORCE
	$(call make_in,$(DGFIP_DIR),$@)
endif


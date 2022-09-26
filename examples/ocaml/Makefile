include ../../Makefile.include

########
# USAGE: specifiying parameters of the generated "calculette"
########
# Specifying a mpp file and its main function:
# make ir.ml MPP_FILE=../../mpp_specs/dgfip_base.mpp MPP_FUNCTION=verif_calcul_primitive_raw
# Using the default m_spec file:
# make ir.ml TAKE_MSPEC=true

MLANG_BIN=dune exec --no-print-director ../../src/main.exe --
MPP_FUNCTION=compute_double_liquidation_pvro
M_SPEC_FILE=$(SELF_DIR)/m_specs/tests_$(YEAR).m_spec

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(MPP_FILE) \
	--mpp_function=$(MPP_FUNCTION)

MLANG_MSPEC=\
	--function_spec=$(M_SPEC_FILE)

ifdef TAKE_MSPEC
MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG) $(MLANG_MSPEC)
SPEC_DEP=$(MPP_FILE) $(M_SPEC_FILE)
else
MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)
SPEC_DEP=$(MPP_FILE)
endif

.PHONY : clean cleangen cleancalc cleanstat cleantest cleanresult

clean: cleancalc cleanstat cleanresult

cleangen:
	rm -f ir.ml
cleancalc: cleangen
	rm -f ir.cmi ir.cmx ir.o ir.cmo ir.exe ir.bc
cleantest:
	rm -f test_harness.cmi test_harness.cmx test_harness.o test_harness.cmo test.exe test.bc
cleanstat: cleantest
	rm -f mvalue.cmi mvalue.cmx mvalue.o mvalue.cmo
cleanresult:
	rm -f results/*
cleanparser:
	$(MAKE) -C parser/ clean

##################################################
# Implicit rules for OCaml modules
##################################################

%.cmo: %.ml
	ocamlc.opt -c $(DEBUG_FLAG) $^

%.cmx: %.ml
	ocamlopt -c $(DEBUG_FLAG) $^

##################################################
# Generating and running OCaml files from Mlang
##################################################

# Generating OCaml files (MLang)
ir.ml: $(SPEC_DEP)
	$(MLANG) \
		--backend ocaml --output ir.ml \
		$(SOURCE_FILES)

test_harness.cmo : test_harness.ml
	ocamlc.opt -c $(DEBUG_FLAG) -I parser $^

.INTERMEDIATE : test_harness.cmo test_harness.cmi test_harness.o test_harness.cmx
# Compiling bytecode
types_module.cmo: 
	$(MAKE) -C parser/ types_module.cmo
test_lexer.cmo :
	$(MAKE) -C parser/ test_lexer.cmo
test_parser.cmo :
	$(MAKE) -C parser/ test_parser.cmo
fip.cmo :
	$(MAKE) -C parser/ fip.cmo

test.bc: types_module.cmo test_lexer.cmo test_parser.cmo fip.cmo mvalue.cmo ir.cmo test_harness.cmo
	ocamlc.opt $(DEBUG_FLAG) -o $@ -I parser unix.cma $^

# Compiling native code
test.exe: parser/fip.cmx mvalue.cmx ir.cmx test_harness.cmx
	ocamlopt $(DEBUG_FLAG) -o $@ unix.cmxa $^

# Running test suite
run: test.bc
	./test.bc "multi" $(TESTS_DIR) "results/y$(YEAR)"

runx: test.exe
	./test.exe "multi" $(TESTS_DIR) "results/y$(YEAR)"
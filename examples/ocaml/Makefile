include Makefile-generic-ocaml-rules.include
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

# Include parser lib directory to make its module available
OCAMLC_INCLUDE_LIST= -I parser

.PHONY : clean cleangen cleancalc cleanstat cleantest cleanresult run_tests

clean: cleancalc cleanstat cleanresult cleanparser

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
# Generating and running OCaml files from Mlang
##################################################

# Generating OCaml files (MLang)
ir.ml: $(SPEC_DEP)
	$(MLANG) \
		--backend ocaml --output ir.ml \
		$(SOURCE_FILES)

.INTERMEDIATE : test_harness.cmo test_harness.cmi test_harness.o test_harness.cmx
# Compiling bytecode
types_module.cmo test_lexer.cmo test_parser.cmo fip.cmo:
	$(MAKE) -C parser/ fip.cmo

test.bc: types_module.cmo test_lexer.cmo test_parser.cmo fip.cmo mvalue.cmo ir.cmo test_harness.cmo
	ocamlc.opt $(DEBUG_FLAG) -o $@ $(OCAMLC_INCLUDE_LIST) unix.cma $^

# Compiling native code
types_module.cmx test_lexer.cmx test_parser.cmx fip.cmx:
	$(MAKE) -C parser/ fip.cmx

test.exe: types_module.cmx test_lexer.cmx test_parser.cmx fip.cmx mvalue.cmx ir.cmx test_harness.cmx
	ocamlopt $(DEBUG_FLAG) -o $@ $(OCAMLC_INCLUDE_LIST) unix.cmxa $^

# Running test suite
run: test.bc
	./test.bc "multi" $(TESTS_DIR) "results/y_$(YEAR)"

runfile: test.bc
	./test.bc "raw" $(FILE) "results/f_$(notdir $(FILE))"

runx: test.exe
	./test.exe "multi" $(TESTS_DIR) "results/y_$(YEAR)"

# run_tests uses an empty string to disable the file output (output is enabled on stdout).
run_tests: test.exe
	./test.exe "multi" $(TESTS_DIR) ""

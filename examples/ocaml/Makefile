include ../../Makefile.include

MLANG_BIN=dune exec --no-print-director ../../src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(MPP_FILE) \
	--mpp_function=compute_double_liquidation_pvro

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

all: backend_tests $(shell find . -name "run_*.py")

clean: 
	rm -f Ir_*.ml

##################################################
# Generating and running Java files from Mlang
##################################################

.PRECIOUS: Ir_%.ml
Ir_%.ml: ../../m_specs/%.m_spec
	$(MLANG) \
		--backend ocaml --output $@ \
		--function_spec $^ \
		$(SOURCE_FILES)

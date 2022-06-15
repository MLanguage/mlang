include ../../Makefile.include

MLANG_BIN=dune exec --no-print-director ../../src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(MPP_FILE) \
	--mpp_function=compute_double_liquidation_pvro

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

clean: 
	rm -f ir_*.ml ir_*.o ir_*.cmi ir_*.cmx ir_*.exe ir_*.cmo ir_*.out mvalue.cmi mvalue.cmo mvalue.out mvalue.o mvalue.cmx

##################################################
# Generating and running OCaml files from Mlang
##################################################

.PRECIOUS: ir_%.ml
ir_%.ml: ../../m_specs/%.m_spec
	$(MLANG) \
		--backend ocaml --output $@ \
		--function_spec $^ \
		$(SOURCE_FILES)

ir_%.exe: ir_%.ml
	ocamlopt -o $@ mvalue.ml $^

ir_%.bc: ir_%.ml
	ocamlc.opt -g -o $@ mvalue.ml $^

mvalue.out: 
	ocamlc.opt -c mvalue.ml
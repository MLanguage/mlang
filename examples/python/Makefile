include ../../Makefile.include

MLANG_BIN=dune exec --no-print-director ../../src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(MPP_FILE) \
	--mpp_function=compute_double_liquidation_pvro

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

##################################################
# Generating and running Python files from Mlang
##################################################

ir_%.py: ../../m_specs/%.m_spec
	$(MLANG) \
		--backend python --output $@ \
		--function_spec $^ \
		$(SOURCE_FILES)

run_%.py: ir_%.py FORCE
	python3 $@

.SECONDARY: ir_%.py
.PRECIOUS: ir_%.py

##################################################
# Running the tests
##################################################


backend_tests: FORCE
	$(MAKE) -C backend_tests all_tests


clean:
	$(MAKE) -C backend_tests clean
	rm -f ir_*.py
	rm -rf __pycache__

FORCE:

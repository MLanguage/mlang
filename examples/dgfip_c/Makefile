include ../../Makefile.include

MPP_FILE=../../../mpp_specs/dgfip_base.mpp

OPTIMIZE_FLAG=-O

MLANG_BIN=dune exec --no-print-director ../../src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=$(MPP_FILE) \
	--mpp_function=dgfip_calculation

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

##################################################
# Generating C files from Mlang
##################################################

ir_%.c: ../../m_specs/%.m_spec $(SOURCE_FILES)
	$(MLANG) \
		--backend dgfip_c --output $@ \
		--function_spec $< \
		$(SOURCE_FILES)

.SECONDARY: ir_%.c ir_%.h
.PRECIOUS: ir_%.c ir_%.h

##################################################
# Compiling the generated C
##################################################

C_COMPILER=clang

ir_%.o: ir_%.c
	$(C_COMPILER) -fbracket-depth=2048 $(C_OPT) -c $< m_value.c

%.o: %.c
	$(C_COMPILER) -c $<

##################################################
# Final targets
##################################################

# To call this target, use "make run_<name of file in m_spec/ without extension>.exe"
run_%.exe: ir_%.o run_%.o m_value.o
	$(C_COMPILER) -lm -o $@ $^

##################################################
# Running the tests
##################################################

backend_tests: FORCE
	$(MAKE) -C backend_tests run_tests

clean:
	$(MAKE) -C backend_tests clean
	rm -f ir_*.c ir_*.h *.o *.exe *.tmp

FORCE:

include ../../Makefile.include

MLANG_BIN=dune exec --no-print-director ../../src/main.exe --

MLANG_DEFAULT_OPTS=\
	--display_time --debug \
	--mpp_file=../../mpp_specs/dgfip_base.mpp \
	--mpp_function=dgfip_calculation

MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)

##################################################
# Generating C files from Mlang
##################################################

ir_%.c: ../../m_specs/%.m_spec $(SOURCE_FILES)
	$(MLANG) \
		--dgfip_options=-rXMk4,-b0,-Ailiad,-m$(YEAR)\
		--backend dgfip_c --output $@ \
		$(SOURCE_FILES)

.SECONDARY: ir_%.c ir_%.h
.PRECIOUS: ir_%.c ir_%.h

##################################################
# Compiling the generated C
##################################################

ifeq ($(C_COMPILER), clang)
    F_BRACKET_OPT=-fbracket-depth=2048
else
    F_BRACKET_OPT=
endif

ir_%.o: ir_%.c
	$(C_COMPILER) $(F_BRACKET_OPT) $(C_OPT) -c $< \
	contexte.c famille.c penalite.c restitue.c revcor.c \
	revenu.c variatio.c var.c irdata.c

%.o: %.c
	$(C_COMPILER) -c $<

##################################################
# Final targets
##################################################

# To call this target, use "make run_<name of file in m_spec/ without extension>.exe"
run_%.exe: ir_%.o run_%.o
	$(C_COMPILER) -lm -o $@ $^ \
	contexte.o famille.o penalite.o restitue.o revcor.o \
	revenu.o variatio.o var.o irdata.o


##################################################
# Running the tests
##################################################

backend_tests: FORCE
	$(MAKE) -C backend_tests run_tests

clean:
	$(MAKE) -C backend_tests clean
	rm -f ir_*.c ir_*.h *.o *.exe *.tmp \
	contexte.* famille.* penalite.* restitue.* revcor.* \
	revenu.* tableg*.* tablev.* variatio.* var.* \
	conf.h annee.h desc.h desc_inv.h

FORCE:

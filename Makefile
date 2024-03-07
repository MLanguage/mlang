# Check Makefile.config.template if you want to override some of the flags
# in this Makefile.

ifndef ROOT_DIR
  ROOT_DIR:=$(realpath $(dir $(realpath $(lastword $(MAKEFILE_LIST)))))
endif

CURR_DIR:=$(realpath $(shell pwd))

YEAR?=2020

include $(ROOT_DIR)/makefiles/functions.mk
include $(ROOT_DIR)/makefiles/variables.mk
include $(ROOT_DIR)/makefiles/mlang.mk
include $(ROOT_DIR)/makefiles/c_backend.mk

-include $(ROOT_DIR)/mlang-deps/makefiles/svn.mk
-include $(ROOT_DIR)/mlang-deps/makefiles/dgfip_backend.mk

.PHONY: default \
  create-switch init-without-switch init deps \
  format dune build build-static \
  doc \
  test tests quick_test test_one \
  calc_dir info_c calc_o dgfip_c_backend compile_dgfip_c_backend \
  backend_tests test_dgfip_c_backend \
  clean_backend clean_backend_c clean_backend_exe clean_backend_tmp clean_backend_res clean_backend_all \
  test_java_backend

FORCE:

.DEFAULT_GOAL:=default

default: FORCE build

test_java_backend: FORCE build
	@echo "\033[0;31mWarning: Java backend not supported\033[0m"
#ifeq ($(OPTIMIZE), 0)
#	@echo "\033[0;31mWarning, non-optimized Java files cannot be executed for now (too many constants for the JVM)\033[0m"
#else
#endif
#	$(MAKE) -C examples/java/ run_tests

all: FORCE quick_test tests test_dgfip_c_backend test_java_backend

clean: FORCE
	$(call make_in,$(DGFIP_DIR),clean_backend_all)
#	$(MAKE) -C examples/java clean
	rm -f doc/doc.html
	dune clean


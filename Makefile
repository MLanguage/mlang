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
  clean_backend clean_backend_c clean_backend_exe clean_backend_tmp clean_backend_res clean_backend_all

FORCE:

.DEFAULT_GOAL:=default

default: FORCE build

all: FORCE quick_test tests test_dgfip_c_backend

clean: FORCE remise_a_zero_versionnage
	$(call make_in,$(DGFIP_DIR),clean_backend_all)
	rm -f doc/doc.html
	dune clean

test:
	_build/default/src/main.exe tests/mlang/${test}.m -A test --mpp_function test_args --dgfip_options='' --run_test tests/mlang/${test}.irj --debug

c:
	_build/default/src/main.exe tests/mlang/${test}.m -A app -b dgfip_c --mpp_function target --dgfip_options='' --output output/${test}.c --debug

t20:
	dune exec src/main.exe --profile release -- -A iliad --display_time --precision double --income-year=2020 --comparison_error_margin=0.000001 --mpp_function=enchainement_primitif_interpreteur ir-calcul/sources2020m_6_5/tgvI.m ir-calcul/sources2020m_6_5/errI.m ir-calcul/sources2020m_6_5/chap-1.m ir-calcul/sources2020m_6_5/chap-2.m ir-calcul/sources2020m_6_5/chap-3.m ir-calcul/sources2020m_6_5/chap-4.m ir-calcul/sources2020m_6_5/chap-51.m ir-calcul/sources2020m_6_5/chap-52.m ir-calcul/sources2020m_6_5/chap-6.m ir-calcul/sources2020m_6_5/chap-7.m ir-calcul/sources2020m_6_5/chap-81.m ir-calcul/sources2020m_6_5/chap-82.m ir-calcul/sources2020m_6_5/chap-83.m ir-calcul/sources2020m_6_5/chap-84.m ir-calcul/sources2020m_6_5/chap-85.m ir-calcul/sources2020m_6_5/chap-86.m ir-calcul/sources2020m_6_5/chap-87.m ir-calcul/sources2020m_6_5/chap-88.m ir-calcul/sources2020m_6_5/chap-aff.m ir-calcul/sources2020m_6_5/chap-cinr.m ir-calcul/sources2020m_6_5/chap-cmajo.m ir-calcul/sources2020m_6_5/chap-cor.m ir-calcul/sources2020m_6_5/chap-ctl.m ir-calcul/sources2020m_6_5/chap-ini.m ir-calcul/sources2020m_6_5/chap-inr.m ir-calcul/sources2020m_6_5/chap-isf.m ir-calcul/sources2020m_6_5/chap-majo.m ir-calcul/sources2020m_6_5/chap-perp.m ir-calcul/sources2020m_6_5/chap-plaf.m ir-calcul/sources2020m_6_5/chap-taux.m ir-calcul/sources2020m_6_5/chap-teff.m ir-calcul/sources2020m_6_5/chap-thr.m ir-calcul/sources2020m_6_5/chap-tl.m ir-calcul/sources2020m_6_5/coc1.m ir-calcul/sources2020m_6_5/coc2.m ir-calcul/sources2020m_6_5/coc3.m ir-calcul/sources2020m_6_5/coc4.m ir-calcul/sources2020m_6_5/coc5.m ir-calcul/sources2020m_6_5/coc7.m ir-calcul/sources2020m_6_5/coi1.m ir-calcul/sources2020m_6_5/coi2.m ir-calcul/sources2020m_6_5/coi3.m ir-calcul/sources2020m_6_5/horizoc.m ir-calcul/sources2020m_6_5/horizoi.m ir-calcul/sources2020m_6_5/res-ser1.m ir-calcul/sources2020m_6_5/res-ser2.m m_ext/2020/cibles.m --run_test='tests/2020/fuzzing/fuzzer_1423.m_test' --dgfip_options='' --debug

t22:
	dune exec /home/kino/Projects/mlang/_build/default/src/main.exe -- -A iliad --display_time --debug --precision double --income-year=2022 --comparison_error_margin=0.000001 --mpp_function=enchainement_primitif_interpreteur /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/tgvI.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/errI.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-1.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-2.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-3.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-4.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-51.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-52.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-6.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-7.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-81.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-82.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-83.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-84.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-85.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-86.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-87.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-88.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-aff.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-cinr.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-cmajo.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-cor.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-ctl.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-ini.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-inr.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-isf.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-majo.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-perp.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-plaf.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-taux.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-teff.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-thr.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/chap-tl.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/coc1.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/coc2.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/coc3.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/coc4.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/coc5.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/coc7.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/coi1.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/coi2.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/coi3.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/horizoc.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/horizoi.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/res-ser1.m /home/kino/Projects/mlang/ir-calcul/sources2022m_6_1/res-ser2.m /home/kino/Projects/mlang/m_ext/2022/cibles.m /home/kino/Projects/mlang/m_ext/2022/codes_1731.m /home/kino/Projects/mlang/m_ext/2022/commence_par_5.m /home/kino/Projects/mlang/m_ext/2022/commence_par_7.m /home/kino/Projects/mlang/m_ext/2022/commence_par_H.m /home/kino/Projects/mlang/m_ext/2022/correctif.m /home/kino/Projects/mlang/m_ext/2022/main.m --run_test="tests/2022/fuzzing/fuzzer_9230.m_test" --dgfip_options=''

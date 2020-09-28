SOURCE_DIR_2018 ?= ../../../ir-calcul/sources2018m_6_7/

SOURCE_FILES_2018?=$(shell find $(SOURCE_DIR_2018) -name "*.m")

MLANG=dune exec ../../../src/main.exe --

all_tests:
	python3.7 gen_m_spec.py $(TESTS_DIR) tests.m_spec all_ins.csv
	$(MLANG) --display_time --debug --optimize \
	        --mpp_file ../../../2018.mpp --mpp_function compute_double_liquidation_pvro \
	        --backend python --output ./tests.py \
                --function_spec ./tests.m_spec \
		$(SOURCE_FILES_2018)
	python3.7 test_file.py all_ins.csv $(TESTS_DIR)

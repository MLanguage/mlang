include ../../../Makefile.include

SOURCE_FILES?=$(shell find $(SOURCE_DIR) -name "*.m")

MLANG=dune exec ../../../src/main.exe --

all_tests:
	python3 gen_m_spec.py $(TESTS_DIR) tests.m_spec all_ins.csv
	$(MLANG) --display_time --debug \
	        --mpp_file $(MPP_FILE) --mpp_function compute_double_liquidation_pvro \
	        --backend python --output ./tests.py \
                --function_spec ./tests.m_spec \
		$(SOURCE_FILES)
	python3 test_file.py all_ins.csv $(TESTS_DIR)

clean:
	rm -f tests.m_spec tests.py all_ins.csv
	rm -rf __pycache__

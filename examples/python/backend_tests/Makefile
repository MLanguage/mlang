SOURCE_DIR_2015=$(wildcard ../../../ir-calcul/sources2015m_4_6/*.m)
SOURCE_DIR_2016=$(wildcard ../../../ir-calcul/sources2016m_4_5/*.m)
SOURCE_DIR_2017=$(wildcard ../../../ir-calcul/sources2017m_6_10/*.m)
SOURCE_DIR_2018=$(wildcard ../../../ir-calcul/sources2018m_6_7/*.m)
SOURCE_DIR_2019=$(wildcard ../../../ir-calcul/sources2019m_8_0/*.m)
YEAR?=2019

ifeq ($(YEAR), 2018)
	SOURCE_DIR?=$(SOURCE_DIR_2018)
	MPP_FILE?=$(PWD)/mpp_specs/2018_6_7.mpp
	TESTS_DIR?=../../../tests/2018/fuzzing/
else ifeq ($(YEAR), 2019)
	SOURCE_DIR?=$(SOURCE_DIR_2019)
	MPP_FILE?=$(PWD)/mpp_specs/2019_8_0.mpp
	TESTS_DIR?=../../../tests/2019/fuzzing/
else
    $(error Unsupported year: $(YEAR))
endif

SOURCE_FILES?=$(shell find $(SOURCE_DIR) -name "*.m")

MLANG=dune exec ../../../src/main.exe --

all_tests:
	python3 gen_m_spec.py $(TESTS_DIR) tests.m_spec all_ins.csv
	$(MLANG) --display_time --debug --optimize \
	        --mpp_file $(MPP_FILE) --mpp_function compute_double_liquidation_pvro \
	        --backend python --output ./tests.py \
                --function_spec ./tests.m_spec \
		$(SOURCE_FILES)
	python3 test_file.py all_ins.csv $(TESTS_DIR)

clean:
	rm -f tests.m_spec tests.py all_ins.csv 
	rm -rf __pycache__
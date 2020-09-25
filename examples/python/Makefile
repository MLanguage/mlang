SOURCE_DIR_2018 ?= ../../ir-calcul/sources2018m_6_7/

SOURCE_FILES_2018=$(shell find $(SOURCE_DIR_2018) -name "*.m")

MLANG=dune exec ../../src/main.exe --

cas_general_2018:
	$(MLANG) --display_time --debug --optimize \
	        --mpp_file ../../2018.mpp --mpp_function compute_double_liquidation_pvro \
	        --backend python --output cas_general_2018.py \
                --function_spec ../../specs/all_ins_and_outs.m_spec \
		$(SOURCE_FILES_2018)

simulateur_simplifie_2018:
	$(MLANG) --display_time --debug --optimize \
	        --mpp_file ../../2018.mpp --mpp_function compute_double_liquidation_pvro \
	        --backend python --output simulateur_simplifie_2018.py \
                --function_spec ../../specs/simulateur_simplifie.m_spec \
		$(SOURCE_FILES_2018)

cas_basique_2018:
	$(MLANG) --display_time --debug --optimize \
	        --mpp_file ../../2018.mpp --mpp_function compute_double_liquidation_pvro \
	        --backend python --output cas_basique_2018.py \
                --function_spec ../../specs/cas_basique.m_spec \
		$(SOURCE_FILES_2018)

autograd:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend autograd --function_spec autograd.m_spec \
		--output ir_2018.py $(SOURCE_FILES_2017) && \
	python example_autograd.py

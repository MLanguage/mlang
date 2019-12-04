SOURCE_DIR_2015=../../ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=../../ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=../../ir-calcul/sources2017m_6_10/
SOURCE_DIR_2018=../../ir-calcul/sources2018m_6_3/

SOURCE_FILES_2017=$(shell find $(SOURCE_DIR_2017) -name "*.m")
SOURCE_FILES_2018=$(shell find $(SOURCE_DIR_2018) -name "*.m")

MLANG=dune exec ../../src/main.exe --

simulateur_simplifie_2018:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend python --output ir_2018.py \
		--function_spec simulateur_simplifie_2018.m_spec \
		$(SOURCE_FILES_2017)
	python simulateur_simplifie_2018.py

simulateur_simplifie_2019:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend python --output ir_2019.py \
		--function_spec simulateur_simplifie_2019.m_spec \
		$(SOURCE_FILES_2018)
	python simulateur_simplifie_2019.py

cas_basique_2018:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend python --function_spec cas_basique.m_spec \
		--output ir_2018.py $(SOURCE_FILES_2017) && \
	python cas_basique_2018.py

cas_basique_2019:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend python --function_spec cas_basique.m_spec \
		--output ir_2019.py $(SOURCE_FILES_2018) && \
	python cas_basique_2019.py

autograd:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend autograd --function_spec autograd.m_spec \
		--output ir_2018.py $(SOURCE_FILES_2017) && \
	python example_autograd.py

verifisc_cas_basique_2018:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend verifisc --function_spec cas_basique.m_spec \
		--output ir_2018.py $(SOURCE_FILES_2017)

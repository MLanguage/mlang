SOURCE_DIR_2015=../../ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=../../ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=../../ir-calcul/sources2017m_6_10/

SOURCE_FILES=$(shell find $(SOURCE_DIR_2017) -name "*.m")

MLANG=mlang

z3_basique:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend z3 --function_spec cas_basique.m_spec \
		$(SOURCE_FILES)

z3_simulateur:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend z3 --function_spec z3_simulateur.m_spec \
		$(SOURCE_FILES)

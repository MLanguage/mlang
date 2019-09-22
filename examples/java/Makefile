SOURCE_DIR_2015=../../ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=../../ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=../../ir-calcul/sources2017m_6_10/

SOURCE_FILES=$(shell find $(SOURCE_DIR_2017) -name "*.m")

MLANG=dune exec ../../src/main.exe --

cas_basique_2018:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend java --function_spec cas_basique.m_spec \
		--output Ir2018.java $(SOURCE_FILES)
	javac Ir2018.java CasBasique2018.java
	java CasBasique2018

simulateur_simplifie_2018:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend java --output Ir2018.java \
		--function_spec simulateur_simplifie_2018.m_spec \
		$(SOURCE_FILES)
	javac Ir2018.java

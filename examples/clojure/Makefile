SOURCE_DIR_2015=../../ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=../../ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=../../ir-calcul/sources2017m_6_10/

SOURCE_FILES=$(shell find $(SOURCE_DIR_2017) -name "*.m")

MLANG=dune exec ../../src/main.exe --

cas_basique_2018:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend clojure --function_spec cas_basique.m_spec \
		--output ir_2018.clj $(SOURCE_FILES)
	clj -i cas_basique_2018.clj -e "(-main)"

simulateur_simplifie_2018:
	$(MLANG) --application iliad \
	 	--display_time --debug --optimize \
		--backend clojure --output ir_2018.clj \
		--function_spec simulateur_simplifie_2018.m_spec \
		$(SOURCE_FILES)

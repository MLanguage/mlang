MLANG_BIN=dune exec ../../src/main.exe --
OPTIMIZE_FLAG=-O
MLANG=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(OPTIMIZE_FLAG)


all: cas_basique simulateur_simplifie

simulateur_simplifie:
	$(MLANG) \
		--backend python --output ir_2019.py \
		--function_spec simulateur_simplifie_2019.m_spec \
		$(SOURCE_FILES)
	python3.7 simulateur_simplifie_2019.py

cas_basique:
	$(MLANG) \
		--backend python --function_spec cas_basique.m_spec \
		--output ir_2019.py $(SOURCE_FILES) && \
	python3.7 cas_basique_2019.py

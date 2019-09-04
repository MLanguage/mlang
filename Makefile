SOURCE_DIR_2015=ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=ir-calcul/sources2017m_6_10/

SOURCE_FILES=$(shell find $(SOURCE_DIR_2017) -name "*.m")

# export LD_LIBRARY_PATH=$(Z3_FOLDER)
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(ocamlfind query z3)

deps:
	opam install ppx_deriving ANSITerminal re ocamlgraph z3 dune

build:
	dune build src/main.exe

simulateur_simplifie_2018: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend python --output processing/ir_2018.py \
		--function_spec specs/simulateur_simplifie_2018.m_spec \
		$(SOURCE_FILES)

cas_basique_2018: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend python --function_spec specs/cas_basique.m_spec \
		--output processing/ir_2018.py $(SOURCE_FILES) && \
	python processing/example_simple.py

autograd: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend autograd --function_spec specs/autograd.m_spec \
		--output processing/ir_2018.py $(SOURCE_FILES) && \
	python processing/example_autograd.py

verifisc_cas_basique_2018: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend verifisc --function_spec specs/cas_basique.m_spec \
		--output processing/ir_2018.py $(SOURCE_FILES)

z3_basique: build
	OCAMLRUNPARAM=b	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend z3 --function_spec specs/cas_basique.m_spec \
		$(SOURCE_FILES)

z3_simulateur: build
	OCAMLRUNPARAM=b	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend z3 --function_spec specs/z3_simulateur.m_spec \
		$(SOURCE_FILES)

# use: TEST_FILE=bla make test
test: #build
	./main.exe --application iliad \
	 	--display_time --debug --backend z3 \
		--function_spec specs/tests.m_spec \
		--run_test=$(TEST_FILE) \
		$(SOURCE_FILES)

tests: build
	OCAMLRUNPARAM=b dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --backend z3 \
		--function_spec specs/tests.m_spec \
		--run_all_tests=tests/ \
		$(SOURCE_FILES)

# check that the repl_debuguer is disabled
test_report: build
	./scripts/tests.sh 2> test_results.txt
	python3 scripts/categorize_erros.py > test_report.txt


doc:
	dune build @doc

graph:
	dot -Ksfdp -Goverlap=false -Goutputorder=edgesfirst -Nmargin=0.22,0.11 -Tsvg -Gratio=0.707106781 -o Graphe_IR.svg dep_graph.dot
	inkscape -z -e Graphe_IR.png -d 96 Graphe_IR.svg
	convert -resize 1980x1024 Graphe_IR.png Graphe_IR_Miniature.png

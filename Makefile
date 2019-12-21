SOURCE_DIR_2015=ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=ir-calcul/sources2017m_6_10/

SOURCE_FILES=$(shell find $(SOURCE_DIR_2017) -name "*.m")

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(ocamlfind query z3)

deps:
	opam install ppx_deriving ANSITerminal re ocamlgraph z3 dune menhir cmdliner dune-build-info visitors

build:
	dune build src/main.exe

# use: TEST_FILE=bla make test
test: build
	OCAMLRUNPARAM=b dune exec src/main.exe --application iliad \
	 	--display_time --debug --backend z3 \
		--function_spec specs/tests.m_spec \
		--run_test=$(TEST_FILE) \
		$(SOURCE_FILES)
test2: #build
	./main.exe --application iliad \
	 	--display_time --debug --backend z3 \
		--function_spec specs/test2.m_spec \
		--run_test=$(TEST_FILE) \
		$(SOURCE_FILES)

tests: build
	OCAMLRUNPARAM=b dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --backend z3 \
		--function_spec specs/tests.m_spec \
		--run_all_tests=tests/ \
		$(SOURCE_FILES)

doc:
	dune build @doc
	ln -s _build/default/_doc/_html/index.html doc.html

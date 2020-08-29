SOURCE_DIR_2015=ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=ir-calcul/sources2017m_6_10/
SOURCE_DIR_2018=ir-calcul/sources2018m_6_7/

SOURCE_FILES=$(SOURCE_DIR_2018)

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(ocamlfind query z3)

default: build

deps:
	opam install ppx_deriving ANSITerminal re ocamlgraph dune menhir \
	cmdliner dune-build-info visitors parmap num ocamlformat
	git submodule update --init --recursive

format:
	dune build @fmt --auto-promote | true

build: #format
	dune build

# use: TEST_FILE=bla make test
test: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --backend interpreter \
		--mpp_file=2018.mpp \
		--run_test=$(TEST_FILE) \
		$(SOURCE_FILES)

tests: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --backend interpreter \
		--mpp_file=2018.mpp \
		--run_all_tests=tests/ \
		$(SOURCE_FILES)


tests2017: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --backend interpreter \
		--function_spec tests.m_spec\
		--run_all_tests=tests_2017/ --year=2017 \
		$(shell find $(SOURCE_DIR_2017) -name "*.m")

test2017: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --backend interpreter\
		--function_spec tests.m_spec\
		--run_test=$(TEST_FILE) --year=2017 \
		$(shell find $(SOURCE_DIR_2017) -name "*.m")


doc:
	dune build @doc
	ln -s _build/default/_doc/_html/index.html doc.html

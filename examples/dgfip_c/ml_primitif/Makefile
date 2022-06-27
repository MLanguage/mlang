TEST_FILES=$(wildcard ../../../tests/2020/fuzzing/fuzzer_*)

prim:
	./build.sh

run_tests: prim
	./run_tests.sh ${TEST_FILES}


clean:
	./build.sh clean

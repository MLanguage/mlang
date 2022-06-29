TEST_FILES=$(realpath ../../../tests/2020/fuzzing/)/fuzzer_*

prim:
	./build.sh

run_tests: prim
	 ./prim ${TEST_FILES} || (echo "Test failure" && false)

clean:
	./build.sh clean

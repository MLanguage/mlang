include ../../../Makefile.include

ifeq ($(TEST_FILTER), 1)
    TEST_FILES=$(TESTS_DIR)[A-Z]*
else
    TEST_FILES=$(TESTS_DIR)*
endif

prim:
	./build.sh

run_tests: prim
	 ./prim ${TEST_FILES} || (echo "Test failure" && false)

clean:
	./build.sh clean

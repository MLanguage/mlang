include ../../../Makefile.include

ifeq ($(TEST_FILTER), 1)
    TEST_FILES=$(TESTS_DIR)[A-Z]*
else
    TEST_FILES=$(TESTS_DIR)*
endif

cal:
	./build.sh

run_tests: cal
	 ./cal ${TEST_FILES}

clean:
	./build.sh cleanall

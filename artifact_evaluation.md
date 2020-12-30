This file gives instructions to reproduce claims made in the CC'21 article.

# Optimizations results in different cases (Figure 11)

The M specification files are located in folder `m_specs`.
All in Figure 11 is `all_ins_and_outs_2018.m_spec`.
Similarly, "Selected outs" is `all_ins_selected_outs_2018.m_spec`, "Tests" is `tests.m_spec`, "Simplified" is `simulateur_simplifie_2018.m_spec`, "Basic" is `basic_case.m_spec`
The run Mlang with a given M specification file, just write `M_SPEC_FILE=m_specs/your_file.m_spec make from_spec`.
Mlang then displays the number of inputs and outputs given by the specification file, as well as the number of instructions generated in the end.
For example, reading from `M_SPEC_FILE=m_specs/tests.m_spec make from_spec` gives:

    [...]
    [DEBUG] M_spec has 1732 inputs and 651 outputs
    [...]
    [DEBUG] Optimizations done! Total effect: 656719 → 115297 (↘ 82.4%)
    [...]

Note that the initial number of instructions differs from each M specification file, because some initialization assignments depend on the number of inputs.

# Performance of the generated C code (Figure 16)

TODO

# Running Mlang with different numerical precision (Section 5.2)

TODO: scaling issues with most numerical backends

You can run the test-suite with different numerical backends.

## 1024-bit floats
In this setting, the precision is sufficient to pass all tests

    PRECISION=mpfr1024 make tests

## Rounding mode
Here, floats are replaced by floating-point intervals, with down rounding for the lower-bound and up rounding for the upper bound.

    PRECISION=interval make tests

Some tests fail with "Tried to convert interval to float, got two different bounds".
This means that the rounding mode changes the results.

## Fixed precision

TODO

## Rationals

TODO


# Test-case generation (Section 5.3)

The randomized tests are provided in `tests/2018/randomized`. You can run them with `TESTS_DIR=tests/2018/randomized/ make tests`.

The fuzzing-based tests are used by default, and can be found in `tests/2018/fuzzing/`.

TODO: explain how to generate more?

# Generated Tests Coverage Measurement (Figure 17)

To measure coverage, just add `CODE_COVERAGE=1` before the make command.
Measuring the code coverage of the fuzzed tests can be done using `CODE_COVERAGE=1 make tests`.
The coverage results are given in the last three lines of the execution trace.
For the randomized tests, you need to run `CODE_COVERAGE=1 TESTS_DIR=tests/2018/randomized/ make tests`.
The DGFiP private tests could not be made publicly available.

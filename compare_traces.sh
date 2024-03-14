#! /bin/bash
DGFIP_TARGET_FLAGS=-g,-O,-k4,-t YEAR=2022 make compile_dgfip_c_backend -B
~/fuzzing-calculette/calculette/trunk/2022/AIT \
    -f tests/2022/fuzzing/fuzzer_3469.m_test |& \
    sed -e 's/\x1b\[[0-9;]*m//g' &> aif_trace.txt
NO_BINARY_COMPARE=1 ./examples/dgfip_c/ml_primitif/cal \
    tests/2022/fuzzing/fuzzer_3469.m_test &> mlang_trace.txt
diff aif_trace.txt mlang_trace.txt -u > diff_trace.txt

#! /bin/bash

i=0
mkdir -p fuzz_tests
FILES=$(find fuzz_findings/*/crashes/ -name "id*")
for f in $FILES 
do 
    i=$((i+1))
    cp $f fuzz_tests/fuzzer_$i.m_crash
done

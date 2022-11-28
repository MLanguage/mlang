#!/bin/bash

for f in ../../../tests/2020/fuzzing/fuzzer_* ; do
  if [ ! -d $f ] ; then
    ./cal $f > /dev/null
    echo $? $f
  done
done

#!/bin/bash

year=${1:-2020}

if [ -d "$year.output" ]; then
    echo "Default year to 2020"
else
    echo "Need a valid tested year";
    exit 1
fi

for f in ../../../tests/$year/fuzzing/fuzzer_* ; do
  if [ ! -d $f ] ; then
    bf=$(basename $f)
    ./cal $f > /dev/null
    echo $? $bf
    mv ./${year}.output/${bf}.tgv ./$year.expected/${bf}.tgv
  fi
done

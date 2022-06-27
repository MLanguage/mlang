#!/bin/bash

TEST_FILES="$@"

for t in ${TEST_FILES}
do
    ret=1
    echo "Testing $t..."
    ./prim $t > /dev/null || break
    ret=0
done
if [ $ret -ne 1 ]; then
    echo "Tests successful"
else
    echo "Tests failure(s)"
fi
exit $ret

# FILES=$(ls -rS tests/*)

# for i in $FILES; do
#     echo -n "Testing $i...";
#     TEST_FILE=$i make test 1>/dev/null;
#     if [ $? -eq 0 ]; then
#         echo " OK"
#     else
#         echo " FAIL"
#     fi
# done
ls -rS tests/* | parallel 'TEST_FILE={} make test 1>/dev/null; if [ $? -eq 0 ]; then echo "Testing of {} successful"; else echo "Testing of {} failed"; fi'

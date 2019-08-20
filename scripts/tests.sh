# FILES=$(ls -rS tests/*)

# for i in $FILES; do
#     echo -n "Testing $i...";
#     TEST_FILE=$i make test 1>/dev/null 2>/dev/null;
#     if [ $? -eq 0 ]; then
#         echo " OK"
#     else
#         TEST_FILE=$i make test2 1>/dev/null;
#         if [ $? -eq 0 ]; then
#             echo " OK2"
#         else
#             echo " FAIL"
#         fi
#     fi
# done
ls -rS tests/* | parallel 'TEST_FILE={} make test 1>/dev/null;
if [ $? -eq 0 ]; then
 echo "{}: success on tests.m_spec";
else
 TEST_FILE={} make test2 1>/dev/null;
 if [ $? -eq 0 ]; then
  echo "{}: success on test2.m_spec";
 else
   echo "{}: failure";
 fi;
fi'

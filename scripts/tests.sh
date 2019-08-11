FILES=$(ls -rS tests/TEST*)

for i in $FILES; do
    echo -n "Testing $i...";
    TEST_FILE=$i make test 1>/dev/null;
    if [ $? -eq 0 ]; then
        echo " OK"
    else
        echo " FAIL"
    fi
done

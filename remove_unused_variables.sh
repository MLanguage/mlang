#!/bin/bash

set -ue

if [ -z "$1" ] || [ -z "$2" ]; then
	echo "Usage: $0 <file containing names of variables to be removed> <directory of the code base>"
	exit 1
fi

var_file="$1"
code_base="$2"

if [ ! -f "$var_file" ]; then
	echo "The file $1 does not exist"
	exit 2
fi

counter=0

while IFS= read -r var; do
	if [ -z "$var" ]; then
		continue
	fi

	echo "Processing variable $var"

	grep -lE "(^|[^(A-Z|a-z|0-9|_)])$var([^(A-Z|a-z|0-9|_)]|$)" "$code_base"/*.m | xargs sed -ibak -E "/(^|[^(A-Z|a-z|0-9|_)])$var([^(A-Z|a-z|0-9|_)]|$)/d"
# La ligne suivante suppose une modification des makefiles pour tester sur une version en cours de développement de Mlang
	YEAR=2024 TEST_FILE=tests/dummy.irj make test
	((counter++)) && echo $counter
done < "$var_file"

echo "Done"

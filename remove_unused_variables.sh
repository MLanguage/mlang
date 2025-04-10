#!/bin/sh

set -ue

if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]; then
	echo "Usage: $0 <file containing names of variables to be removed> <directory of the code base> \
	<name of the file that will contain the SQL queries>"
	exit 1
fi

var_file="$1"
code_base="$2"
sql_file="$3"

if [ ! -f "$var_file" ]; then
	echo "The file $var_file does not exist"
	exit 2
fi
if [ -f "$sql_file" ] && [ ! -s "$sql_file" ]; then
	echo "The file $sql_file exists and is not empty"
	exit 3
fi

counter=0

while IFS= read -r var; do
	if [ -z "$var" ]; then
		continue
	fi

	echo "Processing variable $var"

	grep -lE "^${var}[^(A-Z|a-z|0-9|_)]" "$code_base/*.m" | while IFS= read -r file
	do
		awk "/^$var([^a-zA-Z0-9_]|$)/ {f=1} f {if (/;/) {f=0} next} 1" "$file" > temp && mv temp "$file"
	done

# La ligne suivante suppose une modification des makefiles pour tester sur une version en cours de développement de Mlang
	YEAR=2024 TEST_FILE=tests/dummy.irj make test
	counter=$((counter + 1)) && echo $counter
	echo "delete from dico_24 where variable='$var';" >> "$sql_file"
done < "$var_file"

echo "Done"

#!/usr/bin/env bash

set -e -x

cd $(dirname $(readlink -f $0))

echo Mise à jour des fichiers issus du dépôt mlang-deps

mkdir -p mlang-deps

cd mlang-deps

git clone --branch master https://forge.dgfip.finances.rie.gouv.fr/dgfip/si-1e/firsth/ir/calculette/mlang-deps

rm -fr mlang-deps/.git

cp -fr mlang-deps/* ./

rm -fr mlang-deps


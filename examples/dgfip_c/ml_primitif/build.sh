#!/bin/bash

. build.conf

if [ "$1" == 'help' ]; then
  echo "La commande $0 sans option compile la calculette corrective".
  echo 'Options supportées :'
  echo 'nom       Ne recompile pas les fichiers M (un peu plus rapide)'
  echo 'nomc      Ne recompile pas les fichiers M ni C issus de M (plus rapide)'
  echo 'clean     Supprime les fichiers .o/.cmo/.cmx/.cmi'
  echo 'cleanall  Supprime tous les fichiers intermediaires'
  echo 'help      Affiche cette aide'
  exit 0
elif [ "$1" == 'clean' ]; then
  echo 'Nettoyage des fichiers binaires intermediaires'
  rm -f *.o
  rm -f *.cm*
  rm -f calc/*.o
  exit 0
elif [ "$1" == 'cleanall' ]; then
  echo 'Nettoyage de de tous les fichiers intermediaires'
  rm -f *.o
  rm -f *.cm*
  rm -f calc/*.o
  rm -f calc/*.[ch]
  rm -f calc/*.inc
  rm -f prim
  exit 0
fi

if [ ! -d ./calc ]; then
  mkdir ./calc
fi

echo 'Copie des fichiers C statiques'

cp $MLANG_ROOT/examples/dgfip_c/const.h ./calc
cp $MLANG_ROOT/examples/dgfip_c/dbg.h ./calc
cp $MLANG_ROOT/examples/dgfip_c/desc_static.h.inc ./calc
cp $MLANG_ROOT/examples/dgfip_c/enchain_static.c.inc ./calc
cp $MLANG_ROOT/examples/dgfip_c/var_static.c.inc ./calc
cp $MLANG_ROOT/examples/dgfip_c/irdata.* ./calc

if [ "$1" == 'nom' ] || [ "$1" == 'nomc' ]; then

  echo 'Compilation des fichiers M ignorée'

else

  echo "Compilation des fichiers M avec Mlang (YEAR=$YEAR)"

  cd ./calc

  # Note: we MUST compile with -k1 (and its dependence -g, cf. how is defined NB_DEBUG01)
  $MLANG -O -b dgfip_c --mpp_file=$MPP_FILE --mpp_function=$MPP_FUN --dgfip_options=-Ailiad,-m$YEAR,-X,-O,-g,-k1 -o enchain.c $M_SOURCES/*.m $M_TGV $M_ERR >/dev/null

  if [ $? -ne 0 ]; then
    echo 'La compilation des fichiers M a échoué'
    exit 1
  fi

  cd ..

fi

if [ "$1" == 'nomc' ]; then

  echo 'Compilation des fichiers C issus des fichiers M ignorée'

else
  if command -v clang; then
    echo "Compilation avec Clang."
    CCOMPILER="clang"
    CCOPTIONS="-fbracket-depth=2048"
  else
    echo "Clang est absent. Compilation avec GCC."
    CCOMPILER="gcc"
    CCOPTIONS=""
  fi

  echo "Compilation des fichiers C issus des fichiers M"


  cd ./calc

  $CCOMPILER -std=c89 -pedantic $CCOPTIONS -O2 -c irdata.c enchain.c var.c contexte.c famille.c revenu.c revcor.c penalite.c variatio.c tableg01.c restitue.c chap-*.c res-ser*.c coc*.c coi*.c horiz*.c

  if [ $? -ne 0 ]; then
    echo 'La compilation des fichiers C issus des fichiers M a échoué'
    exit 1
  fi

  cd ..

fi

echo 'Compilation de la calculette primitive'

ocamlopt -cc $CCOMPILER -ccopt $CCOPTIONS -ccopt -std=c99 -ccopt -fno-common unix.cmxa ./calc/*.o stubs.c common.ml m.ml read_test.ml main.ml -o prim

if [ $? -ne 0 ]; then
  echo 'La compilation de la calculette primitive a échoué'
  exit 1
fi

rm -f *.o
rm -f *.cm*

echo 'Compilation terminée'

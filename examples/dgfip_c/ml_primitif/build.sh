#!/bin/bash

. build.conf

if [ "$1" == 'help' ]; then
  echo "La commande $0 sans option compile la calculette corrective".
  echo 'Options supportées :'
  echo 'nom       Ne recompile pas les fichiers M (un peu plus rapide)'
  echo 'clean     Supprime les fichiers .o/.cmo/.cmx/.cmi'
  echo 'cleanall  Supprime tous les fichiers intermediaires'
  echo 'help      Affiche cette aide'
  exit 0
elif [ "$1" == 'clean' ]; then
  echo 'Nettoyage des fichiers binaires intermediaires'
  rm -f *.o
  rm -f *.cm*
  exit 0
elif [ "$1" == 'cleanall' ]; then
  echo 'Nettoyage de de tous les fichiers intermediaires'
  rm -f *.o
  rm -f *.cm*
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

if [ "$1" == 'nom' ]; then

  echo 'Compilation des fichiers M ignorée'

else

  echo "Compilation des fichiers M avec Mlang (YEAR=$YEAR)"

  cd ./calc

  # Note: we MUST compile with -k1 (and its dependence -g, cf. how is defined NB_DEBUG01)
  $MLANG -b dgfip_c --mpp_file=$MPP_FILE --mpp_function=$MPP_FUN --dgfip_options=-Ailiad,-m$YEAR,-M,-g,-k1 -o enchain.c $M_SOURCES/*.m >/dev/null

  if [ $? -ne 0 ]; then
    echo 'La compilation des fichiers M a échoué'
    exit 1
  fi

  cd ..

  #sed -i 's/calc\/enchain.h/enchain.h/g' calc/enchain.c

fi

echo 'Compilation de la calculette primitive'

#ocamlopt -g -inline 0 -cc clang -ccopt -fbracket-depth=2048 ./calc/irdata.c ./calc/enchain.c ./calc/var.c ./calc/contexte.c ./calc/famille.c ./calc/revenu.c ./calc/revcor.c ./calc/penalite.c ./calc/variatio.c ./calc/tableg01.c ./calc/restitue.c stubs.c common.ml m.ml correctif.ml read_test.ml main.ml -o corr

ocamlopt -cc clang -ccopt -fbracket-depth=2048 unix.cmxa ./calc/irdata.c ./calc/enchain.c ./calc/var.c ./calc/contexte.c ./calc/famille.c ./calc/revenu.c ./calc/revcor.c ./calc/penalite.c ./calc/variatio.c ./calc/tableg01.c ./calc/restitue.c ./calc/chap-*.c ./calc/res-ser*.c ./calc/coc*.c ./calc/coi*.c ./calc/horiz*.c stubs.c common.ml m.ml read_test.ml main.ml -o prim

if [ $? -ne 0 ]; then
  echo 'La compilation de la calculette primitive a échoué'
  exit 1
fi

echo 'Compilation terminée'

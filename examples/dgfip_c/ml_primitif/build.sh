#!/bin/bash

if test -f "build.conf"; then
    . build.conf
else
    . build.conf.default
fi

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
  rm -f cal
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

  # Note: we MUST compile with -kN whith N=1..4 (and its dependence -g, cf. how is defined NB_DEBUG01)
  $MLANG -O -b dgfip_c --mpp_file=$MPP_FILE --mpp_function=$MPP_FUN --dgfip_options=-Ailiad,-m$YEAR,-X,-O,-g,-k4 -o enchain.c $M_SOURCES/*.m $M_TGV $M_ERR >/dev/null

  if [ $? -ne 0 ]; then
    echo 'La compilation des fichiers M a échoué'
    exit 1
  fi

  cd ..

fi

if [ "$1" == 'nomc' ]; then

  echo 'Compilation des fichiers C issus des fichiers M ignorée'

else

  echo "Compilation des fichiers C issus des fichiers M (CC=$CC, CFLAGS=$CFLAGS)"


  cd ./calc

  $CC $CFLAGS -c irdata.c enchain.c var.c contexte.c famille.c revenu.c revcor.c penalite.c variatio.c tableg??.c restitue.c chap-*.c res-ser*.c coc*.c coi*.c horiz*.c

  if [ $? -ne 0 ]; then
    echo 'La compilation des fichiers C issus des fichiers M a échoué'
    exit 1
  fi

  cd ..

fi

echo "Compilation de la calculette primitive (OCAMLFLAGS=$OCAMLFLAGS, WITH_BISECT=$WITH_BISECT)"

if [ $WITH_BISECT -eq 1 ]; then

  ocamlopt -I $(ocamlfind query bisect_ppx)/common -I $(ocamlfind query bisect_ppx)/runtime -ppx "$(ocamlfind query bisect_ppx)/ppx.exe --as-ppx" -cc $CC $OCAMLFLAGS unix.cmxa bisect_common.cmxa bisect.cmxa ./calc/*.o stubs.c common.ml m.ml read_test.ml main.ml -o cal

else

  ocamlopt -cc $CC $OCAMLFLAGS unix.cmxa ./calc/*.o stubs.c common.ml m.ml read_test.ml main.ml -o cal

fi

if [ $? -ne 0 ]; then
  echo 'La compilation de la calculette primitive a échoué'
  exit 1
fi

rm -f *.o
rm -f *.cm*

echo 'Compilation terminée'

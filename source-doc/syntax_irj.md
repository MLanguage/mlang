(syntax_irj)=

# Les fichiers IRJ

## Syntaxe

Les fichiers IRJ sont des fichiers de test faisant corespondre entrées et
sorties du calcul d'un programme M.
Chaque fichier IRJ est divisé en 7 parties, plus 3 parties optionnelles,
et est terminé par la ligne `##`.

* `NOM` : le nom du test.
* `ENTREES-PRIMITIF` : les valeurs des variables d'entrées au début du calcul. A chaque variable `VAR` est associée une valeur `val` entière ou décimale sur une ligne de la forme `VAR/val`. Les variables d'entrées qui ne sont pas définies dans le fichier IRJ seront initialisées à `indefini`.
* `CONTROLES-PRIMITIF` : à documenter
* `RESULTATS-PRIMITIF` : les valeurs des variables restituées à la fin du calcul. Comme pour les variables d'entrées, elles sont décrites sous la forme `VAR/val`. Les valeurs restituées absentes du fichiers IRJ sont supposées avoir la valeur `indefini` à la fin du calcul.
* `ENTREES-CORRECTIF` : à documenter
* `CONTROLES-CORRECTIF` : à documenter
* `RESULTATS-CORRECTIF` : à documenter
* `ENTREES-RAPPELS` : partie optionnelle, à documenter
* `CONTROLES-RAPPELS` : partie optionnelle, à documenter
* `RESULTATS-RAPPELS` : partie optionnelle, à documenter

Voici un exemple de fichier IRJ minimal
```
   #NOM
   MON-TEST
   #ENTREES-PRIMITIF
   X/0.0
   #CONTROLES-PRIMITIF
   #RESULTATS-PRIMITIF
   Y/1
   Z/2.0
   #ENTREES-CORRECTIF
   #CONTROLES-CORRECTIF
   #RESULTATS-CORRECTIF
   ##
```

De nombreux exemples de fichiers IRJ sont disponibles dans le dossier `tests/`.

## Utilisation

Trois utilitaires sont dédiés à l'utilisation des fichiers IRJ.

### IRJ checker

Les sources de mlang mettent à disposition un code de vérification de fichiers
IRJ.
Après compilation, il peut être exécuté via la commande :
```
$ dune exec -- irj_checker
```

### Interpreteur

Le binaire mlang intègre un interpreteur de M qui prend en entrée un ou
plusieurs fichiers M ainsi qu'un fichier IRJ.
Après compilation, l'interpreteur peut être exécuté via la commande :
```
$ dune exec -- mlang test.m -A application -b interpreter --mpp_function cible_dentree  --dgfip_options='' -r test.irj
```

L'option `-r` peut être remplacée par `-R` pour tester l'ensemble des tests d'un
dossier complet.

### Fichiers C

Il est également possible d'utiliser les fichiers IRJ comme entrée du code C
généré à partir d'une compilation de code M par `mlang`.
Ce traitement est effectué dans le cas du backend `dgfip_c` de mlang, dont
le code est disponible dans `examples/dgfip_c/ml_primitif`.
La calculette d'une année donnée peut êter compilée via :
```
$ make YEAR=year compile_dgfip_c_backend
```

Les scripts présents dans `examples/dgfip_c/ml_primitif/ml_driver` permettent
d'interfacer les fichiers IRJ avec les valeurs `C` de type `T_irdata` contenant
le TGV (Tableau Général des Variables).
La commande suivante permet de tester la calculette d'une année sur l'ensemble
des tests fuzzés de la dite année.

```
$ make test_dgfip_c_backend
```

L'utilisation du script `cal` utilisé dans cette règle est documentée dans
`examples/dgfip_c/README.md`.

(mlang)=

# Le compilateur MLang

## Utiliser MLang

Le binaire `mlang` prend en argument le fichier *M* à exécuter. 
Il peut également prendre en argument une liste de fichiers, auquel cas leur
traitement sera équivalent au traitement d'un seul et même fichier dans lequel
serait concatené le contenu de chaque fichier.
%%

Les deux options principales sont : 
* `-A`: le nom de l'application à traiter;
* `--mpp_function`: le nom de la fonction principale à traiter.

### Mode interpreteur

Le mode interpreteur de `mlang` utilise un fichier *IRJ* (voir {ref}`syntax_irj`)
pour exécuter le code *M* directement depuis sa représentation abstraite.

Voici une commande simple pour invoquer l'interpreteur :
```
$ mlang test.m \
	-A mon_application \
	-b interpreter \
	--mpp_function hello_world \
	--run_test test.irj \
	--without_dfgip_m
```

### Mode transpilation

Le mode transpilation de `mlang` permet de traduire le code *M* dans un autre
langage.
En 2025, seul le langage C est supporté.
Voici une commande simple pour traduire un fichier *M* en *C* :

```
$ mlang test.m \
	-A mon_application \
	-b dgfip_c \
	--mpp_function hello_world
	--dgfip_options='' 
	--output output/mon-test.c
```

NB: le dossier `output` doit avoir été créé en amont.

### Options DGFiP

Les options DGFiP sont à usage interne.

```
       -b VAL
           Set application to "batch" (b0 = normal, b1 = with EBCDIC sort)

       -D  Generate labels for output variables

       -g  Generate for test (debug)

       -I  Generate immediate controls

       -k VAL (absent=0)
           Number of debug files

       -L  Generate calls to ticket function

       -m VAL (absent=1991)
           Income year

       -O  Optimize generated code (inline min_max function)

       -o  Generate overlays

       -P  Primitive calculation only

       -r  Pass TGV pointer as register in rules

       -R  Set application to both "iliad" and "pro"

       -s  Strip comments from generated output

       -S  Generate separate controls

       -t  Generate trace code

       -U  Set application to "cfir"

       -x  Generate cross references

       -X  Generate global extraction

       -Z  Colored output in chainings
```

## Comportement

* Traduction du code M dans l'arbre de syntaxe abstraite M_AST.
* Extension des constructions.
* Vérification de cohérence.
* Selon le mode : 
  * **Interpreteur** Lecture du fichier IRJ et interpretation du code.
  * **Transpilation** Ecriture du code C équivalent au code M.

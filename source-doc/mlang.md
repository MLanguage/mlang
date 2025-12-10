(mlang)=

# Le compilateur MLang

## Utiliser MLang

Le binaire `mlang` prend en argument le fichier *M* à exécuter.
%%
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
%%
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
%%
En 2025, seul le langage C est supporté.
%%
Voici une commande simple pour traduire un fichier *M* en *C* :

```
$ mlang test.m \
	-A mon_application \
	-b dgfip_c \
	--mpp_function hello_world
	--dgfip_options='' 
	--output output/mon-test.c
```

### Options DGFiP

## Comportement

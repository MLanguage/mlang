# Note importante

Le compilateur MLang est désormais hébergé sur la forge Adullact.

# Le compilateur Mlang

![CI Tests](https://github.com/MLanguage/mlang/actions/workflows/check_correctness.yml/badge.svg)
[![Documentation deployed](https://img.shields.io/badge/Developper%20documentation-deployed-GREEN.svg)](https://mlanguage.github.io/mlang/mlang/index.html)

Le langage M a été inventé par la Direction Générale des Finances Publiques (DGFiP) française pour transcrire le code des impôts en instructions lisibles par une machine. Il s'agit d'un petit langage dédié (DSL - Domain Specific Language) basé sur des déclarations de variables et des opérations arithmétiques. Ce travail est basé sur une rétro-ingénierie de la syntaxe et de la sémantique de M, à partir de la base de code précédemment publiée par la DGFiP sur la forge Framagit et désormais régulièrement publiée sur la forge Adullact.
[Framagit forge](https://framagit.org) and now regularly published on
[Adullact forge](https://gitlab.adullact.net/dgfip/ir-calcul).

## Avertissement

Il n'existe actuellement aucune garantie juridique d'aucune sorte quant à l'exactitude du code produit par le compilateur Mlang, ou des résultats produits par l'interprète de Mlang. Cependant, les auteurs ont travaillé en étroite collaboration avec la DGFiP pour valider Mlang, et le système passe tous les tests privés de la DGFiP en date de septembre 2026 pour la version des fichiers sources responsables du calcul de l'impôt des années 2018 à 2024.

## Installation

Mlang est implémenté en OCaml. Pour gérer les dépendances, installez opam et basculez vers une version d'OCaml au moins égale à 4.14.2. Afin de supporter les calculs en virgule flottante multi-précision, vous devrez également installer la bibliothèque MPFR.

Pour les distributions basées sur Debian, exécutez simplement :
    
	sudo apt install libgmp-dev libmpfr-dev git opam

Pour les distributions basées sur Red Hat, exécutez d'abord :

	sudo yum install gmp-devel mpfr-devel git

Opam n'est packagé que pour Fedora. Pour les autres distributions utilisant RPM, veuillez vous référer à la  [documentation officielle](https://opam.ocaml.org/doc/Install.html). Notez que pour utiliser la version binaire d'Opam et installer les dépendances de Mlang, vous aurez besoin d'un compilateur C et des logiciels suivants comme dépendances Opam : `patch`,`unzip`, `bubblewrap` et `bzip2`.

Si vous souhaitez générer des tests à l'aide du fuzzer, vous devrez installer AFL :
	
	sudo apt install afl++ afl++-clang

Si vous n'avez jamais utilisé opam auparavant, lancez :
	
	opam init
	opam update -y

Ensuite, vous pouvez initialiser votre projet Mlang en utilisant :

	make init

Cette commande crée un "switch" Opam local (analogue à un environnement virtuel), installe les dépendances OCaml de Mlang et clone le dépôt du code source M publié par la DGFiP avec :

	git submodule update --init ir-calcul

Vous pouvez ensuite utiliser :

	make build

pour construire le compilateur. Si nécessaire,

	make deps

réinstallera les dépendances OCaml et récupérera à nouveau le code source M.

L'interprète et le backend C dans `examples/dgfip_c` devraient être utilisables immédiatement, car le compilateur C a été installé pour Opam. 
Les résultats de Mlang sont testés sur GCC et Clang, ce dernier étant préféré s'il est disponible.

## Utilisation

Mlang a également besoin d'un fichier M pour savoir comment exécuter le mécanisme de "liquidations multiples" qui est nécessaire pour calculer correctement l'impôt sur le revenu. Par exemple, le fichier `ir_calcul/2022/cible.m` correspond au code non publié de la DGFiP pour la version des sources M 2022 publiées dans ir-calcul.

Certains drapeaux (flags) du Makefile peuvent être configurés de manière permanente en modifiant le fichier makefiles/variables.mk.

Si vous souhaitez générer les fichiers sources du backend ML, lancez la commande :

	make YEAR=<2020 ou 2022> dgfip_c_backend

ou
	
	make dgfip_c_backend

avec l'année 2022 par défaut. Les fichiers sont ensuite générés dans `example/dgfip_c/ml_primitif/calc`.

Si vous souhaitez générer l'exécutable du backend ML, lancez la commande :

	make YEAR=<2020 ou 2022> compile_dgfip_c_backend

ou

	make compile_dgfip_c_backend

avec l'année 2022 par défaut.

## Tests

Mlang est testé en utilisant le format de fichier de test IRJ utilisé par la DGFiP pour tester ses outils internes. Les options `--run_test` et `--run_all_tests` facilitent le processus de test de l'interprète (avec ou sans optimisations) et rapportent les erreurs de test dans un format pratique.

Les backends de Mlang sont également testés en utilisant le même format IRJ.

Lors de l'exécution de `--run_all_tests`, vous pouvez activer l'instrumentation de la couverture de code avec l'option `--code_coverage`. Une autre option intéressante est `--precision`, qui vous permet de choisir comment les nombres sont représentés pour le calcul de l'impôt. La valeur par défaut est `--precision double`, qui utilise la représentation en virgule flottante 64 bits IEEE754 et les opérations associées. C'est ce que la DGFiP utilise. L'option `--precision mpfr` vous permet d'utiliser des nombres en virgule flottante de 1024 bits pour une précision virtuellement infinie. Enfin, `--precision fixed<n>` utilise l'arithmétique en virgule fixe avec les grands entiers fournis par GMP. Les nombres en virgule fixe sont représentés avec le format de nombre Q et `<n>` est le nombre de bits fractionnaires. Les bits entiers sont illimités.

La DGFiP ne publie pas sa base de tests interne. Cependant, des cas de test aléatoires ont été créés pour les versions 2018 à 2024 du logiciel de l'impôt sur le revenu, dans le dossier tests. Le fait que Mlang passe ces tests signifie seulement qu'il reproduit fidèlement le calcul effectué par la DGFiP à l'aide de logiciels non publiés. Notamment, cela ne signifie pas que les sources M (publiées par la DGFiP) et les sources M++ (recréées à partir de sources non publiées) sont fidèles à la manière dont la loi dispose que les impôts doivent être calculés.

Pour vérifier que Mlang passe tous les tests aléatoires, invoquez simplement :

	make tests

Certains tests peuvent échouer en utilisant des paramètres de précision autres que ceux par défaut, même si le message d'erreur ne montre aucune différence entre la valeur attendue et la valeur calculée. C'est parce que nous contrôlons une différence de 0 entre le calculé et l'attendu, mais lors de calculs avec une précision plus élevée, une différence inférieure au plus petit flottant représentable peut apparaître. Pour réussir le test, nous avons fourni l'option de ligne de commande `--test_error_margin=0.0000001` pour vous permettre de définir la marge d'erreur que vous souhaitez tolérer lors de l'exécution des tests.

Si vous souhaitez lancer l'interprète mlang sur tous les tests d'une année d'imposition, lancez la commande :

	make YEAR=<2020 ou 2022> tests

ou

	make tests

avec l'année 2022 par défaut.

Si vous souhaitez lancer l'interprète mlang sur un test spécifique d'une année d'imposition, lancez la commande :

	make YEAR=<2020 ou 2022> TEST_ONE=<fichier test> test_one

ou

	make TEST_ONE=<fichier test> test_one

avec l'année 2022 par défaut. Les fichiers de tests sont stockés dans `tests/<année>/fuzzing`.

Si vous souhaitez tester la sortie de l'interprète sur une situation que vous avez créée, éditez votre propre fichier .m_test et lancez-le avec la commande :

	make YEAR=<2020 ou 2022> TEST_FILE=<chemin vers .m_test> make test_file

ou

	make TEST_ONE=<fichier test> test_one

avec l'année 2022 par défaut.

Si vous souhaitez tester la sortie du backend ML sur tous les tests d'une année d'imposition, lancez la commande :

	make YEAR=<2020 ou 2022> test_dgfip_c_backend

ou

	make TEST_ONE=<fichier test> test_dgfip_c_backend

avec l'année 2022 par défaut.

Veuillez lire le fichier `tests/README.md` pour un guide détaillé de ce qui se passe dans les fichiers d'entrée.

## Documentation

Le code OCaml est auto-documenté en utilisant le style ocamldoc. Elle est disponible ici. Vous pouvez également générer la documentation HTML en utilisant :
code Code

make doc

Pour consulter la documentation, ouvrez simplement le fichier `documentation/index.html` avec votre navigateur.

## M++

Afin de calculer correctement le montant des impôts d'un foyer fiscal, la DGFiP exécute le programme M plusieurs fois, en changeant à chaque fois les valeurs de certaines variables pour activer ou désactiver des parties du calcul.

La DGFiP n'a pas publié le code source de ce calcul itératif. Cependant, les auteurs de Mlang ont conçu un nouveau DSL appelé M++, utilisé pour décrire la logique de ce calcul itératif. Cette extension M est utilisée dans `m_ext/2018..2024` et a été utilisée avec succès pour le calcul de taxation primitif et correctif.

## Contributions

Le projet accepte les "pull requests". Il n'y a actuellement pas de guide de contribution formalisé ou de lieu de discussion centralisé sur le projet. Veuillez envoyer un e-mail aux auteurs si vous êtes intéressé :

david POINT michel1 AT dgfip POINT finances POINT gouv POINT fr
steven AT ocamlpro POINT com
alexandre POINT doussot AT ocamlpro POINT com
denis POINT merigoux AT inria POINT fr
raphael POINT monat AT lip6 POINT fr

Veuillez noter que le droit d'auteur de ce code appartient à la DGFiP et à l'Inria, ainsi que toutes les contributions à ce code.

N'oubliez pas d'utiliser make format avant de soumettre vos modifications (commit) afin de garantir un style uniforme.

## Sémantique formelle

Le dossier formal_semantics contient la formalisation du cœur du langage M, qui correspond approximativement à la représentation interne Mir dans Mlang. La formalisation de référence est écrite en Coq, dans le fichier semantique.v. Consultez l'[article de recherche](https://hal.inria.fr/hal-03002266) pour plus de détails.

## Licence

Le compilateur est publié sous la licence CeCILL (version 2.1).

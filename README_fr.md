# Le compilateur Mlang

[![état construction](https://gitlab.com/rmonat/mlang/badges/master/pipeline.svg)](https://gitlab.com/rmonat/mlang/-/commits/master)

Le langage M a été inventé par la Direction Générale des Finances Publiques (DGFiP) française,
équivalente à l'IRS aux États-Unis, afin de traduire le code des impôts en instructions
compréhensibles par un ordinateur. C'est un petit langage dédié qui s'appuie sur des 
déclarations de variables et des opérations arithmétiques. Ce projet est le fruit d'une
rétro-ingénierie du syntaxe et de la sémantique du M, à partir du [code](https://framagit.org/dgfip/ir-calcul) 
publié par la DGFiP.

## Clause de non-responsabilité 

Il n'y actuellement aucune garantie juridique de quelque sorte au sujet de la justesse
du code produit par le compilateur M ou bien des résultats produits par l'interprétateur 
de Mlang. Toutefois, les auteurs ont été en contact avec la DGFiP afin de valider
Mlang et le système réussit tous les tests en date de septembre 2020 avec la version
des sources correspondantes au calcul de l'impôt sur le revenu de 2018.

## Installation

Mlang est implémenté en OCaml. Afin de gérer les dépendances,
[installez opam](https://opam.ocaml.org/doc/Install.html) et passer en version
d'OCaml d'au moins 4.0.9. Afin de gérer le calcul à virgule flottant multi-précision,
vous aurez besoin également de la bibliothèque MPFR. Pour les distributions dérivées de
Debian, exécutez simplement : 

    sudo apt install libgmp-dev libmpfr-dev m4 perl python3 clang git opam

Si vous souhaitez générer des tests en utilisant le fuzzer, vous aurez besoin d'installer AFL :

    sudo apt install afl++ afl++-clang

Nous avons besoin d'OCaml >= 4.0.9. Si vous n'avez jamais utilisé opam, lancez :

    opam init
    opam update -y
    opam switch create 4.11.1 -y

Ensuite, vous pourrez installer les dépendances OCaml de Mlang en utilisant

    make deps

Cette commande récupère les sources M publiés par la DGFIP :

    git submodule update --init --recursive

Vous pouvez alors lancer 

    make build

pour construire le compilateur.

## Utilisation

Veuillez lire le fichier `m_specs/complex_case_with_ins_outs_2018.m_spec` en tant que guide pour ce
qui se passe dans cet exemple. 

Vous pouvez comparer ce qui se passe sur le 
[simulateur officiel](https://www3.impots.gouv.fr/simulateur/calcul_impot/2019/simplifie/index.htm)
en saisissant exactement les mêmes montants dans les mêmes cases. Tout doit être pareil.

Les variables saisie que vous voulez utiliser doivent être déclarées au préalable dans 
le fichier  `.m_spec`, dans la section `const`. Si vous entrez une variable dans la section
`saisie`, vous serez invité à saisir le montant à l'interprétation. Vous pouvez aussi 
modifier les variables que souhaitez voir restitué par l'interprétateur dans la section
`sortie`.

Si vous lancez `make quick_test`, Make vous montrera les options que Mlang utilise 
pour lancer un test simple de l'inteprétateur Mlang.

Please refer to the DGFiP's simulator for the meaning of the variables. Important variables are:
Veuillez vous reférez au simulateur DGFiP afin de connaître la signification des variables. 
Quelques variables importantes sont :

* `0AC` et `0AM`, qui doivent être valorisés à 1 pour un célibataire ou des personnes mariées respectivement;
* `1AJ` et `1BJ`, traitements et salaires des déclarants 1 et 2;
* `0CF`, nombre de personnes à charge (enfants);
* ...

Le déroulement de Mlang est configuré par un fichier de spécification (`.m_spec`), cf
le [README dédié](m_specs/README_fr.md) pour plus de détails.

Mlang a aussi besoin d'un fichier M++ afin de gérer le mécanisme
des "liquidations multiples" qui est nécessaire afin de calculer correctement 
l'impôt sur le recenu. Le fichier `mpp_specs/2018_6_7.mpp` correspond à la partie 
non publiée du code DGFiP pour la version 2018 des sources publiées dans `ir-calcul`.

Si vous voulez vérifier la sortie de l'interprétateur pour une situation donnée,
vous pouvez créer votre propre `.m_spec`et le lancer avec la commande :

    YEAR=<2018 or 2019> M_SPEC_FILE=<path to .m_spec> make quick_test

Afin de produire des fichiers sources prêts à l'utilisation
pour calculer l'impôt dans votre application, consultez le 
[README dédié](examples/README_fr.md)

## Tests

Mlang est testé en utilisant le format de fichier `FIP` utilisé par la DGFiP
pour les tests de leur outillage interne. Les options `--run_test` et 
`--run_all` facilitent le processus de test avec l'interprétateur (avec ou 
sans optimisations) et rapportent les erreurs dans un format pratique.

Les modules de génération de code Mlang sont aussi testés avec ce même format
`FIP`, voir par exemple `examples/python/backend_test`

Quand on lance `--run_all_tests`, vous pouvez activer la mesure de la couverture
de code avec le paramètre `--code_coverage`. Une autre option intéressante est 
`--precision`, qui permet de choisir la représentation des nombre lors du calcul 
de l'impôt. Par défaut c'est `--precision double`, qui utilise la représentation 
64 bits IEEE754 de virgule flottante et les opérations associées. C'est ce qui 
est utilisé par la DGFiP. Le paramètre `--precision mpfr` permet d'utiliser des 
nombres à virgule flottant de 1024 bits pour une précision quasi-infini. Enfin,
la précision, `--precision fixed<n>` utilise de l'arthmétique à virgule fixe
avec de grands entiers fournis par GMP. Les nombres à virgule fixe sont 
représentés au [format de nombre Q](https://en.wikipedia.org/wiki/Q_(number_format) 
et `<n>` est le nombre de bits fractionnelles. Les bits d'entiers sont sans limite.

La DGFiP ne publie pas sa base de tests interne. Toutefois des tests aléatoires 
ont été générés pour la version du logiciel qui traite des revenus de 2018 dans
le répertoire `random_tests`. Le fait que Mlang réussit ces tests signifie 
seulement qu'il reproduit fidèlement le calcul de la DGFiP en utilisant des
le logiciel non-publié. Notamment, cela ne signifie pas que les sources M 
(publiés par la DGFiP) et les sources M++ (recréés depuis les sources non 
publiés) sont fidèles à la législative en matière de calcul de l'impôt.

Pour vérifier que Mlang réussit tous les tests aléatoires, lancez simplement

    make tests

Quelques tests pourraient échouer en utilisant des valeurs de précision autres
que ceux par défaut, même si le message d'erreur n'affiche aucune différence entre
la valeur attendue et la valeur obtenue. Cela arrive car nous vérifions un écart 
de 0 entre l'attendue et l'obtenue mais quand on fait des calculs à plus haute 
précision, une différence inférieur au plus petit montant à virgule flottant qui
peut être représenté peut apparaître. Pour passer le test, nous avons fourni 
le paramètre de ligne de commande `--test_error_margin=0.0000001` afin de vous 
permettre de définir la marge d'erreur que vous souhaitez tolérer pendant les 
tests. 

## Documentation

The OCaml code is self-documented using `ocamldoc` style. You can generate the HTML
documentation using

    make doc

To browse the documentation, just open the file `doc.html` with your browser. Here
is a high-level picture describing the architecture of the compiler:

<center>
<img src="doc/architecture.png" alt="Architecture" height="300"/>
</center>

First, the code is parsed into AST (both for M and M++). Then, the AST are
desugared into M and M++ intermediate representations. BIR stands for Backend
IR, and collects the result of inlining the M code inside M++. OIR is the
Optimization IR, which is a CFG-form of BIR.

## Known Limitations

The code released by the DGFiP is not complete as of September 2020. Indeed,
in order to correctly compute the amount of taxes for a fiscal household, the DGFiP
executes the M program several times, each time changing the values of some variables
to enable or disable parts of the computation.

The DGFiP has not published the source code of this iterative computation. However,
the authors of Mlang have come up with a new DSL called M++, used for describing
the logic of this iterative computation. Currently, the authors have transcribed
the unpublished source code into the `mpp_specs/2018_6_7.mpp` file, which has been tested only
for the computation of taxes for the 2018 income.

## Contributions

The project accepts pull requests. There is currently no formalized contribution
guide or centralized discussion place about the project. Please email the authors
if you are interested:

  denis DOT merigoux AT inria DOT fr
  raphael DOT monat AT lip6 DOT fr

Please note that the copyright of this code is owned by Inria; by contributing,
you disclaim all copyright interests in favor of Inria.

Don't forget format to use `make format` before you commit to ensure a uniform style.

## Formal semantics

The `formal_semantics` folder contains the formalization for the core of the
M language, that roughly corresponds to the `Mir` internal representation in Mlang.
The reference formalization is written in Coq, in file `semantique.v`.
See [the research paper](https://hal.inria.fr/hal-03002266) for
more details.

## License

The compiler is released under the GPL license (version 3).

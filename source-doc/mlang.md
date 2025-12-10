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

### Traduction

### Pré-traitement

Le prétraitement est une opération purement syntaxique.
Son but est triple :
- éliminer les constructions relatives aux applications non-sélectionnées ;
- remplacer les constantes par leur valeur numérique ;
- remplacer les expressions numériques débutant par `*somme*` avec des
  additions ;
- éliminer les `<multi-formule>`s en les remplaçant par des séries de `<formule>`s.

Toute substitution transformant un programme M syntaxiquement valide en un
texte ne correspondant à aucun programme M provoque l'échec du traitement.

#### Cas des applications

On élimine du programme M:
- les déclarations des applications non-sélectionnées ;
- les déclarations des enchaîneurs ne spécifiant pas une application
  sélectionnée ;
- les déclarations des règles ne spécifiant pas une application
  sélectionnée ;
- les déclarations des vérifications ne spécifiant pas une application
  sélectionnée ;
- les déclarations des cibles ne spécifiant pas une application sélectionnée.

Dans les déclarations des règles spécifiant une application sélectionnée, on
élimine les enchaîneurs qui ne sont plus déclarés dans le programme M obtenu.

#### Cas des constantes

Pour prétraiter un programme, on le parcourt du début à la fin. Pour chaque
`<variable>` rencontrée, si elle correspond à une contante défini précédemment,
alors il est remplacé dans le programme par la valeur numérique correspondante.

Un intervalle de la forme `<naturel:début>..<symbole:const>` est
converti par substitution de la constante `const` en l'intervalle
`<naturel:début>..<naturel:fin>`, avec `fin` le naturel correspondant
à `const`.

*Exemple* : considérons le programme suivant :
```
fin : const = 10;
… 
pour i = 9-fin :
  Bi = Bi + fin;
```
   
Par substitution de la constante `fin`, il est remplacé dans un premier temps
remplacée par le programme :

```
fin : const = 10;`
… 
pour i = 9..10 :
  Bi = Bi + fin;
```

puis dans un second temps par le programme :
```
fin : const = 10;
…
pour i = 9..10 :
  Bi = Bi + 10;
```

#### Interprétation des indices

Pour rappel, les *indices* ont la forme suivante :
```
<indices> ::= <indice> ; …
<indice> ::= <minuscule> = <intervalle> , …
```

avec :
* `<minuscule> ::= [a-z]`
* `<majuscule> ::= [A-Z]`
* `<intervalle> ::= <majuscule>+ | <majuscule> .. <majuscule> | <naturel> (.. <naturel> | - <variable>)?`

Chaque `<indice>` associe à une lettre minuscule une série de chaînes de
caractères de même taille.
Cette série est composée de la succession de chaque
<intervalle> à droite du symbole `=`.

Les séries associées aux <intervalles> sont définis comme suit :
* `<majuscule>+` est la série composée de chacune des majuscules prises
  séparément, donc de taille 1 (*par exemple* : `AXF` représente la série `A`, `X`, `F`);
* `<majuscule:/début/>****<majuscule:/fin/>` est la série composée de toutes
  les majuscules comprises entre /début/ et /fin/, bornes comprises, suivant
  l'ordre alphabétique, donc de taille 1 (*par exemple* : `A..D` représente la série `A`, `B`, `C`, `D`);
* `<naturel:début>..<naturel:fin>` est la série composée de tous les
  nombres naturels entre `début` et `fin`, bornes comprises, la taille des
  éléments étant égale à la taille du plus grand naturel en base 10; les
  naturels trop petits pour avoir la taille requise  sont complétés par des 0 à
  gauche (*par exemple* : `9..11` représente la série `09`, `10`, `11`);
* `<naturel>-<variable>` est converti lors du prétraitement des constantes
  en un intervalle de la forme `<naturel>..<naturel>`.

#### Cas des expressions `somme(…)`

Pour une expression numérique
`somme ( <indice:ind>: <expression numerique:expr> )`,
l'expression *expr* sera remplacée par la somme des expressions construites
ainsi : pour chaque chaîne de caractères de la série introduite par *ind*, les
symboles apparaîssant dans *expr* sont remplacés par ceux dans lesquels la
lettre minuscule de /ind/ est substituéé par la chaine de caractère.
%
La somme ainsi générée est parenthésée.

Une expression numérique
`somme ( <indice:ind> ; <indices:inds> : <expression numérique:expr> )`,
est remplacée par la somme des expressions
*`somme`*` <indices:inds> : expr'> )` avec `expr'` prenant sa valeur
dans la série `sub(ind, expr)`.
%
La procédure est ensuite appliquée récursivement à cette somme.
%
La somme ainsi générée est parenthésée.
%
Notons que les sous-sommes récursivement produites n'ont pas besoin de l'être
car l'addition est associative.

On applique cette transformation à toutes les expressions `somme(…)` tant
qu'il en existe dans le programme.

**Exemple**. Considérons l'expression suivante :

* `somme(i = XZ ; j = 9..10 : Bi + Bj)`

Elle est dans un premier temps remplacée par la somme suivante :

* `(somme(j = 9..10 : BX + Bj) + somme(j = 9..10 : BZ + Bj))`

puis dans un second temps par la somme :

* `(BX + B09 + BX + B10 + BZ + B09 + BZ + B10)`

On remarque que seule l'expression globale est parenthésée car l'addition est
associative.

#### Cas des multi-formules

On commence par substituer toutes les expressions `somme(…)` apparaissant dans
les multi-formules.
%
Puis on transforme les multi-formules en séries de formules en suivant la
méthode décrite ci-dessous.

Pour chaque multi-formule rencontrée, de la forme `pour <indices> : <formule>`,
dès que les constantes apparaîssant dans les <indices> ont été substitués, on
remplace cette multi-formule par la série de formules générée de la manière
suivante.

Pour une multi-formule `pour <indice:ind> : <formule:f>`, la
formule `f` sera remplacée par la série des formules construites ainsi : pour
chaque chaîne de caractères de la série introduite par `ind`, les symboles
apparaîssant dans `f` sont remplacés par ceux dans lesquels la lettre
minuscule de `ind` est substituéé par la chaïne de caractère.
%
On notera `sub(ind, f)` la série constituée des formules ainsi régérées.

Une multi-formule `pour <indice:ind> ; <indices:inds> : <formule:f>`,
est remplacée par la série des multi-formules
`pour <indices:inds> : f'>` avec `f'` prenant sa valeur
dans la série `sub(ind, f)`.
%
La procédure est ensuite appliquée récursivement
à ces multi-formules.

**Exemple**. Considérons la multi-formule suivante :
```
pour i = XZ ; j = 9..10 :
  Bi = Bi + Bj;
```

Elle est dans un premier temps remplacée par la série suivante :
```
pour j = 9..10 :
  BX = BX + Bj;
 
pour j = 9..10 :
  BZ = BZ + Bj;
```

Puis dans un second temps par la série des formules :
```
BX = BX + B09;
BX = BX + B10;
BZ = BZ + B09;
BZ = BZ + B10;
````

### Vérification de cohérence

### Traitement
* Traduction du code M dans l'arbre de syntaxe abstraite M_AST.
* Extension des constructions.
* Vérification de cohérence.
* Selon le mode : 
  * **Interpreteur** Lecture du fichier IRJ et interpretation du code.
  * **Transpilation** Ecriture du code C équivalent au code M.

(syntax)=

# La syntaxe du M

## Programme M et applications

Un programme M est formé d'une suite de caractères codés sur 8 bits.
%%
Les 128 premiers codes de caractères correspondent aux codes ASCII.
%%
Un programme M est constitué d'une suite d'éléments parmi lesquels on compte :
* des déclarations d'applications ;
* des définitions de constantes ;
* des déclarations d'enchaîneurs ;
* des définitions de catégories de variables ;
* des déclarations de variables ;
* des déclarations d'erreurs ;
* des déclarations de fonctions externes ;
* des définitions de domaines de règles ;
* des définitions de domaines de vérifications ;
* des déclarations de sorties ;
* des règles ;
* des vérifications ;
* des fonctions ;
* des cibles.

Un programme M peut définir plusieurs applications.
%%
Pour être traité (compilation, interprétation, etc.), il nécessite la donnée
d'une liste d'applications.
%%
Cette liste est typiquement fournie comme argument du compilateur, de
l'interpréteur ou de tout autre outil de traitement.
%%
Cette liste sera appelée la liste des applications sélectionnées.

## Définitions liminaires
Les formes de Backus-Naur étendues sont utilisées pour préciser la morphologie
du langage.
%%
Un lexème est une suite de caractères et chaque forme est susceptible d'en
reconnaître un ensemble.

Ces formes sont étendues avec les notations suivantes :

* `<non-terminal:identifiant>` représente le non-terminal, pour lequel la
chaîne de caractères qu'il reconnaît est représentée par identifiant ;
%%
* `forme 1 ^ … ^ forme n` qui représente une suite de lexèmes correspondants
aux formes 1 à n, dans n'importe quel ordre;
%%
* et : `(forme 0) séparateur …` qui représente une liste non-vide de lexèmes
reeconnus par la forme 0, tous séparés par des lexèmes correspondants au
séparateur.

Les lexèmes suivants sont les mots réservés :
**BOOLEEN**,
**DATE_AAAA**,
**DATE_JJMMAAAA**,
**DATE_MM**,
**ENTIER**,
**REEL**,
**afficher**,
**afficher_erreur**,
**aiguillage**,
**ajouter**,
**alias**,
**alors**,
**anomalie**,
**application**,
**apres**,
**argument**,
**arranger_evenements**,
**attribut**,
**autorise**,
**avec**,
**base**,
**calculable**,
**calculee**,
**calculer**,
**cas**,
**categorie**,
**champ_evenement**,
**cible**,
**const**,
**dans**,
**dans_domaine**,
**discordance**,
**domaine**,
**enchaineur**,
**entre**,
**erreur**,
**espace**,
**espace_variables**,
**et**,
**evenement**,
**evenements**,
**exporte_erreurs**,
**faire**,
**filtrer**,
**finalise_erreurs**,
**finquand**,
**finsi**,
**fonction**,
**increment**,
**indefini**,
**indenter**,
**informative**,
**iterer**,
**leve_erreur**,
**meme_variable**,
**nb_anomalies**,
**nb_bloquantes**,
**nb_categorie**,
**nb_discordances**,
**nb_informatives**,
**neant**,
**nettoie_erreurs**,
**nettoie_erreurs_finalisees**,
**nom**,
**non**,
**numero_compl**,
**numero_verif**,
**ou**,
**par_defaut**,
**pour**,
**puis_quand**,
**quand**,
**reference**,
**regle**,
**restaurer**,
**restituee**,
**resultat**,
**saisie**,
**si**,
**sinon**,
**sinon_si**,
**sortie**,
**specialise**,
**stop**,
**tableau**,
**taille**,
**trier**,
**type**,
**un**,
**valeur**
**variable**,
**verif**,
**verifiable**,
et **verifier**.

Les lexèmes numériques sont définis comme suit :
* `<naturel> ::= [0-9] [0-9 _]*`
* `<réel> ::= <naturel> (. <naturel>)?`
* `<symbole>` est la forme `[a-z A-Z 0-9 _]+` dont sont exclus `<naturel>`s et
les mots réservés ;
* `<variable>` est un `<symbole>` que l'on distingue pour représenter les
mots pouvant prendre le nom d'une constante.

On définit également les atomes numériques, prenant une valeur soit par un
nombre littéral, soit par un symbole représentant une constante ou une
variable :

* `<atome naturel> ::= <naturel> | <variable>`
* `<atome reel> ::= <réel> | <variable>`

Les chaînes de caractères littérales ont la forme suivante :
* `<chaîne> :: = " [^ "]* "`

### Déclaration une d'application

Les *déclarations d'applications* ont la forme suivante :
```
application <symbole>;
```

où `<symbole>` est le nom de l'application
déclarée.

### Définition d'une constante

Les *définitions de constantes* ont la forme suivante :
```
<symbole> : const = <atome réel> ;
```

Le symbole est le nom de la constante.

### Déclaration d'un enchaîneur
Les *déclarations d'enchaîneurs* ont la forme suivante :
```
enchaineur <symbole> : <symboles> ;
```

avec :
`<symboles> ::= <symbole> , …` représente l'ensemble des applications incluant cet enchaîneur.
%%
Le `<symbole>` de l'enchaineur est le nom de l'application déclarée.
 

### Définition d'une catégorie de variables

Les *définitions de catégories de variables* ont la forme suivante :
```
variable (saisie | calculee) <nom> (: attribut <symboles>)? ;
```

avec :

* `<nom> ::= <symbole>+` est le nom de la catégorie de variables;
* `<symboles> ::= <symbole> , …` représente l'ensemble de ses attributs.

### Déclaration d'une variable

La forme des *types de variables *est comme suit :

```
<type variable> ::= BOOLEEN | DATE_AAAA | DATE_JJMMAAAA | DATE_MM | ENTIER | REEL
```

#### Variable saisie

Les *déclarations de variables saisies* ont la forme suivante :

```
<symbole> : saisie <catégorie> <attributs> alias <symbole> : <chaîne> type <type variable> ;
```

avec :

* `<catégorie> ::= <symbole>+`
* `<attributs> ::= <attribut> restituee? <attribut>`
* `<attribut> ::= <symbole> = <atome naturel>`

Le `<symbole>` est le nom de la variable.

#### Variable calculée

Les *déclarations de variables calculées* ont la forme suivante :
```
<symbole>
: (table [ <atome naturel> ])? calculee (base? ^ restituee?)
: <chaîne> type <type variable> ;
```

Le <symbole> est le nom de la variable. Si c'est un tableau, l'<atome naturel>
est sa taille.

### Déclaration d'une erreur

Les *déclarations d'erreurs* ont la forme suivante :
```
<symbole> : <type erreur> : <chaîne> : <chaîne> : <chaîne> : <chaîne> : <chaîne> ;
```
avec :
* `<type erreur> ::= anomalie | discordance | informative`

Le symbole est le nom de l'erreur.
%%
% A faire: documenter les <chaine>s !

### Déclaration d'une fonction externe

Les *déclarations de fonctions* externes ont la forme suivante :

```
<symbole> : fonction <atome naturel> ;
```

Le `<symbole>` est le nom de la fonction, et l'<atome naturel> son arité.

### Définition d'un domaine de règles

Les *défintions de domaines de règles* ont la forme suivante :

```
domaine regle <nom> <paramètres> ;
```

avec :
* `<nom> ::= <symbole>+` est le nom du domaine de règles.
* `<paramètres> ::= (: specialise <noms>)? ^ (: calculable)? ^ (: par_defaut)?
* <noms> ::= <nom> , …` représente l'ensemble des domaines de règles que spécialise ce domaine.

### Définition d'un domaine de vérifications

Les *définitions de domaines de vérifications* ont la forme suivante :

```
domaine verif <nom> <paramètres> ;
```

avec :
* `<nom> ::= <symbole>+` est le nom domaine de vérifications que spécialise ce domaine.
* `<paramètres> ::= (: specialise <noms>)? ^ (: autorise <catvars>)? ^ (: par_defaut)? ^ (: verifiable)?`
* `<noms> ::= <nom> , …`
* `<catvars> ::= * | saisie (* | <nom>) | calculee (* | base)?` représente les catégories des variables autorisées à êtres utilisées dans les vérifications de ce domaine.

### Déclaration d'une sortie

Les *déclarations de sorties* ont la forme suivante :
```
sortie ( <symbole> ) ;
```

### Indices

Les *indices* ont la forme suivante :

```
<indices> ::= <indice> ; …
<indice> ::= <minuscule> = <intervalle> , …
```

avec :
* `<minuscule> ::= [a-z]`
* `<majuscule> ::= [A-Z]`
* `<intervalle> ::= <majuscule>+ | <majuscule> .. <majuscule> | <naturel> (.. <naturel> | - <variable>)?`


### Expressions

Les *expressions atomiques* ont la forme suivante :

```
<atomique booléen> ::=
( <expression booléenne> )
| <expression numérique> (<= | >= | < | > | != | =) <expression numérique>
| <expression numérique> non? dans ( <intervalles numériques> )
| (present | non_present) ( <symbole> ([ <expression numérique> ])? )

<atomique numérique> ::=
| <expression numerique>
| <atome réel>
| <symbole> [ <expression numérique> ]
| <symbole> ( (<expression numerique> , …)? )
| somme ( <indices> : <expression numerique> )
| si ( <expression boolenne> )
  alors ( <expression numerique> )
  (( sinon <expression numerique> ))?
  finsi
```

avec :
* `<expression ou> ::= <expression et> | <expression ou> ou <expression ou>`
* `<expression et> ::= <expression non> | <expression et> et <expression et>`
* `<expression non> ::= <atomique booléen> | non <expression non>`
* `<expression + -> ::= <expression * /> | <expression + -> (+ | -) <expression + ->`
* `<expression * /> ::= <expression -> | <expression * /> (* | /) <expression * />`
* `<expression -> ::= <atomique numérique> | - <expression ->`
* `<intervalles numériques> ::= <intervalle numérique> , …`
* `<intervalle numérique> ::= <expression numérique> (.. <expression numérique>)?`


Les `formules` ont la forme suivante :
```
<formule> ::=
    <symbole> ([ <expression numérique> ])? = <expression numérique>
```

Les `multi-formules` ont la forme suivante :
```
<multi-formule> ::= pour <indices> : <formule>
```

### Instructions

Les *instructions* ont la forme suivante :
```
<instruction> ::=
  <formule>
  | <multi-formule> 
  | si <expression~booléenne>
    alors <instruction>+
    (sinon <instruction>+)?
    finsi 
  | calculer domaine <symbole>+;
  | calculer enchaineur <symbole>;
  | calculer cible <symbole>
    (: avec <atome~réel> , …)?;
  | verifier domaine <symbole>+
    (: avec <expression~booléenne>);
  | (afficher | afficher__erreur) <affichable>+;
  | iterer 
    : variable <symbole>
    : categorie <symbole>+ , …
    (: avec <expression~booléenne>)? 
    : dans ( <instruction>+ ) 
  | restaurer 
    ( 
    : <symbole> , … 
     | : variable <symbole> : categorie <symbole>+ , … 
       (: avec <expression~booléenne>)? 
    )+ 
    : apres ( <instruction>+ ) 
  | leve__erreur <symbole> <symbole>?;
  | (
      nettoie__erreurs
      | exporte__erreurs
      | finalise__erreurs
    ) ;
  | aiguillage (nom)? (<symbole>) : (<multiple_cas>)
  | stop <type_stop>?
```

avec :

```
<affichable> ::=
  <chaîne> 
  | `*(*` <expression~numérique> `*)*`
    (`*:*` <atome~naturel> (`*..*` <atome~naturel>)?)? 
  | (`*nom*` | `*alias*`) `*(*` <symbole> `*)*` 
  | `*indenter*` `*(*` <atome~naturel> `*)*`

<multiple cas> ::= <cas>*

<cas> ::=
  cas <atome reel> : <instruction>+
  | cas indefini : <instruction>+
  | cas <symbole> : <instruction>+
  | par_default : <instruction>+

<type_stop> ::=
  application
  | cible
  | fonction
  | <symbole>
```

Les *instructions simples* ont la forme suivante :

```
<instruction simple> ::= 
  <formule> 
  | <multi-formule> 
  | `*si*` <expression~booléenne>
    `*alors*` <instruction~simple>+
    (`*sinon*` <instruction~simple>+)?
    `*finsi*` 
  | (`*afficher*` | `*afficher__erreur*`) <affichable>+ `*;*`
```

### Définition d'une règle

Les *règles* ont la forme suivante :
```
<regle> ::= 
 regle <symbole>.. <naturel> :
    ( 
      application : <symbole> , … ; 
      ^^ (enchaineur : <symbole> , … ;)? 
      ^^ (variable temporaire : <déclaration> , … ;)? 
    ) 
    <instruction~simple>+
```

avec :

* `<déclaration> ::= <symbole> ([ <atome~naturel> ])?`

### Définition d'une vérification

Les *vérifications* ont la forme suivante :

```
<vérification> ::= 
  verif <symbole> <naturel> : 
  application : <symbole> , … ; 
  si <expression~booléenne>
  alors erreur <symbole> <symbole>?
  ;
```
### Définition d'une fonction

Les *fonctions* ont la forme suivante :
```
<fonction> ::= 
   fonction <symbole> : 
    ( 
      application : <symbole> , … ; 
      ^^ (variable temporaire : <déclaration> , … ;)? 
      ^^ (argument : <symbole> , … ;)? 
      ^^ resultat : <symbole> ; 
    ) 
    <instruction~simple>+
```

avec :
* `<déclaration> ::= <symbole> ([ <atome~naturel> ])?`

### Définition d'une cible

Les *cibles* ont la forme suivante :

```
<cible> ::= 
  cible <symbole> : 
  ( 
    application : <symbole> , … ; 
    ^^ (enchaineur : <symbole> , … ;)? 
    ^^ (variable temporaire : <déclaration> , … ;)? 
    ^^ (argument : <symbole> , … ;)?    
  ) 
  <instruction>+
```

avec :
* `<déclaration> ::= <symbole> ([ <atome~naturel> ])?`

### Commentaires

Les commentaires sont précédés du caractère `#`.
%%
Il est possible d'inclure des commentaires multi-ligne avec les délimiteurs `#{` et
`}#`.

Exemple:

```
# Ceci est un commentaire sur une ligne
#{ Ceci est un commentaire 
   sur plusieurs lignes. }#
```

## Prétraitement

Le prétraitement est une opération purement syntaxique.
%%
Son but est triple :
- éliminer les constructions relatives aux applications non-sélectionnées ;
- remplacer les constantes par leur valeur numérique ;
- remplacer les expressions numériques débutant par `*somme*` avec des
  additions ;
- éliminer les `<multi-formule>`s en les remplaçant par des séries de `<formule>`s.

Toute substitution transformant un programme M syntaxiquement valide en un
texte ne correspondant à aucun programme M provoque l'échec du traitement.

### Cas des applications

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

### Cas des constantes

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

### Interprétation des indices

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
%%
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

### Cas des expressions `somme(…)`

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

### Cas des multi-formules

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

(syntax)=

# La syntaxe du M

## Programme M et applications

Un programme M est formé d'une suite de caractères codés sur 8 bits.
Les 128 premiers codes de caractères correspondent aux codes ASCII.
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
Pour être traité (compilation, interprétation, etc.), il nécessite la donnée
d'une liste d'applications.
Cette liste est typiquement fournie comme argument du compilateur, de
l'interpréteur ou de tout autre outil de traitement.
Cette liste sera appelée la liste des applications sélectionnées.

## Définitions liminaires
Les formes de Backus-Naur étendues sont utilisées pour préciser la morphologie
du langage.
Un lexème est une suite de caractères et chaque forme est susceptible d'en
reconnaître un ensemble.

Ces formes sont étendues avec les notations suivantes :

* `<non-terminal:identifiant>` représente le non-terminal, pour lequel la
chaîne de caractères qu'il reconnaît est représentée par identifiant ;
* `forme 1 ^ … ^ forme n` qui représente une suite de lexèmes correspondants
aux formes 1 à n, dans n'importe quel ordre;
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
Il est possible d'inclure des commentaires multi-ligne avec les délimiteurs `#{` et
`}#`.

Exemple:

```
# Ceci est un commentaire sur une ligne
#{ Ceci est un commentaire 
   sur plusieurs lignes. }#
```

(valeurs)=
## Les valeurs

Les variables en M prennent soit leur valeur sur les flottants, soit ont la
valeur `indefini`.
Les valeurs de type booléen sont représentées par les flottants `0` et
`1`, respectivement pour `faux` et `vrai`.

Les résultats suivants peuvent être observés en exécutant le script disponible
dans l'exemple sur le {ref}`exemples/valeurs`.

### Calcul booléen 

Les opérations booléennes standard (`et`, `ou`, `non`) comportent sur les
flottants de façon standard.
Toute valeur flottante différente de `0` sera considerée comme `vrai`e dans
le cas d'un calcul booléen (`10 ou 0 = 1`).
Les calculs booléens impliquant la valeur `indefini` ont un comportement
spécifique :

* `indefini ou indefini = indefini`
* `indefini ou b = (0 si b = 0, 1 sinon)`
* `b ou indefini = (0 si b = 0, 1 sinon)`
* `indefini et indefini = indefini`
* `b et indefini = indefini`
* `indefini et b = indefini`
* `non indefini = indefini`

Les comparaisons (`=`, `<`, `>`, `<=`, `>=`) renvoient soit `0` soit `1`
lorsque des valeurs definies sont comparées.
Si l'une des valeurs comparée est `indéfini`e, alors le résultat est également
`indefini`.

**Note**: même l'égalité `indefini = indefini` renvoie `indefini`.
Pour vérifier si une valeur est définie ou non, il faut utiliser la fonction
`present` qui renvoie `0` si la valeur est indéfinie et `1` sinon.

### Calcul numérique

Les opérations arithmétiques standard (`+`, `-`, `*`, `/`) se comportent sur 
les flottants de façon standard, à l'exception de la division d'un flottant par
zero qui renvoie toujours `0`.
Les calculs impliquant la valeur `indefini` ont un comportement spécifique :

* `indefini + indefini = indefini`
* `indefini + n = n`
* `n + indefini = n`
* `indefini - indefini = indefini`
* `indefini - n = -n`
* `n - indefini = n`
* `indefini * indefini = indefini`
* `indefini * n = indefini`
* `n * indefini = indefini`
* `indefini / indefini = indefini`
* `indefini / n = indefini`
* `n / indefini = indefini`

Pour résumer, seules les opérations d'addition et de soustraction avec une
valeur renvoient autre chose que `indefini`.


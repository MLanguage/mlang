(syntax_m)=

# Le langage M

## Introduction

Un programme M se caracterise par la définition successive :
* de types de variable avec leurs attributs ;
* de domaines et d'événements ;
* d'espaces de variables ;
* d'applications ;
* de variables et de constantes ;
* de règles de calcul associées ou non à une application.

L'exécution d'un tel programme pour une application donnée correspond à
exécuter l'ensemble des règles de calcul associée la dite application.
%%
L'ordre d'exécution des règles de calcul dépend des dépendances entre les
variables. Si une variable est affectée dans une règle, alors cette règle
sera exécutée avant toutes celles utilisant la valeur de la dite variable.
%%
Si une variable est lue et écrite cycliquement, l'ordre d'exécution des règles
n'est pas garanti.

## Définitions par défaut

Les types de variables, leurs attributs, les domaines et les événements
sont historiquement des mots clés propres au langage M.
%%
Il est aujourd'hui possible de les définir à la main.

## Aperçu de la syntaxe

Voici un exemple simple de programme M minimal.

```
# Fichier: test.m

# On définit un espace de variable global dans lequel nos variables
# seront stockées par défaut.
espace_variables GLOBAL : par_defaut;

# On définit deux domaines obligatoires:
# * un domaine pour les règles;
# * un domaine pour les vérificateurs.
domaine regle mon_domaine_de_regle: par_defaut;
domaine verif mon_domaine_de_verifications: par_defaut;

# On définit une ou plusiseurs application pour nos règles.
application mon_application;

# On définit une cible, un ensemble d'instructions à exécuter.
cible hello_world:
application : mon_application;
afficher "Bonjour, monde!\n";
```

Cet exemple jouet n'utilise ni ne définit aucune variable, les domaines
et les espaces de variables ne sont pas utilisés ici mais ils seront nécessaires
plus tard.
%%
Pour essayer notre exemple, nous allons créer un fichier 'test.irj' avec le
contenu suivant :
%%
```
#NOM
MON-TEST
#ENTREES-PRIMITIF
#CONTROLES-PRIMITIF
#RESULTATS-PRIMITIF
#ENTREES-CORRECTIF
#CONTROLES-CORRECTIF
#RESULTATS-CORRECTIF
##
```
Le détail de la syntaxe irj peut être sur la page dédiée : {ref}`syntax_irj`

Enfin, on peut lancer mlang.

```
 $ mlang --without_dfgip_m test.m -A mon_application --mpp_function hello_world --run_test test.irj
 Parsing: completed!
 Bonjour, monde!
 [RESULT] my-examples/m/scratch/fuzzer_1210z.m_test
 [RESULT] No failure!
 [RESULT] Test passed!
```

## Définition de variables

Les variables sont divisées en deux catégories.
* Les variables `saisie`s sont les variables d'entrées du
programme M.
* Les variables `calculee`s sont les variables sur lesquelles seront
calculées les données.

Chacune de ces catégories principales doivent être dotées d'attributs,
un ensemble d'entiers constants pour une variable donnée.
%%
Ainsi, on peut rajouter à notre programme test les lignes suivantes :
```
variable saisie : attribut mon_attribut;
variable calculee : attribut mon_attribut;
X : saisie mon_attribut = 0 alias ALIAS_DE_X : "Cette variable s'appelle X";
Y : calculee mon_attribut = 1 : "Cette variable s'appelle Y";
```

Notez que la variable `X` a un alias `ALIAS_DE_X`.
%%
Toutes les variables saisies doivent être parées d'un alias qui peut être
utilisé de la même façon que son nom original.
%%
Cet alias existe initialement pour faire le lien entre la variable utilisée
dans le M (nom original) et le code de la variable dans le formulaire de
déclaration de l'impot sur le revenu tel qu'on le retrouve aujourd'hui sur
le site de déclaration (1AP, 8TV, ...).

Ajoutons une nouvelle cible à notre calcul :
```
cible hello_world2:
application : mon_application;
afficher "Bonjour, monde, X = ";
afficher (X);
afficher " !\n";
Y = X + 1;
afficher "Y = ";
afficher (Y);
afficher " !\n";
```

Et lançons le calcul :
```
 $ mlang --without_dfgip_m test.m -A mon_application --mpp_function hello_world2 --run_test test.irj
Parsing: completed!
Bonjour, monde, X = indefini !
Y = 1 !
[RESULT] fuzzer_1210z.m_test
[RESULT] No failure!
[RESULT] Test passed!
```

Pour comprendre la valeur finale de Y, référez-vous à la
section {ref}`arithmetique`.
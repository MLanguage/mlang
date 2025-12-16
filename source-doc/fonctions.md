(fonctions)=
# Les fonctions

Voici la liste de toutes les fonctions standard du M.
Leur utilisation est présentée dans l'exemple sur 
{ref}`exemples/fonctions`.

## abs(X)

Cette fonction prend un argument et renvoie sa valeur absolue.
Si `X` est `indefini`, alors `abs(X) = indefini`.

## afficher(X) / afficher "texte"

Cette fonction affiche sur la sortie standard la valeur de l'expression en argument.

## arr(X)

Cette fonction prend un argument et renvoie l'entier le plus
proche, ou `indefini` s'il est `indefini`.

## attribut(X, attribut)

Cette fonction prend un argument `X` et un nom d'attribut `a` et retourne
la valeur associée à l'attribut `a` de `X`.

## champ_evenement(X, champ)

Cette fonction prend un argument `X` et un nom de champ d'événement `champ`.
La valeur `X` correspond à l'identifiant de l'événement. %TODO: référence chap événement
Si le `champ` correspond à une valeur, `champ_evenement` la retourne. Si
le `champ` correspond à une variable, `champ_evenement` retourne sa valeur.

**Note** : `champ_evenement` peut être utilisée pour assigner des valeurs et
des références à des événéments. Exemple:
```
evenement
: valeur ev_val
: variable ev_var;

# Associe la valeur 42 au champ ev_val de l'événement 0.
champ_evenement (0,ev_val) = 42;
# Associe la variable X au champ ev_var de l'événement 0.
champ_evenement (0,ev_var) reference X;
```

## inf(X)

Cette fonction prend un argument et renvoie l'entier inferieur 
le plus proche, ou `indefini` s'il est `indefini`.

## max(X, Y)

Cette fonction prend deux arguments et renvoie la plus grande
valeur des deux.
Si les deux valeurs sont `indefini`es, alors la fonction renvoie
`indefini`. 
Si `X` est défini et `Y` est `indefini`, alors `max(X,Y) = max(X, 0)`. De même,
si `X` est `indéfini` et `Y` est defini, alors `max(X,Y) = max(0, Y)`.

## meme_variable(X, Y)

Cette fonction prend en argument deux variables et vérifie s'il s'agit de la même
variable.
Elle est notamment utilisée dans les iterateurs de variables pour effectuer des 
traitements spécifiques.

## multimax(X, TAB)

Cette fonction prend deux arguments `X` une valeur et `TAB` un tableau
de valeurs.
Elle calcule la plus grande valeur contenue dans `TAB` entre `0` et `X - 1`.
Si X est `indefini` ou `X <= 0`, alors `multimax(X, TAB) = indefini`.
`X` peut être plus grand que la taille de `TAB`, auquel cas `multimax` calculera le
maximum de `TAB`.

## min(X, Y)

Cette fonction prend deux arguments et renvoie la plus petite
valeur des deux.
Si les deux valeurs sont `indefini`es, alors la fonction renvoie
`indefini`. 
Si `X` est défini et `Y` est `indefini`, alors `min(X,Y) = min(X, 0)`. De même,
si `X` est `indéfini` et `Y` est defini, alors `min(X,Y) = min(0, Y)`.

## nb_evenements()

Cette fonction renvoie le nombre total d'événements.

## null(X)

Cette fonction prend un argument `X` et renvoie `1` si `X` est
égal à `0`, `0` si `X` est défini et différent de `0`, 
et `indefini` s'il est `indefini`.

Cette fonction est strictement équivalente à l'expression `(X = 0)`

## positif(X)

Cette fonction prend un argument `X` et renvoie `1` si `X` est
**strictement** supérieur à `0`, `0` si `X` est inferieur ou égal à `0`, et
`indefini` si `X` est indefini.

Cette fonction est strictement équivalente à l'expression `X > 0`.

## positif_ou_nul(X)

Cette fonction prend un argument `X` et renvoie `1` si `X` est
supérieur ou égale à `0`, `0` si `X` est strictement inferieur ou égal à
`0`, et `indefini` si `X` est indefini.

Cette fonction est strictement équivalente à l'expression `X >= 0`.

## present(X)

Cette fonction prend un argument `X` et renvoie `0` s'il
est `indefini` et `1` sinon.

**Note** : cette fonction est la seule façon de tester si une valeur est
égale à `indefini` car l'expression booléenne `(indefini = indefini)`
est égale à `indefini`.

## somme(X, Y, ...)

Cette fonction n'a pas de limite d'arguments. 
Elle calcule leur somme.
Si aucun argument n'est donné à la fonction `somme`, elle renvoie `0`.
Si tous ses arguments sont `indefini`s, alors elle renvoie la valeur
`indefini`.

## supzero(X)

Cette fonction prend un argument `X` et renvoie sa valeur s'il est strictement supérieur
à `0`, sinon il renvoie `indefini`.

## taille(TAB)

Cette fonction prend en argument soit une variable, soit un tableau.
S'il s'agit d'une variable, `taille` renvoie `1`, sinon elle renvoie la taille du
tableau.
Elle échoue si l'argument est une constante ou une valeur.

% TODO

% ## numero_compil()
% Unsupported, raises assert false!

% ## numero_verif()
% Unsupported as well

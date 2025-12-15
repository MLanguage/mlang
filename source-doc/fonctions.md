(fonctions)=
# Les fonctions

Les fonctions prédéfinies sont gérées directement par le compilateur `mlang`
qui en définit le comportement par des instructions M.

## abs(X)

Cette fonction prend exactement un argument et renvoie sa valeur absolue.
Si `X` est `indefini`, alors `abs(X) = indefini`.

## arr(X)

Cette fonction prend exactement un argument et renvoie l'entier le plus
proche, ou `indefini` s'il est `indefini`.

## inf(X)

Cette fonction prend exactement un argument et renvoie l'entier inferieur 
le plus proche, ou `indefini` s'il est `indefini`.

## max(X, Y)

Cette fonction prend exactement deux arguments et renvoie la plus grande
valeur des deux.
Si les deux valeurs sont `indefini`es, alors la fonction renvoie
`indefini`. 
Si `X` est défini et `Y` est `indefini`, alors `max(X,Y) = max(X, 0)`. De même,
si `X` est `indéfini` et `Y` est defini, alors `max(X,Y) = max(0, Y)`.

## min(X, Y)

Cette fonction prend exactement deux arguments et renvoie la plus petite
valeur des deux.
Si les deux valeurs sont `indefini`es, alors la fonction renvoie
`indefini`. 
Si `X` est défini et `Y` est `indefini`, alors `min(X,Y) = min(X, 0)`. De même,
si `X` est `indéfini` et `Y` est defini, alors `min(X,Y) = min(0, Y)`.


## null(X)

Cette fonction prend exactement un argument `X` et renvoie `1` si `X` est
égal à `0`, `0` si `X` est défini et différent de `0`, 
et `indefini` s'il est `indefini`.

Cette fonction est strictement équivalente à l'expression `(X = 0)`

## positif(X)

Cette fonction prend exactement un argument `X` et renvoie `1` si `X` est
**strictement** supérieur à `0`, `0` si `X` est inferieur ou égal à `0`, et
`indefini` si `X` est indefini.

Cette fonction est strictement équivalente à l'expression `X > 0`.

## positif_ou_nul(X)

Cette fonction prend exactement un argument `X` et renvoie `1` si `X` est
supérieur ou égale à `0`, `0` si `X` est strictement inferieur ou égal à
`0`, et `indefini` si `X` est indefini.

Cette fonction est strictement équivalente à l'expression `X >= 0`.

## present(X)

Cette fonction prend exactement un argument `X` et renvoie `0` s'il
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

% TODO

## multimax()

## nb_evenements()

## numero_compil()

## numero_verif()

## supzero()

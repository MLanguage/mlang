(arithmetique)=

# L'arithmetique en M

Les variables en M prennent soit leur valeur sur les flottants, soit ont la
valeur `indefini`.
Les opérations arithmétiques standard (+, -, *, /) se comportent sur les
flottants de façon standard, à l'exception de la division d'un flottant par
zero qui renvoie toujours 0. 

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
* `indefini / indefini` = indefini`
* `indefini / n` = indefini`
* `n / indefini = indefini`

Pour résumer, seules les opérations d'addition et de soustraction avec une
valeur renvoient autre chose que `indefini`.

Les comparaisons (=, <, >, ...) renvoient soit `0` soit `1` lorsque des
valeurs definies sont comparées.
%
Toutes les les opérations booléennes renvoient `indefini` si un de ses
membres est `indefini` (même l'égalité `indefini = indefini` renvoie
`indefini`).
Pour vérifier si une valeur est définie ou non, il faut utiliser la fonction
`present` qui renvoie `0` si la valeur est indéfinie et `1` sinon.


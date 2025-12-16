(exemples/valeurs)=

# Calcul des valeurs

## Fichier test : test.m
```
espace_variables GLOBAL : par_defaut;
domaine regle mon_domaine_de_regle: par_defaut;
domaine verif mon_domaine_de_verifications: par_defaut;
application mon_application;
variable saisie : attribut mon_attribut;
variable calculee : attribut mon_attribut;
X : saisie mon_attribut = 0 alias AX : "";

cible indefini_test:
application : mon_application;
afficher "Bonjour, monde, X = ";
afficher (X);
afficher " !\n";
# Addition
afficher "X + 1 = ";
afficher (X + 1);
afficher " !\n";
afficher "1 + X = ";
afficher (1 + X);
afficher " !\n";
afficher "X + X = ";
afficher (X + X);
afficher " !\n";
# Soustraction
afficher "X - 1 = ";
afficher (X - 1);
afficher " !\n";
afficher "1 - X = ";
afficher (1 - X);
afficher " !\n";
afficher "X - X = ";
afficher (X - X);
afficher " !\n";
# Multiplication
afficher "X * 1 = ";
afficher (X * 1);
afficher " !\n";
afficher "1 * X = ";
afficher (1 * X);
afficher " !\n";
afficher "X * X = ";
afficher (X * X);
afficher " !\n";
# Division
afficher "X / 1 = ";
afficher (X / 1);
afficher " !\n";
afficher "1 / X = ";
afficher (1 / X);
afficher " !\n";
afficher "1 / 0 = ";
afficher (1 / 0);
afficher " !\n";
afficher "X / 0 = ";
afficher (X / 0);
afficher " !\n";
afficher "X / X = ";
afficher (X / X);
afficher " !\n";
# Comparaisons
afficher "(X = 0) = ";
afficher (X = 0);
afficher " !\n";
afficher "(X = X) = ";
afficher (X = X);
afficher " !\n";
afficher "(X >= 0) = ";
afficher (X >= 0);
afficher " !\n";
afficher "(X >= X) = ";
afficher (X >= X);
afficher " !\n";
afficher "(X > 0) = ";
afficher (X > 0);
afficher " !\n";
afficher "(X > X) = ";
afficher (X > X);
afficher " !\n";
afficher "(X <= 0) = ";
afficher (X <= 0);
afficher " !\n";
afficher "(X < 0) = ";
afficher (X < 0);
afficher " !\n";
afficher "(X <= X) = ";
afficher (X <= X);
afficher " !\n";
afficher "(X < X) = ";
afficher (X < X);
afficher " !\n";
# Opérations booléennes
afficher "(X et X) = ";
afficher (X et X);
afficher " !\n";
afficher "(X et 1) = ";
afficher (X et 1);
afficher " !\n";
afficher "(X ou X) = ";
afficher (X ou X);
afficher " !\n";
afficher "(X ou 1) = ";
afficher (X ou 1);
afficher " !\n";
afficher "non X = ";
afficher (non X);
afficher " !\n";
# Vérifier la définition d'une valeur
afficher "present(X) = ";
afficher (present(X));
afficher " !\n";
```

## Fichier irj : test.irj

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

## Commande

```
mlang test.m \
	--without_dfgip_m \
	-A mon_application \
	--mpp_function indefini_test \
	--run_test test.irj
```

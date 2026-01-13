(exemples/fonctions)=

# Les fonctions

## Fichier test : test.m
```
espace_variables GLOBAL : par_defaut;
domaine regle mon_domaine_de_regle: par_defaut;
domaine verif mon_domaine_de_verifications: par_defaut;
application mon_application;
variable calculee : attribut mon_attribut;

evenement
: valeur ev_val
: variable ev_var;

X : calculee mon_attribut = 0 : "" type REEL;
Y : calculee mon_attribut = 1 : "";
TAB : tableau[10] calculee mon_attribut = 2 : "" type ENTIER;

cible init_tab:
application : mon_application;
iterer : variable I : entre 0..(taille(TAB) - 1) increment 1 : dans (
  TAB[I] = I;
)

cible reinit_tab:
application : mon_application;
iterer : variable I : entre 0..(taille(TAB) - 1) increment 1 : dans (
  TAB[I] = indefini;
)

cible test_abs:
application : mon_application;
afficher "__ABS__\n" indenter(2);
afficher "abs(indefini) = " (abs(indefini)) "\n";
afficher "abs(1) = "        (abs(1))        "\n";
afficher "abs(-1) = "       (abs(-1))       "\n";
afficher "\n" indenter(-2);

cible test_afficher:
application : mon_application;
afficher "__AFFICHER__\n" indenter(2);
afficher "afficher \"Bonjour, monde!\" : ";
afficher "Bonjour, monde!";
afficher "\nafficher (0) : ";
afficher (0);
afficher "\nafficher (3.1415926535):2 : ";
afficher (3.1415926535):2;
afficher "\nafficher (3.1415926535):2..4 : ";
afficher (3.1415926535):2..4;
afficher "\n\n" indenter (-2);

cible test_arr:
application : mon_application;
afficher "__ARR__\n" indenter(2);
afficher "arr(indefini) = " (arr(indefini)) "\n";
afficher "arr(1.8) = "      (arr(1.8))      "\n";
afficher "arr(-1.7) = "     (arr(-1.7))     "\n";
afficher "\n" indenter(-2);

cible test_attribut:
application : mon_application;
afficher "__ATTRIBUT__\n" indenter(2);
afficher "attribut(X, mon_attribut) = "   (attribut(X, mon_attribut))   "\n";
afficher "attribut(TAB, mon_attribut) = " (attribut(TAB, mon_attribut)) "\n";
afficher "\n" indenter(-2);

cible test_champ_evenement_base:
application : mon_application;
afficher indenter(2);
afficher "champ_evenement(0, ev_val) = " (champ_evenement (0,ev_val)) "\n";
afficher "champ_evenement(0, ev_var) = " (champ_evenement (0,ev_var)) "\n";
afficher indenter(-2);

cible test_champ_evenement:
application : mon_application;
afficher "__CHAMP_EVENEMENT__\n" indenter(2);
arranger_evenements
  : ajouter 1
  : dans (
      afficher "Avant d'initialiser les champs de l'événement:\n";
      calculer cible test_champ_evenement_base;
      champ_evenement (0,ev_var) reference X;      
      champ_evenement (0,ev_val) = 42;
      X = 2;
      afficher "Après avoir initialisé les champs de l'événement:\n";
      calculer cible test_champ_evenement_base;
      X = indefini;
    )
afficher "\n" indenter(-2);

cible test_inf:
application : mon_application;
afficher "__INF__\n" indenter(2);
afficher "inf(indefini) = " (inf(indefini)) "\n";
afficher "inf(1.8) = "      (inf(1.8))      "\n";
afficher "inf(-1.7) = "     (inf(-1.7))     "\n";
afficher "\n" indenter(-2);

cible test_max:
application : mon_application;
afficher "__MAX__\n" indenter(2);
afficher "max(indefini, indefini) = " (max(indefini, indefini)) "\n";
afficher "max(-1, indefini) = "       (max(-1, indefini))       "\n";
afficher "max(indefini, -1) = "       (max(indefini, -1))       "\n";
afficher "max(1, indefini) = "        (max(1, indefini))        "\n";
afficher "\n" indenter(-2);

cible test_meme_variable:
application : mon_application;
afficher "__MEME_VARIABLE__\n" indenter(2);
afficher "meme_variable(X,X) = "        (meme_variable(X,X))        "\n";
afficher "meme_variable(X,TAB) = "      (meme_variable(X,TAB))      "\n";
afficher "meme_variable(TAB[0],TAB) = " (meme_variable(TAB[0],TAB)) "\n";
afficher "\n" indenter(-2);

cible test_min:
application : mon_application;
afficher "__MIN__" indenter(2);
afficher "min(indefini, indefini) = " (min(indefini, indefini)) "\n";
afficher "min(1, indefini) = "        (min(1, indefini))        "\n";
afficher "min(indefini, 1) = "        (min(indefini, 1))        "\n";
afficher "min(-1, indefini) = "       (min(-1, indefini))       "\n";
afficher "\n" indenter(-2);

cible test_multimax_base:
application : mon_application;
afficher indenter(2);
afficher "multimax(indefini, TAB) = " (multimax(indefini, TAB)) "\n";
afficher "multimax(7, TAB) = " (multimax(7, TAB)) "\n";
afficher "multimax(taille(TAB) + 1, TAB) = " (multimax(taille(TAB) + 1, TAB)) "\n";
afficher "multimax(0, TAB) = "  (multimax(0, TAB))  "\n";
afficher "multimax(-1, TAB) = " (multimax(-1, TAB)) "\n";
afficher indenter(-2);

cible test_multimax:
application : mon_application;
afficher "__MULTIMAX__\n" indenter(2);
afficher "Avant initialisation du tableau :\n";
calculer cible test_multimax_base;
calculer cible init_tab;
afficher "Après initialisation du tableau :\n";
calculer cible test_multimax_base;
calculer cible reinit_tab;
afficher "\n" indenter(-2);

cible test_nb_evenements_base:
application : mon_application;
afficher indenter(2);
afficher "nb_evenements() = " (nb_evenements ()) "\n";
afficher indenter(-2);

cible test_nb_evenements:
application : mon_application;
afficher "__NB_EVENEMENTS__\n" indenter(2);
afficher "Avant la définition d'un événement :\n";
calculer cible test_nb_evenements_base;
arranger_evenements
  : ajouter 1
  : dans (
      afficher "Pendant la définition d'un événement :\n";
      calculer cible test_nb_evenements_base;
    )
afficher "Après la définition d'un événement :\n";
calculer cible test_nb_evenements_base;
afficher "\n" indenter(-2);

cible test_null:
application : mon_application;
afficher "__NULL__\n" indenter(2);
afficher "null(indefini) = " (null(indefini)) "\n";
afficher "null(0) = "        (null(0))        "\n";
afficher "null(1) = "        (null(1))        "\n";
afficher "\n" indenter(-2);

cible test_positif:
application : mon_application;
afficher "__POSITIF__\n" indenter(2);
afficher "positif(indefini) = " (positif(indefini)) "\n";
afficher "positif(0) = "        (positif(0))        "\n";
afficher "positif(1) = "        (positif(1))        "\n";
afficher "positif(-1) = "       (positif(-1))       "\n";
afficher "\n";
afficher indenter(-2);

cible test_positif_ou_nul:
application : mon_application;
afficher "__POSITIF OU NUL__\n" indenter(2);
afficher "positif_ou_nul(indefini) = " (positif_ou_nul(indefini)) "\n";
afficher "positif_ou_nul(0) = "        (positif_ou_nul(0))        "\n";
afficher "positif_ou_nul(1) = "        (positif_ou_nul(1))        "\n";
afficher "positif_ou_nul(-1) = "       (positif_ou_nul(-1))       "\n";
afficher "\n" indenter(-2);

cible test_present:
application : mon_application;
afficher "__PRESENT__\n" indenter(2);
afficher "present(indefini) = " (present(indefini)) "\n";
afficher "present(0) = "        (present(0))        "\n";
afficher "present(1) = "        (present(1))        "\n";
afficher "\n" indenter(-2);

cible test_somme:
application : mon_application;
afficher "__SOMME__\n" indenter(2);
afficher "somme() = "              (somme())              "\n";
afficher "somme(indefini) = "      (somme(indefini))      "\n";
afficher "somme(1, indefini) = "   (somme(1, indefini))   "\n";
afficher "somme(1, 2, 3, 4, 5) = " (somme(1, 2, 3, 4, 5)) "\n";
afficher "\n" indenter(-2);

cible test_supzero:
application : mon_application;
afficher "__SUPZERO__\n" indenter(2);
afficher "supzero(indefini) = " (supzero(indefini)) "\n";
afficher "supzero(42) = "       (supzero(42))       "\n";
afficher "supzero(-1) = "       (supzero(-1))       "\n";
afficher "supzero(0) = "        (supzero(0))        "\n";
afficher "\n" indenter(-2);

cible test_taille:
application : mon_application;
afficher "__TAILLE__\n" indenter(2);
afficher "taille(TAB) = " (taille(TAB)) "\n";
afficher "taille(X) = "   (taille(X))   "\n";
afficher "\n" indenter(-2);

cible test_type:
application : mon_application;
afficher "__TYPE__\n" indenter(2);
afficher "type(X, REEL) = "          (type(X, REEL))          "\n";
afficher "type(X, ENTIER) = "        (type(X, ENTIER))        "\n";
afficher "type(TAB, ENTIER) = "      (type(TAB, ENTIER))      "\n";
afficher "type(Y, ENTIER) = "        (type(Y, ENTIER))        "\n";
afficher "type(Y, REEL) = "          (type(Y, REEL))          "\n";
afficher "type(Y, BOOLEEN) = "       (type(Y, BOOLEEN))       "\n";
afficher "type(Y, DATE_AAAA) = "     (type(Y, DATE_AAAA))     "\n";
afficher "type(Y, DATE_JJMMAAAA) = " (type(Y, DATE_JJMMAAAA)) "\n";
afficher "type(Y, DATE_MM) = "       (type(Y, DATE_MM))       "\n";
afficher "\n" indenter(-2);

cible fun_test:
application : mon_application;
# Abs
calculer cible test_abs;
# Afficher
calculer cible test_afficher;
# Arr
calculer cible test_arr;
# Attribut
calculer cible test_attribut;
# Champ evenement
calculer cible test_champ_evenement;
# Inf
calculer cible test_inf;
# Max
calculer cible test_max;
# Meme variable
calculer cible test_meme_variable;
# Min
calculer cible test_min;
# Multimax
calculer cible test_multimax;
# Null
calculer cible test_nb_evenements;
# Null
calculer cible test_null;
# Positif
calculer cible test_positif;
# Positif ou nul
calculer cible test_positif_ou_nul;
# Présent
calculer cible test_present;
# Somme
calculer cible test_somme;
# Supzero
calculer cible test_supzero;
# Taille
calculer cible test_taille;
# Type
calculer cible test_type;
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
	--mpp_function fun_test \
	--run_test test.irj
```

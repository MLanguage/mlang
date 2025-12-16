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

X : calculee mon_attribut = 0 : "";
TAB : tableau[10] calculee mon_attribut = 2 : "";

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
afficher "\n__ABS__";
afficher "\n abs(indefini) = ";
afficher  (abs(indefini));
afficher "\n abs(1) = ";
afficher  (abs(1));
afficher "\n abs(-1) = ";
afficher  (abs(-1));
afficher "\n";

cible test_arr:
application : mon_application;
afficher "\n__ARR__";
afficher "\n arr(indefini) = ";
afficher  (arr(indefini));
afficher "\n arr(1.8) = ";
afficher  (arr(1.8));
afficher "\n arr(-1.7) = ";
afficher  (arr(-1.7));
afficher "\n";

cible test_attribut:
application : mon_application;
afficher "\n__ATTRIBUT__";
afficher "\n attribut(X, mon_attribut) = ";
afficher  (attribut(X, mon_attribut));
afficher "\n attribut(TAB, mon_attribut) = ";
afficher  (attribut(TAB, mon_attribut));
afficher "\n";

cible test_champ_evenement_base:
application : mon_application;
afficher "\n champ_evenement(0, ev_val) = ";
afficher (champ_evenement (0,ev_val));
afficher "\n champ_evenement(0, ev_var) = ";
afficher (champ_evenement (0,ev_var));
afficher "\n";

cible test_champ_evenement:
application : mon_application;
afficher "\n__CHAMP_EVENEMENT__";
arranger_evenements
  : ajouter 1
  : dans (
      afficher "\nAvant d'initialiser les champs de l'événement:";
      calculer cible test_champ_evenement_base;
      champ_evenement (0,ev_var) reference X;      
      champ_evenement (0,ev_val) = 42;
      X = 2;
      afficher "\n Après avoir initialisé les champs de l'événement:";
      calculer cible test_champ_evenement_base;
      X = indefini;
    )

cible test_inf:
application : mon_application;
afficher "\n__INF__";
afficher "\n inf(indefini) = ";
afficher  (inf(indefini));
afficher "\n inf(1.8) = ";
afficher  (inf(1.8));
afficher "\n inf(-1.7) = ";
afficher  (inf(-1.7));
afficher "\n";

cible test_max:
application : mon_application;
afficher "\n__MAX__";
afficher "\n max(indefini, indefini) = ";
afficher   (max(indefini, indefini));
afficher "\n max(-1, indefini) = ";
afficher   (max(-1, indefini));
afficher "\n max(indefini, -1) = ";
afficher   (max(indefini, -1));
afficher "\n max(1, indefini) = ";
afficher   (max(1, indefini));
afficher "\n";

cible test_meme_variable:
application : mon_application;
afficher "\n__MEME_VARIABLE__";
afficher "\n meme_variable(X,X) = ";
afficher (meme_variable(X,X));
afficher "\n meme_variable(X,TAB) = ";
afficher (meme_variable(X,TAB));
afficher "\n meme_variable(TAB[0],TAB) = ";
afficher (meme_variable(TAB[0],TAB));
afficher "\n";

cible test_min:
application : mon_application;
afficher "\n__MIN__";
afficher "\n min(indefini, indefini) = ";
afficher (min(indefini, indefini));
afficher "\n min(1, indefini) = ";
afficher (min(1, indefini));
afficher "\n min(indefini, 1) = ";
afficher (min(indefini, 1));
afficher "\n min(-1, indefini) = ";
afficher (min(-1, indefini));
afficher "\n";

cible test_multimax_base:
application : mon_application;
afficher "\n multimax(indefini, TAB) = ";
afficher (multimax(indefini, TAB));
afficher "\n multimax(7, TAB) = ";
afficher (multimax(7, TAB));
afficher "\n multimax(taille(TAB) + 1, TAB) = ";  
afficher (multimax(taille(TAB) + 1, TAB));
afficher "\n multimax(0, TAB) = ";  
afficher (multimax(0, TAB));
afficher "\n multimax(-1, TAB) = ";  
afficher (multimax(-1, TAB));

cible test_multimax:
application : mon_application;
afficher "\n__MULTIMAX__";
afficher "\nAvant initialisation du tableau :";
calculer cible test_multimax_base;
calculer cible init_tab;
afficher "\nAprès initialisation du tableau :";
calculer cible test_multimax_base;
calculer cible reinit_tab;
afficher "\n";

cible test_nb_evenements_base:
application : mon_application;
afficher "\n nb_evenements() = ";
afficher (nb_evenements ());
afficher "\n";

cible test_nb_evenements:
application : mon_application;
afficher "\n__NB_EVENEMENTS__";
afficher "\nAvant la définition d'un événement :";
calculer cible test_nb_evenements_base;
arranger_evenements
  : ajouter 1
  : dans (
      afficher "\nPendant la définition d'un événement :";
      calculer cible test_nb_evenements_base;
    )
afficher "\nAprès la définition d'un événement :";
calculer cible test_nb_evenements_base;

cible test_null:
application : mon_application;
afficher "\n__NULL__";
afficher "\n null(indefini) = ";
afficher  (null(indefini));
afficher "\n null(0) = ";
afficher  (null(0));
afficher "\n null(1) = ";
afficher  (null(1));
afficher "\n";

cible test_positif:
application : mon_application;
afficher "\n__POSITIF__";
afficher "\n positif(indefini) = ";
afficher (positif(indefini));
afficher "\n positif(0) = ";
afficher (positif(0));
afficher "\n positif(1) = ";
afficher (positif(1));
afficher "\n positif(-1) = ";
afficher (positif(-1));
afficher "\n";

cible test_positif_ou_nul:
application : mon_application;
afficher "\n__POSITIF OU NUL__";
afficher "\n positif_ou_nul(indefini) = ";
afficher (positif_ou_nul(indefini));
afficher "\n positif_ou_nul(0) = ";
afficher (positif_ou_nul(0));
afficher "\n positif_ou_nul(1) = ";
afficher (positif_ou_nul(1));
afficher "\n positif_ou_nul(-1) = ";
afficher (positif_ou_nul(-1));
afficher "\n";

cible test_present:
application : mon_application;
afficher "\n__PRESENT__";
afficher "\n present(indefini) = ";
afficher  (present(indefini));
afficher "\n present(0) = ";
afficher  (present(0));
afficher "\n present(1) = ";
afficher  (present(1));
afficher "\n";

cible test_somme:
application : mon_application;
afficher "\n__SOMME__";
afficher "\n somme() = ";
afficher (somme());
afficher "\n somme(indefini) = ";
afficher (somme(indefini));
afficher "\n somme(1, indefini) = ";
afficher (somme(1, indefini));
afficher "\n somme(1, 2, 3, 4, 5) = ";
afficher (somme(1, 2, 3, 4, 5));
afficher "\n";

cible test_supzero:
application : mon_application;
afficher "\n__SUPZERO__";
afficher "\n supzero(indefini) = ";
afficher (supzero(indefini));
afficher "\n supzero(42) = ";
afficher (supzero(42));
afficher "\n supzero(-1) = ";
afficher (supzero(-1));
afficher "\n supzero(0) = ";
afficher (supzero(0));
afficher "\n";

cible test_taille:
application : mon_application;
afficher "\n__TAILLE__";
afficher "\n taille(TAB) = ";
afficher (taille(TAB));
afficher "\n taille(X) = ";
afficher (taille(X));
afficher "\n";

cible fun_test:
application : mon_application;
# Abs
calculer cible test_abs;
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
mlang --without_dfgip_m test.m -A mon_application --mpp_function fun_test --run_test test.irj
```

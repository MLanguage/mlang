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
afficher "\n__ABS__";
afficher indenter(2);
afficher "\nabs(indefini) = ";
afficher (abs(indefini));
afficher "\nabs(1) = ";
afficher  (abs(1));
afficher "\nabs(-1) = ";
afficher  (abs(-1));
afficher "\n";
afficher indenter(-2);

cible test_arr:
application : mon_application;
afficher "\n__ARR__";
afficher indenter(2);
afficher "\narr(indefini) = ";
afficher  (arr(indefini));
afficher "\narr(1.8) = ";
afficher  (arr(1.8));
afficher "\ arr(-1.7) = ";
afficher  (arr(-1.7));
afficher "\n";
afficher indenter(-2);

cible test_attribut:
application : mon_application;
afficher "\n__ATTRIBUT__";
afficher indenter(2);
afficher "\nattribut(X, mon_attribut) = ";
afficher  (attribut(X, mon_attribut));
afficher "\nattribut(TAB, mon_attribut) = ";
afficher  (attribut(TAB, mon_attribut));
afficher "\n";
afficher indenter(-2);

cible test_champ_evenement_base:
application : mon_application;
afficher indenter(2);
afficher "\nchamp_evenement(0, ev_val) = ";
afficher (champ_evenement (0,ev_val));
afficher "\nchamp_evenement(0, ev_var) = ";
afficher (champ_evenement (0,ev_var));
afficher "\n";
afficher indenter(-2);

cible test_champ_evenement:
application : mon_application;
afficher "\n__CHAMP_EVENEMENT__";
afficher indenter(2);
arranger_evenements
  : ajouter 1
  : dans (
      afficher "\nAvant d'initialiser les champs de l'événement:";
      calculer cible test_champ_evenement_base;
      champ_evenement (0,ev_var) reference X;      
      champ_evenement (0,ev_val) = 42;
      X = 2;
      afficher "\nAprès avoir initialisé les champs de l'événement:";
      calculer cible test_champ_evenement_base;
      X = indefini;
    )
afficher indenter(-2);

cible test_inf:
application : mon_application;
afficher "\n__INF__";
afficher indenter(2);
afficher "\ninf(indefini) = ";
afficher  (inf(indefini));
afficher "\ninf(1.8) = ";
afficher  (inf(1.8));
afficher "\ninf(-1.7) = ";
afficher  (inf(-1.7));
afficher "\n";
afficher indenter(-2);

cible test_max:
application : mon_application;
afficher "\n__MAX__";
afficher indenter(2);
afficher "\nmax(indefini, indefini) = ";
afficher   (max(indefini, indefini));
afficher "\nmax(-1, indefini) = ";
afficher   (max(-1, indefini));
afficher "\nmax(indefini, -1) = ";
afficher   (max(indefini, -1));
afficher "\nmax(1, indefini) = ";
afficher   (max(1, indefini));
afficher "\n";
afficher indenter(-2);

cible test_meme_variable:
application : mon_application;
afficher "\n__MEME_VARIABLE__";
afficher indenter(2);
afficher "\nmeme_variable(X,X) = ";
afficher (meme_variable(X,X));
afficher "\nmeme_variable(X,TAB) = ";
afficher (meme_variable(X,TAB));
afficher "\nmeme_variable(TAB[0],TAB) = ";
afficher (meme_variable(TAB[0],TAB));
afficher "\n";
afficher indenter(-2);

cible test_min:
application : mon_application;
afficher "\n__MIN__";
afficher indenter(2);
afficher "\nmin(indefini, indefini) = ";
afficher (min(indefini, indefini));
afficher "\nmin(1, indefini) = ";
afficher (min(1, indefini));
afficher "\nmin(indefini, 1) = ";
afficher (min(indefini, 1));
afficher "\nmin(-1, indefini) = ";
afficher (min(-1, indefini));
afficher "\n";
afficher indenter(-2);

cible test_multimax_base:
application : mon_application;
afficher indenter(2);
afficher "\nmultimax(indefini, TAB) = ";
afficher (multimax(indefini, TAB));
afficher "\nmultimax(7, TAB) = ";
afficher (multimax(7, TAB));
afficher "\nmultimax(taille(TAB) + 1, TAB) = ";  
afficher (multimax(taille(TAB) + 1, TAB));
afficher "\nmultimax(0, TAB) = ";  
afficher (multimax(0, TAB));
afficher "\nmultimax(-1, TAB) = ";  
afficher (multimax(-1, TAB));
afficher indenter(-2);

cible test_multimax:
application : mon_application;
afficher "\n__MULTIMAX__";
afficher indenter(2);
afficher "\nAvant initialisation du tableau :";
calculer cible test_multimax_base;
calculer cible init_tab;
afficher "\nAprès initialisation du tableau :";
calculer cible test_multimax_base;
calculer cible reinit_tab;
afficher "\n";
afficher indenter(-2);

cible test_nb_evenements_base:
application : mon_application;
afficher indenter(2);
afficher "\nnb_evenements() = ";
afficher (nb_evenements ());
afficher "\n";
afficher indenter(-2);

cible test_nb_evenements:
application : mon_application;
afficher "\n__NB_EVENEMENTS__";
afficher indenter(2);
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
afficher indenter(-2);

cible test_null:
application : mon_application;
afficher "\n__NULL__";
afficher indenter(2);
afficher "\nnull(indefini) = ";
afficher  (null(indefini));
afficher "\nnull(0) = ";
afficher  (null(0));
afficher "\nnull(1) = ";
afficher  (null(1));
afficher "\n";
afficher indenter(-2);

cible test_positif:
application : mon_application;
afficher "\n__POSITIF__";
afficher indenter(2);
afficher "\npositif(indefini) = ";
afficher (positif(indefini));
afficher "\npositif(0) = ";
afficher (positif(0));
afficher "\npositif(1) = ";
afficher (positif(1));
afficher "\npositif(-1) = ";
afficher (positif(-1));
afficher "\n";
afficher indenter(-2);

cible test_positif_ou_nul:
application : mon_application;
afficher "\n__POSITIF OU NUL__";
afficher indenter(2);
afficher "\npositif_ou_nul(indefini) = ";
afficher (positif_ou_nul(indefini));
afficher "\npositif_ou_nul(0) = ";
afficher (positif_ou_nul(0));
afficher "\npositif_ou_nul(1) = ";
afficher (positif_ou_nul(1));
afficher "\npositif_ou_nul(-1) = ";
afficher (positif_ou_nul(-1));
afficher "\n";
afficher indenter(-2);

cible test_present:
application : mon_application;
afficher "\n__PRESENT__";
afficher indenter(2);
afficher "\npresent(indefini) = ";
afficher  (present(indefini));
afficher "\npresent(0) = ";
afficher  (present(0));
afficher "\npresent(1) = ";
afficher  (present(1));
afficher "\n";
afficher indenter(-2);

cible test_somme:
application : mon_application;
afficher "\n__SOMME__";
afficher indenter(2);
afficher "\nsomme() = ";
afficher (somme());
afficher "\nsomme(indefini) = ";
afficher (somme(indefini));
afficher "\nsomme(1, indefini) = ";
afficher (somme(1, indefini));
afficher "\nsomme(1, 2, 3, 4, 5) = ";
afficher (somme(1, 2, 3, 4, 5));
afficher "\n";
afficher indenter(-2);

cible test_supzero:
application : mon_application;
afficher "\n__SUPZERO__";
afficher indenter(2);
afficher "\nsupzero(indefini) = ";
afficher (supzero(indefini));
afficher "\nsupzero(42) = ";
afficher (supzero(42));
afficher "\nsupzero(-1) = ";
afficher (supzero(-1));
afficher "\nsupzero(0) = ";
afficher (supzero(0));
afficher "\n";
afficher indenter(-2);

cible test_taille:
application : mon_application;
afficher "\n__TAILLE__";
afficher indenter(2);
afficher "\ntaille(TAB) = ";
afficher (taille(TAB));
afficher "\ntaille(X) = ";
afficher (taille(X));
afficher "\n";
afficher indenter(-2);

cible test_type:
application : mon_application;
afficher "\n__TYPE__";
afficher indenter(2);
afficher "\ntype(X, REEL) = ";
afficher (type(X, REEL));
afficher "\ntype(X, ENTIER) = ";
afficher (type(X, ENTIER));
afficher "\ntype(TAB, ENTIER) = ";
afficher (type(TAB, ENTIER));
afficher "\ntype(Y, ENTIER) = ";
afficher (type(Y, ENTIER));
afficher "\ntype(Y, REEL) = ";
afficher (type(Y, REEL));
afficher "\ntype(Y, BOOLEEN) = ";
afficher (type(Y, BOOLEEN));
afficher "\ntype(Y, DATE_AAAA) = ";
afficher (type(Y, DATE_AAAA));
afficher "\ntype(Y, DATE_JJMMAAAA) = ";
afficher (type(Y, DATE_JJMMAAAA));
afficher "\ntype(Y, DATE_MM) = ";
afficher (type(Y, DATE_MM));
afficher "\n";
afficher indenter(-2);

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

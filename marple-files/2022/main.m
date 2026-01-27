# primitif iterpréteur

cible enchaineur_primitif:
application: iliad;
GLOBAL.IND_TRAIT = 4; # primitif
GLOBAL.ANCSDED = ANREV + 1;
calculer cible enchaine_verification_prim;
si nb_anomalies() = 0 alors
  calculer cible calcul_primitif_isf_prim;
  calculer cible verif_calcul_primitive_isf_prim;
  calculer cible enchaine_calcul_prim;
finsi
# finalise_erreurs;
# exporte_erreurs;

cible enchaineur_correctif:
application: iliad;
calculer cible calcul;

cible enchainement_primitif_interpreteur:
application: iliad;
V_IND_TRAIT = 4; # primitif
calculer cible enchainement_primitif;
exporte_erreurs;


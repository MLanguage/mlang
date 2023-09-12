cible calcul_primitif:
application: iliad;
calculer domaine primitive;

cible compute_article1731bis:
application: iliad;
variable temporaire: TOTO;
TOTO = 0;
ART1731BIS = 0;
si
  CMAJ = 8 ou CMAJ = 11
alors
  ART1731BIS = 1;
  PREM8_11 = 1;
finsi


application iliad;

V_ANCSDED : saisie contexte
  classe = 0 priorite = 10 categorie_TL = 20 modcat = 1 primrest = 0
  restituee
  alias ANCSDED
  : "Annee de revenu pour variation CSG";

V_ANREV : saisie contexte
  classe = 0 priorite = 10 categorie_TL = 20 modcat = 1 primrest = 0
  restituee
  alias ANREV
  : "Annee des revenus" type REEL;

V_IND_TRAIT : saisie contexte
  classe = 0 priorite = 10 categorie_TL = 20 modcat = 1 primrest = 0
  restituee
  alias IND_TRAIT
  : "indicateur de nature de traitement primitif ou correctif";

RESULTAT : calculee primrest = 0 restituee : "resultat du traitement" ;
TOTO01 : calculee primrest = 0 restituee : "" ;
TOTO03 : calculee primrest = 0 restituee : "" ;
TOTO05 : calculee primrest = 0 restituee : "" ;
TUTU : tableau[5] calculee primrest = 0 restituee : "" ;

cible test_varcons:
application: iliad;
variables_temporaires: TITI tableau[5];
afficher "entree test_varcons\n" indenter(2);
iterer : variable I : 1..5 increment 2 : dans (
  TOTO[00: I] = 450 + I;
  afficher nom(TOTO[00: I]) " = " (TOTO[00: I]) "\n";
)
iterer : variable I : 0..(taille(TUTU) - 1) increment 1 : dans (
  TUTU[I] = 787800 + I;
)
iterer : variable I : 0..(taille(TUTU) - 1) increment 1 : dans (
  afficher nom(TUTU[0: I]) " = " (TUTU[I]) "\n";
)
iterer : variable I : 0..(taille(TITI) - 1) increment 1 : dans (
  TITI[I] = 717100 + I;
)
iterer : variable I : 0..(taille(TITI) - 1) increment 1 : dans (
  afficher nom(TITI[0: I]) " = " (TITI[I]) "\n";
)
afficher indenter(-2) "sortie test_varcons\n";

cible enchainement_primitif:
application: iliad;
afficher "\n";
si V_IND_TRAIT = 4 alors
  afficher "--- Primitif ---\n";
sinon_si V_IND_TRAIT = 5 alors
  afficher "--- Correctif ---\n";
finsi
afficher "Bonjour le monde !\n";
calculer cible test_varcons;
RESULTAT = 0;

cible enchainement_primitif_interpreteur:
application: iliad;
calculer cible enchainement_primitif;



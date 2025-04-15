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
afficher_erreur "entree test_varcons\n" indenter(2);
iterer : variable I : 1..5 increment 2 : dans (
  TOTO[00: I] = 450 + I;
  afficher_erreur nom(TOTO[00: I]) " = " (TOTO[00: I]) "\n";
)
iterer : variable I : 0..(taille(TUTU) - 1) increment 1 : dans (
  TUTU[I] = 787800 + I;
)
iterer : variable I : 0..(taille(TUTU) - 1) increment 1 : dans (
  afficher_erreur nom(TUTU[0: I]) " = " (TUTU[I]) "\n";
)
iterer : variable I : 0..(taille(TITI) - 1) increment 1 : dans (
  TITI[I] = 717100 + I;
)
iterer : variable I : 0..(taille(TITI) - 1) increment 1 : dans (
  afficher_erreur nom(TITI[0: I]) " = " (TITI[I]) "\n";
)
afficher_erreur indenter(-2) "sortie test_varcons\n";

fonction toto_fonction:
application: iliad;
arguments: A0, A1, A2, A3, A4, A5, A6;
resultat: R;
variables_temporaires: PROUT0, PROUT1, PROUT2;
R = A0 + A1 + A2 + A3 + A4 + A5 + A6;

cible toto_cible:
application: iliad;
arguments: A0, A1, A2, A3, A4, A5, A6, R;
iterer : variable PROUT0 : categorie calculee * : dans (
  iterer : variable PROUT1 : categorie calculee * : dans (
    iterer : variable PROUT2 : categorie calculee * : dans (
      R = A0 + A1 + A2 + A3 + A4 + A5 + A6;
    )
  )
)

cible test_args:
application: iliad;
variables_temporaires: A0, A1, A2, A3, AA tableau[3], A4, A5, A6, R;
afficher_erreur "entree test_args\n" indenter(2);
iterer : variable I : 0..6 increment 1 : dans (
  A[0: I] = I;
)
R = 7;
calculer cible toto_cible : avec A0, A1, A2, A3, A4, A5, A6, R;
afficher_erreur "toto_cible(...) = " (R) "\n";
afficher_erreur "toto_fonction(...) = ";
afficher_erreur (toto_fonction(A0, A1, A2, A3, A4, A5, A6));
afficher_erreur "\n";
afficher_erreur indenter(-2) "sortie test_args\n";

cible tmpref_cible:
application: iliad;
arguments: X, A;
variables_temporaires: T;
afficher_erreur nom(X) " = " (X) " " nom(A) " = " (A) "\n";
X = 444;
A = 12300 + A;

cible test_tmpref:
application: iliad;
variables_temporaires: A0, A1, A2, A3, A4, A5, A6, R;
afficher_erreur "entree test_tmpref\n" indenter(2);
A0 = 0;
A1 = 1;
A2 = 2;
A3 = 3;
A4 = 4;
A5 = 5;
A6 = 6;
R = 7;
afficher_erreur nom(A0) " = " (A0) "\n";
afficher_erreur nom(A1) " = " (A1) "\n";
afficher_erreur nom(A2) " = " (A2) "\n";
afficher_erreur nom(A3) " = " (A3) "\n";
afficher_erreur nom(A4) " = " (A4) "\n";
afficher_erreur nom(A5) " = " (A5) "\n";
afficher_erreur nom(A6) " = " (A6) "\n";
afficher_erreur nom(R) " = " (R) "\n";
calculer cible tmpref_cible : avec R, A3;
afficher_erreur nom(A0) " = " (A0) "\n";
afficher_erreur nom(A1) " = " (A1) "\n";
afficher_erreur nom(A2) " = " (A2) "\n";
afficher_erreur nom(A3) " = " (A3) "\n";
afficher_erreur nom(A4) " = " (A4) "\n";
afficher_erreur nom(A5) " = " (A5) "\n";
afficher_erreur nom(A6) " = " (A6) "\n";
afficher_erreur nom(R) " = " (R) "\n";
afficher_erreur indenter(-2) "sortie test_tmpref\n";

cible tests:
application: iliad;
variables_temporaires: U0, UUU tableau[5], U1;
#calculer cible test_varcons;
#calculer cible test_args;
calculer cible test_tmpref;

cible enchainement_primitif:
application: iliad;
afficher_erreur "\n";
si V_IND_TRAIT = 4 alors
  afficher_erreur "--- Primitif ---\n";
sinon_si V_IND_TRAIT = 5 alors
  afficher_erreur "--- Correctif ---\n";
finsi
afficher_erreur "Bonjour le monde !\n";
calculer cible tests;
RESULTAT = 0;

cible enchainement_primitif_interpreteur:
application: iliad;
calculer cible enchainement_primitif;
afficher_erreur "FIN\n";



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

V_ARG : saisie contexte
  classe = 0 priorite = 10 categorie_TL = 20 modcat = 1 primrest = 0
  restituee
  alias ARG
  : "argument";

V_CODE : saisie contexte
  classe = 0 priorite = 10 categorie_TL = 20 modcat = 1 primrest = 0
  restituee
  alias CODE
  : "argument code";

V_BLA : saisie contexte
  classe = 0 priorite = 10 categorie_TL = 20 modcat = 1 primrest = 10
  restituee
  alias BLA
  : "bla";

V_TAB : tableau[5] calculee base primrest = 0 : "argument tableau";

RESULTAT : calculee primrest = 0 restituee : "resultat du traitement" ;
TOTO01 : calculee primrest = 0 restituee : "" ;
TOTO02 : calculee primrest = 0 restituee : "" ;
TOTO03 : calculee primrest = 0 restituee : "" ;
TOTO05 : calculee primrest = 0 restituee : "" ;
TUTU : tableau[5] calculee primrest = 0 restituee : "" ;

BOBO1 : calculee base primrest = 0 restituee : "" ;
BOBO2 : calculee base primrest = 0 restituee : "" ;
BOBO3 : calculee base primrest = 0 restituee : "" ;
BOBO4 : calculee base primrest = 0 restituee : "" ;
BOBORES : calculee base primrest = 0 restituee : "" ;

espace_variables ESP : categorie saisie, base;

regle primitive 10:
application : iliad;
BOBO1 = V_IND_TRAIT;

regle primitive 20:
application : iliad;
BOBO2 = BOBO1 * 10 + 1;

regle primitive 30:
application : iliad;
BOBORES = BOBO2 + 1;

cible calc_prim:
application : iliad;
afficher_erreur "entree calc_prim\n" indenter(2);
afficher_erreur "primitive 0:"
  indenter(2) "\n"
  nom(V_IND_TRAIT) " = " (V_IND_TRAIT) "\n"
  nom(BOBO1) " = " (BOBO1) "\n"
  nom(BOBO2) " = " (BOBO2) "\n"
  nom(BOBORES) " = " (BOBORES) "\n"
  indenter(-2);
calculer domaine primitive;
afficher_erreur "primitive 1:"
  indenter(2) "\n"
  nom(V_IND_TRAIT) " = " (V_IND_TRAIT) "\n"
  nom(BOBO1) " = " (BOBO1) "\n"
  nom(BOBO2) " = " (BOBO2) "\n"
  nom(BOBORES) " = " (BOBORES) "\n"
  indenter(-2);
afficher_erreur indenter(-2) "sortie calc_prim\n";

cible test_esp:
application : iliad;
afficher_erreur "entree test_esp\n" indenter(2);
afficher_erreur nom(V_IND_TRAIT) " = " (V_IND_TRAIT) "\n";
afficher_erreur nom(GLOBAL.V_IND_TRAIT) " = " (GLOBAL.V_IND_TRAIT) "\n";
afficher_erreur "0: " nom(ESP.V_IND_TRAIT) " = " (ESP.V_IND_TRAIT) "\n";
ESP.V_IND_TRAIT = 47;
afficher_erreur "1: " nom(ESP.V_IND_TRAIT) " = " (ESP.V_IND_TRAIT) "\n";
calculer cible calc_prim : espace GLOBAL;
calculer cible calc_prim : espace ESP;
afficher_erreur indenter(-2) "sortie test_esp\n";

cible cible_sp_args:
application : iliad;
arguments: ARG_TMP, ARG;
afficher_erreur "entree cible_sp_args\n" indenter(2);
afficher_erreur "0: "
  nom(ARG_TMP) " = " (ARG_TMP) ", "
  nom(ARG) " = " (ARG) "\n";
ARG_TMP = ARG_TMP + 1;
ARG = 42;
afficher_erreur "1: "
  nom(ARG_TMP) " = " (ARG_TMP) ", "
  nom(ARG) " = " (ARG) "\n";
afficher_erreur indenter(-2) "sortie cible_sp_args\n";

cible test_cible_avec_args:
application : iliad;
variables_temporaires: TMP;
afficher_erreur "entree test_cible_avec_args\n" indenter(2);
V_ARG = indefini;
ESP.V_ARG = indefini;
TMP = 0;
afficher_erreur
  nom(TMP) " = " (TMP) ", "
  nom(V_ARG) " = " (V_ARG) "\n";
calculer cible cible_sp_args : avec TMP, V_ARG;
afficher_erreur
  nom(TMP) " = " (TMP) ", "
  nom(V_ARG) " = " (V_ARG) "\n";
afficher_erreur
  nom(TMP) " = " (TMP) ", " 
  nom(ESP.V_ARG) " = " (ESP.V_ARG) "\n";
calculer cible cible_sp_args : avec TMP, ESP.V_ARG;
afficher_erreur
  nom(TMP) " = " (TMP) ", " 
  nom(ESP.V_ARG) " = " (ESP.V_ARG) "\n";
afficher_erreur
  nom(TMP) " = " (TMP) ", " 
  nom(TUTU[2]) " = " (TUTU[2]) "\n";
calculer cible cible_sp_args : avec TMP, TUTU[2];
afficher_erreur
  nom(TMP) " = " (TMP) ", " 
  nom(TUTU[2]) " = " (TUTU[2]) "\n";
afficher_erreur
  nom(TMP) " = " (TMP) ", " 
  nom(champ_evenement(2, code)) " = " (champ_evenement(2, code)) "\n";
calculer cible cible_sp_args : avec TMP, champ_evenement(2, code);
afficher_erreur
  nom(TMP) " = " (TMP) ", " 
  nom(champ_evenement(2, code)) " = " (champ_evenement(2, code)) "\n";
afficher_erreur "cible_sp_args sans exec ?\n";
calculer cible cible_sp_args : avec TUTU[-2], TMP;
afficher_erreur "cible_sp_args sans exec !\n";
V_ARG = 12;
ESP.V_ARG = 13;
V_TAB[2] = 14;
ESP.V_TAB[2] = 15;
V_CODE = 16;
ESP.V_CODE = 17;
champ_evenement(3, code) reference V_CODE;
V_BLA = 18;
ESP.V_BLA = 19;
afficher_erreur
  nom(V_ARG) " = " (V_ARG) ", " 
  nom(ESP.V_ARG) " = " (ESP.V_ARG) ", "
  nom(V_TAB[2]) " = " (V_TAB[2]) ", " 
  nom(ESP.V_TAB[2]) " = " (ESP.V_TAB[2]) ", "
  nom(V_CODE) " = " (V_CODE) ", "
  nom(ESP.V_CODE) " = " (ESP.V_CODE) ", "
  nom(V_BLA) " = " (V_BLA) ", "
  nom(ESP.V_BLA) " = " (ESP.V_BLA) "\n";
restaurer
: V_ARG, ESP.V_ARG, V_TAB[2], ESP.V_TAB[2], champ_evenement(3, code), ESP.champ_evenement(3, code)
: variable V : categorie saisie contexte : avec attribut(V, primrest) = 10
: variable V : categorie saisie contexte : avec attribut(V, primrest) = 10 : espace ESP
: apres (
  V_ARG = 2;
  ESP.V_ARG = 3;
  V_TAB[2] = 4;
  ESP.V_TAB[2] = 5;
  champ_evenement(3, code) = 6;
  ESP.champ_evenement(3, code) = 7;
  V_BLA = 8;
  ESP.V_BLA = 9;
  afficher_erreur
    nom(V_ARG) " = " (V_ARG) ", " 
    nom(ESP.V_ARG) " = " (ESP.V_ARG) ", "
    nom(V_TAB[2]) " = " (V_TAB[2]) ", " 
    nom(ESP.V_TAB[2]) " = " (ESP.V_TAB[2]) ", "
    nom(V_CODE) " = " (V_CODE) ", "
    nom(ESP.V_CODE) " = " (ESP.V_CODE) ", "
    nom(V_BLA) " = " (V_BLA) ", "
    nom(ESP.V_BLA) " = " (ESP.V_BLA) "\n";
)
afficher_erreur
  nom(V_ARG) " = " (V_ARG) ", " 
  nom(ESP.V_ARG) " = " (ESP.V_ARG) ", "
  nom(V_TAB[2]) " = " (V_TAB[2]) ", " 
  nom(ESP.V_TAB[2]) " = " (ESP.V_TAB[2]) ", "
  nom(V_CODE) " = " (V_CODE) ", "
  nom(ESP.V_CODE) " = " (ESP.V_CODE) ", "
  nom(V_BLA) " = " (V_BLA) ", "
  nom(ESP.V_BLA) " = " (ESP.V_BLA) "\n";
iterer : variable V
: V_ARG, ESP.V_ARG, V_TAB[2], ESP.V_TAB[2], champ_evenement(3, code), ESP.champ_evenement(3, code)
: categorie saisie contexte : avec attribut(V, primrest) = 10
: categorie saisie contexte : avec attribut(V, primrest) = 10 : espace ESP
: dans (
  afficher_erreur "it -- " nom(V) " = " (V) "\n"; 
)
afficher_erreur indenter(-2) "sortie test_cible_avec_args\n";

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
iterer : variable I : entre 0..6 increment 1 : dans (
  A0 = 0;
  A1 = 1;
  A2 = 2;
  A3 = 3;
  A4 = 4;
  A5 = 5;
  A6 = 6;
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
afficher_erreur indenter(2);
afficher_erreur nom(X) " = " (X) " " nom(A) " = " (A) "\n";
X = 444;
A = 12300 + A;
afficher_erreur indenter(-2);

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

fonction test_aff_fonction:
application: iliad;
arguments: ARG0, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6;
resultat: RES;
variables_temporaires: PROUT0, PROUT1, PROUT2;
afficher_erreur "entree test_aff_fonction\n" indenter(2);
afficher_erreur "argument ARG3 <" nom(ARG3) "> <" alias(ARG3) ">\n";
afficher_erreur "resultat RES <" nom(RES) "> <" alias(RES) ">\n";
afficher_erreur indenter(-2) "sortie test_aff_fonction\n";

cible test_aff_cible:
application: iliad;
arguments: ARG0, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6;
variables_temporaires: PROUT0, PROUT1, PROUT2;
afficher_erreur "entree test_aff_cible\n" indenter(2);
afficher_erreur "argument ARG3 <" nom(ARG3) "> <" alias(ARG3) ">\n";
afficher_erreur indenter(-2) "sortie test_aff_cible\n";

cible test_aff:
application: iliad;
variables_temporaires: A0, A1, A2, AA tableau [5], A3, A4, A5, A6, R;
afficher_erreur "entree test_aff\n" indenter(2);
afficher_erreur "saisie V_IND_TRAIT <" nom(V_IND_TRAIT) "> <" alias(V_IND_TRAIT) ">\n";
afficher_erreur "calculee TOTO01 <" nom(TOTO01) "> <" alias(TOTO01) ">\n";
afficher_erreur "calculee tableau TUTU <" nom(TUTU) "> <" alias(TUTU) ">\n";
afficher_erreur "temporaire A0 <" nom(A0) "> <" alias(A0) ">\n";
afficher_erreur "temporaire A2 <" nom(A2) "> <" alias(A2) ">\n";
afficher_erreur "temporaire tableau AA <" nom(AA) "> <" alias(AA) ">\n";
afficher_erreur "temporaire AA[2] <" nom(AA[2]) "> <" alias(AA[2]) ">\n";
iterer : variable VAR : V_IND_TRAIT, TOTO01, A2 : dans (
  afficher_erreur "reference VAR <" nom(VAR) "> <" alias(VAR) ">\n";
)
afficher_erreur "champ_evenement(2, code) <" nom(champ_evenement(2, code)) "> <" alias(champ_evenement(2, code)) ">\n";
R = test_aff_fonction(A0, A1, A2, A3, A4, A5, A6);
calculer cible test_aff_cible : avec A0, A1, A2, A3, A4, A5, A6;
afficher_erreur indenter(-2) "sortie test_aff\n";

cible test_tab:
application: iliad;
variables_temporaires: A0, A1, A2, AA tableau [5], A3, A4, A5, A6, R;
afficher_erreur "entree test_tab\n" indenter(2);
iterer : variable I : entre 0..4 increment 1 : dans (
  AA[I] = 1000 + I;
)
afficher_erreur nom(AA) "[-2] = " (AA[-2]) "\n";
iterer : variable I : entre 0..4 increment 1 : dans (
  afficher_erreur nom(AA) "[" (I) "] = " (AA[I]) "\n";
)
afficher_erreur nom(AA) "[7] = " (AA[7]) "\n";
iterer : variable I : entre 0..4 increment 1 : dans (
  TUTU[I] = 1000 + I;
)
afficher_erreur nom(TUTU) "[-2] = " (TUTU[-2]) "\n";
iterer : variable I : entre 0..4 increment 1 : dans (
  afficher_erreur nom(TUTU) "[" (I) "] = " (TUTU[I]) "\n";
)
afficher_erreur nom(TUTU) "[7] = " (TUTU[7]) "\n";
afficher_erreur indenter(-2) "sortie test_tab\n";

cible test_est_variable:
application: iliad;
variables_temporaires: A0, AA tableau[25], AKK3, X;
afficher_erreur "entree test_est_variable\n" indenter(2);
X = 3;
afficher_erreur
  nom(V_ANREV) ": V_ANREV " (est_variable(V_ANREV, V_ANREV))
  ", ANREV " (est_variable(V_ANREV, ANREV))
  ", PROUT " (est_variable(V_ANREV, PROUT)) "\n";
afficher_erreur
  nom(TUTU) ": TUTU " (est_variable(TUTU, TUTU))
  ", PROUT " (est_variable(TUTU, PROUT)) "\n";
afficher_erreur
  nom(TUTU) "[" (X) "]: TUTU3 " (est_variable(TUTU[X], TUTU3))
  ", PROUT " (est_variable(TUTU[X], PROUT)) "\n";
afficher_erreur
  nom(A0) ": A0 " (est_variable(A0, A0))
  ", PROUT " (est_variable(A0, PROUT)) "\n";
afficher_erreur
  nom(AA) ": AA " (est_variable(AA, AA))
  ", PROUT " (est_variable(AA, PROUT)) "\n";
afficher_erreur
  nom(AA) "[" (X) "]: AA03 " (est_variable(AA[X], AA03))
  ", PROUT " (est_variable(AA[X], PROUT)) "\n";
iterer
: variable VAR
: V_ANREV, A0, AA03, AKK3
: dans (
  afficher_erreur nom(VAR) ": "
  "V_ANREV " (est_variable(VAR, V_ANREV))
  ", ANREV " (est_variable(VAR, ANREV))
  ", TUTU " (est_variable(VAR, TUTU))
  ", A0 " (est_variable(VAR, A0))
  ", AA03 " (est_variable(VAR, AA03))
  ", AKK3 " (est_variable(VAR, AKK3))
  ", PROUT " (est_variable(VAR, PROUT)) "\n";
)
afficher_erreur
  nom(champ_evenement(0, code)) ": "
  "RESULTAT " (est_variable(champ_evenement(0, code), RESULTAT))
  ", PROUT " (est_variable(champ_evenement(0, code), PROUT)) "\n";
afficher_erreur indenter(-2) "sortie test_est_variable\n";

cible tests:
application: iliad;
variables_temporaires: U0, UUU tableau[5], U1;
calculer cible test_esp;
calculer cible test_cible_avec_args;
#calculer cible test_varcons;
#calculer cible test_args;
#calculer cible test_tmpref;
#calculer cible test_aff;
#calculer cible test_tab;
#calculer cible test_est_variable;

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



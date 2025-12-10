application test;

V_ANCSDED : saisie revenu acompte = 0 avfisc = 0 categorie_TL = 0 classe = 0 cotsoc = 0 ind_abat = 0 modcat = 0 nat_code = 0 primrest = 0 priorite = 0 rapcat = 0 sanction = 0 alias V_POUET: "v_ancsed";


fonction toto_fonction:
application: test;
arguments: A0, A1, A2, A3, A4, A5, A6;
resultat: R;
variables_temporaires: PROUT0, PROUT1, PROUT2;
R = A0 + A1 + A2 + A3 + A4 + A5 + A6;

cible toto_cible:
application: test;
arguments: A0, A1, A2, A3, A4, A5, A6, R;
iterer : variable PROUT0 : categorie calculee * : dans (
  iterer : variable PROUT1 : categorie calculee * : dans (
    iterer : variable PROUT2 : categorie calculee * : dans (
      R = A0 + A1 + A2 + A3 + A4 + A5 + A6;
    )
  )
)

cible test_args:
application: test;
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
afficher_erreur "\n";
afficher_erreur indenter(-2) "sortie test_args\n";

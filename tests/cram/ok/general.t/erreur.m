A100:anomalie :"famille":"code_bo":"sous_code":"libelle":"is_isf";
application app;

V_ANCSDED : saisie revenu acompte = 0 avfisc = 0 categorie_TL = 0 classe = 0 cotsoc = 0 ind_abat = 0 modcat = 0 nat_code = 0 primrest = 0 priorite = 0 rapcat = 0 sanction = 0 alias V_ANCSDED : "v_ancsed";
X: calculee restituee primrest = 0: "x";

regle 1:
application: app;
X = 3;

verif 1:
application: app;

si X > 1 alors erreur A100;

cible target:
application: app;
calculer domaine primitive;
afficher_erreur "whatever\n";
leve_erreur A100;
finalise_erreurs;

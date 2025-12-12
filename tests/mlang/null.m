application app;

V_ANCSDED : saisie revenu acompte = 0 avfisc = 0 categorie_TL = 0 classe = 0 cotsoc = 0 ind_abat = 0 modcat = 0 nat_code = 0 primrest = 0 priorite = 0 rapcat = 0 sanction = 0 alias V_POUET: "v_ancsed";
X : calculee restituee primrest = 0 base : "x";
Y : calculee restituee primrest = 0 base : "y";
Z : calculee restituee primrest = 0 base : "z";

regle 1337:
application: app;
X = -3;
Y = null(X);
X = 0;
Z = null(X);

cible target:
application: app;
calculer domaine primitive;

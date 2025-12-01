application app;

V_ANCSDED : saisie revenu acompte = 0 avfisc = 0 categorie_TL = 0 classe = 0 cotsoc = 0 ind_abat = 0 modcat = 0 nat_code = 0 primrest = 0 priorite = 0 rapcat = 0 sanction = 0 alias V_POUET: "v_ancsed";
X: calculee restituee primrest = 0: "x";
Y: calculee primrest = 0: "y";
TXMARJ: calculee primrest = 0: "TXMARJ";
ANNEE: calculee primrest = 0: "annee en cours";
TAUX: const=20;
REVENU : calculee restituee primrest = 0: "revenu en fin";
ENTREE: saisie revenu acompte = 0 avfisc = 0 categorie_TL = 0 classe = 0 cotsoc = 0 ind_abat = 0 modcat = 0 nat_code = 0 primrest = 0 priorite = 0 rapcat = 0 sanction = 0 alias IDEF: "entree suite IRDV";

regle 1337:
application: app;

X = 3;
Y = 3 + X * TAUX;
TXMARJ = Y - TAUX;
REVENU = ENTREE * TAUX / 100 + TXMARJ;
cible target:
application: app;
calculer domaine primitive;

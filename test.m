Z : saisie revenu alias Z : "UNKOWN" type ENTIER;
X1 : calculee : "UNKNOWN" type ENTIER;
X2 : calculee : "UNKNOWN" type ENTIER;
X3 : calculee : "UNKNOWN" type ENTIER;
X4 : calculee : "UNKNOWN" type ENTIER;
X5 : calculee : "UNKNOWN" type ENTIER;
X6 : calculee : "UNKNOWN" type ENTIER;
X7 : calculee : "UNKNOWN" type ENTIER;
X8 : calculee : "UNKNOWN" type ENTIER;
X9 : calculee : "UNKNOWN" type ENTIER;
X10 : calculee : "UNKNOWN" type ENTIER;
Y : tableau[9] calculee base : " UNKOWN" type ENTIER;
sortie (X9);
sortie (Y);
sortie (X10);


regle 1:
application : truc  ;
X1 = X2 + 1;
X2 = 3;
X3 = X2 + 3;

pour i=4..5:
Xi = si Z > 2 alors 42 sinon 1 finsi;

X6 = X8;
X7 = X8;
X8 = Z;

X9 = somme(i=1..8:Xi);
Y[0] = X9;
Y[X] = 3*X - Z + X9 / 6;

X10 = Y[5];

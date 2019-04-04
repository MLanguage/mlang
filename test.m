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
Y : tableau[9] calculee base : " UNKOWN" type ENTIER;
sortie (X9);
sortie (Y);


regle 1:
application : truc  ;
X1 = X2 + 1;
X2 = 3;
X3 = X2 + 3;
X4 = Z + 4;

pour i=5..8:
Xi = Z + 45;

X9 = somme(i=1..8:Xi);
Y[0] = X9;
Y[X] = 3*X - Z + X9 / 6;

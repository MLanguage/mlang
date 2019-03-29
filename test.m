Z : saisie revenu alias Z : "UNKOWN" ;
M : const=1.00000  ;
X1 : calculee : "UNKNOWN";
X2 : calculee : "UNKNOWN";
X3 : calculee : "UNKNOWN";
X4 : calculee : "UNKNOWN";
X5 : calculee : "UNKNOWN";
X6 : calculee : "UNKNOWN";
X7 : calculee : "UNKNOWN";
X8 : calculee : "UNKNOWN";
X9 : calculee : "UNKNOWN";
Y : tableau[9] calculee base : " UNKOWN" ;

regle 1:
application : truc  ;
X1 = Z + 1;
X2 = Z + 2;
X3 = Z + 3;
X4 = Z + 4;
X5 = Z + 5;
X6 = Z + 6;
X7 = Z + 7;
X8 = Z + 8;
X9 = somme(i=1..8:Xi);
Y[X] = X9 + 3*X;

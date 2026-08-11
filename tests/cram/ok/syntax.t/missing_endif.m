application app;
variable saisie : attribut mon_attribut;
X : saisie mon_attribut = 0 alias AX : "";
Z : calculee restituee primrest = 0 : "";

cible cond:
application: app;
si positif(X)
  alors Z = 1;
  sinon Z = 0;

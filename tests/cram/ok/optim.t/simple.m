application app;
variable saisie : attribut mon_attribut;
X : saisie mon_attribut = 0 alias AX : "";
Y : saisie mon_attribut = 0 alias AY : "";
Z : calculee restituee primrest = 0 : "";

cible soustraction:
application : app;
Z = X * (X - Y + X);

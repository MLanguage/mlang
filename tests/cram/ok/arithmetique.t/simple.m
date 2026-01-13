espace_variables GLOBAL : par_defaut;
domaine regle mon_domaine_de_regle: par_defaut;
domaine verif mon_domaine_de_verifications: par_defaut;
application app;
variable saisie : attribut mon_attribut;
variable calculee : attribut mon_attribut;
X : saisie mon_attribut = 0 alias AX : "";
Y : saisie mon_attribut = 0 alias AY : "";
Z : calculee restituee mon_attribut = 0 : "";

cible addition:
application : app;
Z = X + Y;

cible soustraction:
application : app;
Z = X - Y;

cible multiplication:
application : app;
Z = X * Y;

cible division:
application : app;
Z = X / Y;

application app;

X : calculee primrest = 0 : "" type REEL;
Y : calculee primrest = 0 : "" type REEL;
Z : calculee primrest = 0 : "" type REEL;

cible aiguillage_val:
application: app;
arguments: V;
aiguillage(V): (
  cas 0: Z = 0;
  cas 1: Z = 1;
  cas indefini: Z = indefini;
  par_defaut: Z = -1; 
)

cible aiguillage_var:
application: app;
arguments: V;
aiguillage nom (V): (
  cas X: Z = 0;
  cas Y: Z = 1;
  cas Z: Z = indefini;
  par_defaut: Z = -1;
)

cible aigui_test:
application : app;
X = 0;
Y = 1;
calculer cible aiguillage_val : avec X;
afficher "Z = " (Z) "\n";
calculer cible aiguillage_val : avec Y;
afficher "Z = " (Z)"\n";
Z = indefini;
calculer cible aiguillage_val : avec Z;
afficher "Z = " (Z)"\n";
calculer cible aiguillage_var : avec X;
afficher "Z = " (Z)"\n";
calculer cible aiguillage_var : avec Y;
afficher "Z = " (Z)"\n";
calculer cible aiguillage_var : avec Z;
afficher "Z = " (Z)"\n";

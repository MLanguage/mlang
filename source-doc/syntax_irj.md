(syntax_irj)=

# Les fichiers IRJ

Les fichiers IRJ sont des fichiers de test faisant corespondre entrées et
sorties du calcul d'un programme M.
%%
Il sont divisés en 7 parties.

* `NOM` : le nom du test.
* `ENTREES-PRIMITIF` : les valeurs des variables d'entrées au début du calcul. A chaque variable `VAR` est associée une valeur `val` entière ou décimale sur une ligne de la forme `VAR/val`. Les variables d'entrées qui ne sont pas définies dans le fichier IRJ seront initialisées à `indefini`.
* `CONTROLES-PRIMITIF` : à documenter
* `RESULTATS-PRIMITIF` : les valeurs des variables restituées à la fin du calcul. Comme pour les variables d'entrées, elles sont décrites sous la forme `VAR/val`. Les valeurs restituées absentes du fichiers IRJ sont supposées avoir la valeur `indefini` à la fin du calcul.
* `ENTREES-CORRECTIF` : à documenter
* `CONTROLES-CORRECTIF` : à documenter
* `RESULTATS-CORRECTIF` : à documenter

Voici un exemple de fichier IRJ minimal
```   
   #NOM
   MON-TEST
   #ENTREES-PRIMITIF
   X/0.0
   #CONTROLES-PRIMITIF
   #RESULTATS-PRIMITIF
   Y/1
   Z/2.0
   #ENTREES-CORRECTIF
   #CONTROLES-CORRECTIF
   #RESULTATS-CORRECTIF
   ##
```

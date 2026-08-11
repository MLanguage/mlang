Addition
Ajouter deux indefinis renvoie un indefini
  $ mlang simple.m --mpp_function addition -A app --without_dfgip_m --run_test deux_indef.irj --no_nondet_display
  [RESULT] deux_indef.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!

Ajouter une valeur a indefini renvoie la valeur
  $ mlang simple.m --mpp_function addition -A app --without_dfgip_m --run_test add_x_i.irj --no_nondet_display
  [RESULT] add_x_i.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!

Sinon, l'addition se calcule bien
  $ mlang simple.m --mpp_function addition -A app --without_dfgip_m --run_test add_x_y_1.irj --no_nondet_display
  [RESULT] add_x_y_1.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!
  $ mlang simple.m --mpp_function addition -A app --without_dfgip_m --run_test add_x_y_2.irj --no_nondet_display
  [ERROR] KO | Z attendue : -2 - evaluée : 25995
  [ERROR] Erreurs:
  add_x_y_2.irj: 1 erreur
  [RESULT] Test exécuté!

Soustraction
  $ mlang simple.m --mpp_function soustraction -A app --without_dfgip_m --run_test deux_indef.irj --no_nondet_display
  [RESULT] deux_indef.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!
  $ mlang simple.m --mpp_function soustraction -A app --without_dfgip_m --run_test sub_x_y_1.irj --no_nondet_display
  [RESULT] sub_x_y_1.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!

Multiplication
  $ mlang simple.m --mpp_function multiplication -A app --without_dfgip_m --run_test deux_indef.irj --no_nondet_display
  [RESULT] deux_indef.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!
  $ mlang simple.m --mpp_function multiplication -A app --without_dfgip_m --run_test mul_x_y_1.irj --no_nondet_display
  [RESULT] mul_x_y_1.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!

Division
  $ mlang simple.m --mpp_function division -A app --without_dfgip_m --run_test deux_indef.irj --no_nondet_display
  [RESULT] deux_indef.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!
La division par zero ne renvoie pas indefini, mais bien la valeur zero.
  $ mlang simple.m --mpp_function division -A app --without_dfgip_m --run_test div_x_0.irj --no_nondet_display
  [ERROR] KO | Z attendue : indefini - evaluée : 0
  [ERROR] Erreurs:
  div_x_0.irj: 1 erreur
  [RESULT] Test exécuté!

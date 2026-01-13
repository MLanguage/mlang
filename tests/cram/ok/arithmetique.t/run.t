Addition
Ajouter deux indefinis renvoie un indefini
  $ mlang simple.m --mpp_function addition -A app --without_dfgip_m --run_test deux_indef.irj --no_nondet_display
  [RESULT] deux_indef.irj
  [RESULT] No failure!
  [RESULT] Test passed!

Ajouter une valeur à indefini renvoie la valeur
  $ mlang simple.m --mpp_function addition -A app --without_dfgip_m --run_test add_x_i.irj --no_nondet_display
  [RESULT] add_x_i.irj
  [RESULT] No failure!
  [RESULT] Test passed!

Sinon, l'addition se calcule bien
  $ mlang simple.m --mpp_function addition -A app --without_dfgip_m --run_test add_x_y_1.irj --no_nondet_display
  [RESULT] add_x_y_1.irj
  [RESULT] No failure!
  [RESULT] Test passed!
  $ mlang simple.m --mpp_function addition -A app --without_dfgip_m --run_test add_x_y_2.irj --no_nondet_display
  [ERROR] KO | Z expected: -2.000000 - evaluated: 25995.000000
  [ERROR] Failure: 1 errors in file add_x_y_2.irj
  [RESULT] Test passed!

Soustraction
  $ mlang simple.m --mpp_function soustraction -A app --without_dfgip_m --run_test deux_indef.irj --no_nondet_display
  [RESULT] deux_indef.irj
  [RESULT] No failure!
  [RESULT] Test passed!

Multiplication
  $ mlang simple.m --mpp_function multiplication -A app --without_dfgip_m --run_test deux_indef.irj --no_nondet_display
  [RESULT] deux_indef.irj
  [RESULT] No failure!
  [RESULT] Test passed!

Division
  $ mlang simple.m --mpp_function division -A app --without_dfgip_m --run_test deux_indef.irj --no_nondet_display
  [RESULT] deux_indef.irj
  [RESULT] No failure!
  [RESULT] Test passed!

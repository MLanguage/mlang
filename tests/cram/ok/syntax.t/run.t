Dans cet exemple, il manque un ';' dans la cible 'addition'.
  $ mlang missing_semicolon.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Symbole innattendu. Avez-vous oublié un ';'?
  
    --> missing_semicolon.m
     | 
  17 | cible soustraction:
     | ^^^^^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]

  $ LANG="en" mlang missing_semicolon.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Unexpected symbol. Did you forget ';'?
  
    --> missing_semicolon.m
     | 
  17 | cible soustraction:
     | ^^^^^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]

Dans cet exemple, il manque une valeur après le signe '='
  $ mlang missing_value_after_eq.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Valeur manquante après le symbole '='.
  
    --> missing_value_after_eq.m
     | 
  17 | cible soustraction:
     | ^^^^^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]

  $ LANG="en" mlang missing_value_after_eq.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Missing value after symbol '='.
  
    --> missing_value_after_eq.m
     | 
  17 | cible soustraction:
     | ^^^^^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]
Dans cet exemple, il manque une valeur après le signe '=' dans la définition
de l'attribut de X
  $ mlang missing_attr_value.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Valeur manquante après le symbole '='.
  
    --> missing_attr_value.m
     | 
  10 | X : saisie mon_attribut = alias AX : "";
     |                           ^^^^^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]

  $ LANG="en" mlang missing_attr_value.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Missing value after symbol '='.
  
    --> missing_attr_value.m
     | 
  10 | X : saisie mon_attribut = alias AX : "";
     |                           ^^^^^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]
Dans cet exemple, il manque un ':' après les attributs de Z
  $ mlang incomplete_attr_list.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Liste d'attributs incomplète. Avez-vous oublié de terminer la liste avec ':' ?
  
    --> incomplete_attr_list.m
     | 
  11 | Z : calculee restituee mon_attribut = 0 "";
     |                                          ^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]

  $ LANG="en" mlang incomplete_attr_list.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Incomplete attribute list. Did you forget to end the list with ':'?
  
    --> incomplete_attr_list.m
     | 
  11 | Z : calculee restituee mon_attribut = 0 "";
     |                                          ^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]

Dans cet exemple, il manque la définition de l'attribut de Z
  $ mlang incomplete_attr_definition.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Définition incomplète d'un attribut.
  
    --> incomplete_attr_definition.m
     | 
  11 | Z : calculee restituee mon_attribut : "";
     |                                     ^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]

  $ LANG="en" mlang incomplete_attr_definition.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Incomplete attribute definition.
  
    --> incomplete_attr_definition.m
     | 
  11 | Z : calculee restituee mon_attribut : "";
     |                                     ^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]

Dans cet exemple, il manque 'finsi' à l'instruction conditionnelle.
  $ mlang missing_endif.m --mpp_function cond -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Si-alors-sinon incomplet. Avez-vous oublié un 'finsi'?
  
    --> missing_endif.m
     | 
  11 | 
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]
  $ LANG="en" mlang missing_endif.m --mpp_function cond -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Unclosed if-then-else. Did you forget 'finsi'?
  
    --> missing_endif.m
     | 
  11 | 
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]


Dans cet exemple, il manque ':' dans la règle, a la selection de l'application.
  $ mlang missing_colon.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Symbole ':' manquant.
  
    --> missing_colon.m
     | 
  14 | application app;
     |             ^^^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]
  $ LANG="en" mlang missing_colon.m --mpp_function addition -A app --without_dfgip_m  --no_nondet_display
  [ERROR] Missing ':'.
  
    --> missing_colon.m
     | 
  14 | application app;
     |             ^^^
  
  Fatal error: exception Utils.Errors.StructuredError(_)
  [2]

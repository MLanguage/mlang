Mlang avec l'optimisation remplaçant les accès mémoire au TGV via irdata par des accès à des variables locales
  $ mlang simple.m --mpp_function soustraction --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c -A app -Olvfa > /dev/null
  $ cat output/m_simple.c
  #include "mlang.h" 
  
  struct S_discord * soustraction(T_irdata* irdata) {
    int sav35_nb_tmps_target = irdata->nb_tmps_target;
    int sav35_nb_refs_target = irdata->nb_refs_target;
    char *def_saisie = irdata->def_saisie;
    double *saisie = irdata->saisie;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
    char *def_base = irdata->def_base;
    double *base = irdata->base;
    T_var_space var_space = irdata->var_space_courant;
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 0;
    
    {
      (def_calculee[0/*Z*/]) = ((def_saisie[0/*X*/]) || (def_saisie[1/*Y*/]));
      if ((def_calculee[0/*Z*/])) {
        (calculee[0/*Z*/]) = (((saisie[0/*X*/])) - ((saisie[1/*Y*/])));
      } else (calculee[0/*Z*/]) = 0.0;
    }
    label_soustraction: ;
    
    irdata->nb_refs_target = sav35_nb_refs_target;
    irdata->nb_tmps_target = sav35_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=soustraction
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides
Mlang avec l'optimisation vérifiant si une règle peut être arrêtée ou non
  $ mlang simple.m --mpp_function soustraction --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c -A app -Oncur > /dev/null
  $ cat output/m_simple.c
  #include "mlang.h" 
  
  struct S_discord * soustraction(T_irdata* irdata) {
    int sav35_nb_tmps_target = irdata->nb_tmps_target;
    int sav35_nb_refs_target = irdata->nb_refs_target;
    T_var_space var_space = irdata->var_space_courant;
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 0;
    
    {
      (irdata->def_calculee[0/*Z*/]) =
        ((irdata->def_saisie[0/*X*/]) || (irdata->def_saisie[1/*Y*/]));
      if ((irdata->def_calculee[0/*Z*/])) {
        (irdata->calculee[0/*Z*/]) =
          (((irdata->saisie[0/*X*/])) - ((irdata->saisie[1/*Y*/])));
      } else (irdata->calculee[0/*Z*/]) = 0.0;
    }
    label_soustraction: ;
    
    irdata->nb_refs_target = sav35_nb_refs_target;
    irdata->nb_tmps_target = sav35_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=soustraction
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides
Mlang avec l'optimisation supprimant les variables boolénnes redondantes dans les formules
  $ mlang simple.m --mpp_function soustraction --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c -A app -Onrbf > /dev/null
  $ cat output/m_simple.c
  #include "mlang.h" 
  
  struct S_discord * soustraction(T_irdata* irdata) {
    int sav35_nb_tmps_target = irdata->nb_tmps_target;
    int sav35_nb_refs_target = irdata->nb_refs_target;
    T_var_space var_space = irdata->var_space_courant;
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 0;
    
    {
      (irdata->def_calculee[0/*Z*/]) =
        ((irdata->def_saisie[0/*X*/]) || (irdata->def_saisie[1/*Y*/]));
      if ((irdata->def_calculee[0/*Z*/])) {
        (irdata->calculee[0/*Z*/]) =
          (((irdata->saisie[0/*X*/])) - ((irdata->saisie[1/*Y*/])));
      } else (irdata->calculee[0/*Z*/]) = 0.0;
    }
    label_soustraction: ;
    
    irdata->nb_refs_target = sav35_nb_refs_target;
    irdata->nb_tmps_target = sav35_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=soustraction
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides
Mlang avec toutes les optimisations
  $ mlang simple.m --mpp_function soustraction --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c -A app -O* > /dev/null
  $ cat output/m_simple.c
  #include "mlang.h" 
  
  struct S_discord * soustraction(T_irdata* irdata) {
    int sav35_nb_tmps_target = irdata->nb_tmps_target;
    int sav35_nb_refs_target = irdata->nb_refs_target;
    char *def_saisie = irdata->def_saisie;
    double *saisie = irdata->saisie;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
    char *def_base = irdata->def_base;
    double *base = irdata->base;
    T_var_space var_space = irdata->var_space_courant;
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 0;
    
    {
      (def_calculee[0/*Z*/]) = ((def_saisie[0/*X*/]) | (def_saisie[1/*Y*/]));
      if ((def_calculee[0/*Z*/])) {
        (calculee[0/*Z*/]) = (((saisie[0/*X*/])) - ((saisie[1/*Y*/])));
      } else (calculee[0/*Z*/]) = 0.0;
    }
    label_soustraction: ;
    
    irdata->nb_refs_target = sav35_nb_refs_target;
    irdata->nb_tmps_target = sav35_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=soustraction
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides

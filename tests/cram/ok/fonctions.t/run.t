
  $ mlang test.m --mpp_function fun_test -A mon_application --income-year=2020 --dgfip_options="-m2020,-X" --run_test test.irj --no_nondet_display
  __ABS__
    abs(indefini) = indefini
    abs(1) = 1
    abs(-1) = 1
  
  __AFFICHER__
    afficher "Bonjour, monde!" : Bonjour, monde!
    afficher (0) : 0
    afficher (3.1415926535):2 : 3,14
    afficher (3.1415926535):2..4 : 3,1416
  
  __ARR__
    arr(indefini) = indefini
    arr(1.8) = 2
    arr(-1.7) = -2
  
  __ATTRIBUT__
    attribut(X, primrest) = 0
    attribut(TAB, primrest) = 2
  
  __CHAMP_EVENEMENT__
    Avant d'initialiser les champs de l'événement:
      champ_evenement(0, numero) = indefini
      champ_evenement(0, code) = indefini
    Après avoir initialisé les champs de l'événement:
      champ_evenement(0, numero) = 42
      champ_evenement(0, code) = 2
  
  __INF__
    inf(indefini) = indefini
    inf(1.8) = 1
    inf(-1.7) = -2
  
  __MAX__
    max(indefini, indefini) = indefini
    max(-1, indefini) = 0
    max(indefini, -1) = 0
    max(1, indefini) = 1
  
  __MEME_VARIABLE__
    meme_variable(X,X) = 1
    meme_variable(X,TAB) = 0
    meme_variable(TAB[0],TAB) = 0
  
  __MIN__min(indefini, indefini) = indefini
    min(1, indefini) = 0
    min(indefini, 1) = 0
    min(-1, indefini) = -1
  
  __MULTIMAX__
    Avant initialisation du tableau :
      multimax(indefini, TAB) = indefini
      multimax(7, TAB) = indefini
      multimax(taille(TAB) + 1, TAB) = indefini
      multimax(0, TAB) = indefini
      multimax(-1, TAB) = indefini
    Après initialisation du tableau :
      multimax(indefini, TAB) = indefini
      multimax(7, TAB) = 6
      multimax(taille(TAB) + 1, TAB) = 9
      multimax(0, TAB) = indefini
      multimax(-1, TAB) = indefini
  
  __NB_EVENEMENTS__
    Avant la définition d'un événement :
      nb_evenements() = 0
    Pendant la définition d'un événement :
      nb_evenements() = 1
    Après la définition d'un événement :
      nb_evenements() = 0
  
  __NULL__
    null(indefini) = indefini
    null(0) = 1
    null(1) = 0
  
  __POSITIF__
    positif(indefini) = indefini
    positif(0) = 0
    positif(1) = 1
    positif(-1) = 0
  
  __POSITIF OU NUL__
    positif_ou_nul(indefini) = indefini
    positif_ou_nul(0) = 1
    positif_ou_nul(1) = 1
    positif_ou_nul(-1) = 0
  
  __PRESENT__
    present(indefini) = 0
    present(0) = 1
    present(1) = 1
  
  __SOMME__
    somme() = 0
    somme(indefini) = indefini
    somme(1, indefini) = 1
    somme(1, 2, 3, 4, 5) = 15
  
  __SUPZERO__
    supzero(indefini) = indefini
    supzero(42) = 42
    supzero(-1) = indefini
    supzero(0) = indefini
  
  __TAILLE__
    taille(TAB) = 10
    taille(X) = 1
  
  __TYPE__
    type(X, REEL) = 1
    type(X, ENTIER) = 0
    type(TAB, ENTIER) = 1
    type(Y, ENTIER) = 0
    type(Y, REEL) = 0
    type(Y, BOOLEEN) = 0
    type(Y, DATE_AAAA) = 0
    type(Y, DATE_JJMMAAAA) = 0
    type(Y, DATE_MM) = 0
  
  [RESULT] test.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!

  $ mlang test.m --mpp_function fun_test -A mon_application --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c > /dev/null
  $ cat output/m_test.c
  #include "mlang.h" 
  
  struct S_discord * fun_test(T_irdata* irdata) {
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
      test_abs(irdata);
    }
    
    {
      test_afficher(irdata);
    }
    
    {
      test_arr(irdata);
    }
    
    {
      test_attribut(irdata);
    }
    
    {
      test_champ_evenement(irdata);
    }
    
    {
      test_inf(irdata);
    }
    
    {
      test_max(irdata);
    }
    
    {
      test_meme_variable(irdata);
    }
    
    {
      test_min(irdata);
    }
    
    {
      test_multimax(irdata);
    }
    
    {
      test_nb_evenements(irdata);
    }
    
    {
      test_null(irdata);
    }
    
    {
      test_positif(irdata);
    }
    
    {
      test_positif_ou_nul(irdata);
    }
    
    {
      test_present(irdata);
    }
    
    {
      test_somme(irdata);
    }
    
    {
      test_supzero(irdata);
    }
    
    {
      test_taille(irdata);
    }
    
    {
      test_type(irdata);
    }
    
    label_fun_test: ;
    
    irdata->nb_refs_target = sav35_nb_refs_target;
    irdata->nb_tmps_target = sav35_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * initalise_tab(T_irdata* irdata) {
    int sav36_nb_tmps_target = irdata->nb_tmps_target;
    int sav36_nb_refs_target = irdata->nb_refs_target;
    char *def_saisie = irdata->def_saisie;
    double *saisie = irdata->saisie;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
    char *def_base = irdata->def_base;
    double *base = irdata->base;
    T_var_space var_space = irdata->var_space_courant;
    {
      int i;
      T_varinfo *info;
      for (i = 0; i < 1; i++) {
        irdata->tmps[irdata->tmps_org + i].def = 0;
        irdata->tmps[irdata->tmps_org + i].val = 0.0;
        irdata->tmps[irdata->tmps_org + i].info = NULL;
      }
      irdata->tmps_org = irdata->tmps_org + 1;
    }
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 0;
    
    {
      {
        double i_val37;
        char e0_def37;
        double e0_val37;
        char e1_def37;
        double e1_val37;
        char step_def37;
        double step_val37;
        {
          e0_def37 = 1;
          e0_val37 = 0.0;
        }
        {
          e1_def37 = 1;
          e1_val37 = 9.0;
        }
        {
          step_def37 = 1;
          step_val37 = 1.0;
        }
        if(e0_def37 && e1_def37 && step_def37 && step_val37 != 0.0) {
          for (i_val37 = e0_val37;
            (step_val37 > 0.0 ? i_val37 <= e1_val37 : i_val37 >= e1_val37);
            i_val37 = i_val37 + step_val37) {
            DT_((-1)/*I*/) = 1;
            T_((-1)/*I*/) = i_val37;
            {
              T_varinfo *info = tab_varinfo[0];
              char idx38_def;
              double idx38_val;
              int idx38;
              {
                idx38_def = DT_((-1)/*I*/);
                if (idx38_def) {
                  idx38_val = T_((-1)/*I*/);
                } else idx38_val = 0.0;
              }
              idx38 = (int)idx38_val;
              if (idx38_def && 0 <= idx38 && idx38 < info->size) {
                char res39_def;
                double res39_val;
                {
                  res39_def = DT_((-1)/*I*/);
                  if (res39_def) {
                    res39_val = T_((-1)/*I*/);
                  } else res39_val = 0.0;
                }
                ecris_tabaccess(irdata, (irdata->var_space), 0, idx38_def, idx38_val, res39_def, res39_val);
              }
            }
          }
        }
      }
      label_I_0:;} /* End of scope label_I_0 */
    label_initalise_tab: ;
    
    irdata->tmps_org = irdata->tmps_org - 1;
    irdata->nb_refs_target = sav36_nb_refs_target;
    irdata->nb_tmps_target = sav36_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * reinitialise_tab(T_irdata* irdata) {
    int sav40_nb_tmps_target = irdata->nb_tmps_target;
    int sav40_nb_refs_target = irdata->nb_refs_target;
    char *def_saisie = irdata->def_saisie;
    double *saisie = irdata->saisie;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
    char *def_base = irdata->def_base;
    double *base = irdata->base;
    T_var_space var_space = irdata->var_space_courant;
    {
      int i;
      T_varinfo *info;
      for (i = 0; i < 1; i++) {
        irdata->tmps[irdata->tmps_org + i].def = 0;
        irdata->tmps[irdata->tmps_org + i].val = 0.0;
        irdata->tmps[irdata->tmps_org + i].info = NULL;
      }
      irdata->tmps_org = irdata->tmps_org + 1;
    }
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 0;
    
    {
      {
        double i_val41;
        char e0_def41;
        double e0_val41;
        char e1_def41;
        double e1_val41;
        char step_def41;
        double step_val41;
        {
          e0_def41 = 1;
          e0_val41 = 0.0;
        }
        {
          e1_def41 = 1;
          e1_val41 = 9.0;
        }
        {
          step_def41 = 1;
          step_val41 = 1.0;
        }
        if(e0_def41 && e1_def41 && step_def41 && step_val41 != 0.0) {
          for (i_val41 = e0_val41;
            (step_val41 > 0.0 ? i_val41 <= e1_val41 : i_val41 >= e1_val41);
            i_val41 = i_val41 + step_val41) {
            DT_((-1)/*I*/) = 1;
            T_((-1)/*I*/) = i_val41;
            {
              T_varinfo *info = tab_varinfo[0];
              char idx42_def;
              double idx42_val;
              int idx42;
              {
                idx42_def = DT_((-1)/*I*/);
                if (idx42_def) {
                  idx42_val = T_((-1)/*I*/);
                } else idx42_val = 0.0;
              }
              idx42 = (int)idx42_val;
              if (idx42_def && 0 <= idx42 && idx42 < info->size) {
                char res43_def;
                double res43_val;
                {
                  res43_def = 0;
                  if (res43_def) {
                    res43_val = 0.0;
                  } else res43_val = 0.0;
                }
                ecris_tabaccess(irdata, (irdata->var_space), 0, idx42_def, idx42_val, res43_def, res43_val);
              }
            }
          }
        }
      }
      label_I_1:;} /* End of scope label_I_1 */
    label_reinitialise_tab: ;
    
    irdata->tmps_org = irdata->tmps_org - 1;
    irdata->nb_refs_target = sav40_nb_refs_target;
    irdata->nb_tmps_target = sav40_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_abs(T_irdata* irdata) {
    int sav44_nb_tmps_target = irdata->nb_tmps_target;
    int sav44_nb_refs_target = irdata->nb_refs_target;
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
      char print45_def;
      double print45_val;
      int print45;
      print_string(stdout, &(irdata->ctx_pr_out), "__ABS__\012");
      {
        print45_def = 1;
        print45_val = 2.0;
      }
      if (print45_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print45_val);
      }
    }
    {
      char print46_def;
      double print46_val;
      int print46;
      print_string(stdout, &(irdata->ctx_pr_out), "abs(indefini) = ");
      {
        print46_def = 0;
        if (print46_def) {
          print46_val = fabs(0.0);
        } else print46_val = 0.0;
      }
      if (print46_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print46_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print47_def;
      double print47_val;
      int print47;
      print_string(stdout, &(irdata->ctx_pr_out), "abs(1) = ");
      {
        print47_def = 1;
        print47_val = fabs(1.0);
      }
      if (print47_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print47_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print48_def;
      double print48_val;
      int print48;
      print_string(stdout, &(irdata->ctx_pr_out), "abs(-1) = ");
      {
        print48_def = 1;
        print48_val = fabs(-1.0);
      }
      if (print48_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print48_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print49_def;
      double print49_val;
      int print49;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print49_def = 1;
        print49_val = -2.0;
      }
      if (print49_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print49_val);
      }
    }
    label_test_abs: ;
    
    irdata->nb_refs_target = sav44_nb_refs_target;
    irdata->nb_tmps_target = sav44_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_afficher(T_irdata* irdata) {
    int sav50_nb_tmps_target = irdata->nb_tmps_target;
    int sav50_nb_refs_target = irdata->nb_refs_target;
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
      char print51_def;
      double print51_val;
      int print51;
      print_string(stdout, &(irdata->ctx_pr_out), "__AFFICHER__\012");
      {
        print51_def = 1;
        print51_val = 2.0;
      }
      if (print51_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print51_val);
      }
    }
    {
      char print52_def;
      double print52_val;
      int print52;
      print_string(stdout, &(irdata->ctx_pr_out), "afficher \042Bonjour, monde!\042 : ");
    }
    {
      char print53_def;
      double print53_val;
      int print53;
      print_string(stdout, &(irdata->ctx_pr_out), "Bonjour, monde!");
    }
    {
      char print54_def;
      double print54_val;
      int print54;
      print_string(stdout, &(irdata->ctx_pr_out), "\012afficher (0) : ");
    }
    {
      char print55_def;
      double print55_val;
      int print55;
      {
        print55_def = 1;
        print55_val = 0.0;
      }
      if (print55_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print55_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
    }
    {
      char print56_def;
      double print56_val;
      int print56;
      print_string(stdout, &(irdata->ctx_pr_out), "\012afficher (3.1415926535):2 : ");
    }
    {
      char print57_def;
      double print57_val;
      int print57;
      {
        print57_def = 1;
        print57_val = 3.141592653500000054;
      }
      if (print57_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print57_val, 2, 2);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
    }
    {
      char print58_def;
      double print58_val;
      int print58;
      print_string(stdout, &(irdata->ctx_pr_out), "\012afficher (3.1415926535):2..4 : ");
    }
    {
      char print59_def;
      double print59_val;
      int print59;
      {
        print59_def = 1;
        print59_val = 3.141592653500000054;
      }
      if (print59_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print59_val, 2, 4);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
    }
    {
      char print60_def;
      double print60_val;
      int print60;
      print_string(stdout, &(irdata->ctx_pr_out), "\012\012");
      {
        print60_def = 1;
        print60_val = -2.0;
      }
      if (print60_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print60_val);
      }
    }
    label_test_afficher: ;
    
    irdata->nb_refs_target = sav50_nb_refs_target;
    irdata->nb_tmps_target = sav50_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_arr(T_irdata* irdata) {
    int sav61_nb_tmps_target = irdata->nb_tmps_target;
    int sav61_nb_refs_target = irdata->nb_refs_target;
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
      char print62_def;
      double print62_val;
      int print62;
      print_string(stdout, &(irdata->ctx_pr_out), "__ARR__\012");
      {
        print62_def = 1;
        print62_val = 2.0;
      }
      if (print62_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print62_val);
      }
    }
    {
      char print63_def;
      double print63_val;
      int print63;
      print_string(stdout, &(irdata->ctx_pr_out), "arr(indefini) = ");
      {
        print63_def = 0;
        if (print63_def) {
          print63_val = my_arr(0.0);
        } else print63_val = 0.0;
      }
      if (print63_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print63_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print64_def;
      double print64_val;
      int print64;
      print_string(stdout, &(irdata->ctx_pr_out), "arr(1.8) = ");
      {
        print64_def = 1;
        print64_val = my_arr(1.800000000000000044);
      }
      if (print64_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print64_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print65_def;
      double print65_val;
      int print65;
      print_string(stdout, &(irdata->ctx_pr_out), "arr(-1.7) = ");
      {
        print65_def = 1;
        print65_val = my_arr(-1.699999999999999956);
      }
      if (print65_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print65_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print66_def;
      double print66_val;
      int print66;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print66_def = 1;
        print66_val = -2.0;
      }
      if (print66_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print66_val);
      }
    }
    label_test_arr: ;
    
    irdata->nb_refs_target = sav61_nb_refs_target;
    irdata->nb_tmps_target = sav61_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_attribut(T_irdata* irdata) {
    int sav67_nb_tmps_target = irdata->nb_tmps_target;
    int sav67_nb_refs_target = irdata->nb_refs_target;
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
      char print68_def;
      double print68_val;
      int print68;
      print_string(stdout, &(irdata->ctx_pr_out), "__ATTRIBUT__\012");
      {
        print68_def = 1;
        print68_val = 2.0;
      }
      if (print68_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print68_val);
      }
    }
    {
      char print69_def;
      double print69_val;
      int print69;
      print_string(stdout, &(irdata->ctx_pr_out), "attribut(X, primrest) = ");
      {
        print69_def = 1;
        print69_val = 0.0;
      }
      if (print69_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print69_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print70_def;
      double print70_val;
      int print70;
      print_string(stdout, &(irdata->ctx_pr_out), "attribut(TAB, primrest) = ");
      {
        print70_def = 1;
        print70_val = 2.0;
      }
      if (print70_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print70_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print71_def;
      double print71_val;
      int print71;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print71_def = 1;
        print71_val = -2.0;
      }
      if (print71_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print71_val);
      }
    }
    label_test_attribut: ;
    
    irdata->nb_refs_target = sav67_nb_refs_target;
    irdata->nb_tmps_target = sav67_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_champ_evenement(T_irdata* irdata) {
    int sav72_nb_tmps_target = irdata->nb_tmps_target;
    int sav72_nb_refs_target = irdata->nb_refs_target;
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
      char print73_def;
      double print73_val;
      int print73;
      print_string(stdout, &(irdata->ctx_pr_out), "__CHAMP_EVENEMENT__\012");
      {
        print73_def = 1;
        print73_val = 2.0;
      }
      if (print73_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print73_val);
      }
    }
    {
      T_event **events_sav74 = irdata->events;
      int nb_events_sav76 = irdata->nb_events;
      int nb_add77 = 0;
      T_event **events_tmp75 = NULL;
      int i78 = 0;
      int j79 = 0;
      {
        char cond81_def;
        double cond81_val;
        {
          cond81_def = 1;
          cond81_val = 1.0;
        }
        nb_add77 = (int)cond81_val;
        if (nb_add77 < 0) nb_add77 = 0;
        if (cond81_def && 0 < nb_add77) {
          int k82 = 0;
          events_tmp75 = (T_event **)malloc((nb_events_sav76 + nb_add77) * (sizeof (T_event *)));
          for (k82 = 0; k82 < nb_add77; k82++) {
            T_event *evt80 = (T_event *)malloc(sizeof (T_event));
            evt80->field_2042_rect_def = 0;
            evt80->field_2042_rect_val = 0.0;
            evt80->field_anc_penalite_def = 0;
            evt80->field_anc_penalite_val = 0.0;
            evt80->field_base_tl_def = 0;
            evt80->field_base_tl_val = 0.0;
            evt80->field_code_var = I_(calculee,0/*TAB*/);
            evt80->field_date_def = 0;
            evt80->field_date_val = 0.0;
            evt80->field_id_evt_def = 0;
            evt80->field_id_evt_val = 0.0;
            evt80->field_montant_def = 0;
            evt80->field_montant_val = 0.0;
            evt80->field_numero_def = 0;
            evt80->field_numero_val = 0.0;
            evt80->field_penalite_def = 0;
            evt80->field_penalite_val = 0.0;
            evt80->field_rappel_def = 0;
            evt80->field_rappel_val = 0.0;
            evt80->field_sens_def = 0;
            evt80->field_sens_val = 0.0;
            evt80->field_strate_def = 0;
            evt80->field_strate_val = 0.0;
            events_tmp75[k82] = evt80;
          }
        } else {
          nb_add77 = 0;
          events_tmp75 = (T_event **)malloc(nb_events_sav76 * (sizeof (T_event *)));
        }
        i78 = nb_add77;
      }
      while (i78 < nb_events_sav76) {
        events_tmp75[i78] = irdata->events[i78];
        i78++;
      }
      irdata->events = events_tmp75;
      irdata->nb_events = i78;
      {
        char print83_def;
        double print83_val;
        int print83;
        print_string(stdout, &(irdata->ctx_pr_out), "Avant d'initialiser les champs de l'\303\251v\303\251nement:\012");
      }
      {
        test_champ_evenement_base(irdata);
      }
      
      {
        char idx84_def;
        double idx84_val;
        int idx84;
        {
          idx84_def = 1;
          idx84_val = 0.0;
        }
        idx84 = (int)idx84_val;
        if (idx84_def && 0 <= idx84 && idx84 < irdata->nb_events) {
          irdata->events[idx84]->field_code_var = I_(calculee,11/*X*/);
        }
      }
      {
        char idx85_def;
        double idx85_val;
        int idx85;
        {
          idx85_def = 1;
          idx85_val = 0.0;
        }
        idx85 = (int)idx85_val;
        if (idx85_def && 0 <= idx85 && idx85 < irdata->nb_events) {
          char res86_def;
          double res86_val;
          {
            res86_def = 1;
            res86_val = 42.0;
          }
          irdata->events[idx85]->field_numero_def = res86_def;
          irdata->events[idx85]->field_numero_val = res86_val;
        }
      }
      {
        (def_calculee[20/*X*/]) = 1;
        (calculee[20/*X*/]) = 2.0;
      }
      {
        char print87_def;
        double print87_val;
        int print87;
        print_string(stdout, &(irdata->ctx_pr_out), "Apr\303\250s avoir initialis\303\251 les champs de l'\303\251v\303\251nement:\012");
      }
      {
        test_champ_evenement_base(irdata);
      }
      
      {
        (def_calculee[20/*X*/]) = 0;
        if ((def_calculee[20/*X*/])) {
          (calculee[20/*X*/]) = 0.0;
        } else (calculee[20/*X*/]) = 0.0;
      }
      free(irdata->events);
      irdata->events = events_sav74;
      irdata->nb_events = nb_events_sav76;
    }
    {
      char print88_def;
      double print88_val;
      int print88;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print88_def = 1;
        print88_val = -2.0;
      }
      if (print88_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print88_val);
      }
    }
    label_test_champ_evenement: ;
    
    irdata->nb_refs_target = sav72_nb_refs_target;
    irdata->nb_tmps_target = sav72_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_champ_evenement_base(T_irdata* irdata) {
    int sav89_nb_tmps_target = irdata->nb_tmps_target;
    int sav89_nb_refs_target = irdata->nb_refs_target;
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
      char print90_def;
      double print90_val;
      int print90;
      {
        print90_def = 1;
        print90_val = 2.0;
      }
      if (print90_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print90_val);
      }
    }
    {
      char print91_def;
      double print91_val;
      int print91;
      print_string(stdout, &(irdata->ctx_pr_out), "champ_evenement(0, numero) = ");
      {
        register int int0;
        register double real0;
        int space0;
        char res92_def;
        double res92_val;
        space0 = (irdata->var_space);
        res92_def =
          event_field_numero(irdata, space0, &res92_def, &res92_val, 1, 0.0);
        res92_val = res92_val;
        print91_def = res92_def;
        if (print91_def) {
          int0 = (NEQ_E((res92_def),(0.0)));
          real0 = res92_val;
          print91_val = (int0 ? real0 : 0.0);
        } else print91_val = 0.0;
      }
      if (print91_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print91_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print93_def;
      double print93_val;
      int print93;
      print_string(stdout, &(irdata->ctx_pr_out), "champ_evenement(0, code) = ");
      {
        register int int0;
        register double real0;
        int space0;
        char res94_def;
        double res94_val;
        space0 = (irdata->var_space);
        res94_def =
          event_field_code(irdata, space0, &res94_def, &res94_val, 1, 0.0);
        res94_val = res94_val;
        print93_def = res94_def;
        if (print93_def) {
          int0 = (NEQ_E((res94_def),(0.0)));
          real0 = res94_val;
          print93_val = (int0 ? real0 : 0.0);
        } else print93_val = 0.0;
      }
      if (print93_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print93_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print95_def;
      double print95_val;
      int print95;
      {
        print95_def = 1;
        print95_val = -2.0;
      }
      if (print95_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print95_val);
      }
    }
    label_test_champ_evenement_base: ;
    
    irdata->nb_refs_target = sav89_nb_refs_target;
    irdata->nb_tmps_target = sav89_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_inf(T_irdata* irdata) {
    int sav96_nb_tmps_target = irdata->nb_tmps_target;
    int sav96_nb_refs_target = irdata->nb_refs_target;
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
      char print97_def;
      double print97_val;
      int print97;
      print_string(stdout, &(irdata->ctx_pr_out), "__INF__\012");
      {
        print97_def = 1;
        print97_val = 2.0;
      }
      if (print97_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print97_val);
      }
    }
    {
      char print98_def;
      double print98_val;
      int print98;
      print_string(stdout, &(irdata->ctx_pr_out), "inf(indefini) = ");
      {
        print98_def = 0;
        if (print98_def) {
          print98_val = my_floor(0.0);
        } else print98_val = 0.0;
      }
      if (print98_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print98_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print99_def;
      double print99_val;
      int print99;
      print_string(stdout, &(irdata->ctx_pr_out), "inf(1.8) = ");
      {
        print99_def = 1;
        print99_val = my_floor(1.800000000000000044);
      }
      if (print99_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print99_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print100_def;
      double print100_val;
      int print100;
      print_string(stdout, &(irdata->ctx_pr_out), "inf(-1.7) = ");
      {
        print100_def = 1;
        print100_val = my_floor(-1.699999999999999956);
      }
      if (print100_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print100_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print101_def;
      double print101_val;
      int print101;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print101_def = 1;
        print101_val = -2.0;
      }
      if (print101_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print101_val);
      }
    }
    label_test_inf: ;
    
    irdata->nb_refs_target = sav96_nb_refs_target;
    irdata->nb_tmps_target = sav96_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_max(T_irdata* irdata) {
    int sav102_nb_tmps_target = irdata->nb_tmps_target;
    int sav102_nb_refs_target = irdata->nb_refs_target;
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
      char print103_def;
      double print103_val;
      int print103;
      print_string(stdout, &(irdata->ctx_pr_out), "__MAX__\012");
      {
        print103_def = 1;
        print103_val = 2.0;
      }
      if (print103_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print103_val);
      }
    }
    {
      char print104_def;
      double print104_val;
      int print104;
      print_string(stdout, &(irdata->ctx_pr_out), "max(indefini, indefini) = ");
      {
        print104_def = 0;
        if (print104_def) {
          print104_val = max(0.0, 0.0);
        } else print104_val = 0.0;
      }
      if (print104_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print104_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print105_def;
      double print105_val;
      int print105;
      print_string(stdout, &(irdata->ctx_pr_out), "max(-1, indefini) = ");
      {
        print105_def = 1;
        print105_val = max(-1.0, 0.0);
      }
      if (print105_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print105_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print106_def;
      double print106_val;
      int print106;
      print_string(stdout, &(irdata->ctx_pr_out), "max(indefini, -1) = ");
      {
        print106_def = 1;
        print106_val = max(0.0, -1.0);
      }
      if (print106_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print106_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print107_def;
      double print107_val;
      int print107;
      print_string(stdout, &(irdata->ctx_pr_out), "max(1, indefini) = ");
      {
        print107_def = 1;
        print107_val = max(1.0, 0.0);
      }
      if (print107_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print107_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print108_def;
      double print108_val;
      int print108;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print108_def = 1;
        print108_val = -2.0;
      }
      if (print108_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print108_val);
      }
    }
    label_test_max: ;
    
    irdata->nb_refs_target = sav102_nb_refs_target;
    irdata->nb_tmps_target = sav102_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_meme_variable(T_irdata* irdata) {
    int sav109_nb_tmps_target = irdata->nb_tmps_target;
    int sav109_nb_refs_target = irdata->nb_refs_target;
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
      char print110_def;
      double print110_val;
      int print110;
      print_string(stdout, &(irdata->ctx_pr_out), "__MEME_VARIABLE__\012");
      {
        print110_def = 1;
        print110_val = 2.0;
      }
      if (print110_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print110_val);
      }
    }
    {
      char print111_def;
      double print111_val;
      int print111;
      print_string(stdout, &(irdata->ctx_pr_out), "meme_variable(X,X) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        T_varinfo* varinfo1;
        char res112_def;
        double res112_val;
        varinfo0 = I_(calculee,11/*X*/);
        varinfo1 = I_(calculee,11/*X*/);
        res112_def =
          meme_variable(varinfo0, varinfo1, &res112_def, &res112_val);
        res112_val = res112_val;
        print111_def = res112_def;
        if (print111_def) {
          int0 = (NEQ_E((res112_def),(0.0)));
          real0 = res112_val;
          print111_val = (int0 ? real0 : 0.0);
        } else print111_val = 0.0;
      }
      if (print111_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print111_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print113_def;
      double print113_val;
      int print113;
      print_string(stdout, &(irdata->ctx_pr_out), "meme_variable(X,TAB) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        T_varinfo* varinfo1;
        char res114_def;
        double res114_val;
        varinfo0 = I_(calculee,11/*X*/);
        varinfo1 = I_(calculee,0/*TAB*/);
        res114_def =
          meme_variable(varinfo0, varinfo1, &res114_def, &res114_val);
        res114_val = res114_val;
        print113_def = res114_def;
        if (print113_def) {
          int0 = (NEQ_E((res114_def),(0.0)));
          real0 = res114_val;
          print113_val = (int0 ? real0 : 0.0);
        } else print113_val = 0.0;
      }
      if (print113_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print113_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print115_def;
      double print115_val;
      int print115;
      print_string(stdout, &(irdata->ctx_pr_out), "meme_variable(TAB[0],TAB) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        T_varinfo* varinfo1;
        char res116_def;
        double res116_val;
        varinfo0 = lis_tabaccess_varinfo(irdata, 0, 1, 0.0);
        varinfo1 = I_(calculee,0/*TAB*/);
        res116_def =
          meme_variable(varinfo0, varinfo1, &res116_def, &res116_val);
        res116_val = res116_val;
        print115_def = res116_def;
        if (print115_def) {
          int0 = (NEQ_E((res116_def),(0.0)));
          real0 = res116_val;
          print115_val = (int0 ? real0 : 0.0);
        } else print115_val = 0.0;
      }
      if (print115_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print115_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print117_def;
      double print117_val;
      int print117;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print117_def = 1;
        print117_val = -2.0;
      }
      if (print117_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print117_val);
      }
    }
    label_test_meme_variable: ;
    
    irdata->nb_refs_target = sav109_nb_refs_target;
    irdata->nb_tmps_target = sav109_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_min(T_irdata* irdata) {
    int sav118_nb_tmps_target = irdata->nb_tmps_target;
    int sav118_nb_refs_target = irdata->nb_refs_target;
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
      char print119_def;
      double print119_val;
      int print119;
      print_string(stdout, &(irdata->ctx_pr_out), "__MIN__");
      {
        print119_def = 1;
        print119_val = 2.0;
      }
      if (print119_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print119_val);
      }
    }
    {
      char print120_def;
      double print120_val;
      int print120;
      print_string(stdout, &(irdata->ctx_pr_out), "min(indefini, indefini) = ");
      {
        print120_def = 0;
        if (print120_def) {
          print120_val = min(0.0, 0.0);
        } else print120_val = 0.0;
      }
      if (print120_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print120_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print121_def;
      double print121_val;
      int print121;
      print_string(stdout, &(irdata->ctx_pr_out), "min(1, indefini) = ");
      {
        print121_def = 1;
        print121_val = min(1.0, 0.0);
      }
      if (print121_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print121_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print122_def;
      double print122_val;
      int print122;
      print_string(stdout, &(irdata->ctx_pr_out), "min(indefini, 1) = ");
      {
        print122_def = 1;
        print122_val = min(0.0, 1.0);
      }
      if (print122_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print122_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print123_def;
      double print123_val;
      int print123;
      print_string(stdout, &(irdata->ctx_pr_out), "min(-1, indefini) = ");
      {
        print123_def = 1;
        print123_val = min(-1.0, 0.0);
      }
      if (print123_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print123_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print124_def;
      double print124_val;
      int print124;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print124_def = 1;
        print124_val = -2.0;
      }
      if (print124_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print124_val);
      }
    }
    label_test_min: ;
    
    irdata->nb_refs_target = sav118_nb_refs_target;
    irdata->nb_tmps_target = sav118_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_multimax(T_irdata* irdata) {
    int sav125_nb_tmps_target = irdata->nb_tmps_target;
    int sav125_nb_refs_target = irdata->nb_refs_target;
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
      char print126_def;
      double print126_val;
      int print126;
      print_string(stdout, &(irdata->ctx_pr_out), "__MULTIMAX__\012");
      {
        print126_def = 1;
        print126_val = 2.0;
      }
      if (print126_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print126_val);
      }
    }
    {
      char print127_def;
      double print127_val;
      int print127;
      print_string(stdout, &(irdata->ctx_pr_out), "Avant initialisation du tableau :\012");
    }
    {
      test_multimax_base(irdata);
    }
    
    {
      initalise_tab(irdata);
    }
    
    {
      char print128_def;
      double print128_val;
      int print128;
      print_string(stdout, &(irdata->ctx_pr_out), "Apr\303\250s initialisation du tableau :\012");
    }
    {
      test_multimax_base(irdata);
    }
    
    {
      reinitialise_tab(irdata);
    }
    
    {
      char print129_def;
      double print129_val;
      int print129;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print129_def = 1;
        print129_val = -2.0;
      }
      if (print129_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print129_val);
      }
    }
    label_test_multimax: ;
    
    irdata->nb_refs_target = sav125_nb_refs_target;
    irdata->nb_tmps_target = sav125_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_multimax_base(T_irdata* irdata) {
    int sav130_nb_tmps_target = irdata->nb_tmps_target;
    int sav130_nb_refs_target = irdata->nb_refs_target;
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
      char print131_def;
      double print131_val;
      int print131;
      {
        print131_def = 1;
        print131_val = 2.0;
      }
      if (print131_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print131_val);
      }
    }
    {
      char print132_def;
      double print132_val;
      int print132;
      print_string(stdout, &(irdata->ctx_pr_out), "multimax(indefini, TAB) = ");
      {
        register int int0;
        register double real0;
        char res133_def;
        double res133_val;
        res133_def =
          multimax_varinfo(irdata, (irdata->var_space), I_(calculee,0/*TAB*/),
            0, 0.0, &res133_def, &res133_val);
        res133_val = res133_val;
        print132_def = res133_def;
        if (print132_def) {
          int0 = (NEQ_E((res133_def),(0.0)));
          real0 = res133_val;
          print132_val = (int0 ? real0 : 0.0);
        } else print132_val = 0.0;
      }
      if (print132_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print132_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print134_def;
      double print134_val;
      int print134;
      print_string(stdout, &(irdata->ctx_pr_out), "multimax(7, TAB) = ");
      {
        register int int0;
        register double real0;
        char res135_def;
        double res135_val;
        res135_def =
          multimax_varinfo(irdata, (irdata->var_space), I_(calculee,0/*TAB*/),
            1, 7.0, &res135_def, &res135_val);
        res135_val = res135_val;
        print134_def = res135_def;
        if (print134_def) {
          int0 = (NEQ_E((res135_def),(0.0)));
          real0 = res135_val;
          print134_val = (int0 ? real0 : 0.0);
        } else print134_val = 0.0;
      }
      if (print134_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print134_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print136_def;
      double print136_val;
      int print136;
      print_string(stdout, &(irdata->ctx_pr_out), "multimax(taille(TAB) + 1, TAB) = ");
      {
        register int int0;
        register double real0;
        char res137_def;
        double res137_val;
        res137_def =
          multimax_varinfo(irdata, (irdata->var_space), I_(calculee,0/*TAB*/),
            1, 11.0, &res137_def, &res137_val);
        res137_val = res137_val;
        print136_def = res137_def;
        if (print136_def) {
          int0 = (NEQ_E((res137_def),(0.0)));
          real0 = res137_val;
          print136_val = (int0 ? real0 : 0.0);
        } else print136_val = 0.0;
      }
      if (print136_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print136_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print138_def;
      double print138_val;
      int print138;
      print_string(stdout, &(irdata->ctx_pr_out), "multimax(0, TAB) = ");
      {
        register int int0;
        register double real0;
        char res139_def;
        double res139_val;
        res139_def =
          multimax_varinfo(irdata, (irdata->var_space), I_(calculee,0/*TAB*/),
            1, 0.0, &res139_def, &res139_val);
        res139_val = res139_val;
        print138_def = res139_def;
        if (print138_def) {
          int0 = (NEQ_E((res139_def),(0.0)));
          real0 = res139_val;
          print138_val = (int0 ? real0 : 0.0);
        } else print138_val = 0.0;
      }
      if (print138_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print138_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print140_def;
      double print140_val;
      int print140;
      print_string(stdout, &(irdata->ctx_pr_out), "multimax(-1, TAB) = ");
      {
        register int int0;
        register double real0;
        char res141_def;
        double res141_val;
        res141_def =
          multimax_varinfo(irdata, (irdata->var_space), I_(calculee,0/*TAB*/),
            1, -1.0, &res141_def, &res141_val);
        res141_val = res141_val;
        print140_def = res141_def;
        if (print140_def) {
          int0 = (NEQ_E((res141_def),(0.0)));
          real0 = res141_val;
          print140_val = (int0 ? real0 : 0.0);
        } else print140_val = 0.0;
      }
      if (print140_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print140_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print142_def;
      double print142_val;
      int print142;
      {
        print142_def = 1;
        print142_val = -2.0;
      }
      if (print142_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print142_val);
      }
    }
    label_test_multimax_base: ;
    
    irdata->nb_refs_target = sav130_nb_refs_target;
    irdata->nb_tmps_target = sav130_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_nb_evenements(T_irdata* irdata) {
    int sav143_nb_tmps_target = irdata->nb_tmps_target;
    int sav143_nb_refs_target = irdata->nb_refs_target;
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
      char print144_def;
      double print144_val;
      int print144;
      print_string(stdout, &(irdata->ctx_pr_out), "__NB_EVENEMENTS__\012");
      {
        print144_def = 1;
        print144_val = 2.0;
      }
      if (print144_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print144_val);
      }
    }
    {
      char print145_def;
      double print145_val;
      int print145;
      print_string(stdout, &(irdata->ctx_pr_out), "Avant la d\303\251finition d'un \303\251v\303\251nement :\012");
    }
    {
      test_nb_evenements_base(irdata);
    }
    
    {
      T_event **events_sav146 = irdata->events;
      int nb_events_sav148 = irdata->nb_events;
      int nb_add149 = 0;
      T_event **events_tmp147 = NULL;
      int i150 = 0;
      int j151 = 0;
      {
        char cond153_def;
        double cond153_val;
        {
          cond153_def = 1;
          cond153_val = 1.0;
        }
        nb_add149 = (int)cond153_val;
        if (nb_add149 < 0) nb_add149 = 0;
        if (cond153_def && 0 < nb_add149) {
          int k154 = 0;
          events_tmp147 = (T_event **)malloc((nb_events_sav148 + nb_add149) * (sizeof (T_event *)));
          for (k154 = 0; k154 < nb_add149; k154++) {
            T_event *evt152 = (T_event *)malloc(sizeof (T_event));
            evt152->field_2042_rect_def = 0;
            evt152->field_2042_rect_val = 0.0;
            evt152->field_anc_penalite_def = 0;
            evt152->field_anc_penalite_val = 0.0;
            evt152->field_base_tl_def = 0;
            evt152->field_base_tl_val = 0.0;
            evt152->field_code_var = I_(calculee,0/*TAB*/);
            evt152->field_date_def = 0;
            evt152->field_date_val = 0.0;
            evt152->field_id_evt_def = 0;
            evt152->field_id_evt_val = 0.0;
            evt152->field_montant_def = 0;
            evt152->field_montant_val = 0.0;
            evt152->field_numero_def = 0;
            evt152->field_numero_val = 0.0;
            evt152->field_penalite_def = 0;
            evt152->field_penalite_val = 0.0;
            evt152->field_rappel_def = 0;
            evt152->field_rappel_val = 0.0;
            evt152->field_sens_def = 0;
            evt152->field_sens_val = 0.0;
            evt152->field_strate_def = 0;
            evt152->field_strate_val = 0.0;
            events_tmp147[k154] = evt152;
          }
        } else {
          nb_add149 = 0;
          events_tmp147 = (T_event **)malloc(nb_events_sav148 * (sizeof (T_event *)));
        }
        i150 = nb_add149;
      }
      while (i150 < nb_events_sav148) {
        events_tmp147[i150] = irdata->events[i150];
        i150++;
      }
      irdata->events = events_tmp147;
      irdata->nb_events = i150;
      {
        char print155_def;
        double print155_val;
        int print155;
        print_string(stdout, &(irdata->ctx_pr_out), "Pendant la d\303\251finition d'un \303\251v\303\251nement :\012");
      }
      {
        test_nb_evenements_base(irdata);
      }
      
      free(irdata->events);
      irdata->events = events_sav146;
      irdata->nb_events = nb_events_sav148;
    }
    {
      char print156_def;
      double print156_val;
      int print156;
      print_string(stdout, &(irdata->ctx_pr_out), "Apr\303\250s la d\303\251finition d'un \303\251v\303\251nement :\012");
    }
    {
      test_nb_evenements_base(irdata);
    }
    
    {
      char print157_def;
      double print157_val;
      int print157;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print157_def = 1;
        print157_val = -2.0;
      }
      if (print157_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print157_val);
      }
    }
    label_test_nb_evenements: ;
    
    irdata->nb_refs_target = sav143_nb_refs_target;
    irdata->nb_tmps_target = sav143_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_nb_evenements_base(T_irdata* irdata) {
    int sav158_nb_tmps_target = irdata->nb_tmps_target;
    int sav158_nb_refs_target = irdata->nb_refs_target;
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
      char print159_def;
      double print159_val;
      int print159;
      {
        print159_def = 1;
        print159_val = 2.0;
      }
      if (print159_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print159_val);
      }
    }
    {
      char print160_def;
      double print160_val;
      int print160;
      print_string(stdout, &(irdata->ctx_pr_out), "nb_evenements() = ");
      {
        register double real0;
        print160_def = 1;
        real0 = nb_evenements(irdata);
        print160_val = real0;
      }
      if (print160_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print160_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print161_def;
      double print161_val;
      int print161;
      {
        print161_def = 1;
        print161_val = -2.0;
      }
      if (print161_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print161_val);
      }
    }
    label_test_nb_evenements_base: ;
    
    irdata->nb_refs_target = sav158_nb_refs_target;
    irdata->nb_tmps_target = sav158_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_null(T_irdata* irdata) {
    int sav162_nb_tmps_target = irdata->nb_tmps_target;
    int sav162_nb_refs_target = irdata->nb_refs_target;
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
      char print163_def;
      double print163_val;
      int print163;
      print_string(stdout, &(irdata->ctx_pr_out), "__NULL__\012");
      {
        print163_def = 1;
        print163_val = 2.0;
      }
      if (print163_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print163_val);
      }
    }
    {
      char print164_def;
      double print164_val;
      int print164;
      print_string(stdout, &(irdata->ctx_pr_out), "null(indefini) = ");
      {
        print164_def = 0;
        if (print164_def) {
          print164_val = 0;
        } else print164_val = 0.0;
      }
      if (print164_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print164_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print165_def;
      double print165_val;
      int print165;
      print_string(stdout, &(irdata->ctx_pr_out), "null(0) = ");
      {
        print165_def = 1;
        print165_val = 1;
      }
      if (print165_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print165_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print166_def;
      double print166_val;
      int print166;
      print_string(stdout, &(irdata->ctx_pr_out), "null(1) = ");
      {
        print166_def = 1;
        print166_val = 0;
      }
      if (print166_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print166_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print167_def;
      double print167_val;
      int print167;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print167_def = 1;
        print167_val = -2.0;
      }
      if (print167_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print167_val);
      }
    }
    label_test_null: ;
    
    irdata->nb_refs_target = sav162_nb_refs_target;
    irdata->nb_tmps_target = sav162_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_positif(T_irdata* irdata) {
    int sav168_nb_tmps_target = irdata->nb_tmps_target;
    int sav168_nb_refs_target = irdata->nb_refs_target;
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
      char print169_def;
      double print169_val;
      int print169;
      print_string(stdout, &(irdata->ctx_pr_out), "__POSITIF__\012");
      {
        print169_def = 1;
        print169_val = 2.0;
      }
      if (print169_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print169_val);
      }
    }
    {
      char print170_def;
      double print170_val;
      int print170;
      print_string(stdout, &(irdata->ctx_pr_out), "positif(indefini) = ");
      {
        print170_def = 0;
        if (print170_def) {
          print170_val = 0;
        } else print170_val = 0.0;
      }
      if (print170_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print170_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print171_def;
      double print171_val;
      int print171;
      print_string(stdout, &(irdata->ctx_pr_out), "positif(0) = ");
      {
        print171_def = 1;
        print171_val = 0;
      }
      if (print171_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print171_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print172_def;
      double print172_val;
      int print172;
      print_string(stdout, &(irdata->ctx_pr_out), "positif(1) = ");
      {
        print172_def = 1;
        print172_val = 1;
      }
      if (print172_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print172_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print173_def;
      double print173_val;
      int print173;
      print_string(stdout, &(irdata->ctx_pr_out), "positif(-1) = ");
      {
        print173_def = 1;
        print173_val = 0;
      }
      if (print173_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print173_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print174_def;
      double print174_val;
      int print174;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print175_def;
      double print175_val;
      int print175;
      {
        print175_def = 1;
        print175_val = -2.0;
      }
      if (print175_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print175_val);
      }
    }
    label_test_positif: ;
    
    irdata->nb_refs_target = sav168_nb_refs_target;
    irdata->nb_tmps_target = sav168_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_positif_ou_nul(T_irdata* irdata) {
    int sav176_nb_tmps_target = irdata->nb_tmps_target;
    int sav176_nb_refs_target = irdata->nb_refs_target;
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
      char print177_def;
      double print177_val;
      int print177;
      print_string(stdout, &(irdata->ctx_pr_out), "__POSITIF OU NUL__\012");
      {
        print177_def = 1;
        print177_val = 2.0;
      }
      if (print177_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print177_val);
      }
    }
    {
      char print178_def;
      double print178_val;
      int print178;
      print_string(stdout, &(irdata->ctx_pr_out), "positif_ou_nul(indefini) = ");
      {
        print178_def = 0;
        if (print178_def) {
          print178_val = 0;
        } else print178_val = 0.0;
      }
      if (print178_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print178_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print179_def;
      double print179_val;
      int print179;
      print_string(stdout, &(irdata->ctx_pr_out), "positif_ou_nul(0) = ");
      {
        print179_def = 1;
        print179_val = 1;
      }
      if (print179_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print179_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print180_def;
      double print180_val;
      int print180;
      print_string(stdout, &(irdata->ctx_pr_out), "positif_ou_nul(1) = ");
      {
        print180_def = 1;
        print180_val = 1;
      }
      if (print180_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print180_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print181_def;
      double print181_val;
      int print181;
      print_string(stdout, &(irdata->ctx_pr_out), "positif_ou_nul(-1) = ");
      {
        print181_def = 1;
        print181_val = 0;
      }
      if (print181_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print181_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print182_def;
      double print182_val;
      int print182;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print182_def = 1;
        print182_val = -2.0;
      }
      if (print182_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print182_val);
      }
    }
    label_test_positif_ou_nul: ;
    
    irdata->nb_refs_target = sav176_nb_refs_target;
    irdata->nb_tmps_target = sav176_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_present(T_irdata* irdata) {
    int sav183_nb_tmps_target = irdata->nb_tmps_target;
    int sav183_nb_refs_target = irdata->nb_refs_target;
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
      char print184_def;
      double print184_val;
      int print184;
      print_string(stdout, &(irdata->ctx_pr_out), "__PRESENT__\012");
      {
        print184_def = 1;
        print184_val = 2.0;
      }
      if (print184_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print184_val);
      }
    }
    {
      char print185_def;
      double print185_val;
      int print185;
      print_string(stdout, &(irdata->ctx_pr_out), "present(indefini) = ");
      {
        print185_def = 1;
        print185_val = 0;
      }
      if (print185_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print185_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print186_def;
      double print186_val;
      int print186;
      print_string(stdout, &(irdata->ctx_pr_out), "present(0) = ");
      {
        print186_def = 1;
        print186_val = 1;
      }
      if (print186_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print186_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print187_def;
      double print187_val;
      int print187;
      print_string(stdout, &(irdata->ctx_pr_out), "present(1) = ");
      {
        print187_def = 1;
        print187_val = 1;
      }
      if (print187_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print187_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print188_def;
      double print188_val;
      int print188;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print188_def = 1;
        print188_val = -2.0;
      }
      if (print188_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print188_val);
      }
    }
    label_test_present: ;
    
    irdata->nb_refs_target = sav183_nb_refs_target;
    irdata->nb_tmps_target = sav183_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_somme(T_irdata* irdata) {
    int sav189_nb_tmps_target = irdata->nb_tmps_target;
    int sav189_nb_refs_target = irdata->nb_refs_target;
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
      char print190_def;
      double print190_val;
      int print190;
      print_string(stdout, &(irdata->ctx_pr_out), "__SOMME__\012");
      {
        print190_def = 1;
        print190_val = 2.0;
      }
      if (print190_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print190_val);
      }
    }
    {
      char print191_def;
      double print191_val;
      int print191;
      print_string(stdout, &(irdata->ctx_pr_out), "somme() = ");
      {
        print191_def = 1;
        print191_val = 0.0;
      }
      if (print191_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print191_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print192_def;
      double print192_val;
      int print192;
      print_string(stdout, &(irdata->ctx_pr_out), "somme(indefini) = ");
      {
        print192_def = 0;
        if (print192_def) {
          print192_val = 0.0;
        } else print192_val = 0.0;
      }
      if (print192_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print192_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print193_def;
      double print193_val;
      int print193;
      print_string(stdout, &(irdata->ctx_pr_out), "somme(1, indefini) = ");
      {
        print193_def = 1;
        print193_val = 1.0;
      }
      if (print193_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print193_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print194_def;
      double print194_val;
      int print194;
      print_string(stdout, &(irdata->ctx_pr_out), "somme(1, 2, 3, 4, 5) = ");
      {
        print194_def = 1;
        print194_val = 15.0;
      }
      if (print194_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print194_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print195_def;
      double print195_val;
      int print195;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print195_def = 1;
        print195_val = -2.0;
      }
      if (print195_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print195_val);
      }
    }
    label_test_somme: ;
    
    irdata->nb_refs_target = sav189_nb_refs_target;
    irdata->nb_tmps_target = sav189_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_supzero(T_irdata* irdata) {
    int sav196_nb_tmps_target = irdata->nb_tmps_target;
    int sav196_nb_refs_target = irdata->nb_refs_target;
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
      char print197_def;
      double print197_val;
      int print197;
      print_string(stdout, &(irdata->ctx_pr_out), "__SUPZERO__\012");
      {
        print197_def = 1;
        print197_val = 2.0;
      }
      if (print197_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print197_val);
      }
    }
    {
      char print198_def;
      double print198_val;
      int print198;
      print_string(stdout, &(irdata->ctx_pr_out), "supzero(indefini) = ");
      {
        print198_def = 0;
        if (print198_def) {
          print198_val = 0.0;
        } else print198_val = 0.0;
      }
      if (print198_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print198_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print199_def;
      double print199_val;
      int print199;
      print_string(stdout, &(irdata->ctx_pr_out), "supzero(42) = ");
      {
        print199_def = 1;
        print199_val = 42.0;
      }
      if (print199_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print199_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print200_def;
      double print200_val;
      int print200;
      print_string(stdout, &(irdata->ctx_pr_out), "supzero(-1) = ");
      {
        print200_def = 0;
        if (print200_def) {
          print200_val = 0.0;
        } else print200_val = 0.0;
      }
      if (print200_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print200_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print201_def;
      double print201_val;
      int print201;
      print_string(stdout, &(irdata->ctx_pr_out), "supzero(0) = ");
      {
        print201_def = 0;
        if (print201_def) {
          print201_val = 0.0;
        } else print201_val = 0.0;
      }
      if (print201_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print201_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print202_def;
      double print202_val;
      int print202;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print202_def = 1;
        print202_val = -2.0;
      }
      if (print202_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print202_val);
      }
    }
    label_test_supzero: ;
    
    irdata->nb_refs_target = sav196_nb_refs_target;
    irdata->nb_tmps_target = sav196_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_taille(T_irdata* irdata) {
    int sav203_nb_tmps_target = irdata->nb_tmps_target;
    int sav203_nb_refs_target = irdata->nb_refs_target;
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
      char print204_def;
      double print204_val;
      int print204;
      print_string(stdout, &(irdata->ctx_pr_out), "__TAILLE__\012");
      {
        print204_def = 1;
        print204_val = 2.0;
      }
      if (print204_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print204_val);
      }
    }
    {
      char print205_def;
      double print205_val;
      int print205;
      print_string(stdout, &(irdata->ctx_pr_out), "taille(TAB) = ");
      {
        print205_def = 1;
        print205_val = 10.0;
      }
      if (print205_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print205_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print206_def;
      double print206_val;
      int print206;
      print_string(stdout, &(irdata->ctx_pr_out), "taille(X) = ");
      {
        print206_def = 1;
        print206_val = 1.0;
      }
      if (print206_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print206_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print207_def;
      double print207_val;
      int print207;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print207_def = 1;
        print207_val = -2.0;
      }
      if (print207_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print207_val);
      }
    }
    label_test_taille: ;
    
    irdata->nb_refs_target = sav203_nb_refs_target;
    irdata->nb_tmps_target = sav203_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * test_type(T_irdata* irdata) {
    int sav208_nb_tmps_target = irdata->nb_tmps_target;
    int sav208_nb_refs_target = irdata->nb_refs_target;
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
      char print209_def;
      double print209_val;
      int print209;
      print_string(stdout, &(irdata->ctx_pr_out), "__TYPE__\012");
      {
        print209_def = 1;
        print209_val = 2.0;
      }
      if (print209_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print209_val);
      }
    }
    {
      char print210_def;
      double print210_val;
      int print210;
      print_string(stdout, &(irdata->ctx_pr_out), "type(X, REEL) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        char res211_def;
        double res211_val;
        varinfo0 = I_(calculee,11/*X*/);
        res211_def = est_type(varinfo0, TYPE_REEL, &res211_def, &res211_val);
        res211_val = res211_val;
        print210_def = res211_def;
        if (print210_def) {
          int0 = (NEQ_E((res211_def),(0.0)));
          real0 = res211_val;
          print210_val = (int0 ? real0 : 0.0);
        } else print210_val = 0.0;
      }
      if (print210_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print210_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print212_def;
      double print212_val;
      int print212;
      print_string(stdout, &(irdata->ctx_pr_out), "type(X, ENTIER) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        char res213_def;
        double res213_val;
        varinfo0 = I_(calculee,11/*X*/);
        res213_def = est_type(varinfo0, TYPE_ENTIER, &res213_def, &res213_val);
        res213_val = res213_val;
        print212_def = res213_def;
        if (print212_def) {
          int0 = (NEQ_E((res213_def),(0.0)));
          real0 = res213_val;
          print212_val = (int0 ? real0 : 0.0);
        } else print212_val = 0.0;
      }
      if (print212_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print212_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print214_def;
      double print214_val;
      int print214;
      print_string(stdout, &(irdata->ctx_pr_out), "type(TAB, ENTIER) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        char res215_def;
        double res215_val;
        varinfo0 = I_(calculee,0/*TAB*/);
        res215_def = est_type(varinfo0, TYPE_ENTIER, &res215_def, &res215_val);
        res215_val = res215_val;
        print214_def = res215_def;
        if (print214_def) {
          int0 = (NEQ_E((res215_def),(0.0)));
          real0 = res215_val;
          print214_val = (int0 ? real0 : 0.0);
        } else print214_val = 0.0;
      }
      if (print214_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print214_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print216_def;
      double print216_val;
      int print216;
      print_string(stdout, &(irdata->ctx_pr_out), "type(Y, ENTIER) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        char res217_def;
        double res217_val;
        varinfo0 = I_(calculee,12/*Y*/);
        res217_def = est_type(varinfo0, TYPE_ENTIER, &res217_def, &res217_val);
        res217_val = res217_val;
        print216_def = res217_def;
        if (print216_def) {
          int0 = (NEQ_E((res217_def),(0.0)));
          real0 = res217_val;
          print216_val = (int0 ? real0 : 0.0);
        } else print216_val = 0.0;
      }
      if (print216_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print216_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print218_def;
      double print218_val;
      int print218;
      print_string(stdout, &(irdata->ctx_pr_out), "type(Y, REEL) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        char res219_def;
        double res219_val;
        varinfo0 = I_(calculee,12/*Y*/);
        res219_def = est_type(varinfo0, TYPE_REEL, &res219_def, &res219_val);
        res219_val = res219_val;
        print218_def = res219_def;
        if (print218_def) {
          int0 = (NEQ_E((res219_def),(0.0)));
          real0 = res219_val;
          print218_val = (int0 ? real0 : 0.0);
        } else print218_val = 0.0;
      }
      if (print218_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print218_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print220_def;
      double print220_val;
      int print220;
      print_string(stdout, &(irdata->ctx_pr_out), "type(Y, BOOLEEN) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        char res221_def;
        double res221_val;
        varinfo0 = I_(calculee,12/*Y*/);
        res221_def =
          est_type(varinfo0, TYPE_BOOLEEN, &res221_def, &res221_val);
        res221_val = res221_val;
        print220_def = res221_def;
        if (print220_def) {
          int0 = (NEQ_E((res221_def),(0.0)));
          real0 = res221_val;
          print220_val = (int0 ? real0 : 0.0);
        } else print220_val = 0.0;
      }
      if (print220_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print220_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print222_def;
      double print222_val;
      int print222;
      print_string(stdout, &(irdata->ctx_pr_out), "type(Y, DATE_AAAA) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        char res223_def;
        double res223_val;
        varinfo0 = I_(calculee,12/*Y*/);
        res223_def =
          est_type(varinfo0, TYPE_DATE_AAAA, &res223_def, &res223_val);
        res223_val = res223_val;
        print222_def = res223_def;
        if (print222_def) {
          int0 = (NEQ_E((res223_def),(0.0)));
          real0 = res223_val;
          print222_val = (int0 ? real0 : 0.0);
        } else print222_val = 0.0;
      }
      if (print222_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print222_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print224_def;
      double print224_val;
      int print224;
      print_string(stdout, &(irdata->ctx_pr_out), "type(Y, DATE_JJMMAAAA) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        char res225_def;
        double res225_val;
        varinfo0 = I_(calculee,12/*Y*/);
        res225_def =
          est_type(varinfo0, TYPE_DATE_JJMMAAAA, &res225_def, &res225_val);
        res225_val = res225_val;
        print224_def = res225_def;
        if (print224_def) {
          int0 = (NEQ_E((res225_def),(0.0)));
          real0 = res225_val;
          print224_val = (int0 ? real0 : 0.0);
        } else print224_val = 0.0;
      }
      if (print224_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print224_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print226_def;
      double print226_val;
      int print226;
      print_string(stdout, &(irdata->ctx_pr_out), "type(Y, DATE_MM) = ");
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        char res227_def;
        double res227_val;
        varinfo0 = I_(calculee,12/*Y*/);
        res227_def =
          est_type(varinfo0, TYPE_DATE_MM, &res227_def, &res227_val);
        res227_val = res227_val;
        print226_def = res227_def;
        if (print226_def) {
          int0 = (NEQ_E((res227_def),(0.0)));
          real0 = res227_val;
          print226_val = (int0 ? real0 : 0.0);
        } else print226_val = 0.0;
      }
      if (print226_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print226_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      char print228_def;
      double print228_val;
      int print228;
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
      {
        print228_def = 1;
        print228_val = -2.0;
      }
      if (print228_def) {
        set_print_indent(stdout, &(irdata->ctx_pr_out), print228_val);
      }
    }
    label_test_type: ;
    
    irdata->nb_refs_target = sav208_nb_refs_target;
    irdata->nb_tmps_target = sav208_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=fun_test
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  __ABS__
    abs(indefini) = indefini
    abs(1) = 1
    abs(-1) = 1
  
  __AFFICHER__
    afficher "Bonjour, monde!" : Bonjour, monde!
    afficher (0) : 0
    afficher (3.1415926535):2 : 3,14
    afficher (3.1415926535):2..4 : 3,1416
  
  __ARR__
    arr(indefini) = indefini
    arr(1.8) = 2
    arr(-1.7) = -2
  
  __ATTRIBUT__
    attribut(X, primrest) = 0
    attribut(TAB, primrest) = 2
  
  __CHAMP_EVENEMENT__
    Avant d'initialiser les champs de l'événement:
      champ_evenement(0, numero) = indefini
      champ_evenement(0, code) = indefini
    Après avoir initialisé les champs de l'événement:
      champ_evenement(0, numero) = 42
      champ_evenement(0, code) = 2
  
  __INF__
    inf(indefini) = indefini
    inf(1.8) = 1
    inf(-1.7) = -2
  
  __MAX__
    max(indefini, indefini) = indefini
    max(-1, indefini) = 0
    max(indefini, -1) = 0
    max(1, indefini) = 1
  
  __MEME_VARIABLE__
    meme_variable(X,X) = 1
    meme_variable(X,TAB) = 0
    meme_variable(TAB[0],TAB) = 0
  
  __MIN__min(indefini, indefini) = indefini
    min(1, indefini) = 0
    min(indefini, 1) = 0
    min(-1, indefini) = -1
  
  __MULTIMAX__
    Avant initialisation du tableau :
      multimax(indefini, TAB) = indefini
      multimax(7, TAB) = indefini
      multimax(taille(TAB) + 1, TAB) = indefini
      multimax(0, TAB) = indefini
      multimax(-1, TAB) = indefini
    Après initialisation du tableau :
      multimax(indefini, TAB) = indefini
      multimax(7, TAB) = 6
      multimax(taille(TAB) + 1, TAB) = 9
      multimax(0, TAB) = indefini
      multimax(-1, TAB) = indefini
  
  __NB_EVENEMENTS__
    Avant la définition d'un événement :
      nb_evenements() = 0
    Pendant la définition d'un événement :
      nb_evenements() = 1
    Après la définition d'un événement :
      nb_evenements() = 0
  
  __NULL__
    null(indefini) = indefini
    null(0) = 1
    null(1) = 0
  
  __POSITIF__
    positif(indefini) = indefini
    positif(0) = 0
    positif(1) = 1
    positif(-1) = 0
  
  __POSITIF OU NUL__
    positif_ou_nul(indefini) = indefini
    positif_ou_nul(0) = 1
    positif_ou_nul(1) = 1
    positif_ou_nul(-1) = 0
  
  __PRESENT__
    present(indefini) = 0
    present(0) = 1
    present(1) = 1
  
  __SOMME__
    somme() = 0
    somme(indefini) = indefini
    somme(1, indefini) = 1
    somme(1, 2, 3, 4, 5) = 15
  
  __SUPZERO__
    supzero(indefini) = indefini
    supzero(42) = 42
    supzero(-1) = indefini
    supzero(0) = indefini
  
  __TAILLE__
    taille(TAB) = 10
    taille(X) = 1
  
  __TYPE__
    type(X, REEL) = 1
    type(X, ENTIER) = 0
    type(TAB, ENTIER) = 1
    type(Y, ENTIER) = 0
    type(Y, REEL) = 0
    type(Y, BOOLEEN) = 0
    type(Y, DATE_AAAA) = 0
    type(Y, DATE_JJMMAAAA) = 0
    type(Y, DATE_MM) = 0
  
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides

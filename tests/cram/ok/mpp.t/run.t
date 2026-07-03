
  $ mlang aiguillage.m --mpp_function aigui_test -A app --income-year=2020 --dgfip_options="-m2020,-X" --run_test test.irj --no_nondet_display
  Z = 0
  Z = 1
  Z = indefini
  Z = 0
  Z = 1
  Z = indefini
  [RESULT] test.irj
  [RESULT] Aucun echec!
  [RESULT] Test exécuté!
  $ mlang aiguillage.m --mpp_function aigui_test -A app --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c > /dev/null
  $ cat output/m_aiguillage.c
  #include "mlang.h" 
  
  struct S_discord * aigui_test(T_irdata* irdata) {
    int sav0_nb_tmps_target = irdata->nb_tmps_target;
    int sav0_nb_refs_target = irdata->nb_refs_target;
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
      (def_calculee[0/*X*/]) = 1;
      (calculee[0/*X*/]) = 0.0;
    }
    {
      (def_calculee[1/*Y*/]) = 1;
      (calculee[1/*Y*/]) = 1.0;
    }
    {
      int must_exec = 1;
      if (must_exec) {
        irdata->refs[irdata->refs_org + 0].name = "V";
        irdata->refs[irdata->refs_org + 0].info = I_(calculee,0/*X*/);
        irdata->refs[irdata->refs_org + 0].var_space = (irdata->var_space);
        irdata->refs[irdata->refs_org + 0].def = &(def_calculee[0/*X*/]);
        irdata->refs[irdata->refs_org + 0].val = &(calculee[0/*X*/]);
      }
      if (must_exec) {
        aiguillage_val(irdata);
      }
      
    }
    
    {
      char print1_def;
      double print1_val;
      int print1;
      print_string(stdout, &(irdata->ctx_pr_out), "Z = ");
      {
        print1_def = (def_calculee[2/*Z*/]);
        if (print1_def) {
          print1_val = (calculee[2/*Z*/]);
        } else print1_val = 0.0;
      }
      if (print1_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print1_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      int must_exec = 1;
      if (must_exec) {
        irdata->refs[irdata->refs_org + 0].name = "V";
        irdata->refs[irdata->refs_org + 0].info = I_(calculee,1/*Y*/);
        irdata->refs[irdata->refs_org + 0].var_space = (irdata->var_space);
        irdata->refs[irdata->refs_org + 0].def = &(def_calculee[1/*Y*/]);
        irdata->refs[irdata->refs_org + 0].val = &(calculee[1/*Y*/]);
      }
      if (must_exec) {
        aiguillage_val(irdata);
      }
      
    }
    
    {
      char print2_def;
      double print2_val;
      int print2;
      print_string(stdout, &(irdata->ctx_pr_out), "Z = ");
      {
        print2_def = (def_calculee[2/*Z*/]);
        if (print2_def) {
          print2_val = (calculee[2/*Z*/]);
        } else print2_val = 0.0;
      }
      if (print2_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print2_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      (def_calculee[2/*Z*/]) = 0;
      if ((def_calculee[2/*Z*/])) {
        (calculee[2/*Z*/]) = 0.0;
      } else (calculee[2/*Z*/]) = 0.0;
    }
    {
      int must_exec = 1;
      if (must_exec) {
        irdata->refs[irdata->refs_org + 0].name = "V";
        irdata->refs[irdata->refs_org + 0].info = I_(calculee,2/*Z*/);
        irdata->refs[irdata->refs_org + 0].var_space = (irdata->var_space);
        irdata->refs[irdata->refs_org + 0].def = &(def_calculee[2/*Z*/]);
        irdata->refs[irdata->refs_org + 0].val = &(calculee[2/*Z*/]);
      }
      if (must_exec) {
        aiguillage_val(irdata);
      }
      
    }
    
    {
      char print3_def;
      double print3_val;
      int print3;
      print_string(stdout, &(irdata->ctx_pr_out), "Z = ");
      {
        print3_def = (def_calculee[2/*Z*/]);
        if (print3_def) {
          print3_val = (calculee[2/*Z*/]);
        } else print3_val = 0.0;
      }
      if (print3_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print3_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      int must_exec = 1;
      if (must_exec) {
        irdata->refs[irdata->refs_org + 0].name = "V";
        irdata->refs[irdata->refs_org + 0].info = I_(calculee,0/*X*/);
        irdata->refs[irdata->refs_org + 0].var_space = (irdata->var_space);
        irdata->refs[irdata->refs_org + 0].def = &(def_calculee[0/*X*/]);
        irdata->refs[irdata->refs_org + 0].val = &(calculee[0/*X*/]);
      }
      if (must_exec) {
        aiguillage_var(irdata);
      }
      
    }
    
    {
      char print4_def;
      double print4_val;
      int print4;
      print_string(stdout, &(irdata->ctx_pr_out), "Z = ");
      {
        print4_def = (def_calculee[2/*Z*/]);
        if (print4_def) {
          print4_val = (calculee[2/*Z*/]);
        } else print4_val = 0.0;
      }
      if (print4_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print4_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      int must_exec = 1;
      if (must_exec) {
        irdata->refs[irdata->refs_org + 0].name = "V";
        irdata->refs[irdata->refs_org + 0].info = I_(calculee,1/*Y*/);
        irdata->refs[irdata->refs_org + 0].var_space = (irdata->var_space);
        irdata->refs[irdata->refs_org + 0].def = &(def_calculee[1/*Y*/]);
        irdata->refs[irdata->refs_org + 0].val = &(calculee[1/*Y*/]);
      }
      if (must_exec) {
        aiguillage_var(irdata);
      }
      
    }
    
    {
      char print5_def;
      double print5_val;
      int print5;
      print_string(stdout, &(irdata->ctx_pr_out), "Z = ");
      {
        print5_def = (def_calculee[2/*Z*/]);
        if (print5_def) {
          print5_val = (calculee[2/*Z*/]);
        } else print5_val = 0.0;
      }
      if (print5_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print5_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    {
      int must_exec = 1;
      if (must_exec) {
        irdata->refs[irdata->refs_org + 0].name = "V";
        irdata->refs[irdata->refs_org + 0].info = I_(calculee,2/*Z*/);
        irdata->refs[irdata->refs_org + 0].var_space = (irdata->var_space);
        irdata->refs[irdata->refs_org + 0].def = &(def_calculee[2/*Z*/]);
        irdata->refs[irdata->refs_org + 0].val = &(calculee[2/*Z*/]);
      }
      if (must_exec) {
        aiguillage_var(irdata);
      }
      
    }
    
    {
      char print6_def;
      double print6_val;
      int print6;
      print_string(stdout, &(irdata->ctx_pr_out), "Z = ");
      {
        print6_def = (def_calculee[2/*Z*/]);
        if (print6_def) {
          print6_val = (calculee[2/*Z*/]);
        } else print6_val = 0.0;
      }
      if (print6_def) {
        print_double(stdout, &(irdata->ctx_pr_out), print6_val, 0, 20);
      } else {
        print_string(stdout, &(irdata->ctx_pr_out), "indefini");
      }
      print_string(stdout, &(irdata->ctx_pr_out), "\012");
    }
    label_aigui_test: ;
    
    irdata->nb_refs_target = sav0_nb_refs_target;
    irdata->nb_tmps_target = sav0_nb_tmps_target;
    return irdata->discords;
  }
  
  struct S_discord * aiguillage_val(T_irdata* irdata) {
    int sav7_nb_tmps_target = irdata->nb_tmps_target;
    int sav7_nb_refs_target = irdata->nb_refs_target;
    char *def_saisie = irdata->def_saisie;
    double *saisie = irdata->saisie;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
    char *def_base = irdata->def_base;
    double *base = irdata->base;
    T_var_space var_space = irdata->var_space_courant;
    irdata->refs_org = irdata->refs_org + 1;
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 1;
    
    {
      char exp8_def;
      double exp8_val;
      {
        exp8_def = *(DR_((-1)/*V*/));
        if (exp8_def) {
          exp8_val = *(R_((-1)/*V*/));
        } else exp8_val = 0.0;
      }
      if (exp8_def) {
        /* Switch cases  */
        if (EQ_E((exp8_val),(0))) {
        
          {
            (def_calculee[2/*Z*/]) = 1;
            (calculee[2/*Z*/]) = 0.0;
          }
        }else {
          if (EQ_E((exp8_val),(1))) {
          
            {
              (def_calculee[2/*Z*/]) = 1;
              (calculee[2/*Z*/]) = 1.0;
            }
          }/* Default switch case */
          
          else {
                  {
                    (def_calculee[2/*Z*/]) = 1;
                    (calculee[2/*Z*/]) = -1.0;
                  }
          }
        }
        }
        /* End of switch cases & default */
        /* Undefined switch case */
         else 
        {
          (def_calculee[2/*Z*/]) = 0;
          if ((def_calculee[2/*Z*/])) {
            (calculee[2/*Z*/]) = 0.0;
          } else (calculee[2/*Z*/]) = 0.0;
        }}
      label_aiguillage_val: ;
      
      irdata->refs_org = irdata->refs_org - 1;
      irdata->nb_refs_target = sav7_nb_refs_target;
      irdata->nb_tmps_target = sav7_nb_tmps_target;
      return irdata->discords;
    }
  
  struct S_discord * aiguillage_var(T_irdata* irdata) {
    int sav9_nb_tmps_target = irdata->nb_tmps_target;
    int sav9_nb_refs_target = irdata->nb_refs_target;
    char *def_saisie = irdata->def_saisie;
    double *saisie = irdata->saisie;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
    char *def_base = irdata->def_base;
    double *base = irdata->base;
    T_var_space var_space = irdata->var_space_courant;
    irdata->refs_org = irdata->refs_org + 1;
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 1;
    
    {
      char exp10_def;
      double exp10_val;{
      /* Switch cases  */
      
      char is_same_var11_def;
      double is_same_var11_val;
      {
        register int int0;
        register double real0;
        T_varinfo* varinfo0;
        T_varinfo* varinfo1;
        char res12_def;
        double res12_val;
        varinfo0 = I_(calculee,0/*X*/);
        varinfo1 = IR_((-1)/*V*/);
        res12_def = meme_variable(varinfo0, varinfo1, &res12_def, &res12_val);
        res12_val = res12_val;
        is_same_var11_def = res12_def;
        if (is_same_var11_def) {
          int0 = (NEQ_E((res12_def),(0.0)));
          real0 = res12_val;
          is_same_var11_val = (int0 ? real0 : 0.0);
        } else is_same_var11_val = 0.0;
      }if (is_same_var11_val) {
      
        {
          (def_calculee[2/*Z*/]) = 1;
          (calculee[2/*Z*/]) = 0.0;
        }
      }else {
        
        char is_same_var13_def;
        double is_same_var13_val;
        {
          register int int0;
          register double real0;
          T_varinfo* varinfo0;
          T_varinfo* varinfo1;
          char res14_def;
          double res14_val;
          varinfo0 = I_(calculee,1/*Y*/);
          varinfo1 = IR_((-1)/*V*/);
          res14_def =
            meme_variable(varinfo0, varinfo1, &res14_def, &res14_val);
          res14_val = res14_val;
          is_same_var13_def = res14_def;
          if (is_same_var13_def) {
            int0 = (NEQ_E((res14_def),(0.0)));
            real0 = res14_val;
            is_same_var13_val = (int0 ? real0 : 0.0);
          } else is_same_var13_val = 0.0;
        }if (is_same_var13_val) {
        
          {
            (def_calculee[2/*Z*/]) = 1;
            (calculee[2/*Z*/]) = 1.0;
          }
        }else {
          
          char is_same_var15_def;
          double is_same_var15_val;
          {
            register int int0;
            register double real0;
            T_varinfo* varinfo0;
            T_varinfo* varinfo1;
            char res16_def;
            double res16_val;
            varinfo0 = I_(calculee,2/*Z*/);
            varinfo1 = IR_((-1)/*V*/);
            res16_def =
              meme_variable(varinfo0, varinfo1, &res16_def, &res16_val);
            res16_val = res16_val;
            is_same_var15_def = res16_def;
            if (is_same_var15_def) {
              int0 = (NEQ_E((res16_def),(0.0)));
              real0 = res16_val;
              is_same_var15_val = (int0 ? real0 : 0.0);
            } else is_same_var15_val = 0.0;
          }if (is_same_var15_val) {
          
            {
              (def_calculee[2/*Z*/]) = 0;
              if ((def_calculee[2/*Z*/])) {
                (calculee[2/*Z*/]) = 0.0;
              } else (calculee[2/*Z*/]) = 0.0;
            }
          }/* Default switch case */
          
          else {
                  {
                    (def_calculee[2/*Z*/]) = 1;
                    (calculee[2/*Z*/]) = -1.0;
                  }
          }
        }
        
      }
      }
      /* End of switch cases & default */
      }
    label_aiguillage_var: ;
    
    irdata->refs_org = irdata->refs_org - 1;
    irdata->nb_refs_target = sav9_nb_refs_target;
    irdata->nb_tmps_target = sav9_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=aigui_test
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  Z = 0
  Z = 1
  Z = indefini
  Z = 0
  Z = 1
  Z = indefini
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides

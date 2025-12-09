#*************************************************************************************************************************
#
#Copyright or © or Copr.[DGFIP][2023]
#
#Ce logiciel a été initialement développé par la Direction Générale des 
#Finances Publiques pour permettre le calcul de l'impôt sur le revenu 2023 
#au titre des revenus perçus en 2022. La présente version a permis la 
#génération du moteur de calcul des chaînes de taxation des rôles d'impôt 
#sur le revenu de ce millésime.
#
#Ce logiciel est régi par la licence CeCILL 2.1 soumise au droit français 
#et respectant les principes de diffusion des logiciels libres. Vous pouvez 
#utiliser, modifier et/ou redistribuer ce programme sous les conditions de 
#la licence CeCILL 2.1 telle que diffusée par le CEA, le CNRS et l'INRIA  sur 
#le site "http://www.cecill.info".
#
#Le fait que vous puissiez accéder à cet en-tête signifie que vous avez pris 
#connaissance de la licence CeCILL 2.1 et que vous en avez accepté les termes.
#
#**************************************************************************************************************************
regle isf 232:
application : iliad ;


SUPIFI[X] = positif(FLAG_RETARD) * positif(FLAG_RECTIFMAJO) * null(X)
            * max(IFI4BASE,0)
                + (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIFMAJO) * null(X))
            * max(0,IFI4BASE - (TIFI4BASE[FLAG_DERSTTR]))
                + (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIFMAJO) * null(X))
            * max(0,(TIFI4BASE[FLAG_DERSTTR])-IFI4BASE)*positif(FLAGDEFAUT10)
                +(1 - positif(FLAG_RETARD) * positif(FLAG_RECTIFMAJO) * null(X))
            * max(0,(TIFI4BASE[FLAG_DERSTTR])-IFI4BASE)*positif(FLAGDEFAUT11);

regle 23111:
application : iliad ;
IRBASE = min(0 , IAN + AVFISCOPTER - IRE + CODZRA - CODCOA)
           + max(0 , IAN + AVFISCOPTER - IRE - CODCOA + CODZRA) * positif(IAMD1 + V_ANTREIR + 1 - SEUIL_61) - IRANT - max(IR9YI_P,IR9YI_PA);

TAXABASE_MAJO = (max(0,TAXASSUR-CODCOU)-max(TAXA9YI_P,TAXA9YI_PA)
                        ) * positif(IAMD1 + 1 + V_ANTREIR - SEUIL_61)*(1-APPLI_BATCH);
CAPBASE_MAJO = (max(0,IPCAPTAXT-CODCOV)-max(CAP9YI_P,CAP9YI_PA)
                         ) * positif(IAMD1 + 1 + V_ANTREIR - SEUIL_61);
HRBASE_MAJO = (max(0,IHAUTREVT+CHRPVIMP-CODCOX)-max(CHR9YI_P,CHR9YI_PA)
                      ) * positif(IAMD1  + 1 + V_ANTREIR - SEUIL_61);

CSBASE_MAJO = (max(0 , CSG - CSGIM - CODCOB)-max(CS9YP_P,CS9YP_PA)
                         ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RDBASE_MAJO = (max(0 , RDSN - CRDSIM - CODCOR)-max(RD9YP_P,RD9YP_PA)
                         ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PSOLBASE_MAJO = (max(0 , PSOL - PRSPROV - CODCOD)-max(PS9YP_P,PS9YP_PA)
                       ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

CVNBASE_MAJO = (max(0,CVNSALC - CICVN - COD8YT-CODCOE)-max(CVN9YP_P,CVN9YP_PA)
                         ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

CDISBASE_MAJO = (max(0,CDIS - CDISPROV-CODCOF)-max(CDIS9YP_P,CDIS9YP_PA)
                        ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61)*(1-APPLI_BATCH);

GLOBASE_MAJO = (max(0,CGLOA - COD8YL-CODCOG)-max(GLO9YP_P,GLO9YP_PA)
                         ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
C820BASE_MAJO = (max(0,MCSG820-CODCOQ)-max(C8209YP_P,C8209YP_PA)
                       ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RSE1BASE_MAJO = (max(0,RSE1N - CSPROVYD-CODCOT)-max(RSE19YP_P,RSE19YP_PA)
                       ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61)*(1-APPLI_BATCH);

RSE2BASE_MAJO = (max(0,max(0, RSE8TV - CIRSE8TV - CSPROVYF) + max(0, RSE8SA - CIRSE8SA - CSPROVYN)-CODCOL)-max(RSE29YP_P,RSE29YP_PA)
                   ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61)*(1-APPLI_BATCH);

RSE3BASE_MAJO = (max(0,RSE3N - CSPROVYG-CODCOM)-max(RSE39YP_P,RSE39YP_PA)
                            ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61)*(1-APPLI_BATCH);
RSE4BASE_MAJO = (max(0,RSE4N - CSPROVYH - CSPROVYP-CODCOO)-max(RSE49YP_P,RSE49YP_PA)
                        ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61)*(1-APPLI_BATCH);

RSE5BASE_MAJO = (max(0,RSE5N - CSPROVYE-CODCOJ)-max(RSE59YP_P,RSE59YP_PA)
                        ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61)*(1-APPLI_BATCH);

RSE6BASE_MAJO = (max(0,RSE6N-CODCOP)-max(RSE69YP_P,RSE69YP_PA)
                        ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61)*(1-APPLI_BATCH);
RSE8BASE_MAJO = (max(0,RSE8N-COD8YV-COD8YX-CODCOH)-max(RSE89YP_P,RSE89YP_PA)
                        ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61)*(1-APPLI_BATCH);
regle corrective 231120:
application :   iliad ;


VARIRDROIT = max( 0 , TOT_REF - IRNIN_P - TAXA_P - CHR_P - PCAP_P
                ) + 0 ;

regle corrective 23112:
application :   iliad ;
TOT_BASE_MAJO = IRBASE + TAXABASE_MAJO + CAPBASE_MAJO + HRBASE_MAJO ;

TOT_REF = TIRBASE[FLAG_DERSTTR] +TTAXABASE[FLAG_DERSTTR] +TPCAPBASE[FLAG_DERSTTR]+TCHRBASE[FLAG_DERSTTR];


TAXA_ISO = TAXASSUR * positif(IAMD1 + 1 - SEUIL_61) ; 
CAP_ISO  = IPCAPTAXT * positif(IAMD1 + 1 - SEUIL_61) ; 
HR_ISO   = (IHAUTREVT + CHRPVIMP) * positif(IAMD1 + 1 - SEUIL_61) ; 


PENA_RESTIT = max(0, IRBASE - TIRBASE[FLAG_DERSTTR]);

NOPENA_RESTIT = max(0 , min( IRBASE - TIRBASE[FLAG_DERSTTR] ,
			     max( 0, IRBASE + TTAXABASE[FLAG_DERSTTR]+TPCAPBASE[FLAG_DERSTTR] +TCHRBASE[FLAG_DERSTTR])
                           )
	           );
SUPIR[X] =  null(X) * positif(FLAG_RETARD) + positif(FLAG_RECTIF)
                   * min( max(0, max(TIRBASE[FLAG_DERSTTR] , IRBASE)) ,
                          max(0, IRBASE2042_FIC )
                        )


	      + ( 1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
		  * positif(null(X-3)+null(X-7)+null(X-8)+null(X-9)+null(X-13)+null(X-14))
		  * PENA_RESTIT


              + ( 1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
                  * (1 - positif((null(X-3)+null(X-7)+null(X-8)+null(X-9)+null(X-13)+null(X-14))))
                  * (1 - positif(null(X-1)))
                  * (1 - positif(null(X-15)+null(X-16)))
	          * ((1 - positif(TARDIFEVT2)*null(X-2))
                      * NOPENA_RESTIT 
                      + positif(TARDIFEVT2) * null(X-2) * TIRBASE[FLAG_DERSTTR]
	            )

           + (1 - positif((null(X-3)+null(X-7)+null(X-8)+null(X-9)+null(X-13)+null(X-14)))) 
              *  null(X-1)*positif( null(CSTRATE1 - 1) 
	                           +null(CSTRATE1 - 7) 
				   +null(CSTRATE1 - 8)
				   +null(CSTRATE1 - 11)
                                   +null(CSTRATE1 - 18)) 
			               * NOPENA_RESTIT

           + null(X-1)* positif( null(CSTRATE1 - 3)
	                        +null(CSTRATE1 - 4)
	                        +null(CSTRATE1 - 35)
			        +null(CSTRATE1 - 5)
			        +null(CSTRATE1 - 6)
		                +null(CSTRATE1 - 55))
                              		* PENA_RESTIT ; 

SUP2IR[X] = null(X) * null(CODE_2042 - 17) * positif(FLAG_RETARD) * positif(FLAG_RECTIF) 
                    * max(IRBASE,0)

	      + ((positif(null(X-15)+null(X-16)+null(X-19)+null(X-21)+null(X-23))
                    * PENA_RESTIT 
	          )
	          + (1 - positif(null(X-15)+null(X-16)+null(X-19)+null(X-21)))* 0)
                 * (1 - positif(null(X-1))) 
           + null(X-1)*positif( null(CSTRATE1 - 1)
                               +null(CSTRATE1 - 17)
	                       +null(CSTRATE1 - 2)
			       +null(CSTRATE1 - 10)
		               +null(CSTRATE1 - 30)) 
	             * PENA_RESTIT;



SUPCS[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TCSBASE[0] ,CSBASE_MAJO  )) ,
                           max(0, CSBASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, CSBASE_MAJO  - TCSBASE[FLAG_DERSTTR]) ;
SUPPSOL[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TPSOLBASE[0] , PSOLBASE_MAJO )) ,
                           max(0, PSOLBASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, PSOLBASE_MAJO - TPSOLBASE[FLAG_DERSTTR]) ;
SUPRD[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TRDBASE[0] ,RDBASE_MAJO )) ,
                           max(0, RDBASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, RDBASE_MAJO  - TRDBASE[FLAG_DERSTTR]) ;
SUPC820[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TC820BASE[0] , C820BASE_MAJO)) ,
                           max(0, C820BASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, C820BASE_MAJO - TC820BASE[FLAG_DERSTTR]) ;
SUPGLO[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TGLOBASE[0] , GLOBASE_MAJO)) ,
                           max(0, GLOBASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, GLOBASE_MAJO - TGLOBASE[FLAG_DERSTTR]) ;


SUPCDIS[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TCDISBASE[0] ,CDISBASE_MAJO)) ,
                           max(0, CDISBASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, CDISBASE_MAJO - TCDISBASE[FLAG_DERSTTR]) ;


SUPCVN[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TCVNBASE[0] ,CVNBASE_MAJO)) ,
                           max(0, CVNBASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, CVNBASE_MAJO - TCVNBASE[FLAG_DERSTTR]) ;


SUPRSE1[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TRSE1BASE[0] , RSE1BASE_MAJO)) ,
                           max(0, RSE1BASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, RSE1BASE_MAJO - TRSE1BASE[FLAG_DERSTTR]) ;


SUPRSE2[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TRSE2BASE[0] , RSE2BASE_MAJO)) ,
                           max(0, RSE2BASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, RSE2BASE_MAJO - TRSE2BASE[FLAG_DERSTTR]) ;


SUPRSE3[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TRSE3BASE[0] , RSE3BASE_MAJO)) ,
                           max(0, RSE3BASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, RSE3BASE_MAJO - TRSE3BASE[FLAG_DERSTTR]) ;


SUPRSE4[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TRSE4BASE[0] , RSE4BASE_MAJO)) ,
                           max(0, RSE4BASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, RSE4BASE_MAJO - TRSE4BASE[FLAG_DERSTTR]) ;


SUPRSE5[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TRSE5BASE[0] , RSE5BASE_MAJO)) ,
                           max(0, RSE5BASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, RSE5BASE_MAJO - TRSE5BASE[FLAG_DERSTTR]) ;

SUPRSE6[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TRSE6BASE[0] , RSE6BASE_MAJO)) ,
                           max(0, RSE6BASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, RSE6BASE_MAJO - TRSE6BASE[FLAG_DERSTTR]) ;

SUPRSE8[X] =  positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
             * min( max(0, max (TRSE8BASE[0] , RSE8BASE_MAJO)) ,
                           max(0, RSE8BASE2042_FIC )
                  )
          +  (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
             * max(0, RSE8BASE_MAJO - TRSE8BASE[FLAG_DERSTTR]) ;



SUPTAXA[X] =  null(X) * positif(FLAG_RETARD) * positif(FLAG_RECTIF) 
                      * max(0 ,
                               min( TAXABASE2042_FIC , 
                                      min ( 
                                            max( TTAXABASE[0] , TAXABASE_MAJO) ,
                                            max( TIRBASE[0] + TTAXABASE[0] , IRBASE + TAXABASE_MAJO)
                                          )
                                  )
                            ) 

	      +( 1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
		 * positif(null(X-3)+null(X-7)+null(X-8)+null(X-9)+null(X-13)+null(X-14))
		 * max( 0, TAXABASE_MAJO - TTAXABASE[FLAG_DERSTTR])


              + ( 1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
		* (1 - positif((null(X-3)+null(X-7)+null(X-8)+null(X-9)+null(X-13)+null(X-14))))
                  * (1 - positif(null(X-15)+null(X-16)))
                * max( 0, min( TAXABASE_MAJO - TTAXABASE[FLAG_DERSTTR],
				     max( 0, IRBASE + TAXABASE_MAJO + TPCAPBASE[FLAG_DERSTTR] + TCHRBASE[FLAG_DERSTTR] )
                             )
		     );



SUP2TAXA[X] = null(X) * null(CODE_2042 - 17) * positif(FLAG_RETARD) * positif(FLAG_RECTIF)
                      * max( 0, min( TAXABASE_MAJO - TTAXABASE[FLAG_DERSTTR],
				     max( 0, IRBASE + TAXABASE_MAJO + TPCAPBASE[FLAG_DERSTTR] + TCHRBASE[FLAG_DERSTTR] )
                                   )
                           )

	      + positif(20 - V_NOTRAIT) * positif(null(X-15))
                      * max( 0, min( TAXABASE_MAJO - TTAXABASE[FLAG_DERSTTR],
				     max( 0, IRBASE + TAXABASE_MAJO + TPCAPBASE[FLAG_DERSTTR] + TCHRBASE[FLAG_DERSTTR] )
                                   )
                           )

	      + (1-positif(20 - V_NOTRAIT)) * positif(null(X-15))
		* max( 0, TAXABASE_MAJO - TTAXABASE[FLAG_DERSTTR])
	        
	      + positif(null(X-16)+null(X-19)+null(X-23))
		* max( 0, TAXABASE_MAJO - TTAXABASE[FLAG_DERSTTR])

	     + (1 - positif(null(X-15)+null(X-16)+null(X-19)+null(X-23))) * 0
	     ;


SUPCAP[X] =  null(X) * positif(FLAG_RETARD) * positif(FLAG_RECTIF)
                      * max(0 ,
                               min( CAPBASE2042_FIC , 
                                      min ( 
                                            max( TPCAPBASE[0] , CAPBASE_MAJO) ,
                                            max( TIRBASE[0] + TTAXABASE[0] + TPCAPBASE[0] , 
                                                 IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                          )
                                  )
                            )				  

	      +( 1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
		 * positif(null(X-3)+null(X-7)+null(X-8)+null(X-9)+null(X-13)+null(X-14))
                 * max( 0, CAPBASE_MAJO - TPCAPBASE[FLAG_DERSTTR] )

	+( 1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
		* (1 - positif((null(X-3)+null(X-7)+null(X-8)+null(X-9)+null(X-13)+null(X-14))))
                  * (1 - positif(null(X-15)+null(X-16)))
	        * max(0, min( CAPBASE_MAJO - TPCAPBASE[FLAG_DERSTTR] , 
	                      max( 0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO + TCHRBASE[FLAG_DERSTTR] )
                            )
                     );
                
SUP2CAP[X] = null(X) * null(CODE_2042 - 17) * positif(FLAG_RETARD) * positif(FLAG_RECTIF)
	             * max(0, min( CAPBASE_MAJO - TPCAPBASE[FLAG_DERSTTR] , 
	                           max( 0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO + TCHRBASE[FLAG_DERSTTR] )
                                 )
                           )

              + positif(20 - V_NOTRAIT) * positif(null(X-15))            
	             * max(0, min( CAPBASE_MAJO - TPCAPBASE[FLAG_DERSTTR] , 
	                           max( 0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO + TCHRBASE[FLAG_DERSTTR] )
                                 )
                           )
              + (1-positif(20 - V_NOTRAIT)) * positif(null(X-15))
		* max(0, CAPBASE_MAJO - TPCAPBASE[FLAG_DERSTTR])
                      
              + positif(null(X-16)+null(X-19)+null(X-21)+null(X-23))
	         * max(0, CAPBASE_MAJO - TPCAPBASE[FLAG_DERSTTR]) 
                 
              + (1 - positif(null(X-15)+null(X-16)+null(X-19)+null(X-21)+null(X-23))) * 0
	      ;

SUPHR[X] =  null(X) * positif(FLAG_RETARD) * positif(FLAG_RECTIF)
                      * max(0 ,
                               min( HRBASE2042_FIC , 
                                      min ( 
                                            max( TCHRBASE[0] , HRBASE_MAJO) ,
                                            max( TIRBASE[0] + TTAXABASE[0] + TPCAPBASE[0] + TCHRBASE[0] , IRBASE + TAXABASE_MAJO + CAPBASE_MAJO + HRBASE_MAJO )
                                          )
                                  )
                            ) 


	      +( 1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
		 * positif(null(X-3)+null(X-7)+null(X-8)+null(X-9)+null(X-13)+null(X-14))
		 * max( 0, HRBASE_MAJO - TCHRBASE[FLAG_DERSTTR] )


              + ( 1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
		* (1 - positif((null(X-3)+null(X-7)+null(X-8)+null(X-9)+null(X-13)+null(X-14))))
                  * (1 - positif(null(X-15)+null(X-16)))
                * max(0 , min( HRBASE_MAJO - TCHRBASE[FLAG_DERSTTR] ,
			       max( 0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO + HRBASE_MAJO )
                             )
                     );


SUP2HR[X] = null(X) * null(CODE_2042 - 17) * positif(FLAG_RETARD) * positif(FLAG_RECTIF) 
                     * max(0 , min( HRBASE_MAJO - TCHRBASE[FLAG_DERSTTR] ,
				    max( 0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO + HRBASE_MAJO )
                                  )
                          )
              + positif(20 - V_NOTRAIT) * positif(null(X-15))
                     * max(0 , min( HRBASE_MAJO - TCHRBASE[FLAG_DERSTTR] ,
				    max( 0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO + HRBASE_MAJO )
                                  )
                          )

              + (1-positif(20 - V_NOTRAIT)) * positif(null(X-15))
	             * max(0 , HRBASE_MAJO - TCHRBASE[FLAG_DERSTTR])

              + positif(null(X-16)+null(X-19)+null(X-21)+null(X-23))
                * max(0 , HRBASE_MAJO - TCHRBASE[FLAG_DERSTTR])
		
              + (1 - positif(null(X-15)+null(X-16)+null(X-19)+null(X-21)+null(X-23))) * 0
	      ;

regle corrective 23113:
application : iliad;
TMAJOIR[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
             * (
                   (1-positif(null(X - 0) * null(CODE_2042 - 17))) * arr(SUPIR[X] * TAUX_STRATE/100)
                   + positif(null(X - 0) * null(CODE_2042 - 17))  * arr(SUPIR[X] * 2 * TX1758A/100)
               );
T2MAJOIR[X] = (1 - null(1 - IND_RJLJ))
     * (
	     (positif(null(X - 0) * null(CODE_2042 - 17) 
		      + null(X-15)+null(X-16)+null(X-19)+null(X-21)+null(X-23)
		     )
        	*(positif(null(X-22)+null(X-23))*TL_IR*arr(SUP2IR[X] * TX1758A/100)
	          +(1-null(X-23)) * (1-null(X-15))* arr(SUP2IR[X] * TX1758A/100)
                  + null(X-15) * arr(SUP2IR[X] * 2 * TX1758A/100)
		 )
	     )

	      + null(X-1) 
	                  *( positif(null(CSTRATE1 - 1)
	                            +null(CSTRATE1 - 2)
		                    +null(CSTRATE1 - 30)
		                    +null(CSTRATE1 - 10)
				    ) * arr(SUP2IR[X] * TX1758A/100)
                            + null(CSTRATE1 - 17) * arr(SUP2IR[X] * 2 * TX1758A/100)
			   )
                 ); 

MAJOIR_ST = MAJOIRST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOIR[X] + T2MAJOIR[X];
TMAJOCS[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPCS[X] * TAUX_STRATE/100 ));
MAJOCS_ST = MAJOCSST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOCS[X] ;
TMAJOPSOL[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPPSOL[X] * TAUX_STRATE/100 ));
MAJOPSOL_ST = MAJOPSOLST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOPSOL[X] ;
TMAJORD[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPRD[X] * TAUX_STRATE/100 ));
MAJORD_ST = MAJORDST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJORD[X] ;

TMAJOCVN[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPCVN[X] * TAUX_STRATE/100 ));
MAJOCVN_ST = MAJOCVNST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOCVN[X] ;

TMAJOCDIS[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPCDIS[X] * TAUX_STRATE/100 ));

MAJOCDIS_ST = MAJOCDISST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOCDIS[X] ;
TMAJOC820[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPC820[X] * TAUX_STRATE/100 ));
MAJOC820_ST = MAJOC820ST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOC820[X] ;
TMAJOGLO[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPGLO[X] * TAUX_STRATE/100 ));
MAJOGLO_ST = MAJOGLOST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOGLO[X] ;
TMAJORSE1[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPRSE1[X] * TAUX_STRATE/100 ));
MAJORSE1_ST = MAJORSE1ST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJORSE1[X] ;
TMAJORSE2[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPRSE2[X] * TAUX_STRATE/100 ));
MAJORSE2_ST = MAJORSE2ST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJORSE2[X] ;
TMAJORSE3[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPRSE3[X] * TAUX_STRATE/100 ));
MAJORSE3_ST = MAJORSE3ST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJORSE3[X] ;
TMAJORSE4[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPRSE4[X] * TAUX_STRATE/100 ));
MAJORSE4_ST = MAJORSE4ST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJORSE4[X] ;
TMAJORSE5[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPRSE5[X] * TAUX_STRATE/100 ));
            
MAJORSE5_ST = MAJORSE5ST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJORSE5[X] ;

TMAJORSE6[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPRSE6[X] * TAUX_STRATE/100 ));
            
MAJORSE6_ST = MAJORSE6ST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJORSE6[X] ;
TMAJORSE8[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPRSE8[X] * TAUX_STRATE/100 ));
            
MAJORSE8_ST = MAJORSE8ST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJORSE8[X] ;
TMAJOTAXA[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPTAXA[X] * TAUX_STRATE/100 ));
T2MAJOTAXA[X] = (1 - null(1 - IND_RJLJ))
			* positif(  null(X - 0) * null(CODE_2042 - 17) 
	                           + null(X-15)+null(X-16)+null(X-19)+null(X-23)
	        	          ) 
	               	 	   *(  null(X-23)*TL_TAXAGA*arr(SUP2TAXA[X] * TX1758A/100)
	                             +(1-null(X-23))*((1-null(X-15))* arr(SUP2TAXA[X] * TX1758A/100)
			                              + null(X-15)* arr(SUP2TAXA[X] * 2 * TX1758A/100) 
                                                     )
			            ) ;
	                  

MAJOTAXA_ST = MAJOTAXAST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOTAXA[X] + T2MAJOTAXA[X];
TMAJOHR[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPHR[X] * TAUX_STRATE/100 ));
T2MAJOHR[X] = (1 - null(1 - IND_RJLJ))
			*  positif(  null(X - 0) * null(CODE_2042 - 17) 
	                           + null(X-15)+null(X-16)+null(X-19)+null(X-21)+null(X-23)
		       		  )	
		        	   *(   positif(null(X-21)+null(X-23))*TL_CHR*arr(SUP2HR[X] * TX1758A/100)
		          	     + (1-positif(null(X-21)+null(X-23))) * ( (1-null(X-15)) * arr(SUP2HR[X] * TX1758A/100)
								               + null(X-15) * arr(SUP2HR[X] * 2 * TX1758A/100)
			                                                    )
			            ) ;
			 
MAJOHR_ST = MAJOHRST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOHR[X] + T2MAJOHR[X];
TMAJOCAP[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
	     * arr( (SUPCAP[X] * TAUX_STRATE/100 ));
T2MAJOCAP[X] = (1 - null(1 - IND_RJLJ))

	     * positif(null(X - 0) * null(CODE_2042 - 17) 
	       	        + null(X-15)+null(X-16)+null(X-19)+null(X-23)
		       )
		        *(null(X-23)*TL_CAP*arr(SUP2CAP[X] * TX1758A/100)
		          +(1-null(X-23)) * ( (1-null(X-15)) * arr(SUP2CAP[X] * TX1758A/100)
			                       + null(X-15)* arr(SUP2CAP[X] * 2 * TX1758A/100)
 					    )
		         ) ;
	       
MAJOCAP_ST = MAJOCAPST_DEF * (1 - positif(FLAG_1STRATE)) + 
            TMAJOCAP[X] + T2MAJOCAP[X];
regle isf  233:
application : iliad;

TMAJOIFI[X] = (1 - null((1 - IND_RJLJ) + (10 - TAUX_STRATE)))
             * arr( (SUPIFI[X] * TAUX_STRATE/100 ))
	     ;

regle corrective 23114:
application : iliad;
MAJOTARDCOA = arr( CODCOA * TAUX_2042/100 );
MAJOTARDCOB = arr( CODCOB * TAUX_2042/100 );
MAJOTARDCOR = arr( CODCOR * TAUX_2042/100 );
MAJOTARDCOD = arr( CODCOD * TAUX_2042/100 );
MAJOTARDCOE = arr( CODCOE * TAUX_2042/100 );
MAJOTARDCOF = arr( CODCOF * TAUX_2042/100 );
MAJOTARDCOQ = arr( CODCOQ * TAUX_2042/100 );
MAJOTARDCOG = arr( CODCOG * TAUX_2042/100 );
MAJOTARDCOT = arr( CODCOT * TAUX_2042/100 );
MAJOTARDCOL = arr( CODCOL * TAUX_2042/100 );
MAJOTARDCOM = arr( CODCOM * TAUX_2042/100 );
MAJOTARDCOO = arr( CODCOO * TAUX_2042/100 );
MAJOTARDCOJ = arr( CODCOJ * TAUX_2042/100 );
MAJOTARDCOP = arr( CODCOP * TAUX_2042/100 );
MAJOTARDCOH = arr( CODCOH * TAUX_2042/100 );
MAJOTARDCOU = arr( CODCOU * TAUX_2042/100 );
MAJOTARDCOV = arr( CODCOV * TAUX_2042/100 );
MAJOTARDCOX = arr( CODCOX * TAUX_2042/100 );
MAJOIR02_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                            min(min(IRBASE2042_FIC,IRBASE),
                                    max(0, IRBASE)
                                   )
                               )
                              * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                               * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                          );
MAJOIR03_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                                 min(min(IRBASE2042_FIC,IRBASE),
                                 max(0, IRBASE)
                                )
                            )
                            * STR_TR14
                            * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                          );
MAJOIR07_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                            min(min(IRBASE2042_FIC,IRBASE),
                                max(0, IRBASE)
                               )
                            )
			* STR_TR17 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              ); 
MAJOIR08_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                            min(min(IRBASE2042_FIC,IRBASE),
                                max(0, IRBASE)
                               )
                            )
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              ); 

MAJOIR17_2TARDIF = max(0,arr(FLAG_TRTARDIF * 2 * TX1758A/100 *
                            min(min(IRBASE2042_FIC,IRBASE),
                                max(0, IRBASE)
                               )
                            )
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              ); 

MAJOIR_TARDIF = somme(x = 02,03,07,08: MAJOIR0x_TARDIF) + MAJOIR17_2TARDIF;
MAJOCS02_TARDIF = max(0,arr(FLAG_TRTARDIF * (CSBASE_MAJO )* TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
        );
MAJOCS03_TARDIF = max(0,arr(FLAG_TRTARDIF * (CSBASE_MAJO) * TAUX_2042/100)
                      * STR_TR14
                       * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
        );
MAJOCS07_TARDIF = max(0,arr(FLAG_TRTARDIF * CSBASE_MAJO * TAUX_2042/100)
			* STR_TR17 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	);
MAJOCS08_TARDIF = max(0,arr(FLAG_TRTARDIF * (CSBASE_MAJO)* TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	);
MAJOCS17_TARDIF = max(0,arr(FLAG_TRTARDIF * (CSBASE_MAJO)* TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	);
MAJOCS_TARDIF = somme(x = 07,08,17 : MAJOCSx_TARDIF);
MAJOPSOL02_TARDIF = max(0,arr(FLAG_TRTARDIF * (PSOLBASE_MAJO )* TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
        );
MAJOPSOL03_TARDIF = max(0,arr(FLAG_TRTARDIF * (PSOLBASE_MAJO )* TAUX_2042/100)
                        * STR_TR14
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
        );
MAJOPSOL07_TARDIF = max(0,arr(FLAG_TRTARDIF * PSOLBASE_MAJO * TAUX_2042/100)
			* STR_TR17 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	);
MAJOPSOL08_TARDIF = max(0,arr(FLAG_TRTARDIF * (PSOLBASE_MAJO )* TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	);
MAJOPSOL17_TARDIF = max(0,arr(FLAG_TRTARDIF * (PSOLBASE_MAJO )* TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	);
MAJOPSOL_TARDIF = somme(x = 07,08,17 : MAJOPSOLx_TARDIF);
MAJORD02_TARDIF = max(0,arr(FLAG_TRTARDIF * (RDBASE_MAJO)* TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                       * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
        );
MAJORD03_TARDIF = max(0,arr(FLAG_TRTARDIF * (RDBASE_MAJO)* TAUX_2042/100)
                     * STR_TR14
                      * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
       );
MAJORD07_TARDIF = max(0,arr(FLAG_TRTARDIF * RDBASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	);
MAJORD08_TARDIF = max(0,arr(FLAG_TRTARDIF * (RDBASE_MAJO)* TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	);
MAJORD17_TARDIF = max(0,arr(FLAG_TRTARDIF * (RDBASE_MAJO)* TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	);
MAJORD_TARDIF = somme(x = 07,08,17 : MAJORDx_TARDIF);

MAJOCVN02_TARDIF = max(0,arr(FLAG_TRTARDIF * CVNBASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                      );
MAJOCVN03_TARDIF = max(0,arr(FLAG_TRTARDIF * CVNBASE_MAJO * TAUX_2042/100)
                             * STR_TR14
                          * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                       );
MAJOCVN07_TARDIF = max(0,arr(FLAG_TRTARDIF * CVNBASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOCVN08_TARDIF = max(0,arr(FLAG_TRTARDIF * CVNBASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOCVN17_TARDIF = max(0,arr(FLAG_TRTARDIF * CVNBASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOCVN_TARDIF = somme(x = 07,08,17 : MAJOCVNx_TARDIF);

MAJOCDIS02_TARDIF = max(0,arr(FLAG_TRTARDIF * CDISBASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                       * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                       );
MAJOCDIS03_TARDIF = max(0,arr(FLAG_TRTARDIF * CDISBASE_MAJO * TAUX_2042/100)
                               * STR_TR14
                              * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                             );
MAJOCDIS07_TARDIF = max(0,arr(FLAG_TRTARDIF * CDISBASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	               );

MAJOCDIS08_TARDIF = max(0,arr(FLAG_TRTARDIF * CDISBASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	               );

MAJOCDIS17_TARDIF = max(0,arr(FLAG_TRTARDIF * CDISBASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	               );

MAJOCDIS_TARDIF = somme(x = 07,08,17 : MAJOCDISx_TARDIF);

MAJOC82002_TARDIF = max(0,arr(FLAG_TRTARDIF * C820BASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                      * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                     );
MAJOC82003_TARDIF = max(0,arr(FLAG_TRTARDIF * C820BASE_MAJO * TAUX_2042/100)
                          * STR_TR14
                              * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                           );
MAJOC82007_TARDIF = max(0,arr(FLAG_TRTARDIF * C820BASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOC82008_TARDIF = max(0,arr(FLAG_TRTARDIF * C820BASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOC82017_TARDIF = max(0,arr(FLAG_TRTARDIF * C820BASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOC820_TARDIF = somme(x = 07,08,17 : MAJOC820x_TARDIF);
MAJOGLO02_TARDIF = max(0,arr(FLAG_TRTARDIF * GLOBASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                       * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                     );
MAJOGLO03_TARDIF = max(0,arr(FLAG_TRTARDIF * GLOBASE_MAJO * TAUX_2042/100)
                            * STR_TR14
                             * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                           );
MAJOGLO07_TARDIF = max(0,arr(FLAG_TRTARDIF * GLOBASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOGLO08_TARDIF = max(0,arr(FLAG_TRTARDIF * GLOBASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOGLO17_TARDIF = max(0,arr(FLAG_TRTARDIF * GLOBASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOGLO_TARDIF = somme(x = 07,08,17 : MAJOGLOx_TARDIF);

MAJORSE102_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE1BASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                       * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                        );
MAJORSE103_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE1BASE_MAJO * TAUX_2042/100)
                        * STR_TR14
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                       );
MAJORSE107_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE1BASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );

MAJORSE108_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE1BASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE117_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE1BASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );

MAJORSE1_TARDIF = somme(x = 07,08,17 : MAJORSE1x_TARDIF);

MAJORSE202_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE2BASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                       * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                        );
MAJORSE203_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE2BASE_MAJO * TAUX_2042/100)
                        * STR_TR14
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                        );
MAJORSE207_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE2BASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE208_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE2BASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE217_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE2BASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE2_TARDIF = somme(x = 07,08,17 : MAJORSE2x_TARDIF);
MAJORSE302_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE3BASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                      * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                       );
MAJORSE303_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE3BASE_MAJO * TAUX_2042/100)
                        * STR_TR14
                       * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                        );
MAJORSE307_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE3BASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE308_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE3BASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE317_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE3BASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE3_TARDIF = somme(x = 07,08,17 : MAJORSE3x_TARDIF);
MAJORSE402_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE4BASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                     * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                     );
MAJORSE403_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE4BASE_MAJO * TAUX_2042/100)
                       * STR_TR14
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                       );
MAJORSE407_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE4BASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE408_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE4BASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE417_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE4BASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE4_TARDIF = somme(x = 07,08,17 : MAJORSE4x_TARDIF);
MAJORSE502_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE5BASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                       * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                       );
MAJORSE503_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE5BASE_MAJO * TAUX_2042/100)
                        * STR_TR14
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                        );
MAJORSE507_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE5BASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE508_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE5BASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE517_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE5BASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE5_TARDIF = somme(x = 07,08,17 : MAJORSE5x_TARDIF);
MAJORSE602_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE6BASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                     );
MAJORSE603_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE6BASE_MAJO * TAUX_2042/100)
                       * STR_TR14
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                        );
MAJORSE607_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE6BASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE608_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE6BASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE617_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE6BASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE6_TARDIF = somme(x = 07,08,17 : MAJORSE6x_TARDIF);
MAJORSE802_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE8BASE_MAJO * TAUX_2042/100)
                        * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                    * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                   );
MAJORSE803_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE8BASE_MAJO * TAUX_2042/100)
                        * STR_TR14
                        * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                        );
MAJORSE807_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE8BASE_MAJO * TAUX_2042/100)
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE808_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE8BASE_MAJO * TAUX_2042/100)
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE817_TARDIF = max(0,arr(FLAG_TRTARDIF * RSE8BASE_MAJO * TAUX_2042/100)
			* STR_TR15 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );
MAJORSE8_TARDIF = somme(x = 07,08,17 : MAJORSE8x_TARDIF);
MAJOHR02_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                            min(min(HRBASE2042_FIC,HRBASE_MAJO),
                              max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                                  )
                              )
                              * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                              * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                            );
MAJOHR03_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                              min(min(HRBASE2042_FIC,HRBASE_MAJO),
                              max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                              )
                                 )
                         * STR_TR14
                          * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                        );
MAJOHR07_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                            min(min(HRBASE2042_FIC,HRBASE_MAJO),
			        max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                               )
                           )
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOHR08_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                            min(min(HRBASE2042_FIC,HRBASE_MAJO),
			        max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                               )
                           )
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );

MAJOHR17_2TARDIF = max(0,arr(FLAG_TRTARDIF * 2 * TX1758A/100 *
                            min(min(HRBASE2042_FIC,HRBASE_MAJO),
			        max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                               )
                           )

			* STR_TR15
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOHR_TARDIF = somme(x = 02,03,07,08 : MAJOHR0x_TARDIF) + MAJOHR17_2TARDIF ;
MAJOCAP02_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                             min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
                              max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                    )
                               )
                          * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                          * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                      );
MAJOCAP03_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                            min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
                                       max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                     )
                                   )
                           * STR_TR14
                           * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                         );
MAJOCAP07_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                             min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
			         max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                )
                            )
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	               );

MAJOCAP08_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
			     min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
			         max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                )
                             )
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	               );


MAJOCAP17_2TARDIF = max(0,arr(FLAG_TRTARDIF * 2 * TX1758A/100 *
                              min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
			          max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                 )
                             )
			* STR_TR15
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	               );
MAJOCAP_TARDIF = somme(x = 02,03,07,08 : MAJOCAP0x_TARDIF) 
		+ MAJOCAP17_1TARDIF + MAJOCAP17_2TARDIF;

MAJOTAXA02_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                             min(min(TAXABASE2042_FIC,TAXABASE_MAJO),
                                   max(0, IRBASE + TAXABASE_MAJO)
                                   )
                                 )
                            * positif(null(1 -STR_TR18)+null(1 -STR_TR19)+null(1 -STR_TR20)+null(1 -STR_TR21)+null(1 -STR_TR22)+null(1 -STR_TR23))
                           * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                    );
MAJOTAXA03_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
                           min(min(TAXABASE2042_FIC,TAXABASE_MAJO),
                            max(0, IRBASE + TAXABASE_MAJO)
                               )
                            )
                    * STR_TR14
                    * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                     );
MAJOTAXA07_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 * 
                             min(min(TAXABASE2042_FIC,TAXABASE_MAJO),
			         max(0, IRBASE + TAXABASE_MAJO)
                                )
                              )
			* STR_TR17
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	                );

MAJOTAXA08_TARDIF = max(0,arr(FLAG_TRTARDIF * TAUX_2042/100 *
			      min(min(TAXABASE2042_FIC,TAXABASE_MAJO),
                                  max(0, IRBASE + TAXABASE_MAJO)
				 )	   
                              ) 
			* STR_TR12 
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	               );


MAJOTA17_2TARDIF = max(0,arr(FLAG_TRTARDIF * 2 * TX1758A/100 *
                             min(min(TAXABASE2042_FIC,TAXABASE_MAJO),
                                  max(0, IRBASE + TAXABASE_MAJO)
                                )
                            )
			* STR_TR15
			* (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
	              );

MAJOTAXA_TARDIF = somme(x = 02,03,07,08 : MAJOTAXA0x_TARDIF) + MAJOTA17_2TARDIF ;


IRNIN_TARDIF = IRBASE * FLAG_TRTARDIF ;

CSG_TARDIF = CSBASE_MAJO* FLAG_TRTARDIF ;

RDS_TARDIF = RDBASE_MAJO* FLAG_TRTARDIF;

PSOL_TARDIF = PSOLBASE_MAJO* FLAG_TRTARDIF;

CVN_TARDIF = CVNBASE_MAJO * FLAG_TRTARDIF;

CDIS_TARDIF = CDISBASE_MAJO * FLAG_TRTARDIF;

GLO_TARDIF = GLOBASE_MAJO * FLAG_TRTARDIF;

C820_TARDIF = C820BASE_MAJO * FLAG_TRTARDIF;

RSE1_TARDIF = RSE1BASE_MAJO * FLAG_TRTARDIF;

RSE2_TARDIF = RSE2BASE_MAJO * FLAG_TRTARDIF;

RSE3_TARDIF = RSE3BASE_MAJO * FLAG_TRTARDIF;

RSE4_TARDIF = RSE4BASE_MAJO * FLAG_TRTARDIF;

RSE5_TARDIF = RSE5BASE_MAJO * FLAG_TRTARDIF;

RSE6_TARDIF = RSE6BASE_MAJO * FLAG_TRTARDIF;

RSE8_TARDIF = RSE8BASE_MAJO * FLAG_TRTARDIF;

TAXA_TARDIF = TAXABASE_MAJO * FLAG_TRTARDIF;

HR_TARDIF = HRBASE_MAJO * FLAG_TRTARDIF;

CAP_TARDIF = CAPBASE_MAJO * FLAG_TRTARDIF;

regle isf 234:
application : iliad;

MAJOIFI08_TARDIF =  max(0,arr(FLAG_TRTARDIF * IFI4BASE * TAUX_2042/100)
                    * STR_TR12
                    * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                                                                   );


MAJOIFI17_TARDIF =  max(0,arr(FLAG_TRTARDIF * IFI4BASE * TAUX_2042/100)
                    * STR_TR15
                    * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                                                                   );

MAJOIFI07_TARDIF =  max(0,arr(FLAG_TRTARDIF * IFI4BASE * TAUX_2042/100)
                    * STR_TR17
                    * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                                                                   );
MAJOIFI10_TARDIF =  max(0,arr(FLAG_TRTARDIF * IFI4BASE * TAUX_2042/100)
                    * STR_TR16
                    * (1 - null((1 -IND_RJLJ) + (10 - TAUX_2042)))
                                                                   );
                                                                  



MAJOIFI_TARDIF = somme(x =08,10,17,07 : MAJOIFIx_TARDIF);

regle corrective 231141:
application : iliad;
FLAG_TRTARDIF_R = FLAG_RETARD * FLAG_RECTIF * FLAG_1STRATE 
		 * (null(CSTRATE99 - 2) + null(CSTRATE99 - 3) +null(CSTRATE99 - 7) + null(CSTRATE99 - 8) + null(CSTRATE99 - 17) );
MAJOIR02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOIR[00];
MAJOIR03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOIR[00];
MAJOIR07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOIR[00];
MAJOIR08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOIR[00];
MAJOIR17_2TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOIR[00];
MAJOIRTARDIF_R = somme(x = 07,08: MAJOIR0xTARDIF_R) + MAJOIR17_2TARDIF_R ;


MAJOCS02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOCS[00];
MAJOCS03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOCS[00];
MAJOCS07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOCS[00];
MAJOCS08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOCS[00];
MAJOCS17TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOCS[00];
MAJOCSTARDIF_R = somme(x = 07,08,17: MAJOCSxTARDIF_R);
MAJOPSOL02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOPSOL[00];
MAJOPSOL03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOPSOL[00];
MAJOPSOL07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOPSOL[00];
MAJOPSOL08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOPSOL[00];
MAJOPSOL17TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOPSOL[00];
MAJOPSOLTARDIF_R = somme(x = 07,08,17: MAJOPSOLxTARDIF_R);
MAJORD02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJORD[00];
MAJORD03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJORD[00];
MAJORD07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJORD[00];
MAJORD08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJORD[00];
MAJORD17TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJORD[00];
MAJORDTARDIF_R = somme(x = 07,08,17: MAJORDxTARDIF_R);
MAJOCVN02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOCVN[00];
MAJOCVN03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOCVN[00];
MAJOCVN07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOCVN[00];
MAJOCVN08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOCVN[00];
MAJOCVN17TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOCVN[00];

MAJOCVNTARDIF_R = somme(x = 07,08,17: MAJOCVNxTARDIF_R);
MAJOCDIS02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOCDIS[00];
MAJOCDIS03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOCDIS[00];
MAJOCDIS07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOCDIS[00];
MAJOCDIS08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOCDIS[00];
MAJOCDIS17TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOCDIS[00];
MAJOCDISTARDIF_R = somme(x = 07,08,17: MAJOCDISxTARDIF_R);
MAJOGLO02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOGLO[00];
MAJOGLO03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOGLO[00];
MAJOGLO07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOGLO[00];
MAJOGLO08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOGLO[00];
MAJOGLO17TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOGLO[00];
MAJOGLOTARDIF_R = somme(x = 07,08,17: MAJOGLOxTARDIF_R);
MAJOC82002TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOC820[00];
MAJOC82003TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOC820[00];
MAJOC82007TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOC820[00];
MAJOC82008TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOC820[00];
MAJOC82017TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOC820[00];
MAJOC820TARDIF_R = somme(x = 07,08,17: MAJOC820xTARDIF_R);
MAJORSE102TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJORSE1[00];
MAJORSE103TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJORSE1[00];
MAJORSE107TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJORSE1[00];
MAJORSE108TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJORSE1[00];
MAJORSE117TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJORSE1[00];
MAJORSE1TARDIF_R = somme(x = 07,08,17: MAJORSE1xTARDIF_R);
MAJORSE202TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJORSE2[00];
MAJORSE203TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJORSE2[00];
MAJORSE207TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJORSE2[00];
MAJORSE208TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJORSE2[00];
MAJORSE217TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJORSE2[00];
MAJORSE2TARDIF_R = somme(x = 07,08,17: MAJORSE2xTARDIF_R);
MAJORSE302TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJORSE3[00];
MAJORSE303TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJORSE3[00];
MAJORSE307TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJORSE3[00];
MAJORSE308TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJORSE3[00];
MAJORSE317TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJORSE3[00];
MAJORSE3TARDIF_R = somme(x = 07,08,17: MAJORSE3xTARDIF_R);
MAJORSE402TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJORSE4[00];
MAJORSE403TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJORSE4[00];
MAJORSE407TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJORSE4[00];
MAJORSE408TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJORSE4[00];
MAJORSE417TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJORSE4[00];
MAJORSE4TARDIF_R = somme(x = 07,08,17: MAJORSE4xTARDIF_R);
MAJORSE502TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJORSE5[00];
MAJORSE503TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJORSE5[00];
MAJORSE507TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJORSE5[00];
MAJORSE508TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJORSE5[00];
MAJORSE517TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJORSE5[00];
MAJORSE5TARDIF_R = somme(x = 07,08,17: MAJORSE5xTARDIF_R);
MAJORSE602TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJORSE6[00];
MAJORSE603TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJORSE6[00];
MAJORSE607TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJORSE6[00];
MAJORSE608TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJORSE6[00];
MAJORSE617TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJORSE6[00];
MAJORSE6TARDIF_R = somme(x = 07,08,17: MAJORSE6xTARDIF_R);
MAJORSE802TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJORSE8[00];
MAJORSE803TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJORSE8[00];
MAJORSE807TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJORSE8[00];
MAJORSE808TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJORSE8[00];
MAJORSE817TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJORSE8[00];
MAJORSE8TARDIF_R = somme(x = 07,08,17: MAJORSE8xTARDIF_R);
MAJOTAXA02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOTAXA[00];
MAJOTAXA03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOTAXA[00];
MAJOTAXA07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOTAXA[00];
MAJOTAXA08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOTAXA[00];
MAJOTA17_2TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOTAXA[00];
MAJOTAXATARDIF_R = somme(x = 02,03,07,08: MAJOTAXA0xTARDIF_R) + MAJOTA17_2TARDIF_R ;
MAJOHR02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOHR[00];
MAJOHR03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOHR[00];
MAJOHR07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOHR[00];
MAJOHR08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOHR[00];
MAJOHR17_2TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOHR[00];
MAJOHRTARDIF_R = somme(x = 02,03,07,08: MAJOHR0xTARDIF_R) + MAJOHR17_2TARDIF_R ;
MAJOCAP02TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-2) * TMAJOCAP[00];
MAJOCAP03TARDIF_R = FLAG_RETARD * FLAG_RECTIF * null(CSTRATE99-3) * TMAJOCAP[00];
MAJOCAP07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOCAP[00];
MAJOCAP08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOCAP[00];
MAJOCP17_2TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOCAP[00];
MAJOCAPTARDIF_R = somme(x = 02,03,07,08: MAJOCAP0xTARDIF_R) + MAJOCP17_2TARDIF_R ;
regle corrective 231142:
application : iliad;
FLAG_TRTARDIF_F = FLAG_RETARD * positif(FLAG_TRDEGTR);
MAJOIR02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,IRBASE) * TAUX_2042/100);
MAJOIR03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,IRBASE) * TAUX_2042/100);
MAJOIR07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(max(0,IRBASE) * TAUX_2042/100);
MAJOIR08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(max(0,IRBASE) * TAUX_2042/100);
MAJOIR17_2TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(max(0,IRBASE) * 2 * TX1758A/100);

MAJOIRTARDIF_F = somme(x = 02,03,07,08: MAJOIR0xTARDIF_F) + MAJOIR17_2TARDIF_F ;

MAJOIR17_1TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(IRNIN * TAUX_2042/100);

MAJOCS02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,CSBASE_MAJO) * TAUX_2042/100);
MAJOCS03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,CSBASE_MAJO)* TAUX_2042/100);
MAJOCS07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(CSBASE_MAJO * TAUX_2042/100);
MAJOCS08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr((CSBASE_MAJO)* TAUX_2042/100);
MAJOCS17TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr((CSBASE_MAJO)* TAUX_2042/100);
MAJOCSTARDIF_F = somme(x = 02,03,07,08,17: MAJOCSxTARDIF_F);
MAJOPSOL02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,PSOLBASE_MAJO)* TAUX_2042/100);
MAJOPSOL03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,PSOLBASE_MAJO)* TAUX_2042/100);
MAJOPSOL07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(PSOLBASE_MAJO * TAUX_2042/100);
MAJOPSOL08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr((PSOLBASE_MAJO )* TAUX_2042/100);
MAJOPSOL17TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr((PSOLBASE_MAJO)* TAUX_2042/100);
MAJOPSOLTARDIF_F = somme(x = 02,03,07,08,17: MAJOPSOLxTARDIF_F);
MAJORD02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,RDBASE_MAJO)* TAUX_2042/100);
MAJORD03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,RDBASE_MAJO)* TAUX_2042/100);
MAJORD07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(RDBASE_MAJO * TAUX_2042/100);
MAJORD08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr((RDBASE_MAJO )* TAUX_2042/100);
MAJORD17TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr((RDBASE_MAJO)* TAUX_2042/100);
MAJORDTARDIF_F = somme(x = 02,03,07,08,17: MAJORDxTARDIF_F);
MAJOCVN02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,CVNBASE_MAJO )* TAUX_2042/100);
MAJOCVN03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,CVNBASE_MAJO )* TAUX_2042/100);
MAJOCVN07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(CVNBASE_MAJO * TAUX_2042/100);
MAJOCVN08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(CVNBASE_MAJO * TAUX_2042/100);
MAJOCVN17TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(CVNBASE_MAJO * TAUX_2042/100);

MAJOCVNTARDIF_F = somme(x = 02,03,07,08,17: MAJOCVNxTARDIF_F);
MAJOCDIS02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,CDISBASE_MAJO)* TAUX_2042/100);
MAJOCDIS03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,CDISBASE_MAJO)* TAUX_2042/100);
MAJOCDIS07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(CDISBASE_MAJO * TAUX_2042/100);
MAJOCDIS08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(CDISBASE_MAJO * TAUX_2042/100);
MAJOCDIS17TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(CDISBASE_MAJO * TAUX_2042/100);
MAJOCDISTARDIF_F = somme(x = 02,03,07,08,17: MAJOCDISxTARDIF_F);
MAJOGLO02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,GLOBASE_MAJO )* TAUX_2042/100);
MAJOGLO03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,GLOBASE_MAJO )* TAUX_2042/100);
MAJOGLO07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(GLOBASE_MAJO * TAUX_2042/100);
MAJOGLO08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(GLOBASE_MAJO * TAUX_2042/100);
MAJOGLO17TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(GLOBASE_MAJO * TAUX_2042/100);
MAJOGLOTARDIF_F = somme(x = 02,03,07,08,17: MAJOGLOxTARDIF_F);
MAJOC82002TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,C820BASE_MAJO)* TAUX_2042/100);
MAJOC82003TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,C820BASE_MAJO)* TAUX_2042/100);
MAJOC82007TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(C820BASE_MAJO * TAUX_2042/100);
MAJOC82008TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(C820BASE_MAJO * TAUX_2042/100);
MAJOC82017TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(C820BASE_MAJO * TAUX_2042/100);
MAJOC820TARDIF_F = somme(x = 02,03,07,08,17: MAJOC820xTARDIF_F);
MAJORSE102TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,RSE1BASE_MAJO)* TAUX_2042/100);
MAJORSE103TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,RSE1BASE_MAJO)* TAUX_2042/100);
MAJORSE107TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(RSE1BASE_MAJO * TAUX_2042/100);
MAJORSE108TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(RSE1BASE_MAJO * TAUX_2042/100);
MAJORSE117TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(RSE1BASE_MAJO * TAUX_2042/100);
MAJORSE1TARDIF_F = somme(x = 02,03,07,08,17: MAJORSE1xTARDIF_F);
MAJORSE202TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,RSE2BASE_MAJO)* TAUX_2042/100);
MAJORSE203TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,RSE2BASE_MAJO)* TAUX_2042/100);
MAJORSE207TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(RSE2BASE_MAJO * TAUX_2042/100);
MAJORSE208TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(RSE2BASE_MAJO * TAUX_2042/100);
MAJORSE217TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(RSE2BASE_MAJO * TAUX_2042/100);
MAJORSE2TARDIF_F = somme(x = 02,03,07,08,17: MAJORSE2xTARDIF_F);
MAJORSE302TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,RSE3BASE_MAJO)* TAUX_2042/100);
MAJORSE303TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,RSE3BASE_MAJO)* TAUX_2042/100);
MAJORSE307TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(RSE3BASE_MAJO * TAUX_2042/100);
MAJORSE308TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(RSE3BASE_MAJO * TAUX_2042/100);
MAJORSE317TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(RSE3BASE_MAJO * TAUX_2042/100);
MAJORSE3TARDIF_F = somme(x = 02,03,07,08,17: MAJORSE3xTARDIF_F);
MAJORSE402TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,RSE4BASE_MAJO)* TAUX_2042/100);
MAJORSE403TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,RSE4BASE_MAJO)* TAUX_2042/100);
MAJORSE407TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(RSE4BASE_MAJO * TAUX_2042/100);
MAJORSE408TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(RSE4BASE_MAJO * TAUX_2042/100);
MAJORSE417TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(RSE4BASE_MAJO * TAUX_2042/100);
MAJORSE4TARDIF_F = somme(x = 02,03,07,08,17: MAJORSE4xTARDIF_F);
MAJORSE502TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,RSE5BASE_MAJO)* TAUX_2042/100);
MAJORSE503TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,RSE5BASE_MAJO)* TAUX_2042/100);
MAJORSE507TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(RSE5BASE_MAJO * TAUX_2042/100);
MAJORSE508TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(RSE5BASE_MAJO * TAUX_2042/100);
MAJORSE517TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(RSE5BASE_MAJO * TAUX_2042/100);
MAJORSE5TARDIF_F = somme(x = 02,03,07,08,17: MAJORSE5xTARDIF_F);
MAJORSE602TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,RSE6BASE_MAJO)* TAUX_2042/100);
MAJORSE603TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,RSE6BASE_MAJO)* TAUX_2042/100);
MAJORSE607TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(RSE6BASE_MAJO * TAUX_2042/100);
MAJORSE608TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(RSE6BASE_MAJO * TAUX_2042/100);
MAJORSE617TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(RSE6BASE_MAJO * TAUX_2042/100);
MAJORSE6TARDIF_F = somme(x = 02,03,07,08,17: MAJORSE6xTARDIF_F);
MAJORSE802TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) * arr(max(0,RSE8BASE_MAJO)* TAUX_2042/100);
MAJORSE803TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) * arr(max(0,RSE8BASE_MAJO)* TAUX_2042/100);
MAJORSE807TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * arr(RSE8BASE_MAJO * TAUX_2042/100);
MAJORSE808TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(RSE8BASE_MAJO * TAUX_2042/100);
MAJORSE817TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(RSE8BASE_MAJO * TAUX_2042/100);
MAJORSE8TARDIF_F = somme(x = 02,03,07,08,17: MAJORSE8xTARDIF_F);
MAJOTAXA02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) *
                     arr( TAUX_2042/100 * min(min(TAXABASE2042_FIC,TAXABASE_MAJO),
                                               max(0, IRBASE + TAXABASE_MAJO)
                                             )
                             );
MAJOTAXA03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) *
                          arr( TAUX_2042/100 * min(min(TAXABASE2042_FIC,TAXABASE_MAJO),
                                            max(0, IRBASE + TAXABASE_MAJO)
                                             )
                        );
MAJOTAXA07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * 
		     arr( TAUX_2042/100 * min(min(TAXABASE2042_FIC,TAXABASE_MAJO), 
		                           max(0, IRBASE + TAXABASE_MAJO)
                                             )
		        );

MAJOTAXA08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) *
		     arr( TAUX_2042/100 * min(min(TAXABASE2042_FIC,TAXABASE_MAJO), 
		                           max(0, IRBASE + TAXABASE_MAJO)
                                             )
		        );


MAJOTA17_2TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * 
		     arr( 2 * TX1758A/100 * min(min(TAXABASE2042_FIC,TAXABASE_MAJO), 
		                           max(0, IRBASE + TAXABASE_MAJO)
                                           )

                        );

MAJOTAXATARDIF_F = somme(x = 02,03,07,08: MAJOTAXA0xTARDIF_F) + MAJOTA17_2TARDIF_F ;
MAJOHR02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) *
                   arr( TAUX_2042/100 * min(min(HRBASE2042_FIC,HRBASE_MAJO),
                                           max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                                                 )
                        );
MAJOHR03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) *
                  arr( TAUX_2042/100 * min(min(HRBASE2042_FIC,HRBASE_MAJO),
                                          max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                                           )
                       );
MAJOHR07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * 
		   arr( TAUX_2042/100 * min(min(HRBASE2042_FIC,HRBASE_MAJO),
                                            max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                                           )
                      );

MAJOHR08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * 
		   arr( TAUX_2042/100 * min(min(HRBASE2042_FIC,HRBASE_MAJO),
                                            max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                                           )
                      );


MAJOHR17_2TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) *
		   arr( 2 * TX1758A/100 * min(min(HRBASE2042_FIC,HRBASE_MAJO),
                                            max(0, IRBASE+TAXABASE_MAJO+CAPBASE_MAJO+HRBASE_MAJO)
                                           )
                      );

MAJOHRTARDIF_F = somme(x = 02,03,07,08: MAJOHR0xTARDIF_F) + MAJOHR17_2TARDIF_F ;
MAJOCAP02TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 2) *
                    arr( TAUX_2042/100 * min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
                                             max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                          )
                            );
MAJOCAP03TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 3) *
                      arr( TAUX_2042/100 * min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
                                            max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                           )
                        );
MAJOCAP07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 7) * 
		    arr( TAUX_2042/100 * min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
                                             max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                            )
                       );

MAJOCAP08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * 
		    arr( TAUX_2042/100 * min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
                                             max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                            )
                       );


MAJOCP17_2TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * 
		     arr( 2 * TX1758A/100 * min(min(CAPBASE2042_FIC,CAPBASE_MAJO),
                                              max(0, IRBASE + TAXABASE_MAJO + CAPBASE_MAJO)
                                             )
                        );


MAJOCAPTARDIF_F = somme(x = 02,03,07,08: MAJOCAP0xTARDIF_F) + MAJOCP17_2TARDIF_F ;

regle corrective 231143:
application : iliad;
MAJOIR02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOIR02_TARDIF
                    + FLAG_TRTARDIF_R * MAJOIR02TARDIF_R
                   + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOIR02TARDIF_R,MAJOIR02TARDIF_F)
                                           + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOIRTARDIF_A+(MAJOTARDCOA_A-MAJOTARDCOA)*null(CSTRATE99-2)), MAJOIR02TARDIF_F))
                         + FLAG_TRMAJOP * max(0,MAJOIR02TARDIF_A+(MAJOTARDCOA_A-MAJOTARDCOA)*null(CSTRATE99-2))
                     + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                              * (positif(FLAG_RECTIF) * MAJOIR02TARDIF_R
                                + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOIR02TARDIF_A+(MAJOTARDCOA_A-MAJOTARDCOA)*null(CSTRATE99-2)))
                      ) ;
MAJOIR03TARDIF_D = FLAG_RETARD *
                       (FLAG_TRTARDIF * MAJOIR03_TARDIF
                     + FLAG_TRTARDIF_R * MAJOIR03TARDIF_R
                          + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOIR03TARDIF_R,MAJOIR03TARDIF_F)
                                         + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOIRTARDIF_A+(MAJOTARDCOA_A-MAJOTARDCOA)*null(CSTRATE99-3)), MAJOIR03TARDIF_F))
                       + FLAG_TRMAJOP * max(0,MAJOIR03TARDIF_A+(MAJOTARDCOA_A-MAJOTARDCOA)*null(CSTRATE99-3))
                         + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                         * (positif(FLAG_RECTIF) * MAJOIR03TARDIF_R
                           + (1 - positif(FLAG_RECTIF)) * max(0,MAJOIR03TARDIF_A+(MAJOTARDCOA_A-MAJOTARDCOA)*null(CSTRATE99-3)))
                    ) ;
MAJOIR07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOIR07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOIR07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOIR07TARDIF_R,MAJOIR07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOIRTARDIF_A, MAJOIR07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOIR07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOIR07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOIR07TARDIF_A)
		   ) ;
MAJOIR08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOIR08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOIR08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOIR08TARDIF_R,MAJOIR08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOIRTARDIF_A, MAJOIR08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOIR08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOIR08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOIR08TARDIF_A)
		   ) ;


MAJOIR17_2TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOIR17_2TARDIF
		    + FLAG_TRTARDIF_R * MAJOIR17_2TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOIR17_2TARDIF_R,MAJOIR17_2TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOIRTARDIF_A, MAJOIR17_2TARDIF_F)
		    ) 
		    + FLAG_TRMAJOP * MAJOIR17_2TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOIR17_2TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOIR17_2TARDIF_A)
		   ) ;

MAJOCS02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOCS02_TARDIF
            + FLAG_TRTARDIF_R * MAJOCS02TARDIF_R
             + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCS02TARDIF_R,MAJOCS02TARDIF_F)
                                     + (1 - positif(FLAG_RECTIF)) * min(max(0,MAJOCS02TARDIF_A+(MAJOTARDCOB_A-MAJOTARDCOB)*null(CSTRATE99-2)), MAJOCS02TARDIF_F))
                      + FLAG_TRMAJOP * max(0,MAJOCS02TARDIF_A+(MAJOTARDCOB_A-MAJOTARDCOB)*null(CSTRATE99-2))
                 + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                             * (positif(FLAG_RECTIF) * MAJOCS02TARDIF_R
                                + (1 - positif(FLAG_RECTIF)) * max(0,MAJOCS02TARDIF_A+(MAJOTARDCOB_A-MAJOTARDCOB)*null(CSTRATE99-2)))
                   ) ;
MAJOCS03TARDIF_D = FLAG_RETARD *
                       (FLAG_TRTARDIF * MAJOCS03_TARDIF
                  + FLAG_TRTARDIF_R * MAJOCS03TARDIF_R
                          + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCS03TARDIF_R,MAJOCS03TARDIF_F)
                                       + (1 - positif(FLAG_RECTIF)) * min(max(0,MAJOCS03TARDIF_A+(MAJOTARDCOB_A-MAJOTARDCOB)*null(CSTRATE99-3)), MAJOCS03TARDIF_F))
                      + FLAG_TRMAJOP * max(0,MAJOCS03TARDIF_A+(MAJOTARDCOB_A-MAJOTARDCOB)*null(CSTRATE99-3))
                        + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                        * (positif(FLAG_RECTIF) * MAJOCS03TARDIF_R
                            + (1 - positif(FLAG_RECTIF)) * max(0,MAJOCS03TARDIF_A+(MAJOTARDCOB_A-MAJOTARDCOB)*null(CSTRATE99-3)))
                  ) ;
MAJOCS07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCS07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCS07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOCS07TARDIF_R,MAJOCS07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCS07TARDIF_A, MAJOCS07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCS07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCS07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOCS07TARDIF_A)
		   ) ;
MAJOCS08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCS08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCS08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCS08TARDIF_R,MAJOCS08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCS08TARDIF_A, MAJOCS08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCS08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCS08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOCS08TARDIF_A)
		   ) ;
MAJOCS17TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCS17_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCS17TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOCS17TARDIF_R,MAJOCS17TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCS17TARDIF_A, MAJOCS17TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCS17TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCS17TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOCS17TARDIF_A)
		   ) ;
MAJOCSTARDIF_D = somme(x = 07,08,17: MAJOCSxTARDIF_D);
MAJOPSOL02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOPSOL02_TARDIF
                   + FLAG_TRTARDIF_R * MAJOPSOL02TARDIF_R
                 + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOPSOL02TARDIF_R,MAJOPSOL02TARDIF_F)
                                            + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOPSOL02TARDIF_A+(MAJOTARDCOD_A-MAJOTARDCOD)*null(CSTRATE99-2)), MAJOPSOL02TARDIF_F))
                      + FLAG_TRMAJOP * max(0,MAJOPSOL02TARDIF_A+(MAJOTARDCOD_A-MAJOTARDCOD)*null(CSTRATE99-2))
               + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                          * (positif(FLAG_RECTIF) * MAJOPSOL02TARDIF_R
                            + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOPSOL02TARDIF_A+(MAJOTARDCOD_A-MAJOTARDCOD)*null(CSTRATE99-2)))
                 ) ;
MAJOPSOL03TARDIF_D = FLAG_RETARD *
                       (FLAG_TRTARDIF * MAJOPSOL03_TARDIF
                      + FLAG_TRTARDIF_R * MAJOPSOL03TARDIF_R
                        + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOPSOL03TARDIF_R,MAJOPSOL03TARDIF_F)
                                         + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOPSOL03TARDIF_A+(MAJOTARDCOD_A-MAJOTARDCOD)*null(CSTRATE99-3)), MAJOPSOL03TARDIF_F))
                     + FLAG_TRMAJOP * max(0,MAJOPSOL03TARDIF_A+(MAJOTARDCOD_A-MAJOTARDCOD)*null(CSTRATE99-3))
                       + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                        * (positif(FLAG_RECTIF) * MAJOPSOL03TARDIF_R
                        + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOPSOL03TARDIF_A+(MAJOTARDCOD_A-MAJOTARDCOD)*null(CSTRATE99-3)))
                  ) ;
MAJOPSOL07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOPSOL07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOPSOL07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOPSOL07TARDIF_R,MAJOPSOL07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOPSOL07TARDIF_A, MAJOPSOL07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOPSOL07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOPSOL07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOPSOL07TARDIF_A)
		   ) ;
MAJOPSOL08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOPSOL08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOPSOL08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOPSOL08TARDIF_R,MAJOPSOL08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOPSOL08TARDIF_A, MAJOPSOL08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOPSOL08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOPSOL08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOPSOL08TARDIF_A)
		   ) ;
MAJOPSOL17TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOPSOL17_TARDIF
		    + FLAG_TRTARDIF_R * MAJOPSOL17TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOPSOL17TARDIF_R,MAJOPSOL17TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOPSOL17TARDIF_A, MAJOPSOL17TARDIF_F))
		    + FLAG_TRMAJOP * MAJOPSOL17TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOPSOL17TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOPSOL17TARDIF_A)
		   ) ;
MAJOPSOLTARDIF_D = somme(x = 07,08,17: MAJOPSOLxTARDIF_D);
MAJORD02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJORD02_TARDIF
                 + FLAG_TRTARDIF_R * MAJORD02TARDIF_R
               + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORD02TARDIF_R,MAJORD02TARDIF_F)
                                            + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORD02TARDIF_A+(MAJOTARDCOR_A-MAJOTARDCOR)*null(CSTRATE99-2)), MAJORD02TARDIF_F))
                         + FLAG_TRMAJOP * max(0,MAJORD02TARDIF_A+(MAJOTARDCOR_A-MAJOTARDCOR)*null(CSTRATE99-2))
                   + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                           * (positif(FLAG_RECTIF) * MAJORD02TARDIF_R
                               + (1 - positif(FLAG_RECTIF)) * max(0,MAJORD02TARDIF_A+(MAJOTARDCOR_A-MAJOTARDCOR)*null(CSTRATE99-2)))
       ) ;
MAJORD03TARDIF_D = FLAG_RETARD *
                         (FLAG_TRTARDIF * MAJORD03_TARDIF
                    + FLAG_TRTARDIF_R * MAJORD03TARDIF_R
                        + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORD03TARDIF_R,MAJORD03TARDIF_F)
                                    + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORD03TARDIF_A+(MAJOTARDCOR_A-MAJOTARDCOR)*null(CSTRATE99-3)), MAJORD03TARDIF_F))
                      + FLAG_TRMAJOP * max(0,MAJORD03TARDIF_A+(MAJOTARDCOR_A-MAJOTARDCOR)*null(CSTRATE99-3))
                       + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                             * (positif(FLAG_RECTIF) * MAJORD03TARDIF_R
                             + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORD03TARDIF_A+(MAJOTARDCOR_A-MAJOTARDCOR)*null(CSTRATE99-3)))
                ) ;
MAJORD07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORD07_TARDIF
		    + FLAG_TRTARDIF_R * MAJORD07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORD07TARDIF_R,MAJORD07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORD07TARDIF_A, MAJORD07TARDIF_F))
		    + FLAG_TRMAJOP * MAJORD07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORD07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORD07TARDIF_A)
		   ) ;

MAJORD08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORD08_TARDIF
		    + FLAG_TRTARDIF_R * MAJORD08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORD08TARDIF_R,MAJORD08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORD08TARDIF_A, MAJORD08TARDIF_F))
		    + FLAG_TRMAJOP * MAJORD08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORD08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJORD08TARDIF_A)
		   ) ;
MAJORD17TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORD17_TARDIF
		    + FLAG_TRTARDIF_R * MAJORD17TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORD17TARDIF_R,MAJORD17TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORD17TARDIF_A, MAJORD17TARDIF_F))
		    + FLAG_TRMAJOP * MAJORD17TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORD17TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORD17TARDIF_A)
		   ) ;
MAJORDTARDIF_D = somme(x = 07,08,17: MAJORDxTARDIF_D);
MAJOCVN02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOCVN02_TARDIF
                + FLAG_TRTARDIF_R * MAJOCVN02TARDIF_R
               + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCVN02TARDIF_R,MAJOCVN02TARDIF_F)
                                     + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOCVN02TARDIF_A+(MAJOTARDCOE_A-MAJOTARDCOE)*null(CSTRATE99-2)), MAJOCVN02TARDIF_F))
                   + FLAG_TRMAJOP * max(0,MAJOCVN02TARDIF_A+(MAJOTARDCOE_A-MAJOTARDCOE)*null(CSTRATE99-2))
                   + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                               * (positif(FLAG_RECTIF) * MAJOCVN02TARDIF_R
                             + (1 - positif(FLAG_RECTIF)) * max(0,MAJOCVN02TARDIF_A+(MAJOTARDCOE_A-MAJOTARDCOE)*null(CSTRATE99-2)))
                  ) ;
MAJOCVN03TARDIF_D = FLAG_RETARD *
                        (FLAG_TRTARDIF * MAJOCVN03_TARDIF
                   + FLAG_TRTARDIF_R * MAJOCVN03TARDIF_R
                        + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCVN03TARDIF_R,MAJOCVN03TARDIF_F)
                                       + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOCVN03TARDIF_A+(MAJOTARDCOE_A-MAJOTARDCOE)*null(CSTRATE99-3)), MAJOCVN03TARDIF_F))
                    + FLAG_TRMAJOP * max(0,MAJOCVN03TARDIF_A+(MAJOTARDCOE_A-MAJOTARDCOE)*null(CSTRATE99-3))
                      + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                             * (positif(FLAG_RECTIF) * MAJOCVN03TARDIF_R
                             + (1 - positif(FLAG_RECTIF)) * max(0,MAJOCVN03TARDIF_A+(MAJOTARDCOE_A-MAJOTARDCOE)*null(CSTRATE99-3)))
                  ) ;
MAJOCVN07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCVN07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCVN07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOCVN07TARDIF_R,MAJOCVN07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCVN07TARDIF_A, MAJOCVN07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCVN07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCVN07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOCVN07TARDIF_A)
		   ) ;
MAJOCVN08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCVN08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCVN08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCVN08TARDIF_R,MAJOCVN08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCVN08TARDIF_A, MAJOCVN08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCVN08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCVN08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOCVN08TARDIF_A)
		   ) ;
MAJOCVN17TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCVN17_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCVN17TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOCVN17TARDIF_R,MAJOCVN17TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCVN17TARDIF_A, MAJOCVN17TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCVN17TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCVN17TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOCVN17TARDIF_A)
		   ) ;
MAJOCVNTARDIF_D = somme(x = 07,08,17: MAJOCVNxTARDIF_D);
MAJOCDIS02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOCDIS02_TARDIF
                  + FLAG_TRTARDIF_R * MAJOCDIS02TARDIF_R
                 + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCDIS02TARDIF_R,MAJOCDIS02TARDIF_F)
                                       + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOCDIS02TARDIF_A+(MAJOTARDCOF_A-MAJOTARDCOF)*null(CSTRATE99-2)), MAJOCDIS02TARDIF_F))
                         + FLAG_TRMAJOP * max(0,MAJOCDIS02TARDIF_A+(MAJOTARDCOF_A-MAJOTARDCOF)*null(CSTRATE99-2))
                 + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                         * (positif(FLAG_RECTIF) * MAJOCDIS02TARDIF_R
                            + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOCDIS02TARDIF_A+(MAJOTARDCOF_A-MAJOTARDCOF)*null(CSTRATE99-2)))
                 ) ;
MAJOCDIS03TARDIF_D = FLAG_RETARD *
                       (FLAG_TRTARDIF * MAJOCDIS03_TARDIF
                     + FLAG_TRTARDIF_R * MAJOCDIS03TARDIF_R
                     + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCDIS03TARDIF_R,MAJOCDIS03TARDIF_F)
                                      + (1 - positif(FLAG_RECTIF)) * min(max(0,MAJOCDIS03TARDIF_A+(MAJOTARDCOF_A-MAJOTARDCOF)*null(CSTRATE99-3)), MAJOCDIS03TARDIF_F))
                    + FLAG_TRMAJOP * max(0,MAJOCDIS03TARDIF_A+(MAJOTARDCOF_A-MAJOTARDCOF)*null(CSTRATE99-3))
                         + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                           * (positif(FLAG_RECTIF) * MAJOCDIS03TARDIF_R
                              + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOCDIS03TARDIF_A+(MAJOTARDCOF_A-MAJOTARDCOF)*null(CSTRATE99-3)))
                  ) ;
MAJOCDIS07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCDIS07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCDIS07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOCDIS07TARDIF_R,MAJOCDIS07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCDIS07TARDIF_A, MAJOCDIS07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCDIS07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCDIS07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOCDIS07TARDIF_A)
		   ) ;
MAJOCDIS08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCDIS08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCDIS08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCDIS08TARDIF_R,MAJOCDIS08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCDIS08TARDIF_A, MAJOCDIS08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCDIS08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCDIS08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOCDIS08TARDIF_A)
		   ) ;
MAJOCDIS17TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCDIS17_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCDIS17TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOCDIS17TARDIF_R,MAJOCDIS17TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCDIS17TARDIF_A, MAJOCDIS17TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCDIS17TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCDIS17TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOCDIS17TARDIF_A)
		   ) ;
MAJOCDISTARDIF_D = somme(x = 07,08,17: MAJOCDISxTARDIF_D);
MAJOGLO02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOGLO02_TARDIF
             + FLAG_TRTARDIF_R * MAJOGLO02TARDIF_R
                  + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOGLO02TARDIF_R,MAJOGLO02TARDIF_F)
                                             + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOGLO02TARDIF_A+(MAJOTARDCOG_A-MAJOTARDCOG)*null(CSTRATE99-2)), MAJOGLO02TARDIF_F))
                         + FLAG_TRMAJOP * max(0,MAJOGLO02TARDIF_A+(MAJOTARDCOG_A-MAJOTARDCOG)*null(CSTRATE99-2))
                  + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                          * (positif(FLAG_RECTIF) * MAJOGLO02TARDIF_R
                             + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOGLO02TARDIF_A+(MAJOTARDCOG_A-MAJOTARDCOG)*null(CSTRATE99-2)))
                 ) ;
MAJOGLO03TARDIF_D = FLAG_RETARD *
                      (FLAG_TRTARDIF * MAJOGLO03_TARDIF
                  + FLAG_TRTARDIF_R * MAJOGLO03TARDIF_R
                         + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOGLO03TARDIF_R,MAJOGLO03TARDIF_F)
                                       + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOGLO03TARDIF_A+(MAJOTARDCOG_A-MAJOTARDCOG)*null(CSTRATE99-3)), MAJOGLO03TARDIF_F))
                     + FLAG_TRMAJOP * max(0,MAJOGLO03TARDIF_A+(MAJOTARDCOG_A-MAJOTARDCOG)*null(CSTRATE99-3))
                       + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                       * (positif(FLAG_RECTIF) * MAJOGLO03TARDIF_R
                             + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOGLO03TARDIF_A+(MAJOTARDCOG_A-MAJOTARDCOG)*null(CSTRATE99-3)))
                 ) ;
MAJOGLO07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOGLO07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOGLO07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOGLO07TARDIF_R,MAJOGLO07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOGLO07TARDIF_A, MAJOGLO07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOGLO07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOGLO07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOGLO07TARDIF_A)
		   ) ;
MAJOGLO08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOGLO08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOGLO08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOGLO08TARDIF_R,MAJOGLO08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOGLO08TARDIF_A, MAJOGLO08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOGLO08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOGLO08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOGLO08TARDIF_A)
		   ) ;
MAJOGLO17TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOGLO17_TARDIF
		    + FLAG_TRTARDIF_R * MAJOGLO17TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOGLO17TARDIF_R,MAJOGLO17TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOGLO17TARDIF_A, MAJOGLO17TARDIF_F))
		    + FLAG_TRMAJOP * MAJOGLO17TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOGLO17TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOGLO17TARDIF_A)
		   ) ;
MAJOGLOTARDIF_D = somme(x = 07,08,17: MAJOGLOxTARDIF_D);
MAJOC82002TARDIF_D = FLAG_RETARD *
                   (FLAG_TRTARDIF * MAJOC82002_TARDIF
                 + FLAG_TRTARDIF_R * MAJOC82002TARDIF_R
                    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOC82002TARDIF_R,MAJOC82002TARDIF_F)
                                            + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOC82002TARDIF_A+(MAJOTARDCOQ_A-MAJOTARDCOQ)*null(CSTRATE99-2)), MAJOC82002TARDIF_F))
                         + FLAG_TRMAJOP * max(0,MAJOC82002TARDIF_A+(MAJOTARDCOQ_A-MAJOTARDCOQ)*null(CSTRATE99-2))
                    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                          * (positif(FLAG_RECTIF) * MAJOC82002TARDIF_R
                           + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOC82002TARDIF_A+(MAJOTARDCOQ_A-MAJOTARDCOQ)*null(CSTRATE99-2)))
                 ) ;
MAJOC82003TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOC82003_TARDIF
                    + FLAG_TRTARDIF_R * MAJOC82003TARDIF_R
                        + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOC82003TARDIF_R,MAJOC82003TARDIF_F)
                                       + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOC82003TARDIF_A+(MAJOTARDCOQ_A-MAJOTARDCOQ)*null(CSTRATE99-3)), MAJOC82003TARDIF_F))
            + FLAG_TRMAJOP * max(0,MAJOC82003TARDIF_A+(MAJOTARDCOQ_A-MAJOTARDCOQ)*null(CSTRATE99-3))
                        + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                             * (positif(FLAG_RECTIF) * MAJOC82003TARDIF_R
                             + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOC82003TARDIF_A+(MAJOTARDCOQ_A-MAJOTARDCOQ)*null(CSTRATE99-3)))
               ) ;
MAJOC82007TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOC82007_TARDIF
		    + FLAG_TRTARDIF_R * MAJOC82007TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOC82007TARDIF_R,MAJOC82007TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOC82007TARDIF_A, MAJOC82007TARDIF_F))
		    + FLAG_TRMAJOP * MAJOC82007TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOC82007TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOC82007TARDIF_A)
		   ) ;
MAJOC82008TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOC82008_TARDIF
		    + FLAG_TRTARDIF_R * MAJOC82008TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOC82008TARDIF_R,MAJOC82008TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOC82008TARDIF_A, MAJOC82008TARDIF_F))
		    + FLAG_TRMAJOP * MAJOC82008TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOC82008TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOC82008TARDIF_A)
		   ) ;
MAJOC82017TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOC82017_TARDIF
		    + FLAG_TRTARDIF_R * MAJOC82017TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOC82017TARDIF_R,MAJOC82017TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOC82017TARDIF_A, MAJOC82017TARDIF_F))
		    + FLAG_TRMAJOP * MAJOC82017TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOC82017TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOC82017TARDIF_A)
		   ) ;
MAJOC820TARDIF_D = somme(x = 07,08,17: MAJOC820xTARDIF_D);
MAJORSE102TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJORSE102_TARDIF
                  + FLAG_TRTARDIF_R * MAJORSE102TARDIF_R
               + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE102TARDIF_R,MAJORSE102TARDIF_F)
                                        + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE102TARDIF_A+(MAJOTARDCOT_A-MAJOTARDCOT)*null(CSTRATE99-2)), MAJORSE102TARDIF_F))
                       + FLAG_TRMAJOP * max(0,MAJORSE102TARDIF_A+(MAJOTARDCOT_A-MAJOTARDCOT)*null(CSTRATE99-2))
                    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                               * (positif(FLAG_RECTIF) * MAJORSE102TARDIF_R
                             + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE102TARDIF_A+(MAJOTARDCOT_A-MAJOTARDCOT)*null(CSTRATE99-2)))
           ) ;
MAJORSE103TARDIF_D = FLAG_RETARD *
                      (FLAG_TRTARDIF * MAJORSE103_TARDIF
                 + FLAG_TRTARDIF_R * MAJORSE103TARDIF_R
                          + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE103TARDIF_R,MAJORSE103TARDIF_F)
                                       + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE103TARDIF_A+(MAJOTARDCOT_A-MAJOTARDCOT)*null(CSTRATE99-3)), MAJORSE103TARDIF_F))
                     + FLAG_TRMAJOP * max(0,MAJORSE103TARDIF_A+(MAJOTARDCOT_A-MAJOTARDCOT)*null(CSTRATE99-3))
                        + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                         * (positif(FLAG_RECTIF) * MAJORSE103TARDIF_R
                               + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE103TARDIF_A+(MAJOTARDCOT_A-MAJOTARDCOT)*null(CSTRATE99-3)))
                 ) ;
MAJORSE107TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE107_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE107TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE107TARDIF_R,MAJORSE107TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE107TARDIF_A, MAJORSE107TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE107TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE107TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE107TARDIF_A)
		   ) ;
MAJORSE108TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE108_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE108TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE108TARDIF_R,MAJORSE108TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE108TARDIF_A, MAJORSE108TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE108TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE108TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJORSE108TARDIF_A)
		   ) ;
MAJORSE117TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE117_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE117TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE117TARDIF_R,MAJORSE117TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE117TARDIF_A, MAJORSE117TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE117TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE117TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE117TARDIF_A)
		   ) ;
MAJORSE1TARDIF_D = somme(x = 07,08,17: MAJORSE1xTARDIF_D);
MAJORSE202TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJORSE202_TARDIF
                  + FLAG_TRTARDIF_R * MAJORSE202TARDIF_R
                 + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE202TARDIF_R,MAJORSE202TARDIF_F)
                                            + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE202TARDIF_A+(MAJOTARDCOL_A-MAJOTARDCOL)*null(CSTRATE99-2)), MAJORSE202TARDIF_F))
                      + FLAG_TRMAJOP * max(0,MAJORSE202TARDIF_A+(MAJOTARDCOL_A-MAJOTARDCOL)*null(CSTRATE99-2))
                    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                        * (positif(FLAG_RECTIF)  * MAJORSE202TARDIF_R
                             + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE202TARDIF_A+(MAJOTARDCOL_A-MAJOTARDCOL)*null(CSTRATE99-2)))
                ) ;
MAJORSE203TARDIF_D = FLAG_RETARD *
                       (FLAG_TRTARDIF * MAJORSE203_TARDIF
                  + FLAG_TRTARDIF_R * MAJORSE203TARDIF_R
                        + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * min(MAJORSE203TARDIF_R,MAJORSE203TARDIF_F)
                                    + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE203TARDIF_A+(MAJOTARDCOL_A-MAJOTARDCOL)*null(CSTRATE99-3)), MAJORSE203TARDIF_F))
                 + FLAG_TRMAJOP * max(0,MAJORSE203TARDIF_A+(MAJOTARDCOL_A-MAJOTARDCOL)*null(CSTRATE99-3))
                     + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                          * (positif(FLAG_RECTIF)  * MAJORSE203TARDIF_R
                              + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE203TARDIF_A+(MAJOTARDCOL_A-MAJOTARDCOL)*null(CSTRATE99-3)))
                   ) ;
MAJORSE207TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE207_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE207TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE207TARDIF_R,MAJORSE207TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE207TARDIF_A, MAJORSE207TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE207TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE207TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE207TARDIF_A)
		   ) ;
MAJORSE208TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE208_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE208TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE208TARDIF_R,MAJORSE208TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE208TARDIF_A, MAJORSE208TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE208TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE208TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJORSE208TARDIF_A)
		   ) ;
MAJORSE217TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE217_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE217TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE217TARDIF_R,MAJORSE217TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE217TARDIF_A, MAJORSE217TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE217TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE217TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE217TARDIF_A)
		   ) ;
MAJORSE2TARDIF_D = somme(x = 07,08,17: MAJORSE2xTARDIF_D);
MAJORSE302TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJORSE302_TARDIF
                  + FLAG_TRTARDIF_R * MAJORSE302TARDIF_R
                 + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE302TARDIF_R,MAJORSE302TARDIF_F)
                                         + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE302TARDIF_A+(MAJOTARDCOM_A-MAJOTARDCOM)*null(CSTRATE99-2)), MAJORSE302TARDIF_F))
                       + FLAG_TRMAJOP * max(0,MAJORSE302TARDIF_A+(MAJOTARDCOM_A-MAJOTARDCOM)*null(CSTRATE99-2))
                    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                           * (positif(FLAG_RECTIF)  * MAJORSE302TARDIF_R
                            + (1 - positif(FLAG_RECTIF)  ) * max(0,MAJORSE302TARDIF_A+(MAJOTARDCOM_A-MAJOTARDCOM)*null(CSTRATE99-2)))
                ) ;
MAJORSE303TARDIF_D = FLAG_RETARD *
                      (FLAG_TRTARDIF * MAJORSE303_TARDIF
                   + FLAG_TRTARDIF_R * MAJORSE303TARDIF_R
                     + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)   * min(MAJORSE303TARDIF_R,MAJORSE303TARDIF_F)
                                   + (1 - positif(FLAG_RECTIF)  ) * min(max(0,MAJORSE303TARDIF_A+(MAJOTARDCOM_A-MAJOTARDCOM)*null(CSTRATE99-3)), MAJORSE303TARDIF_F))
                  + FLAG_TRMAJOP * max(0,MAJORSE303TARDIF_A+(MAJOTARDCOM_A-MAJOTARDCOM)*null(CSTRATE99-3))
                        + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                           * (positif(FLAG_RECTIF)  * MAJORSE303TARDIF_R
                            + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE303TARDIF_A+(MAJOTARDCOM_A-MAJOTARDCOM)*null(CSTRATE99-3)))
                 ) ;
MAJORSE307TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE307_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE307TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE307TARDIF_R,MAJORSE307TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE307TARDIF_A, MAJORSE307TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE307TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE307TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE307TARDIF_A)
		   ) ;
MAJORSE308TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE308_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE308TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE308TARDIF_R,MAJORSE308TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE308TARDIF_A, MAJORSE308TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE308TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE308TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJORSE308TARDIF_A)
		   ) ;
MAJORSE317TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE317_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE317TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE317TARDIF_R,MAJORSE317TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE317TARDIF_A, MAJORSE317TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE317TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE317TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE317TARDIF_A)
		   ) ;
MAJORSE3TARDIF_D = somme(x = 07,08,17: MAJORSE3xTARDIF_D);
MAJORSE402TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJORSE402_TARDIF
                     + FLAG_TRTARDIF_R * MAJORSE402TARDIF_R
                + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE402TARDIF_R,MAJORSE402TARDIF_F)
                                           + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE402TARDIF_A+(MAJOTARDCOO_A-MAJOTARDCOO)*null(CSTRATE99-2)), MAJORSE402TARDIF_F))
                         + FLAG_TRMAJOP * max(0,MAJORSE402TARDIF_A+(MAJOTARDCOO_A-MAJOTARDCOO)*null(CSTRATE99-2))
                   + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                               * (positif(FLAG_RECTIF) * MAJORSE402TARDIF_R
                            + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE402TARDIF_A+(MAJOTARDCOO_A-MAJOTARDCOO)*null(CSTRATE99-2)))
                 ) ;
MAJORSE403TARDIF_D = FLAG_RETARD *
                   (FLAG_TRTARDIF * MAJORSE403_TARDIF
                   + FLAG_TRTARDIF_R * MAJORSE403TARDIF_R
                       + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE403TARDIF_R,MAJORSE403TARDIF_F)
                                     + (1 - positif(FLAG_RECTIF)) * min(max(0,MAJORSE403TARDIF_A+(MAJOTARDCOO_A-MAJOTARDCOO)*null(CSTRATE99-3)), MAJORSE403TARDIF_F))
                   + FLAG_TRMAJOP * max(0,MAJORSE403TARDIF_A+(MAJOTARDCOO_A-MAJOTARDCOO)*null(CSTRATE99-3))
                    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                       * (positif(FLAG_RECTIF) * MAJORSE403TARDIF_R
                           + (1 - positif(FLAG_RECTIF)) * max(0,MAJORSE403TARDIF_A+(MAJOTARDCOO_A-MAJOTARDCOO)*null(CSTRATE99-3)))
                 ) ;
MAJORSE407TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE407_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE407TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE407TARDIF_R,MAJORSE407TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE407TARDIF_A, MAJORSE407TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE407TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE407TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE407TARDIF_A)
		   ) ;
MAJORSE408TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE408_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE408TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE408TARDIF_R,MAJORSE408TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE408TARDIF_A, MAJORSE408TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE408TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE408TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJORSE408TARDIF_A)
		   ) ;
MAJORSE417TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE417_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE417TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE417TARDIF_R,MAJORSE417TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE417TARDIF_A, MAJORSE417TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE417TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE417TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE417TARDIF_A)
		   ) ;
MAJORSE4TARDIF_D = somme(x = 07,08,17: MAJORSE4xTARDIF_D);
MAJORSE502TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJORSE502_TARDIF
                  + FLAG_TRTARDIF_R * MAJORSE502TARDIF_R
                + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE502TARDIF_R,MAJORSE502TARDIF_F)
                                          + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE502TARDIF_A+(MAJOTARDCOJ_A-MAJOTARDCOJ)*null(CSTRATE99-2)), MAJORSE502TARDIF_F))
                       + FLAG_TRMAJOP * max(0,MAJORSE502TARDIF_A+(MAJOTARDCOJ_A-MAJOTARDCOJ)*null(CSTRATE99-2))
                  + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                           * (positif(FLAG_RECTIF) * MAJORSE502TARDIF_R
                            + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE502TARDIF_A+(MAJOTARDCOJ_A-MAJOTARDCOJ)*null(CSTRATE99-2)))
              ) ;
MAJORSE503TARDIF_D = FLAG_RETARD *
                   (FLAG_TRTARDIF * MAJORSE503_TARDIF
                     + FLAG_TRTARDIF_R * MAJORSE503TARDIF_R
                       + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE503TARDIF_R,MAJORSE503TARDIF_F)
                                       + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE503TARDIF_A+(MAJOTARDCOJ_A-MAJOTARDCOJ)*null(CSTRATE99-3)), MAJORSE503TARDIF_F))
            + FLAG_TRMAJOP * max(0,MAJORSE503TARDIF_A+(MAJOTARDCOJ_A-MAJOTARDCOJ)*null(CSTRATE99-3))
                      + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                    * (positif(FLAG_RECTIF) * MAJORSE503TARDIF_R
                          + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE503TARDIF_A+(MAJOTARDCOJ_A-MAJOTARDCOJ)*null(CSTRATE99-3)))
               ) ;
MAJORSE507TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE507_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE507TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE507TARDIF_R,MAJORSE507TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE507TARDIF_A, MAJORSE507TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE507TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE507TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE507TARDIF_A)
		   ) ;
MAJORSE508TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE508_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE508TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE508TARDIF_R,MAJORSE508TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE508TARDIF_A, MAJORSE508TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE508TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE508TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJORSE508TARDIF_A)
		   ) ;
MAJORSE517TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE517_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE517TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE517TARDIF_R,MAJORSE517TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE517TARDIF_A, MAJORSE517TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE517TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE517TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE517TARDIF_A)
		   ) ;
MAJORSE5TARDIF_D = somme(x = 07,08,17: MAJORSE5xTARDIF_D);
MAJORSE602TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJORSE602_TARDIF
                       + FLAG_TRTARDIF_R * MAJORSE602TARDIF_R
                   + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE602TARDIF_R,MAJORSE602TARDIF_F)
                                             + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE602TARDIF_A+(MAJOTARDCOP_A-MAJOTARDCOP)*null(CSTRATE99-2)), MAJORSE602TARDIF_F))
                         + FLAG_TRMAJOP * max(0,MAJORSE602TARDIF_A+(MAJOTARDCOP_A-MAJOTARDCOP)*null(CSTRATE99-2))
                 + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                         * (positif(FLAG_RECTIF) * MAJORSE602TARDIF_R
                           + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE602TARDIF_A+(MAJOTARDCOP_A-MAJOTARDCOP)*null(CSTRATE99-2)))
                   ) ;
MAJORSE603TARDIF_D = FLAG_RETARD *
                     (FLAG_TRTARDIF * MAJORSE603_TARDIF
                    + FLAG_TRTARDIF_R * MAJORSE603TARDIF_R
                      + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE603TARDIF_R,MAJORSE603TARDIF_F)
                                   + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE603TARDIF_A+(MAJOTARDCOP_A-MAJOTARDCOP)*null(CSTRATE99-3)), MAJORSE603TARDIF_F))
                + FLAG_TRMAJOP * max(0,MAJORSE603TARDIF_A+(MAJOTARDCOP_A-MAJOTARDCOP)*null(CSTRATE99-3))
                     + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                          * (positif(FLAG_RECTIF) * MAJORSE603TARDIF_R
                           + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE603TARDIF_A+(MAJOTARDCOP_A-MAJOTARDCOP)*null(CSTRATE99-3)))
                  ) ;
MAJORSE607TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE607_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE607TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE607TARDIF_R,MAJORSE607TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE607TARDIF_A, MAJORSE607TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE607TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE607TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE607TARDIF_A)
		   ) ;
MAJORSE608TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE608_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE608TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE608TARDIF_R,MAJORSE608TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE608TARDIF_A, MAJORSE608TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE608TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE608TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJORSE608TARDIF_A)
		   ) ;
MAJORSE617TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE617_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE617TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE617TARDIF_R,MAJORSE617TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE617TARDIF_A, MAJORSE617TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE617TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE617TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE617TARDIF_A)
		   ) ;
MAJORSE6TARDIF_D = somme(x = 07,08,17: MAJORSE6xTARDIF_D);
MAJORSE802TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJORSE802_TARDIF
                      + FLAG_TRTARDIF_R * MAJORSE802TARDIF_R
                + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE802TARDIF_R,MAJORSE802TARDIF_F)
                                         + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE802TARDIF_A+(MAJOTARDCOH_A-MAJOTARDCOH)*null(CSTRATE99-2)), MAJORSE802TARDIF_F))
                        + FLAG_TRMAJOP * max(0,MAJORSE802TARDIF_A+(MAJOTARDCOH_A-MAJOTARDCOH)*null(CSTRATE99-2))
               + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                         * (positif(FLAG_RECTIF) * MAJORSE802TARDIF_R
                             + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE802TARDIF_A+(MAJOTARDCOH_A-MAJOTARDCOH)*null(CSTRATE99-2)))
                 ) ;
MAJORSE803TARDIF_D = FLAG_RETARD *
                      (FLAG_TRTARDIF * MAJORSE803_TARDIF
                      + FLAG_TRTARDIF_R * MAJORSE803TARDIF_R
                      + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE803TARDIF_R,MAJORSE803TARDIF_F)
                                     + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJORSE803TARDIF_A+(MAJOTARDCOH_A-MAJOTARDCOH)*null(CSTRATE99-3)), MAJORSE803TARDIF_F))
               + FLAG_TRMAJOP * max(0,MAJORSE803TARDIF_A+(MAJOTARDCOH_A-MAJOTARDCOH)*null(CSTRATE99-3))
                    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
               * (positif(FLAG_RECTIF) * MAJORSE803TARDIF_R
                        + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE803TARDIF_A+(MAJOTARDCOH_A-MAJOTARDCOH)*null(CSTRATE99-3)))
              ) ;
MAJORSE807TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE807_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE807TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE807TARDIF_R,MAJORSE807TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE807TARDIF_A, MAJORSE807TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE807TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE807TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE807TARDIF_A)
		   ) ;
MAJORSE808TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE808_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE808TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJORSE808TARDIF_R,MAJORSE808TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE808TARDIF_A, MAJORSE808TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE808TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE808TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJORSE808TARDIF_A)
		   ) ;
MAJORSE817TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJORSE817_TARDIF
		    + FLAG_TRTARDIF_R * MAJORSE817TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJORSE817TARDIF_R,MAJORSE817TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJORSE817TARDIF_A, MAJORSE817TARDIF_F))
		    + FLAG_TRMAJOP * MAJORSE817TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJORSE817TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJORSE817TARDIF_A)
		   ) ;
MAJORSE8TARDIF_D = somme(x = 07,08,17: MAJORSE8xTARDIF_D);
MAJOTAXA02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOTAXA02_TARDIF
                       + FLAG_TRTARDIF_R * MAJOTAXA02TARDIF_R
                  + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOTAXA02TARDIF_R,MAJOTAXA02TARDIF_F)
                                           + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOTAXA02TARDIF_A+(MAJOTARDCOU_A-MAJOTARDCOU)*null(CSTRATE99-2)), MAJOTAXA02TARDIF_F))
                       + FLAG_TRMAJOP * max(0,MAJOTAXA02TARDIF_A+(MAJOTARDCOU_A-MAJOTARDCOU)*null(CSTRATE99-2))
                  + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                             * (positif(FLAG_RECTIF) * MAJOTAXA02TARDIF_R
                            + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOTAXA02TARDIF_A+(MAJOTARDCOU_A-MAJOTARDCOU)*null(CSTRATE99-2)))
               ) ;
MAJOTAXA03TARDIF_D = FLAG_RETARD *
                     (FLAG_TRTARDIF * MAJOTAXA03_TARDIF
                   + FLAG_TRTARDIF_R * MAJOTAXA03TARDIF_R
                    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOTAXA03TARDIF_R,MAJOTAXA03TARDIF_F)
                                  + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOTAXA03TARDIF_A+(MAJOTARDCOU_A-MAJOTARDCOU)*null(CSTRATE99-3)), MAJOTAXA03TARDIF_F))
               + FLAG_TRMAJOP * max(0,MAJOTAXA03TARDIF_A+(MAJOTARDCOU_A-MAJOTARDCOU)*null(CSTRATE99-3))
                     + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                     * (positif(FLAG_RECTIF) * MAJOTAXA03TARDIF_R
                        + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOTAXA03TARDIF_A+(MAJOTARDCOU_A-MAJOTARDCOU)*null(CSTRATE99-3)))
                   ) ;
MAJOTAXA07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOTAXA07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOTAXA07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOTAXA07TARDIF_R,MAJOTAXA07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOTAXA07TARDIF_A, MAJOTAXA07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOTAXA07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOTAXA07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOTAXA07TARDIF_A)
		   ) ;
MAJOTAXA08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOTAXA08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOTAXA08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOTAXA08TARDIF_R,MAJOTAXA08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOTAXA08TARDIF_A, MAJOTAXA08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOTAXA08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOTAXA08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOTAXA08TARDIF_A)
		   ) ;


MAJOTA17_2TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOTA17_2TARDIF
		    + FLAG_TRTARDIF_R * MAJOTA17_2TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOTA17_2TARDIF_R,MAJOTA17_2TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOTA17_2TARDIF_A, MAJOTA17_2TARDIF_F))
		    + FLAG_TRMAJOP * MAJOTA17_2TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOTA17_2TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOTA17_2TARDIF_A)
		   ) ;

MAJOTAXATARDIF_D = somme(x = 07,08: MAJOTAXA0xTARDIF_D) + MAJOTA17_2TARDIF_D ;
MAJOHR02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOHR02_TARDIF
                    + FLAG_TRTARDIF_R * MAJOHR02TARDIF_R
              + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOHR02TARDIF_R,MAJOHR02TARDIF_F)
                                   + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOHR02TARDIF_A+(MAJOTARDCOX_A-MAJOTARDCOX)*null(CSTRATE99-2)), MAJOHR02TARDIF_F))
                 + FLAG_TRMAJOP * max(0,MAJOHR02TARDIF_A+(MAJOTARDCOX_A-MAJOTARDCOX)*null(CSTRATE99-2))
               + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                          * (positif(FLAG_RECTIF) * MAJOHR02TARDIF_R
                          + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOHR02TARDIF_A+(MAJOTARDCOX_A-MAJOTARDCOX)*null(CSTRATE99-2)))
               ) ;
MAJOHR03TARDIF_D = FLAG_RETARD *
                     (FLAG_TRTARDIF * MAJOHR03_TARDIF
                   + FLAG_TRTARDIF_R * MAJOHR03TARDIF_R
                       + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOHR03TARDIF_R,MAJOHR03TARDIF_F)
                                      + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOHR03TARDIF_A+(MAJOTARDCOX_A-MAJOTARDCOX)*null(CSTRATE99-3)), MAJOHR03TARDIF_F))
        + FLAG_TRMAJOP * max(0,MAJOHR03TARDIF_A+(MAJOTARDCOX_A-MAJOTARDCOX)*null(CSTRATE99-3))
                     + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                     * (positif(FLAG_RECTIF) * MAJOHR03TARDIF_R
                   + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOHR03TARDIF_A+(MAJOTARDCOX_A-MAJOTARDCOX)*null(CSTRATE99-3)))
              ) ;
MAJOHR07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOHR07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOHR07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOHR07TARDIF_R,MAJOHR07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOHR07TARDIF_A, MAJOHR07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOHR07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOHR07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOHR07TARDIF_A)
		   ) ;
MAJOHR08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOHR08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOHR08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOHR08TARDIF_R,MAJOHR08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOHR08TARDIF_A, MAJOHR08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOHR08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOHR08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOHR08TARDIF_A)
		   ) ;

MAJOHR17_2TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOHR17_2TARDIF
		    + FLAG_TRTARDIF_R * MAJOHR17_2TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOHR17_2TARDIF_R,MAJOHR17_2TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOHR17_2TARDIF_A, MAJOHR17_2TARDIF_F))
		    + FLAG_TRMAJOP * MAJOHR17_2TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOHR17_2TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOHR17_2TARDIF_A)
		   ) ;


MAJOHRTARDIF_D = somme(x = 07,08: MAJOHR0xTARDIF_D) + MAJOHR17_2TARDIF_D ;


MAJOLO02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOLO02_TARDIF
        + FLAG_TRTARDIF_R * MAJOLO02TARDIF_R
          + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOLO02TARDIF_R,MAJOLO02TARDIF_F)
                                     + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOLO02TARDIF_A+(MAJOTARDCOW_A-MAJOTARDCOW)*null(CSTRATE99-2)), MAJOLO02TARDIF_F))
                 + FLAG_TRMAJOP * max(0,MAJOLO02TARDIF_A+(MAJOTARDCOW_A-MAJOTARDCOW)*null(CSTRATE99-2))
                  + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                         * (positif(FLAG_RECTIF) * MAJOLO02TARDIF_R
                         + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOLO02TARDIF_A+(MAJOTARDCOW_A-MAJOTARDCOW)*null(CSTRATE99-2)))
              ) ;
MAJOLO03TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOLO03_TARDIF
                + FLAG_TRTARDIF_R * MAJOLO03TARDIF_R
                  + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOLO03TARDIF_R,MAJOLO03TARDIF_F)
                                  + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOLO03TARDIF_A+(MAJOTARDCOW_A-MAJOTARDCOW)*null(CSTRATE99-3)), MAJOLO03TARDIF_F))
                   + FLAG_TRMAJOP * max(0,MAJOLO03TARDIF_A+(MAJOTARDCOW_A-MAJOTARDCOW)*null(CSTRATE99-3))
                 + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
         * (positif(FLAG_RECTIF) * MAJOLO03TARDIF_R
                            + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOLO03TARDIF_A+(MAJOTARDCOW_A-MAJOTARDCOW)*null(CSTRATE99-3)))
               ) ;
MAJOLO07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOLO07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOLO07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOLO07TARDIF_R,MAJOLO07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOLO07TARDIF_A, MAJOLO07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOLO07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOLO07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOLO07TARDIF_A)
		   ) ;
MAJOLO08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOLO08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOLO08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOLO08TARDIF_R,MAJOLO08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOLO08TARDIF_A, MAJOLO08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOLO08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOLO08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOLO08TARDIF_A)
		   ) ;
MAJOLO17_1TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOLO17_1TARDIF
		    + FLAG_TRTARDIF_R * MAJOLO17_1TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOLO17_1TARDIF_R,MAJOLO17_1TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOLO17_1TARDIF_A, MAJOLO17_1TARDIF_F))
		    + FLAG_TRMAJOP * MAJOLO17_1TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOLO17_1TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOLO17_1TARDIF_A)
		   ) ;


MAJOCAP02TARDIF_D = FLAG_RETARD *
                    (FLAG_TRTARDIF * MAJOCAP02_TARDIF
            + FLAG_TRTARDIF_R * MAJOCAP02TARDIF_R
             + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCAP02TARDIF_R,MAJOCAP02TARDIF_F)
                                    + (1 - positif(FLAG_RECTIF) ) * min(max(0,MAJOCAP02TARDIF_A+(MAJOTARDCOV_A-MAJOTARDCOV)*null(CSTRATE99-2)) , MAJOCAP02TARDIF_F))
                + FLAG_TRMAJOP * max(0,MAJOCAP02TARDIF_A+(MAJOTARDCOV_A-MAJOTARDCOV)*null(CSTRATE99-2))
             + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                         * (positif(FLAG_RECTIF) * MAJOCAP02TARDIF_R
                         + (1 - positif(FLAG_RECTIF) ) 
			 * max(0,MAJOCAP02TARDIF_A+(MAJOTARDCOV_A-MAJOTARDCOV)*null(CSTRATE99-2)))
                 ) ;
MAJOCAP03TARDIF_D = FLAG_RETARD *
                      (FLAG_TRTARDIF * MAJOCAP03_TARDIF
                 + FLAG_TRTARDIF_R * MAJOCAP03TARDIF_R
                        + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCAP03TARDIF_R,MAJOCAP03TARDIF_F)
                                      + (1 - positif(FLAG_RECTIF)) * min(max(0,MAJOCAP03TARDIF_A+(MAJOTARDCOV_A-MAJOTARDCOV)*null(CSTRATE99-3)), MAJOCAP03TARDIF_F))
                    + FLAG_TRMAJOP * max(0,MAJOCAP03TARDIF_A+(MAJOTARDCOV_A-MAJOTARDCOV)*null(CSTRATE99-3))
                      + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                 * (positif(FLAG_RECTIF) * MAJOCAP03TARDIF_R
                       + (1 - positif(FLAG_RECTIF)) 
			 * max(0,MAJOCAP03TARDIF_A+(MAJOTARDCOV_A-MAJOTARDCOV)*null(CSTRATE99-3)))
                  ) ;
MAJOCAP07TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCAP07_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCAP07TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOCAP07TARDIF_R,MAJOCAP07TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCAP07TARDIF_A, MAJOCAP07TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCAP07TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCAP07TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOCAP07TARDIF_A)
		   ) ;
MAJOCAP08TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCAP08_TARDIF
		    + FLAG_TRTARDIF_R * MAJOCAP08TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * min(MAJOCAP08TARDIF_R,MAJOCAP08TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCAP08TARDIF_A, MAJOCAP08TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCAP08TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCAP08TARDIF_R
			 + (1 - positif(FLAG_RECTIF)) * MAJOCAP08TARDIF_A)
		   ) ;


MAJOCP17_2TARDIF_D = FLAG_RETARD *
		    (FLAG_TRTARDIF * MAJOCAP17_2TARDIF
		    + FLAG_TRTARDIF_R * MAJOCP17_2TARDIF_R
		    + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOCP17_2TARDIF_R,MAJOCP17_2TARDIF_F)
					 + (1 - positif(FLAG_RECTIF)) * min(MAJOCP17_2TARDIF_A, MAJOCP17_2TARDIF_F))
		    + FLAG_TRMAJOP * MAJOCP17_2TARDIF_A
		    + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP)) 
			  * (positif(FLAG_RECTIF) * MAJOCP17_2TARDIF_R
			    + (1 - positif(FLAG_RECTIF)) * MAJOCP17_2TARDIF_A)
		   ) ;
MAJOCAPTARDIF_D = somme(x = 07,08: MAJOCAP0xTARDIF_D) + MAJOCP17_2TARDIF_D ;

regle isf 2351:
application : iliad;

MAJOIFI08TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-8) * TMAJOIFI[00];
MAJOIFI17TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-17) * TMAJOIFI[00];
MAJOIFI07TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-7) * TMAJOIFI[00];
MAJOIFI10TARDIF_R = FLAG_RETARD * FLAG_RECTIFMAJO * null(CSTRATE99-10) * TMAJOIFI[00];

MAJOIFITARDIF_R = somme(x = 08,10,17,07: MAJOIFIxTARDIF_R);


MAJOIFI08TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 8) * arr(IFI4BASE * TAUX_2042/100);
MAJOIFI17TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 17) * arr(IFI4BASE * TAUX_2042/100);
MAJOIFI07TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 07) * arr(IFI4BASE * TAUX_2042/100);
MAJOIFI10TARDIF_F = FLAG_RETARD * null(FLAG_TRDEGTR - 10) * arr(IFI4BASE * TAUX_2042/100);


MAJOIFITARDIF_F = somme(x =08,10,17,07: MAJOIFIxTARDIF_F);

MAJOIFI08TARDIF_D = FLAG_RETARD *
                   (FLAG_TRTARDIF * MAJOIFI08_TARDIF
                   + FLAG_TRTARDIF_R * MAJOIFI08TARDIF_R
                   + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOIFI08TARDIF_R,MAJOIFI08TARDIF_F)
                   + (1 - positif(FLAG_RECTIFMAJO)) * min(MAJOIFI08TARDIF_A, MAJOIFI08TARDIF_F))
                   + FLAG_TRMAJOP * MAJOIFI08TARDIF_A
                   + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                   * (positif(FLAG_RECTIFMAJO) * MAJOIFI08TARDIF_R
                   + (1 - positif(FLAG_RECTIFMAJO)) * MAJOIFI08TARDIF_A)
                   ) ;


MAJOIFI17TARDIF_D = FLAG_RETARD *
                   (FLAG_TRTARDIF * MAJOIFI17_TARDIF
                   + FLAG_TRTARDIF_R * MAJOIFI17TARDIF_R
                   + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOIFI17TARDIF_R,MAJOIFI17TARDIF_F)
                   + (1 - positif(FLAG_RECTIFMAJO)) * min(MAJOIFI17TARDIF_A, MAJOIFI17TARDIF_F))
                   + FLAG_TRMAJOP * MAJOIFI17TARDIF_A
                   + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                   * (positif(FLAG_RECTIFMAJO) * MAJOIFI17TARDIF_R
                   + (1 - positif(FLAG_RECTIFMAJO)) * MAJOIFI17TARDIF_A)
                   ) ;
MAJOIFI10TARDIF_D = FLAG_RETARD *
                   (FLAG_TRTARDIF * MAJOIFI10_TARDIF
                   + FLAG_TRTARDIF_R * MAJOIFI10TARDIF_R
                   + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOIFI10TARDIF_R,MAJOIFI10TARDIF_F)
                   + (1 - positif(FLAG_RECTIFMAJO)) * min(MAJOIFI10TARDIF_A, MAJOIFI10TARDIF_F))
                   + FLAG_TRMAJOP * MAJOIFI10TARDIF_A
                   + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                   * (positif(FLAG_RECTIFMAJO) * MAJOIFI10TARDIF_R
                   + (1 - positif(FLAG_RECTIFMAJO)) * MAJOIFI10TARDIF_A)
                   ) ;


MAJOIFI07TARDIF_D = FLAG_RETARD *
                   (FLAG_TRTARDIF * MAJOIFI07_TARDIF
                   + FLAG_TRTARDIF_R * MAJOIFI07TARDIF_R
                   + FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * min(MAJOIFI07TARDIF_R,MAJOIFI07TARDIF_F)
                   + (1 - positif(FLAG_RECTIFMAJO)) * min(MAJOIFI07TARDIF_A, MAJOIFI07TARDIF_F))
                   + FLAG_TRMAJOP * MAJOIFI07TARDIF_A
                   + (1 - positif(FLAG_TRTARDIF + FLAG_TRTARDIF_R + FLAG_TRTARDIF_F + FLAG_TRMAJOP))
                   * (positif(FLAG_RECTIFMAJO) * MAJOIFI07TARDIF_R
                   + (1 - positif(FLAG_RECTIFMAJO)) * MAJOIFI07TARDIF_A)
                   ) ;



MAJOIFITARDIF_D = somme(x = 08,17,07 : MAJOIFIxTARDIF_D);		   


regle corrective 10941:
application : iliad;
TIRBASE[0] =   positif(FLAG_RETARD) *
                   (IRBASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TIRBASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TIRBASE[0]  + 0 
                ;
TNAPCR[0] =   positif(FLAG_RETARD) *
                   (NAPCRTARDIF_A * (1 - positif(FLAG_NBSTRTR))
                   + TNAPCR[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TNAPCR[0] + 0 
                ;
TCSBASE[0] =   positif(FLAG_RETARD) *
                    (CSGBASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TCSBASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TCSBASE[0] + 0 
                ;
TPSOLBASE[0] =   positif(FLAG_RETARD) *
                   (PSOLBASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TPSOLBASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TPSOLBASE[0] + 0 
                ;
TRDBASE[0] =   positif(FLAG_RETARD) *
                   (RDSBASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TRDBASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TRDBASE[0] + 0 
                ;

TCVNBASE[0] =   positif(FLAG_RETARD) *
                   (CVNBASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TCVNBASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TCVNBASE[0] + 0 ;

TCDISBASE[0] =   positif(FLAG_RETARD) *
                   (CDISBASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TCDISBASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TCDISBASE[0] + 0 
                ;

TGLOBASE[0] =   positif(FLAG_RETARD) *
                   (GLOBASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TGLOBASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TGLOBASE[0] + 0 
                ;

TC820BASE[0] =   positif(FLAG_RETARD) *
                   (C820BASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TC820BASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TC820BASE[0] + 0 
                ;

TRSE1BASE[0] =   positif(FLAG_RETARD) *
                   (RSE1BASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TRSE1BASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TRSE1BASE[0] + 0 
                ;
TRSE2BASE[0] =   positif(FLAG_RETARD) *
                   (RSE2BASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TRSE2BASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TRSE2BASE[0] + 0 
                ;

TRSE3BASE[0] =   positif(FLAG_RETARD) *
                    (RSE3BASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TRSE3BASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TRSE3BASE[0] + 0 
                ;

TRSE4BASE[0] =   positif(FLAG_RETARD) *
                    (RSE4BASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TRSE4BASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TRSE4BASE[0] + 0 
                ;

TRSE5BASE[0] =   positif(FLAG_RETARD) *
                    (RSE5BASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TRSE5BASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TRSE5BASE[0] + 0 
                ;

TRSE6BASE[0] =   positif(FLAG_RETARD) *
                    (RSE6BASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TRSE6BASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TRSE6BASE[0] + 0 
                ;
TRSE8BASE[0] =   positif(FLAG_RETARD) *
                    (RSE8BASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TRSE8BASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TRSE8BASE[0] + 0 
                ;
TTAXABASE[0] =  positif(FLAG_RETARD) *
                    (TAXABASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TTAXABASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TTAXABASE[0] + 0 
                ;
TCHRBASE[0] =  positif(FLAG_RETARD) *
                    (HRBASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TCHRBASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TCHRBASE[0] + 0 
                ;
TPCAPBASE[0] =  positif(FLAG_RETARD) *
                    (CAPBASE_REF * (1 - positif(FLAG_NBSTRTR))
                   + TPCAPBASE[0] * positif(FLAG_NBSTRTR))
              + (1 - positif(FLAG_RETARD)) * TPCAPBASE[0] + 0 
                ;
regle isf 236:
application : iliad;


TIFI4BASE[0] =   positif(FLAG_RETARD) *
                  (IFITARDIF_A * (1 - positif(FLAG_NBSTRTR))
	          + TIFI4BASE[0] * positif(FLAG_NBSTRTR))
	          + (1 - positif(FLAG_RETARD)) * TIFI4BASE[0] + 0
	       ;

regle corrective 23115:
application : iliad;
PROPIR07 = arr((T_RABP07 / T_RABP) * 10000)/10000 * FLAG_TRMAJOP ;
PROPIR08 = arr((T_RABP08 / T_RABP) * 10000)/10000 * FLAG_TRMAJOP ;
PROPIR10 = arr((T_RABP10 / T_RABP) * 10000)/10000 * FLAG_TRMAJOP ;
PROPIR11 = arr((T_RABP11 / T_RABP) * 10000)/10000 * FLAG_TRMAJOP ;
PROPIR17 = arr((T_RABP17 / T_RABP) * 10000)/10000 * FLAG_TRMAJOP ;
PROPIR31 = arr((T_RABP31 / T_RABP) * 10000)/10000 * FLAG_TRMAJOP ;

PROPHR07 = arr((T_RABPCH07 / T_RABPCH) * 10000)/10000 * FLAG_TRMAJOP ;
PROPHR08 = arr((T_RABPCH08 / T_RABPCH) * 10000)/10000 * FLAG_TRMAJOP ;
PROPHR10 = arr((T_RABPCH10 / T_RABPCH) * 10000)/10000 * FLAG_TRMAJOP ;
PROPHR11 = arr((T_RABPCH11 / T_RABPCH) * 10000)/10000 * FLAG_TRMAJOP ;
PROPHR17 = arr((T_RABPCH17 / T_RABPCH) * 10000)/10000 * FLAG_TRMAJOP ;
PROPHR31 = arr((T_RABPCH31 / T_RABPCH) * 10000)/10000 * FLAG_TRMAJOP ;

PROPCS07 = arr((T_RABPCS07 / T_RABPCS) * 10000)/10000 * FLAG_TRMAJOP  + 0 ;
PROPCS08 = arr((T_RABPCS08 / T_RABPCS) * 10000)/10000 * FLAG_TRMAJOP  + 0 ;
PROPCS10 = arr((T_RABPCS10 / T_RABPCS) * 10000)/10000 * FLAG_TRMAJOP  + 0 ;
PROPCS11 = arr((T_RABPCS11 / T_RABPCS) * 10000)/10000 * FLAG_TRMAJOP  + 0 ;
PROPCS17 = arr((T_RABPCS17 / T_RABPCS) * 10000)/10000 * FLAG_TRMAJOP  + 0 ;
PROPCS31 = arr((T_RABPCS31 / T_RABPCS) * 10000)/10000 * FLAG_TRMAJOP  + 0 ;
 
PROPRD07 = arr((T_RABPRD07 / T_RABPRD) * 10000)/10000 * FLAG_TRMAJOP ;
PROPRD08 = arr((T_RABPRD08 / T_RABPRD) * 10000)/10000 * FLAG_TRMAJOP ;
PROPRD10 = arr((T_RABPRD10 / T_RABPRD) * 10000)/10000 * FLAG_TRMAJOP ;
PROPRD11 = arr((T_RABPRD11 / T_RABPRD) * 10000)/10000 * FLAG_TRMAJOP ;
PROPRD17 = arr((T_RABPRD17 / T_RABPRD) * 10000)/10000 * FLAG_TRMAJOP ;
PROPRD31 = arr((T_RABPRD31 / T_RABPRD) * 10000)/10000 * FLAG_TRMAJOP ;

PROPPS07 = arr((T_RABPPS07 / T_RABPPS) * 10000)/10000 * FLAG_TRMAJOP ;
PROPPS08 = arr((T_RABPPS08 / T_RABPPS) * 10000)/10000 * FLAG_TRMAJOP ;
PROPPS10 = arr((T_RABPPS10 / T_RABPPS) * 10000)/10000 * FLAG_TRMAJOP ;
PROPPS11 = arr((T_RABPPS11 / T_RABPPS) * 10000)/10000 * FLAG_TRMAJOP ;
PROPPS17 = arr((T_RABPPS17 / T_RABPPS) * 10000)/10000 * FLAG_TRMAJOP ;
PROPPS31 = arr((T_RABPPS31 / T_RABPPS) * 10000)/10000 * FLAG_TRMAJOP ;

regle corrective 23116:
application : iliad;

MAJOIR_P08 = arr( max(0,IRBASE) * PROPIR08 * T08/100) * FLAG_TRMAJOP ;
MAJOIR_P11 = arr( max(0,IRBASE) * PROPIR11 * T11/100) * FLAG_TRMAJOP ;
MAJOIR_P31 = arr( max(0,IRBASE) * PROPIR31 * T31/100) * FLAG_TRMAJOP ;

MAJOHR_P08 = arr( max(0,IHAUTREVT + CHRPVIMP) * PROPHR08 * T08/100) * FLAG_TRMAJOP ;
MAJOHR_P11 = arr( max(0,IHAUTREVT + CHRPVIMP) * PROPHR11 * T11/100) * FLAG_TRMAJOP ;
MAJOHR_P31 = arr( max(0,IHAUTREVT + CHRPVIMP) * PROPHR31 * T31/100) * FLAG_TRMAJOP ;

MAJOCS_P08 = arr( max(0,CSBASE_MAJO) * PROPCS08 * T08/100) * FLAG_TRMAJOP ;
MAJOCS_P11 = arr( max(0,CSBASE_MAJO) * PROPCS11 * T11/100) * FLAG_TRMAJOP ;
MAJOCS_P31 = arr( max(0,CSBASE_MAJO) * PROPCS31 * T31/100) * FLAG_TRMAJOP ;

MAJORD_P08 = arr( max(0,RDBASE_MAJO) * PROPRD08 * T08/100) * FLAG_TRMAJOP ;
MAJORD_P11 = arr( max(0,RDBASE_MAJO) * PROPRD11 * T11/100) * FLAG_TRMAJOP ;
MAJORD_P31 = arr( max(0,RDBASE_MAJO) * PROPRD31 * T31/100) * FLAG_TRMAJOP ;

MAJOPSOL_P08 = arr( max(0,PSOLBASE_MAJO) * PROPPS08 * T08/100) * FLAG_TRMAJOP ;
MAJOPSOL_P11 = arr( max(0,PSOLBASE_MAJO) * PROPPS11 * T11/100) * FLAG_TRMAJOP ;
MAJOPSOL_P31 = arr( max(0,PSOLBASE_MAJO) * PROPPS31 * T31/100) * FLAG_TRMAJOP ;
regle corrective 23117:
application : iliad;
MAJOIR_P07 = arr( max(0,IRBASE) * PROPIR07 * T07/100)
	 * (1 - null((1 -IND_RJLJ) + (10 - T07)))
         * FLAG_TRMAJOP;
MAJOIR_P10_2 = arr( max(0,IRBASE) * PROPIR10 * TX1758A/100)
	 * (1 - null((1 -IND_RJLJ) + (10 - TX1758A)))
         * FLAG_TRMAJOP;

MAJOIR_P17_2 = arr( max(0,IRBASE) * PROPIR17 * 2 * TX1758A/100)
	 * (1 - null((1 -IND_RJLJ) + (10 - TX1758A)))
         * FLAG_TRMAJOP;

MAJOHR_P07 = arr( max(0,IHAUTREVT + CHRPVIMP) * PROPHR07 * T07/100)
	 * (1 - null((1 -IND_RJLJ) + (10 - T07)))
         * FLAG_TRMAJOP;

MAJOHR_P10_2 = arr( max(0,IHAUTREVT + CHRPVIMP) * PROPHR10 * TX1758A/100)
	 * (1 - null((1 -IND_RJLJ) + (10 - TX1758A)))
         * FLAG_TRMAJOP;

MAJOHR_P17_2 = arr( max(0,IHAUTREVT + CHRPVIMP) * PROPHR17 * 2 * TX1758A/100)
	 * (1 - null((1 -IND_RJLJ) + (10 - TX1758A)))
         * FLAG_TRMAJOP;

MAJOCS_P07 = arr( max(0,CSBASE_MAJO) * PROPCS07 * T07 /100)
            * (1 - null((1 -IND_RJLJ) + (10 - T07))) * FLAG_TRMAJOP; 
MAJOCS_P10 = arr( max(0,CSBASE_MAJO) * PROPCS10 * T10/100)
            * (1 - null((1 -IND_RJLJ) + (10 - T10))) * FLAG_TRMAJOP; 
MAJOCS_P17 = arr( max(0,CSBASE_MAJO) * PROPCS17 * T17/100)
            * (1 - null((1 -IND_RJLJ) + (10 - T17))) * FLAG_TRMAJOP; 

MAJORD_P07 = arr( max(0,RDBASE_MAJO) * PROPRD07 * T07/100)
            * (1 - null((1 -IND_RJLJ) + (10 - T07))) * FLAG_TRMAJOP; 
MAJORD_P10 = arr( max(0,RDBASE_MAJO) * PROPRD10 * T10/100)
            * (1 - null((1 -IND_RJLJ) + (10 - T10))) * FLAG_TRMAJOP; 
MAJORD_P17 = arr( max(0,RDBASE_MAJO) * PROPRD17 * T17/100)
            * (1 - null((1 -IND_RJLJ) + (10 - T17))) * FLAG_TRMAJOP; 

MAJOPSOL_P07 = arr( max(0,PSOLBASE_MAJO) * PROPPS07 * T07/100)
            * (1 - null((1 -IND_RJLJ) + (10 - T07))) * FLAG_TRMAJOP; 
MAJOPSOL_P10 = arr( max(0,PSOLBASE_MAJO) * PROPPS10 * T10/100)
            * (1 - null((1 -IND_RJLJ) + (10 - T10))) * FLAG_TRMAJOP; 
MAJOPSOL_P17 = arr( max(0,PSOLBASE_MAJO) * PROPPS17 * T17/100)
            * (1 - null((1 -IND_RJLJ) + (10 - T17))) * FLAG_TRMAJOP; 

regle corrective 231171:
application : iliad;
IRNIN_MAJOP = IRBASE * FLAG_TRMAJOP ;
HR_MAJOP = HRBASE_MAJO * FLAG_TRMAJOP ;

CSG_MAJOP = CSBASE_MAJO * FLAG_TRMAJOP ;
RDS_MAJOP = RDBASE_MAJO * FLAG_TRMAJOP;
PSOL_MAJOP = PSOLBASE_MAJO * FLAG_TRMAJOP;
regle corrective 23118:
application : iliad;
PROPIR = somme(i=02,03,07,08,10,11,17,31 : PROPIRi)
		* FLAG_TRMAJOP
		* FLAG_RETARD ;
PROPHR = somme(i=02,03,07,08,10,11,17,31 : PROPHRi)
		* FLAG_TRMAJOP
		* FLAG_RETARD ;
PROPCS = somme(i=02,03,07,08,10,11,17,31 : PROPCSi)
		* FLAG_TRMAJOP
		* FLAG_RETARD ;
PROPRD = somme(i=02,03,07,08,10,11,17,31 : PROPRDi)
		* FLAG_TRMAJOP
		* FLAG_RETARD ;
PROPPSOL = somme(i=02,03,07,08,10,11,17,31 : PROPPSi)
		* FLAG_TRMAJOP
		* FLAG_RETARD ;
regle corrective 231181:
application :  iliad;
MAJOIR02TARDIF_P =  arr(MAJOIR02TARDIF_D * (1 - PROPIR_A))
                * FLAG_TRTARDIF_F
               * FLAG_RETARD
               * positif(PROPIR_A) ;
MAJOIR03TARDIF_P =  arr(MAJOIR03TARDIF_D * (1 - PROPIR_A))
                * FLAG_TRTARDIF_F
               * FLAG_RETARD
               * positif(PROPIR_A) ;
MAJOIR07TARDIF_P =  arr(MAJOIR07TARDIF_D * (1 - PROPIR_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPIR_A) ;

MAJOIR08TARDIF_P =  arr(MAJOIR08TARDIF_D * (1 - PROPIR_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPIR_A)
		+ 0;


MAJOIR17_2TARDIF_P =  arr(MAJOIR17_2TARDIF_D * (1 - PROPIR_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPIR_A) ;


MAJOIRTARDIF_P = somme (x = 02,03,07,08 : MAJOIR0xTARDIF_P) + MAJOIR17_2TARDIF_P ;

MAJOHR02TARDIF_P =  arr(MAJOHR02TARDIF_D * (1 - PROPHR_A))
                * FLAG_TRTARDIF_F
                * FLAG_RETARD
              * positif(PROPHR_A) ;
MAJOHR03TARDIF_P =  arr(MAJOHR03TARDIF_D * (1 - PROPHR_A))
              * FLAG_TRTARDIF_F
                * FLAG_RETARD
                * positif(PROPHR_A) ;
MAJOHR07TARDIF_P =  arr(MAJOHR07TARDIF_D * (1 - PROPHR_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPHR_A) ;

MAJOHR08TARDIF_P =  arr(MAJOHR08TARDIF_D * (1 - PROPHR_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPHR_A)
		+ 0;

MAJOHR17_2TARDIF_P =  arr(MAJOHR17_2TARDIF_D * (1 - PROPHR_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPHR_A) ;


MAJOHRTARDIF_P = somme (x = 02,03,07,08 : MAJOHR0xTARDIF_P) + MAJOHR17_2TARDIF_P ;
MAJOCS02TARDIF_P =  arr(MAJOCS02TARDIF_D * (1 - PROPCS_A))
                * FLAG_TRTARDIF_F
               * FLAG_RETARD
              * positif(PROPCS_A);
MAJOCS03TARDIF_P =  arr(MAJOCS03TARDIF_D * (1 - PROPCS_A))
            * FLAG_TRTARDIF_F
                * FLAG_RETARD
               * positif(PROPCS_A);
MAJOCS07TARDIF_P =  arr(MAJOCS07TARDIF_D * (1 - PROPCS_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPCS_A);
MAJOCS08TARDIF_P =  arr(MAJOCS08TARDIF_D * (1 - PROPCS_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPCS_A);
MAJOCS17TARDIF_P =  arr(MAJOCS17TARDIF_D * (1 - PROPCS_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPCS_A);
MAJOCSTARDIF_P = somme (x = 02,03,07,08,17 : MAJOCSxTARDIF_P);
MAJORD02TARDIF_P =  arr(MAJORD02TARDIF_D * (1 - PROPRD_A))
                * FLAG_TRTARDIF_F
                * FLAG_RETARD
                * positif(PROPRD_A);
MAJORD03TARDIF_P =  arr(MAJORD03TARDIF_D * (1 - PROPRD_A))
            * FLAG_TRTARDIF_F
               * FLAG_RETARD
                * positif(PROPRD_A);
MAJORD07TARDIF_P =  arr(MAJORD07TARDIF_D * (1 - PROPRD_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPRD_A);
MAJORD08TARDIF_P =  arr(MAJORD08TARDIF_D * (1 - PROPRD_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPRD_A);
MAJORD17TARDIF_P =  arr(MAJORD17TARDIF_D * (1 - PROPRD_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPRD_A);
MAJORDTARDIF_P = somme (x = 02,03,07,08,17 : MAJORDxTARDIF_P);
MAJOPSOL02TARDIF_P =  arr(MAJOPSOL02TARDIF_D * (1 - PROPPS_A))
                * FLAG_TRTARDIF_F
                * FLAG_RETARD
             * positif(PROPPS_A);
MAJOPSOL03TARDIF_P =  arr(MAJOPSOL03TARDIF_D * (1 - PROPPS_A))
              * FLAG_TRTARDIF_F
               * FLAG_RETARD
                * positif(PROPPS_A);
MAJOPSOL07TARDIF_P =  arr(MAJOPSOL07TARDIF_D * (1 - PROPPSOL_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPPSOL_A);
MAJOPSOL08TARDIF_P =  arr(MAJOPSOL08TARDIF_D * (1 - PROPPSOL_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPPSOL_A);
MAJOPSOL17TARDIF_P =  arr(MAJOPSOL17TARDIF_D * (1 - PROPPSOL_A))
		* FLAG_TRTARDIF_F
		* FLAG_RETARD 
		* positif(PROPPSOL_A);
MAJOPSOLTARDIF_P = somme (x = 02,03,07,08,17 : MAJOPSOLxTARDIF_P);
regle corrective 23119:
application :   iliad ;
MAJOTO =  MAJOIR_ST + MAJOPIR_TOT  
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOIRTARDIF_A
	+ FLAG_TRTARDIF * MAJOIRTARDIF_D
	+ FLAG_TRTARDIF_F 
	       * (positif(PROPIR_A) * MAJOIRTARDIF_P
	       + (1 - positif(PROPIR_A)) * MAJOIRTARDIF_D)
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOIRTARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOIRTARDIF_A)

        + MAJOCAP_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOCAPTARDIF_A
	+ FLAG_TRTARDIF * MAJOCAPTARDIF_D
	+ FLAG_TRTARDIF_F * MAJOCAPTARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOCAPTARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOCAPTARDIF_A)

        + MAJOHR_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOHRTARDIF_A
	+ FLAG_TRTARDIF * MAJOHRTARDIF_D
	+ FLAG_TRTARDIF_F 
	       * (positif(PROPHR_A) * MAJOHRTARDIF_P
	       + (1 - positif(PROPHR_A)) * MAJOHRTARDIF_D)
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOHRTARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOHRTARDIF_A)

        + MAJOCS_ST + MAJOPCS_TOT  
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOCSTARDIF_A
	+ FLAG_TRTARDIF * MAJOCSTARDIF_D
	+ FLAG_TRTARDIF_F 
	       * (positif(PROPCS_A) * MAJOCSTARDIF_P
	       + (1 - positif(PROPCS_A)) * MAJOCSTARDIF_D)
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOCSTARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOCSTARDIF_A)

        + MAJOPSOL_ST + MAJOPPSOL_TOT  
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOPSOLTARDIF_A
	+ FLAG_TRTARDIF * MAJOPSOLTARDIF_D
	+ FLAG_TRTARDIF_F 
	       * (positif(PROPPSOL_A) * MAJOPSOLTARDIF_P
	       + (1 - positif(PROPPSOL_A)) * MAJOPSOLTARDIF_D)
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOPSOLTARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOPSOLTARDIF_A)

        + MAJORD_ST + MAJOPRD_TOT  
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJORDTARDIF_A
	+ FLAG_TRTARDIF * MAJORDTARDIF_D
	+ FLAG_TRTARDIF_F 
	       * (positif(PROPRD_A) * MAJORDTARDIF_P
	       + (1 - positif(PROPRD_A)) * MAJORDTARDIF_D)
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJORDTARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJORDTARDIF_A)

        + MAJOCVN_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOCVNTARDIF_A
	+ FLAG_TRTARDIF * MAJOCVNTARDIF_D
	+ FLAG_TRTARDIF_F * MAJOCVNTARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOCVNTARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOCVNTARDIF_A)


        + MAJOCDIS_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOCDISTARDIF_A
	+ FLAG_TRTARDIF * MAJOCDISTARDIF_D
	+ FLAG_TRTARDIF_F * MAJOCDISTARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOCDISTARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOCDISTARDIF_A)

        + MAJOGLO_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOGLOTARDIF_A
	+ FLAG_TRTARDIF * MAJOGLOTARDIF_D
	+ FLAG_TRTARDIF_F * MAJOGLOTARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOGLOTARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOGLOTARDIF_A)
        + MAJOC820_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOC820TARDIF_A
	+ FLAG_TRTARDIF * MAJOC820TARDIF_D
	+ FLAG_TRTARDIF_F * MAJOC820TARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOC820TARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOC820TARDIF_A)

        + MAJOTAXA_ST  
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJOTAXATARDIF_A
	+ FLAG_TRTARDIF * MAJOTAXATARDIF_D
	+ FLAG_TRTARDIF_F * MAJOTAXATARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJOTAXATARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJOTAXATARDIF_A)


        + MAJORSE1_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJORSE1TARDIF_A
	+ FLAG_TRTARDIF * MAJORSE1TARDIF_D
	+ FLAG_TRTARDIF_F * MAJORSE1TARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJORSE1TARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJORSE1TARDIF_A)


        + MAJORSE2_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJORSE2TARDIF_A
	+ FLAG_TRTARDIF * MAJORSE2TARDIF_D
	+ FLAG_TRTARDIF_F * MAJORSE2TARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJORSE2TARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJORSE2TARDIF_A)


        + MAJORSE3_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJORSE3TARDIF_A
	+ FLAG_TRTARDIF * MAJORSE3TARDIF_D
	+ FLAG_TRTARDIF_F * MAJORSE3TARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJORSE3TARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJORSE3TARDIF_A)


        + MAJORSE4_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJORSE4TARDIF_A
	+ FLAG_TRTARDIF * MAJORSE4TARDIF_D
	+ FLAG_TRTARDIF_F * MAJORSE4TARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJORSE4TARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJORSE4TARDIF_A)

        + MAJORSE5_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJORSE5TARDIF_A
	+ FLAG_TRTARDIF * MAJORSE5TARDIF_D
	+ FLAG_TRTARDIF_F * MAJORSE5TARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJORSE5TARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJORSE5TARDIF_A)

        + MAJORSE6_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJORSE6TARDIF_A
	+ FLAG_TRTARDIF * MAJORSE6TARDIF_D
	+ FLAG_TRTARDIF_F * MAJORSE6TARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJORSE6TARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJORSE6TARDIF_A)

        + MAJORSE8_ST 
	+ FLAG_RETARD * (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718) ) * MAJORSE8TARDIF_A
	+ FLAG_TRTARDIF * MAJORSE8TARDIF_D
	+ FLAG_TRTARDIF_F * MAJORSE8TARDIF_D
	- FLAG_TRTARDIF_F * (positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718)  * MAJORSE8TARDIF_R
			   + (1 - positif(FLAG_RECTIFMAJO) * positif(FLAG_RETARD0718 )) * MAJORSE8TARDIF_A)
	;


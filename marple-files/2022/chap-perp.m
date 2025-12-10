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
                                                                       
  ####   #    #    ##    #####      #     #####  #####   ######         
 #    #  #    #   #  #   #    #     #       #    #    #  #          
 #       ######  #    #  #    #     #       #    #    #  #####      
 #       #    #  ######  #####      #       #    #####   #             
 #    #  #    #  #    #  #          #       #    #   #   #              
  ####   #    #  #    #  #          #       #    #    #  ######  
regle 31000:
application : iliad   ;

PERP_BOOL = positif(null(1 - (V_0CF + V_0CH + V_0CR + V_0DJ + V_0DN + V_0DP)) 
                    * null(positif(TSHALLO2 + 0) + positif(ALLO2 + 0) + positif(TSHALLO3 + 0) + positif(ALLO3 + 0) + positif(TSHALLO4 + 0) + positif(ALLO4 + 0) 
                           + positif(FRN2 + 0) + positif(FRN3 + 0) + positif(FRN4 + 0)
                           + positif(CARTSP2 + 0) + positif(CARTSP3 + 0) + positif(CARTSP4 + 0)
                           + positif(REMPLAP2 + 0) + positif(REMPLAP3 + 0) + positif(REMPLAP4 + 0)
                           + positif(COD1DF + 0) + positif(COD1DG + 0) + positif(COD1EF + 0) 
                           + positif(COD1EG + 0) + positif(COD1FF + 0) + positif(COD1FG + 0) 
                           + positif(CODRDF + 0) + positif(CODRDG + 0) + positif(CODREF + 0) 
                           + positif(CODRGG + 0) + positif(CODRFF + 0) + positif(CODRFG + 0) 
                           + positif(COD1JB + 0)
                           + positif(COD1DA + 0) + positif(COD1EA + 0) + positif(COD1FA + 0)
                           + positif(COD1JF + 0) + positif(COD1KF + 0) + positif(COD1LF + 0)
                           + positif(COD1JG + 0) + positif(COD1KG + 0) + positif(COD1LG + 0)
                           + positif(COD1JH + 0) + positif(COD1KH + 0) + positif(COD1LH + 0)
                           + positif(COD1PE + 0) + positif(COD1PF + 0) + positif(COD1PG + 0)
                           + positif(COD1DD + 0) + positif(COD1DV + 0) + positif(COD1ED + 0)
                           + positif(COD1EV + 0) + positif(COD1FD + 0) + positif(COD1FV + 0)
			   )) ; 


regle 31002:
application : iliad   ;

PERPSALV = TSHALLOV + COD1AF + COD1AG + ALLOV + GLDGRATV + BPCOSAV + TSASSUV + CARTSV  
           + REMPLAV + CODDAJ + CODEAJ + SALEXTV + COD1NX + COD1PM + CODRAF + CODRAG
           + COD1GB + COD1AA + COD1GF + COD1GG + COD1GH + COD1TP + COD1AD + COD1PB;

PERPSALC = TSHALLOC + COD1BF + COD1BG + ALLOC + GLDGRATC + BPCOSAC + TSASSUC + CARTSC  
           + REMPLAC + CODDBJ + CODEBJ + SALEXTC + COD1OX + COD1QM + CODRBF + CODRBG 
	   + COD1HB + COD1BA + COD1HF + COD1HG + COD1HH + COD1UP + COD1BD + COD1PC;

PERPSALP = (TSHALLO1 + COD1CF + COD1CG + ALLO1 + CARTSP1 + REMPLAP1 + SALEXT1
            + CODRCF + CODRCG + COD1IB + COD1CA + COD1IF + COD1IG + COD1IH + COD1PD + COD1CD) * PERP_BOOL ;

regle 31003:
application : iliad   ;

PERPSALDV = PREP10V ;

PERPSALDC = PREP10C ;

PERPSALDP = PREP10P * PERP_BOOL ;

regle 31004:
application : iliad   ;
PERPSALNV = PERPSALV - PERPSALDV ;
PERPSALNC = PERPSALC - PERPSALDC ;
PERPSALNP = PERPSALP -PERPSALDP ;
regle 31005:
application : iliad ;

PERPBANV = BAFPVV + BAEXV + BACREV - BACDEV + BAHEXV + BAHREV - BAHDEV + BAPERPV + 4BACREV + 4BAHREV 
           + BAFORESTV + BANOCGAV + COD5XT + COD5XV - COD5XO + COD5AK + COD5AL + COD5XA + BAMICV ;

PERPBANC = BAFPVC + BAEXC + BACREC - BACDEC + BAHEXC + BAHREC - BAHDEC + BAPERPC + 4BACREC + 4BAHREC 
           + BAFORESTC + BANOCGAC + COD5XU + COD5XW - COD5YO + COD5BK + COD5BL + COD5YA + BAMICC ;

PERPBANP = (BAFPVP + BAEXP + BACREP - BACDEP + BAHEXP + BAHREP - BAHDEP + BAPERPP + 4BACREP  
            + 4BAHREP + BAFORESTP + BANOCGAP - COD5ZO + COD5CK + COD5CL + COD5ZA + BAMICP) * PERP_BOOL ;

regle 31006:
application :  iliad   ;

PERPBICMNV = TPMIB_NETPV + TPMIB_NETVV + MIBEXV + MIBPVV - BICPMVCTV ;
PERPBICMNC = TPMIB_NETPC + TPMIB_NETVC + MIBEXC + MIBPVC - BICPMVCTC ;
PERPBICMNP = (TPMIB_NETPP + TPMIB_NETVP + MIBEXP + MIBPVP - BICPMVCTP) * PERP_BOOL ; 

regle 31007:
application : iliad   ;

PERPBICPNV = BICEXV + BICNOV - BICDNV + BIHEXV + BIHNOV - BIHDNV + COD5DF + COD5DG + CODCKC + CODCKI ;

PERPBICPNC = BICEXC + BICNOC - BICDNC + BIHEXC + BIHNOC - BIHDNC + COD5EF + COD5EG + CODCLC + CODCLI ;

PERPBICPNP = (BICEXP + BICNOP - BICDNP + BIHEXP + BIHNOP - BIHDNP + COD5FF + COD5FG + CODCMC + CODCMI) * PERP_BOOL ;

regle 31008:
application : iliad   ;
PERPBNCMNV = BNCPROEXV + max(0 , (BNCPROV + AUTOBNCV) - max(arr((BNCPROV + AUTOBNCV) * SPETXAB/100) , MIN_SPEBNC)) + BNCPROPVV - BNCPMVCTV ;

PERPBNCMNC = BNCPROEXC + max(0 , (BNCPROC + AUTOBNCC) - max(arr((BNCPROC + AUTOBNCC) * SPETXAB/100) , MIN_SPEBNC)) + BNCPROPVC - BNCPMVCTC ;

PERPBNCMNP = (BNCPROEXP + max(0 , (BNCPROP + AUTOBNCP) - max(arr((BNCPROP + AUTOBNCP) * SPETXAB/100) , MIN_SPEBNC)) + BNCPROPVP - BNCPMVCTP) * PERP_BOOL ;

regle 31009:
application :  iliad   ;

PERPBNCPNV = BNCEXV + BNCREV - BNCDEV + BNHEXV + BNHREV - BNHDEV + BNCCRV + COD5XJ + COD5XK + CODCQC + CODCQI ;

PERPBNCPNC = BNCEXC + BNCREC - BNCDEC + BNHEXC + BNHREC - BNHDEC + BNCCRC + COD5YJ + COD5YK + CODCRC + CODCRI ;

PERPBNCPNP = (BNCEXP + BNCREP - BNCDEP + BNHEXP + BNHREP - BNHDEP + BNCCRP + COD5ZJ + COD5ZK + CODCSC + CODCSI) * PERP_BOOL ; 

regle 31010:
application :  iliad   ;

PERPNONSALV = PERPBANV + PERPBICMNV + PERPBICPNV + PERPBNCMNV + PERPBNCPNV ;
PERPNONSALC = PERPBANC + PERPBICMNC + PERPBICPNC + PERPBNCMNC + PERPBNCPNC ;
PERPNONSALP = PERPBANP + PERPBICMNP + PERPBICPNP + PERPBNCMNP + PERPBNCPNP ;

regle 31011:
application :  iliad   ;

PERPREVTOTV = max(0 , PERPSALNV + PERPNONSALV) ;
PERPREVTOTC = max(0 , PERPSALNC + PERPNONSALC) ;
PERPREVTOTP = max(0 , PERPSALNP + PERPNONSALP) ;

regle 31012:
application : iliad   ;

PERP_INDV = positif(0 + positif(PERPSALV) 
	              + positif(BAFPVV + BAEXV + BACREV + BACDEV + BAHEXV + BAHREV + BAHDEV + BAPERPV + 4BACREV + 4BAHREV
	                        + BAFORESTV + BANOCGAV + COD5XT + COD5XV + COD5XO + COD5AK + COD5AL + COD5XA + BAMICV)
		      + positif(MIBEXV + MIBVENV + MIBPRESV + MIBPVV + AUTOBICVV + AUTOBICPV + MIBGITEV + LOCGITV )
                      + positif(BICEXV + BICNOV + BICDNV + BIHEXV + BIHNOV + BIHDNV + COD5DF + COD5DG + CODCKC + CODCKI)
                      + positif(BNCPROEXV + BNCPROV + BNCPROPVV + AUTOBNCV) 
                      + positif(BNCEXV + BNCREV + BNCDEV + BNHEXV + BNHREV + BNHDEV + BNCCRV + COD5XJ + COD5XK + CODCQC + CODCQI)) ;

PERP_INDC = positif(0 + positif(PERPSALC)
                      + positif(BAFPVC + BAEXC + BACREC + BACDEC + BAHEXC + BAHREC + BAHDEC + BAPERPC + 4BACREC + 4BAHREC 
		                + BAFORESTC + BANOCGAC + COD5XU + COD5XW + COD5YO + COD5BK + COD5BL + COD5YA + BAMICC)
                      + positif(MIBEXC + MIBVENC + MIBPRESC + MIBPVC + AUTOBICVC + AUTOBICPC + MIBGITEC + LOCGITC + MIBMEUC)
                      + positif(BICEXC + BICNOC + BICDNC + BIHEXC + BIHNOC + BIHDNC + COD5EF + COD5EG + CODCLC + CODCLI)
                      + positif(BNCPROEXC + BNCPROC + BNCPROPVC + AUTOBNCC) 
                      + positif(BNCEXC + BNCREC + BNCDEC + BNHEXC  + BNHREC + BNHDEC + BNCCRC + COD5YJ + COD5YK + CODCRC + CODCRI)) ;

PERP_INDP = positif(0 + positif(PERPSALP)
                      + positif(BAFPVP + BAEXP + BACREP + 4BACREP + BACDEP + BAHEXP + BAHREP + 4BAHREP + BAHDEP + BAPERPP
                                + BAFORESTP + BANOCGAP + COD5ZO + COD5CK + COD5CL + COD5ZA + BAMICP)
                      + positif(MIBEXP + MIBVENP + MIBPRESP + MIBPVP + AUTOBICVP + AUTOBICPP+ MIBGITEP+LOCGITP+ MIBMEUP)
                      + positif(BICEXP + BICNOP + BICDNP + BIHEXP + BIHNOP  + BIHDNP + COD5FF + COD5FG + CODCMC + CODCMI)
                      + positif(BNCPROEXP + BNCPROP + BNCPROPVP + AUTOBNCP)
                      + positif(BNCEXP + BNCREP + BNCDEP + BNHEXP  + BNHREP + BNHDEP + BNCCRP + COD5ZJ + COD5ZK + COD5ZK + CODCSC + CODCSI)) ;

regle 31013:
application : iliad   ;



PERPINDV = positif(((positif(positif(PERP_INDV)
                    + (1 - positif(PERP_INDV)) * (1 - positif(PRBRV + PALIV + COD1AI + PCAPTAXV))) * positif(INDREV1A8))
                    + (1 - positif(PERP_INDV)) * positif(PRBRV + PALIV + COD1AI + PCAPTAXV) * positif(PERP_COTV+COD6NS)
                    + PERPMUTU * (1 - positif(PERP_INDV + PERP_COTV+COD6NS)))
                             * (1-positif(present(PRBRV)+present(PALIV)+present(COD1AI)+present(PCAPTAXV)))
	            + positif(PERP_INDV) * positif(PRBRV + PALIV + COD1AI + PCAPTAXV ) 	
		    + PERPMUTU * (1-positif(PERPIMPATRIE))* BOOL_0AM
		    + (1 - positif(PERP_INDV)) * positif(PRBRV + PALIV + COD1AI + PCAPTAXV) * positif(PERP_COTV+COD6NS)
                    +  positif(PERPREVTOTV) * (1-positif(PERP_COTV+COD6NS)) * (1-positif(PERPMUTU))
                    * positif(present(PRBRV)+present(PALIV)+present(COD1AI)+present(PCAPTAXV)))
                    * (1 - PERP_NONV) * (1 - V_CNR)
                     ;


PERPINDC = positif(((positif(positif(PERP_INDC)
	            + (1 - positif(PERP_INDC)) * (1 - positif(PRBRC + PALIC + COD1BI + PCAPTAXC))) * positif(INDREV1A8))
	            + (1 - positif(PERP_INDC)) * positif(PRBRC + PALIC + COD1BI + PCAPTAXC) * positif(PERP_COTC+COD6NT) 
	            + PERPMUTU * (1 - positif(PERP_INDC + PERP_COTC+COD6NT)))
	                     * (1-positif(present(PRBRC)+present(PALIC)+present(COD1BI)+present(PCAPTAXC)))
	            + positif(PERP_INDC) * positif(PRBRC + PALIC + COD1BI + PCAPTAXC )
	  	    + PERPMUTU * (1-positif(PERPIMPATRIE)) * BOOL_0AM
		    + (1 - positif(PERP_INDC)) * positif(PRBRC + PALIC + COD1BI + PCAPTAXC) * positif(PERP_COTC+COD6NT)
		    + positif(PERPREVTOTC) * (1-positif(PERP_COTC+COD6NT)) * (1-positif(PERPMUTU))
		    * positif(present(PRBRC)+present(PALIC)+present(COD1BI)+present(PCAPTAXC)))
		    * (1 - PERP_NONC) * BOOL_0AM * (1 -V_CNR)
	             ;

PERPINDP = positif(((positif(positif(PERP_INDP)
                    + (1 - positif(PERP_INDP)) * (1 - positif(PRBR1 + PALIP + COD1CI + COD1DI + COD1EI + COD1FI + COD1CT + COD1DT + COD1ET + COD1FT))) * positif(INDREV1A8))
                    + (1 - positif(PERP_INDP)) * positif(PRBR1 + PALIP + COD1CI + COD1DI + COD1EI + COD1FI + COD1CT + COD1DT + COD1ET + COD1FT) * positif(PERP_COTP+COD6NU))
		    *(1-positif(present(PRBR1)+present(PRBR2)+present(PRBR3)+present(PRBR4)+present(PALIP)+present(COD1CI)+present(COD1DI)+present(COD1EI)+present(COD1FI))) 
		    + positif(PERP_INDP)  * positif(PRBR1 + PALI1 + COD1CI + COD1CT )
		    + (1 - positif(PERP_INDP)) * positif(PRBR1 + PALI1 + COD1CI + COD1CT) * positif(PERP_COTP+COD6NU)
	           + positif(PERPREVTOTP) * positif(present(PRBR1)+present(PRBR2)+present(PRBR3)+present(PRBR4)+present(PALIP)+present(COD1CI)+present(COD1DI)+present(COD1EI)+present(COD1FI)))
            * PERP_BOOL * (1 - V_CNR) 
	    ;
	   
regle 31014:
application : iliad   ;

PERPINDCV = positif(V_BTPERPTOTV + PERPPLAFCV + PERPPLAFNUV1 + PERPPLAFNUV2 + PERPPLAFNUNV +PERP_COTV ) * PERPINDV * (1 -V_CNR) ;

PERPINDCC = BOOL_0AM * positif(V_BTPERPTOTC + PERPPLAFCC + PERPPLAFNUC1 + PERPPLAFNUC2 + PERPPLAFNUNC + PERP_COTC) * PERPINDC * (1 -V_CNR) ;

PERPINDCP = positif(V_BTPERPTOTP + PERPPLAFCP + PERPPLAFNUP1 + PERPPLAFNUP2 + PERPPLAFNUNP + PERP_COTP) * PERPINDP * PERP_BOOL * (1 - V_CNR) ;

regle 31015:
application : iliad   ;
PERPPLAFV = 
	      max(0,positif(PERPREVTOTV) 
	      * (max(min(arr(PERPREVTOTV * TX_PERPPLAF/100),LIM_PERPMAX),LIM_PERPMIN)-(PERPV+COD6OS))
            + (1 - positif(PERPREVTOTV)) * (LIM_PERPMIN - (PERPV+COD6OS))
               )
 	   * (1 -V_CNR) ;
PERPPLAFC = BOOL_0AM * 
		max(0,positif(PERPREVTOTC) 
		* (max(min(arr(PERPREVTOTC * TX_PERPPLAF/100),LIM_PERPMAX),LIM_PERPMIN)-(PERPC+COD6OT))
                + (1 - positif(PERPREVTOTC)) * (LIM_PERPMIN - (PERPC+COD6OT))
                   ) 
 	   * (1 -V_CNR) ;
PERPPLAFP = PERP_BOOL *
	      max(0,positif(PERPREVTOTP) 
	      * (max(min(arr(PERPREVTOTP * TX_PERPPLAF/100),LIM_PERPMAX) , LIM_PERPMIN) - (PERPP + COD6OU))
            + (1 - positif(PERPREVTOTP)) * (LIM_PERPMIN - (PERPP + COD6OU)) 
               )
 	   * (1 -V_CNR) ;
regle 31016:
application : iliad   ;


PERPPLAFTV = max(0 , PERPPLAFV + PERPPLAFNUNV + PERPPLAFNU1V + PERPPLAFNU2V) * (1 - V_CNR) ;
PERPPLAFTC = max(0 , PERPPLAFC + PERPPLAFNUNC + PERPPLAFNU1C + PERPPLAFNU2C) * (1 - V_CNR) ;
PERPPLAFTP = max(0 , PERPPLAFP + PERPPLAFNUNP + PERPPLAFNU1P + PERPPLAFNU2P) * PERP_BOOL * (1 - V_CNR) ;

regle 31017:
application : iliad   ;
PERPPLATVANT = (1 - positif(present(PERPPLAFCV) + present(PERPPLAFNUV1)
		+ present(PERPPLAFNUV2) + present(PERPPLAFNUV3))) * V_BTPERPTOTV
		+ positif(present(PERPPLAFCV) + present(PERPPLAFNUV1)
		+ present(PERPPLAFNUV2) + present(PERPPLAFNUV3))
		 *(PERPPLAFCV + PERPPLAFNUV1 + PERPPLAFNUV2 + PERPPLAFNUV3) ;
PERPPLATCANT = (1 - positif(present(PERPPLAFCC) + present(PERPPLAFNUC1)
		+ present(PERPPLAFNUC2) + present(PERPPLAFNUC3))) * V_BTPERPTOTC
		+ positif(present(PERPPLAFCC) + present(PERPPLAFNUC1)
		+ present(PERPPLAFNUC2) + present(PERPPLAFNUC3))
		 *(PERPPLAFCC + PERPPLAFNUC1 + PERPPLAFNUC2 + PERPPLAFNUC3) ;
PERPPLATPANT = (1 - positif(present(PERPPLAFCP) + present(PERPPLAFNUP1)
		+ present(PERPPLAFNUP2) + present(PERPPLAFNUP3))) * V_BTPERPTOTP
		+ positif(present(PERPPLAFCP) + present(PERPPLAFNUP1)
		+ present(PERPPLAFNUP2) + present(PERPPLAFNUP3))
		 *(PERPPLAFCP + PERPPLAFNUP1 + PERPPLAFNUP2 + PERPPLAFNUP3) ;
PERPPLAFVANT = present(PERPPLAFCV) * PERPPLAFCV + (1 - present(PERPPLAFCV)) * V_BTPERPV ;
PERPPLAFCANT = present(PERPPLAFCC) * PERPPLAFCC + (1 - present(PERPPLAFCC)) * V_BTPERPC ;
PERPPLAFPANT = present(PERPPLAFCP) * PERPPLAFCP + (1 - present(PERPPLAFCP)) * V_BTPERPP ;
PERPPLAFNUV2ANT = present(PERPPLAFNUV2) * PERPPLAFNUV2 +(1 - present(PERPPLAFNUV2)) * V_BTPERPNUV2  ;
PERPPLAFNUC2ANT = present(PERPPLAFNUC2) * PERPPLAFNUC2 +(1 - present(PERPPLAFNUC2)) * V_BTPERPNUC2  ;
PERPPLAFNUP2ANT = present(PERPPLAFNUP2) * PERPPLAFNUP2 +(1 - present(PERPPLAFNUP2)) * V_BTPERPNUP2  ;
PERPPLAFNUV3ANT = present(PERPPLAFNUV3) * PERPPLAFNUV3 +(1 - present(PERPPLAFNUV3)) * V_BTPERPNUV3  ;
PERPPLAFNUC3ANT = present(PERPPLAFNUC3) * PERPPLAFNUC3 +(1 - present(PERPPLAFNUC3)) * V_BTPERPNUC3  ;
PERPPLAFNUP3ANT = present(PERPPLAFNUP3) * PERPPLAFNUP3 +(1 - present(PERPPLAFNUP3)) * V_BTPERPNUP3  ;
regle 31018:
application : iliad   ;
PERPPLAFNUTV = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* max(PERPPLATVANT - RPERPV,0)
		+ positif(PERP_COND1) * 0
		+ positif(PERP_COND2) * max(0,PERPPLATVANT - RPERPV - RPERPMUTC)
		 ;
PERPPLAFNUTC = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* max(PERPPLATCANT - RPERPC,0)
		+ positif(PERP_COND1) * max(0,PERPPLATCANT - RPERPC - RPERPMUTV)
		+ positif(PERP_COND2) * 0
		 ;
PERPPLAFNUTP = max(PERPPLATPANT - RPERPP,0) ;
PERPPLAFNUV = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* ((1 - positif(PERPIMPATRIE+0)) * max(0,PERPPLAFVANT - RPERPV)
	         + positif(PERPIMPATRIE+0) * max(0,PERPPLAFV - RPERPV))
	       + positif(PERP_COND1) * 0
	       + positif(PERP_COND2) 
	       * ((1 - positif(PERPIMPATRIE))
		 * max(0,PERPPLAFVANT - RPERPV - RPERPMUTC)
		 + positif(PERPIMPATRIE)
		 * max(0,PERPPLAFV - RPERPV - RPERPMUTC))
	        ;
PERPPLAFNUC = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* ((1 - positif(PERPIMPATRIE+0)) * max(0,PERPPLAFCANT - RPERPC)
	         + positif(PERPIMPATRIE+0) * max(0,PERPPLAFC - RPERPC))
	       + positif(PERP_COND1) 
	       * ((1 - positif(PERPIMPATRIE))
	       * max(0,PERPPLAFCANT - RPERPC - RPERPMUTV)
		 + positif(PERPIMPATRIE)
	       * max(0,PERPPLAFC - RPERPC - RPERPMUTV))
	       + positif(PERP_COND2) * 0
	        ;
PERPPLAFNUP = (1 - positif(PERPIMPATRIE+0)) * max(0,PERPPLAFPANT - RPERPP)
	       + positif(PERPIMPATRIE+0) * max(0,PERPPLAFP - RPERPP)
	        ;
PERPPLAFNUNV = max(0,PERPPLAFNUV) ;
PERPPLAFNUNC = max(0,PERPPLAFNUC) ;
PERPPLAFNUNP = max(0,PERPPLAFNUP) ;
PERPPLAFNU3V = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* ((1 - positif(PERPIMPATRIE+0)) 
		* (positif(PERPPLAFNUV) * PERPPLAFNUV3ANT
	             + (1 - positif(PERPPLAFNUV)) 
		    * max(0,PERPPLAFNUV3ANT - (RPERPV - PERPPLAFVANT)))
		    + positif(PERPIMPATRIE+0) * 0 )
   		+ positif(PERP_COND1) * 0
   		+ positif(PERP_COND2) * (positif(PERPPLAFNUV) * PERPPLAFNUV3ANT
			+ (1 - positif(PERPPLAFNUV)) *max(0,PERPPLAFNUV3ANT - (RPERPV + RPERPMUTC- PERPPLAFVANT)))
		     ;
PERPPLAFNU3C = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* ((1 - positif(PERPIMPATRIE+0)) 
		* (positif(PERPPLAFNUC) * PERPPLAFNUC3ANT
	             + (1 - positif(PERPPLAFNUC)) 
		    * max(0,PERPPLAFNUC3ANT - (RPERPC - PERPPLAFCANT)))
		    + positif(PERPIMPATRIE+0) * 0 )
   		+ positif(PERP_COND1) * (positif(PERPPLAFNUC) * PERPPLAFNUC3ANT
			+ (1 - positif(PERPPLAFNUC)) *max(0,PERPPLAFNUC3ANT - (RPERPC + RPERPMUTV- PERPPLAFCANT)))
   		+ positif(PERP_COND2) * 0
		     ;
PERPPLAFNU3P = (1 - positif(PERPIMPATRIE+0)) 
		* (
		  max(0,positif(PERPPLAFNUP) * PERPPLAFNUP3ANT
	             + (1 - positif(PERPPLAFNUP+0)) 
		    * (PERPPLAFNUP3ANT - (RPERPP - PERPPLAFPANT)))
		    )
		 + positif(PERPIMPATRIE+0) * 0  ;
PERPPLAFNU2V = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* ((1 - positif(PERPIMPATRIE+0)) 
		* (positif(PERPPLAFVANT + PERPPLAFNUV3ANT - RPERPV) 
				* PERPPLAFNUV2ANT
	          + (1 - positif(PERPPLAFVANT + PERPPLAFNUV3ANT - RPERPV)) 
		    * max(0,PERPPLAFNUV2ANT - (RPERPV - PERPPLAFVANT - PERPPLAFNUV3ANT)))
		    + positif(PERPIMPATRIE+0) * 0 )
   		+ positif(PERP_COND1) * 0
   		+ positif(PERP_COND2) 
		* ((1 - positif(PERPIMPATRIE+0))
		* (positif(PERPPLAFVANT + PERPPLAFNUV3ANT - RPERPV - RPERPMUTC)
				* PERPPLAFNUV2ANT
	          + (1 - positif(PERPPLAFVANT + PERPPLAFNUV3ANT - RPERPV - RPERPMUTC)) 
		    * max(0,PERPPLAFNUV2ANT - ((RPERPV + RPERPMUTC) - (PERPPLAFVANT + PERPPLAFNUV3ANT))))
		    + positif(PERPIMPATRIE+0) * 0 )
		 ;
PERPPLAFNU2C = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* ((1 - positif(PERPIMPATRIE+0)) 
		* (positif(PERPPLAFCANT + PERPPLAFNUC3ANT - RPERPC) 
				* PERPPLAFNUC2ANT
	          + (1 - positif(PERPPLAFCANT + PERPPLAFNUC3ANT - RPERPC)) 
		    * max(0,PERPPLAFNUC2ANT - (RPERPC - PERPPLAFCANT - PERPPLAFNUC3ANT)))
		    + positif(PERPIMPATRIE+0) * 0 )
   		+ positif(PERP_COND1) 
		* ((1 - positif(PERPIMPATRIE+0))
	            *(positif(PERPPLAFNUC2ANT-((RPERPC+RPERPMUTV)-(PERPPLAFNUC3ANT + PERPPLAFCANT)))	
				* PERPPLAFNUC2ANT
	          + (1 - positif(PERPPLAFCANT + PERPPLAFNUC3ANT - RPERPC - RPERPMUTV)) 
		    * max(0,PERPPLAFNUC2ANT - (RPERPC + RPERPMUTV) - (PERPPLAFCANT + PERPPLAFNUC3ANT)))
		    + positif(PERPIMPATRIE+0) * 0 )
   		+ positif(PERP_COND2) * 0
		 ;
PERPPLAFNU2P = (1 - positif(PERPIMPATRIE+0)) 
             * (
             max(0,positif(PERPPLAFPANT + PERPPLAFNUP3ANT - RPERPP) 
             * PERPPLAFNUP2ANT
             + (1 - positif(PERPPLAFPANT + PERPPLAFNUP3ANT - RPERPP)) 
             * max(0,PERPPLAFNUP2ANT - (RPERPP - PERPPLAFPANT - PERPPLAFNUP3ANT)))
             )
             + positif(PERPIMPATRIE+0) * 0  ;
PERPPLAFNU1V = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* ((1 - positif(PERPIMPATRIE+0)) 
	         * max(PERPPLAFNUTV - PERPPLAFNUNV - PERPPLAFNU3V - PERPPLAFNU2V,0)
	       	+ positif(PERPIMPATRIE+0) * 0 )
		+ positif(PERP_COND1) * 0
		+ positif(PERP_COND2) 
		* ((1 - positif(PERPIMPATRIE+0)) 
	         * max(PERPPLAFNUTV - PERPPLAFNUNV - PERPPLAFNU3V - PERPPLAFNU2V,0)
   		+ positif(PERP_COND2) * 0)
	        ;
PERPPLAFNU1C = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2))
		* ((1 - positif(PERPIMPATRIE+0)) 
	         * max(PERPPLAFNUTC - PERPPLAFNUNC - PERPPLAFNU3C - PERPPLAFNU2C,0)
	       	+ positif(PERPIMPATRIE+0) * 0 )
		+ positif(PERP_COND1) 
		* ((1 - positif(PERPIMPATRIE+0)) 
	         * max(PERPPLAFNUTC - PERPPLAFNUNC - PERPPLAFNU3C - PERPPLAFNU2C,0)
		    + positif(PERPIMPATRIE+0) * 0) 
		+ positif(PERP_COND2) * 0
	        ;
PERPPLAFNU1P = (1 - positif(PERPIMPATRIE+0)) 
	         * max(PERPPLAFNUTP - PERPPLAFNUNP - PERPPLAFNU3P - PERPPLAFNU2P,0)
	       + positif(PERPIMPATRIE+0) * 0  ;
regle 31019:
application : iliad   ;
PERP_NONV = positif(
		(1 - positif(PERP_INDV)) * (1 - positif(PRBRV + PALIV + COD1AI + PCAPTAXV))
		* (1 - positif(PERP_COTV))
		* (1 - positif(PERP_INDC)) * positif(PRBRC + PALIC + COD1BI + PCAPTAXC)
	  ) ;
PERP_NONC = BOOL_0AM * positif(
		(1 - positif(PERP_INDC)) * (1 - positif(PRBRC + PALIC + COD1BI + PCAPTAXC))
		* (1 - positif(PERP_COTC))
		* (1 - positif(PERP_INDV)) * positif(PRBRV + PALIV + COD1AI + PCAPTAXV)
	  ) ;
PERP_NONP = PERP_BOOL * positif(PERP_NONC + PERP_NONV) ;
regle 31020:
application : iliad   ;
PERPPLAFCOMV = positif(PERPIMPATRIE) * PERPPLAFV *3 + (1 - positif(PERPIMPATRIE)) * 0 ;
PERPPLAFCOMC = positif(PERPIMPATRIE) * PERPPLAFC *3 + (1 - positif(PERPIMPATRIE)) * 0 ;
PERPPLAFCOMP = positif(PERPIMPATRIE) * PERPPLAFP *3 + (1 - positif(PERPIMPATRIE)) * 0 ;
PERPPLAFIMPV = positif(PERPIMPATRIE) * (PERPPLAFCOMV + PERPPLAFV) + (1 - positif(PERPIMPATRIE)) * 0 ;
PERPPLAFIMPC = positif(PERPIMPATRIE) * (PERPPLAFCOMC + PERPPLAFC) + (1 - positif(PERPIMPATRIE)) * 0 ;
PERPPLAFIMPP = positif(PERPIMPATRIE) * (PERPPLAFCOMP + PERPPLAFP) + (1 - positif(PERPIMPATRIE)) * 0 ;
regle 31021:
application : iliad   ;
PERP_MUT = positif(PERPMUTU)
	   * positif(V_0AM+V_0AO)
	   * (1 - positif(V_0AC+V_0AD+V_0AV))
	    ;
PERP_COND1 =  positif(PERP_MUT)
	      *((1 - positif(PERPIMPATRIE))
	      * positif(PERP_COTV+COD6NS  - PERPPLATVANT)
	      * positif(PERPPLATCANT - PERP_COTC-COD6NT)
	      + positif(PERPIMPATRIE)
	      * positif(PERP_COTV+COD6NS  - PERPPLAFIMPV)
	      * positif(PERPPLAFIMPC - PERP_COTC-COD6NT)
	      ) ;
PERP_COND2 =  positif(PERP_MUT) 
	      *((1 - positif(PERPIMPATRIE))
	      * positif(PERP_COTC+COD6NT  - PERPPLATCANT)
	      * positif(PERPPLATVANT - PERP_COTV-COD6NS)
	      + positif(PERPIMPATRIE)
	      * positif(PERP_COTC+COD6NT  - PERPPLAFIMPC)
	      * positif(PERPPLAFIMPV - PERP_COTV-COD6NS)
	      ) ;
regle 3102121:
application : iliad   ;
PERPPLAFMUTV = positif(PERP_COND1)
	      *((1 - positif(PERPIMPATRIE))
	       * (PERPPLATVANT + max(0,PERPPLATCANT - PERP_COTC-COD6NT))
	      + positif(PERPIMPATRIE)
	       * (PERPPLAFIMPV + max(0,PERPPLAFIMPC - PERP_COTC-COD6NT))
	      ) ;
PERPPLAFMUTC = positif(PERP_COND2)
	      *((1 - positif(PERPIMPATRIE))
	       * (PERPPLATCANT + max(0,PERPPLATVANT - PERP_COTV-COD6NS))
	      + positif(PERPIMPATRIE)
	       * (PERPPLAFIMPC + max(0,PERPPLAFIMPV - PERP_COTV-COD6NS))
	      ) ;
regle 310211:
application : iliad   ;
PERPPLAFMU1V = positif(PERP_COND1) 
	      *((1 - positif(PERPIMPATRIE)) * (PERPPLATVANT + RPERPMUTV)
	      + positif(PERPIMPATRIE) * (PERPPLAFIMPV + RPERPMUTV))
		+ positif(PERP_COND2) 
	      *((1 - positif(PERPIMPATRIE)) * (PERPPLATVANT - RPERPMUTC)
	      + positif(PERPIMPATRIE) * (PERPPLAFIMPV - RPERPMUTC)) ;
PERPPLAFMU1C = positif(PERP_COND1) 
	      *((1 - positif(PERPIMPATRIE)) * (PERPPLATCANT - RPERPMUTV)
	      + positif(PERPIMPATRIE) * (PERPPLAFIMPC - RPERPMUTV))
		+ positif(PERP_COND2) 
      		*((1 - positif(PERPIMPATRIE)) * (PERPPLATCANT + RPERPMUTC)
      		+positif(PERPIMPATRIE) *(PERPPLAFIMPC + RPERPMUTC)) ;
regle 31021166:
application : iliad   ;


DPERPV = PERP_COTV+COD6NS;
DPERPC = PERP_COTC+COD6NT;
DPERPP = PERP_COTP+COD6NU;
regle 310225:
application : iliad   ;
RPERPV = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2)) 
          * ((1 - positif(PERPIMPATRIE))
		 * max(0,min(PERP_COTV+COD6NS,PERPPLATVANT))
	    + positif(PERPIMPATRIE)
		 * max(0,min(PERP_COTV+COD6NS,PERPPLAFIMPV)))
	 + positif(PERP_COND1) 
		* (min(PERP_COTV+COD6NS,PERPPLAFMUTV))
	 + positif(PERP_COND2) 
          * ((1 - positif(PERPIMPATRIE))
		 * max(0,min(PERP_COTV+COD6NS,PERPPLATVANT))
	    + positif(PERPIMPATRIE)
		 * max(0,min(PERP_COTV+COD6NS,PERPPLAFIMPV)))
	  ;
RPERPC = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2)) 
          * ((1 - positif(PERPIMPATRIE))
		 * max(0,min(PERP_COTC+COD6NT,PERPPLATCANT))
	    + positif(PERPIMPATRIE)
		 * max(0,min(PERP_COTC+COD6NT,PERPPLAFIMPC)))
	 + positif(PERP_COND1) 
          * ((1 - positif(PERPIMPATRIE))
		 * max(0,min(PERP_COTC+COD6NT,PERPPLATCANT))
	    + positif(PERPIMPATRIE)
		 * max(0,min(PERP_COTC+COD6NT,PERPPLAFIMPC)))
	 + positif(PERP_COND2) * (min(PERP_COTC+COD6NT,PERPPLAFMUTC))
	  ;
RPERPP = ( (1 - positif(PERPIMPATRIE))
		 * max(0,min(PERP_COTP+COD6NU,PERPPLATPANT))
	    + positif(PERPIMPATRIE)
		 * max(0,min(PERP_COTP+COD6NU,PERPPLAFIMPP))
	  );	
regle 31022:
application : iliad   ;
APERPV = (1 - V_CNR) * max(min(RPERPV,RBG1 - RPALE - RPALP - RFACC - RDDIV - RD6DG - DDCSG + TOTALQUO -SDDD), 0);
APERPC = (1 - V_CNR) * max(min(RPERPC,RBG1 - RPALE - RPALP  - RFACC - RDDIV - RD6DG - DDCSG + TOTALQUO -SDDD - APERPV), 0);
APERPP = (1 - V_CNR) * max(min(RPERPP,RBG1 - RPALE - RPALP  - RFACC - RDDIV - RD6DG - DDCSG + TOTALQUO -SDDD - APERPV - APERPC), 0);
regle 310227:
application :  iliad   ;
PERPDCOTV = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2)) 
          * ((1 - positif(PERPIMPATRIE))
		 * min(PERP_COTV+COD6NS,PERPPLATVANT)
	    + positif(PERPIMPATRIE)
		 * min(PERP_COTV+COD6NS,PERPPLAFIMPV))
	 + positif(PERP_COND1) 
		* min(PERP_COTV+COD6NS,PERPPLAFMU1V)
	 + positif(PERP_COND2) 
          * ((1 - positif(PERPIMPATRIE))
		 * min(PERP_COTV+COD6NS,PERPPLATVANT)
	    + positif(PERPIMPATRIE)
		 * min(PERP_COTV+COD6NS,PERPPLAFIMPV))
	  ;
PERPDCOTC = (1 - positif(PERP_COND1)) * (1 - positif(PERP_COND2)) 
          * ((1 - positif(PERPIMPATRIE))
		 * min(PERP_COTC+COD6NT,PERPPLATCANT)
	    + positif(PERPIMPATRIE)
		 * min(PERP_COTC+COD6NT,PERPPLAFIMPC))
	 + positif(PERP_COND1) 
          * ((1 - positif(PERPIMPATRIE))
		 * min(PERP_COTC+COD6NT,PERPPLATCANT)
	    + positif(PERPIMPATRIE)
		 * min(PERP_COTC+COD6NT,PERPPLAFIMPC))
	 + positif(PERP_COND2) * min(PERP_COTC+COD6NT,PERPPLAFMU1C)
	  ;
PERPDCOTP = ( (1 - positif(PERPIMPATRIE))
		 * min(PERP_COTP+COD6NU,PERPPLATPANT)
	    + positif(PERPIMPATRIE)
		 * min(PERP_COTP+COD6NU,PERPPLAFIMPP)
	  ) ;	
regle 31023:
application : iliad   ;
RPERPMUTV = positif(PERP_COND1) 
	      *((1 - positif(PERPIMPATRIE))
		* max(0,min(PERP_COTV+COD6NS - PERPPLATVANT,PERPPLATCANT - PERP_COTC-COD6NT))
	      + positif(PERPIMPATRIE)
		* max(0,min(PERP_COTV+COD6NS - PERPPLAFIMPV,PERPPLAFIMPC - PERP_COTC-COD6NT))
		) ;
RPERPMUTC = positif(PERP_COND2) 
	      *((1 - positif(PERPIMPATRIE))
		* max(0,min(PERP_COTC+COD6NT - PERPPLATCANT,PERPPLATVANT - PERP_COTV-COD6NS))
	      + positif(PERPIMPATRIE)
		* max(0,min(PERP_COTC+COD6NT - PERPPLAFIMPC,PERPPLAFIMPV - PERP_COTV-COD6NS))
		) ;
regle 31024:
application : iliad   ;
IND_BTANC = null(V_IND_TRAIT -4)
           * (positif(APPLI_OCEANS) * 1
	    + positif(APPLI_COLBERT)
	    + positif(APPLI_BATCH) * V_BTANC
	    + positif(APPLI_ILIAD) * ( positif(V_CALCULIR) * 1
				     + (1 - positif(V_CALCULIR)) * V_BTANC)
	     )
	     + null(V_IND_TRAIT - 5) * 1 ;
PERPINDAFFV = positif(PERPINDV 
		* (1 - V_CNR) * (1 - positif(ANNUL2042))
		* positif(PERPSALNV) * positif(present(PRBRV)+present(PALIV)+present(COD1AI)+present(PCAPTAXV)) * (1-positif(PERP_COTV+COD6NS)) * (1-positif(PERPMUTU))
		* ((null(IND_BTANC - 1)
		* (positif(PERPIMPATRIE+0)
		* positif(PERPPLAFNUNV+PERPPLAFV+positif_ou_nul(PERPV+COD6OS)*positif(PERPREVTOTV))
		+ (1 - positif(PERPIMPATRIE+0))
		* (present(PERPPLAFCV) + present(V_BTPERPV)) 
		* (present(PERPPLAFNUV1) + present(V_BTPERPNUV1))
	        * (present(PERPPLAFNUV2) + present(V_BTPERPNUV2))
		* (present(PERPPLAFNUV3) + present(V_BTPERPNUV3))
	        ))
		+((null(IND_BTANC - 2)
		* positif(V_BTPERPV + V_BTPERPNUV1 + V_BTPERPNUV2 + V_BTPERPNUV3
		     + PERPPLAFCV + PERPPLAFNUV1 + PERPPLAFNUV2 + PERPPLAFNUV3))))) ;
PERPINDAFFC = positif(PERPINDC 
		* (1 - V_CNR) * (1 - positif(ANNUL2042))
		* positif(PERPSALNC) * positif(present(PRBRC)+present(PALIC)+present(COD1BI)+present(PCAPTAXC)) * (1-positif(PERP_COTC+COD6NT)) * (1-positif(PERPMUTU))
		* ((null(IND_BTANC - 1)
		* (positif(PERPIMPATRIE+0)
		* positif(PERPPLAFNUNC+PERPPLAFC+positif_ou_nul(PERPC+COD6OT)*positif(PERPREVTOTC))
		+ (1 - positif(PERPIMPATRIE+0))
		* (present(PERPPLAFCC) + present(V_BTPERPC)) 
		* (present(PERPPLAFNUC1) + present(V_BTPERPNUC1))
	        * (present(PERPPLAFNUC2) + present(V_BTPERPNUC2))
		* (present(PERPPLAFNUC3) + present(V_BTPERPNUC3))
	        ))
		+((null(IND_BTANC - 2)
		* positif(V_BTPERPC + V_BTPERPNUC1 + V_BTPERPNUC2 + V_BTPERPNUC3
		     + PERPPLAFCC + PERPPLAFNUC1 + PERPPLAFNUC2 + PERPPLAFNUC3))))) ;
PERPINDAFFP = positif(PERPINDP 
		* (1 - V_CNR) * (1 - positif(ANNUL2042))
		* positif(PERPSALNP) * positif(present(PRBR1)+present(PRBR2)+present(PRBR3)+present(PRBR4)+present(PALIP)+present(COD1CI)+present(COD1DI)+present(COD1EI)+present(COD1FI)) 
		* (1-positif(PERP_COTP+COD6NU)) * (1-positif(PERPMUTU))
		* ((null(IND_BTANC - 1)
		* (positif(PERPIMPATRIE+0)
		* positif(PERPPLAFNUNP+PERPPLAFP+positif_ou_nul(PERPP+COD6OU)*positif(PERPREVTOTP))
		+ (1 - positif(PERPIMPATRIE+0))
		* (present(PERPPLAFCP) + present(V_BTPERPP)) 
		* (present(PERPPLAFNUP1) + present(V_BTPERPNUP1))
	        * (present(PERPPLAFNUP2) + present(V_BTPERPNUP2))
		* (present(PERPPLAFNUP3) + present(V_BTPERPNUP3))
	        ))
		+((null(IND_BTANC - 2)
		* positif(V_BTPERPP + V_BTPERPNUP1 + V_BTPERPNUP2 + V_BTPERPNUP3
		     + PERPPLAFCP + PERPPLAFNUP1 + PERPPLAFNUP2 + PERPPLAFNUP3))))) ;

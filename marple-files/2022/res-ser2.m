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
 #
 #
 # #####   ######   ####    #####     #     #####  
 # #    #  #       #          #       #       #   
 # #    #  #####    ####      #       #       #  
 # #####   #            #     #       #       # 
 # #   #   #       #    #     #       #       # 
 # #    #  ######   ####      #       #       # 
 #
 #      #####   #####   #####   #
 #          #   #   #   #   #   #
 #      #####   #   #   #   #   #
 #      #       #   #   #   #   #
 #      #####   #####   #####   #
 #
 #
 #
 #
 #                     RES-SER2.m
 #                    =============
 #
 #
 #                      zones restituees par l'application
 #
 #
regle 221000:
application : iliad  ;


IDRS = INDTXMIN*IMI + 
       INDTXMOY*IMO + 
       (1-INDTXMIN) * (1-INDTXMOY) * max(0,IPHQ2 - ADO1) ;

regle 221010:
application : iliad  , bareme ;


RECOMP = max(0 ,( IPHQANT2 - IPHQ2 )*(1-INDTXMIN) * (1-INDTXMOY)) 
         * (1 - positif(IPMOND+INDTEFF)) ;

regle 221020:
application : iliad  ;


IDRSANT = INDTXMIN*IMI + INDTXMOY*IMO 
         + (1-INDTXMIN) * (1-INDTXMOY) * max(0,IPHQANT2 - ADO1) ;


IDRS2 = (1 - positif(IPMOND+INDTEFF))  * 
        ( 
         ((IDRSANT + ( positif(ABADO)*ABADO + positif(ABAGU)*ABAGU ))
                  * positif(IDRSANT)
         + IPHQANT2 * (1 - positif(IDRSANT))) * (1-positif(RE168+TAX1649))
         + positif(RE168+TAX1649) * IAMD2
        )
   + positif(IPMOND+INDTEFF) 
         * ( IDRS*(1-positif(IPHQ2)) + IPHQ2 * positif(IPHQ2) );

IDRS3 = IDOM11 ;

IDRS4 = max(0 , IDRS3 - IDEC) ;

regle 221030:
application : iliad  ;


PLAFQF = positif(IS521 - PLANT - IS511) * ( positif(abs(TEFF)) * positif(IN51) + (1 - positif(abs(TEFF))) );

regle 221040:
application : iliad  ;

REVMETRO = max(0,RG - PRODOM - PROGUY);

regle 221050:
application : iliad  ;


RGPAR =   positif(positif(PRODOM)+positif(CODDAJ)+positif(CODDBJ)) * 1 
       +  positif(positif(PROGUY)+positif(CODEAJ)+positif(CODEBJ)) * 2
       +  positif(positif(PROGUY)+positif(CODEAJ)+positif(CODEBJ))*positif(positif(PRODOM)+positif(CODDAJ)+positif(CODDBJ)) 
       ;

regle 221060:
application : iliad  ;


IBAEX = (IPQT2) * (1 - INDTXMIN) * (1 - INDTXMOY) ;

regle 221070:
application : iliad  ;


PRELIB = PPLIB + RCMLIB + COD2RA + COD2XX + COD2VM ;

regle 221080:
application : iliad  ;


IDEC = DEC11 * (1 - V_CNR) ;

regle 221090:
application : iliad  ;


IPROP = ITP ;

regle 221100:
application : iliad  ;


IREP = REI ;

regle 221110:
application :  iliad ;


RETIR = RETIR2 + arr(BTO * TXINT/100) * (1-INDTXMIN) + arr(BTO * TXINT/100) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN ;

RETTAXA = RETTAXA2 + arr(max(0,TAXASSUR-TAXA9YI- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT)) * TXINT/100) * (1-INDTXMIN)
                   + arr(max(0,TAXASSUR-TAXA9YI- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT)) * TXINT/100) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN;

RETPCAP = RETPCAP2+arr(max(0,IPCAPTAXT-CAP9YI- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))+min(0,IRN - IRANT+TAXASSUR)) * TXINT/100) * (1-INDTXMIN)
                  +arr(max(0,IPCAPTAXT-CAP9YI- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))+min(0,IRN - IRANT+TAXASSUR)) * TXINT/100) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN;

RETHAUTREV = RETCHR2 + arr(max(0 , IHAUTREVT + CHRPVIMP - CHR9YI + min(0,IRN - IRANT + TAXASSUR + IPCAPTAXT)) * TXINT/100) * (1-INDTXMIN)
                     + arr(max(0 , IHAUTREVT + CHRPVIMP - CHR9YI + min(0,IRN - IRANT + TAXASSUR + IPCAPTAXT)) * TXINT/100) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN ;

RETCS = (RETCS2 + arr(max(0, CSGC-CS9YP-CICSG-CSGIM) * TXINT/100))* positif_ou_nul(CSTOTSSPENA - SEUIL_61) ;

RETRD = (RETRD2 + arr(max(0, RDSC-RD9YP-CRDSIM-CIRDS) * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RETPSOL = (RETPSOL2  + arr(max(0, MPSOL-PS9YP-CIPSOL-PRSPROV) * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RETCVN = (RETCVN2 + arr(max(0, CVNN-CVN9YP - COD8YT) * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);


RETCDIS = (RETCDIS2 + arr(max(0, CDIS-CDIS9YP - CDISPROV) * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
RETCSG820 = (RETCSG8202 + arr(max(0, MCSG820-C8209YP-COD8ZH) * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RETGLOA = (RETGLOA2 + arr(GLOBASE * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RETRSE1 = (RETRSE12 + arr(max(0, RSE1N-RSE19YP-CSPROVYD) * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RETRSE2 = (RETRSE22 + arr((max(0, max(0, RSE8TV - CIRSE8TV - CSPROVYF) + max(0, RSE8SA -CIRSE8SA - CSPROVYN) - RSE29YP)) * TXINT/100
                        )) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RETRSE3 = (RETRSE32 + arr(max(0, RSE3N-RSE39YP-CSPROVYG) * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RETRSE4 = (RETRSE42 + arr((max(0, RSE4N - CSPROVYH - CSPROVYP - RSE49YP)) * TXINT/100
                        )) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RETRSE5 = (RETRSE52 + arr(max(0, RSE5N-RSE59YP-CSPROVYE) * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

RETRSE6 = (RETRSE62 + arr(RSE6BASE * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
RETRSE8 = (RETRSE82 + arr(RSE8BASE * TXINT/100)) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

regle 221120:
application :  iliad ;

MAJOIRTARDIF_A1 = MAJOIRTARDIF_A - MAJOIR17_2TARDIF_A;
MAJOTAXATARDIF_A1 = MAJOTAXATARDIF_A - MAJOTA17_2TARDIF_A;
MAJOCAPTARDIF_A1 = MAJOCAPTARDIF_A - MAJOCP17_2TARDIF_A;
MAJOHRTARDIF_A1 = MAJOHRTARDIF_A - MAJOHR17_2TARDIF_A;
MAJOIRTARDIF_D1 = ( MAJOIRTARDIF_D - MAJOIR17_2TARDIF_D ) * (1 - null(CODE_2042-17));
MAJOTAXATARDIF_D1 = ( MAJOTAXATARDIF_D - MAJOTA17_2TARDIF_D ) * (1 - null(CODE_2042-17));
MAJOCAPTARDIF_D1 = ( MAJOCAPTARDIF_D - MAJOCP17_2TARDIF_D ) * (1 - null(CODE_2042-17));
MAJOHRTARDIF_D1 = ( MAJOHRTARDIF_D - MAJOHR17_2TARDIF_D ) * (1 - null(CODE_2042-17));
MAJOIRTARDIF_P1 = MAJOIRTARDIF_P - MAJOIR17_2TARDIF_P;
MAJOHRTARDIF_P1 = MAJOHRTARDIF_P - MAJOHR17_2TARDIF_P;
MAJOIRTARDIF_R1 = MAJOIRTARDIF_R - MAJOIR17_2TARDIF_R;
MAJOTAXATARDIF_R1 = MAJOTAXATARDIF_R - MAJOTA17_2TARDIF_R;
MAJOCAPTARDIF_R1 = MAJOCAPTARDIF_R - MAJOCP17_2TARDIF_R;
MAJOHRTARDIF_R1 = MAJOHRTARDIF_R - MAJOHR17_2TARDIF_R;


NMAJ1 = max(0,MAJO1728IR + arr(BTO * (1-positif(null(CMAJ-10)+null(CMAJ-17)+null(CMAJ-07)))* COPETO/100) * (1-INDTXMIN)
                + arr(BTO * (1-positif(null(CMAJ-10)+null(CMAJ-17)+null(CMAJ-07)))* COPETO/100
		     ) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN 

		+ FLAG_TRTARDIF * MAJOIR08TARDIF_D

		+ FLAG_TRTARDIF_F * (positif(PROPIR_A) * MAJOIR08TARDIF_P
		                     + (1 - positif(PROPIR_A) ) * MAJOIR08TARDIF_D
				    )
		- FLAG_TRTARDIF_F * (1 - positif(PROPIR_A))
				  * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718) * MAJOIR08TARDIF_R
				      + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718) ) * MAJOIR08TARDIF_A
				    )
		);


NMAJTAXA1 = max(0,MAJO1728TAXA + arr(max(0,TAXASSUR-TAXA9YI - min(TAXASSUR+0-TAXA9YI,max(0,INE-IRB+AVFISCOPTER))
                                                    + min(0,IRN-IRANT)
				        ) * (1-positif(null(CMAJ-10)+null(CMAJ-17)+null(CMAJ-07))) * COPETO/100
				    ) * (1-INDTXMIN)
                                + arr(max(0,TAXASSUR -TAXA9YI- min(TAXASSUR+0-TAXA9YI,max(0,INE-IRB+AVFISCOPTER))
				                     + min(0,IRN-IRANT)
				         ) *(1-positif(null(CMAJ-10)+null(CMAJ-17)+null(CMAJ-07))) * COPETO/100
				     ) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN

		+ FLAG_TRTARDIF * MAJOTAXA08TARDIF_D

		+ FLAG_TRTARDIF_F * MAJOTAXA08TARDIF_D
        	- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJOTAXA08TARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJOTAXA08TARDIF_A)
		);


NMAJPCAP1 = max(0,MAJO1728PCAP + arr(max(0,IPCAPTAXT -CAP9YI- min(IPCAPTAXT+0-CAP9YI,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))
                                                     + min(0,IRN-IRANT+TAXASSUR)
				        ) * (1-positif(null(CMAJ-10)+null(CMAJ-17)+null(CMAJ-07))) * COPETO/100
				    )  * (1-INDTXMIN)
                               + arr(max(0,IPCAPTAXT -CAP9YI- min(IPCAPTAXT+0-CAP9YI,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))
			                             + min(0,IRN-IRANT+TAXASSUR)
				        ) * (1-positif(null(CMAJ-10)+null(CMAJ-17)+null(CMAJ-07))) * COPETO/100
				    ) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN

                + FLAG_TRTARDIF * MAJOCAP08TARDIF_D

                + FLAG_TRTARDIF_F * MAJOCAP08TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJOCAP08TARDIF_R
                                      + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJOCAP08TARDIF_A)
                );


NMAJCHR1 = max(0,MAJO1728CHR + arr(max(0,IHAUTREVT-CHR9YI+CHRPVIMP + min(0 , IRN - IRANT + TAXASSUR + IPCAPTAXT)
				      ) * (1-positif(null(CMAJ-10)+null(CMAJ-17)+null(CMAJ-07))) * COPETO/100
				  ) * (1-INDTXMIN)
                             + arr(max(0,IHAUTREVT-CHR9YI+CHRPVIMP + min(0 , IRN - IRANT + TAXASSUR + IPCAPTAXT)
			              ) * (1-positif(null(CMAJ-10)+null(CMAJ-17)+null(CMAJ-07))) * COPETO/100
				  ) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN
                + FLAG_TRTARDIF * MAJOHR08TARDIF_D
		+ FLAG_TRTARDIF_F * (positif(PROPIR_A) * MAJOHR08TARDIF_P
		                     + (1 - positif(PROPIR_A) ) * MAJOHR08TARDIF_D)
		- FLAG_TRTARDIF_F * (1 - positif(PROPIR_A))
				    * ( positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 ) * MAJOHR08TARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJOHR08TARDIF_A)
              );


NMAJC1 = max(0,MAJO1728CS + arr((CSGC-CS9YP - CSGIM - CICSG) * COPETO/100)  
		+ FLAG_TRTARDIF * MAJOCSTARDIF_D
		+ FLAG_TRTARDIF_F 
		* (positif(PROPCS_A) * MAJOCSTARDIF_P 
		  + (1 - positif(PROPCS_A) ) * MAJOCSTARDIF_D)
		- FLAG_TRTARDIF_F * (1 - positif(PROPCS_A))
				    * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718)  * MAJOCSTARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJOCSTARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJR1 = max(0,MAJO1728RD + arr((RDSC -RD9YP- CRDSIM - CIRDS) * COPETO/100) 
		+ FLAG_TRTARDIF * MAJORDTARDIF_D
		+ FLAG_TRTARDIF_F 
		* (positif(PROPRD_A) * MAJORDTARDIF_P 
		  + (1 - positif(PROPRD_A) ) * MAJORDTARDIF_D)
		- FLAG_TRTARDIF_F * (1 - positif(PROPCS_A))
				    * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJORDTARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJORDTARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
regle 221125:
application : iliad  ;

NMAJPSOL1 = max(0,MAJO1728PSOL  + arr((MPSOL -PS9YP-CIPSOL -PRSPROV) * COPETO/100)
		+ FLAG_TRTARDIF * MAJOPSOLTARDIF_D
		+ FLAG_TRTARDIF_F 
		* (positif(PROPPSOL_A) * MAJOPSOLTARDIF_P 
		  + (1 - positif(PROPPSOL_A) ) * MAJOPSOLTARDIF_D)
		- FLAG_TRTARDIF_F * (1 - positif(PROPPSOL_A))
				    * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJOPSOLTARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJOPSOLTARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);


NMAJCVN1 = max(0,MAJO1728CVN + arr((CVNN -CVN9YP- COD8YT) * COPETO/100)
		+ FLAG_TRTARDIF * MAJOCVNTARDIF_D
		+ FLAG_TRTARDIF_F  * MAJOCVNTARDIF_D
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJOCVNTARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJOCVNTARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

NMAJCDIS1 = max(0,MAJO1728CDIS + arr((CDIS -CDIS9YP- CDISPROV) * COPETO/100)  * (1 - V_CNR)
		+ FLAG_TRTARDIF * MAJOCDISTARDIF_D
		+ FLAG_TRTARDIF_F  * MAJOCDISTARDIF_D
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJOCDISTARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJOCDISTARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJC8201 = max(0,MAJO1728C820 + arr((MCSG820-C8209YP-COD8ZH) * COPETO/100)  * (1 - V_CNR)
		+ FLAG_TRTARDIF * MAJOC820TARDIF_D
		+ FLAG_TRTARDIF_F  * MAJOC820TARDIF_D
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJOC820TARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJOC820TARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

NMAJGLO1 = max(0,MAJO1728GLO + arr((GLOBASE) * COPETO/100)
                + FLAG_TRTARDIF * MAJOGLOTARDIF_D
                + FLAG_TRTARDIF_F  * MAJOGLOTARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJOGLOTARDIF_R
                                     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJOGLOTARDIF_A)
              ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

NMAJRSE11 = max(0,MAJO1728RSE1 + arr((RSE1N -RSE19YP- CSPROVYD) * COPETO/100)  
		+ FLAG_TRTARDIF * MAJORSE1TARDIF_D
		+ FLAG_TRTARDIF_F  * MAJORSE1TARDIF_D
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJORSE1TARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJORSE1TARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

NMAJRSE21 = max(0,MAJO1728RSE2 + arr(( max(0, RSE8TV -RSE29YP- CIRSE8TV - CSPROVYF) + max(0, RSE8SA -CIRSE8SA - CSPROVYN )) * COPETO/100) * (1 - V_CNR)
		+ FLAG_TRTARDIF * MAJORSE2TARDIF_D
		+ FLAG_TRTARDIF_F  * MAJORSE2TARDIF_D
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJORSE2TARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJORSE2TARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

NMAJRSE31 = max(0,MAJO1728RSE3 + arr((RSE3N -RSE39YP- CSPROVYG)* COPETO/100) 
		+ FLAG_TRTARDIF * MAJORSE3TARDIF_D
		+ FLAG_TRTARDIF_F  * MAJORSE3TARDIF_D
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJORSE3TARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJORSE3TARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

NMAJRSE41 = max(0,MAJO1728RSE4 + arr((RSE4N -RSE49YP- CSPROVYH - CSPROVYP) * COPETO/100) 
		+ FLAG_TRTARDIF * MAJORSE4TARDIF_D
		+ FLAG_TRTARDIF_F  * MAJORSE4TARDIF_D
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJORSE4TARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJORSE4TARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

NMAJRSE51 = max(0,MAJO1728RSE5 + arr((RSE5N -RSE59YP- CSPROVYE) * COPETO/100) 
		+ FLAG_TRTARDIF * MAJORSE5TARDIF_D
		+ FLAG_TRTARDIF_F  * MAJORSE5TARDIF_D
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJORSE5TARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJORSE5TARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

NMAJRSE61 = max(0,MAJO1728RSE6 + arr(RSE6BASE * COPETO/100) 
		+ FLAG_TRTARDIF * MAJORSE6TARDIF_D
		+ FLAG_TRTARDIF_F  * MAJORSE6TARDIF_D
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJORSE6TARDIF_R
				     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJORSE6TARDIF_A)
		) * positif_ou_nul(CSTOTSSPENA - SEUIL_61) ;

NMAJRSE81 = max(0,MAJO1728RSE8 + arr(RSE8BASE* COPETO/100)
                + FLAG_TRTARDIF * MAJORSE8TARDIF_D
                + FLAG_TRTARDIF_F  * MAJORSE8TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * positif(FLAG_RETARD0718 )* MAJORSE8TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) * positif(FLAG_RETARD0718 )) * MAJORSE8TARDIF_A)
                ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61) ;



NMAJ3 = max(0,MAJO1758AIR + arr(BTO * 2 * COPETO/100) * null(CMAJ-17) 
                + arr(BTO * COPETO/100) * positif(null(CMAJ-10)+null(CMAJ-07))
                + FLAG_TRTARDIF * (MAJOIR17_2TARDIF_D+MAJOIR07TARDIF_D+MAJOIR02TARDIF_D)
		+ FLAG_TRTARDIF_F * (positif(PROPIR_A) * (MAJOIR17_2TARDIF_P +MAJOIR07TARDIF_P+MAJOIR02TARDIF_P)
	                     + (1 - positif(PROPIR_A) ) * (MAJOIR17_2TARDIF_D+MAJOIR07TARDIF_D+MAJOIR02TARDIF_D))
        	- FLAG_TRTARDIF_F * (1 - positif(PROPIR_A))
				    * ( positif(FLAG_RECTIF) * (MAJOIR17_2TARDIF_R+MAJOIR07TARDIF_R+MAJOIR02TARDIF_R)
				        + (1 - positif(FLAG_RECTIF) ) * (MAJOIR17_2TARDIF_A+MAJOIR07TARDIF_A+max(0,MAJOIR02TARDIF_A+(MAJOTARDCOA_A-MAJOTARDCOA)*null(CSTRATE99-2)))
				      )
	   );


NMAJTAXA3 = max(0,MAJO1758ATAXA + arr(max(0,TAXASSUR-TAXA9YI+min(0,IRN-IRANT)) * 2 * COPETO/100)
			              * null(CMAJ-17) 
                + arr(max(0,TAXASSUR-TAXA9YI+min(0,IRN-IRANT)) * COPETO/100) * positif(null(CMAJ-10)+null(CMAJ-07))
		+  FLAG_TRTARDIF * (MAJOTA17_2TARDIF_D+MAJOTAXA07TARDIF_D+MAJOTAXA02TARDIF_D) * (1-positif(MAJO1758ATAXA))
		+ FLAG_TRTARDIF_F * (MAJOTA17_2TARDIF_D +MAJOTAXA07TARDIF_D+MAJOTAXA02TARDIF_D)
		- FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * (MAJOTA17_2TARDIF_R +MAJOTAXA07TARDIF_R+MAJOTAXA02TARDIF_R)
				        + (1 - positif(FLAG_RECTIF) ) * (MAJOTA17_2TARDIF_A+MAJOTAXA07TARDIF_A+max(0,MAJOTAXA02TARDIF_A+(MAJOTARDCOU_A-MAJOTARDCOU)*null(CSTRATE99-2)))
				    )
	        );


NMAJPCAP3 = max(0,MAJO1758APCAP
                   + arr(max(0,IPCAPTAXT-CAP9YI+min(0,IRN-IRANT+TAXASSUR)) * 2 * COPETO/100)
                            * null(CMAJ-17)
                    + arr(max(0,IPCAPTAXT-CAP9YI+min(0,IRN-IRANT+TAXASSUR)) * COPETO/100) * positif(null(CMAJ-10)+null(CMAJ-07))
                       + FLAG_TRTARDIF * (MAJOCP17_2TARDIF_D+MAJOCAP07TARDIF_D+MAJOCAP02TARDIF_D)
                       + FLAG_TRTARDIF_F * (MAJOCP17_2TARDIF_D +MAJOCAP07TARDIF_D+MAJOCAP02TARDIF_D)
                     - FLAG_TRTARDIF_F *  (positif(FLAG_RECTIF) * (MAJOCP17_2TARDIF_R +MAJOCAP07TARDIF_R+MAJOCAP02TARDIF_R)
                                             + (1 - positif(FLAG_RECTIF)) * (MAJOCP17_2TARDIF_A+MAJOCAP07TARDIF_A
                                                      + max(0,MAJOCAP02TARDIF_A+(MAJOTARDCOV_A-MAJOTARDCOV)*null(CSTRATE99-2)) )
                                         )
                    );


NMAJCHR3 = max(0,MAJO1758ACHR 
                 + arr(max(0,IHAUTREVT-CHR9YI+CHRPVIMP+min(0,IRN-IRANT+TAXASSUR+IPCAPTAXT)) 
                       * 2 * COPETO/100) * null(CMAJ-17)
                + arr(max(0,IHAUTREVT-CHR9YI+CHRPVIMP+min(0,IRN-IRANT+TAXASSUR+IPCAPTAXT)) * COPETO/100) * positif(null(CMAJ-10)+null(CMAJ-07))
                + FLAG_TRTARDIF * (MAJOHR17_2TARDIF_D+MAJOHR07TARDIF_D+MAJOHR02TARDIF_D)
		+ FLAG_TRTARDIF_F * (positif(PROPHR_A) * (MAJOHR17_2TARDIF_P+MAJOHR07TARDIF_P+MAJOHR02TARDIF_P)
				     + (1 - positif(PROPHR_A) ) * (MAJOHR17_2TARDIF_D+MAJOHR07TARDIF_D+MAJOHR02TARDIF_D))
		- FLAG_TRTARDIF_F * (1 - positif(PROPHR_A))
				  * ( positif(FLAG_RECTIF) * (MAJOHR17_2TARDIF_R+MAJOHR07TARDIF_R+MAJOHR02TARDIF_R)
				     + (1 - positif(FLAG_RECTIF) ) * (MAJOHR17_2TARDIF_A+MAJOHR07TARDIF_A+max(0,MAJOHR02TARDIF_A+(MAJOTARDCOX_A-MAJOTARDCOX)*null(CSTRATE99-2)))
				     )
		);


NMAJ4    =      (somme (i=03..06,30,32,35,55: MAJOIRi))
                + FLAG_TRTARDIF * MAJOIR03TARDIF_D 
                + FLAG_TRTARDIF_F * (positif(PROPIR_A) * MAJOIR03TARDIF_P
                                    + (1 - positif(PROPIR_A) ) * MAJOIR03TARDIF_D)
                     - FLAG_TRTARDIF_F * (1 - positif(PROPIR_A))
                                         * ( positif(FLAG_RECTIF) * MAJOIR03TARDIF_R
                                         + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOIR03TARDIF_A+(MAJOTARDCOA_A-MAJOTARDCOA)*null(CSTRATE99-3)))
			 ;
NMAJTAXA4  =    (somme (i=03..06,35,55: MAJOTAXAi))
                + FLAG_TRTARDIF * MAJOTAXA03TARDIF_D
                + FLAG_TRTARDIF_F * MAJOTAXA03TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJOTAXA03TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOTAXA03TARDIF_A+(MAJOTARDCOU_A-MAJOTARDCOU)*null(CSTRATE99-3))) 
                 ;
NMAJPCAP4 =  somme(i=03..06,35,55:MAJOCAPi)
               + FLAG_TRTARDIF * MAJOCAP03TARDIF_D
                + FLAG_TRTARDIF_F * MAJOCAP03TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF)  * MAJOCAP03TARDIF_R
                                     + (1 - positif(FLAG_RECTIF)) 
                                                      * max(0,MAJOCAP03TARDIF_A+(MAJOTARDCOV_A-MAJOTARDCOV)*null(CSTRATE99-3)))
				     ;
NMAJCHR4 =  (somme(i=03..06,30,32,35,55:MAJOHRi))
                + FLAG_TRTARDIF * MAJOHR03TARDIF_D 
                + FLAG_TRTARDIF_F * (positif(PROPIR_A) * MAJOHR03TARDIF_P
                                     + (1 - positif(PROPIR_A) ) * MAJOHR03TARDIF_D)
                     - FLAG_TRTARDIF_F * (1 - positif(PROPIR_A))
                                         * ( positif(FLAG_RECTIF) * MAJOHR03TARDIF_R
                                      + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOHR03TARDIF_A+(MAJOTARDCOX_A-MAJOTARDCOX)*null(CSTRATE99-3))) 
		;

NMAJC4 =  (somme(i=03..06,30,32,35,55:MAJOCSi))
                + FLAG_TRTARDIF * MAJOCS03TARDIF_D
                + FLAG_TRTARDIF_F
                * (positif(PROPCS_A) * MAJOCS03TARDIF_P
                  + (1 - positif(PROPCS_A) ) * MAJOCS03TARDIF_D)
                  - FLAG_TRTARDIF_F * (1 - positif(PROPCS_A))
                                      * ( positif(FLAG_RECTIF) * MAJOCS03TARDIF_R
                                           + (1 - positif(FLAG_RECTIF)) * max(0,MAJOCS03TARDIF_A+(MAJOTARDCOB_A-MAJOTARDCOB)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJR4 =  (somme(i=03..06,30,32,35,55:MAJORDi))
                + FLAG_TRTARDIF * MAJORD03TARDIF_D
               + FLAG_TRTARDIF_F
                * (positif(PROPRD_A) * MAJORD03TARDIF_P
                  + (1 - positif(PROPRD_A) ) * MAJORD03TARDIF_D)
                  - FLAG_TRTARDIF_F * (1 - positif(PROPCS_A))
                                      * ( positif(FLAG_RECTIF) * MAJORD03TARDIF_R
                                           + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORD03TARDIF_A+(MAJOTARDCOR_A-MAJOTARDCOR)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJPSOL4 =  (somme(i=03..06,30,32,35,55:MAJOPSOLi))
                + FLAG_TRTARDIF * MAJOPSOL03TARDIF_D
                + FLAG_TRTARDIF_F
                * (positif(PROPPS_A) * MAJOPSOL03TARDIF_P
                  + (1 - positif(PROPPS_A) ) * MAJOPSOL03TARDIF_D)
                  - FLAG_TRTARDIF_F * (1 - positif(PROPPS_A))
                                      * ( positif(FLAG_RECTIF) * MAJOPSOL03TARDIF_R
                                           + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOPSOL03TARDIF_A+(MAJOTARDCOD_A-MAJOTARDCOD)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJCVN4 =  (somme(i=03..06,35,55:MAJOCVNi))
                + FLAG_TRTARDIF * MAJOCVN03TARDIF_D
                + FLAG_TRTARDIF_F * MAJOCVN03TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJOCVN03TARDIF_R
                                    + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOCVN03TARDIF_A+(MAJOTARDCOE_A-MAJOTARDCOE)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJCDIS4 =  (somme(i=03..06,35,55:MAJOCDISi))
                + FLAG_TRTARDIF * MAJOCDIS03TARDIF_D
                + FLAG_TRTARDIF_F * MAJOCDIS03TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJOCDIS03TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOCDIS03TARDIF_A+(MAJOTARDCOF_A-MAJOTARDCOF)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJC8204 =  (somme(i=03..06,35,55:MAJOC820i))
                + FLAG_TRTARDIF * MAJOC82003TARDIF_D 
                + FLAG_TRTARDIF_F * MAJOC82003TARDIF_D 
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJOC82003TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOC82003TARDIF_A+(MAJOTARDCOQ_A-MAJOTARDCOQ)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJGLO4 =  (somme(i=03..06,35,55:MAJOGLOi))
                + FLAG_TRTARDIF * MAJOGLO03TARDIF_D 
                + FLAG_TRTARDIF_F * MAJOGLO03TARDIF_D 
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJOGLO03TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJOGLO03TARDIF_A+(MAJOTARDCOG_A-MAJOTARDCOG)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJRSE14 =  (somme(i=03..06,35,55:MAJORSE1i))
                + FLAG_TRTARDIF * MAJORSE103TARDIF_D
                + FLAG_TRTARDIF_F * MAJORSE103TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJORSE103TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE103TARDIF_A+(MAJOTARDCOT_A-MAJOTARDCOT)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJRSE24 =  (somme(i=03..06,35,55:MAJORSE2i))
                + FLAG_TRTARDIF * MAJORSE203TARDIF_D
                + FLAG_TRTARDIF_F * MAJORSE203TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJORSE203TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE203TARDIF_A+(MAJOTARDCOL_A-MAJOTARDCOL)*null(CSTRATE99-3))) 
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJRSE34 =  (somme(i=03..06,35,55:MAJORSE3i))
                + FLAG_TRTARDIF * MAJORSE303TARDIF_D
                + FLAG_TRTARDIF_F * MAJORSE303TARDIF_D
               - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJORSE303TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE303TARDIF_A+(MAJOTARDCOM_A-MAJOTARDCOM)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJRSE44 =  (somme(i=03..06,35,55:MAJORSE4i))
                + FLAG_TRTARDIF * MAJORSE403TARDIF_D
                + FLAG_TRTARDIF_F * MAJORSE403TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJORSE403TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE403TARDIF_A+(MAJOTARDCOO_A-MAJOTARDCOO)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJRSE54 =  (somme(i=03..06,35,55:MAJORSE5i))
              + FLAG_TRTARDIF * MAJORSE503TARDIF_D 
              + FLAG_TRTARDIF_F * MAJORSE503TARDIF_D 
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJORSE503TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE503TARDIF_A+(MAJOTARDCOJ_A-MAJOTARDCOJ)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJRSE64 =  (somme(i=03..06,35,55:MAJORSE6i))
                + FLAG_TRTARDIF * MAJORSE603TARDIF_D 
                + FLAG_TRTARDIF_F * MAJORSE603TARDIF_D 
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJORSE603TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE603TARDIF_A+(MAJOTARDCOP_A-MAJOTARDCOP)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
NMAJRSE84 =  (somme(i=03..06,35,55:MAJORSE8i))
                + FLAG_TRTARDIF * MAJORSE803TARDIF_D
                + FLAG_TRTARDIF_F * MAJORSE803TARDIF_D
                - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJORSE803TARDIF_R
                                     + (1 - positif(FLAG_RECTIF) ) * max(0,MAJORSE803TARDIF_A+(MAJOTARDCOH_A-MAJOTARDCOH)*null(CSTRATE99-3)))
                         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
regle isf 221130:
application :  iliad ;

MAJOISFTARDIF_A1 = MAJOISFTARDIF_A - MAJOISF17TARDIF_A;
MAJOISFTARDIF_D1 = MAJOISFTARDIF_D - MAJOISF17TARDIF_D;
MAJOISFTARDIF_R1 = MAJOISFTARDIF_R - MAJOISF17TARDIF_R;
NMAJISF1BIS = max(0,MAJO1728ISF + arr(ISF4BASE * COPETO/100)
                   + FLAG_TRTARDIF * MAJOISFTARDIF_D
                   + FLAG_TRTARDIF_F * MAJOISFTARDIF_D
                   - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIF) * MAJOISFTARDIF_R
					 + (1 - positif(FLAG_RECTIF)) * MAJOISFTARDIF_A)
                 );

regle 221140:
application : iliad  ;


IAVIM = IRB + PTOT + TAXASSUR + PTAXA + IPCAPTAXTOT + PPCAP + CHRAPRES + CHRPVIMP + PHAUTREV ;

regle 221150:
application : iliad  ;


CDBA = positif_ou_nul(SEUIL_IMPDEFBA-SHBA-(REVTP-BA1)
      -REVQTOTQHT);
AGRBG = SHBA + (REVTP-BA1) + REVQTOTQHT ;

regle 221170:
application : iliad  ;

RBAT = BAHQT;
regle 221180:
application : iliad  ;


DEFIBA = min(max(1 + SEUIL_IMPDEFBA - SHBA - (REVTP - BA1) - REVQTOTQHT , 0) , 1) 
         * min(0 , BANOR ) * -1;

regle 221190:
application : iliad  ;

IINET = max(0 , NAPTEMPCX - TOTIRPSANT) * positif_ou_nul(max(0 , NAPTEMPCX - TOTIRPSANT) - SEUIL_12) ;
IINETIR = max(0 , NAPTIR) ;

regle 221200:
application : bareme ;

IINET = IRNET * positif ( IRNET - SEUIL_61 ) ;

regle 221212:
application : bareme , iliad  ;

IMPETAL19 = (V_BTIMPETAL9 * (1-positif(CODBIS)) + CODBIS) * positif(2023 - V_ANREV) ;
IMPETAL20 = (V_BTIMPETAL0 * (1-positif(CODBJS)) + CODBJS) * positif(2023 - V_ANREV) ;
IMPETAL21 = (V_BTIMPETAL1 * (1-positif(CODBKS)) + CODBKS) * positif(2023 - V_ANREV) ;
regle 221220:
application : iliad  ;

IRNET2 = (IAR + PIR - IRANT+ NRINET + IMPRET + CODZRA + (BRASAR * V_CNR)) * (1 - INDTXMIN)  * (1 - INDTXMOY)
         + min(0, IAR + PIR - IRANT+ NRINET + IMPRET + CODZRA + (BRASAR * V_CNR)) * (INDTXMIN + INDTXMOY)
         + max(0, IAR + PIR - IRANT+ NRINET + IMPRET + CODZRA + (BRASAR * V_CNR)) * (INDTXMIN * positif_ou_nul(IAVIMBIS - SEUIL_TXMIN)
                                                                                     + INDTXMOY * positif_ou_nul(IAVIMBIS - SEUIL_61)) ;

regle 221225:
application : iliad  ;

IRNETTER = max(0 , IRNET2
                   + (TAXASSUR + PTAXA - min(TAXASSUR+PTAXA+0,max(0,INE-IRB+AVFISCOPTER))
                      - max(0,TAXASSUR + PTAXA  - min(TAXASSUR + PTAXA + 0,max(0,INE-IRB+AVFISCOPTER))+ min(0,IRNET2)))
                   + (IPCAPTAXT + PPCAP - min(IPCAPTAXT + PPCAP,max(0,INE-IRB+AVFISCOPTER -TAXASSUR-PTAXA))
                      - max(0,IPCAPTAXT+PPCAP -min(IPCAPTAXT+PPCAP,max(0,INE-IRB+AVFISCOPTER- TAXASSUR - PTAXA ))+ min(0,TAXANEG)))
                   + (IHAUTREVT + PHAUTREV + CHRPVIMP - max(0 , IHAUTREVT + PHAUTREV + CHRPVIMP))) ;

IRNETBIS = max(0 , IRNETTER - PIR * positif(SEUIL_12 - IRNETTER + PIR) 
				  * positif(SEUIL_12 - PIR) 
          			  * positif_ou_nul(IRNETTER - SEUIL_12)) ;

regle 221230:
application : iliad  ;

IRNET =  IRNETBIS * positif_ou_nul(IRB - min(max(0,IRB-AVFISCOPTER),INE)) * (1-ANNUL2042);
regle 221240:
application : iliad  ;


TOTNET = max (0,NAPTIR) ;

regle 221250:
application : iliad  ;

TAXANEG = min(0 , TAXASSUR + PTAXA - min(TAXASSUR + PTAXA + 0 , max(0,INE-IRB+AVFISCOPTER)) + min(0 , IRNET2)) ;
TAXNET = positif(TAXASSUR)
	  * max(0 , TAXASSUR + PTAXA  - min(TAXASSUR + PTAXA + 0,max(0,INE-IRB+AVFISCOPTER)) + min(0 , IRNET2)) ;
TAXANET = (null(NRINET + IMPRET + CODZRA + BRASAR + 0) * TAXNET
	   + positif(NRINET + IMPRET + CODZRA + BRASAR + 0)
             * (positif_ou_nul(IAMD1 - SEUIL_61) * TAXNET + (1 - positif_ou_nul(IAMD1  - SEUIL_61)) * 0)) * (1-ANNUL2042) ;

regle 221260:
application : iliad  ;

PCAPNEG =  min(0,IPCAPTAXT+PPCAP -min(IPCAPTAXT+PPCAP,max(0,INE-IRB+AVFISCOPTER- TAXASSUR - PTAXA ))+ min(0,TAXANEG)) ;
PCAPTAXNET = positif(IPCAPTAXT)
                * max(0,IPCAPTAXT+PPCAP -min(IPCAPTAXT+PPCAP,max(0,INE-IRB+AVFISCOPTER- TAXASSUR - PTAXA ))+ min(0,TAXANEG)) ;
PCAPNET = (null(NRINET + IMPRET + CODZRA + BRASAR + 0) * PCAPTAXNET
	   + positif(NRINET + IMPRET + CODZRA + BRASAR + 0)
			* ( positif_ou_nul(IAMD1  - SEUIL_61) * PCAPTAXNET + (1 - positif_ou_nul(IAMD1 - SEUIL_61)) * 0 )) * (1-ANNUL2042) ;

regle 221280:
application : iliad  ;

CHRNEG = min(0 , IHAUTREVT + PHAUTREV + CHRPVIMP + min(0 , PCAPNEG)) ;
CHRNET = positif(IHAUTREVT + CHRPVIMP) * max(0 , IHAUTREVT + PHAUTREV + CHRPVIMP+ min(0 , PCAPNEG)) ;
HAUTREVNET = (null(NRINET + IMPRET + CODZRA + BRASAR + 0) * CHRNET
              +
              positif(NRINET + IMPRET + CODZRA + BRASAR + 0)
              * ( positif_ou_nul(IAMD1 - SEUIL_61) * CHRNET
              + (1 - positif_ou_nul(IAMD1 - SEUIL_61)) * 0 )
              ) * (1-ANNUL2042) 
              ;

regle 221290:
application : bareme ;


IRNET = max(0 , IRNET2 + RECOMP) ;

regle 221300:
application : iliad  ;


IRPROV = min (IRANT , IAR + PIR) * positif(IRANT) ;

regle 221310:
application :  iliad ;

NAPPSOLAVIM = (PSOL + PPSOL ) ;
NAPCSAVIM = (CSG + PCSG ) ;
NAPRDAVIM = (RDSN + PRDS) ;
NAPCVNAVIM = (CVNN + PCVN) ;
NAPCDISAVIM = (CDIS + PCDIS) ;
NAPC820AVIM = (MCSG820 + PCSG820) ;
NAPGLOAVIM = (CGLOA + PGLOA-COD8YL) ;
NAPRSE1AVIM = (RSE1N + PRSE1) ;
NAPRSE2AVIM = (RSE2N + PRSE2) ;
NAPRSE3AVIM = (RSE3N + PRSE3) ;
NAPRSE4AVIM = (RSE4N + PRSE4) ;
NAPRSE5AVIM = (RSE5N + PRSE5) ;
NAPRSE6AVIM = (RSE6N + PRSE6) ;
NAPRSE8AVIM = (RSE8N + PRSE8) ;
NAPCRPAVIM = max(0 , NAPPSOLAVIM + NAPCSAVIM + NAPRDAVIM + NAPCVNAVIM + NAPCDISAVIM 
                     + NAPGLOAVIM + NAPRSE1AVIM + NAPRSE2AVIM + NAPRSE3AVIM + NAPRSE4AVIM 
                     + NAPRSE5AVIM + NAPRSE6AVIM+NAPRSE8AVIM);


NAPPSOLAVIMB = (MPSOL + PPSOL );
NAPCSAVIMB = (CSGC + PCSG ) ;
NAPRDAVIMB = (RDSC + PRDS) ;

NAPCRPAVIMB = max(0 ,NAPPSOLAVIMB + NAPCSAVIMB + NAPRDAVIMB + NAPCVNAVIM + NAPCDISAVIM
                     + NAPGLOAVIM + NAPRSE1AVIM + NAPRSE2AVIM + NAPRSE3AVIM + NAPRSE4AVIM
		                          + NAPRSE5AVIM + NAPRSE6AVIM + NAPRSE8AVIM);

regle 221320:
application :  iliad ;

NAPCRPIAMD1 = PSOL+CSG+RDSN +CVNN + CDIS + MCSG820+ CGLOA + RSE1N + RSE2N + RSE3N + RSE4N + RSE5N + RSE6N + RSE8N;

regle 221330:
application :  iliad ;

NAPCS      = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * CSNET ;
NAPRD      = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * RDNET ;
NAPPSOL    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * PSOLNET ;
NAPCVN     = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * CVNNET ;
NAPCDIS    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * CDISNET ;
NAPCSG820  = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * CSG820NET ;
NAPGLOA    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * CGLOANET ;
NAPRSE1    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * RSE1NET ;
NAPRSE2    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * RSE2NET ;
NAPRSE3    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * RSE3NET ;
NAPRSE4    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * RSE4NET ;
NAPRSE5    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * RSE5NET ;
NAPRSE6    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * RSE6NET ;
NAPRSE8    = positif(SEUIL_61 - VARPS61) * 0 + (1 - positif(SEUIL_61 - VARPS61)) * RSE8NET ;


NAPCSB = positif(SEUIL_61 - VARPS611) * 0 +  (1- positif(SEUIL_61 - VARPS611)) *  CSBRUT  ;
NAPRDB = positif(SEUIL_61 - VARPS611) * 0 +  (1- positif(SEUIL_61 - VARPS611)) *  RDBRUT  ;
NAPPSOLB = positif(SEUIL_61 - VARPS611) * 0 +  (1- positif(SEUIL_61 - VARPS611)) *  PSOLBRUT  ;

NAPCRP2 = max(0 , NAPPSOLB + NAPCSB + NAPRDB + NAPCVN + NAPCDIS + NAPCSG820 + NAPGLOA + NAPRSE1 + NAPRSE2 + NAPRSE3 + NAPRSE4 + NAPRSE5  + NAPRSE6 + NAPRSE8);
regle 221340:
application : iliad  ;


IMPTHNET = max(0 , (IRB * positif_ou_nul(IRB-SEUIL_61)-INE-IRE)
		       * positif_ou_nul((IRB*positif_ou_nul(IRB-SEUIL_61)-INE-IRE)-SEUIL_12)) 
	     * (1 - V_CNR) ;

regle 221350:
application : iliad  ;

IRESTIT = abs(min(0 , IRN + PIR + NRINET + IMPRET + CODZRA + BRASAR 
                    + (TAXASSUR + PTAXA - min(TAXASSUR+PTAXA+0,max(0,INE-IRB+AVFISCOPTER)))
                    + (IPCAPTAXT + PPCAP - min(IPCAPTAXT + PPCAP,max(0,INE-IRB+AVFISCOPTER -TAXASSUR-PTAXA)))
                    + ((IHAUTREVT + PHAUTREV+CHRPVIMP) 
                      -min((IHAUTREVT + PHAUTREV+CHRPVIMP),max(0,INE-IRB+AVFISCOPTER-TAXASSUR-PTAXA-IPCAPTAXT-PPCAP)))
                    + null(4-V_IND_TRAIT)* max(0 ,  TOTCR - CSGIM - CRDSIM - PRSPROV - COD8YT - CDISPROV -COD8YL-CSPROVYD
                                                          -CSPROVYE - CSPROVYF - CSPROVYN - CSPROVYG - CSPROVYH - CSPROVYP - COD8YV - COD8YX )
                             * positif_ou_nul((TOTCR - CSGIM - CRDSIM - PRSPROV - COD8YT - CDISPROV -COD8YL-CSPROVYD
                                                     - CSPROVYE-CSPROVYF- CSPROVYN-CSPROVYG-CSPROVYH - CSPROVYP-COD8YV-COD8YX) - SEUIL_61) 
                    + null(5-V_IND_TRAIT) * max(0 , (TOTCR - CSGIM - CRDSIM - PRSPROV - COD8YT - CDISPROV -COD8YL-CSPROVYD
                                                           - CSPROVYE-CSPROVYF- CSPROVYN -CSPROVYG-CSPROVYH- CSPROVYP - COD8YV-COD8YX))
                          * positif_ou_nul((TOTCR - CSGIM - CRDSIM - PRSPROV - COD8YT - CDISPROV -COD8YL-CSPROVYD
                                                  -CSPROVYE-CSPROVYF- CSPROVYN-CSPROVYG-CSPROVYH- CSPROVYP-COD8YV-COD8YX) - SEUIL_61) 
                 )
             ) ;

regle 221360:
application : iliad  ;

IRESTITIR = abs(min(0 , IRN + PIR + NRINET + IMPRET + CODZRA + BRASAR
                    + (TAXASSUR + PTAXA - min(TAXASSUR+PTAXA+0,max(0,INE-IRB+AVFISCOPTER)))
                    + (IPCAPTAXT + PPCAP - min(IPCAPTAXT + PPCAP,max(0,INE-IRB+AVFISCOPTER -TAXASSUR-PTAXA)))
                    + ((IHAUTREVT + PHAUTREV+CHRPVIMP) -min((IHAUTREVT + PHAUTREV+CHRPVIMP),max(0,INE-IRB+AVFISCOPTER-TAXASSUR-PTAXA-IPCAPTAXT-PPCAP)))
                   )
               ) * (1 - ANNUL2042) ;

regle 221370:
application : iliad  ;

IREST = max(0,max(0,-(NAPTEMPCX)) - max(0,-(TOTIRPSANT))) ;

regle 221380:
application : iliad  ;

IRESTIR = max(0 , IRESTITIR - RECUMBISIR);
IINETCALC = max(0,NAPTEMP - TOTIRPSANT);
VARNON = IRPSCUM -RECUM - TOTIRPSANT;
NONMER  =  positif(20 - V_NOTRAIT) * (
                                           positif(SEUIL_8 - RECUM) * positif(SEUIL_12 - IRPSCUM) * IRPSCUM
                                          + (1-positif(SEUIL_8 - RECUM) * positif(SEUIL_12 - IRPSCUM)) * 0
                                     )
        + (1-positif(20-V_NOTRAIT)) * (
                          positif(SEUIL_8 - RECUM) * positif(SEUIL_12 - IRPSCUM) * (
                                                                                              positif(SEUIL_12 - abs(TOTIRPSANT))* max(0,IRPSCUM-RECUM-TOTIRPSANT)
                                                                                            + (1-positif(SEUIL_12 - abs(TOTIRPSANT))) * IRPSCUM
                                                                                   )
                   + (1-positif(SEUIL_8 - RECUM) * positif(SEUIL_12 - IRPSCUM)) * (
                                                                                           positif(positif(SEUIL_12-VARNON) * positif(VARNON)
                                                                                                  + positif(SEUIL_8-abs(VARNON)) * (1-positif(VARNON)))
                                                                                                    * max(0,IRPSCUM-RECUM-TOTIRPSANT)
                                                                                       +(1-positif(positif(SEUIL_12-VARNON) * positif(VARNON)
                                                                                                 + positif(SEUIL_8-abs(VARNON)) * (1-positif(VARNON))))
                                                                                                    * 0
                                                                                  )
                                      );


NONREST  =  positif(20 - V_NOTRAIT) * (
                                           positif(SEUIL_8 - RECUM) * positif(SEUIL_12 - IRPSCUM) * RECUM
                                        + (1-positif(SEUIL_8 - RECUM) * positif(SEUIL_12 - IRPSCUM)) * 0 
                                      )
        + (1-positif(20-V_NOTRAIT)) * (
                          positif(SEUIL_8 - RECUM) * positif(SEUIL_12 - IRPSCUM) * (
                                                                                              positif(SEUIL_12 - abs(TOTIRPSANT))* max(0,TOTIRPSANT - (IRPSCUM-RECUM))
                                                                                            + (1-positif(SEUIL_12 - abs(TOTIRPSANT))) * RECUM
                                                                                   )
                   + (1-positif(SEUIL_8 - RECUM) * positif(SEUIL_12 - IRPSCUM)) * (
                                                                                           positif(positif(SEUIL_12-VARNON) * positif(VARNON)
                                                                                                  + positif(SEUIL_8-abs(VARNON)) * (1-positif(VARNON)))
                                                                                                      * max(0,TOTIRPSANT - (IRPSCUM-RECUM))
                                                                                       +(1-positif(positif(SEUIL_12-VARNON) * positif(VARNON)
                                                                                                 + positif(SEUIL_8-abs(VARNON)) * (1-positif(VARNON))))
                                                                                                      * 0
                                                                                  )
                                     );


regle 221390:
application :  iliad ;


TOTREC = positif_ou_nul(IRN + TAXANET + PIR + PCAPNET + HAUTREVNET - SEUIL_12) ;

regle 221400:
application :  iliad ;


CSRECB = positif(NAPCRPB) * positif_ou_nul(NAPCRPAVIMB - SEUIL_61);		 

CSREC = positif(NAPCRP) * positif_ou_nul(NAPCRPAVIM - SEUIL_61);

CSRECINR = positif(NAPCRINR) ;

regle 221410:
application :  iliad ;

RSEREC = positif(max(0 , NAPRSE1 + NAPRSE2 + NAPRSE3 + NAPRSE4 + NAPRSE5 + NAPRSE6 + NAPRSE8)
                 * positif_ou_nul(NAPCRP- SEUIL_12)) ;

regle 221420:
application :  iliad ;

CSRECA = positif_ou_nul(PSOL_A + PPSOL_A + CSG_A + RDS_A + PCSG_A + PRDS_A
                       + CVN_A+PCVN_A + CDIS_A +PCDIS_A+ MCSG820_A+PCSG820_A+ CGLOA_A +PGLOA_A
                       + RSE1BASE_A + PRSE1_A + RSE2BASE_A + PRSE2_A 
                       + RSE3BASE_A + PRSE3_A + RSE4BASE_A + PRSE4_A
                       + RSE5BASE_A + PRSE5_A + RSE6BASE_A + PRSE6_A + RSE8BASE_A + PRSE8_A
                       + IRNIN_A + PIR_A + TAXABASE_A + PTAXA_A + CHRBASE_A + PCHR_A 
                       + PCAPBASE_A + PPCAP_A - SEUIL_12) ;

regle isf 221430:
application : iliad ;

ISFDEGR = max(0,(ANTISFAFF  - ISF4BIS * positif_ou_nul (ISF4BIS - SEUIL_12)) 
	   * (1-positif_ou_nul (ISF4BIS - SEUIL_12))
          + (ANTISFAFF  - ISFNET * positif_ou_nul (ISFNET - SEUIL_12))
	   * positif_ou_nul(ISF4BIS - SEUIL_12)) ;


ISFDEG = ISFDEGR * positif_ou_nul(ISFDEGR - SEUIL_8) ;

regle isf 221431:
application : iliad ;

IFIDEGR = max(0,(ANTISFAFF  - IFI4BIS * positif_ou_nul (IFI4BIS - SEUIL_12))
           * (1-positif_ou_nul (IFI4BIS - SEUIL_12))
             + (ANTISFAFF  - IFINET * positif_ou_nul (IFINET - SEUIL_12))
             * positif_ou_nul(IFI4BIS - SEUIL_12)) ;


IFIDEG = IFIDEGR * positif_ou_nul(IFIDEGR - SEUIL_8) ;
regle corrective 221440:
application : iliad ;

IDEGR = max(0,max(0,TOTIRPSANT) - max(0,NAPTEMPCX));

IRDEG = positif(NAPTOTAIR - IRNET) * positif(NAPTOTAIR) * max(0 , V_ANTIR - max(0,IRNET))
	* positif_ou_nul(IDEGR - SEUIL_8) ;                   

TAXDEG = positif(NAPTOTAIR - TAXANET) * positif(NAPTOTAIR) * max(0 , V_TAXANT - max(0,TAXANET)) ;                    

TAXADEG = positif(TAXDEG) * positif(V_TAXANT) * max(0 , V_TAXANT - max(0,TOTAXAGA))
          * positif_ou_nul(IDEGR - SEUIL_8) ;

PCAPTAXDEG = positif(NAPTOTAIR - PCAPNET) * positif(NAPTOTAIR) * max(0 , V_PCAPANT- max(0,PCAPNET)) ;

PCAPDEG = positif(PCAPTAXDEG) * positif (V_PCAPANT) * max(0 , V_PCAPANT - max(0,PCAPTOT)) 
          * positif_ou_nul(IDEGR - SEUIL_8) ;

HAUTREVTAXDEG =  positif(NAPTOTAIR - HAUTREVNET) * positif(NAPTOTAIR) * max(0 , V_CHRANT - max(0,HAUTREVNET)) ;

HAUTREVDEG = positif(HAUTREVTAXDEG) * positif(V_CHRANT) * max(0 , V_CHRANT - max(0,HAUTREVTOT)) 
             * positif_ou_nul(IDEGR - SEUIL_8) ;

regle 221450:
application :  iliad ;


ABSRE = ABMAR + ABVIE ;

regle 221460:
application :  iliad ;


RPEN = PTOTD * positif(APPLI_ILIAD + APPLI_COLBERT) ;

regle isf 221470:
application : iliad ;

ANTISFAFF = V_ANTIFI* (1-positif(APPLI_OCEANS));
regle 221480:
application : iliad ;

ANTIRAFF = V_ANTIR  * APPLI_ILIAD
            + IRNET_A * APPLI_OCEANS
	    + 0 ;

TAXANTAFF = V_TAXANT * APPLI_ILIAD * (1- APPLI_OCEANS)
            + TAXANET_A * APPLI_OCEANS
	    + 0 ;

PCAPANTAFF = V_PCAPANT * APPLI_ILIAD * (1- APPLI_OCEANS)
            + PCAPNET_A * APPLI_OCEANS
	    + 0 ;

HAUTREVANTAF = V_CHRANT * APPLI_ILIAD * (1- APPLI_OCEANS)
            + CHRNET_A * APPLI_OCEANS
	    + 0 ;
regle 221500:
application : iliad  ;


IAVT = INE + IRE;


regle 221510:
application : iliad  ;

INDTXMOY = positif(positif(TX_MIN_MET - TMOY) * positif_ou_nul(max(0 , LIM_BAR3 - BMI))
                 + positif(TX_MIN_MET2 - TMOY) * positif(max(0 , BMI - LIM_BAR3)))
           * positif( (present(RMOND) + present(DMOND))*null(4-V_IND_TRAIT)
	              +(positif(RMOND) + positif(DMOND)+present(RMOND_P)+present(DMOND_P))*null(5-V_IND_TRAIT) ) * V_CNR
           * positif_ou_nul(IMI - IMO) ;

regle 221515:
application : iliad  ;

INDTXMIN = positif_ou_nul(IMI - IPQ1) * positif(1 - INDTXMOY) * V_CNR ;

regle 221520:
application :  iliad ;

IND_REST = positif(IREST) ;

regle 221530:
application : iliad  ;

IND_NI =  null(NAPT) * (null (IRNET)) ;

regle 221540:
application : iliad  ;

IND_IMP = positif(NAPT) ;

INDNMR =  null(NAPT) * null(NAT1BIS) * (positif (IRNET + TAXANET + PCAPNET + HAUTREVNET )) ;

IND61IR = (positif_ou_nul(IAMD1 - SEUIL_61) * 2
           + (1 - positif_ou_nul(IAMD1 - SEUIL_61)) * positif(IAMD1) * 1) * (1 - positif(INDTXMIN))
          + (positif_ou_nul(IAMD1 - SEUIL_TXMIN) * 2
             + (1 - positif_ou_nul(IAMD1 - SEUIL_TXMIN)) * positif(IAMD1) * 1) * positif(INDTXMIN)  
          + null(IAMD1) * 3 ;

IND61PS = positif_ou_nul(NAPCRP - SEUIL_61) * 2
	  + (1 - positif_ou_nul(NAPCRP - SEUIL_61)) * positif(NAPCRP) * 1
	  + (null(NAPCRP) * 3) ;

regle 221550:
application : iliad  ;

INDCEX = null(1 - NATIMP) * 1
         + positif(null(11 - NATIMP) + null(21 - NATIMP) + null(81 - NATIMP) + null(91 - NATIMP)) * 2
         + null(0 - NATIMP) * 3 ;

INDNMRI = INDNMR * positif(RED) ;

INDNIRI = positif(IDOM11 - DEC11) * null(IAD11) ;

INDRFR = positif(REVKIRE - LIMRFR) ;

regle 221560:
application :  iliad ;

IND_REST50 = positif(SEUIL_8 - IREST) * positif(IREST) * (1-positif(APPLI_OCEANS));
IND08 = positif(NAPT*(-1)) * (positif(SEUIL_8 - abs(NAPT)) * 1 
                          + (1-positif(SEUIL_8 - abs(NAPT))) * 2 );

regle 221570:
application : iliad  ;


INDMAJREV = positif(BIHNOV + BIHNOC + BIHNOP + BICHREV + BICHREC + BICHREP + BNHREV 
                    + BNHREC + BNHREP + ANOCEP + ANOVEP + ANOPEP + BAHREV + BAHREC 
		    + BAHREP + 4BAHREV + 4BAHREC + 4BAHREP + REGPRIV) ;

regle 221580:
application : iliad  ;


INDNMR1 = (1 - positif(IAMD1 + 1 - SEUIL_61)) 
	   * null(NAPT) * positif(IAMD1) ;

INDNMR2 = positif(INDNMR) * (1 - positif(INDNMR1)) ;
IND12 = positif(12 - IRPSNET) * positif(IRPSNET)
        + positif_ou_nul(IRPSNET - 12) * 2
	+ null(IRPSNET) * 3 ;

VARIRPSNET = IRPSNET * (positif(IRPSNET) * null(2 - IND12) + (1 - positif(IRPSNET))) ;

regle 221590:
application :  iliad ;


INDV = positif_ou_nul(positif(ALLOV) + positif(REMPLAV) + positif(REMPLANBV) + positif(BACDEV) + positif(BACREV) + positif(4BACREV) + positif(4BAHREV) 
                      + positif(BAFPVV) + positif(BAHDEV) + positif(BAHREV) + positif(BICDEV) + positif (BICDNV) + positif (BICHDEV) + positif(BICHREV) 
		      + positif(BICNOV) + positif(BICREV) + positif (BIHDNV) + positif(BIHNOV) + positif (BNCAADV) + positif(BNCAABV) + positif(BNCDEV) 
		      + positif(BNCNPPVV) + positif(BNCNPV) + positif(BNCPROPVV) + positif(BNCPROV) + positif(BNCREV) + positif(BNHDEV) + positif(BNHREV) 
		      + positif(BPCOSAV) + positif(CARPENBAV) + positif(CARPEV) + positif(CARTSNBAV) + positif(CARTSV) + positif(COTFV) + positif(FRNV) 
		      + positif(GLDGRATV) + positif(ANOCEP) + positif(MIBNPPRESV) + positif(MIBNPPVV) + positif(MIBNPVENV) + positif(MIBPRESV) + positif(MIBPVV) 
		      + positif(MIBVENV) + positif(PALIV) + positif(PENSALV) + positif(PENSALNBV) + positif(PEBFV) + positif(PRBRV) + positif(TSHALLOV) 
		      + positif(DNOCEP) + positif(BAFORESTV) + positif(LOCNPCGAV) + positif(LOCNPV) + positif(LOCDEFNPCGAV) + positif(LOCDEFNPV) + positif(MIBMEUV) 
		      + positif(MIBGITEV) + positif(BICPMVCTV) + positif(BNCPMVCTV) + positif(LOCGITV) + positif(PENINV) 
                      + positif(CODRAZ) + positif(CODDAJ) + positif(CODEAJ) + positif(COD1AF) + positif(COD1AG) + positif(COD1AL) + positif(COD1AM) 
		      + positif(COD1GB) + positif(CODRAF) + positif(CODNAF) + positif(CODRAG) + positif(CODNAG) + positif(CODRAL) + positif(CODNAL) 
		      + positif(CODRAM) + positif(CODNAM) + positif(COD5AK) + positif(COD5AL) + positif(COD5AQ) + positif(COD5AR) + positif(COD5AY) 
		      + positif(COD5AZ) + positif(COD5DF) + positif(COD5DG) + positif(COD5DK) + positif(COD5DL) + positif(COD5DM) + positif(COD5DN)
                      + positif(COD5EY) + positif(COD5EZ) + positif(BNCNPDCT) + positif(COD5UR) + positif(COD5US) + positif(COD5UT) + positif(COD5UU) 
		      + positif(COD5UY) + positif(COD5UZ) + positif(COD5VM) + positif(COD5VN) + positif(COD5XA) + positif(COD5XB) + positif(COD5XH) 
		      + positif(COD5XJ) + positif(COD5XK) + positif(COD5XL) + positif(COD5XN) + positif(COD5XO) + positif(COD5XP) + positif(COD5XQ) 
		      + positif(COD5XR) + positif(COD5XS) + positif(COD5XX) + positif(COD5XY) + positif(COD5XZ) + positif(CODCJG) + positif(CODCKC) 
		      + positif(CODCKI) + positif(CODCNC) + positif(CODCNI) + positif(COD5NW) + positif(CODCQC) + positif(CODCQI) + positif(CODCSN)
                      + positif(COD1AA) + positif(COD1GE) + positif(COD1GF) + positif(COD5XM) + positif(COD5YM)  
		      + positif(COD1AI) + positif(COD1GG) + positif(COD1GH) + positif(COD1GK) + positif(COD1GL) + positif(CODBIS) 
		      + positif(COD5EA) + positif(COD5EC) + positif(COD5HA) + positif(COD5QA) + positif(COD5QJ) + positif(COD5TF) + positif(COD5UI) 
		      + positif(CODRAI) + positif(COD5TJ) + positif(COD5TK) + positif(COD5TL) + positif(COD5WE) + positif(COD5WF)
                      + positif(COD1AD + COD1AV + COD1PB + COD5AC + COD5AE + COD5AG + COD5AT + COD5AX)
                     ) ;

INDC = positif_ou_nul(positif(ALLOC) + positif(REMPLAC) + positif(REMPLANBC) + positif(BACDEC) + positif(BACREC) + positif(4BACREC) + positif(4BAHREC)
                      + positif(ANOVEP) + positif(DNOCEPC) + positif(BAFPVC) + positif(BAHDEC) + positif(BAHREC) + positif(BICDEC) + positif(BICDNC) 
		      + positif(BICHDEC) + positif(BICHREC) + positif(BICNOC) + positif(BICREC)  + positif(BIHDNC) + positif(BIHNOC) + positif(BNCAADC) 
		      + positif(BNCAABC) + positif(BNCDEC) + positif(BNCNPC) + positif(BNCNPPVC) + positif(BNCPROC) + positif(BNCPROPVC) + positif(BNCREC) 
		      + positif(BNHDEC) + positif(BNHREC) + positif(BPCOSAC) + positif(CARPEC) + positif(CARPENBAC) + positif(CARTSC) + positif(CARTSNBAC) 
		      + positif(COTFC) + positif(FRNC) + positif(GLDGRATC) + positif(MIBNPPRESC) + positif(MIBNPPVC) + positif(MIBNPVENC) + positif(MIBPRESC) 
		      + positif(MIBPVC) + positif(MIBVENC) + positif(PALIC) + positif(PENSALC) + positif(PENSALNBC) + positif(PEBFC) + positif(PRBRC) 
		      + positif(TSHALLOC) + positif(BAFORESTC) + positif(LOCNPCGAC) + positif(LOCNPC) + positif(LOCDEFNPCGAC) + positif(LOCDEFNPC) + positif(MIBMEUC) 
		      + positif(MIBGITEC) + positif(BICPMVCTC) + positif(BNCPMVCTC) + positif(LOCGITC) + positif(PENINC) + positif(CODRBZ) + positif(CODDBJ) 
		      + positif(CODEBJ) + positif(COD1BF) + positif(COD1BG) + positif(COD1BL) + positif(COD1BM) + positif(COD1HB) + positif(CODRBF) 
		      + positif(CODNBF) + positif(CODRBG) + positif(CODNBG) + positif(CODRBL) + positif(CODNBL) + positif(CODRBM) + positif(CODNBM)
                      + positif(COD5BK) + positif(COD5BL) + positif(COD5BQ) + positif(COD5BR) + positif(COD5BY) + positif(COD5BZ) + positif(COD5EF) 
		      + positif(COD5EG) + positif(COD5EK) + positif(COD5EL) + positif(COD5EM) + positif(COD5EN) + positif(COD5FY) + positif(COD5FZ) 
		      + positif(COD5LD) + positif(COD5RZ) + positif(COD5VR) + positif(COD5VS) + positif(COD5VT) + positif(COD5VU) + positif(COD5VY) 
		      + positif(COD5VZ) + positif(COD5WM) + positif(COD5WN) + positif(COD5YA) + positif(COD5YB) + positif(COD5YH) + positif(COD5YJ) 
		      + positif(COD5YK) + positif(COD5YL) + positif(COD5YN) + positif(COD5YO) + positif(COD5YP) + positif(COD5YQ) + positif(COD5YR) 
		      + positif(COD5YS) + positif(COD5YX) + positif(COD5YY) + positif(COD5YZ) + positif(CODCLC) + positif(CODCLI) + positif(CODCNS) 
		      + positif(CODCOC) + positif(CODCOI) + positif(COD5OW) + positif(CODCRC) + positif(CODCRF) + positif(CODCRI) + positif(COD1BA) 
		      + positif(COD1HE) + positif(COD1HF) + positif(COD5YT) + positif(COD5YU)
		      + positif(COD1BI) + positif(COD1GL) + positif(COD1HG) + positif(COD1HH) + positif(COD1HL) 
		      + positif(COD5EI) + positif(COD5EQ) + positif(COD5IA) + positif(COD5RA) + positif(COD5RJ) + positif(COD5UF) + positif(COD5VI)
                      + positif(CODRBI) + positif(COD5UJ) + positif(COD5UK) + positif(COD5UL) + positif(COD5XE) + positif(COD5XF) 
                      + positif(COD1BD + COD1BV + COD1PC + COD5BC + COD5BE + COD5BG + COD5BT + COD5BX)
                     ) ;

INDP = positif_ou_nul(positif(ALLO1) + positif(ALLO2) + positif(ALLO3) + positif(ALLO4) + positif(CARTSP1) + positif(CARTSP2) + positif(CARTSP3) + positif(CARTSP4)
                      + positif(CARTSNBAP1) + positif(CARTSNBAP2) + positif(CARTSNBAP3) + positif(CARTSNBAP4) + positif(REMPLAP1) + positif(REMPLAP2) 
		      + positif(REMPLAP3) + positif(REMPLAP4) + positif(REMPLANBP1) + positif(REMPLANBP2) + positif(REMPLANBP3) + positif(REMPLANBP4)
                      + positif(BACDEP) + positif(BACREP) + positif(4BACREP) + positif(4BAHREP) + positif(ANOPEP) + positif(DNOCEPP) + positif(BAFPVP) 
		      + positif(BAHDEP) + positif(BAHREP) + positif(BICDEP) + positif(BICDNP) + positif(BICHDEP) + positif(BICHREP) + positif(BICNOP)
                      + positif(BICREP) + positif(BIHDNP) + positif(BIHNOP) + positif(BNCAADP) + positif(BNCAABP) + positif(BNCDEP) + positif(BNCNPP) 
		      + positif(BNCNPPVP) + positif(BNCPROP) + positif(BNCPROPVP) + positif(BNCREP) + positif(BNHDEP) + positif(BNHREP) + positif(COTF1) 
		      + positif(COTF2) + positif(COTF3) + positif(COTF4) + positif(FRN1) + positif(FRN2) + positif(FRN3) + positif(FRN4) + positif(MIBNPPRESP) 
		      + positif(MIBNPPVP) + positif(MIBNPVENP) + positif(MIBPRESP) + positif(MIBPVP) + positif(MIBVENP) + positif(PALI1) + positif(PALI2) 
		      + positif(PALI3) + positif(PALI4) + positif(PENSALP1) + positif(PENSALP2) + positif(PENSALP3) + positif(PENSALP4) + positif(PENSALNBP1) 
		      + positif(PENSALNBP2) + positif(PENSALNBP3) + positif(PENSALNBP4) + positif(PEBF1) + positif(PEBF2) + positif(PEBF3) + positif(PEBF4) 
		      + positif(PRBR1) + positif(PRBR2) + positif(PRBR3) + positif(PRBR4) + positif(CARPEP1) + positif(CARPEP2) + positif(CARPEP3) 
		      + positif(CARPEP4) + positif(CARPENBAP1) + positif(CARPENBAP2) + positif(CARPENBAP3) + positif(CARPENBAP4) + positif(TSHALLO1) 
		      + positif(TSHALLO2) + positif(TSHALLO3) + positif(TSHALLO4) + positif(BAFORESTP) + positif(LOCNPCGAPAC) + positif(LOCNPPAC) 
		      + positif(LOCDEFNPCGAPAC) + positif(LOCDEFNPPAC) + positif(MIBMEUP) + positif(MIBGITEP)  + positif(BICPMVCTP) + positif(BNCPMVCTP) 
		      + positif(LOCGITP) + positif(COD1CF) + positif(COD1CG) + positif(COD1CL) + positif(COD1CM) + positif(COD1DF) + positif(COD1DG) 
		      + positif(COD1DL) + positif(COD1DM) + positif(COD1EF) + positif(COD1EG) + positif(COD1EL) + positif(COD1EM) + positif(COD1FF) 
		      + positif(COD1FG) + positif(COD1FL) + positif(COD1FM) + positif(COD1IB) + positif(COD1JB) + positif(CODRCF) + positif(CODNCF) 
		      + positif(COD1CI) + positif(COD1DI) + positif(COD1EI) + positif(COD1FI)
		      + positif(CODRCG) + positif(CODNCG) + positif(CODRDF) + positif(CODNDF) + positif(CODRDG) + positif(CODNDG) + positif(CODREF) 
		      + positif(CODNEF) + positif(CODRGG) + positif(CODNGG) + positif(CODRFF) + positif(CODNFF) + positif(CODRFG) + positif(CODNFG) 
		      + positif(CODRCL) + positif(CODNCL) + positif(CODRCM) + positif(CODNCM) + positif(CODRDL) + positif(CODNDL) + positif(CODRDM) 
		      + positif(CODNDM) + positif(CODREL) + positif(CODNEL) + positif(CODREM) + positif(CODNEM) + positif(CODRFL) + positif(CODNFL) 
                      + positif(CODRFM) + positif(CODNFM) + positif(PENIN1) + positif(PENIN2) + positif(PENIN3) + positif(PENIN4) + positif(CODRCZ) 
		      + positif(CODRDZ) + positif(CODREZ) + positif(CODRFZ) + positif(COD5CK) + positif(COD5CL) + positif(COD5FF) + positif(COD5FG) 
		      + positif(COD5GY) + positif(COD5GZ) + positif(COD5MD) + positif(COD5SZ) + positif(COD5WR) + positif(COD5WS) + positif(COD5ZA) 
		      + positif(COD5ZB) + positif(COD5ZJ) + positif(COD5ZK) + positif(COD5ZN) + positif(COD5ZO) + positif(COD5ZS) + positif(COD5ZX) 
		      + positif(COD5AH) + positif(COD5BH) + positif(COD5CM) + positif(COD5CN) + positif(COD5CQ) + positif(COD5CR) + positif(COD5CU) 
		      + positif(COD5CV) + positif(COD5CY) + positif(COD5CZ) + positif(COD5ED) + positif(COD5FB) + positif(COD5FD) + positif(COD5FK) 
                      + positif(COD5FL) + positif(COD5FM) + positif(COD5FN) + positif(CODCMC) + positif(CODCMI) + positif(CODCOS) + positif(CODCPC) 
		      + positif(CODCPI) + positif(COD5PW) + positif(CODCSC) + positif(CODCSF) + positif(CODCSI) + positif(COD5TP) + positif(COD5VQ) 
		      + positif(COD5VV) + positif(COD5VW) + positif(COD5VX) + positif(COD5ZH) + positif(COD5ZI) + positif(COD5ZL) + positif(COD5ZM) 
		      + positif(COD5ZP) + positif(COD5ZQ) + positif(COD5ZR) + positif(COD5ZW) + positif(COD5ZY) + positif(COD5ZZ) + positif(COD1CA) 
		      + positif(COD1DA) + positif(COD1EA) + positif(COD1FA) + positif(COD1IE) + positif(COD1IF) 
		      + positif(COD1JE) + positif(COD1JF) + positif(COD1KE) + positif(COD1KF) + positif(COD1LE) 
		      + positif(COD1LF) + positif(COD5ZT) + positif(COD5ZU)
		      + positif(COD1GP) + positif(COD1GQ) + positif(COD1GR) + positif(COD1GS) + positif(COD1HP) + positif(COD1HQ) + positif(COD1HR)
		      + positif(COD1HS) + positif(COD1IG) + positif(COD1IH) + positif(COD1JG) + positif(COD1JH) + positif(COD1KG) + positif(COD1KH)
		      + positif(COD1LG) + positif(COD1LH) + positif(COD5EU) + positif(COD5EV) + positif(COD5JA)
		      + positif(COD5SA) + positif(COD5SJ) + positif(COD5VF) + positif(COD5WI) 
                      + positif(CODRCK) + positif(COD5VJ) + positif(COD5VK) + positif(COD5VL) + positif(COD5YE) + positif(COD5YF)
                      + positif(COD1CD + COD1CV + COD1DD + COD1DV + COD1ED + COD1EV + COD1FD + COD1FV + COD1PD + COD1PE + COD1PF + COD1PG 
		                + COD5CC + COD5CE + COD5CG + COD5CT + COD5CX)
                     ) ;

regle 221600:
application : iliad  ;




INDREV1A8BIS = positif(
              positif(4BACREC) + positif(4BACREP) + positif(4BACREV) + positif(4BAHREC) + positif(4BAHREP) + positif(4BAHREV) + positif(ABDETPLUS) + positif(ALLO1) + positif(ALLO2) + positif(ALLO3) + positif(ALLO4) 
	    + positif(ALLOC) + positif(ALLOV) + positif(ANOCEP) + positif(ANOPEP) + positif(ANOVEP) + positif(AUTOBICPC) + positif(AUTOBICPP) + positif(AUTOBICPV) + positif(AUTOBICVC) + positif(AUTOBICVP) 
	    + positif(AUTOBICVV) + positif(AUTOBNCC) + positif(AUTOBNCP) + positif(AUTOBNCV) + positif(BA1AC) + positif(BA1AP) + positif(BA1AV) + positif(BACDEC) + positif(BACDEP) + positif(BACDEV) + positif(BACREC) 
	    + positif(BACREP) + positif(BACREV) + positif(BAEXC) + positif(BAEXP) + positif(BAEXV) + positif(BAF1AC) + positif(BAF1AP) + positif(BAF1AV) + positif(BAFORESTC) + positif(BAFORESTP) + positif(BAFORESTV) 
            + positif(BAFPVC) + positif(BAFPVP) + positif(BAFPVV) + positif(BAHDEC) + positif(BAHDEP) + positif(BAHDEV) + positif(BAHEXC) + positif(BAHEXP) + positif(BAHEXV) + positif(BAHREC) + positif(BAHREP) + positif(BAHREV) 
	    + positif(BAILOC98) + positif(BAPERPC) + positif(BAPERPP) + positif(BAPERPV) + positif(BI1AC) + positif(BI1AP) + positif(BI1AV) + positif(BI2AC) + positif(BI2AP) + positif(BI2AV) + positif(BICDEC) + positif(BICDEP) 
	    + positif(BICDEV) + positif(BICDNC) + positif(BICDNP) + positif(BICDNV) + positif(BICEXC) + positif(BICEXP) + positif(BICEXV) + positif(BICHDEC) + positif(BICHDEP) + positif(BICHDEV) + positif(BICHREC) 
	    + positif(BICHREP) + positif(BICHREV) + positif(BICNOC) + positif(BICNOP) + positif(BICNOV) + positif(BICNPEXC) + positif(BICNPEXP) + positif(BICNPEXV) + positif(BICNPHEXC) + positif(BICNPHEXP) 
	    + positif(BICNPHEXV) + positif(BICREC) + positif(BICREP) + positif(BICREV) + positif(BIHDNC) + positif(BIHDNP) + positif(BIHDNV) + positif(BIHEXC) + positif(BIHEXP) + positif(BIHEXV) + positif(BIHNOC) 
	    + positif(BIHNOP) + positif(BIHNOV) + positif(BN1AC) + positif(BN1AP) + positif(BN1AV) + positif(BNCAABC) + positif(BNCAABP) + positif(BNCAABV) + positif(BNCAADC) + positif(BNCAADP) + positif(BNCAADV)
            + positif(BNCCRC) + positif(BNCCRFC) + positif(BNCCRFP) + positif(BNCCRFV) + positif(BNCCRP) + positif(BNCCRV) + positif(BNCDEC) + positif(BNCDEP) + positif(BNCDEV) + positif(BNCEXC) + positif(BNCEXP) 
	    + positif(BNCEXV) + positif(BNCNP1AC) + positif(BNCNP1AP) + positif(BNCNP1AV) + positif(BNCNPC) + positif(BNCNPDCT) + positif(BNCNPDEC) + positif(BNCNPDEP) + positif(BNCNPDEV) + positif(BNCNPP) 
            + positif(BNCNPPVC) + positif(BNCNPPVP) + positif(BNCNPPVV) + positif(BNCNPREXAAC) + positif(BNCNPREXAAP) + positif(BNCNPREXAAV) + positif(BNCNPREXC) + positif(BNCNPREXP) 
	    + positif(BNCNPREXV) + positif(BNCNPV) + positif(BNCPRO1AC) + positif(BNCPRO1AP) + positif(BNCPRO1AV) + positif(BNCPROC) + positif(BNCPMVCTV) + positif(BNCPMVCTC) + positif(BNCPMVCTP) 
            + positif(BNCPRODEC) + positif(BNCPRODEP) + positif(BNCPRODEV) + positif(BNCPROEXC) + positif(BNCPROEXP) + positif(BNCPROEXV) + positif(BNCPROP) + positif(BNCPROPVC) + positif(BNCPROPVP) 
            + positif(BNCPROPVV) + positif(BNCPROV) + positif(BNCREC) + positif(BNCREP) + positif(BNCREV) + positif(BNHDEC) + positif(BNHDEP) + positif(BNHDEV) + positif(BNHEXC) + positif(BNHEXP) + positif(BNHEXV) 
	    + positif(BNHREC) + positif(BNHREP) + positif(BNHREV) + positif(BPCOPTV) + positif(BPCOSAC) + positif(BPCOSAV) + positif(BPV18V) + positif(BPV40V) + positif(BPVRCM) + positif(CARPEC) + positif(CARPENBAC) 
            + positif(CARPENBAV) + positif(CARPEV) + positif(CARPEP1) + positif(CARPEP2) + positif(CARPEP3) + positif(CARPEP4) + positif(CARPENBAP1) + positif(CARPENBAP2) + positif(CARPENBAP3) 
            + positif(CARPENBAP4) + positif(CARTSC) + positif(CARTSNBAC) + positif(CARTSNBAV) + positif(CARTSV) + positif(CARTSP1) + positif(CARTSP2) + positif(CARTSP3) + positif(CARTSP4) + positif(CARTSNBAP1) 
            + positif(CARTSNBAP2) + positif(CARTSNBAP3) + positif(CARTSNBAP4) + positif(REMPLAV) + positif(REMPLAC) + positif(REMPLAP1) + positif(REMPLAP2) + positif(REMPLAP3) + positif(REMPLAP4) 
            + positif(REMPLANBV) + positif(REMPLANBC) + positif(REMPLANBP1) + positif(REMPLANBP2) + positif(REMPLANBP3) + positif(REMPLANBP4) + positif(PENSALV) + positif(PENSALC) + positif(PENSALP1) 
            + positif(PENSALP2) + positif(PENSALP3) + positif(PENSALP4) + positif(PENSALNBV) + positif(PENSALNBC) + positif(PENSALNBP1) + positif(PENSALNBP2) + positif(PENSALNBP3) + positif(PENSALNBP4) 
            + positif(RENTAX) + positif(RENTAX5) + positif(RENTAX6) + positif(RENTAX7) + positif(RENTAXNB) + positif(RENTAXNB5) + positif(RENTAXNB6) + positif(RENTAXNB7) + positif(REVACT) + positif(REVPEA) 
	    + positif(PROVIE) + positif(DISQUO) + positif(RESTUC) + positif(INTERE) + positif(REVACTNB) + positif(REVPEANB) + positif(PROVIENB) + positif(DISQUONB) + positif(RESTUCNB) + positif(INTERENB) 
            + positif(CESSASSC) + positif(CESSASSV) + positif(COTF1) + positif(COTF2) + positif(COTF3) + positif(COTF4) + positif(COTFC) + positif(COTFV) + positif(DABNCNP1) + positif(DABNCNP2) + positif(DABNCNP3) 
            + positif(DABNCNP4) + positif(DABNCNP5) + positif(DABNCNP6) + positif(DAGRI1) + positif(DAGRI2) + positif(DAGRI3) + positif(DAGRI4) + positif(DAGRI5) + positif(DAGRI6) + positif(DEFBIC1) + positif(DEFBIC2) 
	    + positif(DEFBIC3) + positif(DEFBIC4) + positif(DEFBIC5) + positif(DEFBIC6) + positif(DNOCEP) + positif(DNOCEPC) + positif(DNOCEPP) + positif(DPVRCM) + positif(FRN1) + positif(FRN2) + positif(FRN3) + positif(FRN4) 
	    + positif(FRNC) + positif(FRNV) + positif(GAINABDET) + positif(GLDGRATV) + positif(GLDGRATC) + positif(LOCDEFNPC) + positif(LOCDEFNPCGAC) + positif(LOCDEFNPCGAPAC) + positif(LOCDEFNPCGAV) 
	    + positif(LOCDEFNPPAC) + positif(LOCDEFNPV) + positif(LOCNPC) + positif(LOCNPCGAC) + positif(LOCNPCGAPAC) + positif(LOCNPCGAV) + positif(LOCNPPAC) + positif(LOCNPV) + positif(MIB1AC) 
	    + positif(MIB1AP) + positif(MIB1AV) + positif(BICPMVCTV) + positif(BICPMVCTC) + positif(BICPMVCTP) + positif(MIBDEC) + positif(MIBDEP) + positif(MIBDEV) + positif(MIBEXC) + positif(MIBEXP) + positif(MIBEXV) 
            + positif(MIBNP1AC) + positif(MIBNP1AP) + positif(MIBNP1AV) + positif(MIBNPDCT) + positif(MIBNPDEC) + positif(MIBNPDEP) + positif(MIBNPDEV) + positif(MIBNPEXC) + positif(MIBNPEXP) 
            + positif(MIBNPEXV) + positif(MIBNPPRESC) + positif(MIBNPPRESP) + positif(MIBNPPRESV) + positif(MIBNPPVC) + positif(MIBNPPVP) + positif(MIBNPPVV) + positif(MIBNPVENC) + positif(MIBNPVENP) 
            + positif(MIBNPVENV) + positif(MIBPRESC) + positif(MIBPRESP) + positif(MIBPRESV) + positif(MIBPVC) + positif(MIBPVP) + positif(MIBPVV) + positif(MIBVENC) + positif(MIBVENP) + positif(MIBVENV) 
	    + positif(PALI1) + positif(PALI2) + positif(PALI3) + positif(PALI4) + positif(PALIC) + positif(PALIV) + positif(PEBF1) + positif(PEBF2) 
	    + positif(PEBF3) + positif(PEBF4) + positif(PEBFC) + positif(PEBFV) + positif(PPLIB) 
            + positif(PRBR1) + positif(PRBR2) + positif(PRBR3) + positif(PRBR4) + positif(PRBRC) + positif(PRBRV) + positif(PVINCE) + positif(PVINPE) + positif(PVINVE) + positif(PVREP8) + positif(PVSOCC) + positif(PVSOCV) 
	    + positif(RCMABD) + positif(RCMAV) + positif(RCMAVFT) + positif(RCMFR) + positif(RCMHAB) + positif(COD2TZ) + positif(RCMHAD) + positif(RCMLIB) + positif(COD2RA)+ positif(RCMRDS) + positif(RCMSOC) + positif(RCMTNC) + positif(RCSC) + positif(RCSP) 
            + positif(RCSV) + positif(REGPRIV) + positif(RFDANT) + positif(RFDHIS) + positif(RFDORD) + positif(RFMIC) + positif(RFORDI) + positif(RFROBOR) + positif(RVB1) + positif(RVB2) + positif(RVB3) + positif(RVB4) 
            + positif(TSASSUC) + positif(TSASSUV) + positif(TSHALLO1) + positif(TSHALLO2) + positif(TSHALLO3) + positif(TSHALLO4) + positif(TSHALLOC) + positif(TSHALLOV) + positif(XETRANC) + positif(XETRANV) 
            + positif(XSPENPC) + positif(XSPENPP) + positif(XSPENPV) + positif(GSALV) + positif(GSALC) + positif(LNPRODEF1) + positif(LNPRODEF2) + positif(LNPRODEF3) + positif(LNPRODEF4) + positif(LNPRODEF5) 
      + positif(LNPRODEF6) + positif(LNPRODEF7) + positif(LNPRODEF8) + positif(LNPRODEF9) + positif(LNPRODEF10) + positif(FONCI) + positif(REAMOR) + positif(FONCINB) + positif(REAMORNB) 
      + positif(MIBMEUV) + positif(MIBMEUC) + positif(MIBMEUP) + positif(MIBGITEV) + positif(MIBGITEC) + positif(MIBGITEP) + positif(PCAPTAXV) + positif(PCAPTAXC) 
      + positif(COD1CT) + positif(COD1DT) + positif(COD1ET) + positif(COD1FT) + positif(PVIMMO) + positif(PVSURSI) 
      + positif(PVIMPOS) + positif(BANOCGAV) + positif(BANOCGAC) + positif(BANOCGAP) + positif(INVENTV) + positif(INVENTC) + positif(INVENTP) + positif(LOCGITV) + positif(LOCGITC) + positif(LOCGITP) 
      + positif(LOCGITCV) + positif(LOCGITCC) + positif(LOCGITCP) + positif(LOCGITHCV) + positif(LOCGITHCC) + positif(LOCGITHCP) + positif(PVTAXSB) + positif(PVMOBNR) + positif(BPVSJ) + positif(BPVSK) 
      + positif(CVNSALAV) + positif(GAINPEA) + positif(PVEXOSEC) + positif(ABPVNOSURSIS) + positif(PVREPORT) + positif(SALEXTV) + positif(SALEXTC) + positif(SALEXT1) + positif(SALEXT2) 
      + positif(SALEXT3) + positif(SALEXT4) + positif(CODDAJ) + positif(CODEAJ) + positif(CODDBJ) + positif(CODEBJ) + positif(PVTITRESOC) + positif(PENIN1) + positif(PENIN2) + positif(PENIN3) + positif(PENIN4) 
      + positif(PENINC) + positif(PENINV) + positif(CODNAZ) + positif(CODNBZ) + positif(CODNCZ) + positif(CODNDZ) + positif(CODNEZ) + positif(CODNFZ) + positif(CODRAZ) + positif(CODRBZ) + positif(CODRCZ) + positif(CODRDZ) 
      + positif(CODREZ) + positif(CODRFZ) + positif(CODNVG) + positif(CODRVG) + positif(ABIMPPV) + positif(COD1AE) + positif(COD1BE) + positif(COD1CE) 
      + positif(COD1DE) + positif(COD1EE) + positif(COD1FE) + positif(COD1AH) + positif(COD1BH) + positif(COD1CH) + positif(COD1DH) + positif(COD1EH) 
      + positif(COD1FH) + positif(COD1TZ) + positif(COD1AF) + positif(COD1AG) + positif(COD1AL) + positif(COD1AM) + positif(COD1AR) + positif(COD1BF) 
      + positif(COD1BG) + positif(COD1BL) + positif(COD1BM) + positif(COD1BR) + positif(COD1CF) + positif(COD1CG) + positif(COD1CL) + positif(COD1CM) 
      + positif(COD1CR) + positif(COD1DF) + positif(COD1DG) + positif(COD1DL) + positif(COD1DM) + positif(COD1DR) + positif(COD1EF) + positif(COD1EG) 
      + positif(COD1EL) + positif(COD1EM) + positif(COD1FF) + positif(COD1FG) + positif(COD1FL) + positif(COD1FM) + positif(COD1NX) + positif(COD1OX) 
      + positif(COD1PM) + positif(COD1QM) + positif(COD1TP) + positif(COD1UP) + positif(COD1UZ) + positif(COD1VZ) + positif(COD1GB) + positif(COD1HB) 
      + positif(COD1IB) + positif(COD1JB) + positif(CODRAF) + positif(CODNAF) + positif(CODRAG) + positif(CODNAG) + positif(CODRBF) + positif(CODNBF) 
      + positif(CODRBG) + positif(CODNBG) + positif(CODRCF) + positif(CODNCF) + positif(CODRCG) + positif(CODNCG) + positif(CODRDF) + positif(CODNDF) 
      + positif(CODRDG) + positif(CODNDG) + positif(CODREF) + positif(CODNEF) + positif(CODRGG) + positif(CODNGG) + positif(CODRFF) + positif(CODNFF) 
      + positif(CODRFG) + positif(CODNFG) + positif(CODRAL) + positif(CODNAL) + positif(CODRAM) + positif(CODNAM) + positif(CODRAR) + positif(CODNAR) 
      + positif(CODRBL) + positif(CODNBL) + positif(CODRBM) + positif(CODNBM) + positif(CODRBR) + positif(CODNBR) + positif(CODRCL) + positif(CODNCL) 
      + positif(CODRCM) + positif(CODNCM) + positif(CODRCR) + positif(CODNCR) + positif(CODRDL) + positif(CODNDL) + positif(CODRDM) + positif(CODNDM) 
      + positif(CODRDR) + positif(CODNDR) + positif(CODREL) + positif(CODNEL) + positif(CODREM) + positif(CODNEM) + positif(CODRFL) + positif(CODNFL) 
      + positif(CODRFM) + positif(CODNFM) + positif(COD1AA) + positif(COD1BA) + positif(COD1CA) + positif(COD1DA) + positif(COD1EA) + positif(COD1FA)
      + positif(COD1GE) + positif(COD1HE) + positif(COD1IE) + positif(COD1JE) + positif(COD1KE) + positif(COD1LE) + positif(COD1GF) + positif(COD1HF) 
      + positif(COD1IF) + positif(COD1JF) + positif(COD1KF) + positif(COD1LF) + positif(COD2CK) + positif(COD2TT) + positif(COD2TU) + positif(COD2OP) 
      + positif(COD2TV) + positif(COD2TW) + positif(COD2UU) + positif(COD2VV) + positif(COD2VM) + positif(COD2VN) + positif(COD2VO) + positif(COD2VP) 
      + positif(COD2TQ) + positif(COD2RB) + positif(COD2RC) + positif(COD2RD) + positif(COD2VQ) + positif(COD2VR) + positif(COD2WW) + positif(COD2XX) 
      + positif(COD2YY) + positif(COD2ZZ) + positif(COD3SL) + positif(COD3UA) + positif(COD3WM) + positif(COD3SA) + positif(COD3WI) + positif(COD3WJ) 
      + positif(COD3PI) + positif(COD3SG) + positif(COD3SZ) + positif(COD3TJ) + positif(COD3WG) + positif(COD3WN) + positif(COD3WP) + positif(COD3WR) 
      + positif(COD3WT) + positif(COD3XA) + positif(COD3XD) + positif(COD3XM) + positif(COD3XN) + positif(COD3YA) + positif(COD4BK) + positif(COD4BL) 
      + positif(COD4BN) + positif(COD5AD) + positif(COD5AF) + positif(COD5AI) + positif(COD5AK) + positif(COD5AL) + positif(COD5AN) + positif(COD5AQ) 
      + positif(COD5AR) + positif(COD5AY) + positif(COD5AZ) + positif(COD5BD) + positif(COD5BF) + positif(COD5BI) + positif(COD5BK) + positif(COD5BL) 
      + positif(COD5BN) + positif(COD5BQ) + positif(COD5BR) + positif(COD5BY) + positif(COD5BZ) + positif(COD5CK) + positif(COD5CL) + positif(COD5DB) 
      + positif(COD5DF) + positif(COD5DG) + positif(COD5DK) + positif(COD5DL) + positif(COD5DM) + positif(COD5DN) + positif(COD5EB) + positif(COD5EF) 
      + positif(COD5EG) + positif(COD5EK) + positif(COD5EL) + positif(COD5EM) + positif(COD5EN) + positif(COD5EY) + positif(COD5EZ) + positif(COD5FF) 
      + positif(COD5FG) + positif(COD5FY) + positif(COD5FZ) + positif(COD5GY) + positif(COD5GZ) + positif(COD5LD) + positif(COD5MD) + positif(COD5RZ) 
      + positif(COD5SZ) + positif(COD5UP) + positif(COD5UR) + positif(COD5US) + positif(COD5UT) + positif(COD5UU) + positif(COD5UY) + positif(COD5UZ) 
      + positif(COD5VM) + positif(COD5VN) + positif(COD5VP) + positif(COD5VR) + positif(COD5VS) + positif(COD5VT) + positif(COD5VU) + positif(COD5VY) 
      + positif(COD5VZ) + positif(COD5WM) + positif(COD5WN) + positif(COD5WR) + positif(COD5WS) + positif(COD5XA) + positif(COD5XB) + positif(COD5XH) 
      + positif(COD5XI) + positif(COD5XJ) + positif(COD5XK) + positif(COD5XL) + positif(COD5XN) + positif(COD5XO) + positif(COD5XP) + positif(COD5XQ) 
      + positif(COD5XR) + positif(COD5XS) + positif(COD5XX) + positif(COD5XY) + positif(COD5XZ) + positif(COD5YA) + positif(COD5YB) + positif(COD5YH) 
      + positif(COD5YI) + positif(COD5YJ) + positif(COD5YK) + positif(COD5YL) + positif(COD5YN) + positif(COD5YO) + positif(COD5YP) + positif(COD5YQ) 
      + positif(COD5YR) + positif(COD5YS) + positif(COD5YX) + positif(COD5YY) + positif(COD5YZ) + positif(COD5ZA) + positif(COD5ZB) + positif(COD5ZJ) 
      + positif(COD5ZK) + positif(COD5ZN) + positif(COD5ZO) + positif(COD5ZS) + positif(COD5ZX) + positif(COD5AH) + positif(COD5BH) + positif(COD5CM) 
      + positif(COD5CN) + positif(COD5CQ) + positif(COD5CR) + positif(COD5CU) + positif(COD5CV) + positif(COD5CY) + positif(COD5CZ) + positif(COD5ED) 
      + positif(COD5FB) + positif(COD5FD) + positif(COD5FK) + positif(COD5FL) + positif(COD5FM) + positif(COD5FN) + positif(CODCJG) + positif(CODCKC) 
      + positif(CODCKI) + positif(CODCLC) + positif(CODCLI) + positif(CODCMC) + positif(CODCMI) + positif(CODCNC) + positif(CODCNI) + positif(CODCNS) 
      + positif(COD5NW) + positif(CODCOC) + positif(CODCOI) + positif(CODCOS) + positif(COD5OW) + positif(CODCPC) + positif(CODCPI) + positif(COD5PW) 
      + positif(CODCQC) + positif(CODCQI) + positif(CODCRC) + positif(CODCRF) + positif(CODCRI) + positif(CODCSC) + positif(CODCSF) + positif(CODCSI) 
      + positif(CODCSN) + positif(COD5TP) + positif(COD5VQ) + positif(COD5VV) + positif(COD5VW) + positif(COD5VX) + positif(COD5XT) + positif(COD5XU) 
      + positif(COD5XV) + positif(COD5XW) + positif(COD5ZH) + positif(COD5ZI) + positif(COD5ZL) + positif(COD5ZM) + positif(COD5ZP) + positif(COD5ZQ) 
      + positif(COD5ZR) + positif(COD5ZW) + positif(COD5ZY) + positif(COD5ZZ) + positif(COD5XM) + positif(COD5YM) + positif(COD5YT) + positif(COD5YU) 
      + positif(COD5ZT) + positif(COD5ZU) + positif(CODNGG) + positif(CODRBT) + positif(COD5HA) + positif(COD5IA) + positif(COD5JA) + positif(COD5UI) 
      + positif(COD5VI) + positif(COD5WI) + positif(COD5TF) + positif(COD5UF) + positif(COD5VF) + positif(COD5QA) + positif(COD5RA) + positif(COD5SA) 
      + positif(COD5QJ) + positif(COD5RJ) + positif(COD5SJ) + positif(COD2DF) + positif(COD1AI) + positif(COD1BI) + positif(COD1CI) + positif(COD1DI) 
      + positif(COD1EI) + positif(COD1FI) + positif(COD1GG) + positif(COD1GH) + positif(COD1GK) + positif(COD1GL) + positif(COD1GP) + positif(COD1GQ) 
      + positif(COD1GR) + positif(COD1GS) + positif(COD1HG) + positif(COD1HH) + positif(COD1HK) + positif(COD1HL) + positif(COD1HP) + positif(COD1HQ) 
      + positif(COD1HR) + positif(COD1HS) + positif(COD1IG) + positif(COD1IH) + positif(COD1JG) + positif(COD1JH) + positif(COD1KG) + positif(COD1KH) 
      + positif(COD1LG) + positif(COD1LH) + positif(COD2DF) + positif(COD2DG) + positif(COD2DI) + positif(COD2RA) + positif(COD2RB) + positif(COD2RC) 
      + positif(COD2RD) + positif(COD2TQ) + positif(COD2TX) + positif(COD3AN) + positif(COD3BN) + positif(COD3TK) + positif(COD5EA) + positif(COD5EC) 
      + positif(COD5EI) + positif(COD5EQ) + positif(COD5EU) + positif(COD5EV) + positif(COD5HA) + positif(COD5IA) + positif(COD5JA) + positif(COD5QA) 
      + positif(COD5QJ) + positif(COD5RA) + positif(COD5RJ) + positif(COD5SA) + positif(COD5SJ) + positif(COD5TF) + positif(COD5UF) + positif(COD5UI) 
      + positif(COD5VF) + positif(COD5VI) + positif(COD5WI) + positif(CODRBE) 
      + positif(CODNBE + CODRBK + CODBIS + CODRAI + CODRBI + CODRCK + COD2TY + COD2VS + CODRYY + COD5TJ + COD5TK + COD5TL + COD5UJ + COD5UK + COD5UL 
                + COD5VJ + COD5VK + COD5VL + COD5WE + COD5WF + COD5XE + COD5XF + COD5YE + COD5YF)
      + positif(COD2VT + CODBJS) 
      + positif(COD1AD + COD1AV + COD1BD + COD1BV + COD1CD + COD1CV + COD1DD + COD1DV + COD1ED + COD1EV + COD1FD + COD1FV + COD1PB + COD1PC + COD1PD 
                + COD1PE + COD1PF + COD1PG + COD2VU + CODBKS + COD5AC + COD5AE + COD5AG + COD5AT + COD5AX + COD5BC + COD5BE + COD5BG + COD5BT + COD5BX
		+ COD5CC + COD5CE + COD5CG + COD5CT + COD5CX)

   + present(ANNUL2042) + present(ASCAPA) + present(AUTOVERSLIB) + present(BRAS) + present(CASEPRETUD)
   + present(CELREPYM) + present(CELREPYN) + present(CELREPYO) + present(CELREPYP) + present(CELREPYT) + present(CELREPYU) 
   + present(CELREPYV) + present(CELREPYW) + present(CELREPWT) + present(CELREPWU) + present(CELREPWV) + present(CELREPWW) 
   + present(CELRREDLQ) + present(CELRREDLR) + present(CELRREDLU) + present(CELRREDLV) 
   + present(DUFLOFK) + present(DUFLOFR) + present(DUFLOFV)
   + present(PINELBI) + present(PINELDI) + present(PINELCZ) + present(PINELEZ) 
   + present(PINELRZ) + present(PINELTZ)
   + present(CHENF1) + present(CHENF2) + present(CHENF3) + present(CHENF4) + present(INVNPROF1) + present(CHNFAC) + present(CHRDED) + present(CHRFAC)
   + present(CIIMPPRO) + present(CIIMPPRO2) + present(CIINVCORSE) + present(CINE1) + present(CINE2) + present(CO35) + present(RISKTEC) 
   + present(CRDSIM) + present(CREAGRIBIO) + present(CREAIDE) + present(CREARTS) + present(CRECONGAGRI) + present(CREDPVREP) + present(CREFAM) + present(CREFORMCHENT) 
   + present(COD8YT) + present(CDISPROV) + present(CSGIM) + present(COD8YL) + present(DCSG) + present(DCSGIM)
   + present(DEFAA0) + present(DEFAA1) + present(DEFAA2) + present(DEFAA3) + present(DEFAA4) + present(DEFAA5) + present(DMOND) + present(ESFP) + present(FCPI)
   + present(FFIP) + present(FIPCORSE) + present(FORET) + present(INAIDE) + present(INTDIFAGRI) + present(INVLGDEB2009)  
   + present(INVLOG2008) + present(INVLOG2009) + present(INVLGAUTRE) + present(INVLGDEB2010) + present(INVLGDEB) 
   + present(INVOMLOGOA) + present(INVOMLOGOB) + present(INVOMLOGOC) + present(INVOMLOGOH) + present(INVOMLOGOI) 
   + present(INVOMLOGOJ) + present(INVOMLOGOK) + present(INVOMLOGOL) + present(INVOMLOGOM) + present(INVOMLOGON) 
   + present(INVOMLOGOO) + present(INVOMLOGOP) + present(INVOMLOGOQ) + present(INVOMLOGOR) + present(INVOMLOGOS) 
   + present(INVOMLOGOT) + present(INVOMLOGOU) + present(INVOMLOGOV) + present(INVOMLOGOW)
   + present(LOCMEUBII) + present(COD7JZ) 
   + present(IPBOCH) + present( IPMOND ) + present( SALECS )
   + present(SALECSG) + present(CICORSENOW) + present(PRESINTER) + present(IPPNCS) + present( IPPRICORSE ) + present( IPRECH ) + present( IPCHER )
   + present(IPREP) + present(IPREPCORSE) + present(IPSOUR) + present(IPSUIS) + present(IPSUISC) + present(IPSURSI) + present(IPTEFN) + present(IPTEFP)
   + present(IPTXMO) + present(IRANT) + present(LOCRESINEUV) + present(RESIVIEU) + present(NBACT)
   + present(NCHENF1) + present(NCHENF2) + present(NCHENF3) + present(NCHENF4) + present(NRBASE) + present(NRINET) 
   + present(IMPRET) + present(BASRET) + present(REPGROREP12)
   + present(REPGROREP13) + present(REPGROREP14) + present(COD6HP) + present(COD6HQ) + present(OPTPLAF15) + present(PAAP) + present(PAAV) 
   + present(PERPC) + present(PERPP) + present(PERPV) + present(PERP_COTC) + present(PERP_COTP) + present(PERP_COTV)
   + present(PLAF_PERPC) + present(PLAF_PERPP) + present(PLAF_PERPV) + present(PREHABT) + present(PREHABTN2) + present(PREHABTVT)
   + present(PREHABT2) + present(PREMAIDE) + present(PRESCOMP2000) + present(PRESCOMPJUGE) + present(PRETUD)
   + present(PRETUDANT) + present(PRODOM) + present(PROGUY) + present(PRSPROV) + present(R1649) + present(PREREV)
   + present(RCCURE) + present(RDCOM) + present(RDDOUP) + present(RDENL) + present(RDENLQAR) + present(RDENS)
   + present(RDENSQAR) + present(RDENU) + present(RDENUQAR) + present(RDEQPAHA) + present(RDDOUP) + present(RDFOREST)
   + present(RDFORESTGES) + present(RDFORESTRA) + present(RDREP) + present(COTFORET) + present(REPSINFOR5)
   + present(RDGARD1) + present(RDGARD1QAR) + present(RDGARD2) + present(RDGARD2QAR) + present(RDGARD3) + present(RDGARD3QAR)
   + present(RDGARD4) + present(RDGARD4QAR) + present(RDTECH) + present(RDMECENAT) + present(RDPRESREPORT) + present(RDREP)
   + present(RDRESU) + present(RDSYCJ) + present(RDSYPP) + present(RDSYVO) + present(RE168) + present(TAX1649) + present(REGCI) 
   + present(REPDON03) + present(REPDON04) + present(REPDON05) + present(REPDON06) + present(REPDON07)   
   + present(REPSOF) + present(REVMAR1) + present(REVMAR2) + present(REVMAR3) + present(RMOND) + present(RSOCREPRISE) + present(RVCURE) + present(SINISFORET)
   + present(SUBSTITRENTE) + present(FIPDOMCOM) + present(ALLECS) + present(INDECS) + present(PENECS) + present(DONETRAN) + present(DONAUTRE)
   + present(RFRN2) + present(RFRN1) + present(RFRH1) + present(RFRH2) + present(COD8TL) + present(COD8UW) + present(V_8ZT) 
   + present(COD6HR) + present(COD7ZO) + present(COD7ZP) + present(COD7NY) + present(COD7NX) + present(COD7UH) + present(COD7CR) + present(COD7CV) 
   + present(COD7CY) + present(COD7UA) + present(COD7UB) + present(COD7UI)  
   + present(COD7DY) + present(COD7OF) + present(COD7OG) + present(COD7OH) + present(COD7OI) + present(COD7OJ) 
   + present(COD7TK) + present(COD7CX) + present(COD7EY) 
   + present(COD7MX) + present(COD7MY) + present(COD7OK) + present(COD7OL) + present(COD7OM) + present(COD7ON) + present(COD7OO) + present(COD7OW) 
   + present(COD7TM) + present(COD7TO) + present(COD7WK) 
   + present(COD7EN) + present(COD7FY) + present(COD7OP) + present(COD7OQ) + present(COD7OR) + present(COD7OS) + present(COD7OT) + present(COD7OX) 
   + present(COD7PP) + present(COD7PQ) + present(COD7PR) + present(COD7PS) + present(COD7PT) + present(COD7TP) + present(COD7TQ) + present(COD7TX) 
   + present(COD7TY) + present(COD7WQ) + present(COD7XX) + present(COD7ZO) + present(COD7ZP) + present(COD7FW) + present(COD7GY) 
   + present(COD7LA) + present(COD7LB) + present(COD7LC) + present(COD7LY) + present(COD7OY) + present(COD7PU) + present(COD7PV) + present(COD7PW) 
   + present(COD7PX) + present(COD7PY) + present(COD7RA) + present(COD7RB) 
   + present(COD7RC) + present(COD7RD) + present(COD7RT) + present(COD7RU) + present(COD7SA) + present(COD7SB) 
   + present(COD7SC) + present(COD7SU) + present(COD7TR) + present(COD7TS) + present(COD7WH) 
   + present(COD7WI) + present(COD7XO) + present(COD7XP) + present(COD7XQ) + present(CODHOD) + present(CODHOE) + present(CODHOF) + present(CODHOG) 
   + present(CODHOX) + present(CODHOY) + present(CODHOZ) + present(CODHUA) + present(CODHUB) + present(CODHUC) + present(CODHUD) + present(CODHUE) 
   + present(CODHUF) + present(CODHUG) 
   + present(CODHJA) + present(CODHUH) + present(CODHUI) + present(CODHUJ) + present(CODHUK) + present(CODHUL) 
   + present(CODHUM) + present(CODHUN) + present(CODHUO) + present(CODHUP) + present(CODHUQ) + present(CODHUR) + present(CODHUS) + present(CODHUT) 
   + present(CODHUU) 
   + present(CODHDI) + present(CODHDJ) + present(CODHDK) + present(CODHDM) + present(CODHDN) 
   + present(CODHDO) + present(CODHDP) + present(CODHDR) + present(CODHDS) + present(CODHDT) + present(CODHDU) + present(CODHDW) + present(CODHVA) 
   + present(CODHVB) + present(CODHVC) + present(CODHVD) + present(CODHVE) + present(CODHVF) + present(CODHVG) + present(CODHXQ) + present(CODHXR) 
   + present(CODHXS) + present(CODHXT) + present(CODHXU) + present(CODHEN) + present(CODHEO) + present(CODHEP) + present(CODHER) + present(CODHES) 
   + present(CODHET) + present(CODHEU) + present(CODHEW) + present(CODHHC) + present(CODHIC) + present(CODHJC) 
   + present(CODHKC) + present(CODHVH) + present(CODHYA) + present(CODHYB) + present(CSPROVYN) + present(CSPROVYP)
   + present(COD8SA) + present(COD8SB) + present(COD8XI) + present(COD8XJ) + present(COD8XY) + present(COD8YM) + present(CODZRU) + present(COD8SC) 
   + present(COD8SW) + present(COD8SX) + present(COD8VL) + present(COD8VM) + present(COD8WM) + present(COD8OV) + present(COD8UM) + present(COD8AU) 
   + present(COD8AV) + present(COD8AW) + present(COD8AX) + present(COD8AY) + present(COD8AZ) + present(CODZRA) + present(CODZRB) + present(CODZRE) 
   + present(CODZRF) + present(COD8BA) + present(COD8BB) + present(COD8BC) + present(COD8BD) + present(COD8EA) + present(COD8SH) + present(COD8XX) 
   + present(COD8XV) + present(COD6NS) + present(COD6NT) + present(COD6NU) + present(COD6OS) + present(COD6OT) + present(COD6OU) + present(COD6NS)
   + present(COD6NT) 
   + present(COD7FX) + present(COD7HO) + present(COD7HP) + present(COD7HQ) + present(COD7HR) 
   + present(COD7HS) + present(COD7MS) + present(COD7MT) + present(COD7MU) + present(COD7MV) + present(COD7NA) + present(COD7NB) 
   + present(COD7NC) + present(COD7ND) + present(COD7PZ) + present(COD7MZ) + present(COD7QQ) + present(COD7QW) + present(COD7QX) + present(COD7QY)
   + present(COD7RE) + present(COD7RF) + present(COD7RG) + present(COD7RH) 
   + present(COD7SN) + present(COD7SO) + present(COD7TT) + present(COD7TU) + present(COD7VJ) + present(COD7VK)  
   + present(COD7WS) + present(COD7YI) 
   + present(COD7YJ) + present(COD7YK) + present(COD7YL) + present(COD8EB) + present(COD8HV) + present(COD8HW) 
   + present(COD8HX) + present(COD8HY) + present(COD8HZ) + present(COD8IE) + present(COD8IF) + present(COD8IV) + present(COD8IW) + present(COD8IX) 
   + present(COD8IY) + present(COD8IZ) + present(COD8JV) + present(COD8JW) + present(COD8JX) + present(COD8JY) + present(COD8JZ) + present(COD8KV) 
   + present(COD8KW) + present(COD8KX) + present(COD8KY) + present(COD8KZ) + present(COD8LG) + present(COD8LH) + present(COD8LI) + present(COD8LJ) 
   + present(COD8LK) + present(COD8LL) + present(COD8LV) + present(COD8LW) + present(COD8LX) + present(COD8LY) + present(COD8LZ) + present(COD8MV) 
   + present(COD8MW) + present(COD8MX) + present(COD8MY) + present(COD8MZ) + present(COD8PB) + present(COD8SD) + present(COD8TH) + present(COD8XN) 
   + present(COD8XO) + present(COD8XV) + present(CODHFN) + present(CODHFO) + present(CODHFP) + present(CODHFR) + present(CODHFS) + present(CODHFT) 
   + present(CODHFU) + present(CODHFW) + present(CODHVI) + present(CODHYC) + present(CODHYD)
   + present(COD6DG) 
   + present(COD7CH) 
   + present(COD7DR) 
   + present(COD7FT) + present(COD7FU) + present(COD7GR) + present(COD7GW) + present(COD7HB) 
   + present(COD7HT) + present(COD7HU) + present(COD7HV) + present(COD7HW) + present(COD7HX) 
   + present(COD7JA) + present(COD7JB) + present(COD7JC) + present(COD7JD) 
   + present(COD7JM) + present(COD7KC) + present(COD7KD) + present(COD7KM)
   + present(COD7KX) + present(COD7LM) + present(COD7MM) + present(COD7MO) + present(COD7MP) + present(COD7MQ) + present(COD7MR)
   + present(COD7NE) + present(COD7NF) + present(COD7NG) + present(COD7NH) + present(COD7QA) + present(COD7QB) + present(COD7QC)
   + present(COD7QD) + present(COD7QF) + present(COD7QH) + present(COD7SP) + present(COD7SV) + present(COD7TV)
   + present(COD7TW) + present(COD7VH) + present(COD7VI) 
   + present(COD7XZ) + present(COD7ZI)
   + present(COD7ZJ) + present(COD7ZK) + present(COD7ZL) + present(CODHGS) + present(CODHGT) + present(CODHGU)
   + present(CODHGW) + present(CODHVJ) + present(CODHYE) + present(COD8TE) + present(COD8YV)
   + present(COD8YX) 
   + present(COD7BS) + present(COD7CI) + present(COD7CS) + present(COD7HA) + present(COD7HD) + present(COD7HE) + present(COD7HF) + present(COD7HG) + present(COD7HH)
   + present(COD7HJ) + present(COD7HK) + present(COD7HN) + present(COD7HY) + present(COD7IA) + present(COD7IB) + present(COD7IC) + present(COD7IE) + present(COD7IF)
   + present(COD7IG) + present(COD7IH) + present(COD7IK) + present(COD7IL) + present(COD7IO) + present(COD7IP) + present(COD7IQ) + present(COD7JN) + present(COD7JO)
   + present(COD7JP) + present(COD7JQ) + present(COD7JR) + present(COD7JS) + present(COD7JT) + present(COD7JU) + present(COD7KW) + present(COD7LD) + present(COD7LE)
   + present(COD7LF) + present(COD7LN) + present(COD7LT) + present(COD7LX) + present(COD7LZ) + present(COD7MA) + present(COD7MB) + present(COD7MC) + present(COD7MD)
   + present(COD7MG) + present(COD7MH) + present(COD7MW) + present(COD7NI) + present(COD7NJ) + present(COD7NK) + present(COD7NL) + present(COD7PA) + present(COD7PC)
   + present(COD7PD) + present(COD7PE) + present(COD7QI) + present(COD7QJ) + present(COD7QK) + present(COD7QL) + present(COD7RX) + present(COD7RY) + present(COD7SD)
   + present(COD7SE) + present(COD7SF) + present(COD7SG) + present(COD7SH) + present(COD7SI) + present(COD7SJ) + present(COD7SK) + present(COD7SL) + present(COD7SM)
   + present(COD7SQ) + present(COD7SR) + present(COD7SW) + present(COD7SX) + present(COD7SY) + present(COD7TA) + present(COD7TB) + present(COD7UJ) + present(COD7UU)
   + present(COD7UV) + present(COD7UW) + present(COD7UX) + present(COD7VM) + present(COD7VN) + present(COD7XA) + present(COD7XB)
   + present(COD7XC) + present(COD7XL) + present(COD7XM) + present(COD7XN) + present(COD7XR) + present(COD7YA) + present(COD7YC) + present(COD7YG) + present(COD7YR)
   + present(COD7YS) + present(COD7ZQ) + present(COD7ZR) + present(COD7ZS) + present(COD7ZT) + present(COD7ZU) + present(COD7ZV) + present(CODHHS) + present(CODHHT)   
   + present(CODHHU) + present(CODHHW) + present(CODHVK) + present(CODHVL) + present(CODHYF) + present(CODHYG)
   + present(COD8UA) + present(COD8UB) + present(COD8UC) + present(COD8WG) + present(COD8WH) + present(COD8ZQ) + present(COD8ZX)
   + present(COD6EX) + present(COD6EZ) + present(COD6GX) + present(COD6GZ)
   + present(COD7AA) + present(COD7AB) + present(COD7AD) + present(COD7AF) + present(COD7AH) + present(COD7AI) + present(COD7AP) + present(COD7AR) + present(COD7AS) 
   + present(COD7AT) + present(COD7AU) + present(COD7BA) + present(COD7BB) + present(COD7BC) + present(COD7BD) + present(COD7BE) + present(COD7BF) + present(COD7BG) 
   + present(COD7BH) + present(COD7BJ) + present(COD7BK) + present(COD7BL) + present(COD7BM) + present(COD7BN) + present(COD7BO) + present(COD7BT) + present(COD7CA) 
   + present(COD7CT) + present(COD7DC) + present(COD7EK) + present(COD7GS) + present(COD7GU) + present(COD7GX) + present(COD7HL) + present(COD7HM) + present(COD7HZ) 
   + present(COD7KE) + present(COD7KF) + present(COD7KG) + present(COD7KH) + present(COD7KI) + present(COD7KJ) + present(COD7KL) + present(COD7KN) 
   + present(COD7KO) + present(COD7KQ) + present(COD7KR) + present(COD7KS) + present(COD7KT) + present(COD7KU) + present(COD7KV) + present(COD7KZ) 
   + present(COD7MN) + present(COD7PB) + present(COD7PI) + present(COD7PJ) + present(COD7QE) + present(COD7RK) + present(COD7RL) + present(COD7RM) 
   + present(COD7RN) + present(COD7SS) + present(COD7TE) + present(COD7TF) + present(COD7UG) + present(COD7VQ) + present(COD7VR) + present(COD7WC) + present(COD7WD) 
   + present(COD7WE) + present(COD7WF) + present(COD7WG) + present(COD7WX) + present(COD7WY) + present(COD7WZ) + present(COD7XH) + present(COD7XI) + present(COD7XJ) 
   + present(COD7XK) + present(COD7XV) + present(COD7YX) + present(COD7YY) + present(COD7YZ) + present(COD7ZW) + present(COD7ZX) + present(COD7ZY) + present(COD7ZZ) 
   + present(CODHIS) + present(CODHIT) + present(CODHIU) + present(CODHIV) + present(CODHIW) + present(COD7QM) + present(COD7QN) + present(COD7QO) + present(COD7QP) 
   + present(COD7JV) + present(COD7JW) + present(COD7JX) + present(COD7JY) + present(COD7RP) + present(COD7RQ) + present(COD7RI) + present(COD7RJ) + present(COD7UY) 
   + present(COD7UZ) + present(COD7RX) + present(COD7RY) + present(COD7NM) + present(COD7NN) + present(COD7PF) + present(COD7PG) + present(COD7LG) + present(COD7LH) 
   + present(COD7LI) + present(COD7LJ) + present(COD7MI) + present(COD7MJ) + present(COD7MK) + present(COD7ML) + present(COD7ZM)
   + present(COD8SG) + present(COD8WK)
 ) ;

INDREV1A8 = positif(INDREV1A8BIS) ;

IND_REV = positif(INDREV1A8 + positif(REVFONC)) ;

VAR9GN = positif(COD9GN + 0) ;


VARPERP = positif(present(PERPPLAFCV) + present(PERPPLAFNUV3) + present(PERPPLAFNUV2) + present(PERPPLAFNUV1)
                  + present(PERPPLAFCC) + present(PERPPLAFNUC3) + present(PERPPLAFNUC2) + present(PERPPLAFNUC1)
		  + present(PERPPLAFCP) + present(PERPPLAFNUP3) + present(PERPPLAFNUP2) + present(PERPPLAFNUP1)) ;  
INDTELEIR = positif(
              positif(4BACREC) + positif(4BACREP) + positif(4BACREV) + positif(4BAHREC) + positif(4BAHREP) + positif(4BAHREV) 
	    + positif(ABDETPLUS) + positif(ALLO1) + positif(ALLO2) + positif(ALLO3) + positif(ALLO4) 
	    + positif(ALLOC) + positif(ALLOV) + positif(ANOCEP) + positif(ANOPEP) + positif(ANOVEP) + positif(AUTOBICPC) 
	    + positif(AUTOBICPP) + positif(AUTOBICPV) + positif(AUTOBICVC) + positif(AUTOBICVP) 
	    + positif(AUTOBICVV) + positif(AUTOBNCC) + positif(AUTOBNCP) + positif(AUTOBNCV) + positif(BA1AC) 
	    + positif(BA1AP) + positif(BA1AV) + positif(BACDEC) + positif(BACDEP) + positif(BACDEV) + positif(BACREC) 
	    + positif(BACREP) + positif(BACREV) + positif(BAEXC) + positif(BAEXP) + positif(BAEXV) + positif(BAF1AC) 
	    + positif(BAF1AP) + positif(BAF1AV) + positif(BAFORESTC) + positif(BAFORESTP) + positif(BAFORESTV) 
            + positif(BAFPVC) + positif(BAFPVP) + positif(BAFPVV) + positif(BAHDEC) + positif(BAHDEP) + positif(BAHDEV) 
	    + positif(BAHEXC) + positif(BAHEXP) + positif(BAHEXV) + positif(BAHREC) + positif(BAHREP) + positif(BAHREV) 
	    + positif(BAILOC98) + positif(BAPERPC) + positif(BAPERPP) + positif(BAPERPV) + positif(BI1AC) + positif(BI1AP) 
	    + positif(BI1AV) + positif(BI2AC) + positif(BI2AP) + positif(BI2AV) + positif(BICDEC) + positif(BICDEP) 
	    + positif(BICDEV) + positif(BICDNC) + positif(BICDNP) + positif(BICDNV) + positif(BICEXC) + positif(BICEXP) + positif(BICEXV)
	    + positif(BICHDEC) + positif(BICHDEP) + positif(BICHDEV) + positif(BICHREC) 
	    + positif(BICHREP) + positif(BICHREV) + positif(BICNOC) + positif(BICNOP) + positif(BICNOV) + positif(BICNPEXC) + positif(BICNPEXP)
	    + positif(BICNPEXV) + positif(BICNPHEXC) + positif(BICNPHEXP) 
	    + positif(BICNPHEXV) + positif(BICREC) + positif(BICREP) + positif(BICREV) + positif(BIHDNC) + positif(BIHDNP) + positif(BIHDNV)
	    + positif(BIHEXC) + positif(BIHEXP) + positif(BIHEXV) + positif(BIHNOC) 
	    + positif(BIHNOP) + positif(BIHNOV) + positif(BN1AC) + positif(BN1AP) + positif(BN1AV) + positif(BNCAABC) + positif(BNCAABP) + positif(BNCAABV)
	    + positif(BNCAADC) + positif(BNCAADP) + positif(BNCAADV)
            + positif(BNCCRC) + positif(BNCCRFC) + positif(BNCCRFP) + positif(BNCCRFV) + positif(BNCCRP) + positif(BNCCRV) + positif(BNCDEC) + positif(BNCDEP) 
	    + positif(BNCDEV) + positif(BNCEXC) + positif(BNCEXP) 
	    + positif(BNCEXV) + positif(BNCNP1AC) + positif(BNCNP1AP) + positif(BNCNP1AV) + positif(BNCNPC) + positif(BNCNPDCT) + positif(BNCNPDEC) + positif(BNCNPDEP) + positif(BNCNPDEV) + positif(BNCNPP) 
            + positif(BNCNPPVC) + positif(BNCNPPVP) + positif(BNCNPPVV) + positif(BNCNPREXAAC) + positif(BNCNPREXAAP) + positif(BNCNPREXAAV) + positif(BNCNPREXC) + positif(BNCNPREXP) 
	    + positif(BNCNPREXV) + positif(BNCNPV) + positif(BNCPRO1AC) + positif(BNCPRO1AP) + positif(BNCPRO1AV) + positif(BNCPROC) + positif(BNCPMVCTV) + positif(BNCPMVCTC) + positif(BNCPMVCTP) 
            + positif(BNCPRODEC) + positif(BNCPRODEP) + positif(BNCPRODEV) + positif(BNCPROEXC) + positif(BNCPROEXP) + positif(BNCPROEXV) + positif(BNCPROP) + positif(BNCPROPVC) + positif(BNCPROPVP) 
            + positif(BNCPROPVV) + positif(BNCPROV) + positif(BNCREC) + positif(BNCREP) + positif(BNCREV) + positif(BNHDEC) + positif(BNHDEP) 
	    + positif(BNHDEV) + positif(BNHEXC) + positif(BNHEXP) + positif(BNHEXV) 
	    + positif(BNHREC) + positif(BNHREP) + positif(BNHREV) + positif(BPCOPTV) + positif(BPCOSAC) + positif(BPCOSAV) 
	    + positif(BPV18V) + positif(BPV40V) + positif(BPVRCM) + positif(CARPEC) + positif(CARPENBAC) 
            + positif(CARPENBAV) + positif(CARPEV) + positif(CARPEP1) + positif(CARPEP2) + positif(CARPEP3) + positif(CARPEP4) + positif(CARPENBAP1) + positif(CARPENBAP2) + positif(CARPENBAP3) 
            + positif(CARPENBAP4) + positif(CARTSC) + positif(CARTSNBAC) + positif(CARTSNBAV) + positif(CARTSV) + positif(CARTSP1) 
	    + positif(CARTSP2) + positif(CARTSP3) + positif(CARTSP4) + positif(CARTSNBAP1) 
            + positif(CARTSNBAP2) + positif(CARTSNBAP3) + positif(CARTSNBAP4) + positif(REMPLAV) + positif(REMPLAC) + positif(REMPLAP1) + positif(REMPLAP2) + positif(REMPLAP3) + positif(REMPLAP4) 
            + positif(REMPLANBV) + positif(REMPLANBC) + positif(REMPLANBP1) + positif(REMPLANBP2) + positif(REMPLANBP3) + positif(REMPLANBP4) + positif(PENSALV) + positif(PENSALC) + positif(PENSALP1) 
            + positif(PENSALP2) + positif(PENSALP3) + positif(PENSALP4) + positif(PENSALNBV) + positif(PENSALNBC) + positif(PENSALNBP1) + positif(PENSALNBP2) + positif(PENSALNBP3) + positif(PENSALNBP4) 
            + positif(RENTAX) + positif(RENTAX5) + positif(RENTAX6) + positif(RENTAX7) + positif(RENTAXNB) + positif(RENTAXNB5) + positif(RENTAXNB6) + positif(RENTAXNB7) + positif(REVACT) + positif(REVPEA) 
	    + positif(PROVIE) + positif(DISQUO) + positif(RESTUC) + positif(INTERE) + positif(REVACTNB) + positif(REVPEANB) + positif(PROVIENB) + positif(DISQUONB) + positif(RESTUCNB) + positif(INTERENB) 
            + positif(CESSASSC) + positif(CESSASSV) + positif(COTF1) + positif(COTF2) + positif(COTF3) + positif(COTF4) + positif(COTFC) 
	    + positif(COTFV) + positif(DABNCNP1) + positif(DABNCNP2) + positif(DABNCNP3) 
            + positif(DABNCNP4) + positif(DABNCNP5) + positif(DABNCNP6) + positif(DAGRI1) + positif(DAGRI2) + positif(DAGRI3) + positif(DAGRI4) 
	    + positif(DAGRI5) + positif(DAGRI6) + positif(DEFBIC1) + positif(DEFBIC2) 
	    + positif(DEFBIC3) + positif(DEFBIC4) + positif(DEFBIC5) + positif(DEFBIC6) + positif(DNOCEP) + positif(DNOCEPC) + positif(DNOCEPP) 
	    + positif(DPVRCM) + positif(FRN1) + positif(FRN2) + positif(FRN3) + positif(FRN4) 
	    + positif(FRNC) + positif(FRNV) + positif(GAINABDET) + positif(GLDGRATV) + positif(GLDGRATC) + positif(LOCDEFNPC) + positif(LOCDEFNPCGAC) + positif(LOCDEFNPCGAPAC) + positif(LOCDEFNPCGAV) 
	    + positif(LOCDEFNPPAC) + positif(LOCDEFNPV) + positif(LOCNPC) + positif(LOCNPCGAC) + positif(LOCNPCGAPAC) + positif(LOCNPCGAV) + positif(LOCNPPAC) + positif(LOCNPV) + positif(MIB1AC) 
	    + positif(MIB1AP) + positif(MIB1AV) + positif(BICPMVCTV) + positif(BICPMVCTC) + positif(BICPMVCTP) + positif(MIBDEC) 
	    + positif(MIBDEP) + positif(MIBDEV) + positif(MIBEXC) + positif(MIBEXP) + positif(MIBEXV) 
            + positif(MIBNP1AC) + positif(MIBNP1AP) + positif(MIBNP1AV) + positif(MIBNPDCT) + positif(MIBNPDEC) + positif(MIBNPDEP) + positif(MIBNPDEV) + positif(MIBNPEXC) + positif(MIBNPEXP) 
            + positif(MIBNPEXV) + positif(MIBNPPRESC) + positif(MIBNPPRESP) + positif(MIBNPPRESV) + positif(MIBNPPVC) + positif(MIBNPPVP) + positif(MIBNPPVV) + positif(MIBNPVENC) + positif(MIBNPVENP) 
            + positif(MIBNPVENV) + positif(MIBPRESC) + positif(MIBPRESP) + positif(MIBPRESV) + positif(MIBPVC) + positif(MIBPVP) + positif(MIBPVV) + positif(MIBVENC) + positif(MIBVENP) + positif(MIBVENV) 
	    + positif(PALI1) + positif(PALI2) + positif(PALI3) + positif(PALI4) + positif(PALIC) + positif(PALIV) + positif(PEBF1) + positif(PEBF2) 
	    + positif(PEBF3) + positif(PEBF4) + positif(PEBFC) + positif(PEBFV) + positif(PPLIB) 
            + positif(PRBR1) + positif(PRBR2) + positif(PRBR3) + positif(PRBR4) + positif(PRBRC) + positif(PRBRV) + positif(PVINCE) 
	    + positif(PVINPE) + positif(PVINVE) + positif(PVREP8) + positif(PVSOCC) + positif(PVSOCV) 
	    + positif(RCMABD) + positif(RCMAV) + positif(RCMAVFT) + positif(RCMFR) + positif(RCMHAB) + positif(RCMHAD) + positif(RCMLIB) +  positif(COD2RA)
	    + positif(RCMRDS) + positif(RCMSOC) + positif(RCMTNC) + positif(RCSC) + positif(RCSP) 
            + positif(RCSV) + positif(REGPRIV) + positif(RFDANT) + positif(RFDHIS) + positif(RFDORD) + positif(RFMIC) + positif(RFORDI) 
	    + positif(RFROBOR) + positif(RVB1) + positif(RVB2) + positif(RVB3) + positif(RVB4) 
            + positif(TSASSUC) + positif(TSASSUV) + positif(TSHALLO1) + positif(TSHALLO2) + positif(TSHALLO3) + positif(TSHALLO4) + positif(TSHALLOC) + positif(TSHALLOV) + positif(XETRANC) + positif(XETRANV) 
            + positif(XSPENPC) + positif(XSPENPP) + positif(XSPENPV) + positif(GSALV) + positif(GSALC) + positif(LNPRODEF1) + positif(LNPRODEF2) + positif(LNPRODEF3) + positif(LNPRODEF4) + positif(LNPRODEF5) 
            + positif(LNPRODEF6) + positif(LNPRODEF7) + positif(LNPRODEF8) + positif(LNPRODEF9) + positif(LNPRODEF10) + positif(FONCI) + positif(REAMOR) + positif(FONCINB) + positif(REAMORNB) 
            + positif(MIBMEUV) + positif(MIBMEUC) + positif(MIBMEUP) + positif(MIBGITEV) + positif(MIBGITEC) + positif(MIBGITEP) + positif(PCAPTAXV) + positif(PCAPTAXC) 
	    + positif(COD1CT) + positif(COD1DT) + positif(COD1ET) + positif(COD1FT) + positif(PVIMMO) + positif(PVSURSI) 
	    + positif(PVIMPOS) + positif(BANOCGAV) + positif(BANOCGAC) + positif(BANOCGAP) + positif(INVENTV) + positif(INVENTC) + positif(INVENTP) + positif(LOCGITV) + positif(LOCGITC) + positif(LOCGITP) 
            + positif(LOCGITCV) + positif(LOCGITCC) + positif(LOCGITCP) + positif(LOCGITHCV) + positif(LOCGITHCC) + positif(LOCGITHCP) + positif(PVTAXSB) + positif(PVMOBNR) + positif(BPVSJ) + positif(BPVSK) 
            + positif(CVNSALAV) + positif(GAINPEA) + positif(PVEXOSEC) + positif(ABPVNOSURSIS) + positif(PVREPORT) + positif(SALEXTV) + positif(SALEXTC) + positif(SALEXT1) + positif(SALEXT2) 
	    + positif(SALEXT3) + positif(SALEXT4) + positif(CODDAJ) + positif(CODEAJ) + positif(CODDBJ) + positif(CODEBJ) 
	    + positif(PVTITRESOC) + positif(PENIN1) + positif(PENIN2) + positif(PENIN3) + positif(PENIN4) 
	    + positif(PENINC) + positif(PENINV) + positif(CODNAZ) + positif(CODNBZ) + positif(CODNCZ) + positif(CODNDZ)
	    + positif(CODNEZ) + positif(CODNFZ) + positif(CODRAZ) + positif(CODRBZ) + positif(CODRCZ) + positif(CODRDZ) 
	    + positif(CODREZ) + positif(CODRFZ) + positif(CODNVG) + positif(CODRVG) + positif(ABIMPPV) + positif(COD1AE) 
	    + positif(COD1BE) + positif(COD1CE) + positif(COD1DE) + positif(COD1EE) + positif(COD1FE) + positif(COD1AH) 
            + positif(COD1BH) + positif(COD1CH) + positif(COD1DH) + positif(COD1EH) + positif(COD1FH) + positif(COD1TZ)
	    + positif(COD1AF) + positif(COD1AG) + positif(COD1AL) + positif(COD1AM) + positif(COD1AR) + positif(COD1BF) 
	    + positif(COD1BG) + positif(COD1BL) + positif(COD1BM) + positif(COD1BR) + positif(COD1CF) + positif(COD1CG) 
	    + positif(COD1CL) + positif(COD1CM) + positif(COD1CR) + positif(COD1DF) + positif(COD1DG) + positif(COD1DL) 
	    + positif(COD1DM) + positif(COD1DR) + positif(COD1EF) + positif(COD1EG) + positif(COD1EL) + positif(COD1EM)
	    + positif(COD1FF) + positif(COD1FG) + positif(COD1FL) + positif(COD1FM) + positif(COD1NX) + positif(COD1OX) 
	    + positif(COD1PM) + positif(COD1QM) + positif(COD1TP) + positif(COD1UP) + positif(COD1UZ) + positif(COD1VZ)
	    + positif(COD1GB) + positif(COD1HB) + positif(COD1IB) + positif(COD1JB) + positif(CODRAF) + positif(CODNAF) 
	    + positif(CODRAG) + positif(CODNAG) + positif(CODRBF) + positif(CODNBF) + positif(CODRBG) + positif(CODNBG)
	    + positif(CODRCF) + positif(CODNCF) + positif(CODRCG) + positif(CODNCG) + positif(CODRDF) + positif(CODNDF) 
	    + positif(CODRDG) + positif(CODNDG) + positif(CODREF) + positif(CODNEF) + positif(CODRGG) + positif(CODNGG)
	    + positif(CODRFF) + positif(CODNFF) + positif(CODRFG) + positif(CODNFG) + positif(CODRAL) + positif(CODNAL) 
	    + positif(CODRAM) + positif(CODNAM) + positif(CODRAR) + positif(CODNAR) + positif(CODRBL) + positif(CODNBL) 
	    + positif(CODRBM) + positif(CODNBM) + positif(CODRBR) + positif(CODNBR) + positif(CODRCL) + positif(CODNCL) 
	    + positif(CODRCM) + positif(CODNCM) + positif(CODRCR) + positif(CODNCR) + positif(CODRDL) + positif(CODNDL)
	    + positif(CODRDM) + positif(CODNDM) + positif(CODRDR) + positif(CODNDR) + positif(CODREL) + positif(CODNEL) 
            + positif(CODREM) + positif(CODNEM) + positif(CODRFL) + positif(CODNFL) + positif(CODRFM) + positif(CODNFM)
	    + positif(COD1AA) + positif(COD1BA) + positif(COD1CA) + positif(COD1DA) + positif(COD1EA) + positif(COD1FA)
            + positif(COD1GE) + positif(COD1HE) + positif(COD1IE) + positif(COD1JE)  
	    + positif(COD1KE) + positif(COD1LE)  
	    + positif(COD1GF) + positif(COD1HF) + positif(COD1IF) + positif(COD1JF) + positif(COD1KF) + positif(COD1LF)
            + positif(COD2CK) + positif(COD2TT) + positif(COD2TU) + positif(COD2OP) + positif(COD2TV) + positif(COD2VM)
	    + positif(COD2VN) + positif(COD2VO) + positif(COD2VP) + positif(COD2VQ) + positif(COD2VR) 
	    + positif(COD3SL) + positif(COD3UA) + positif(COD3WM) + positif(COD3SA) + positif(COD3WI) + positif(COD3WJ) 
	    + positif(COD3PI) + positif(COD3SG) + positif(COD3SZ) + positif(COD3WG) + positif(COD3WN) + positif(COD3WP) 
	    + positif(COD3WR) + positif(COD3WT) + positif(COD3XA) + positif(COD3XD) + positif(COD3XM) + positif(COD3YA) 
	    + positif(COD4BK) + positif(COD4BL) + positif(COD4BN) + positif(COD5AD) 
	    + positif(COD5AF) + positif(COD5AI) + positif(COD5AK) + positif(COD5AL) + positif(COD5AN) + positif(COD5AQ)
	    + positif(COD5AR) + positif(COD5AY) + positif(COD5AZ) + positif(COD5BD) + positif(COD5BF) + positif(COD5BI) 
	    + positif(COD5BK) + positif(COD5BL) + positif(COD5BN) + positif(COD5BQ) + positif(COD5BR) + positif(COD5BY)
	    + positif(COD5BZ) + positif(COD5CK) + positif(COD5CL) + positif(COD5DB) + positif(COD5DF) + positif(COD5DG) 
	    + positif(COD5DK) + positif(COD5DL) + positif(COD5DM) + positif(COD5DN) + positif(COD5EB) + positif(COD5EF)
	    + positif(COD5EG) + positif(COD5EK) + positif(COD5EL) + positif(COD5EM) + positif(COD5EN) + positif(COD5EY) 
	    + positif(COD5EZ) + positif(COD5FF) + positif(COD5FG) + positif(COD5FY) + positif(COD5FZ) + positif(COD5GY)
	    + positif(COD5GZ) + positif(COD5LD) + positif(COD5MD) + positif(COD5RZ) + positif(COD5SZ) + positif(COD5UP) 
            + positif(COD5UR) + positif(COD5US) + positif(COD5UT) + positif(COD5UU) + positif(COD5UY) + positif(COD5UZ)
	    + positif(COD5VM) + positif(COD5VN) + positif(COD5VP) + positif(COD5VR) + positif(COD5VS) + positif(COD5VT) 
	    + positif(COD5VU) + positif(COD5VY) + positif(COD5VZ) + positif(COD5WM) + positif(COD5WN) + positif(COD5WR)
	    + positif(COD5WS) + positif(COD5XA) + positif(COD5XB) + positif(COD5XH) + positif(COD5XI) 
	    + positif(COD5XJ) + positif(COD5XK) + positif(COD5XL) + positif(COD5XN) + positif(COD5XO) + positif(COD5XP)
	    + positif(COD5XQ) + positif(COD5XR) + positif(COD5XS) + positif(COD5XX) + positif(COD5XY) + positif(COD5XZ) 
	    + positif(COD5YA) + positif(COD5YB) + positif(COD5YH) + positif(COD5YI) + positif(COD5YJ)
	    + positif(COD5YK) + positif(COD5YL) + positif(COD5YN) + positif(COD5YO) + positif(COD5YP) + positif(COD5YQ) 
	    + positif(COD5YR) + positif(COD5YS) + positif(COD5YX) + positif(COD5YY) + positif(COD5YZ) + positif(COD5ZA)
	    + positif(COD5ZB) + positif(COD5ZJ) + positif(COD5ZK) + positif(COD5ZN) + positif(COD5ZO) 
	    + positif(COD5ZS) + positif(COD5ZX) + positif(COD5AH) + positif(COD5BH) + positif(COD5CM) + positif(COD5CN)
	    + positif(COD5CQ) + positif(COD5CR) + positif(COD5CU) + positif(COD5CV) + positif(COD5CY) + positif(COD5CZ) 
	    + positif(COD5ED) + positif(COD5FB) + positif(COD5FD) + positif(COD5FK) + positif(COD5FL) + positif(COD5FM)
	    + positif(COD5FN) + positif(CODCJG) + positif(CODCKC) + positif(CODCKI) + positif(CODCLC) + positif(CODCLI) 
            + positif(CODCMC) + positif(CODCMI) + positif(CODCNC) + positif(CODCNI) + positif(CODCNS) + positif(COD5NW)
	    + positif(CODCOC) + positif(CODCOI) + positif(CODCOS) + positif(COD5OW) + positif(CODCPC) + positif(CODCPI) 
	    + positif(COD5PW) + positif(CODCQC) + positif(CODCQI) + positif(CODCRC) + positif(CODCRF) + positif(CODCRI)
	    + positif(CODCSC) + positif(CODCSF) + positif(CODCSI) + positif(CODCSN) + positif(COD5TP) + positif(COD5VQ) 
	    + positif(COD5VV) + positif(COD5VW) + positif(COD5VX) + positif(COD5XT) + positif(COD5XU) + positif(COD5XV)
	    + positif(COD5XW) + positif(COD5ZH) + positif(COD5ZI) + positif(COD5ZL) + positif(COD5ZM) + positif(COD5ZP) 
	    + positif(COD5ZQ) + positif(COD5ZR) + positif(COD5ZW) + positif(COD5ZY) + positif(COD5ZZ) 
            + positif(COD5XM) + positif(COD5YM) + positif(COD5YT) + positif(COD5YU) 
	    + positif(COD5ZT) + positif(COD5ZU) + positif(CODNGG) + positif(CODRBT) + positif(COD2TW) + positif(COD2UU) 
	    + positif(COD2VV) + positif(COD2WW) + positif(COD2XX) + positif(COD2YY) + positif(COD2ZZ) + positif(COD3TJ)
	    + positif(COD3XN)
            + positif(COD1AI) + positif(COD1BI) + positif(COD1CI) + positif(COD1DI) + positif(COD1EI) + positif(COD1FI)
	    + positif(COD1GG) + positif(COD1GH) + positif(COD1GK) + positif(COD1GL) + positif(COD1GP) + positif(COD1GQ)
            + positif(COD1GR) + positif(COD1GS) + positif(COD1HG) + positif(COD1HH) + positif(COD1HK) + positif(COD1HL) + positif(COD1HP) + positif(COD1HQ)
            + positif(COD1HR) + positif(COD1HS) + positif(COD1IG) + positif(COD1IH) + positif(COD1JG) + positif(COD1JH) + positif(COD1KG) + positif(COD1KH)
            + positif(COD1LG) + positif(COD1LH)
            + positif(COD2DF) + positif(COD2DG) + positif(COD2DI) + positif(COD2RA) + positif(COD2RB) + positif(COD2RC) + positif(COD2RD) + positif(COD2TQ)
            + positif(COD2TX)
            + positif(COD3AN) + positif(COD3BN) + positif(COD3TK)
            + positif(COD5EA) + positif(COD5EC) + positif(COD5EI) + positif(COD5EQ) + positif(COD5EU) + positif(COD5EV) + positif(COD5HA) + positif(COD5IA)
            + positif(COD5JA) + positif(COD5QA) + positif(COD5QJ) + positif(COD5RA) + positif(COD5RJ) + positif(COD5SA) + positif(COD5SJ) + positif(COD5TF)
            + positif(COD5UF) + positif(COD5UI) + positif(COD5VF) + positif(COD5VI) + positif(COD5WI)
            + positif(CODRBE) + positif(CODNBE) + positif(CODRBK) + positif(CODBIS)
	    + positif(CODRAI) + positif(CODRBI) + positif(CODRCK) + positif(COD2TY) + positif(COD2VS) + positif(CODRYY) + positif(COD5TJ)
	    + positif(COD5TK) + positif(COD5TL) + positif(COD5UJ) + positif(COD5UK) + positif(COD5UL) + positif(COD5VJ) + positif(COD5VK) + positif(COD5VL)
	    + positif(COD5WE) + positif(COD5WF) + positif(COD5XE) + positif(COD5XF) + positif(COD5YE) + positif(COD5YF) + positif(COD2VT) + positif(CODBJS)
            + positif(COD1AD + COD1AV + COD1BD + COD1BV + COD1CD + COD1CV + COD1DD + COD1DV + COD1ED + COD1EV + COD1FD + COD1FV + COD1PB + COD1PC + COD1PD 
                      + COD1PE + COD1PF + COD1PG + COD2VU + CODBKS + COD5AC + COD5AE + COD5AG + COD5AT + COD5AX + COD5BC + COD5BE + COD5BG + COD5BT + COD5BX
		      + COD5CC + COD5CE + COD5CG + COD5CT + COD5CX)

           + positif(ANNUL2042)
           + positif(ASCAPA) + positif(AUTOVERSLIB) + positif(BRAS) +  positif(CASEPRETUD)
           + positif(CELREPYM) + positif(CELREPYN) + positif(CELREPYO) + positif(CELREPYP) + positif(CELREPYT) + positif(CELREPYU) + positif(CELREPYV) 
	   + positif(CELREPYW) + positif(CELREPWT) + positif(CELREPWU) + positif(CELREPWV) + positif(CELREPWW) + positif(CELRREDLQ) + positif(CELRREDLR) 
	   + positif(CELRREDLU) + positif(CELRREDLV)
           + positif(DUFLOFK) + positif(DUFLOFR) + positif(DUFLOFV)
           + positif(PINELBI) + positif(PINELDI) + positif(PINELCZ) + positif(PINELEZ) + positif(PINELRZ) + positif(PINELTZ)
           + positif(CHENF1) + positif(CHENF2) + positif(CHENF3) + positif(CHENF4)
           + positif(INVNPROF1) 
           + positif(CHNFAC) + positif(CHRDED) + positif(CHRFAC)
           + positif(CIIMPPRO) + positif(CIIMPPRO2) + positif(CIINVCORSE) 
           + positif(CINE1) + positif(CINE2) + positif(CO35) + positif(RISKTEC) 
           + positif(CRDSIM) + positif(CREAGRIBIO) + positif(CREAIDE) + positif(CREARTS)
           + positif(CRECONGAGRI) + positif(CREDPVREP)
	   + positif(CREFAM) + positif(CREFORMCHENT) 
           + positif(COD8YT) + positif(CDISPROV) + positif(CSGIM) + positif(COD8YL)
           + positif(DCSG) + positif(DCSGIM)
	   + positif(DEFAA0) + positif(DEFAA1) + positif(DEFAA2) + positif(DEFAA3) + positif(DEFAA4) + positif(DEFAA5)
           + positif(DMOND) + positif(ESFP) + positif(FCPI)
           + positif(FFIP) + positif(FIPCORSE) + positif(FORET)
           + positif(INAIDE) + positif(INTDIFAGRI)
	   + positif(INVLGDEB2009)
           + positif(INVLOG2008) + positif(INVLOG2009) + positif(INVLGAUTRE)
           + positif(INVLGDEB2010) + positif(INVLGDEB)
           + positif(INVOMLOGOA) + positif(INVOMLOGOB) + positif(INVOMLOGOC) + positif(INVOMLOGOH) + positif(INVOMLOGOI)
           + positif(INVOMLOGOJ) + positif(INVOMLOGOK) + positif(INVOMLOGOL) + positif(INVOMLOGOM) + positif(INVOMLOGON)
           + positif(INVOMLOGOO) + positif(INVOMLOGOP) + positif(INVOMLOGOQ) + positif(INVOMLOGOR) + positif(INVOMLOGOS)
           + positif(INVOMLOGOT) + positif(INVOMLOGOU) + positif(INVOMLOGOV) + positif(INVOMLOGOW)
           + positif(LOCMEUBII)
           + positif(COD7JZ)
           + positif(IPBOCH) + positif( IPMOND ) + positif( SALECS )
           + positif(SALECSG) + positif( CICORSENOW ) + positif( PRESINTER )
           + positif(IPPNCS) + positif( IPPRICORSE ) + positif( IPRECH ) + positif( IPCHER )
           + positif(IPREP) + positif( IPREPCORSE ) + positif( IPSOUR )
           + positif(IPSUIS) + positif( IPSUISC ) + positif( IPSURSI )
           + positif( IPTEFN ) + positif( IPTEFP )
           + positif(IPTXMO) + positif( IRANT )
           + positif(LOCRESINEUV) + positif(RESIVIEU) + positif(NBACT)
           + positif(NCHENF1) + positif(NCHENF2) + positif(NCHENF3) + positif(NCHENF4) + positif(NRBASE) + positif(NRINET)
           + positif(IMPRET) + positif(BASRET)
           + positif( REPGROREP12 )
           + positif(REPGROREP13) + positif( REPGROREP14 ) + positif( COD6HP ) + positif(COD6HQ)
           + positif(OPTPLAF15) + positif( PAAP ) + positif( PAAV )
           + positif(PERPC) + positif( PERPP ) + positif( PERPV )
           + positif(PERP_COTC) + positif( PERP_COTP ) + positif( PERP_COTV )
           + positif(PLAF_PERPC) + positif( PLAF_PERPP ) + positif( PLAF_PERPV )
           + positif(PREHABT) + positif( PREHABTN2 ) + positif( PREHABTVT )
           + positif(PREHABT2) + positif( PREMAIDE )
           + positif(PRESCOMP2000) + positif( PRESCOMPJUGE ) + positif( PRETUD )
           + positif(PRETUDANT) + positif( PRODOM ) + positif( PROGUY )
           + positif(PRSPROV) + positif( R1649 ) + positif( PREREV )
           + positif(RCCURE) + positif( RDCOM ) + positif( RDDOUP )
           + positif(RDENL) + positif( RDENLQAR ) + positif(RDENS)
	   + positif(RDENSQAR) + positif(RDENU) + positif(RDENUQAR)
	   + positif(RDEQPAHA) + positif(RDDOUP) + positif(RDFOREST)
	   + positif(RDFORESTGES) + positif( RDFORESTRA ) + positif( RDREP ) + positif( COTFORET )
	   + positif(REPSINFOR5 )
	   + positif(RDGARD1) + positif(RDGARD1QAR) + positif(RDGARD2)
	   + positif(RDGARD2QAR) + positif(RDGARD3) + positif(RDGARD3QAR)
	   + positif(RDGARD4) + positif(RDGARD4QAR) + positif(RDTECH)
	   + positif(RDMECENAT) + positif(RDPRESREPORT) + positif(RDREP)
	   + positif(RDRESU) + positif(RDSYCJ) + positif(RDSYPP) + positif(RDSYVO) + positif(RE168)
	   + positif(TAX1649) + positif(REGCI)
	   + positif(REPDON03) + positif(REPDON04) + positif(REPDON05) + positif(REPDON06) + positif(REPDON07)
	   + positif(REPSOF) + positif(REVMAR1) + positif(REVMAR2) + positif(REVMAR3)
	   + positif(RMOND) + positif(RSOCREPRISE) + positif(RVCURE) + positif(SINISFORET)
	   + positif(SUBSTITRENTE) + positif(FIPDOMCOM)
	   + positif(ALLECS) + positif(INDECS) + positif(PENECS) + positif(DONETRAN) + positif(DONAUTRE)
	   + positif(COD8TL) + positif(COD8UW) + positif(V_8ZT)
	   + positif(COD6HR)
	   + positif(COD7ZO) + positif(COD7ZP) + positif(COD7NY) + positif(COD7NX)
           + positif(COD7UH) + positif(COD7CR) + positif(COD7CV) + positif(COD7CY)
           + positif(COD7UA) + positif(COD7UB) + positif(COD7UI)
           + positif(COD7BS) + positif(COD7DY) + positif(COD7OF)
           + positif(COD7OG) + positif(COD7OH) + positif(COD7OI) + positif(COD7OJ)
           + positif(COD7TK) + positif(COD7CX) + positif(COD7CI) + positif(COD7CS)
   	   + positif(COD7EY) + positif(COD7MX) + positif(COD7MY) + positif(COD7OK) + positif(COD7OL) + positif(COD7OM) + positif(COD7ON)
	   + positif(COD7OO) + positif(COD7OW)
	   + positif(COD7TM) + positif(COD7TO) + positif(COD7WK) + positif(COD7EN) + positif(COD7FY)
	   + positif(COD7OP) + positif(COD7OQ) + positif(COD7OR) + positif(COD7OS) + positif(COD7OT) + positif(COD7OX) + positif(COD7PP)
	   + positif(COD7PQ) + positif(COD7PR) + positif(COD7PS) + positif(COD7PT) + positif(COD7TP) + positif(COD7TQ) + positif(COD7TX)
	   + positif(COD7TY) + positif(COD7WQ) + positif(COD7XX)
	   + positif(COD7FW) + positif(COD7GY) + positif(COD7LA)
	   + positif(COD7LB) + positif(COD7LC) + positif(COD7LY) + positif(COD7OY) + positif(COD7PU) + positif(COD7PV) + positif(COD7PW)
	   + positif(COD7PX) + positif(COD7PY) + positif(COD7RA)
	   + positif(COD7RB) + positif(COD7RC) + positif(COD7RD) + positif(COD7RT) + positif(COD7RU)
	   + positif(COD7SA) + positif(COD7SB) + positif(COD7SC) + positif(COD7SU)
	   + positif(COD7TR) + positif(COD7TS) + positif(COD7WH) + positif(COD7WI) + positif(COD7XO)
	   + positif(COD7XP) + positif(COD7XQ)
	   + positif(CODHOD) + positif(CODHOE) + positif(CODHOF) + positif(CODHOG) + positif(CODHOX) + positif(CODHOY) + positif(CODHOZ)
           + positif(CODHUA) + positif(CODHUB) + positif(CODHUC) + positif(CODHUD) + positif(CODHUE) + positif(CODHUF) + positif(CODHUG)
           + positif(CODHJA) + positif(CODHUH) + positif(CODHUI)
           + positif(CODHUJ) + positif(CODHUK) + positif(CODHUL) + positif(CODHUM) + positif(CODHUN) + positif(CODHUO) + positif(CODHUP)
           + positif(CODHUQ) + positif(CODHUR) + positif(CODHUS) + positif(CODHUT) + positif(CODHUU)
           + positif(CODHDI) + positif(CODHDJ) + positif(CODHDK) + positif(CODHDM) + positif(CODHDN) + positif(CODHDO)
           + positif(CODHDP) + positif(CODHDR) + positif(CODHDS) + positif(CODHDT) + positif(CODHDU) + positif(CODHDW) + positif(CODHVA)
           + positif(CODHVB) + positif(CODHVC) + positif(CODHVD) + positif(CODHVE) + positif(CODHVF) + positif(CODHVG) + positif(CODHXQ)
	   + positif(CODHXR) + positif(CODHXS) + positif(CODHXT) + positif(CODHXU) + positif(CODHEN) + positif(CODHEO)
	   + positif(CODHEP) + positif(CODHER) + positif(CODHES) + positif(CODHET) + positif(CODHEU)
	   + positif(CODHEW) + positif(CODHHC) + positif(CODHIC) + positif(CODHJC) + positif(CODHKC)
	   + positif(CODHVH) + positif(CODHYA) + positif(CODHYB)
	   + positif(CSPROVYN) + positif(CSPROVYP)
	   + positif(COD8SA) + positif(COD8SB) + positif(COD8XI) + positif(COD8XJ) + positif(COD8XY) + positif(COD8YM) + positif(CODZRU)
	   + positif(COD8SC) + positif(COD8SW) + positif(COD8SX) + positif(COD8VL) + positif(COD8VM) + positif(COD8WM) + positif(COD8OV)
	   + positif(COD8UM) + positif(COD8AU) + positif(COD8AV) + positif(COD8AW) + positif(COD8AX) + positif(COD8AY)
	   + positif(COD8AZ) + positif(CODZRA) + positif(CODZRB) + positif(CODZRE) + positif(CODZRF) + positif(COD8BA)
	   + positif(COD8BB) + positif(COD8BC) + positif(COD8BD) + positif(COD8EA) + positif(COD8SH) + positif(COD8XX)
	   + positif(COD8XV)
	   + positif(COD6NS) + positif(COD6NT) + positif(COD6NU) + positif(COD6OS) + positif(COD6OT) + positif(COD6OU) + positif(COD6NS) + positif(COD6NT)
           + positif(COD7FX) + positif(COD7HO) + positif(COD7HP) + positif(COD7HQ)
           + positif(COD7HR) + positif(COD7HS) + positif(COD7MS) + positif(COD7MT) + positif(COD7MU) + positif(COD7MV) + positif(COD7NA)
           + positif(COD7NB) + positif(COD7NC) + positif(COD7ND) + positif(COD7PZ) + positif(COD7MZ) + positif(COD7QQ) + positif(COD7QW) + positif(COD7QX) + positif(COD7QY)
           + positif(COD7RE) + positif(COD7RF) + positif(COD7RG) + positif(COD7RH)
           + positif(COD7SN) + positif(COD7SO) + positif(COD7TT)
           + positif(COD7TU) + positif(COD7VJ) + positif(COD7VK) + positif(COD7WS)
	   + positif(COD7YI) + positif(COD7YJ) + positif(COD7YK) + positif(COD7YL) + positif(COD8EB) + positif(COD8HV)
	   + positif(COD8HW) + positif(COD8HX) + positif(COD8HY) + positif(COD8HZ) + positif(COD8IE) + positif(COD8IF) + positif(COD8IV) + positif(COD8IW)
	   + positif(COD8IX) + positif(COD8IY) + positif(COD8IZ) + positif(COD8JV) + positif(COD8JW) + positif(COD8JX) + positif(COD8JY) + positif(COD8JZ)
	   + positif(COD8KV) + positif(COD8KX) + positif(COD8KW) + positif(COD8KY) + positif(COD8KZ) + positif(COD8LG) + positif(COD8LH) + positif(COD8LI)
	   + positif(COD8LJ) + positif(COD8LK) + positif(COD8LL) + positif(COD8LV) + positif(COD8LW) + positif(COD8LX) + positif(COD8LY) + positif(COD8LZ)
	   + positif(COD8MV) + positif(COD8MW) + positif(COD8MX) + positif(COD8MY) + positif(COD8MZ) + positif(COD8PB) + positif(COD8SD) + positif(COD8TH)
	   + positif(COD8XN) + positif(COD8XO) + positif(COD8XV) + positif(CODHFN) + positif(CODHFO) + positif(CODHFP) + positif(CODHFR)
	   + positif(CODHFS) + positif(CODHFT) + positif(CODHFU) + positif(CODHFW) + positif(CODHVI) + positif(CODHYC) + positif(CODHYD)
	   + positif(COD6DG)
           + positif(COD7CH)
	   + positif(COD7DR)
	   + positif(COD7FT) + positif(COD7FU)
	   + positif(COD7GR) + positif(COD7GW) + positif(COD7HB)
	   + positif(COD7HT) + positif(COD7HU) + positif(COD7HV) + positif(COD7HW) + positif(COD7HX)
	   + positif(COD7JA) + positif(COD7JB)
	   + positif(COD7JC) + positif(COD7JD) + positif(COD7JM) + positif(COD7KC) + positif(COD7KD) + positif(COD7KM)
	   + positif(COD7KX) + positif(COD7LM) + positif(COD7MM) + positif(COD7MO) + positif(COD7MP) + positif(COD7MQ) + positif(COD7MR)
	   + positif(COD7NE) + positif(COD7NF) + positif(COD7NG) + positif(COD7NH) + positif(COD7QA) + positif(COD7QB) + positif(COD7QC)
	   + positif(COD7QD) + positif(COD7QF) + positif(COD7QH) + positif(COD7SP) + positif(COD7SV) + positif(COD7TV)
	   + positif(COD7TW) + positif(COD7VH) + positif(COD7VI)
	   + positif(COD7XZ) + positif(COD7ZI)
	   + positif(COD7ZJ) + positif(COD7ZK) + positif(COD7ZL) + positif(CODHGS) + positif(CODHGT) + positif(CODHGU)
	   + positif(CODHGW) + positif(CODHVJ) + positif(CODHYE) + positif(COD8TE) + positif(COD8YV)
	   + positif(COD8YX)
+ positif(COD7BS) + positif(COD7CI) + positif(COD7CS) + positif(COD7HA) + positif(COD7HD) + positif(COD7HE) + positif(COD7HF) + positif(COD7HG) + positif(COD7HH)
+ positif(COD7HJ) + positif(COD7HK) + positif(COD7HN) + positif(COD7HY) + positif(COD7IA) + positif(COD7IB) + positif(COD7IC) + positif(COD7IE) + positif(COD7IF)
+ positif(COD7IG) + positif(COD7IH) + positif(COD7IK) + positif(COD7IL) + positif(COD7IO) + positif(COD7IP) + positif(COD7IQ) + positif(COD7JN) + positif(COD7JO)
+ positif(COD7JP) + positif(COD7JQ) + positif(COD7JR) + positif(COD7JS) + positif(COD7JT) + positif(COD7JU) + positif(COD7KW) + positif(COD7LD) + positif(COD7LE)
+ positif(COD7LF) + positif(COD7LN) + positif(COD7LT) + positif(COD7LX) + positif(COD7LZ) + positif(COD7MA) + positif(COD7MB) + positif(COD7MC) + positif(COD7MD)
+ positif(COD7MG) + positif(COD7MH) + positif(COD7MW) + positif(COD7NI) + positif(COD7NJ) + positif(COD7NK) + positif(COD7NL) + positif(COD7PA) + positif(COD7PC)
+ positif(COD7PD) + positif(COD7PE) + positif(COD7QI) + positif(COD7QJ) + positif(COD7QK) + positif(COD7QL) + positif(COD7RX) + positif(COD7RY) + positif(COD7SD)
+ positif(COD7SE) + positif(COD7SF) + positif(COD7SG) + positif(COD7SH) + positif(COD7SI) + positif(COD7SJ) + positif(COD7SK) + positif(COD7SL) + positif(COD7SM)
+ positif(COD7SQ) + positif(COD7SR) + positif(COD7SW) + positif(COD7SX) + positif(COD7SY) + positif(COD7TA) + positif(COD7TB) + positif(COD7UJ) + positif(COD7UU)
+ positif(COD7UV) + positif(COD7UW) + positif(COD7UX) + positif(COD7VM) + positif(COD7VN) + positif(COD7XA) + positif(COD7XB)
+ positif(COD7XC) + positif(COD7XL) + positif(COD7XM) + positif(COD7XN) + positif(COD7XR) + positif(COD7YA) + positif(COD7YC) + positif(COD7YG) + positif(COD7YR)
+ positif(COD7YS) + positif(COD7ZQ) + positif(COD7ZR) + positif(COD7ZS) + positif(COD7ZT) + positif(COD7ZU) + positif(COD7ZV) + positif(CODHHS) + positif(CODHHT)
+ positif(CODHHU) + positif(CODHHW) + positif(CODHVK) + positif(CODHVL) + positif(CODHYF) + positif(CODHYG)
+ positif(COD8UA) + positif(COD8UB) + positif(COD8UC) + positif(COD8WG) + positif(COD8WH) + positif(COD8ZQ) + positif(COD8ZX)
+ positif(COD6EX) + positif(COD6EZ) + positif(COD6GX) + positif(COD6GZ)
+ positif(COD7AA) + positif(COD7AB) + positif(COD7AD) + positif(COD7AF) + positif(COD7AH) + positif(COD7AI) + positif(COD7AP) + positif(COD7AR) + positif(COD7AS)
+ positif(COD7AT) + positif(COD7AU) + positif(COD7BA) + positif(COD7BB) + positif(COD7BC) + positif(COD7BD) + positif(COD7BE) + positif(COD7BF) + positif(COD7BG)
+ positif(COD7BH) + positif(COD7BJ) + positif(COD7BK) + positif(COD7BL) + positif(COD7BM) + positif(COD7BN) + positif(COD7BO) + positif(COD7BT) + positif(COD7CA)
+ positif(COD7CT) + positif(COD7DC) + positif(COD7EK) + positif(COD7GS) + positif(COD7GU) + positif(COD7GX) + positif(COD7HL) + positif(COD7HM) + positif(COD7HZ)
+ positif(COD7KE) + positif(COD7KF) + positif(COD7KG) + positif(COD7KH) + positif(COD7KI) + positif(COD7KJ) + positif(COD7KL) + positif(COD7KN)
+ positif(COD7KO) + positif(COD7KQ) + positif(COD7KR) + positif(COD7KS) + positif(COD7KT) + positif(COD7KU) + positif(COD7KV) + positif(COD7KZ)
+ positif(COD7MN) + positif(COD7PB) + positif(COD7PI) + positif(COD7PJ) + positif(COD7QE) + positif(COD7RK) + positif(COD7RL) + positif(COD7RM)
+ positif(COD7RN) + positif(COD7SS) + positif(COD7TE) + positif(COD7TF) + positif(COD7UG) + positif(COD7VQ) + positif(COD7VR) + positif(COD7WC) + positif(COD7WD)
+ positif(COD7WE) + positif(COD7WF) + positif(COD7WG) + positif(COD7WX) + positif(COD7WY) + positif(COD7WZ) + positif(COD7XH) + positif(COD7XI) + positif(COD7XJ)
+ positif(COD7XK) + positif(COD7XV) + positif(COD7YX) + positif(COD7YY) + positif(COD7YZ) + positif(COD7ZW) + positif(COD7ZX) + positif(COD7ZY) + positif(COD7ZZ)
+ positif(CODHIS) + positif(CODHIT) + positif(CODHIU) + positif(CODHIV) + positif(CODHIW) + positif(COD7QM) + positif(COD7QN) + positif(COD7QO) + positif(COD7QP)
+ positif(COD7JV) + positif(COD7JW) + positif(COD7JX) + positif(COD7JY) + positif(COD7RP) + positif(COD7RQ) + positif(COD7RI) + positif(COD7RJ) + positif(COD7UY)
+ positif(COD7UZ) + positif(COD7RX) + positif(COD7RY) + positif(COD7NM) + positif(COD7NN) + positif(COD7PF) + positif(COD7PG) + positif(COD7LG) + positif(COD7LH)
+ positif(COD7LI) + positif(COD7LJ) + positif(COD7MI) + positif(COD7MJ) + positif(COD7MK) + positif(COD7ML) + positif(COD7ZM)
+ positif(COD8SG) + positif(COD8WK) + 0
 ) ;
regle 221610:
application :  iliad ;


IND_SPR = positif(  
            somme(i=V,C,1,2,3,4: present(PRBi) + present(TSBNi) + present(FRNi))
	    + somme(j=V,C,1,2,3,4 : present(2TSNj) + present(2PRBj))
                 ) ;

regle 221620:
application : iliad  ;


INDPL = null(PLA - PLAF_CDPART) ;

regle 221630:
application : iliad  ;

INDTEFF = positif(AUTOBICVV + AUTOBICPV + AUTOBICVC + AUTOBICPC + AUTOBICVP + AUTOBICPP + AUTOBNCV + AUTOBNCC + AUTOBNCP 
                  + SALEXTV + COD1AE + COD1AH + SALEXTC + COD1BE + COD1BH + SALEXT1 + COD1CE + COD1CH + SALEXT2 + COD1DE  
                  + COD1DH + SALEXT3 + COD1EE + COD1EH + SALEXT4 + COD1FE + COD1FH)
          * (1 - positif(positif(VARIPTEFP) + positif(VARIPTEFN))) 
	  * (1 - V_CNR) ;

regle 221650:
application : iliad ;


R_QUOTIENT = TONEQUO ;

regle 221670:
application : iliad ;


TXTO = null(4-V_IND_TRAIT) * (COPETO + TXINT) 
     + null(5-V_IND_TRAIT) * (TXINR * (1-positif(TXINR_A)) + (-1) * positif(TXINR_A) * positif(TXINR) * positif(TXINR - TXINR_A)
		+ TXINR * positif(TXINR_A) * null(TXINR - TXINR_A));

regle 221680:
application : iliad  ;


TXPFI = si (V_CODPFI=03 ou V_CODPFI=30 ou V_CODPFI=55)
	alors (40)
	sinon (
	  si (V_CODPFI=04 ou V_CODPFI=05 ou V_CODPFI=32)
          alors (80)
	  sinon (
	    si (V_CODPFI=06) alors (100)
	    finsi)
          finsi)
        finsi ;

TXPFITAXA = si (V_CODPFITAXA=03 ou V_CODPFITAXA=30 ou V_CODPFITAXA=55)
            alors (40)
	    sinon (
	      si (V_CODPFITAXA=04 ou V_CODPFITAXA=05)
	      alors (80)
              sinon (
                si (V_CODPFITAXA=06) alors (100)
	        finsi)
              finsi)
            finsi ;

TXPFICAP = si (V_CODPFICAP=03 ou V_CODPFICAP=30 ou V_CODPFICAP=55)
            alors (40)
	    sinon (
	      si (V_CODPFICAP=04 ou V_CODPFICAP=05)
	      alors (80)
              sinon (
                si (V_CODPFICAP=06) alors (100)
	        finsi)
              finsi)
            finsi ;

TXPFICHR = si (V_CODPFICHR=03 ou V_CODPFICHR=30 ou V_CODPFICHR=55)
            alors (40)
	    sinon (
	      si (V_CODPFICHR=04 ou V_CODPFICHR=05 ou V_CODPFICHR=32)
	      alors (80)
              sinon (
                si (V_CODPFICHR=06) alors (100)
	        finsi)
              finsi)
            finsi ;


TXPFICRP = si (V_CODPFICRP=03 ou V_CODPFICRP=30 ou V_CODPFICRP=55)
	   alors (40)
	   sinon (
	     si (V_CODPFICRP=04 ou V_CODPFICRP=05 ou V_CODPFICRP=32)
	     alors (80)
	     sinon (
	       si (V_CODPFICRP=06) alors (100)
	       finsi)
             finsi)
           finsi ;

TXPFICVN = si (V_CODPFICVN=03 ou V_CODPFICVN=30 ou V_CODPFICVN=55) 
	    alors (40)
	    sinon (
	      si (V_CODPFICVN=04 ou V_CODPFICVN=05) alors (80)
	      sinon (
	        si (V_CODPFICVN=06) alors (100)
	        finsi)
              finsi)
	    finsi ;

TXPFICDIS = si (V_CODPFICDIS=03 ou V_CODPFICDIS=30 ou V_CODPFICDIS=55)
            alors (40)
            sinon (
	      si (V_CODPFICDIS=04 ou V_CODPFICDIS=05)
	      alors (80)
	      sinon (
		si (V_CODPFICDIS=06) alors (100)
	        finsi)
              finsi)
            finsi ;
TXPFIC820 = si (V_CODPFIC820=03 ou V_CODPFIC820=30 ou V_CODPFIC820=55)
            alors (40)
            sinon (
	      si (V_CODPFIC820=04 ou V_CODPFIC820=05)
	      alors (80)
	      sinon (
		si (V_CODPFIC820=06) alors (100)
	        finsi)
              finsi)
            finsi ;

TXPFIGLO = si (V_CODPFIGLO=03 ou V_CODPFIGLO=30 ou V_CODPFIGLO=55)
            alors (40)
            sinon (
	      si (V_CODPFIGLO=04 ou V_CODPFIGLO=05)
	      alors (80)
	      sinon (
		si (V_CODPFIGLO=06) alors (100)
	        finsi)
              finsi)
            finsi ;


TXPFIRSE1 = si (V_CODPFIRSE1=03 ou V_CODPFIRSE1=30 ou V_CODPFIRSE1=55)
            alors (40)
            sinon (
	      si (V_CODPFIRSE1=04 ou V_CODPFIRSE1=05)
	      alors (80)
	      sinon (
		si (V_CODPFIRSE1=06) alors (100)
	        finsi)
              finsi)
            finsi ;

TXPFIRSE5 = si (V_CODPFIRSE5=03 ou V_CODPFIRSE5=30 ou V_CODPFIRSE5=55)
            alors (40)
            sinon (
	      si (V_CODPFIRSE5=04 ou V_CODPFIRSE5=05)
	      alors (80)
	      sinon (
		si (V_CODPFIRSE5=06) alors (100)
	        finsi)
              finsi)
            finsi ;

TXPFIRSE2 = si (V_CODPFIRSE2=03 ou V_CODPFIRSE2=30 ou V_CODPFIRSE2=55)
            alors (40)
            sinon (
	      si (V_CODPFIRSE2=04 ou V_CODPFIRSE2=05)
	      alors (80)
	      sinon (
		si (V_CODPFIRSE2=06) alors (100)
	        finsi)
              finsi)
            finsi ;

TXPFIRSE3 = si (V_CODPFIRSE3=03 ou V_CODPFIRSE3=30 ou V_CODPFIRSE3=55)
            alors (40)
            sinon (
	      si (V_CODPFIRSE3=04 ou V_CODPFIRSE3=05)
	      alors (80)
	      sinon (
		si (V_CODPFIRSE3=06) alors (100)
	        finsi)
              finsi)
            finsi ;

TXPFIRSE4 = si (V_CODPFIRSE4=03 ou V_CODPFIRSE4=30 ou V_CODPFIRSE4=55)
            alors (40)
            sinon (
	      si (V_CODPFIRSE4=04 ou V_CODPFIRSE4=05)
	      alors (80)
	      sinon (
		si (V_CODPFIRSE4=06) alors (100)
	        finsi)
              finsi)
            finsi ;

TXPFIRSE6 = si (V_CODPFIRSE6=03 ou V_CODPFIRSE6=30 ou V_CODPFIRSE6=55)
            alors (40)
            sinon (
	      si (V_CODPFIRSE6=04 ou V_CODPFIRSE6=05)
	      alors (80)
	      sinon (
		si (V_CODPFIRSE6=06) alors (100)
	        finsi)
              finsi)
            finsi ;

TXPFIRSE8 = si (V_CODPFIRSE8=03 ou V_CODPFIRSE8=30 ou V_CODPFIRSE8=55)
            alors (40)
            sinon (
             si (V_CODPFIRSE8=04 ou V_CODPFIRSE8=05)
             alors (80)
             sinon (
               si (V_CODPFIRSE8=06) alors (100)
                 finsi)
             finsi)
           finsi ;

TXPF1728 = si (V_CODPF1728=07 ou V_CODPF1728=10 ou V_CODPF1728=17 ou V_CODPF1728=18)
	   alors (10)
	   sinon (
	     si (V_CODPF1728=08 ou V_CODPF1728=11)
	     alors (40)
	     sinon (
	       si (V_CODPF1728=31)
	       alors (80)
	       finsi)
             finsi)
           finsi ;

TXPF1728CAP = si (V_CODPF1728CAP=07 ou V_CODPF1728CAP=10 ou V_CODPF1728CAP=17 ou V_CODPF1728CAP=18)
	      alors (10)
	      sinon (
		si (V_CODPF1728CAP=08 ou V_CODPF1728CAP=11)
	        alors (40)
	        sinon (
		  si (V_CODPF1728CAP=31) 
		  alors (80)
	          finsi)
		finsi)
	      finsi ;

TXPF1728CHR = si (V_CODPF1728CHR=07 ou V_CODPF1728CHR=10 ou V_CODPF1728CHR=17 ou V_CODPF1728CHR=18)
	      alors (10)
	      sinon (
		si (V_CODPF1728CHR=08 ou V_CODPF1728CHR=11)
	        alors (40)
	        sinon (
		  si (V_CODPF1728CRP=31) 
		  alors (80)
	          finsi)
		finsi)
	      finsi ;


TXPF1728CRP = si (V_CODPF1728CRP=07 ou V_CODPF1728CRP=10 ou V_CODPF1728CRP=17 ou V_CODPF1728CRP=18)
	      alors (10)
	      sinon (
		si (V_CODPF1728CRP=08 ou V_CODPF1728CRP=11)
	        alors (40)
	        sinon (
		  si (V_CODPF1728CRP=31) 
		  alors (80)
	          finsi)
		finsi)
	      finsi ;

TXPF1728CVN = si (V_CODPF1728CVN=07 ou V_CODPF1728CVN=10 ou V_CODPF1728CVN=17 ou V_CODPF1728CVN=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728CVN=08 ou V_CODPF1728CVN=11)
	         alors (40)
                 sinon (
		   si (V_CODPF1728CVN=31) 
		   alors (80)
		   finsi)
		 finsi)
	       finsi ;

TXPF1728CDIS = si (V_CODPF1728CDIS=07 ou V_CODPF1728CDIS=10 ou V_CODPF1728CDIS=17 ou V_CODPF1728CDIS=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728CDIS=08 ou V_CODPF1728CDIS=11)
	         alors (40)
	         sinon (
		   si (V_CODPF1728CDIS=31) alors (80)
		   finsi)
		 finsi)
               finsi ;
TXPF1728C820 = si (V_CODPF1728C820=07 ou V_CODPF1728C820=10 ou V_CODPF1728C820=17 ou V_CODPF1728C820=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728C820=08 ou V_CODPF1728C820=11)
	         alors (40)
	         sinon (
		   si (V_CODPF1728C820=31) alors (80)
		   finsi)
		 finsi)
               finsi ;

TXPF1728GLO = si (V_CODPF1728GLO=07 ou V_CODPF1728GLO=10 ou V_CODPF1728GLO=17 ou V_CODPF1728GLO=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728GLO=08 ou V_CODPF1728GLO=11)
	         alors (40)
	         sinon (
		   si (V_CODPF1728GLO=31) alors (80)
		   finsi)
		 finsi)
               finsi ;

TXPF1728RSE1 = si (V_CODPF1728RSE1=07 ou V_CODPF1728RSE1=10 ou V_CODPF1728RSE1=17 ou V_CODPF1728RSE1=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728RSE1=08 ou V_CODPF1728RSE1=11)
	         alors (40)
                 sinon (
		   si (V_CODPF1728RSE1=31) 
		   alors (80)
		   finsi)
		 finsi)
	       finsi ;

TXPF1728RSE5 = si (V_CODPF1728RSE5=07 ou V_CODPF1728RSE5=10 ou V_CODPF1728RSE5=17 ou V_CODPF1728RSE5=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728RSE5=08 ou V_CODPF1728RSE5=11)
	         alors (40)
                 sinon (
		   si (V_CODPF1728RSE5=31) 
		   alors (80)
		   finsi)
		 finsi)
	       finsi ;

TXPF1728RSE2 = si (V_CODPF1728RSE2=07 ou V_CODPF1728RSE2=10 ou V_CODPF1728RSE2=17 ou V_CODPF1728RSE2=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728RSE2=08 ou V_CODPF1728RSE2=11)
	         alors (40)
                 sinon (
		   si (V_CODPF1728RSE2=31) 
		   alors (80)
		   finsi)
		 finsi)
	       finsi ;

TXPF1728RSE3 = si (V_CODPF1728RSE3=07 ou V_CODPF1728RSE3=10 ou V_CODPF1728RSE3=17 ou V_CODPF1728RSE3=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728RSE3=08 ou V_CODPF1728RSE3=11)
	         alors (40)
                 sinon (
		   si (V_CODPF1728RSE3=31) 
		   alors (80)
		   finsi)
		 finsi)
	       finsi ;

TXPF1728RSE4 = si (V_CODPF1728RSE4=07 ou V_CODPF1728RSE4=10 ou V_CODPF1728RSE4=17 ou V_CODPF1728RSE4=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728RSE4=08 ou V_CODPF1728RSE4=11)
	         alors (40)
                 sinon (
		   si (V_CODPF1728RSE4=31) 
		   alors (80)
		   finsi)
		 finsi)
	       finsi ;

TXPF1728RSE6 = si (V_CODPF1728RSE6=07 ou V_CODPF1728RSE6=10 ou V_CODPF1728RSE6=17 ou V_CODPF1728RSE6=18)
	       alors (10)
	       sinon (
		 si (V_CODPF1728RSE6=08 ou V_CODPF1728RSE6=11)
	         alors (40)
                 sinon (
		   si (V_CODPF1728RSE6=31) 
		   alors (80)
		   finsi)
		 finsi)
	       finsi ;

TXPF1728RSE8 = si (V_CODPF1728RSE8=07 ou V_CODPF1728RSE8=10 ou V_CODPF1728RSE8=17 ou V_CODPF1728RSE8=18)
               alors (10)
               sinon (
                 si (V_CODPF1728RSE8=08 ou V_CODPF1728RSE8=11)
                 alors (40)
                 sinon (
                   si (V_CODPF1728RSE8=31)
                   alors (80)
                   finsi)
                 finsi)
               finsi ;

regle 221690:
application : iliad  ;


MAJTX1 = (1 - positif(V_NBCOD1728))
	  * ((1 - positif(CMAJ)) * positif(NMAJ1+NMAJTAXA1) * TXPF1728 
	     + positif(CMAJ) * COPETO)
	 + positif(V_NBCOD1728) * (-1) ;

MAJTXPCAP1 = (1 - positif(V_NBCOD1728CAP))
	      * ((1 - positif(CMAJ)) * positif(NMAJPCAP1) * TXPF1728CAP + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728CAP) * (-1) ;

MAJTXCHR1 = (1 - positif(V_NBCOD1728CHR))
	      * ((1 - positif(CMAJ)) * positif(NMAJCHR1) * TXPF1728 + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728CHR) * (-1);



MAJTXC1 = (1 - positif(V_NBCOD1728CRP))
           * ((1 - positif(CMAJ)) * positif(NMAJC1) * TXPF1728CRP 
	      + positif(CMAJ) * COPETO)
	  + positif(V_NBCOD1728CRP) * (-1) ;

MAJTXR1 = (1 - positif(V_NBCOD1728CRP))
           * ((1 - positif(CMAJ)) * positif(NMAJR1) * TXPF1728CRP 
	      + positif(CMAJ) * COPETO)
	  + positif(V_NBCOD1728CRP) * (-1) ;

MAJTXP1 = (1 - positif(V_NBCOD1728CRP))
           * ((1 - positif(CMAJ)) * positif(NMAJPSOL1) * TXPF1728CRP 
	      + positif(CMAJ) * COPETO)
	  + positif(V_NBCOD1728CRP) * (-1) ;

MAJTXCVN1 = (1 - positif(V_NBCOD1728CVN))
	      * ((1 - positif(CMAJ)) * positif(NMAJCVN1) * TXPF1728CVN + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728CVN) * (-1) ;

MAJTXCDIS1 = (1 - positif(V_NBCOD1728CDIS))
	      * ((1 - positif(CMAJ)) * positif(NMAJCDIS1) * TXPF1728CDIS + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728CDIS) * (-1) ;
MAJTXC8201 = (1 - positif(V_NBCOD1728C820))
	      * ((1 - positif(CMAJ)) * positif(NMAJC8201) * TXPF1728C820 + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728C820) * (-1) ;

MAJTXGLO1 = (1 - positif(V_NBCOD1728GLO))
              * ((1 - positif(CMAJ)) * positif(NMAJGLO1) * TXPF1728GLO + positif(CMAJ) * COPETO)
             + positif(V_NBCOD1728GLO) * (-1) ;

MAJTXRSE11 = (1 - positif(V_NBCOD1728RSE1))
	      * ((1 - positif(CMAJ)) * positif(NMAJRSE11) * TXPF1728RSE1 + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728RSE1) * (-1) ;

MAJTXRSE51 = (1 - positif(V_NBCOD1728RSE5))
	      * ((1 - positif(CMAJ)) * positif(NMAJRSE51) * TXPF1728RSE5 + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728RSE5) * (-1) ;

MAJTXRSE21 = (1 - positif(V_NBCOD1728RSE2))
	      * ((1 - positif(CMAJ)) * positif(NMAJRSE21) * TXPF1728RSE2 + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728RSE2) * (-1) ;

MAJTXRSE31 = (1 - positif(V_NBCOD1728RSE3))
	      * ((1 - positif(CMAJ)) * positif(NMAJRSE31) * TXPF1728RSE3 + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728RSE3) * (-1) ;

MAJTXRSE41 = (1 - positif(V_NBCOD1728RSE4))
              * ((1 - positif(CMAJ)) * positif(NMAJRSE41) * TXPF1728RSE4 + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728RSE4) * (-1) ;

MAJTXRSE61 = (1 - positif(V_NBCOD1728RSE6))
              * ((1 - positif(CMAJ)) * positif(NMAJRSE61) * TXPF1728RSE6 + positif(CMAJ) * COPETO)
	     + positif(V_NBCOD1728RSE6) * (-1) ;

MAJTXRSE81 = (1 - positif(V_NBCOD1728RSE8))
              * ((1 - positif(CMAJ)) * positif(NMAJRSE81) * TXPF1728RSE8 + positif(CMAJ) * COPETO)
	                   + positif(V_NBCOD1728RSE8) * (-1) ;

MAJTX3 = positif(NMAJ3) * (10 * positif(MAJOIR10_2 + MAJOPIR10_2 + MAJOIR07 + MAJOPIR07+MAJOIR07TARDIF_D+MAJOIR02TARDIF_D
                                                    +MAJOIR07TARDIF_P+MAJOIR02TARDIF_P +MAJOIR07TARDIF_R+MAJOIR02TARDIF_R
                                                                           +MAJOIR07TARDIF_A+MAJOIR02TARDIF_A
                                         + MAJOIR02_2_NTL + MAJOIR02_2_TL + MAJOIR02_2_NTL15 + null(CMAJ - 10)+ (arr(BTO * COPETO/100) * null(CMAJ-07)))
                            + 20 * positif(MAJOIR17_2 + MAJOPIR17_2 + MAJOIR17_2TARDIF_D+ MAJOIR17_2TARDIF_P
                                                                          + MAJOIR17_2TARDIF_R+ MAJOIR17_2TARDIF_A+ null(CMAJ - 17))) ;

MAJTXTAXA3 = positif(NMAJTAXA3) * (10 * positif(MAJOTAXA10_2 + MAJOTAXA07
                                       + MAJOTAXA02_2_NTL + MAJOTAXA02_2_TL + null(CMAJ - 10)+ arr(max(0,TAXASSUR-TAXA9YI+min(0,IRN-IRANT)) * COPETO/100) * null(CMAJ-07)
                                                      + MAJOTAXA07TARDIF_D+MAJOTAXA02TARDIF_D
                                                         + MAJOTAXA07TARDIF_R+MAJOTAXA02TARDIF_R + MAJOTAXA07TARDIF_A+MAJOTAXA02TARDIF_A)
                                    + 20 * positif(MAJOTAXA17_2 + MAJOTA17_2TARDIF_D
                                                          + MAJOTA17_2TARDIF_R + MAJOTA17_2TARDIF_A + null(CMAJ - 17))) ;
MAJTXPCAP3 = positif(NMAJPCAP3) * (10 * positif(MAJOCAP10_2 + MAJOCAP07 + arr(max(0,IPCAPTAXT-CAP9YI+min(0,IRN-IRANT+TAXASSUR)) * COPETO/100) * null(CMAJ-07)
                                                          + MAJOCAP07TARDIF_D+MAJOCAP02TARDIF_D
                                                     + MAJOCAP07TARDIF_R+MAJOCAP02TARDIF_R + MAJOCAP07TARDIF_A+MAJOCAP02TARDIF_A
                                                           + MAJOCAP02_2_NTL + MAJOCAP02_2_TL + null(CMAJ - 10))
                                        + 20 * positif(MAJOCAP17_2 + MAJOCP17_2TARDIF_D
                                                      +  MAJOCP17_2TARDIF_R + MAJOCP17_2TARDIF_A + null(CMAJ - 17))) ;

MAJTXCHR3 = positif(NMAJCHR3) * (10 * positif(MAJOHR10_2 + MAJOPHR10_2 + MAJOHR07 + MAJOPHR07 + MAJOHR07TARDIF_D+MAJOHR02TARDIF_D
                                                          + MAJOHR07TARDIF_P+MAJOHR02TARDIF_P + MAJOHR07TARDIF_R+MAJOHR02TARDIF_R
                                                   + MAJOHR07TARDIF_A+MAJOHR02TARDIF_A
                                             + MAJOHR02_2_NTL + MAJOHR02_2_TL + MAJOHR02_2_TL15 + null(CMAJ - 10)
                                           + arr(max(0,IHAUTREVT-CHR9YI+CHRPVIMP+min(0,IRN-IRANT+TAXASSUR+IPCAPTAXT)) * COPETO/100) * null(CMAJ-07))
                                      + 20 * positif(MAJOHR17_2 + MAJOPHR17_2 + MAJOHR17_2TARDIF_D
                                                        + MAJOHR17_2TARDIF_P + MAJOHR17_2TARDIF_R + MAJOHR17_2TARDIF_A + null(CMAJ - 17))) ;


MAJTX4 =  positif(positif(MAJOIR03+MAJOIR55+MAJOIR30+FLAG_TRTARDIF * MAJOIR03TARDIF_D)+positif(MAJOIR04+MAJOIR35+MAJOIR05+MAJOIR32)+positif(MAJOIR06) - 1) *-1
              + positif(MAJOIR03+MAJOIR55+MAJOIR30+FLAG_TRTARDIF * MAJOIR03TARDIF_D)* (1-positif(MAJOIR04+MAJOIR35+ MAJOIR05+ MAJOIR06+MAJOIR32)) * 40
              + positif(MAJOIR04+MAJOIR35+MAJOIR05+MAJOIR32)* (1-positif(MAJOIR03+MAJOIR55+MAJOIR06+MAJOIR30+FLAG_TRTARDIF * MAJOIR03TARDIF_D)) * 80
              + positif(MAJOIR06)*(1-positif(MAJOIR03+MAJOIR04+MAJOIR35+MAJOIR05+ MAJOIR55+MAJOIR30+MAJOIR32+FLAG_TRTARDIF * MAJOIR03TARDIF_D)) * 100;

MAJTXTAXA4 =  positif(positif(MAJOTAXA03+MAJOTAXA55+ FLAG_TRTARDIF * MAJOTAXA03TARDIF_D) + positif(MAJOTAXA04+MAJOTAXA35+MAJOTAXA05) + positif(MAJOTAXA06)-1) * -1
              + positif(MAJOTAXA03+MAJOTAXA55+FLAG_TRTARDIF * MAJOTAXA03TARDIF_D)* (1-positif(MAJOTAXA04+MAJOTAXA35+ MAJOTAXA05+ MAJOTAXA06)) * 40
              + positif(MAJOTAXA04+MAJOTAXA35+MAJOTAXA05)* (1-positif(MAJOTAXA03+ MAJOTAXA55+MAJOTAXA06+FLAG_TRTARDIF * MAJOTAXA03TARDIF_D)) * 80
              + positif(MAJOTAXA06)*(1-positif(MAJOTAXA03+MAJOTAXA04+MAJOTAXA35+MAJOTAXA05+ MAJOTAXA55+FLAG_TRTARDIF * MAJOTAXA03TARDIF_D)) * 100;

MAJTXPCAP4 =  positif(positif(MAJOCAP03+MAJOCAP55+ FLAG_TRTARDIF * MAJOCAP03TARDIF_D) + positif(MAJOCAP04+MAJOCAP35+MAJOCAP05)+ positif(MAJOCAP06)- 1) * -1
              + positif(MAJOCAP03+MAJOCAP55+ FLAG_TRTARDIF * MAJOCAP03TARDIF_D)* (1-positif(MAJOCAP04+MAJOCAP35+ MAJOCAP05+MAJOCAP06)) * 40
              + positif(MAJOCAP04+MAJOCAP35+MAJOCAP05)* (1-positif(MAJOCAP03+MAJOCAP55+MAJOCAP06+ FLAG_TRTARDIF * MAJOCAP03TARDIF_D)) * 80
              + positif(MAJOCAP06)*(1-positif(MAJOCAP03+MAJOCAP04+MAJOCAP35+ MAJOCAP05+ MAJOCAP55+ FLAG_TRTARDIF * MAJOCAP03TARDIF_D)) * 100;

MAJTXCHR4 =  positif(positif(MAJOHR03+MAJOHR30+MAJOHR55+ FLAG_TRTARDIF * MAJOHR03TARDIF_D) + positif(MAJOHR04+MAJOHR35+MAJOHR05+MAJOHR32)+ positif(MAJOHR06)-1) * -1
	    + positif(MAJOHR03+MAJOHR30+MAJOHR55+ FLAG_TRTARDIF * MAJOHR03TARDIF_D)* (1-positif(MAJOHR04+MAJOHR35+ MAJOHR05+ MAJOHR06+MAJOHR32)) * 40
	    + positif(MAJOHR04+MAJOHR35+MAJOHR05+MAJOHR32)* (1-positif(MAJOHR03+ MAJOHR30+ MAJOHR55+MAJOHR06+ FLAG_TRTARDIF * MAJOHR03TARDIF_D)) * 80
	    + positif(MAJOHR06)*(1-positif(MAJOHR03+MAJOHR04+MAJOHR35+ MAJOHR05+ MAJOHR30+MAJOHR32+MAJOHR55+ FLAG_TRTARDIF * MAJOHR03TARDIF_D)) * 100;
MAJTXC4 =  positif(positif(MAJOCS03+MAJOCS55+MAJOCS30+ FLAG_TRTARDIF * MAJOCS03TARDIF_D) + positif(MAJOCS04+MAJOCS35+MAJOCS05+MAJOCS32)+ positif(MAJOCS06)-1) * -1
          + positif(MAJOCS03+MAJOCS55+MAJOCS30+ FLAG_TRTARDIF * MAJOCS03TARDIF_D)* (1-positif(MAJOCS04+MAJOCS35+ MAJOCS05+ MAJOCS06+MAJOCS32)) * 40
          + positif(MAJOCS04+MAJOCS35+MAJOCS05+MAJOCS32)* (1-positif(MAJOCS03+ MAJOCS55+MAJOCS06+MAJOCS30+ FLAG_TRTARDIF * MAJOCS03TARDIF_D)) * 80
          + positif(MAJOCS06)*(1-positif(MAJOCS03+MAJOCS04+MAJOCS35+ MAJOCS05+MAJOCS55+ FLAG_TRTARDIF * MAJOCS03TARDIF_D)) * 100;


MAJTXR4 = positif(positif(MAJORD03+MAJORD55+MAJORD30+ FLAG_TRTARDIF * MAJORD03TARDIF_D) + positif(MAJORD04+MAJORD35+MAJORD05+MAJORD32)+ positif(MAJORD06)-1) * -1
            + positif(MAJORD03+MAJORD55+MAJORD30+ FLAG_TRTARDIF * MAJORD03TARDIF_D)* (1-positif(MAJORD04+MAJORD35+ MAJORD05+ MAJORD06+MAJORD32)) * 40
            + positif(MAJORD04+MAJORD35+MAJORD05+MAJORD32)* (1-positif(MAJORD03+ MAJORD55+MAJORD06+MAJORD30+ FLAG_TRTARDIF * MAJORD03TARDIF_D)) * 80
            + positif(MAJORD06)*(1-positif(MAJORD03+MAJORD04+MAJORD35+ MAJORD05+MAJORD55+ FLAG_TRTARDIF * MAJORD03TARDIF_D)) * 100;


MAJTXP4 = positif(positif(MAJOPSOL03+MAJOPSOL55+MAJOPSOL30+FLAG_TRTARDIF * MAJOPSOL03TARDIF_D)
                                          + positif(MAJOPSOL04+MAJOPSOL35+MAJOPSOL05+MAJOPSOL32)+ positif(MAJOPSOL06)-1) * -1
              + positif(MAJOPSOL03+MAJOPSOL55+MAJOPSOL30+FLAG_TRTARDIF * (MAJOPSOL03TARDIF_D))
                                                             * (1-positif(MAJOPSOL04+MAJOPSOL35+ MAJOPSOL05+ MAJOPSOL06+MAJOPSOL32)) * 40
              + positif(MAJOPSOL04+MAJOPSOL35+MAJOPSOL05+MAJOPSOL32)
                                              * (1-positif(MAJOPSOL03+ MAJOPSOL55+MAJOPSOL06+MAJOPSOL30+FLAG_TRTARDIF * MAJOPSOL03TARDIF_D)) * 80
              + positif(MAJOPSOL06)*(1-positif(MAJOPSOL03+MAJOPSOL04+MAJOPSOL35+ MAJOPSOL05+MAJOPSOL55+FLAG_TRTARDIF * MAJOPSOL03TARDIF_D)) * 100;

MAJTXCVN4 =  positif(positif(MAJOCVN03+MAJOCVN55+ FLAG_TRTARDIF * MAJOCVN03TARDIF_D) + positif(MAJOCVN04+MAJOCVN35+MAJOCVN05)+ positif(MAJOCVN06)-1) * -1
	    + positif(MAJOCVN03+MAJOCVN55+ FLAG_TRTARDIF * MAJOCVN03TARDIF_D)* (1-positif(MAJOCVN04+MAJOCVN35+ MAJOCVN05+ MAJOCVN06)) * 40
	    + positif(MAJOCVN04+MAJOCVN35+MAJOCVN05)* (1-positif(MAJOCVN03 + MAJOCVN55+MAJOCVN06+ FLAG_TRTARDIF * MAJOCVN03TARDIF_D)) * 80
	    + positif(MAJOCVN06)*(1-positif(MAJOCVN03+MAJOCVN04+MAJOCVN35+ MAJOCVN05+MAJOCVN55+ FLAG_TRTARDIF * MAJOCVN03TARDIF_D)) * 100;
MAJTXCDIS4 =  positif(positif(MAJOCDIS03+MAJOCDIS55+ FLAG_TRTARDIF * MAJOCDIS03TARDIF_D) + positif(MAJOCDIS04+MAJOCDIS35+MAJOCDIS05)+ positif(MAJOCDIS06)-1) * -1
            + positif(MAJOCDIS03+MAJOCDIS55+ FLAG_TRTARDIF * MAJOCDIS03TARDIF_D)* (1-positif(MAJOCDIS04+MAJOCDIS35+ MAJOCDIS05+ MAJOCDIS06)) * 40
            + positif(MAJOCDIS04+MAJOCDIS35+MAJOCDIS05)* (1-positif(MAJOCDIS03 + MAJOCDIS55+MAJOCDIS06+ FLAG_TRTARDIF * MAJOCDIS03TARDIF_D)) * 80
            + positif(MAJOCDIS06)*(1-positif(MAJOCDIS03+MAJOCDIS04+MAJOCDIS35+MAJOCDIS05+MAJOCDIS55+ FLAG_TRTARDIF * MAJOCDIS03TARDIF_D)) * 100;

MAJTXC8204 =  positif(positif(MAJOC82003+MAJOC82055+ FLAG_TRTARDIF * MAJOC82003TARDIF_D) + positif(MAJOC82004+MAJOC82035+MAJOC82005)+ positif(MAJOC82006)-1) * -1
            + positif(MAJOC82003+MAJOC82055+ FLAG_TRTARDIF * MAJOC82003TARDIF_D)* (1-positif(MAJOC82004+MAJOC82035+ MAJOC82005+ MAJOC82006)) * 40
            + positif(MAJOC82004+MAJOC82035+MAJOC82005)* (1-positif(MAJOC82003 + MAJOC82055+MAJOC82006+ FLAG_TRTARDIF * MAJOC82003TARDIF_D)) * 80
            + positif(MAJOC82006)*(1-positif(MAJOC82003+MAJOC82004+MAJOC82035+ MAJOC82005+MAJOC82055+ FLAG_TRTARDIF * MAJOC82003TARDIF_D)) * 100;


MAJTXGLO4 =  positif(positif(MAJOGLO03+MAJOGLO55+ FLAG_TRTARDIF * MAJOGLO03TARDIF_D) + positif(MAJOGLO04+MAJOGLO35+MAJOGLO05)+ positif(MAJOGLO06)-1) * -1
	    + positif(MAJOGLO03+MAJOGLO55+ FLAG_TRTARDIF * MAJOGLO03TARDIF_D)* (1-positif(MAJOGLO04+MAJOGLO35+ MAJOGLO05+ MAJOGLO06)) * 40
	    + positif(MAJOGLO04+MAJOGLO35+MAJOGLO05)* (1-positif(MAJOGLO03+ MAJOGLO55+MAJOGLO06+ FLAG_TRTARDIF * MAJOGLO03TARDIF_D)) * 80
	    + positif(MAJOGLO06)*(1-positif(MAJOGLO03+MAJOGLO04+MAJOGLO35+ MAJOGLO05+MAJOGLO55+ FLAG_TRTARDIF * MAJOGLO03TARDIF_D)) * 100;

MAJTXRSE14 =  positif(positif(MAJORSE103+MAJORSE155+ FLAG_TRTARDIF * MAJORSE103TARDIF_D) + positif(MAJORSE104+MAJORSE135+MAJORSE105)+ positif(MAJORSE106)-1) * -1
              + positif(MAJORSE103+MAJORSE155+ FLAG_TRTARDIF * MAJORSE103TARDIF_D)* (1-positif(MAJORSE104+MAJORSE135+ MAJORSE105+ MAJORSE106)) * 40
              + positif(MAJORSE104+MAJORSE135+MAJORSE105)* (1-positif(MAJORSE103+ MAJORSE155+MAJORSE106+ FLAG_TRTARDIF * MAJORSE103TARDIF_D)) * 80
              + positif(MAJORSE106)*(1-positif(MAJORSE103+MAJORSE104+MAJORSE135+ MAJORSE105+MAJORSE155+ FLAG_TRTARDIF * MAJORSE103TARDIF_D)) * 100;

MAJTXRSE54 =  positif(positif(MAJORSE503+MAJORSE555+ FLAG_TRTARDIF * MAJORSE203TARDIF_D) + positif(MAJORSE504+MAJORSE535+MAJORSE505)+ positif(MAJORSE506)-1) * -1
	    + positif(MAJORSE503+MAJORSE555+ FLAG_TRTARDIF * MAJORSE203TARDIF_D)* (1-positif(MAJORSE504+MAJORSE535+ MAJORSE505+ MAJORSE506)) * 40
	    + positif(MAJORSE504+MAJORSE535+MAJORSE505)* (1-positif(MAJORSE503+ MAJORSE555+MAJORSE506+ FLAG_TRTARDIF * MAJORSE203TARDIF_D)) * 80
	    + positif(MAJORSE506)*(1-positif(MAJORSE503+MAJORSE504+MAJORSE535+ MAJORSE505+MAJORSE555+ FLAG_TRTARDIF * MAJORSE203TARDIF_D)) * 100;
MAJTXRSE24 =  positif(positif(MAJORSE203+MAJORSE255+ FLAG_TRTARDIF * MAJORSE303TARDIF_D) + positif(MAJORSE204+MAJORSE235+MAJORSE205)+ positif(MAJORSE206)-1) * -1
            + positif(MAJORSE203+MAJORSE555+ FLAG_TRTARDIF * MAJORSE303TARDIF_D)* (1-positif(MAJORSE204+MAJORSE235+ MAJORSE205+ MAJORSE206)) * 40
            + positif(MAJORSE204+MAJORSE235+MAJORSE205)* (1-positif(MAJORSE203+ MAJORSE255+MAJORSE206+ FLAG_TRTARDIF * MAJORSE303TARDIF_D)) * 80
            + positif(MAJORSE206)*(1-positif(MAJORSE203+MAJORSE204+MAJORSE235+ MAJORSE205+MAJORSE255+ FLAG_TRTARDIF * MAJORSE303TARDIF_D)) * 100;

MAJTXRSE34 =  positif(positif(MAJORSE303+MAJORSE355+ FLAG_TRTARDIF * MAJORSE403TARDIF_D) + positif(MAJORSE304+MAJORSE335+MAJORSE305)+ positif(MAJORSE306)-1) * -1
            + positif(MAJORSE303+MAJORSE355+ FLAG_TRTARDIF * MAJORSE403TARDIF_D)* (1-positif(MAJORSE304+MAJORSE335+ MAJORSE305+ MAJORSE306)) * 40
            + positif(MAJORSE304+MAJORSE335+MAJORSE305)* (1-positif(MAJORSE303+ MAJORSE355+MAJORSE306+ FLAG_TRTARDIF * MAJORSE403TARDIF_D)) * 80
            + positif(MAJORSE306)*(1-positif(MAJORSE303+MAJORSE304+MAJORSE335+ MAJORSE305+MAJORSE355+ FLAG_TRTARDIF * MAJORSE403TARDIF_D)) * 100;

MAJTXRSE44 =  positif(positif(MAJORSE403+MAJORSE455+ FLAG_TRTARDIF * MAJORSE503TARDIF_D) + positif(MAJORSE404+MAJORSE435+MAJORSE405)+ positif(MAJORSE406)-1) * -1
            + positif(MAJORSE403+MAJORSE455+ FLAG_TRTARDIF * MAJORSE503TARDIF_D)* (1-positif(MAJORSE404+MAJORSE435+ MAJORSE405+ MAJORSE406)) * 40
            + positif(MAJORSE404+MAJORSE435+MAJORSE405)* (1-positif(MAJORSE403+ MAJORSE455+MAJORSE406+ FLAG_TRTARDIF * MAJORSE503TARDIF_D)) * 80
            + positif(MAJORSE406)*(1-positif(MAJORSE403+MAJORSE404+MAJORSE435+ MAJORSE405+MAJORSE455+ FLAG_TRTARDIF * MAJORSE503TARDIF_D)) * 100;

MAJTXRSE64 =  positif(positif(MAJORSE603+MAJORSE655+ FLAG_TRTARDIF * MAJORSE603TARDIF_D) + positif(MAJORSE604+MAJORSE635+MAJORSE605)+ positif(MAJORSE606)-1) * -1
            + positif(MAJORSE603+MAJORSE655+ FLAG_TRTARDIF * MAJORSE603TARDIF_D)* (1-positif(MAJORSE604+MAJORSE635+ MAJORSE605+ MAJORSE606)) * 40
            + positif(MAJORSE604+MAJORSE635+MAJORSE605)* (1-positif(MAJORSE603+ MAJORSE655+MAJORSE606+ FLAG_TRTARDIF * MAJORSE603TARDIF_D)) * 80
            + positif(MAJORSE606)*(1-positif(MAJORSE603+MAJORSE604+MAJORSE635+ MAJORSE605+MAJORSE655+ FLAG_TRTARDIF * MAJORSE603TARDIF_D)) * 100;

MAJTXRSE84 =  positif(positif(MAJORSE803+MAJORSE855+ FLAG_TRTARDIF * MAJORSE803TARDIF_D) + positif(MAJORSE804+MAJORSE835+MAJORSE805)+ positif(MAJORSE806)-1) * -1
            + positif(MAJORSE803+MAJORSE855+ FLAG_TRTARDIF * MAJORSE803TARDIF_D)* (1-positif(MAJORSE804+MAJORSE835+ MAJORSE805+ MAJORSE806)) * 40
            + positif(MAJORSE804+MAJORSE835+MAJORSE805)* (1-positif(MAJORSE803+ MAJORSE855+MAJORSE806+ FLAG_TRTARDIF * MAJORSE803TARDIF_D)) * 80
            + positif(MAJORSE806)*(1-positif(MAJORSE803+MAJORSE804+MAJORSE835+ MAJORSE805+MAJORSE855+ FLAG_TRTARDIF * MAJORSE803TARDIF_D)) * 100;

regle 221700:
application : iliad ;

RETX = positif(CMAJ) * TXINT
       + (1-positif(DOTOT))* (
                TXINR * positif(INRIR_NET+INRCSG_NET+INRRDS_NET+INRPSOL_NET+INRCDIS_NET+INRC820_NET +INRGLO_NET+INRCHR_NET+INRTAXA_NET
                       +INRRSE1_NET+INRRSE2_NET+INRRSE3_NET+INRRSE4_NET+INRRSE5_NET+INRRSE6_NET+INRRSE8_NET)
                         *
                              (1-positif(INRIR_NET_1+INRCSG_NET_1+INRRDS_NET_1+INRPSOL_NET_1+INRCDIS_NET_1+INRC820_NET_1 +INRGLO_NET_1+INRCHR_NET_1+INRTAXA_NET_1
                                    +INRRSE1_NET_1+INRRSE2_NET_1+INRRSE3_NET_1+INRRSE4_NET_1+INRRSE5_NET_1+INRRSE6_NET_1+ INRRSE8_NET_1+INRTOT_NETADEF+INRTOT_NET1ADEF))
	        + TINR_1 * positif(INRIR_NET_1+INRCSG_NET_1+INRRDS_NET_1+INRPSOL_NET_1+INRCDIS_NET_1+INRC820_NET_1 +INRGLO_NET_1+INRCHR_NET_1+INRTAXA_NET_1
			                         +INRRSE1_NET_1+INRRSE2_NET_1+INRRSE3_NET_1+INRRSE4_NET_1+INRRSE5_NET_1+INRRSE6_NET_1+INRRSE8_NET_1)
                          *
                             (1-positif(INRIR_NET+INRCSG_NET+INRRDS_NET+INRPSOL_NET+INRCDIS_NET+INRC820_NET +INRGLO_NET+INRCHR_NET+INRTAXA_NET
                                        +INRRSE1_NET+INRRSE2_NET+INRRSE3_NET+INRRSE4_NET+INRRSE5_NET+INRRSE6_NET+ INRRSE8_NET+INRTOT_NETADEF+INRTOT_NET1ADEF))
                + (-1) * positif(INRIR_NET_1+INRCSG_NET_1+INRRDS_NET_1+INRPSOL_NET_1+INRCDIS_NET_1+INRC820_NET_1 +INRGLO_NET_1+INRCHR_NET_1+INRTAXA_NET_1
	                          +INRRSE1_NET_1+INRRSE2_NET_1+INRRSE3_NET_1+INRRSE4_NET_1+INRRSE5_NET_1+INRRSE6_NET_1+ INRRSE8_NET_1+INRTOT_NET1ADEF+INRTOT_NETADEF)
                      *
                                  positif(INRIR_NET+INRCSG_NET+INRRDS_NET+INRPSOL_NET+INRCDIS_NET+INRC820_NET +INRGLO_NET+INRCHR_NET+INRTAXA_NET
                                      +INRRSE1_NET+INRRSE2_NET+INRRSE3_NET+INRRSE4_NET+INRRSE5_NET+INRRSE6_NET+ INRRSE8_NET+INRTOT_NETADEF+INRTOT_NET1ADEF)
                                   )
             + positif(DOTOT)* (
                 (-1) * positif(INRIR_NET1ADEF+INRCSG_NET1ADEF+INRRDS_NET1ADEF+INRPSOL_NET1ADEF+INRCDIS_NET1ADEF+INRC820_NET1ADEF 
		                                    +INRGLO_NET1ADEF+INRCHR_NET1ADEF+INRTAXA_NET1ADEF
                                  +INRRSE1_NET1ADEF+INRRSE2_NET1ADEF+INRRSE3_NET1ADEF+INRRSE4_NET1ADEF+INRRSE5_NET1ADEF+INRRSE6_NET1ADEF+INRRSE8_NET1ADEF)
                    * positif(INRIR_NETADEF+INRCSG_NETADEF+INRRDS_NETADEF+INRPSOL_NETADEF+INRCDIS_NETADEF+INRC820_NETADEF +INRGLO_NETADEF+INRCHR_NETADEF+INRTAXA_NETADEF
                            +INRRSE1_NETADEF+INRRSE2_NETADEF+INRRSE3_NETADEF+INRRSE4_NETADEF+INRRSE5_NETADEF+INRRSE6_NETADEF+INRRSE8_NETADEF)
             + TINR_1_A * positif(INRIR_NET1ADEF+INRCSG_NET1ADEF+INRRDS_NET1ADEF+INRPSOL_NET1ADEF+INRCDIS_NET1ADEF+INRC820_NET1ADEF 
                                                                       +INRGLO_NET1ADEF+INRCHR_NET1ADEF+INRTAXA_NET1ADEF
                               +INRRSE1_NET1ADEF+INRRSE2_NET1ADEF+INRRSE3_NET1ADEF+INRRSE4_NET1ADEF+INRRSE5_NET1ADEF+INRRSE6_NET1ADEF+INRRSE8_NET1ADEF)
                          * (1-positif(INRIR_NETADEF+INRCSG_NETADEF+INRRDS_NETADEF+INRPSOL_NETADEF+INRCDIS_NETADEF+INRC820_NETADEF 
			                                            +INRGLO_NETADEF+INRCHR_NETADEF+INRTAXA_NETADEF
	                                     +INRRSE1_NETADEF+INRRSE2_NETADEF+INRRSE3_NETADEF+INRRSE4_NETADEF+INRRSE5_NETADEF+INRRSE6_NETADEF+INRRSE8_NETADEF))
             + TINR_A * (1-positif(INRIR_NET1ADEF+INRCSG_NET1ADEF+INRRDS_NET1ADEF+INRPSOL_NET1ADEF+INRCDIS_NET1ADEF+INRC820_NET1ADEF 
                                                         +INRGLO_NET1ADEF+INRCHR_NET1ADEF+INRTAXA_NET1ADEF
                            +INRRSE1_NET1ADEF+INRRSE2_NET1ADEF+INRRSE3_NET1ADEF+INRRSE4_NET1ADEF+INRRSE5_NET1ADEF+INRRSE6_NET1ADEF+INRRSE8_NET1ADEF))
                    * positif(INRIR_NETADEF+INRCSG_NETADEF+INRRDS_NETADEF+INRPSOL_NETADEF+INRCDIS_NETADEF+INRC820_NETADEF +INRGLO_NETADEF+INRCHR_NETADEF+INRTAXA_NETADEF
                                      +INRRSE1_NETADEF+INRRSE2_NETADEF+INRRSE3_NETADEF+INRRSE4_NETADEF+INRRSE5_NETADEF+INRRSE6_NETADEF+INRRSE8_NETADEF)
                              )
                                ;
TXPFC = si (V_CODPFC=01 ou V_CODPFC=02) alors (0)
        sinon (
	  si (V_CODPFC=07 ou V_CODPFC=10 ou V_CODPFC=17 ou V_CODPFC=18) alors (10)
	  sinon (
	    si (V_CODPFC=03 ou V_CODPFC=08 ou V_CODPFC=11 ou V_CODPFC=30) alors (40)
	    sinon (
	      si (V_CODPFC=04 ou V_CODPFC=35 ou V_CODPFC=05 ou V_CODPFC=09 ou V_CODPFC=12 ou V_CODPFC=31) alors (80)
	      sinon (
                si (V_CODPFI=06) alors (100)
                finsi)
	      finsi)
            finsi)
	  finsi)
        finsi ;
TXPFR = si (V_CODPFR=01 ou V_CODPFR=02) alors (0)
        sinon (
	  si (V_CODPFR=07 ou V_CODPFR=10 ou V_CODPFR=17 ou V_CODPFR=18) alors (10)
	  sinon (
	    si (V_CODPFR=03 ou V_CODPFR=08 ou V_CODPFR=11 ou V_CODPFR=30) alors (40)
	    sinon (
	      si (V_CODPFR=04 ou V_CODPFR=35 ou V_CODPFR=05 ou V_CODPFR=09 ou V_CODPFR=12 ou V_CODPFR=31) alors (80)
	      sinon (
	        si (V_CODPFI=06) alors (100)
	      finsi)
	    finsi)
	  finsi)
	finsi)
      finsi ;
TXPFP = si (V_CODPFP=01 ou V_CODPFP=02) alors (0)
        sinon (
          si (V_CODPFP=07 ou V_CODPFP=10 ou V_CODPFP=17 ou V_CODPFP=18) alors (10)
	  sinon (
	    si (V_CODPFP=03 ou V_CODPFP=08 ou V_CODPFP=11 ou V_CODPFP=30) alors (40)
	    sinon (
	      si (V_CODPFP=04 ou V_CODPFP=35 ou V_CODPFP=05 ou V_CODPFP=09 ou V_CODPFP=12 ou V_CODPFP=31) alors (80)
	      sinon (
	        si (V_CODPFI=06) alors (100)
	      finsi)
	    finsi)
	  finsi)
	finsi)
      finsi ;

TXPFCDIS = si (V_CODPFCDIS=01 ou V_CODPFCDIS=02) alors (0)
        sinon (
          si (V_CODPFCDIS=07 ou V_CODPFCDIS=10 ou V_CODPFCDIS=17 ou V_CODPFCDIS=18) alors (10)
	  sinon (
	    si (V_CODPFCDIS=03 ou V_CODPFCDIS=08 ou V_CODPFCDIS=11) alors (40)
	    sinon (
	      si (V_CODPFCDIS=04 ou V_CODPFCDIS=35 ou V_CODPFCDIS=05 ou V_CODPFCDIS=12) alors (80)
	      sinon (
	        si (V_CODPFICDIS=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;
TXPFC820 = si (V_CODPFC820=01 ou V_CODPFC820=02) alors (0)
        sinon (
          si (V_CODPFC820=07 ou V_CODPFC820=10 ou V_CODPFC820=17 ou V_CODPFC820=18) alors (10)
	  sinon (
	    si (V_CODPFC820=03 ou V_CODPFC820=08 ou V_CODPFC820=11) alors (40)
	    sinon (
	      si (V_CODPFC820=04 ou V_CODPFC820=35 ou V_CODPFC820=05 ou V_CODPFC820=12) alors (80)
	      sinon (
	        si (V_CODPFIC820=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;

TXPFCVN = si (V_CODPFCVN=01 ou V_CODPFCVN=02) alors (0) 
        sinon (
          si (V_CODPFCVN=07 ou V_CODPFCVN=10 ou V_CODPFCVN=17 ou V_CODPFCVN=18) alors (10)
	  sinon (
	    si (V_CODPFCVN=03 ou V_CODPFCVN=08 ou V_CODPFCVN=11) alors (40)
	    sinon (
	      si (V_CODPFCVN=04 ou V_CODPFCVN=35 ou V_CODPFCVN=05 ou V_CODPFCVN=12 ) alors (80)
	      sinon (
	        si (V_CODPFICVN=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;

TXPFGLO = si (V_CODPFGLO=01 ou V_CODPFGLO=02) alors (0) 
        sinon (
          si (V_CODPFGLO=07 ou V_CODPFGLO=10 ou V_CODPFGLO=17 ou V_CODPFGLO=18) alors (10)
	  sinon (
	    si (V_CODPFGLO=03 ou V_CODPFGLO=08 ou V_CODPFGLO=11) alors (40)
	    sinon (
	      si (V_CODPFGLO=04 ou V_CODPFGLO=35 ou V_CODPFGLO=05 ou V_CODPFGLO=12 ) alors (80)
	      sinon (
	        si (V_CODPFIGLO=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;

TXPFRSE5 = si (V_CODPFRSE5=01 ou V_CODPFRSE5=02) alors (0)
        sinon (
          si (V_CODPFRSE5=07 ou V_CODPFRSE5=10 ou V_CODPFRSE5=17 ou V_CODPFRSE5=18) alors (10)
	  sinon (
	    si (V_CODPFRSE5=03 ou V_CODPFRSE5=08 ou V_CODPFRSE5=11) alors (40)
	    sinon (
	      si (V_CODPFRSE5=04 ou V_CODPFRSE5=35 ou V_CODPFRSE5=05 ou V_CODPFRSE5=12 ) alors (80)
	      sinon (
	        si (V_CODPFIRSE5=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;

TXPFRSE1 = si (V_CODPFRSE1=01 ou V_CODPFRSE1=02) alors (0)
        sinon (
          si (V_CODPFRSE1=07 ou V_CODPFRSE1=10 ou V_CODPFRSE1=17 ou V_CODPFRSE1=18) alors (10)
	  sinon (
	    si (V_CODPFRSE1=03 ou V_CODPFRSE1=08 ou V_CODPFRSE1=11) alors (40)
	    sinon (
	      si (V_CODPFRSE1=04 ou V_CODPFRSE1=35 ou V_CODPFRSE1=05 ou V_CODPFRSE1=12 ) alors (80)
	      sinon (
	        si (V_CODPFIRSE1=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;

TXPFRSE2 = si (V_CODPFRSE2=01 ou V_CODPFRSE2=02) alors (0)
        sinon (
          si (V_CODPFRSE2=07 ou V_CODPFRSE2=10 ou V_CODPFRSE2=17 ou V_CODPFRSE2=18) alors (10)
	  sinon (
	    si (V_CODPFRSE2=03 ou V_CODPFRSE2=08 ou V_CODPFRSE2=11) alors (40)
	    sinon (
	      si (V_CODPFRSE2=04 ou V_CODPFRSE2=35 ou V_CODPFRSE2=05 ou V_CODPFRSE2=12 ) alors (80)
	      sinon (
	        si (V_CODPFIRSE2=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;

TXPFRSE3 = si (V_CODPFRSE3=01 ou V_CODPFRSE3=02) alors (0)
        sinon (
          si (V_CODPFRSE3=07 ou V_CODPFRSE3=10 ou V_CODPFRSE3=17 ou V_CODPFRSE3=18) alors (10)
	  sinon (
	    si (V_CODPFRSE3=03 ou V_CODPFRSE3=08 ou V_CODPFRSE3=11) alors (40)
	    sinon (
	      si (V_CODPFRSE3=04 ou V_CODPFRSE3=35 ou V_CODPFRSE3=05 ou V_CODPFRSE3=12 ) alors (80)
	      sinon (
	        si (V_CODPFIRSE3=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;

TXPFRSE4 = si (V_CODPFRSE4=01 ou V_CODPFRSE4=02) alors (0)
        sinon (
          si (V_CODPFRSE4=07 ou V_CODPFRSE4=10 ou V_CODPFRSE4=17 ou V_CODPFRSE4=18) alors (10)
	  sinon (
	    si (V_CODPFRSE4=03 ou V_CODPFRSE4=08 ou V_CODPFRSE4=11) alors (40)
	    sinon (
	      si (V_CODPFRSE4=04 ou V_CODPFRSE4=35 ou V_CODPFRSE4=05 ou V_CODPFRSE4=12 ) alors (80)
	      sinon (
	        si (V_CODPFIRSE4=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;

TXPFRSE6 = si (V_CODPFRSE6=01 ou V_CODPFRSE6=02) alors (0)
        sinon (
          si (V_CODPFRSE6=07 ou V_CODPFRSE6=10 ou V_CODPFRSE6=17 ou V_CODPFRSE6=18) alors (10)
	  sinon (
	    si (V_CODPFRSE6=03 ou V_CODPFRSE6=08 ou V_CODPFRSE6=11) alors (40)
	    sinon (
	      si (V_CODPFRSE6=04 ou V_CODPFRSE6=35 ou V_CODPFRSE6=05 ou V_CODPFRSE6=12 ) alors (80)
	      sinon (
	        si (V_CODPFIRSE6=06) alors (100)
	        finsi)
	      finsi)
            finsi)
          finsi)
        finsi ;

TXPFRSE8 = si (V_CODPFRSE8=01 ou V_CODPFRSE8=02) alors (0)
        sinon (
          si (V_CODPFRSE8=07 ou V_CODPFRSE8=10 ou V_CODPFRSE8=17 ou V_CODPFRSE8=18) alors (10)
          sinon (
            si (V_CODPFRSE8=03 ou V_CODPFRSE8=08 ou V_CODPFRSE8=11) alors (40)
            sinon (
              si (V_CODPFRSE8=04 ou V_CODPFRSE8=35 ou V_CODPFRSE8=05 ou V_CODPFRSE8=12 ) alors (80)
              sinon (
                si (V_CODPFIRSE8=06) alors (100)
                finsi)
              finsi)
            finsi)
          finsi)
        finsi ;

NATMAJI = present(CMAJ) +
	 si (V_CODPFI =01 ou V_CODPFI=22 ou V_CODPFI=24) alors (1) sinon (
	   si (V_CODPFI =02) alors (2)
	   sinon (
	     si (V_CODPFI=03  ou V_CODPFI=04 ou V_CODPFI=35 ou V_CODPFI=05 ou V_CODPFI=06
	         ou V_CODPFI=30 ou V_CODPFI=32 ou V_CODPFI=55) 
	     alors (4)
             sinon (
               si (V_CODPF1728=07 ou V_CODPF1728=08 ou V_CODPF1728=10 ou V_CODPF1728=11 
                   ou V_CODPF1728=17 ou V_CODPF1728=18 ou V_CODPF1728=31) 
	       alors (1)
	       finsi)
             finsi)
           finsi)
         finsi ;
NATMAJ = NATMAJI * (1 - positif(V_NBCODI+V_NBCOD1728)) + 9 * positif(V_NBCODI+V_NBCOD1728) ;
NATMAJCI = present(CMAJ) +
           si (V_CODPFC=01 ou V_CODPFC=22 ou V_CODPFC=24) alors (1) sinon (
	     si (V_CODPFC=02) alors (2)
	     sinon (
	       si (V_CODPFC=03  ou V_CODPFC=04 ou V_CODPFC=35 ou V_CODPFC=05 ou V_CODPFC=06
		   ou V_CODPFC=22 ou V_CODPFC=32 ou V_CODPFC=30 ou V_CODPFC=55) 
	       alors (4)
	       sinon (
                 si (V_CODPFC=07 ou V_CODPFC=08 ou V_CODPFC=09 ou V_CODPFC=10 ou V_CODPFC=11
		     ou V_CODPFC=12 ou V_CODPFC=17 ou V_CODPFC=18 ou V_CODPFC=31) 
		 alors (1)
	         finsi)
	       finsi)
	     finsi)
           finsi ;
NATMAJC = NATMAJCI * (1 - positif(V_NBCODC)) + 9 * positif(V_NBCODC) ;
NATMAJRI = present(CMAJ) +
	   si (V_CODPFR=01 ou V_CODPFR=22 ou V_CODPFR=24) alors (1) sinon (
	     si (V_CODPFR=02) alors (2)
	     sinon (
	       si (V_CODPFR=03  ou V_CODPFR=04 ou V_CODPFR=35 ou V_CODPFR=05 ou V_CODPFR=06
	           ou V_CODPFR=22 ou V_CODPFR=32 ou V_CODPFR=30 ou V_CODPFR=55) 
	       alors (4)
	       sinon (
	         si (V_CODPFR=07 ou V_CODPFR=08 ou V_CODPFR=09 ou V_CODPFR=10 ou V_CODPFR=11
	             ou V_CODPFR=12 ou V_CODPFR=17 ou V_CODPFR=18 ou V_CODPFR=31) 
	         alors (1)
	         finsi)
               finsi)
             finsi)
           finsi ;
NATMAJR = (NATMAJRI * (1 - positif(V_NBCODR)) + 9 * positif(V_NBCODR)) * positif(RETRD) ;
NATMAJPI = present(CMAJ) +
	   si (V_CODPFP=01 ou V_CODPFP=22 ou V_CODPFP=24) alors (1) sinon (
	     si (V_CODPFP=02) alors (2)
	     sinon (
	       si (V_CODPFP=03 ou V_CODPFP=04 ou V_CODPFP=35 ou V_CODPFP=05 ou V_CODPFP=06
	           ou V_CODPFP=22 ou V_CODPFP=32 ou V_CODPFP=30 ou V_CODPFP=55) 
	       alors (4)
	       sinon (
	         si (V_CODPFP=07 ou V_CODPFP=08 ou V_CODPFP=09 ou V_CODPFP=10 ou V_CODPFP=11 
		     ou V_CODPFP=12 ou V_CODPFP=17  ou V_CODPFP=18 ou V_CODPFP=31) 
		 alors (1)
	         finsi)
               finsi)
             finsi)
           finsi ;
NATMAJP = NATMAJPI * (1 - positif(V_NBCODP)) + 9 * positif(V_NBCODP) ;
NATMAJCAPI = present(CMAJ) +
	      si (V_CODPFICAP=01 ou V_CODPFICAP=22 ou V_CODPFICAP=24) alors (1) sinon (
		si (V_CODPFICAP=02) alors (2)
		sinon (
		  si (V_CODPFICAP=03 ou V_CODPFICAP=04 ou V_CODPFICAP=35 ou V_CODPFICAP=05 ou V_CODPFICAP=06
		      ou V_CODPFICAP=22 ou V_CODPFICAP=30 ou V_CODPFICAP=30 ou V_CODPFICAP=55)
                  alors (4)
		  sinon (
		    si (V_CODPFICAP=07 ou V_CODPFICAP=08 ou V_CODPFICAP=09 ou V_CODPFICAP=10
			ou V_CODPFICAP=11 ou V_CODPFICAP=12 ou V_CODPFICAP=17  ou V_CODPFICAP=18
			ou V_CODPFICAP=31)
	            alors (1)
	            finsi)
                  finsi)
                finsi)
              finsi ;
NATMAJCAP = NATMAJCAPI * (1 - positif(V_NBCODICAP)) + 9 * positif(V_NBCODICAP) ;
NATMAJCHRI = present(CMAJ) +
	      si (V_CODPFICHR=01 ou V_CODPFICHR=22 ou V_CODPFICHR=24) alors (1) sinon (
		si (V_CODPFICHR=02) alors (2)
		sinon (
		  si (V_CODPFICHR=03 ou V_CODPFICHR=04 ou V_CODPFICHR=05 ou V_CODPFICHR=06
		      ou V_CODPFICHR=22 ou V_CODPFICHR=30 ou V_CODPFICHR=35 ou V_CODPFICHR=55)
                  alors (4)
		  sinon (
		    si (V_CODPFICHR=07 ou V_CODPFICHR=08 ou V_CODPFICHR=09 ou V_CODPFICHR=10
			ou V_CODPFICHR=11 ou V_CODPFICHR=12 ou V_CODPFICHR=17  ou V_CODPFICHR=18
			ou V_CODPFICHR=31)
	            alors (1)
	            finsi)
                  finsi)
                finsi)
              finsi ;
NATMAJCHR = NATMAJCHRI * (1 - positif(V_NBCODICHR)) + 9 * positif(V_NBCODICHR) ;
NATMAJCDISI = present(CMAJ) +
	      si (V_CODPFCDIS=01 ou V_CODPFCDIS=22 ou V_CODPFCDIS=24) alors (1) sinon (
	        si (V_CODPFCDIS=02) alors (2)
	        sinon (
	          si (V_CODPFCDIS=03  ou V_CODPFCDIS=04 ou V_CODPFCDIS=35 ou V_CODPFCDIS=05 ou V_CODPFCDIS=06
	              ou V_CODPFCDIS=22 ou V_CODPFCDIS=30 ou V_CODPFCDIS=55)
		  alors (4)
                  sinon (
                    si (V_CODPFCDIS=07 ou V_CODPFCDIS=08 ou V_CODPFCDIS=09 ou V_CODPFCDIS=10 
			ou V_CODPFCDIS=11 ou V_CODPFCDIS=12 ou V_CODPFCDIS=17 ou V_CODPFCDIS=18 
			ou V_CODPFCDIS=31) 
		    alors (1)
	            finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJCDIS = NATMAJCDISI * (1 - positif(V_NBCODCDIS)) + 9 * positif(V_NBCODCDIS) ;
NATMAJC820I = present(CMAJ) +
	      si (V_CODPFC820=01 ou V_CODPFC820=22 ou V_CODPFC820=24) alors (1) sinon (
	        si (V_CODPFC820=02) alors (2)
	        sinon (
	          si (V_CODPFC820=03  ou V_CODPFC820=04 ou V_CODPFC820=35 ou V_CODPFC820=05 ou V_CODPFC820=06
	              ou V_CODPFC820=22 ou V_CODPFC820=30 ou V_CODPFC820=55)
		  alors (4)
                  sinon (
                    si (V_CODPFC820=07 ou V_CODPFC820=08 ou V_CODPFC820=09 ou V_CODPFC820=10 
			ou V_CODPFC820=11 ou V_CODPFC820=12 ou V_CODPFC820=17 ou V_CODPFC820=18 
			ou V_CODPFC820=31) 
		    alors (1)
	            finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJC820 = NATMAJC820I * (1 - positif(V_NBCODC820)) + 9 * positif(V_NBCODC820) ;
NATMAJGLOAI = present(CMAJ) +
	      si (V_CODPFGLO=01 ou V_CODPFGLO=22 ou V_CODPFGLO=24) alors (1) sinon (
	        si (V_CODPFGLO=02) alors (2)
	        sinon (
	          si (V_CODPFGLO=03  ou V_CODPFGLO=04 ou V_CODPFGLO=35 ou V_CODPFGLO=05 ou V_CODPFGLO=06
	              ou V_CODPFGLO=22 ou V_CODPFGLO=30 ou V_CODPFGLO=55)
		  alors (4)
                  sinon (
                    si (V_CODPFGLO=07 ou V_CODPFGLO=08 ou V_CODPFGLO=09 ou V_CODPFGLO=10 
			ou V_CODPFGLO=11 ou V_CODPFGLO=12 ou V_CODPFGLO=17 ou V_CODPFGLO=18 
			ou V_CODPFGLO=31) 
		    alors (1)
	            finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJGLOA = NATMAJGLOAI * (1 - positif(V_NBCODGLO)) + 9 * positif(V_NBCODGLO) ;
NATMAJCVNI = present(CMAJ) +
	      si (V_CODPFCVN=01 ou V_CODPFCVN=22 ou V_CODPFCVN=24) alors (1) sinon (
	        si (V_CODPFCVN=02) alors (2)
	        sinon (
		  si (V_CODPFCVN=03  ou V_CODPFCVN=04 ou V_CODPFCVN=35 ou V_CODPFCVN=05 ou V_CODPFCVN=06
	              ou V_CODPFCVN=22 ou V_CODPFCVN=30 ou V_CODPFCVN=55)
		  alors (4)
		  sinon (
	            si (V_CODPFCVN=07 ou V_CODPFCVN=08 ou V_CODPFCVN=09 ou V_CODPFCVN=10
	                ou V_CODPFCVN=11 ou V_CODPFCVN=12 ou V_CODPFCVN=17 ou V_CODPFCVN=18
	                ou V_CODPFCVN=31)
	            alors (1)
                    finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJCVN = NATMAJCVNI * (1 - positif(V_NBCODCVN)) + 9 * positif(V_NBCODCVN) ;
NATMAJRSE1I = present(CMAJ) +
	      si (V_CODPFRSE1=01 ou V_CODPFRSE1=22 ou V_CODPFRSE1=24) alors (1) sinon (
	        si (V_CODPFRSE1=02) alors (2)
	        sinon (
	 	  si (V_CODPFRSE1=03 ou V_CODPFRSE1=04 ou V_CODPFRSE1=35 ou V_CODPFRSE1=05 ou V_CODPFRSE1=06
		      ou V_CODPFRSE1=22 ou V_CODPFRSE1=30 ou V_CODPFRSE1=55)
                  alors (4)
		  sinon (
		    si (V_CODPFRSE1=07 ou V_CODPFRSE1=08 ou V_CODPFRSE1=09 ou V_CODPFRSE1=10
		        ou V_CODPFRSE1=11 ou V_CODPFRSE1=12 ou V_CODPFRSE1=17 ou V_CODPFRSE1=18
		        ou V_CODPFRSE1=31)
		    alors (1)
	            finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJRSE1 = NATMAJRSE1I * (1 - positif(V_NBCODRSE1)) + 9 * positif(V_NBCODRSE1) ;
NATMAJRSE2I = present(CMAJ) +
	      si (V_CODPFRSE2=01 ou V_CODPFRSE2=22 ou V_CODPFRSE2=24) alors (1) sinon (
	        si (V_CODPFRSE2=02) alors (2)
	        sinon (
	 	  si (V_CODPFRSE2=03 ou V_CODPFRSE2=04 ou V_CODPFRSE2=35 ou V_CODPFRSE2=05 ou V_CODPFRSE2=06
		      ou V_CODPFRSE2=22 ou V_CODPFRSE2=30 ou V_CODPFRSE2=55)
                  alors (4)
		  sinon (
		    si (V_CODPFRSE2=07 ou V_CODPFRSE2=08 ou V_CODPFRSE2=09 ou V_CODPFRSE2=10
		        ou V_CODPFRSE2=11 ou V_CODPFRSE2=12 ou V_CODPFRSE2=17 ou V_CODPFRSE2=18
		        ou V_CODPFRSE2=31)
		    alors (1)
	            finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJRSE2 = NATMAJRSE2I * (1 - positif(V_NBCODRSE2)) + 9 * positif(V_NBCODRSE2) ;
NATMAJRSE3I = present(CMAJ) +
	      si (V_CODPFRSE3=01 ou V_CODPFRSE3=22 ou V_CODPFRSE3=24) alors (1) sinon (
	        si (V_CODPFRSE3=02) alors (2)
	        sinon (
	 	  si (V_CODPFRSE3=03 ou V_CODPFRSE3=04 ou V_CODPFRSE3=35 ou V_CODPFRSE3=05 ou V_CODPFRSE3=06
		      ou V_CODPFRSE3=22 ou V_CODPFRSE3=30 ou V_CODPFRSE3=55)
                  alors (4)
		  sinon (
		    si (V_CODPFRSE3=07 ou V_CODPFRSE3=08 ou V_CODPFRSE3=09 ou V_CODPFRSE3=10
		        ou V_CODPFRSE3=11 ou V_CODPFRSE3=12 ou V_CODPFRSE3=17 ou V_CODPFRSE3=18
		        ou V_CODPFRSE3=31)
		    alors (1)
	            finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJRSE3 = NATMAJRSE3I * (1 - positif(V_NBCODRSE3)) + 9 * positif(V_NBCODRSE3) ;
NATMAJRSE4I = present(CMAJ) +
	      si (V_CODPFRSE4=01 ou V_CODPFRSE4=22 ou V_CODPFRSE4=24) alors (1) sinon (
	        si (V_CODPFRSE4=02) alors (2)
	        sinon (
	 	  si (V_CODPFRSE4=03 ou V_CODPFRSE4=04 ou V_CODPFRSE4=35 ou V_CODPFRSE4=05 ou V_CODPFRSE4=06
		      ou V_CODPFRSE4=22 ou V_CODPFRSE4=30 ou V_CODPFRSE4=55)
                  alors (4)
		  sinon (
		    si (V_CODPFRSE4=07 ou V_CODPFRSE4=08 ou V_CODPFRSE4=09 ou V_CODPFRSE4=10
		        ou V_CODPFRSE4=11 ou V_CODPFRSE4=12 ou V_CODPFRSE4=17 ou V_CODPFRSE4=18
		        ou V_CODPFRSE4=31)
		    alors (1)
	            finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJRSE4 = NATMAJRSE4I * (1 - positif(V_NBCODRSE4)) + 9 * positif(V_NBCODRSE4) ;
NATMAJRSE5I = present(CMAJ) +
	      si (V_CODPFRSE5=01 ou V_CODPFRSE5=22 ou V_CODPFRSE5=24) alors (1) sinon (
	        si (V_CODPFRSE5=02) alors (2)
	        sinon (
	 	  si (V_CODPFRSE5=03 ou V_CODPFRSE5=04 ou V_CODPFRSE5=35 ou V_CODPFRSE5=05 ou V_CODPFRSE5=06
		      ou V_CODPFRSE5=22 ou V_CODPFRSE5=30 ou V_CODPFRSE5=55)
                  alors (4)
		  sinon (
		    si (V_CODPFRSE5=07 ou V_CODPFRSE5=08 ou V_CODPFRSE5=09 ou V_CODPFRSE5=10
		        ou V_CODPFRSE5=11 ou V_CODPFRSE5=12 ou V_CODPFRSE5=17 ou V_CODPFRSE5=18
		        ou V_CODPFRSE5=31)
		    alors (1)
	            finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJRSE5 = NATMAJRSE5I * (1 - positif(V_NBCODRSE5)) + 9 * positif(V_NBCODRSE5) ;
NATMAJRSE6I = present(CMAJ) +
	      si (V_CODPFRSE6=01 ou V_CODPFRSE6=22 ou V_CODPFRSE6=24) alors (1) sinon (
	        si (V_CODPFRSE6=02) alors (2)
	        sinon (
	 	  si (V_CODPFRSE6=03 ou V_CODPFRSE6=04 ou V_CODPFRSE6=35 ou V_CODPFRSE6=05 ou V_CODPFRSE6=06
		      ou V_CODPFRSE6=22 ou V_CODPFRSE6=30 ou V_CODPFRSE6=55)
                  alors (4)
		  sinon (
		    si (V_CODPFRSE6=07 ou V_CODPFRSE6=08 ou V_CODPFRSE6=09 ou V_CODPFRSE6=10
		        ou V_CODPFRSE6=11 ou V_CODPFRSE6=12 ou V_CODPFRSE6=17 ou V_CODPFRSE6=18
		        ou V_CODPFRSE6=31)
		    alors (1)
	            finsi)
	          finsi)
	        finsi)
	      finsi ;
NATMAJRSE6 = NATMAJRSE6I * (1 - positif(V_NBCODRSE6)) + 9 * positif(V_NBCODRSE6) ;
NATMAJRSE8I = present(CMAJ) +
              si (V_CODPFRSE8=01 ou V_CODPFRSE8=22 ou V_CODPFRSE8=24) alors (1) sinon (
                si (V_CODPFRSE8=02) alors (2)
                sinon (
                  si (V_CODPFRSE8=03 ou V_CODPFRSE8=04 ou V_CODPFRSE8=35 ou V_CODPFRSE8=05 ou V_CODPFRSE8=06
                      ou V_CODPFRSE8=22 ou V_CODPFRSE8=30 ou V_CODPFRSE8=55)
                  alors (4)
                  sinon (
                    si (V_CODPFRSE8=07 ou V_CODPFRSE8=08 ou V_CODPFRSE8=09 ou V_CODPFRSE8=10
                        ou V_CODPFRSE8=11 ou V_CODPFRSE8=12 ou V_CODPFRSE8=17 ou V_CODPFRSE8=18
                        ou V_CODPFRSE8=31)
                    alors (1)
                    finsi)
                  finsi)
                finsi)
             finsi ;
NATMAJRSE8 = NATMAJRSE8I * (1 - positif(V_NBCODRSE8)) + 9 * positif(V_NBCODRSE8) ;
MAJTXC = (1-positif(V_NBCODC)) * ( positif(CMAJ)*COPETO + TXPFC )
         + positif(V_NBCODC) * (-1) ;
MAJTXR = (1-positif(V_NBCODR)) * ( positif(CMAJ)*COPETO + TXPFR )
         + positif(V_NBCODR) * (-1) ;
MAJTXP = (1-positif(V_NBCODP)) * ( positif(CMAJ)*COPETO + TXPFP)
         + positif(V_NBCODP) * (-1) ;
MAJTXCVN = (1-positif(V_NBCODCVN)) * ( positif(CMAJ)*COPETO + TXPFCVN)
	    + positif(V_NBCODCVN) * (-1) ;
MAJTXCDIS = (1-positif(V_NBCODCDIS)) * ( positif(CMAJ)*COPETO + TXPFCDIS)
            + positif(V_NBCODCDIS) * (-1) ;
MAJTXC820 = (1-positif(V_NBCODC820)) * ( positif(CMAJ)*COPETO + TXPFC820)
            + positif(V_NBCODC820) * (-1) ;
MAJTXGLOA = (1-positif(V_NBCODGLO)) * ( positif(CMAJ)*COPETO + TXPFGLO)
            + positif(V_NBCODGLO) * (-1) ;
MAJTXRSE5 = (1-positif(V_NBCODRSE5)) * ( positif(CMAJ)*COPETO + TXPFRSE5)
            + positif(V_NBCODRSE5) * (-1) ;
MAJTXRSE1 = (1-positif(V_NBCODRSE1)) * ( positif(CMAJ)*COPETO + TXPFRSE1)
            + positif(V_NBCODRSE1) * (-1) ;
MAJTXRSE2 = (1-positif(V_NBCODRSE2)) * ( positif(CMAJ)*COPETO + TXPFRSE2)
            + positif(V_NBCODRSE2) * (-1) ;
MAJTXRSE3 = (1-positif(V_NBCODRSE3)) * ( positif(CMAJ)*COPETO + TXPFRSE3)
            + positif(V_NBCODRSE3) * (-1) ;
MAJTXRSE4 = (1-positif(V_NBCODRSE4)) * ( positif(CMAJ)*COPETO + TXPFRSE4)
            + positif(V_NBCODRSE4) * (-1) ;
MAJTXRSE6 = (1-positif(V_NBCODRSE6)) * ( positif(CMAJ)*COPETO + TXPFRSE6)
            + positif(V_NBCODRSE6) * (-1) ;
MAJTXRSE8 = (1-positif(V_NBCODRSE8)) * ( positif(CMAJ)*COPETO + TXPFRSE8)
            + positif(V_NBCODRSE8) * (-1) ;

TXC = (   RETX * positif_ou_nul(RETX) * positif(RETCS)
        + MAJTXC * positif_ou_nul(MAJTXC)* positif(NMAJC1)*null(1-NATMAJC)
        + MAJTXC1 * positif_ou_nul(MAJTXC1)* positif(NMAJC1)*(1-positif(MAJTXC))
        + MAJTXC4 * positif_ou_nul(MAJTXC4)*positif(NMAJC4)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXC)+null(1+MAJTXC1)+null(1+MAJTXC4)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXC)+null(1+MAJTXC1)+null(1+MAJTXC4))
             * positif(RETCS+NMAJC1+NMAJC4)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXR = (   RETX * positif_ou_nul(RETX) * positif(RETRD)
        + MAJTXR * positif_ou_nul(MAJTXR)* positif(NMAJR1)*null(1-NATMAJR)
        + MAJTXR1 * positif_ou_nul(MAJTXR1)* positif(NMAJR1)*(1-positif(MAJTXR))
        + MAJTXR4 * positif_ou_nul(MAJTXR4)*positif(NMAJR4)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXR)+null(1+MAJTXR1)+null(1+MAJTXR4)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXR)+null(1+MAJTXR1)+null(1+MAJTXR4))
             * positif(RETRD+NMAJR1+NMAJR4)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXP = (   RETX * positif_ou_nul(RETX) * positif(RETPSOL)
        + MAJTXP * positif_ou_nul(MAJTXP)* positif(NMAJPSOL1)*null(1-NATMAJP)
        + MAJTXP1 * positif_ou_nul(MAJTXP1)* positif(NMAJPSOL1)*(1-positif(MAJTXP))
        + MAJTXP4 * positif_ou_nul(MAJTXP4)*positif(NMAJPSOL4)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXP)+null(1+MAJTXP1)+null(1+MAJTXP4)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXP)+null(1+MAJTXP1)+null(1+MAJTXP4))
             * positif(RETPSOL+NMAJPSOL1+NMAJPSOL4)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXCVN = (   RETX * positif_ou_nul(RETX) * positif(RETCVN)
        + MAJTXCVN * positif_ou_nul(MAJTXCVN)* positif(NMAJCVN1)*null(1-NATMAJCVN)
        + MAJTXCVN1 * positif_ou_nul(MAJTXCVN1)* positif(NMAJCVN1)*(1-positif(MAJTXCVN))
        + MAJTXCVN4 * positif_ou_nul(MAJTXCVN4)*positif(NMAJCVN4)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXCVN)+null(1+MAJTXCVN1)+null(1+MAJTXCVN4)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXCVN)+null(1+MAJTXCVN1)+null(1+MAJTXCVN4))
             * positif(RETCVN+NMAJCVN1+NMAJCVN4)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXCDIS = (   RETX * positif_ou_nul(RETX) * positif(RETCDIS)
        + MAJTXCDIS * positif_ou_nul(MAJTXCDIS)* positif(NMAJCDIS1)*null(1-NATMAJCDIS)
        + MAJTXCDIS1 * positif_ou_nul(MAJTXCDIS1)* positif(NMAJCDIS1)*(1-positif(MAJTXCDIS))
        + MAJTXCDIS4 * positif_ou_nul(MAJTXCDIS4)*positif(NMAJCDIS4)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXCDIS)+null(1+MAJTXCDIS1)+null(1+MAJTXCDIS4)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXCDIS)+null(1+MAJTXCDIS1)+null(1+MAJTXCDIS4))
             * positif(RETCDIS+NMAJCDIS1+NMAJCDIS4)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXCSG820 = (   RETX * positif_ou_nul(RETX) * positif(RETCSG820)
        + MAJTXC820 * positif_ou_nul(MAJTXC820)* positif(NMAJC8201)*null(1-NATMAJC820)
        + MAJTXC8201 * positif_ou_nul(MAJTXC8201)* positif(NMAJC8201)*(1-positif(MAJTXC820))
        + MAJTXC8204 * positif_ou_nul(MAJTXC8204)*positif(NMAJC8204)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXC820)+null(1+MAJTXC8201)+null(1+MAJTXC8204)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXC820)+null(1+MAJTXC8201)+null(1+MAJTXC8204))
             * positif(RETCSG820+NMAJC8201+NMAJC8204)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXGLOA = (   RETX * positif_ou_nul(RETX) * positif(RETGLOA)
        + MAJTXGLOA * positif_ou_nul(MAJTXGLOA)* positif(NMAJGLO1)*null(1-NATMAJGLOA)
        + MAJTXGLO1 * positif_ou_nul(MAJTXGLO1)* positif(NMAJGLO1)*(1-positif(MAJTXGLOA))
        + MAJTXGLO4 * positif_ou_nul(MAJTXGLO4)*positif(NMAJGLO4)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXGLOA)+null(1+MAJTXGLO1)+null(1+MAJTXGLO4)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXGLOA)+null(1+MAJTXGLO1)+null(1+MAJTXGLO4))
             * positif(RETGLOA+NMAJGLO1+NMAJGLO4)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXRSE1 = (   RETX * positif_ou_nul(RETX) * positif(RETRSE1)
        + MAJTXRSE1 * positif_ou_nul(MAJTXRSE1)* positif(NMAJRSE11)*null(1-NATMAJRSE1)
        + MAJTXRSE11 * positif_ou_nul(MAJTXRSE11)* positif(NMAJRSE11)*(1-positif(MAJTXRSE1))
        + MAJTXRSE14 * positif_ou_nul(MAJTXRSE14)*positif(NMAJRSE14)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXRSE1)+null(1+MAJTXRSE11)+null(1+MAJTXRSE14)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXRSE1)+null(1+MAJTXRSE11)+null(1+MAJTXRSE14))
             * positif(RETRSE1+NMAJRSE11+NMAJRSE14)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXRSE2 = (   RETX * positif_ou_nul(RETX) * positif(RETRSE2)
        + MAJTXRSE2 * positif_ou_nul(MAJTXRSE2)* positif(NMAJRSE21)*null(1-NATMAJRSE2)
        + MAJTXRSE21 * positif_ou_nul(MAJTXRSE21)* positif(NMAJRSE21)*(1-positif(MAJTXRSE2))
        + MAJTXRSE24 * positif_ou_nul(MAJTXRSE24)*positif(NMAJRSE24)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXRSE2)+null(1+MAJTXRSE21)+null(1+MAJTXRSE24)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXRSE2)+null(1+MAJTXRSE21)+null(1+MAJTXRSE24))
             * positif(RETRSE2+NMAJRSE21+NMAJRSE24)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXRSE3 = (   RETX * positif_ou_nul(RETX) * positif(RETRSE3)
        + MAJTXRSE3 * positif_ou_nul(MAJTXRSE3)* positif(NMAJRSE31)*null(1-NATMAJRSE3)
        + MAJTXRSE31 * positif_ou_nul(MAJTXRSE31)* positif(NMAJRSE31)*(1-positif(MAJTXRSE3))
        + MAJTXRSE34 * positif_ou_nul(MAJTXRSE34)*positif(NMAJRSE34)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXRSE3)+null(1+MAJTXRSE31)+null(1+MAJTXRSE34)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXRSE3)+null(1+MAJTXRSE31)+null(1+MAJTXRSE34))
             * positif(RETRSE3+NMAJRSE31+NMAJRSE34)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXRSE4 = (   RETX * positif_ou_nul(RETX) * positif(RETRSE4)
        + MAJTXRSE4 * positif_ou_nul(MAJTXRSE4)* positif(NMAJRSE41)*null(1-NATMAJRSE4)
        + MAJTXRSE41 * positif_ou_nul(MAJTXRSE41)* positif(NMAJRSE41)*(1-positif(MAJTXRSE4))
        + MAJTXRSE44 * positif_ou_nul(MAJTXRSE44)*positif(NMAJRSE44)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXRSE4)+null(1+MAJTXRSE41)+null(1+MAJTXRSE44)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXRSE4)+null(1+MAJTXRSE41)+null(1+MAJTXRSE44))
             * positif(RETRSE4+NMAJRSE41+NMAJRSE44)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXRSE5 = (   RETX * positif_ou_nul(RETX) * positif(RETRSE5)
        + MAJTXRSE5 * positif_ou_nul(MAJTXRSE5)* positif(NMAJRSE51)*null(1-NATMAJRSE5)
        + MAJTXRSE51 * positif_ou_nul(MAJTXRSE51)* positif(NMAJRSE51)*(1-positif(MAJTXRSE5))
        + MAJTXRSE54 * positif_ou_nul(MAJTXRSE54)*positif(NMAJRSE54)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXRSE5)+null(1+MAJTXRSE51)+null(1+MAJTXRSE54)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXRSE5)+null(1+MAJTXRSE51)+null(1+MAJTXRSE54))
             * positif(RETRSE5+NMAJRSE51+NMAJRSE54)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXRSE6 = (   RETX * positif_ou_nul(RETX) * positif(RETRSE6)
        + MAJTXRSE6 * positif_ou_nul(MAJTXRSE6)* positif(NMAJRSE61)*null(1-NATMAJRSE6)
        + MAJTXRSE61 * positif_ou_nul(MAJTXRSE61)* positif(NMAJRSE61)*(1-positif(MAJTXRSE6))
        + MAJTXRSE64 * positif_ou_nul(MAJTXRSE64)*positif(NMAJRSE64)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXRSE6)+null(1+MAJTXRSE61)+null(1+MAJTXRSE64)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXRSE6)+null(1+MAJTXRSE61)+null(1+MAJTXRSE64))
             * positif(RETRSE6+NMAJRSE61+NMAJRSE64)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
TXRSE8 = (   RETX * positif_ou_nul(RETX) * positif(RETRSE8)
        + MAJTXRSE8 * positif_ou_nul(MAJTXRSE8)* positif(NMAJRSE81)*null(1-NATMAJRSE8)
        + MAJTXRSE81 * positif_ou_nul(MAJTXRSE81)* positif(NMAJRSE81)*(1-positif(MAJTXRSE8))
        + MAJTXRSE84 * positif_ou_nul(MAJTXRSE84)*positif(NMAJRSE84)
      ) * positif_ou_nul (TXTO) * (1-positif(null(1+RETX)+null(1+MAJTXRSE8)+null(1+MAJTXRSE81)+null(1+MAJTXRSE84)))
      + (-1) * positif (TXTO) * positif(null(1+RETX)+null(1+MAJTXRSE8)+null(1+MAJTXRSE81)+null(1+MAJTXRSE84))
             * positif(RETRSE8+NMAJRSE81+NMAJRSE84)
      + (-1) * (1 - positif_ou_nul(TXTO)) * positif(TXTO * (-1));
regle 221720:
application : iliad ;

pour x=01..12,30,31,55:
RAP_UTIx = NUTOTx_D * positif(APPLI_OCEANS) ;

regle 221730:
application : iliad ;

pour x=02..12,30,31,55;i=RF,BA,LO,NC,CO:
RAPi_REPx = NViDx_D * positif(APPLI_OCEANS) ;

regle 221740:
application : iliad ;

pour i=01..12,30,31,55:
RAPCO_Ni = NCCOi_D * positif(APPLI_OCEANS) ;

regle 221750:
application : iliad ;

FPV = FPTV - DEDSV* positif(APPLI_OCEANS);
FPC = FPTC - DEDSC* positif(APPLI_OCEANS);
FPP = somme(i=1..4: FPTi) - DEDSP* positif(APPLI_OCEANS);

regle 221760:
application :  iliad ;

IMPNET = null(4 - V_IND_TRAIT) * (IINET + IREST * (-1))
	 + null(5 - V_IND_TRAIT) * 
		(positif(IDEGR) * positif(IREST) * positif(SEUIL_8 - IREST) * IDEGR * (-1)
		+ (1 - positif(positif(IDEGR) * positif(IREST) * positif(SEUIL_8 - IREST))) * (IINET - IREST - IDEGR)) ;

IMPNETCS = NAPCS - (V_CSANT - V_CSNANT) ;
IMPNETRD = NAPRD - V_RDANT ;
IMPNETPSOL = NAPPSOL - (V_PSOLANT - V_PSOLNANT) ;
IMPNETCSAL = NAPCVN - V_CVNANT ;
IMPNETCDIS = NAPCDIS - V_CDISANT ;
IMPNETC820 = NAPCSG820 - V_CSG820ANT ;
IMPNETGLO = NAPGLOA - V_GLOANT ;
IMPNETRSE = NAPRSE1 + NAPRSE2 + NAPRSE3 + NAPRSE4 + NAPRSE5 + NAPRSE8
          - V_RSE1ANT- V_RSE2ANT- V_RSE3ANT- V_RSE4ANT- V_RSE5ANT - V_RSE8ANT;
IMPNETRSE6 =  NAPRSE6 - V_RSE6ANT ;

BASEXOGEN = (1-present(IPTEFP)) * 
            max(0,( RG+ TOTALQUO))*(1-positif(APPLI_COLBERT));
MONTNETCS = (PSOL +PTOPSOL)*(1-positif(APPLI_COLBERT));
DBACT = si ((APPLI_COLBERT=0) et ( present(RDCOM)=1 et present(NBACT)=0 ))
        alors (0)
        sinon (NBACT)
        finsi;

IMPNETIR = (NAPTIR - V_ANTIR - V_PCAPANT - V_TAXANT - V_CHRANT) * null(4 - V_IND_TRAIT)
	    + (IMPNET - IMPNETCS - IMPNETRD - IMPNETPSOL - IMPNETCSAL - IMPNETCDIS - IMPNETC820 - IMPNETGLO - IMPNETRSE - IMPNETRSE6 + ACPASIR + ACPASCS + ACPASPSOL + (V_ACPASIRNANT  - V_ACPASIRANT + V_ACPASCSNANT - V_ACPASCSANT + V_ACPASPSONANT - V_ACPASPSOLANT ) * positif_ou_nul(ACPASIR +V_ACPASIRNANT - V_ACPASIRANT) ) * null(5 - V_IND_TRAIT) ;
regle 221770:
application : iliad  ;

IMPNETPS = NAPCR61 - TOTCRA + ACPASCS - V_ACPASCSANT + V_ACPASCSNANT + ACPASPSOL - V_ACPASPSOLANT + V_ACPASPSONANT ;

regle 221780:
application : iliad  ;

RECUMBIS = si (V_NIMPA+0 = 1)
           alors (V_ANTRE+RECUM_A)
           sinon ((V_ANTRE+RECUM_A) * positif_ou_nul((V_ANTRE+RECUM_A) - SEUIL_8))
           finsi;
RECUMBISIR = si (V_NIMPAIR+0 = 1)
                alors (V_ANTREIR)
                sinon (V_ANTREIR * positif_ou_nul(V_ANTREIR - SEUIL_8))
             finsi;

regle 221790:
application : iliad  ;

IRCUMBIS = si
               (( (V_ANTIR + IRCUM_A - (IRNET+IRANT) * positif(IRNET+IRANT) - TAXANET - PCAPNET - HAUTREVNET 
	          + (TOTCRA-CSTOT)) > 0 et
                 (V_ANTIR + IRCUM_A - (IRNET+IRANT) * positif(IRNET+IRANT) - TAXANET - PCAPNET - HAUTREVNET  
		 + (TOTCRA-CSTOT)) < SEUIL_8 )
                 ou
                  ( (TAXANET + PCAPNET + HAUTREVNET + (IRNET+IRANT) * positif(IRNET+IRANT) - V_ANTIR- IRCUM_A
		  + (CSTOT-TOTCRA)) > 0 et
                    (TAXANET + PCAPNET + HAUTREVNET + (IRNET+IRANT) * positif(IRNET+IRANT) - V_ANTIR- IRCUM_A 
		    + (CSTOT-TOTCRA)) < SEUIL_12 ) )
                 alors
                      (V_ANTIR + IRCUM_A + 0)
                 sinon
                      (IRNET + IRANT)
                 finsi ;

regle 221800:
application : iliad  ;


TOTAXAGA = si ((APPLI_COLBERT=0) et  (IRNET - V_ANTIR + TAXANET - V_TAXANT + PCAPNET - V_PCAPANT + HAUTREVNET - V_CHRANT >= SEUIL_12)
                ou ( (-IRNET + V_ANTIR - TAXANET + V_TAXANT  - PCAPNET + V_PCAPANT - HAUTREVNET + V_CHRANT ) >= SEUIL_8) )
                alors(TAXANET * positif(TAXACUM))
                sinon(V_TAXANT * positif(TAXACUM) + 0 )
                finsi;

PCAPTOT = si ((APPLI_COLBERT=0) et ( (IRNET - V_ANTIR + TAXANET - V_TAXANT + PCAPNET - V_PCAPANT + HAUTREVNET - V_CHRANT>= SEUIL_12)
                ou ( (-IRNET + V_ANTIR - TAXANET + V_TAXANT - PCAPNET + V_PCAPANT - HAUTREVNET + V_CHRANT) >= SEUIL_8) ))
                alors(PCAPNET * positif(PCAPCUM))
                sinon(V_PCAPANT * positif(PCAPCUM) + 0 )
                finsi;

HAUTREVTOT = si ((APPLI_COLBERT=0) et ( (IRNET - V_ANTIR + TAXANET - V_TAXANT + PCAPNET - V_PCAPANT + HAUTREVNET - V_CHRANT >= SEUIL_12)
                ou ( (-IRNET + V_ANTIR - TAXANET + V_TAXANT - PCAPNET + V_PCAPANT - HAUTREVNET + V_CHRANT ) >= SEUIL_8) ))
                alors(HAUTREVNET * positif(HAUTREVCUM))
                sinon(V_CHRANT * positif(HAUTREVCUM) + 0 )
                finsi;


regle isf 221815:
application : iliad  ;

IFICUM = null (4 - V_IND_TRAIT) *
                                (IFINET * positif_ou_nul (IFINET - SEUIL_12)
                                        + min( 0, IFINET) * positif( SEUIL_12 - IFINET )
                                )

         + null(5 - V_IND_TRAIT)*
                                (positif(SEUIL_12 - IFI4BIS) * 0
                                 + (1-positif(SEUIL_12 - IFI4BIS)) *                                     
                                     (
                                         positif(positif_ou_nul(-IFINET + V_ANTIFI - SEUIL_8)				     
                                                 + positif_ou_nul(IFINET - V_ANTIFI - SEUIL_12)
                                                ) * IFINET
                                      + (1-positif(positif_ou_nul(-IFINET + V_ANTIFI - SEUIL_8)
                                                   + positif_ou_nul(IFINET - V_ANTIFI - SEUIL_12)
                                                  )
                                        ) * V_ANTIFI
                                     )
                                )* (1-positif(APPLI_OCEANS));
regle 221820:
application : iliad  ;

INDSEUIL61 = positif_ou_nul(IAMD1 - SEUIL_61) ;

INDSEUIL12 = positif_ou_nul(max(0 , CSNET + RDNET + PRSNET + PSOLNET + CVNNET + CDISNET + CSG820NET
                                    + CGLOANET + RSE1NET + RSE2NET + RSE3NET + RSE4NET
                                    + RSE5NET + RSE6NET + RSE8NET) - SEUIL_12) ;
				   
INDSEUIL12IR = positif_ou_nul(IRNET + TAXANET + PCAPNET + HAUTREVNET - SEUIL_12) ;

regle 221840:
application : iliad  ;


NAPTEMP = positif(positif(SEUIL_8 - abs(IRPSCUM - RECUM)) * (1-positif(IRPSCUM-RECUM))+ positif(SEUIL_12 - IRPSCUM - RECUM)*positif(IRPSCUM-RECUM)) * 0 
        + (1-positif(positif(SEUIL_8 - abs(IRPSCUM - RECUM)) * (1-positif(IRPSCUM-RECUM))+ positif(SEUIL_12 - IRPSCUM - RECUM)*positif(IRPSCUM-RECUM)))*(IRPSCUM - RECUM) ;

regle 221850:
application : iliad  ;


NAPTEMPCX = IRPSCUM - NONMER - RECUM + (NONREST * positif(IRPSCUM - RECUM - TOTIRPSANT + 0)) ;

regle 221860:
application : iliad  ;

VARPS61 = CSG + RDSN + PSOL + PCSG + PRDS + PPSOL + CVNN + PCVN + CDIS + PCDIS  + MCSG820 + PCSG820
          + CGLOA + PGLOA + RSE1N + PRSE1 + RSE2N + PRSE2 + RSE3N + PRSE3 + RSE4N + PRSE4
          + RSE5N + PRSE5 + RSE6N + PRSE6 + RSE8N + PRSE8
          - CSGIM - CRDSIM - PRSPROV - COD8YT - CDISPROV -COD8YL - CSPROVYD - CSPROVYE - CSPROVYF - CSPROVYN
          - CSPROVYG - CSPROVYH - CSPROVYP - COD8YQ - COD8YV - COD8YX + 0 ;

VARPS611 =  max(0,CSGC - CICSG) + (RDSC - CIRDS) + max(0,MPSOL - CIPSOL) + PCSG + PRDS +PPSOL + CVNN + PCVN + CDIS + PCDIS  +MCSG820 + PCSG820
           + CGLOA + PGLOA + RSE1N + PRSE1 + RSE2N + PRSE2 + RSE3N + PRSE3 + RSE4N + PRSE4
           + RSE5N + PRSE5 + RSE6N + PRSE6 + RSE8N + PRSE8
           - CSGIM - CRDSIM - PRSPROV - COD8YT - CDISPROV -COD8YL-CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYN
           -CSPROVYG-CSPROVYH-CSPROVYP-COD8YQ-COD8YV-COD8YX+0 ;



VARIR61 = IAMD1 + V_ANTREIR + 0 ;
VARIR12 = BRASAR + NRINET + IMPRET + CODZRA + 0 ;

regle 221870:
application : iliad  ;


TAXACUM = (1 - positif(IRESTITIR)) 
          * (1 - positif(positif(SEUIL_61 - VARIR61) * positif(SEUIL_61 - VARIRDROIT) * (1 - INDTXMIN)
                         + positif_ou_nul(SEUIL_TXMIN - IAMD1) * positif(SEUIL_TXMIN - VARIR61) * positif(SEUIL_TXMIN - VARIRDROIT) * INDTXMIN) * null(TOTPENIR))
          * TAXANET ;

PCAPCUM = (1 - positif(IRESTITIR))
          * (1 - positif(positif(SEUIL_61 - VARIR61) * positif(SEUIL_61 - VARIRDROIT) * (1 - INDTXMIN)
                         + positif_ou_nul(SEUIL_TXMIN - IAMD1) * positif(SEUIL_TXMIN - VARIR61) * positif(SEUIL_TXMIN - VARIRDROIT) * INDTXMIN) * null(TOTPENIR))
          * PCAPNET ;

HAUTREVCUM = (1 - positif(IRESTITIR)) 
             * (1 - positif(positif(SEUIL_61 - VARIR61) * positif(SEUIL_61 - VARIRDROIT) * (1 - INDTXMIN)
                            + positif_ou_nul(SEUIL_TXMIN - IAMD1) * positif(SEUIL_TXMIN - VARIR61) * positif(SEUIL_TXMIN - VARIRDROIT) * INDTXMIN) * null(TOTPENIR)) 
	     * HAUTREVNET ;

IRCUM = (1 - positif(IRESTITIR)) 
        * (1 - positif(positif(SEUIL_61 - VARIR61) * positif(SEUIL_61 - VARIRDROIT) * (1 - INDTXMIN)
                      + positif_ou_nul(SEUIL_TXMIN - IAMD1) * positif(SEUIL_TXMIN - VARIR61) * positif(SEUIL_TXMIN - VARIRDROIT) * INDTXMIN) * null(TOTPENIR)) 
	* IRNET ;

regle 221874:
application : iliad  ;

TOTIRCUM = IRCUM + TAXACUM + PCAPCUM + HAUTREVCUM + AVRICIIR + CIADCREB3 - RASSALIR - RASACOIR - IMPETAL + IMPETAL5 + RASCTXIR ; 

regle 221876:
application : iliad  ;

RECUM = max(0 , -(TOTIRCUM - RECUMIR + NAPCR61 + NAPCRP * (1 - positif(NAPCRP)))) ;

IRPSCUM = max(0 , TOTIRCUM - RECUMIR + NAPCR61 + NAPCRP * (1 - positif(NAPCRP))) ;

regle 221880:
application : iliad  ;


RECUMIR = positif(IRESTITIR) * IRESTITIR ;

regle 221890:
application : iliad  ;


TOTIRPS = (IRPSCUM - NONMER + NONREST - RECUM) ;

regle 221900:
application :  iliad ;

CSTOT = max(0 , CSG + RDSN + PSOL + PCSG + PRDS + PPSOL + CVNN + PCVN + CDIS + PCDIS + MCSG820 + PCSG820
                + CSGLOA + PGLOA + RSE1N + PRSE1 + RSE2N + PRSE2 + RSE3N + PRSE3 + RSE4N + PRSE4
                + RSE5N + PRSE5 + RSE6N + PRSE6 + RSE8N + PRSE8) ;

regle 221910:
application : iliad  ;

TOTCRBIS = si (
               ( (TOTCRA-CSTOT>0) et (TOTCRA-CSTOT<SEUIL_8)
                 et (CSTOT >= SEUIL_61) )
               ou (
                   (CSTOT-TOTCRA>0) et (CSTOT-TOTCRA<SEUIL_61)
                   et (V_IND_TRAIT=4)
                  )
               ou (
                   (CSTOT-TOTCRA>0) et (CSTOT-TOTCRA<SEUIL_12)
                   et (V_IND_TRAIT>4)
                  )
              )
           alors (TOTCRA + 0)
           sinon (CSTOT * positif_ou_nul(CSTOT - SEUIL_61))
           finsi;
TOTCR = positif(SEUIL_61 - TOTCRBIS + CSGIM + CRDSIM + PRSPROV + COD8YT + CDISPROV + COD8YL + CSPROVYD + CSPROVYE + CSPROVYF + CSPROVYN + CSPROVYG + CSPROVYH + CSPROVYP + COD8YQ + COD8ZH + COD8YV + COD8YX)
        * positif(CSGIM + CRDSIM + PRSPROV + COD8YT + CDISPROV + COD8YL + CSPROVYD + CSPROVYE + CSPROVYF + CSPROVYG + CSPROVYH + CSPROVYP + COD8YQ + COD8ZH + COD8YV + COD8YX + 0)
        * (CSGIM + CRDSIM + PRSPROV + COD8YT + CDISPROV + COD8YL + CSPROVYD + CSPROVYE + CSPROVYF + CSPROVYN + CSPROVYG + CSPROVYH + CSPROVYP + COD8YQ + COD8ZH + COD8YV + COD8YX + 0)
        + (1 - positif(SEUIL_61 - TOTCRBIS + CSGIM + CRDSIM + PRSPROV + COD8YT + CDISPROV + COD8YL + CSPROVYD + CSPROVYE + CSPROVYF + CSPROVYN + CSPROVYG + CSPROVYH + CSPROVYP + COD8YQ + COD8ZH + COD8YV + COD8YX)
               * positif(CSGIM + CRDSIM + PRSPROV + COD8YT + CDISPROV + COD8YL + CSPROVYD + CSPROVYE + CSPROVYF + CSPROVYG + CSPROVYH + CSPROVYP + COD8YQ + COD8ZH + COD8YV + COD8YX + 0))
          * (TOTCRBIS + 0) ;


regle 221920:
application : iliad  ;

CSNETEMP = CSNET * INDSEUIL61;
PSOLNETEMP = PSOLNET * INDSEUIL61;
RDNETEMP = RDNET * INDSEUIL61;
CVNNETEMP = CVNNET * INDSEUIL61;
CDISNETEMP = CDISNET * INDSEUIL61;
C820NETEMP = CSG820NET * INDSEUIL61;
GLONETEMP = CGLOANET * INDSEUIL61;
RSE1NETEMP = RSE1NET * INDSEUIL61;
RSE2NETEMP = RSE2NET * INDSEUIL61;
RSE3NETEMP = RSE3NET * INDSEUIL61;
RSE4NETEMP = RSE4NET * INDSEUIL61;
RSE5NETEMP = RSE5NET * INDSEUIL61;
RSE6NETEMP = RSE6NET * INDSEUIL61;
RSE8NETEMP = RSE8NET * INDSEUIL61;

regle 221930:
application : iliad  ;

NAPCRPB = max(0 , CSBRUT+RDBRUT+PSOLBRUT+CVNNET+CDISNET+CSG820NET+CGLOANET+RSE1NET+RSE2NET+RSE3NET+RSE4NET
                 +RSE5NET+RSE6NET+RSE8NET );

NAPCRP = CSNET + RDNET + PSOLNET + CVNNET + CDISNET + CSG820NET + CGLOANET + RSE1NET + RSE2NET + RSE3NET + RSE4NET + RSE5NET + RSE6NET + RSE8NET ;

NAPCRTOT = NAPCRP;

regle 221940:
application : iliad  ;


NAPCR = null(4-V_IND_TRAIT)
               * max(0 ,  TOTCR - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL
                                -CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYN-CSPROVYG-CSPROVYH-CSPROVYP-COD8YT-COD8YQ - COD8ZH - COD8YV - COD8YX)
               * positif_ou_nul((TOTCR - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL
                                       -CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYN-CSPROVYG-CSPROVYH-CSPROVYP-COD8YT-COD8YQ-COD8ZH - COD8YV - COD8YX) - SEUIL_61)
        + null(5-V_IND_TRAIT)
               * max(0 , (TOTCR - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL
                                -CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYN-CSPROVYG-CSPROVYH-CSPROVYP-COD8YT-COD8YQ-COD8ZH-COD8YV-COD8YX) - TOTCRA )
               * positif_ou_nul((TOTCR - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL
                                       -CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYN-CSPROVYG-CSPROVYH-CSPROVYP-COD8YT-COD8YQ-COD8ZH-COD8YV-COD8YX) -
                         TOTCRA - SEUIL_12);
NAPCRBIS = null(4-V_IND_TRAIT)
               * max(0 ,  TOTCR - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL-CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYG-CSPROVYH-COD8YT-COD8YV-COD8YX)
        * positif_ou_nul((TOTCR - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL-CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYG-CSPROVYH-COD8YT-COD8YV-COD8YX) - SEUIL_61)
        + null(5-V_IND_TRAIT)
               * max(0 , (TOTCR - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL-CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYG-CSPROVYH-COD8YT-COD8YV-COD8YX) - TOTCRA )
        * positif_ou_nul((TOTCR - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL-CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYG-CSPROVYH-COD8YT-COD8YV-COD8YX) -
                         TOTCRA - SEUIL_12);
NAPCRINR = null(4-V_IND_TRAIT)
               * max(0 ,  CSTOT - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL-CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYG-CSPROVYH-COD8YT-COD8YV-COD8YX)
        + null(5-V_IND_TRAIT)
               * max(0 , (CSTOT - CSGIM - CRDSIM - PRSPROV - CDISPROV -COD8YL-CSPROVYD-CSPROVYE-CSPROVYF-CSPROVYG-CSPROVYH-COD8YT-COD8YV-COD8YX) );
regle 221944:
application : iliad  ;
NAPCR61 = NAPCS + NAPRD + NAPPSOL + NAPCVN + NAPCDIS + NAPCSG820 + NAPGLOA + NAPRSE1 + NAPRSE2 + NAPRSE3 + NAPRSE4 
          + NAPRSE5 + NAPRSE6 + NAPRSE8 - RASACOPS + RASCTXPS - RESTITCS - RESTITPSOL ;

NAPCRB61 = NAPCS + NAPRD + NAPPSOL + NAPCVN + NAPCDIS + NAPCSG820 + NAPGLOA + NAPRSE1 + NAPRSE2 + NAPRSE3 + NAPRSE4
           + NAPRSE5 + NAPRSE6 + NAPRSE8 ;
regle 221945:
application : iliad  ;
IRPSNET = NAPCR61 + NAPTIRNET + NAPCRP * (1 - positif(NAPCRP)) ;

regle 221947:
application : iliad  ;


NAPC61 = NAPCSB + NAPRDB + NAPPSOLB + NAPCVN + NAPCDIS + NAPCSG820 + NAPGLOA + NAPRSE1 + NAPRSE2 + NAPRSE3 + NAPRSE4 + NAPRSE5 + NAPRSE6 + NAPRSE8 ;

regle 221950:
application : iliad  ;


CRDEG = max(0 , TOTCRA - TOTCR) * positif_ou_nul(TOTCRA - (TOTCR - SEUIL_8)) ;

regle 221960:
application : iliad ;


CS_DEG = max(0 , TOTCRA - CSTOT * positif_ou_nul(CSTOT - SEUIL_61)) * ( 1-positif(APPLI_OCEANS));

ECS_DEG = arr((CS_DEG / TAUX_CONV) * 100) / 100 * ( 1-positif(APPLI_OCEANS));

regle 221970:
application:  iliad ;


ABSPE = (1-positif(NDA)) * 9
        +
        positif(NAB) * (1-positif(NAB-1)) * (1-positif(NDA-1)) * positif (NDA)
        +
        positif(NAB-1) * (1-positif(NDA-1)) * positif(NDA) * 2
        +
        positif(NAB) * (1-positif(NAB-1)) * positif(NDA-1) * 3
        +
        positif(NAB-1) * positif(NDA-1) * 6;

INDDG =  positif(DAR - RG - TOTALQUO) * positif(DAR) ;

regle 221980:
application :  iliad ;

CODINI =  99 * positif(NATIMP)
        + 0 * null(NATIMP)
        ;

regle 221990:
application : iliad ;

NAT1 =            (1-positif(V_IND_TRAIT - 4)) * positif(NAPT)
                  +
                   positif(V_IND_TRAIT - 4) * positif(positif_ou_nul(IRPSCUM-SEUIL_12) *  null(NAPT) + positif(NAPT));
NAT1BIS = (positif (IRANT)) * (1 - positif (NAT1) )
          * (1 - positif(IDEGR))+0;
NAT11 = (11 * positif(SEUIL_12 - V_IRPSANT) * positif(IREST * (1 - ANNUL2042))
           * positif(IRE + RASSALIR + RASACOIR - RASCTXIR - AVRICIIR  - CIADCREB3 + RASACOPS - RASCTXPS + RESTITPS - IREST)) * (1 - positif_ou_nul(NAPT)) ;
NAT21 = (21 * positif(SEUIL_12 - V_IRPSANT) * positif(IREST * (1 - ANNUL2042))
           * null(IRE + RASSALIR + RASACOIR - RASCTXIR - AVRICIIR  - CIADCREB3 + RASACOPS - RASCTXPS + RESTITPS - IREST)) * (1 - positif_ou_nul(NAPT)) ;
NAT70 = 70 * null(NAPTEMPCX)* (1-positif_ou_nul(NAPT));
NAT71 = 71 * positif(NAPTEMPCX) * (1-positif_ou_nul(NAPT));
NAT81 = 81 * positif_ou_nul(V_IRPSANT-SEUIL_12) * positif(IREST * (1 - ANNUL2042)) * positif(IRE - IREST)* (1-positif_ou_nul(NAPT));
NAT91 = 91 * positif_ou_nul(V_IRPSANT-SEUIL_12) * positif(IREST * (1 - ANNUL2042)) * null(IRE - IREST)* (1-positif_ou_nul(NAPT));
NATIMP = ( NAT1 +
             (1-positif(NAT1))*(NAT11 + NAT21 + NAT70 + NAT71 + NAT81 + NAT91) );

regle 222000:
application :  iliad ;

NATIMPIR = null(V_IND_TRAIT - 4) 
	   * positif (positif(NAPTOT - NAPTOTAIR - IRANT) * positif_ou_nul(IAMD1 - SEUIL_61)
                      + positif(IRE - IRESTITIR) * positif(IRESTITIR))

           + null(V_IND_TRAIT - 5) * (positif(positif_ou_nul(IAMD1 - SEUIL_61) 
	                                     ) * (1 - ANNUL2042)
	                              + ANNUL2042 * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66))) ;


IITAZIR = (IINETIR - IRESTITIR) * FLAG_BAREM + V_BARIITAZIR * (null(FLAG_BARIITANEG) - positif(FLAG_BARIITANEG)) * (1 - FLAG_BAREM) ; 

INDIRN1 = positif((1 - positif(IITAZIR))
                + positif(positif(IITAZIR) * null(1 - IND61IR))
                + positif(positif(IITAZIR) * null(2 - IND61IR) * positif(12 - (IITAZIR + NAPCR)))) ;

IINETCIMR = IITAZIR * positif_ou_nul(IITAZIR) ;

IRESTCIMR = IITAZIR * (positif_ou_nul(IITAZIR) - 1) ;

regle 222010:
application : iliad  ;

NATCRP = si (NAPCR > 0) 
         alors (1)
         sinon (si (NAPCRP + 0 > 0)
                alors (2)
                sinon (si (CRDEG+0>0)
                       alors (3)
                       sinon (0)
                       finsi
                      )
                finsi
               )
         finsi;

regle isf 222020:
application : iliad  ;


NATIMPIFI = (1-positif(ANNUL2042))* max (0, (1 * positif(IFICUM)

                 + 2 * (1 - positif(IFICUM)) * (1 - null(IFINET))

                  + 3 *  null(IFINET) * positif(IFIPAT)

                  + 0 * (null(INDCTX23) * null(5-V_IND_TRAIT) * null(IFIPAT)
                         + positif_ou_nul(COD9GY) * null(4-V_IND_TRAIT)))
                );

INDCEXIFI = null(1 - NATIMPIFI) * 1
          + positif(null(0 - NATIMPIFI)+null(2 - NATIMPIFI)+null(3 - NATIMPIFI)) * 3 ;

regle 222030:
application : iliad   ;


IFG = positif(min(PLAF_REDGARD,RDGARD1) + min(PLAF_REDGARD,RDGARD2)
            + min(PLAF_REDGARD,RDGARD3) + min(PLAF_REDGARD,RDGARD4) 
            - max(0,RP)) * positif(somme(i=1..4:RDGARDi));

regle 222040:
application :  iliad ;


INDGARD = IFG + 9 * (1 - positif(IFG)) ;

regle 222050:
application :  iliad ;

DEFTS = (1 - positif(somme(i=V,C,1..4:TSNTi + PRNNi) -  GLN3) ) *
      abs( somme(i=V,C,1..4:TSNTi + PRNNi) - GLN3 )*(1-positif(APPLI_COLBERT)) ;
PRN = max(0 , somme(i=V,C,1..4:PRNi) + PRN1AI + PRNRAI + PRN1BI + PRNRBI + PRNRCK + PRN1CI + PRN1DI + PRN1EI + PRN1FI 
              + min(0 , somme(i=V,C,1..4:TSNi))) * (1 - positif(APPLI_COLBERT)) ;
TSN = (1 - positif(DEFTS)) * max(0 , somme(i=V,C,1..4:TPRi) - (PRN - PRN1AI - PRNRAI - PRN1BI - PRNRBI - PRNRCK - PRN1CI - PRN1DI - PRN1EI - PRN1FI)) * (1 - positif(APPLI_COLBERT)) ;

regle 222060:
application :  iliad ;


REVDECTAX = (
   TSHALLOV
 + ALLOV
 + TSHALLOC
 + ALLOC
 + TSHALLO1
 + ALLO1
 + TSHALLO2
 + ALLO2
 + TSHALLO3
 + ALLO3
 + TSHALLO4
 + ALLO4
 + PALIV
 + PALIC
 + PALI1
 + PALI2
 + PALI3
 + PALI4
 + PRBRV
 + PRBRC 
 + PRBR1 
 + PRBR2 
 + PRBR3 
 + PRBR4 
 + RVB1 
 + RVB2 
 + RVB3 
 + RVB4 
 + GLDGRATV 
 + GLDGRATC 

 + REGPRIV
 + BICREP
 + RCMABD
 + RCMTNC 
 + RCMAV 
 + RCMHAD
 + RCMHAB
 + PPLIB
 + RCMLIB
 + COD2RA
 + BPV40V
 + BPVRCM
 - DPVRCM
 + BPCOPTV
 + BPCOSAV 
 + BPCOSAC 
 + RFORDI
 - RFDORD
 - RFDHIS
 - RFDANT
 + RFMIC 
 + BNCPRO1AV  
 + BNCPRO1AC  
 + BNCPRO1AP  
 + BACREV 
 + BACREC 
 + BACREP 
 + BAHREV 
 + BAHREC 
 + BAHREP 
 - BACDEV
 - BACDEC
 - BACDEP
 - BAHDEV
 - BAHDEC
 - BAHDEP
 - DAGRI6
 - DAGRI5
 - DAGRI4
 - DAGRI3
 - DAGRI2
 - DAGRI1
 + BICNOV 
 + BICNOC
 + BICNOP
 + BIHNOV 
 + BIHNOC 
 + BIHNOP 
 - BICDNV 
 - BICDNC
 - BICDNP
 - BIHDNV
 - BIHDNC
 - BIHDNP
 + BICREV 
 + BICREC 
 + BICHREV 
 + BICHREC 
 + BICHREP 
 - BICDEV
 - BICDEC
 - BICDEP
 - BICHDEV
 - BICHDEC
 - BICHDEP
 + BNCREV 
 + BNCREC 
 + BNCREP 
 + BNHREV 
 + BNHREC 
 + BNHREP 
 - BNCDEV
 - BNCDEC
 - BNCDEP
 - BNHDEV
 - BNHDEC
 - BNHDEP
 + ANOCEP 
 - DNOCEP
 + BAFPVV 
 + BAFPVC 
 + BAFPVP 
 + BAF1AV 
 + BAF1AC 
 + BAF1AP 
 + MIBVENV 
 + MIBVENC 
 + MIBVENP 
 + MIBPRESV 
 + MIBPRESC 
 + MIBPRESP 
 + MIBPVV 
 + MIBPVC 
 + MIBPVP 
 - BICPMVCTV
 - BICPMVCTC
 - BICPMVCTP
 + MIBNPVENV 
 + MIBNPVENC 
 + MIBNPVENP 
 + MIBNPPRESV 
 + MIBNPPRESC 
 + MIBNPPRESP 
 + MIBNPPVV 
 + MIBNPPVC 
 + MIBNPPVP 
 - MIBNPDCT
 - DEFBIC6
 - DEFBIC5
 - DEFBIC4
 - DEFBIC3
 - DEFBIC2
 - DEFBIC1
 + BNCPROV 
 + BNCPROC 
 + BNCPROP 
 + BNCPROPVV 
 + BNCPROPVC 
 + BNCPROPVP 
 - BNCPMVCTV
 + BNCNPV 
 + BNCNPC 
 + BNCNPP 
 + BNCNPPVV 
 + BNCNPPVC 
 + BNCNPPVP 
 + PVINVE
 - BNCNPDCT
 + BA1AV 
 + BA1AC 
 + BA1AP 
 + BI1AV 
 + BI1AC 
 + BI1AP 
 + MIB1AV 
 + MIB1AC 
 + MIB1AP 
 - MIBDEV 
 - MIBDEC 
 - MIBDEP 
 + BI2AV 
 + BI2AC 
 + BI2AP 
 + MIBNP1AV 
 + MIBNP1AC 
 + MIBNP1AP 
 - MIBNPDEV 
 - MIBNPDEC 
 - MIBNPDEP 
 - BNCPRODEV 
 - BNCPRODEC 
 - BNCPRODEP 
 + BN1AV 
 + BN1AC 
 + BN1AP 
 + BNCNP1AV 
 + BNCNP1AC 
 + BNCNP1AP 
 - BNCNPDEV 
 - BNCNPDEC 
 - BNCNPDEP) * (1-positif(APPLI_COLBERT+APPLI_OCEANS));

REVDECEXO =(
  BAEXV 
 + BAEXC 
 + BAEXP 
 + BAHEXV 
 + BAHEXC 
 + BAHEXP 
 + MIBEXV 
 + MIBEXC 
 + MIBEXP 
 + BICEXV 
 + BICEXC 
 + BICEXP 
 + BIHEXV 
 + BIHEXC 
 + BIHEXP 
 + MIBNPEXV 
 + MIBNPEXC 
 + MIBNPEXP 
 + BICNPEXV 
 + BICNPEXC 
 + BICNPEXP 
 + BICNPHEXV 
 + BICNPHEXC 
 + BICNPHEXP 
 + BNCPROEXV 
 + BNCPROEXC 
 + BNCPROEXP 
 + BNCEXV 
 + BNCEXC 
 + BNCEXP 
 + BNHEXV 
 + BNHEXC 
 + BNHEXP) * (1-positif(APPLI_COLBERT+APPLI_OCEANS));

regle 222070:
application :  iliad ;

AGRIV = (BAPERPV + BANOCGAV) * (1-positif(APPLI_OCEANS)) ; 
AGRIC = (BAPERPC + BANOCGAC) * (1-positif(APPLI_OCEANS)) ;  
AGRIP = (BAPERPP + BANOCGAP) * (1-positif(APPLI_OCEANS)) ; 

regle 222080:
application :  iliad ;


XBATOT = somme (i=V,C,P: XBAi) ;

XBICPRO = somme (i=V,C,P: XBIPi) ;
XBICNPRO = somme (i=V,C,P: XBINPi) ;

XBIMN = somme (i=V,C,P: MIBEXi + MIBNPEXi) ;
XBICMPRO = somme (i=V,C,P: MIBEXi) ;
XBICMNPRO = somme (i=V,C,P: MIBNPEXi) ;

XBNCMPRO = somme (i=V,C,P: BNCPROEXi) ;
XBNCMNPRO = somme (i=V,C,P: XSPENPi) ;
XBNCPRO = somme (i=V,C,P: XBNi) ;
XBNCNPRO = somme (i=V,C,P: XBNNPi) ;

XTSNN = somme (i=V,C: XTSNNi) ;
DEFBA = DEFBA1 + DEFBA2 + DEFBA3 + DEFBA4 + DEFBA5 + DEFBA6 ; 
BNCDF = BNCDF1 + BNCDF2 + BNCDF3 + BNCDF4 + BNCDF5 + BNCDF6 ;
DLMRNT = DLMRN1 + DLMRN2 + DLMRN3 + DLMRN4 + DLMRN5 + DLMRN6 ;
DEFLOC = DEFLOC1 + DEFLOC2 + DEFLOC3 + DEFLOC4 + DEFLOC5 + DEFLOC6 + DEFLOC7+ DEFLOC8+ DEFLOC9+ DEFLOC10;
AGRI = somme(i=V,C,P : AGRIi) ;
JEUNART = somme(i=V,C,P : BNCCREAi) ;

regle 222100:
application :  iliad ;

MIBDREPV =(     (MIBDEV - MIB1AV ) * positif(MIBDEV - MIB1AV) 
              - (MIBNP1AV - MIBNPDEV) * positif(MIBNP1AV - MIBNPDEV) 
          )
         *( positif( (MIBDEV - MIB1AV ) * positif(MIBDEV - MIB1AV)
                      - (MIBNP1AV - MIBNPDEV) * positif(MIBNP1AV - MIBNPDEV)
                    )
          );
MIBDREPC =(     (MIBDEC - MIB1AC ) * positif(MIBDEC - MIB1AC) 
              - (MIBNP1AC - MIBNPDEC) * positif(MIBNP1AC - MIBNPDEC) 
          )
         *( positif( (MIBDEC - MIB1AC ) * positif(MIBDEC - MIB1AC)
                      - (MIBNP1AC - MIBNPDEC) * positif(MIBNP1AC - MIBNPDEC)
                    )
          );
MIBDREPP =(     (MIBDEP - MIB1AP ) * positif(MIBDEP - MIB1AP) 
              - (MIBNP1AP - MIBNPDEP) * positif(MIBNP1AP - MIBNPDEP) 
          )
         *( positif( (MIBDEP - MIB1AP ) * positif(MIBDEP - MIB1AP)
                      - (MIBNP1AP - MIBNPDEP) * positif(MIBNP1AP - MIBNPDEP)
                    )
          );
MIBDREPNPV =(  (MIBNPDEV -MIBNP1AV )*positif(MIBNPDEV - MIBNP1AV) 
             - (MIB1AV-MIBDEV)*positif(MIB1AV-MIBDEV) 
            )
           *(positif( (MIBNPDEV -MIBNP1AV )*positif(MIBNPDEV - MIBNP1AV) 
                       - (MIB1AV-MIBDEV)*positif(MIB1AV-MIBDEV) 
                    )
            );
MIBDREPNPC =(  (MIBNPDEC -MIBNP1AC )*positif(MIBNPDEC - MIBNP1AC) 
             - (MIB1AC-MIBDEC)*positif(MIB1AC-MIBDEC) 
            )
           *(positif( (MIBNPDEC -MIBNP1AC )*positif(MIBNPDEC - MIBNP1AC) 
                       - (MIB1AC-MIBDEC)*positif(MIB1AC-MIBDEC) 
                    )
            );
MIBDREPNPP =(  (MIBNPDEP -MIBNP1AP )*positif(MIBNPDEP - MIBNP1AP) 
             - (MIB1AP-MIBDEP)*positif(MIB1AP-MIBDEP) 
            )
           *(positif( (MIBNPDEP -MIBNP1AP )*positif(MIBNPDEP - MIBNP1AP) 
                       - (MIB1AP-MIBDEP)*positif(MIB1AP-MIBDEP) 
                    )
            );

SPEDREPV = (     (BNCPRODEV - BNCPRO1AV) * positif(BNCPRODEV - BNCPRO1AV)
              -  (BNCNP1AV - BNCNPDEV)   * positif (BNCNP1AV - BNCNPDEV)
           )
          *( positif((BNCPRODEV - BNCPRO1AV) * positif(BNCPRODEV - BNCPRO1AV)
                       -(BNCNP1AV - BNCNPDEV)   * positif (BNCNP1AV - BNCNPDEV)
                     )
           );
SPEDREPC = (     (BNCPRODEC - BNCPRO1AC) * positif(BNCPRODEC - BNCPRO1AC)
              -  (BNCNP1AC - BNCNPDEC)   * positif (BNCNP1AC - BNCNPDEC)
           )
          *( positif((BNCPRODEC - BNCPRO1AC) * positif(BNCPRODEC - BNCPRO1AC)
                       -(BNCNP1AC - BNCNPDEC)   * positif (BNCNP1AC - BNCNPDEC)
                     )
           );
SPEDREPP = (     (BNCPRODEP - BNCPRO1AP) * positif(BNCPRODEP - BNCPRO1AP)
              -  (BNCNP1AP - BNCNPDEP)   * positif (BNCNP1AP - BNCNPDEP)
           )
          *( positif((BNCPRODEP - BNCPRO1AP) * positif(BNCPRODEP - BNCPRO1AP)
                       -(BNCNP1AP - BNCNPDEP)   * positif (BNCNP1AP - BNCNPDEP)
                     )
           );


SPEDREPNPV = ( (BNCNPDEV -BNCNP1AV )*positif(BNCNPDEV - BNCNP1AV) 
              -(BNCPRO1AV-BNCPRODEV)*positif(BNCPRO1AV-BNCPRODEV) 
             )
             *( positif( (BNCNPDEV -BNCNP1AV )*positif(BNCNPDEV - BNCNP1AV) 
                          -(BNCPRO1AV-BNCPRODEV)*positif(BNCPRO1AV-BNCPRODEV) 
                       )
              );
SPEDREPNPC = ( (BNCNPDEC -BNCNP1AC )*positif(BNCNPDEC - BNCNP1AC) 
              -(BNCPRO1AC-BNCPRODEC)*positif(BNCPRO1AC-BNCPRODEC) 
             )
             *( positif( (BNCNPDEC -BNCNP1AC )*positif(BNCNPDEC - BNCNP1AC) 
                          -(BNCPRO1AC-BNCPRODEC)*positif(BNCPRO1AC-BNCPRODEC) 
                       )
              );
SPEDREPNPP = ( (BNCNPDEP -BNCNP1AP )*positif(BNCNPDEP - BNCNP1AP) 
              -(BNCPRO1AP-BNCPRODEP)*positif(BNCPRO1AP-BNCPRODEP) 
             )
             *( positif( (BNCNPDEP -BNCNP1AP )*positif(BNCNPDEP - BNCNP1AP) 
                          -(BNCPRO1AP-BNCPRODEP)*positif(BNCPRO1AP-BNCPRODEP) 
                       )
              );

regle 222110:
application :  iliad ;


R8ZT = min(RBG2 + TOTALQUO , V_8ZT) ;

RZRE = min(RBG2 + TOTALQUO - R8ZT , CODZRE) ;

RZRF = min(RBG2 + TOTALQUO - R8ZT - CODZRE , CODZRF) ;

regle 222120:
application :  iliad ;


TXMOYIMPC = arr(TXMOYIMPNUM/TXMOYIMPDEN*100)/100;

TXMOYIMP = max(0 , TXMOYIMPC)
           * positif(IRCUM + TAXACUM + PCAPCUM + HAUTREVCUM - RECUMIR + IRANT - NONMER)
           * positif(TX_BAR5 - TXMOYIMPC) ;

regle 222130:
application :  iliad ;


TXMOYIMPNUM = positif(IRCUM + IRANT + TAXACUM + PCAPCUM + HAUTREVCUM 
               - max(0 , -(IRCUM + TAXACUM + PCAPCUM + HAUTREVCUM - RECUMIR ))
               - PIR - PTAXA - PPCAP - PHAUTREV) * 
              (max(0 , (IRCUM + IRANT + TAXACUM + PCAPCUM + HAUTREVCUM 
               - max(0 , -(IRCUM + TAXACUM + PCAPCUM + HAUTREVCUM - RECUMIR ))
	        - PIR - PTAXA - PPCAP - PHAUTREV)
                    * positif_ou_nul((IRNET2 + TAXASSUR + IPCAPTAXT + IHAUTREVT + CHRPVIMP) - SEUIL_12) 
                 + (IRNET2 + TAXASSUR + IPCAPTAXT + IHAUTREVT + CHRPVIMP + IRANT)
                    * positif(SEUIL_12 - (IRNET2 + TAXASSUR + IPCAPTAXT + IHAUTREVT + CHRPVIMP)) 

                 + arr((RCMLIB+COD2RA) * TX_PREVLIB / 100) + COD2CK * (1-V_CNR) - IPREP-IPPRICORSE
                   )) * positif_ou_nul(IAMD1 - SEUIL_61) * 100;

regle 222140:
application :  iliad ;

TXMOYIMPDEN =  max(0,TXMOYIMPDEN1 - TXMOYIMPDEN2 + TXMOYIMPDEN3 
               + TXMOYIMPDEN4 + TXMOYIMPDEN5 + TXMOYIMPDEN6) ;

regle 222150:
application :  iliad ;

TXMOYIMPDEN1 =   somme (i=V,C,1,2,3,4: TSNTi) 
        + somme (i=V,C,1,2,3,4: PRBi) + COD1AI + COD1BI + COD1CI + COD1DI + COD1EI + COD1FI 
        + RVTOT + T2RV 
        + max(0 , RCMABD + RCMTNC + RCMAV + RCMHAD + RCMHAB + REGPRIV * MAJREV + COD2TT + COD2VV + COD2WW + COD2YY + COD2ZZ 
	          + COD2VN + COD2VO + COD2VP + COD2RB + COD2RC + COD2RD + COD2TQ + COD2TZ
                  + REVACT + REVPEA + PROVIE + DISQUO + INTERE + CODRYY + RESTUC * MAJREV * positif(COD2OP) + RESTUC * (1 - positif(COD2OP))
	          - min(RCMORDTOTNET + RCMQUOTOTNET , RCMFR + DEFRCM + DEFRCM2 + DEFRCM3 + DEFRCM4 + DEFRCM5 + DEFRCM6) * positif(COD2OP)) * (1 - V_CNR)
        + RMFN
        + (RFCG + DRCF)
        + max(0,NPLOCNETF)     
	  + max(0,BANOR) + min(0,BANOR) * positif(SEUIL_IMPDEFBA + 1 - SHBA - (REVTP-BA1) - REVQTOTQHT)
          + max(0,BATMARGTOT)+BAQTOTAVIS
          + BNCPHQF + max(0,BNCNPHQF) + BNCPQF + max(0,BNCNPQF)
	  + BICPROOF + max(0,BICNPF) + BICPROQF + max(0,BICNPQTF)
         + REPSOF
         + (BTPM3VG + BTPM3UA + PVTAXSB+COD3SZ+ BTPM3TJ) * (1-positif(present(TAX1649)+present(RE168)))
         + (max(0,CODRUA - CODRVA-CODRSL)+max(0,CODRVG-CODRSG)) * positif(COD2OP) * (1-positif(present(TAX1649)+present(RE168)))
         + COD1TZ + GAINPEA * positif(COD2OP)
                ;
TXMOYIMPDEN2 =  somme (i=0,1,2,3,4,5: DEFAAi ) * (1-positif(RNIDF))
         + RDCSG
         + DDPA
         + APERPV + APERPC + APERPP
         + DRFRP  * positif(RRFI);
TXMOYIMPDEN3 = somme(i=V,C,P: BN1Ai  + BI1Ai + BI2Ai + max(0 , BA1i)) 
               + MIB_1AF + SPEPV + PVINVE + PVINCE + PVINPE + INVENTV + INVENTC + INVENTP
	       + COD5QJ + COD5RJ + COD5SJ + COD5HA + COD5IA + COD5TF + COD5UF + COD5VF 
	       + COD5UI + COD5VI + COD5WI + COD5QA + COD5RA + COD5SA
               + (BPTPVT + BPTPSJ + BPTPWI + BPTPWJ + BPTP4 + BPV40V + BPV18V + BPTPPI + COD3AN)* (1 - positif(present(TAX1649) + present(RE168)))
               + COD3WP ;
TXMOYIMPDEN4 = 2PRBV + 2PRBC + 2PRB1 + 2PRB2 + 2PRB3 + 2PRB4 + max(0,BAQTOT) * (1-positif(DEFBA6+DEFBA5+DEFBA4+DEFBA3+DEFBA2+DEFBA1))
							     + somme(i=V,C,1..4:PEBFi)
							     + RMFNQ + CODRAI + CODRBI + CODRCK 
	       ;
TXMOYIMPDEN5 = RCMLIB + COD2RA ;
TXMOYIMPDEN6 = CESSASSV + CESSASSC + BPCAPTAXV + BPCAPTAXC + BPCAPTAXP ;

regle 222160:
application : iliad  ;


GGIRSEUL =  IAD11 + ITP + REI + AVFISCOPTER ;

regle 222170:
application : iliad  ;


GGIDRS =  IDOM11 + ITP + REI + PIR ;

regle 222180:
application : iliad  ;


GGIAIMP =  IAD11 ;

regle 222190:
application : iliad  ;


GGINET = si ( positif(RE168+TAX1649+0) = 0)
      alors
       (si    ( V_REGCO = 2 )
        alors (GGIAIMP - 0 + EPAV + CICA + CIGE )
        sinon (max(0,GGIAIMP - CIRCMAVFT + EPAV + CICA + CIGE ))
        finsi)
       sinon (max(0,GGIAIMP - CIRCMAVFT))
       finsi;

regle 222200:
application : iliad  ;




SEUILCIRIRFN1 = positif(V_BTNBP1)*(arr(
              (12264 + (3275 * ((V_BTNBP1/100) - 1) * 2 )
              ) * (1-null(V_REGCO - 5)) * (1-null(V_REGCO - 6))
            + (14510 + (3602 * ( min((V_BTNBP1/100) , 1.5) - 1) * 2)
            + (3275 * ( max(0 , (V_BTNBP1/100) - 1.5)) * 2)
              ) * null(V_REGCO - 5)
            + (15173 + (3766 * ( min((V_BTNBP1/100) , 1.5) - 1) * 2)
            + (3275 * ( max(0 , (V_BTNBP1/100) - 1.5)) * 2)
              ) * null(V_REGCO - 6)
                )) ;															                  

SEUILCIRIRF = arr( 
              (12264 + (3275 * (NBPT - 1) * 2 )
              ) * (1-null(V_REGCO - 5)) * (1-null(V_REGCO - 6))
            + (14510 + (3602 * ( min(NBPT , 1.5) - 1) * 2)
                     + (3275 * ( max(0 , NBPT - 1.5)) * 2)
              ) * null(V_REGCO - 5)
            + (15173 + (3766 * ( min(NBPT , 1.5) - 1) * 2)
            + (3275 * ( max(0 , NBPT - 1.5)) * 2)
              ) * null(V_REGCO - 6) 
                ) ;


CIRIRF = null( (1-null( IND_TDR)) +  positif_ou_nul( SEUILCIRIRF - REVKIRE ) - 2)
         + 2 * (1 - null( (1-null( IND_TDR)) +  positif_ou_nul( SEUILCIRIRF - REVKIRE ) - 2)); 

regle 222210:
application : iliad  ;


SEUILCIIMSIN1 =positif(V_BTNBP1)* (arr(
              (16033 + (4281 * ((V_BTNBP1/100) - 1) * 2 )
              ) * (1-null(V_REGCO - 5)) * (1-null(V_REGCO - 6))
            + (17541 + (4707 * ( min((V_BTNBP1/100) , 1.5) - 1) * 2)
            + (4281 * ( max(0 , (V_BTNBP1/100) - 1.5)) * 2)
              ) * null(V_REGCO - 5)
            + (18373 + (4922 * ( min((V_BTNBP1/100) , 1.5) - 1) * 2)
            + (4281 * ( max(0 , (V_BTNBP1/100) - 1.5)) * 2)
            ) * null(V_REGCO - 6)
                 )) ;

SEUILCIIMSI = arr(
              (16033 + (4281 * (NBPT - 1) * 2 )
              ) * (1-null(V_REGCO - 5)) * (1-null(V_REGCO - 6))
            + (17541 + (4707 * ( min(NBPT , 1.5) - 1) * 2)
            + (4281 * ( max(0 , NBPT - 1.5)) * 2)
              ) * null(V_REGCO - 5)
            + (18373 + (4922 * ( min(NBPT , 1.5) - 1) * 2)
            + (4281 * ( max(0 , NBPT - 1.5)) * 2)
              ) * null(V_REGCO - 6)
                ) ;


regle 222211:
application : iliad  ;


SEUILCIIMSIN11 = positif(V_BTNBP1)*(arr(
              (24884 + (6642 * ((V_BTNBP1/100) - 1) * 2 )
              ) * (1-null(V_REGCO - 5)) * (1-null(V_REGCO - 6))
           + (24884 + (6642 * ( (V_BTNBP1/100)  - 1) * 2)
             ) * null(V_REGCO - 5)
           + (24884 + (6642 * ( (V_BTNBP1/100)  - 1) * 2)
             ) * null(V_REGCO - 6)
               )) ;

SEUILCIIMSI1 = arr(
              (24884 + (6642 * (NBPT - 1) * 2 )
              ) * (1-null(V_REGCO - 5)) * (1-null(V_REGCO - 6))
             + (24884 + (6642 * ( NBPT  - 1) * 2)
             ) * null(V_REGCO - 5)
            + (24884 + (6642 * ( NBPT  - 1) * 2)
            ) * null(V_REGCO - 6)
            ) ;


regle 222212:
application : iliad  ;


IND8ZK =   0*(1-present(IND_TDR))
           + 1* null(IND_TDR)
           + 2* positif(IND_TDR);






CIIMSI1 = 2*(positif_ou_nul (SEUILCIIMSI -REVKIRE)*positif((1-present(IND_TDR)) + positif(IND_TDR)))
         +3*(positif(REVKIRE-SEUILCIIMSI) * positif(SEUILCIIMSI1-REVKIRE)*positif((1-present(IND_TDR)) + positif(IND_TDR)))
         +4*positif((positif_ou_nul(REVKIRE-SEUILCIIMSI1) + (null(IND_TDR))));



CIIMSI2 =  1*null(V_BTNBP1/100) * null(V_BTRFRN1)
         +2*positif_ou_nul (SEUILCIIMSIN1 -V_BTRFRN1)*positif(V_BTRFRN1)* ( 1-null(1- V_BT8ZKN1))
	 +3*positif(V_BTRFRN1-SEUILCIIMSIN1)*positif(V_BTRFRN1)*positif(SEUILCIIMSIN11-V_BTRFRN1)* ( 1-null(1- V_BT8ZKN1))
	          +4*positif(positif(V_BTRFRN1)*positif_ou_nul(V_BTRFRN1-SEUILCIIMSIN11) + (null(1-V_BT8ZKN1))); 


	 

CIIMSI = 2*positif(positif(null(3-CIIMSI1)+ null(4-CIIMSI1)) * null(2-CIIMSI2) * positif((1-present(IND_TDR)) + positif(IND_TDR)))
         +(1-positif(positif(null(3-CIIMSI1)+ null(4-CIIMSI1))* null(2-CIIMSI2)*positif((1-present(IND_TDR)) + positif(IND_TDR))))*CIIMSI1; 


CIIMSI21 = 1*(null(V_BTNBP1/100)*positif(null(V_BTRFRN1)+(1-present(V_BTRFRN1)))*positif(null(V_BT8ZKN1) + null(2-V_BT8ZKN1)));
CIIMSI22 = 2*(positif_ou_nul (SEUILCIIMSIN1 -V_BTRFRN1)*positif(V_BTRFRN1)*positif(null( V_BT8ZKN1) + null(2-V_BT8ZKN1)));
CIIMSI23 =3*(positif(V_BTRFRN1-SEUILCIIMSIN1)*present(V_BTRFRN1)*positif(SEUILCIIMSIN11-V_BTRFRN1)*positif(null( V_BT8ZKN1) + null(2-V_BT8ZKN1)));

CIIMSI24 =4*present(V_BTRFRN1)*positif((positif_ou_nul(V_BTRFRN1-SEUILCIIMSIN11) + (null(1-V_BT8ZKN1))));

regle 222220:
application : iliad  ;

REPCT = (min(0,MIB_NETNPCT) * positif(MIBNPDCT+COD5RZ+COD5SZ) * positif(DLMRN1)
	+ min(0,SPENETNPCT) * positif(BNCNPDCT) * positif(BNCDF1)) * (-1);

regle 222230:
application : iliad  ;

RBGTH = 
   TSHALLOV + TSHALLOC + TSHALLO1 + TSHALLO2 + TSHALLO3 + TSHALLO4  
 + ALLOV + ALLOC + ALLO1 + ALLO2 + ALLO3 + ALLO4  
 + SALEXTV + SALEXTC + SALEXT1 + SALEXT2 + SALEXT3 + SALEXT4 
 + TSASSUV + TSASSUC + XETRANV + XETRANC + IPMOND
 + PRBRV + PRBRC + PRBR1 + PRBR2 + PRBR3 + PRBR4  
 + COD1AH + COD1BH + COD1CH + COD1DH + COD1EH + COD1FH 
 + PCAPTAXV + PCAPTAXC + COD1CT + COD1DT + COD1ET + COD1FT
 + PALIV + PALIC + PALI1 + PALI2 + PALI3 + PALI4
 + RVB1 + RVB2 + RVB3 + RVB4  
 + GLDGRATV  
 + GLDGRATC  
 + PENINV + PENINC + PENIN1 + PENIN2 + PENIN3 + PENIN4
 + RCMABD + RCMTNC + RCMAV + RCMHAD + REGPRIV  
 + RCMHAB + PPLIB + RCMIMPAT + RCMLIB + COD2RA
 + BPV40V
 + BPVRCM  
 + BPCOPTV  
 + BPCOSAV  
 + BPCOSAC  
 + GAINABDET  
 + BPV18V  
 + ABIMPPV
 + BPVSJ
 + BPVSK
 + GAINPEA
 + PVSURSI
 + PVIMPOS
 + PVIMMO
 + ABDETPLUS
 + PVEXOSEC
 + PVREPORT
 + COD3SL
 + COD3UA
 + RFMIC  
 + RFORDI  
 + BAFPVV + BAFPVC + BAFPVP + BAF1AV + BAF1AC + BAF1AP  
 + BAEXV + BAEXC + BAEXP + BACREV + BACREC + BACREP  
 + BA1AV  
 + BA1AC  
 + BA1AP  
 + BAHEXV + BAHEXC + BAHEXP + BAHREV + BAHREC + BAHREP  
 + BAFORESTV  
 + BAFORESTC  
 + BAFORESTP  
 + BAPERPV + BANOCGAV + BAPERPC + BANOCGAC + BAPERPP + BANOCGAP
 + MIBEXV  
 + MIBEXC  
 + MIBEXP  
 + MIBVENV  
 + MIBVENC  
 + MIBVENP  
 + MIBPRESV  
 + MIBPRESC  
 + MIBPRESP  
 + MIBPVV  
 + MIBPVC  
 + MIBPVP  
 + MIB1AV  
 + MIB1AC  
 + MIB1AP  
 + BICEXV  
 + BICEXC  
 + BICEXP  
 + BICNOV  
 + BICNOC  
 + BICNOP  
 + BI1AV  
 + BI1AC  
 + BI1AP  
 + BIHEXV  
 + BIHEXC  
 + BIHEXP  
 + BIHNOV  
 + BIHNOC  
 + BIHNOP  
 + MIBNPEXV  
 + MIBNPEXC  
 + MIBNPEXP  
 + MIBNPVENV  
 + MIBNPVENC  
 + MIBNPVENP  
 + MIBNPPRESV  
 + MIBNPPRESC  
 + MIBNPPRESP  
 + MIBNPPVV  
 + MIBNPPVC  
 + MIBNPPVP  
 + MIBNP1AV  
 + MIBNP1AC  
 + MIBNP1AP  
 + BICNPEXV  
 + BICNPEXC  
 + BICNPEXP  
 + BICREV  
 + BICREC  
 + BICREP  
 + BI2AV  
 + BI2AC  
 + BI2AP  
 + BICNPHEXV  
 + BICNPHEXC  
 + BICNPHEXP  
 + BICHREV  
 + BICHREC  
 + BICHREP  
 + LOCNPCGAV
 + LOCNPV
 + LOCNPCGAC
 + LOCNPC
 + LOCNPCGAPAC
 + LOCNPPAC
 + MIBMEUV
 + MIBMEUC
 + MIBMEUP
 + MIBGITEV
 + MIBGITEC
 + MIBGITEP
 + LOCGITCV
 + LOCGITHCV
 + LOCGITCC
 + LOCGITHCC
 + LOCGITCP
 + LOCGITHCP
 + LOCGITV
 + LOCGITC
 + LOCGITP
 + AUTOBICVV
 + AUTOBICPV
 + AUTOBICVC
 + AUTOBICPC
 + AUTOBICVP
 + AUTOBICPP
 + BNCPROEXV  
 + BNCPROEXC  
 + BNCPROC  
 + BNCPROP  
 + BNCPROPVV  
 + BNCPROPVC  
 + BNCPROPVP  
 + BNCPRO1AV  
 + BNCPRO1AC  
 + BNCPRO1AP  
 + BNCEXV  
 + BNCEXC  
 + BNCEXP  
 + BNCREV  
 + BNCREC  
 + BNCREP  
 + BN1AV  
 + BN1AC  
 + BN1AP  
 + BNHEXV  
 + BNHEXC  
 + BNHEXP  
 + BNHREV  
 + BNHREC  
 + BNHREP  
 + BNCCRV  
 + BNCCRC  
 + BNCCRP  
 + BNCNPV  
 + BNCNPC  
 + BNCNPP  
 + BNCNPPVV  
 + BNCNPPVC  
 + BNCNPPVP  
 + BNCNP1AV  
 + BNCNP1AC  
 + BNCNP1AP  
 + ANOCEP  
 + PVINVE  
 + BNCCRFV  
 + ANOVEP  
 + PVINCE  
 + BNCCRFC  
 + ANOPEP  
 + PVINPE  
 + BNCCRFP  
 + BNCAABV  
 + BNCAABC  
 + BNCAABP  
 + BNCNPREXAAV  
 + BNCNPREXV  
 + BNCNPREXAAC  
 + BNCNPREXC  
 + BNCNPREXAAP  
 + BNCNPREXP  
 + BNCPROEXP
 + BNCPROV
 + CESSASSV
 + CESSASSC
 + INVENTV
 + INVENTC
 + INVENTP
 + AUTOBNCV
 + AUTOBNCC
 + AUTOBNCP
 + XSPENPV
 + XSPENPC
 + XSPENPP
 + REPSOF
 # Ajout 2016
 + COD5XA  
 + COD5XB  
 + COD5YA  
 + COD5YB  
 + COD5ZA  
 + COD5ZB  
 + COD5UR  
 + COD5US  
 + COD5UT  
 + COD5UU  
 + COD5UY  
 + COD5UZ  
 + COD5VR  
 + COD5VS  
 + COD5VT  
 + COD5VU  
 + COD5VY  
 + COD5VZ  
 + COD5WR  
 + COD5WS  
 + COD5VM  
 + COD5VN  
 + COD5WM  
 + COD5WN  
 + COD5ZJ  
 + COD5ZK  
 + COD5ZS  
 + COD5ZX  
 + COD5XH  
 + COD5XJ  
 + COD5XK  
 + COD5XL  
 + COD5XP  
 + COD5XQ  
 + COD5XS  
 + COD5XX  
 + COD5XY  
 + COD5XZ  
 + COD5YH + COD5YJ + COD5YK + COD5YL + COD5YP + COD5YQ + COD5YS + COD5YX + COD5YY + COD5YZ  
 + COD1GG + COD1HG + COD1IG + COD1JG + COD1KG + COD1LG + COD1GH + COD1HH + COD1IH 
 + COD1JH + COD1KH + COD1LH + COD1AI + COD1BI + COD1CI + COD1DI + COD1EI + COD1FI
 + COD1WZ
 + COD2RA + COD2RB + COD2RC + COD2RD + COD2TQ + COD3AN 
 + COD5HA + COD5IA + COD5JA + COD5UI + COD5VI + COD5WI + COD5TF + COD5UF + COD5VF 
 + COD5QA + COD5RA + COD5SA + COD5QJ + COD5RJ + COD5SJ 
 ;
regle 222240:
application : iliad  ;


XETRAN = XETSNNV + XETSNNC ;

regle 222250:
application : iliad ;


TLIR  = TL_IR * positif(APPLI_OCEANS) ;
TLTAXAGA = TL_TAXAGA * positif(APPLI_OCEANS) ;

regle 222270:
application : iliad  ;


VARPS = max(0 , PRELCS - (V_PRELCSANT - V_PRELCSNANT))
        + max(0,NAPRD - V_RDANT)
        + max(0 , PRELPSOL - (V_PRELPSOLANT - V_PRELPSOLNANT))
        + max(0 , NAPCVN - V_CVNANT)
        + max(0 , NAPCDIS - V_CDISANT)
        + max(0 , NAPCSG820 - V_CSG820ANT)
        + max(0 , NAPGLOA - V_GLOANT)
        + max(0 , NAPRSE1 - V_RSE1ANT)
        + max(0 , NAPRSE2 - V_RSE2ANT)
        + max(0 , NAPRSE3 - V_RSE3ANT)
        + max(0 , NAPRSE4 - V_RSE4ANT)
        + max(0 , NAPRSE5 - V_RSE5ANT) 
        + max(0 , NAPRSE6 - V_RSE6ANT) 
        + max(0 , NAPRSE8 - V_RSE8ANT) ;

regle 222280:
application : iliad  ;


COMPENSACI = min(abs(max(NAPTIRNET2 , NAPTIRNET)) , PREVSOCNET) * (1 - positif(NAPTIRNET)) * (1 - positif(NAPTIRNET2)) * positif(PREVSOCNET)
              * positif(20 - V_NOTRAIT) 
             + max(0 , min(min(0 , IRCOMPANT) - min(0 , IRCOMP) , min(0 , IRCOMPANT + V_ACPASIRANT) - min(0 , IRCOMP + max(0 , ACPASIR)))) * positif(V_NOTRAIT - 20) 
	       * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) ;

COMPENSTOT = max(0 , min(0 , TOTIRPSANT - TOTCRA) - min(0 , TOTIRPS - NAPCR61 + NONMER)) * positif(V_NOTRAIT - 20) 
             * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) ;

regle 222284:
application : iliad  ;

COMPANVPS = ((PREVSOCNET + NAPTIRNET) * positif(PREVSOCNET - NAPTIRNET) * (1 - positif_ou_nul(NAPTIRNET)) * (1 - null(2 - IND12) - null(IND12))
             + PREVSOCNET * positif_ou_nul(NAPTIRNET) * positif(PREVSOCNET) * null(1 - IND12)) 
	       * (positif(20 - V_NOTRAIT) + positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)))
            + max(0 , min(NONMER - V_NONMERANT , NONMERPS - V_NONMERPSANT)) * positif(V_NOTRAIT - 20) 
	      * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) ;

COMPANVIR = ((NAPTIRNET + PREVSOCNET) * positif(NAPTIRNET - PREVSOCNET) * (1 - positif_ou_nul(PREVSOCNET)) * (1 - null(2 - IND12) - null(IND12))
             + NAPTIRNET * positif(NAPTIRNET) * positif(PREVSOCNET) * null(1 - IND12)) 
	     * (positif(20 - V_NOTRAIT) + positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)))
            + (max(0 , NONMER - V_NONMERANT) - COMPANVPS) * positif(V_NOTRAIT - 20) 
	      * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) ;

COMPENSANV = (COMPANVIR + COMPANVPS) * (positif(20 - V_NOTRAIT) 
                                        + positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)))
             + max(0 , NONMER - V_NONMERANT) * positif(V_NOTRAIT - 20) 
	       * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) ;

regle 222286:
application : iliad  ;

COMPENSACD = min(abs(max(NAPTIRNET2 , NAPTIRNET)) , PREVSOCNET) * (1 - positif(NAPTIRNET)) * (1 - positif(NAPTIRNET2)) * positif(PREVSOCNET) ;

COMPANVIRD = (NAPTIRNET + PREVSOCNET) * positif(NAPTIRNET - PREVSOCNET) * (1 - positif_ou_nul(PREVSOCNET)) * (1 - null(2 - IND12) - null(IND12))
             + NAPTIRNET * positif(NAPTIRNET) * positif(PREVSOCNET) * null(1 - IND12) ;

COMPANVPSD = (PREVSOCNET + NAPTIRNET) * positif(PREVSOCNET - NAPTIRNET) * (1 - positif_ou_nul(NAPTIRNET)) * (1 - null(2 - IND12) - null(IND12)) 
             + PREVSOCNET * positif_ou_nul(NAPTIRNET) * positif(PREVSOCNET) * null(1 - IND12) ;

COMPENSAND = COMPANVIRD + COMPANVPSD ;

regle 222288:
application : iliad  ;

COMPENSIR = max(0 , max(0 , IRCOMPANT) - max(0 , IRCOMP)) * positif(V_NOTRAIT - 20) 
            * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) ;

COMPENSPS = (
             max(0 , max(0 , CSGCOMPANT) - max(0 , CSGCOMP))
             + max(0 , V_RDANT - NAPRD)
	     + max(0 , max(0 , PSOLCOMPANT) - max(0 , PSOLCOMP))
             + max(0 , V_CVNANT - NAPCVN)
             + max(0 , V_CDISANT - NAPCDIS)
             + max(0 , V_CSG820ANT - NAPCSG820)
             + max(0 , V_GLOANT - NAPGLOA)
             + max(0 , V_RSE1ANT - NAPRSE1)
             + max(0 , V_RSE2ANT - NAPRSE2)
             + max(0 , V_RSE3ANT - NAPRSE3)
             + max(0 , V_RSE4ANT - NAPRSE4)
             + max(0 , V_RSE5ANT - NAPRSE5)
             + max(0 , V_RSE6ANT - NAPRSE6)
             + max(0 , V_RSE8ANT - NAPRSE8)
	    ) * (1 - null( 2 - null(VARPS) - positif(1 - NATIMP))) * positif(V_NOTRAIT - 20) 
	    * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) ;

regle 222290:
application : iliad  ;

B1507INR = IRNIN_INR +TAXABASE +PCAPBASE +CHRBASE;
B1507MAJO1 = IRNIN * positif(NMAJ1)
	    + TAXASSUR * positif(NMAJTAXA1) 
	    + IPCAPTAXT * positif(NMAJPCAP1) 
	    + (IHAUTREVT + CHRPVIMP)* positif(NMAJCHR1) ;

B1507MAJO3 = IRNIN * positif(NMAJ3)
	    + TAXASSUR * positif(NMAJTAXA3) 
	    + IPCAPTAXT * positif(NMAJPCAP3) 
	    + (IHAUTREVT + CHRPVIMP)* positif(NMAJCHR3) ;

B1507MAJO4 = IRNIN * positif(NMAJ4)
	    + TAXASSUR * positif(NMAJTAXA4) 
	    + IPCAPTAXT * positif(NMAJPCAP4) 
	    + (IHAUTREVT + CHRPVIMP)* positif(NMAJCHR4) ;

regle 223010 :
application :  iliad ;

INDEFCAP = positif(present(DEFRCM)      + present(DEFRCM2)   + present(DEFRCM3)        + present(DEFRCM4)     + present(DEFRCM5)      + present(DEFRCM6) 
                   + present(RFDORD)    + present(RFDHIS)    + present(RFDANT)         + present(LNPRODEF10)  + present(LNPRODEF9)    + present(LNPRODEF8) 
		   + present(LNPRODEF7) + present(LNPRODEF6) + present(LNPRODEF5)      + present(LNPRODEF4)   + present(LNPRODEF3)    + present(LNPRODEF2) 
		   + present(LNPRODEF1) + present(BACDEV)    + present(BAHDEV)         + present(DABNCNP6)    + present(BACDEC)       + present(BAHDEC) 
		   + present(DABNCNP5)  + present(BACDEP)    + present(BNCAADV)        + present(BAHDEP)      + present(DABNCNP4)     + present(BICDNV) 
		   + present(BIHDNV)    + present(DABNCNP3)  + present(BICDNC)         + present(BIHDNC)      + present(DABNCNP2)     + present(BICDNP) 
		   + present(BIHDNP)    + present(DABNCNP1)  + present(BICDEV)         + present(BICHDEV)     + present(DNOCEPC)      + present(LOCDEFNPCGAV) 
		   + present(LOCDEFNPV) + present(BICDEC)    + present(BICHDEC)        + present(DNOCEPP)     + present(LOCDEFNPCGAC) + present(LOCDEFNPC) 
		   + present(BICDEP)    + present(BICHDEP)   + present(LOCDEFNPCGAPAC) + present(LOCDEFNPPAC) + present(BNCDEV)       + present(DAGRI6) 
		   + present(DAGRI5)    + present(BNHDEV)    + present(DAGRI4)         + present(DAGRI3)      + present(DAGRI2)       + present(DAGRI1) 
		   + present(BNCDEC)    + present(BNCAADC)   + present(BNHDEC)         + present(DEFBIC6)     + present(DEFBIC5)      + present(DEFBIC4) 
		   + present(DEFBIC3)   + present(DEFBIC2)   + present(DEFBIC1)        + present(BNCDEP)      + present(BNCAADP)      + present(BNHDEP) 
		   + present(DNOCEP)    + present(DEFAA5)    + present(DEFAA4)         + present(DEFAA3)      + present(DEFAA2)       + present(DEFAA1) 
		   + present(DEFAA0)    + present(COD8YJ)    + present(IPTEFN)         + present(DMOND) 
		   + present(COD5WE)    + present(COD5WF)    + present(COD5XE)         + present(COD5XF)      + present(COD5YE)       + present(COD5YF) + 0) ;

regle 224000 :
application :  iliad ;
THPVRO = (min(45,arr((V_IPVRO - IAD11 )/COD3WG*10000)/100)* positif(COD2OP) + positif(PVREPORT)*TX128 * positif(1-positif(COD2OP))) *null(FLAG_PVRO);

regle 224010 :

application : iliad;

BAIMP = BAHQT + BAQTOTAVIS - DEFIBA - DBAIP + BAFPVV + BAFPVC + BAFPVP + BAFORESTV + BAFORESTC + BAFORESTP + BAMICV + BAMICC + BAMICP;

IMPUTBA = positif(BAIMP)*min(COD5XO + COD5YO + COD5ZO , BAIMP)
 +(1-positif(BAIMP))*0;

 # fin regle=============================================================
regle 224015 :
application :  iliad ;
TOTIMPUT = I2DH + IAVF + CRDIE;
TOTDONS = RREPA + RDONS;
regle 224020 :
application : iliad;

INDIRRAS = positif(RASSALIR + RASACOIR + RASACOPS) * 1
         + (1-positif(RASSALIR + RASACOIR + RASACOPS)) * 0;
regle 224025 :
application : iliad;


TOTCHARDED  = RDCSG + CHTOT ; 


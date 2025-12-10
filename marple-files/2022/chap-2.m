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
 #     CHAPITRE 2. CALCUL DU NET A PAYER
 #
 #
 #
regle 201000:
application : iliad ;

NAPINI = ( IRN + PIR - IRANT )* (1 - INDTXMIN) *(1 - INDTXMOY)
       + min(0, IRN + PIR - IRANT) * (INDTXMIN + INDTXMOY)
       + max(0, IRN + PIR - IRANT) * 
                                (INDTXMIN*positif((IAVIMBIS-NAPCRPAVIM)-SEUIL_61 )
			       + INDTXMOY* positif((IAVIMO-NAPCRPAVIM)-SEUIL_61))
                      + BRASAR ;

RC1INI = positif( NAPINI + 1 - SEUIL_12 ) ;

regle 201010:
application : iliad ;

NAPTOT = IRCUM + TAXACUM + PCAPCUM + HAUTREVCUM  - RECUM;

NAPTOT = IRCUM + TAXACUM + PCAPCUM + HAUTREVCUM - RECUMIR ;

regle 201020:
application : iliad ;


NAPTOTA = V_IRPSANT - V_ANTRE ;
NAPTOTAIR = V_TOTIRANT - V_TOTIRNANT - V_ANTREIR ;
TOTCRA = V_ANTCR - V_NANTCR ;
TOTIRPSANT = V_TOTIRANT - V_TOTIRNANT - V_ANTREIR + V_ANTCR - V_NANTCR - V_NONMERANT + V_NONRESTANT ;

regle 201030:
application : iliad ;

OCEDIMP = IRNIN ;

regle 201040:
application : iliad ;

IRNIN = (IRN - IRANT+CODZRA) * positif(IRN - IRANT+CODZRA) ;

regle isf 201050:
application : iliad ;

IFI4BASE = IFI4BIS * positif_ou_nul(IFI4BIS - SEUIL_12) ;

IFIBASE_INR = IFI4BIS;
IFIIN = IFI4BASE;
regle 201060:
application : iliad ;

IRNIN_INR = max(0 , min(0 , IAN + AVFISCOPTER - IRE - CODCOA + CODZRA) 
                               + max(0 , IAN + AVFISCOPTER - IRE - CODCOA + CODZRA) 
                                       * positif(IAMD1 + 1 + V_ANTREIR - SEUIL_61) - IRANT -IR9YI+max(0,NIRNINBIS_A-NIRNINBIS)) 
               * positif(min(0 , IAN + AVFISCOPTER - IRE - CODCOA + CODZRA) 
	                       + max(0 , IAN + AVFISCOPTER - IRE - CODCOA + CODZRA) 
				                         * positif(IAMD1 + 1 + V_ANTREIR - SEUIL_61) - IRANT) ;
CSBASE_INR = max(0 , CSG - CSGIM - CODCOB)+max(0,NCSBASEBIS_A-NCSBASEBIS) ;
RDBASE_INR = max(0 , RDSN - CRDSIM - CODCOR)+max(0,NRDBASEBIS_A-NRDBASEBIS) ;
PSOLBASE_INR = max(0 , PSOL - PRSPROV - CODCOD)+max(0,NPSOLBASEBIS_A-NPSOLBASEBIS) ;
PSOLBASEMAJO_INR = max(0 , PSOL - PRSPROV - CODCOD) ;
CVNBASE_INR = max(0,CVNN - COD8YT  -CODCOE);
CDISBASE_INR = max(0,CDIS - CDISPROV  -CODCOF);
C820BASE_INR = max(0,MCSG820 - COD8ZH -CODCOQ);
GLOBASE_INR = max(0,CGLOA - COD8YL  -CODCOG);
RSE1BASE_INR = max(0,RSE1N - CSPROVYD -CODCOT);
RSE2BASE_INR = max(0, max(0, RSE8TV - CIRSE8TV - CSPROVYF) + max(0, RSE8SA -CIRSE8SA - CSPROVYN) -CODCOL);
RSE3BASE_INR = max(0,RSE3N - CSPROVYG -CODCOM);
RSE4BASE_INR = max(0, RSE4N  - CSPROVYH -CODCOO);
RSE5BASE_INR = max(0,RSE5N - CSPROVYE -CODCOJ);
RSE6BASE_INR = max(0,RSE6N - COD8YQ -CODCOP);
RSE8BASE_INR = max(0,RSE8N - COD8YV -COD8YX -CODCOH);
TAXABASE_INR = arr(max(TAXASSUR -CODCOU+ min(0,min( 0, IAN + AVFISCOPTER - IRE) + max( 0, IAN + AVFISCOPTER - IRE )  * positif( IAMD1 + 1 + V_ANTREIR - SEUIL_61)  
                                      - IRANT),0)) * positif(IAMD1 + 1 + V_ANTREIR - SEUIL_61);
PCAPBASE_INR = arr(max(IPCAPTAXT -CODCOV+ min(0,min( 0, IAN + AVFISCOPTER - IRE ) + max( 0, IAN + AVFISCOPTER - IRE )  * positif( IAMD1 + 1 + V_ANTREIR - SEUIL_61)  
                                      - IRANT + TAXASSUR),0)) * positif(IAMD1 + 1 + V_ANTREIR - SEUIL_61);
CHRBASE_INR = arr(max(IHAUTREVT +CHRPVIMP -CODCOX+ min(0,min( 0, IAN + AVFISCOPTER - IRE) + max( 0, IAN + AVFISCOPTER - IRE)  * positif( IAMD1 + 1 + V_ANTREIR - SEUIL_61)  
                                          - IRANT+TAXASSUR + IPCAPTAXT+ TAXASSUR+IPCAPTAXT),0)) * positif(IAMD1 + 1 + V_ANTREIR - SEUIL_61) ;

NIRNINBIS = abs(min(0,IAN + AVFISCOPTER - IRE-CODCOA+CODZRA))*positif_ou_nul(CODCOA);
NCSBASEBIS = abs(min(0,CSG - CSGIM  - CS9YP-CODCOB))*positif_ou_nul(CODCOB);
NRDBASEBIS = abs(min(0,RDSN - CRDSIM  - RD9YP-CODCOR))*positif_ou_nul(CODCOR);
NPSOLBASEBIS = abs(min(0,PSOL - PRSPROV  - PS9YP-CODCOD)*positif_ou_nul(CODCOD));
NIRNIN = min(0,IAN + AVFISCOPTER - IRE-CODCOA-COD8EA+CODZRA)*positif_ou_nul(IAN + AVFISCOPTER - IRE)*positif_ou_nul(CODCOA);
NCSBASE = min(0,CSG - CSGIM  - CS9YP-CODCOB)*positif_ou_nul(CODCOB);
NRDBASE = min(0,RDSN - CRDSIM  - RD9YP-CODCOR)*positif_ou_nul(CODCOR);
NPSOLBASE = min(0,PSOL - PRSPROV  - PS9YP-CODCOD)*positif_ou_nul(CODCOD);
NCVNBASE = min(0,CVNN - COD8YT  - CVN9YP-CODCOE-NCVNBASE_PA)*positif_ou_nul(CODCOE);
NCDISBASE = min(0,CDIS - CDISPROV  - CDIS9YP-CODCOF-NCDISBASE_PA)*positif_ou_nul(CODCOF);
NC820BASE = min(0,MCSG820 - COD8ZH -C8209YP-CODCOQ-NC820BASE_PA)*positif_ou_nul(CODCOQ);
NGLOBASE = min(0,CGLOA - COD8YL  - GLO9YP-CODCOG-NGLOBASE_PA)*positif_ou_nul(CODCOG);
NRSE1BASE = min(0,RSE1N - CSPROVYD - RSE19YP-CODCOT-NRSE1BASE_PA)*positif_ou_nul(CODCOT);
NRSE2BASE = (min(0, max(0, RSE8TV - CIRSE8TV - CSPROVYF) + max(0, RSE8SA -CIRSE8SA - CSPROVYN) - RSE29YP-CODCOL)-NRSE2BASE_PA)*positif_ou_nul(CODCOL);
NRSE3BASE = min(0,RSE3N - CSPROVYG - RSE39YP-CODCOM-NRSE3BASE_PA)*positif_ou_nul(CODCOM);
NRSE4BASE = min(0, RSE4N  - CSPROVYH - CSPROVYP - RSE49YP-CODCOO-NRSE4BASE_PA)*positif_ou_nul(CODCOO);
NRSE5BASE = min(0,RSE5N - CSPROVYE - RSE59YP-CODCOJ-NRSE5BASE_PA)*positif_ou_nul(CODCOJ);
NRSE6BASE = min(0,RSE6N - COD8YQ - RSE69YP-CODCOP-NRSE6BASE_PA)*positif_ou_nul(CODCOP);
NRSE8BASE = min(0,RSE8N - COD8YV -COD8YX - RSE89YP-CODCOH-NRSE8BASE_PA)*positif_ou_nul(CODCOH);
NTAXABASE = min(0,TAXASSUR - TAXA9YI -CODCOU-NTAXABASE_PA)*positif_ou_nul(CODCOU);
NPCAPBASE = min(0,IPCAPTAXT - CAP9YI -CODCOV-NPCAPBASE_PA)*positif_ou_nul(CODCOV);
NCHRBASE = min(0,IHAUTREVT +CHRPVIMP- CHR9YI -CODCOX-NCHRBASE_PA)*positif_ou_nul(CODCOX);

CSPROVRSE6 = COD8YQ;

CSBASE = max(0 , CSG - CSGIM - CODCOB-CS9YP) ;
RDBASE = max(0 , RDSN - CRDSIM - CODCOR)-RD9YP ;
PSOLBASE = max(0 , PSOL - PRSPROV - CODCOD-PS9YP) ;
CVNBASE = max(0,CVNN - COD8YT -CODCOE)-CVN9YP;
CDISBASE = max(0,CDIS - CDISPROV-CODCOF)-CDIS9YP ;
C820BASE = max(0,MCSG820 - COD8ZH-CODCOQ)-C8209YP;
GLOBASE = max(0,CSGLOA - COD8YL -CODCOG)-GLO9YP;
RSE1BASE = max(0,RSE1N - CSPROVYD-CODCOT)-RSE19YP;
RSE2BASE = max(0,max(0, RSE8TV - CIRSE8TV - CSPROVYF) 
         + max(0, RSE8SA - CIRSE8SA - CSPROVYN)-CODCOL)-RSE29YP ;
RSE3BASE = max(0,RSE3N - CSPROVYG-CODCOM)-RSE39YP;
RSE4BASE = max(0, RSE4N - CSPROVYH - CSPROVYP-CODCOO)-RSE49YP ;
RSE5BASE = max(0,RSE5N - CSPROVYE -CODCOJ)-RSE59YP;
RSE6BASE = max(0,RSE6N - COD8YQ-CODCOP)-RSE69YP ;
RSE8BASE = max(0,RSE8N - COD8YV - COD8YX - CODCOH)-RSE89YP;
TAXABASE = arr(max(TAXASSUR + min(0,IRN - IRANT)-CODCOU,0)-TAXA9YI) * positif(IAMD1 + 1 - SEUIL_61);
PCAPBASE = arr(max(IPCAPTAXT + min(0,IRN - IRANT + TAXASSUR-CODCOV-IR9YI-TAXA9YI),0)-CAP9YI) * positif(IAMD1 + 1 - SEUIL_61);
CHRBASE = arr(max(IHAUTREVT + CHRPVIMP + min(0 , IRN - IRANT + TAXASSUR + IPCAPTAXT - CODCOX-IR9YI-TAXA9YI-CAP9YI) , 0)-CHR9YI) * positif(IAMD1 + 1 - SEUIL_61) ;

IRBASE_I = (IRN -IRANT)*positif(IRN+1-SEUIL_12);

IRBASE_N = (IRN - IRANT)*(1 - positif (IRN-IRANT + TAXABASE_IRECT+CAPBASE_IRECT+HRBASE_IRECT))
           + (IAN - min( IAN , IRE )) * positif (IRN-IRANT + TAXABASE_IRECT+CAPBASE_IRECT+HRBASE_IRECT);
TAXABASE_I = TAXASSUR * positif(IAMD1 + 1 - SEUIL_61);
TAXABASE_N = arr(max(TAXASSUR + min(0,IRN - IRANT),0)) * positif(IAMD1 + 1 - SEUIL_61);
CAPBASE_I = IPCAPTAXT * positif(IAMD1 + 1 - SEUIL_61);
CAPBASE_N = arr(max(IPCAPTAXT + min(0,IRN - IRANT + TAXASSUR),0)) * positif(IAMD1 + 1 - SEUIL_61);
HRBASE_I = (IHAUTREVT +CHRPVIMP)* positif(IAMD1 + 1 - SEUIL_61);
HRBASE_N = arr(max(IHAUTREVT+CHRPVIMP + min(0 , IRN - IRANT + TAXASSUR + IPCAPTAXT) , 0)) * positif(IAMD1 + 1 - SEUIL_61) ;


IRNN = IRNIN ;

regle 201070:
application : iliad ;

PIR = (INCIR_NET
       + NMAJ1 + NMAJ3 + NMAJ4 
       + arr((BTO) * TXINT / 100)* positif(CMAJ)) * (1-INDTXMIN)
       + 
      (INCIR_NET
       + NMAJ1 + NMAJ3 + NMAJ4 
       + arr((BTO) * TXINT / 100)* positif(CMAJ)
       ) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN
       ;

PPSOL = (
       INCPSOL_NET 
       + NMAJPSOL1+ NMAJPSOL4
       + arr(max(0,MPSOL-PRSPROV-CIPSOL-max(0,PS9YP)) * TXINT / 100)* positif(CMAJ)
              ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);


PCSG = (
       INCCS_NET
       + NMAJC1 + NMAJC4
       + arr(max(0,CSGC-CSGIM-CICSG-CS9YP) * TXINT / 100) * positif(CMAJ)
       ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);


PRDS = (
       INCRD_NET
       + NMAJR1 + NMAJR4
       + arr(max(0,RDSC-CRDSIM-CIRDS-RD9YP) * TXINT / 100) * positif(CMAJ)
       ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);


PCVN = (
       INCCVN_NET
       + NMAJCVN1 + NMAJCVN4
       + arr(max(0,CVNN - COD8YT-CVN9YP) * TXINT / 100) * positif(CMAJ)
       ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);


PTAXA = (INCTAXA_NET
        + NMAJTAXA1 + NMAJTAXA3 + NMAJTAXA4
        + arr(max(0,TAXASSUR- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT)-TAXA9YI) * TXINT / 100
	     )* positif(CMAJ)
	) 
         * (1-INDTXMIN)
        + (INCTAXA_NET
        + NMAJTAXA1 + NMAJTAXA3 + NMAJTAXA4
        + arr(max(0,TAXASSUR- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT)-TAXA9YI) * TXINT / 100
	     )* positif(CMAJ)
	  )
	   * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN;
PPCAP = ( INCPCAP_NET
         + NMAJPCAP1 + NMAJPCAP3 + NMAJPCAP4
         + arr(max(0,IPCAPTAXT- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))+min(0,IRN - IRANT+TAXASSUR)-CAP9YI) * TXINT / 100
	      )* positif(CMAJ)
        ) 
         * (1-INDTXMIN)
       +(INCPCAP_NET
       + NMAJPCAP1 + NMAJPCAP3 + NMAJPCAP4
       + arr(max(0,IPCAPTAXT- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))+min(0,IRN - IRANT+TAXASSUR)-CAP9YI) * TXINT / 100
            )* positif(CMAJ)
	 )
	  * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN;

PHAUTREV  = ( INCCHR_NET
       + NMAJCHR1 + NMAJCHR3 + NMAJCHR4
         + arr(max(0,IHAUTREVT+CHRPVIMP+min(0,IRN - IRANT+TAXASSUR+IPCAPTAXT)-CHR9YI) * TXINT / 100)* positif(CMAJ)
	 ) * (1-INDTXMIN)
       + (INCCHR_NET
       + NMAJCHR1 + NMAJCHR3 + NMAJCHR4
         + arr(max(0,IHAUTREVT+CHRPVIMP+min(0,IRN - IRANT+TAXASSUR+IPCAPTAXT)-CHR9YI) * TXINT / 100)* positif(CMAJ)
	 ) * positif_ou_nul(IAMD1 - SEUIL_TXMIN) * INDTXMIN;

PGLOA = (
       INCGLOA_NET
       + NMAJGLO1 + NMAJGLO4
       + arr(max(0,GLOBASE-GLO9YP)* TXINT / 100) * positif(CMAJ)
       ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PRSE1 = (
       INCRSE1_NET
       + NMAJRSE11 + NMAJRSE14
         + arr(max(0,RSE1 -CIRSE1 -CSPROVYD-RSE19YP)* TXINT / 100) * positif(CMAJ)
	 ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PRSE2 = (
      	  INCRSE2_NET
       	  + NMAJRSE21 + NMAJRSE24
       	  + arr(max(0,(max(0,RSE8TV -CIRSE8TV -CSPROVYF)+ max(0, RSE8SA -CIRSE8SA - CSPROVYN)-RSE29YP)) * TXINT / 100
               ) * positif(CMAJ)
        ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
PRSE3 = (
       INCRSE3_NET
       + NMAJRSE31 + NMAJRSE34
         + arr(max(0,RSE3 -CIRSE3 -CSPROVYG-RSE39YP)* TXINT / 100) * positif(CMAJ)
	 ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
PRSE4 = (
      	          INCRSE4_NET
       		+ NMAJRSE41 + NMAJRSE44
                + arr(max(0,RSE4 - CIRSE4 - CSPROVRSE4 - RSE49YP)* TXINT / 100) * positif(CMAJ)
        ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PRSE5 = (
       INCRSE5_NET
       + NMAJRSE51 + NMAJRSE54
         + arr(max(0,RSE5 -CIRSE5 -CSPROVYE-RSE59YP)* TXINT / 100) * positif(CMAJ)
	 ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
PRSE6 = (
       INCRSE6_NET
       + NMAJRSE61 + NMAJRSE64
         + arr(max(0,RSE6BASE -RSE69YP)* TXINT / 100) * positif(CMAJ)
	 ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
PRSE8 = (
       INCRSE8_NET
       + NMAJRSE81 + NMAJRSE84
         + arr(max(0,RSE8BASE -RSE89YP)* TXINT / 100) * positif(CMAJ)
	 ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
PCSG92=PRSE8;
PCDIS = (
       INCCDIS_NET
       + NMAJCDIS1 + NMAJCDIS4
         + arr(max(0,CDIS-CDISPROV-CDIS9YP) * TXINT / 100) * positif(CMAJ)
	 ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
PCSG820 = (
       INCC820_NET
       + NMAJC8201 + NMAJC8204
         + arr(max(0,MCSG820-COD8ZH-C8209YP) * TXINT / 100) * positif(CMAJ)
	 ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);


PDEG = max(0,PIR_A + PTAXA_A + PPCAP_A - PCHR_A - PIR - PTAXA - PPCAP - PHAUTREV);

regle 201090:
application : iliad ;
TOTPENIR = (PIR + PTAXA + PHAUTREV + PPCAP) * positif_ou_nul(VARIR61-SEUIL_61);
TOTPENIR = (PIR + PTAXA + PHAUTREV + PPCAP)
             * positif ( positif_ou_nul(VARIR61-SEUIL_61)
                         + positif_ou_nul(VARIRDROIT-SEUIL_61)
                       ) ;

TOTPENCS = (PPSOL+ PCSG + PRDS + PCVN + PCDIS + PGLOA + PRSE1 + PRSE2 + PRSE3 + PRSE4 + PRSE5 + PRSE6+PRSE8+PCSG820) * positif_ou_nul(VARPS61-SEUIL_61);

INCTOTIR = RETIR + RETTAXA + RETPCAP + RETHAUTREV ;

INCTOTCS = RETCS+RETRD+RETPS+RETPSOL+RETCVN+RETCDIS+RETGLOA
           +RETRSE1+RETRSE2+RETRSE3+RETRSE4
           +RETRSE5+RETRSE6+RETRSE8+RETCSG820;
RETIRCSTOT = INCTOTIR + INCTOTCS ;

regle 201100:
application : iliad;


PTOT = PIR ;

regle 201110:
application : iliad ;


ILI_SYNT_IR =  (1-ANNUL2042) * (positif(TOTIRCUM - NONMER - RECUMIR + NONREST - TOTPENIR) * max(0 , IRCUM - NONMER + NONREST - PIR)
              + (1 - positif(TOTIRCUM - NONMER - RECUMIR + NONREST - TOTPENIR)) * (TOTIRCUM - NONMER - RECUMIR + NONREST - TOTPENIR)) ;

PIRNEG = (1-ANNUL2042) * abs(min(0 , IRCUM - NONMER + NONREST - PIR)) ;

ILI_SYNT_TAXA = (1-ANNUL2042) * (positif(TOTIRCUM - NONMER - RECUMIR + NONREST - TOTPENIR) * max(0,TAXACUM - PTAXA - PIRNEG)
               + (1 - positif(TOTIRCUM - NONMER - RECUMIR + NONREST - TOTPENIR)) * 0);

PTAXANEG = (1-ANNUL2042) * abs(min(0 , TAXACUM - PTAXA - PIRNEG)) ;

ILI_SYNT_CAP = (1-ANNUL2042) * (positif(TOTIRCUM - NONMER - RECUMIR + NONREST - TOTPENIR) * max(0 , PCAPCUM - PPCAP - PTAXANEG)
               + (1 - positif(TOTIRCUM - NONMER - RECUMIR + NONREST - TOTPENIR)) * 0) ;

PPCAPNEG = (1-ANNUL2042) * abs(min(0 , PCAPCUM - PPCAP - PTAXANEG)) ;

ILI_SYNT_CHR = (1-ANNUL2042) * (positif(TOTIRCUM - NONMER - RECUMIR + NONREST - TOTPENIR) * max(0 , HAUTREVCUM - PHAUTREV - PPCAPNEG)
               + (1 - positif(TOTIRCUM - NONMER - RECUMIR + NONREST - TOTPENIR)) * 0) ;

ILI_SYNT_TOTIR = (1-ANNUL2042) * (ILI_SYNT_IR + ILI_SYNT_TAXA + ILI_SYNT_CAP + ILI_SYNT_CHR ) * (1-APPLI_BATCH);

regle 201120:
application : iliad ;


ILIIRNET =  (1-ANNUL2042) * (positif_ou_nul(TOTIRCUM - RECUMIR - TOTPENIR + ACPASIR) * max(0,IRCUM-PIR)
              + (1 - positif_ou_nul(TOTIRCUM - RECUMIR - TOTPENIR + ACPASIR)) * (TOTIRCUM - RECUMIR - TOTPENIR + ACPASIR));

PIRNETNEG =  (1-ANNUL2042) * max(0,PIR-IRCUM);

ILITAXANET = (1-ANNUL2042) * (positif_ou_nul(TOTIRCUM - RECUMIR - TOTPENIR + ACPASIR) * max(0,TAXACUM - PTAXA - PIRNETNEG)
	       + (1 - positif_ou_nul(TOTIRCUM - RECUMIR - TOTPENIR + ACPASIR)) * 0);

PTAXANETNEG =  (1-ANNUL2042) * max(0,PIR+PTAXA-IRCUM-TAXACUM);

ILICAPNET = (1-ANNUL2042) * (positif_ou_nul(TOTIRCUM - RECUMIR - TOTPENIR + ACPASIR) * max(0,PCAPCUM -PPCAP-PTAXANETNEG)
	       + (1 - positif_ou_nul(TOTIRCUM - RECUMIR - TOTPENIR + ACPASIR)) * 0);

PPCAPNETNEG =  (1-ANNUL2042) * max(0,PIR+PTAXA+PPCAP-IRCUM-TAXACUM-PCAPCUM);

ILICHRNET = (1-ANNUL2042) * (positif_ou_nul(TOTIRCUM - RECUMIR - TOTPENIR + ACPASIR) * max(0,HAUTREVCUM-PHAUTREV-PPCAPNETNEG) 
	       + (1 - positif_ou_nul(TOTIRCUM - RECUMIR - TOTPENIR + ACPASIR)) * 0);

ILITOTIRNET = (1 - ANNUL2042) * (ILIIRNET + ILITAXANET + ILICAPNET + ILICHRNET) ;

ILITOTPSNET = (1 - ANNUL2042) * max(0, NAPCRB61 - TOTPENCS) ;

TOTTP = TTPVQ + REVTP ;

TOTIRE = IREP - ITRED - IRE - INE ;
regle 201130:
application : iliad ;


MAJOTOT28IR = NMAJ1     +
               NMAJTAXA1 +
               NMAJPCAP1 +
               NMAJCHR1 ;

MAJOTOT28PS = NMAJC1 +
               NMAJR1    +
                NMAJPSOL1    +
                NMAJCVN1  +
                NMAJCDIS1 +
                NMAJC8201 +
                NMAJGLO1  +
                NMAJRSE11 +
                NMAJRSE21 +
                NMAJRSE31 +
                NMAJRSE41 +
                NMAJRSE51 +
                NMAJRSE61 +
                NMAJRSE81 ;
MAJO1728TOT = MAJOTOT28IR + MAJOTOT28PS ;

regle 201140:
application : iliad ;


DEC_CGA_AGA = BAHREV - BAHDEV
            + BAHREC - BAHDEC
            + BAHREP - BAHDEP
            + BIHNOV - BIHDNV
            + BIHNOC - BIHDNC
            + BIHNOP - BIHDNP
            + BICHREV - BICHDEV
            + BICHREC - BICHDEC
            + BICHREP - BICHDEP
            + BNHREV - BNHDEV
            + BNHREC - BNHDEC
            + BNHREP - BNHDEP
            + ANOCEP - DNOCEP
            + ANOVEP - DNOCEPC
            + ANOPEP - DNOCEPP
            ;
MAJ_CGA_AGA = arr(SUPREV * max(0,BAHREV - BAHDEV ))
            + arr(SUPREV * max(0,BAHREC - BAHDEC ))
            + arr(SUPREV * max(0,BAHREP - BAHDEP ))
            + arr(SUPREV * max(0,BIHNOV - BIHDNV ))
            + arr(SUPREV * max(0,BIHNOC - BIHDNC ))
            + arr(SUPREV * max(0,BIHNOP - BIHDNP ))
            + arr(SUPREV * max(0,BICHREV - BICHDEV ))
            + arr(SUPREV * max(0,BICHREC - BICHDEC ))
            + arr(SUPREV * max(0,BICHREP - BICHDEP ))
            + arr(SUPREV * max(0,BNHREV - BNHDEV ))
            + arr(SUPREV * max(0,BNHREC - BNHDEC ))
            + arr(SUPREV * max(0,BNHREP - BNHDEP ))
            + arr(SUPREV * max(0,ANOCEP - DNOCEP ))
            + arr(SUPREV * max(0,ANOVEP - DNOCEPC ))
            + arr(SUPREV * max(0,ANOPEP - DNOCEPP ))
            ;
TOT_CGA_AGA = DEC_CGA_AGA + MAJ_CGA_AGA ;


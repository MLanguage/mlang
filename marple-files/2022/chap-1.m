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
  ####   #    #    ##    #####      #     #####  #####   ######           ##
 #    #  #    #   #  #   #    #     #       #    #    #  #               # #
 #       ######  #    #  #    #     #       #    #    #  #####             #
 #       #    #  ######  #####      #       #    #####   #                 #
 #    #  #    #  #    #  #          #       #    #   #   #                 #
  ####   #    #  #    #  #          #       #    #    #  ###### #######  #####
 #
 #
 #
 #
 #
 #                         CALCUL DU NET A PAYER
 #
 #
 #
 #
 #
 #
regle 101000:
application : bareme ;


RC1 = positif( NAPI + 1 - SEUIL_12 ) +0 ;


regle 101005:
application : iliad ;

TESTCOMP = IRNET3 + AVRICIIR3 + TAXANET3 + PCAPNET3 + HAUTREVNET3 + max(0 , CSGPAT) + max(0 , CRDSPAT) + max(0 , PRELPSOL) + PREVAUTSOC 
           - IMPCSGPS - IMPPSOLPS - IMPCSGIR - IMPPSOLIR - COMPENSACI - COMPENSPAS - COMPENSANV - CRESTACI - CPASIRREST - CPASCSGREST - CPASPSOLREST
	   - IINET + IREST ;

regle 101011:
application : iliad ;


NAPTIR2 = IRCUM + TAXACUM + PCAPCUM + HAUTREVCUM - IRESTITIR - RASSALIR - RASACOIR + RASCTXIR + AVRICIIR + CIADCREB3 ;

NAPTIRNET = IRCUM + TAXACUM + PCAPCUM + HAUTREVCUM - IRESTITIR - RASSALIR - RASACOIR + RASCTXIR + AVRICIIR - IMPETAL + IMPETAL5 + CIADCREB3 ;

TESTBLOC = positif(RASSALIR + RASACOIR + RASCTXIR + AVRICIIR + IMPETAL + IMPETAL5 + CIADCREB3 + RASACOPS + RASCTXPS + PREVAUTSOC) + 0 ;

ACPASIR = RASSALIR + RASACOIR - RASCTXIR - AVRICIIR + IMPETAL - IMPETAL5 - CIADCREB3 ;

ACPASCS = RASACOCS - RASCTXCS + RESTITCS ;

ACPASPSOL = RASACOPSOL - RASCTXPSOL + RESTITPSOL ;

ACPASTOT = ACPASIR + ACPASCS + ACPASPSOL ;
ACPASTOTANT = V_ACPASTOTPANT - V_ACPASTOTNANT ;

regle 101012:
application : iliad ;


IRNETBA = IRNET * positif(NAPTIR1) + NAPTIR1 * (1 - positif(NAPTIR1)) - IMPETAL + IMPETAL5 ;

PASIR = RASCTXIR - RASSALIR - RASACOIR ;

IRACPAS = RASSALIR + RASACOIR - RASCTXIR ;

PSACPAS = RASACOPS - RASCTXPS ;

CONDCOMP = positif(NAPTIRNET) * ((1 - null(abs(PRELCS) + abs(PRELPSOL) + abs(PREVAUTSOC))) 
                                 + null(abs(PRELCS) + abs(PRELPSOL) + abs(PREVAUTSOC)) * null(2 - IND12)) ;

IRNET3 = max(0 , IRNETBA + PASIR) * CONDCOMP ;

TAXANET3 = max(0 , TAXANET - max(0 , - PASIR - IRNETBA)) * positif(NAPTIR1) * CONDCOMP ;

PCAPNET3 = max(0 , PCAPNET - max(0 , - PASIR - IRNETBA - TAXANET)) * positif(NAPTIR1) * CONDCOMP ;

HAUTREVNET3 = max(0 , HAUTREVNET - max(0 , - PASIR - IRNETBA - TAXANET - PCAPNET)) * positif(NAPTIR1) * CONDCOMP ;

AVRICIIR3 = max(0 , AVRICIIR + CIADCREB3 - max(0 , - PASIR - IRNETBA - (TAXANET + PCAPNET + HAUTREVNET) * positif(NAPTIR1))) * CONDCOMP ;

CSGPAT = arr(PRELCS * T_CSG/T_CSGCRDS) ;

CRDSPAT = PRELCS - CSGPAT ;

PREVAUSOCANT = V_PREVSOCANT - V_PREVSOCNANT - V_PRELCSANT + V_PRELCSNANT - V_PRELPSOLANT + V_PRELPSOLNANT ;

IMPCSGPS = min(abs(PRELCS) , PREVAUTSOC + PRELPSOL * positif(PRELPSOL)) * (1 - positif(PRELCS)) * positif(PREVAUTSOC)
           + min(abs(PRELCS) , PRELPSOL) * (1 - positif(PRELCS)) * positif(PRELPSOL) * null(PREVAUTSOC) ;

IMPCSGPSD = min(abs(PRELCS) , PREVAUTSOC + PRELPSOL * positif(PRELPSOL)) * (1 - positif(PRELCS)) * positif(PREVAUTSOC)
            + min(abs(PRELCS) , PRELPSOL) * (1 - positif(PRELCS)) * positif(PRELPSOL) * null(PREVAUTSOC) ;

IMPPSOLPS = min(abs(PRELPSOL) , PREVAUTSOC + PRELCS * positif(PRELCS) - IMPCSGPS * (1 - positif(PRELCS))) * (1 - positif(PRELPSOL)) * positif(PREVAUTSOC)
            + min(abs(PRELPSOL) , PRELCS) * (1 - positif(PRELPSOL)) * positif(PRELCS) * (1 - positif(PREVAUTSOC)) ;

IMPPSOLPSD = min(abs(PRELPSOL) , PREVAUTSOC + PRELCS * positif(PRELCS) - IMPCSGPSD * (1 - positif(PRELCS))) * (1 - positif(PRELPSOL)) * positif(PREVAUTSOC)
             + min(abs(PRELPSOL) , PRELCS) * (1 - positif(PRELPSOL)) * positif(PRELCS) * (1 - positif(PREVAUTSOC)) ;

SOLDCSGPS = max(0 , max(0 , abs(PRELCS) - IMPCSGPS)) * (1 - positif(PREVSOCNET)) * (1 - positif(PRELCS)) ;

SOLDCSGPSD = max(0 , max(0 , abs(PRELCS) - IMPCSGPSD)) * (1 - positif(PREVSOCNET)) * (1 - positif(PRELCS)) ;

SOLDPSOLPS = max(0 , max(0 , abs(PRELPSOL) - IMPPSOLPS)) * (1 - positif(PREVSOCNET)) * (1 - positif(PRELPSOL)) ;

SOLDPSOLPSD = max(0 , max(0 , abs(PRELPSOL) - IMPPSOLPSD)) * (1 - positif(PREVSOCNET)) * (1 - positif(PRELPSOL)) ;

IMPCSGIR = min(SOLDCSGPS , NAPTIRNET) * positif(NAPTIRNET) ;

regle 101013:
application : iliad ;

IMPCSGIR3 = IMPCSGIR * positif(20 - V_NOTRAIT) + max(0 , min(0 , CSGCOMPANT) - min(0 , CSGCOMP) - IMPCSGPS3) * positif(V_NOTRAIT - 20) ;

IMPCSGIRD = min(SOLDCSGPSD , NAPTIRNET) * positif(NAPTIRNET) ;

IMPPSOLIR = min(SOLDPSOLPS , NAPTIRNET - IMPCSGIR) * positif(NAPTIRNET) ;

IMPPSOLIR3 = IMPPSOLIR * positif(20 - V_NOTRAIT) + max(0 , min(0 , PSOLCOMPANT) - min(0 , PSOLCOMP) - IMPPSOLPS3) * positif(V_NOTRAIT - 20) ;

IMPPSOLIRD = min(SOLDPSOLPSD , NAPTIRNET - IMPCSGIRD) * positif(NAPTIRNET) ;

FRAISCSGIR = arr(IMPCSGIR * TX041/100) ;

FRAISCSGIR3 = arr(IMPCSGIR3 * TX041/100) ;

FRAISCSGID = arr(IMPCSGIRD * TX041/100) ;

NETFRSCSGIR = IMPCSGIR - FRAISCSGIR ;

NETFRSCSGIR3 = IMPCSGIR3 - FRAISCSGIR3 ;

NETFRSCSGD = IMPCSGIRD - FRAISCSGID ;

NAPTIRNET2 = NAPTIR1 + AVRICIIR + CIADCREB3 - IMPETAL + IMPETAL5 ;

regle 101014:
application : iliad ;

COMPENSPAS = min(abs(PASIR - V_PASIRANT + V_PASIRNANT) , min(PREVSOCNET - V_PREVSOCANT + V_PREVSOCNANT - COMPENSACI , abs(NAPTIRNET - V_ANTIR + V_ANTREIR)))
              * positif(PREVSOCNET - V_PREVSOCANT + V_PREVSOCNANT) * (1 - positif(PASIR - V_PASIRANT + V_PASIRNANT))
              * (1 - positif(NAPTIRNET - V_ANTIR + V_ANTREIR)) * positif(14 - V_NOTRAIT) 
             + max(0 , min(0 , IRCOMPANT) - min(0 , IRCOMP) - COMPENSACI) * positif(V_NOTRAIT - 20) 
	       * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) ;

COMPENSACI3 = min(abs(max(NAPTIRNET2 , NAPTIRNET)) , PREVSOCNET) * (1 - positif(NAPTIRNET)) * (1 - positif(NAPTIRNET2)) * positif(PREVSOCNET) ;

CRESTACI = min(abs(max(NAPTIRNET2 + V_NAPTIRN2NANT , NAPTIRNET + V_NAPTIRNTNANT) + COMPENSACI) , abs(IREST))
           * (1 - positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)))
           * (1 - positif(IRPSNET)) * (1 - positif(NAPTIRNET)) * (1 - positif(NAPTIRNET2 + COMPENSACI)) ;

CPASIRREST = max(0 , min(min(abs(PASIR - V_PASIRANT + V_PASIRNANT + COMPENSPAS) , abs(NAPTIRNET - V_NAPTIRNTNANT + COMPENSPAS)) , abs(IREST) - CRESTACI))
             * (1 - positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)))
             * (1 - positif(IRPSNET)) * (1 - positif(NAPTIRNET)) * (1 - positif(PASIR + COMPENSPAS)) ;

CPASCSGREST = max(0,min(min(abs(PRELCS + IMPCSGPS + IMPCSGIR) , abs(PREVSOCNET)) , abs(IREST) - CRESTACI - CPASIRREST)
              * (1 - positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)))
              * (1 - positif(IRPSNET)) * (1 - positif(PRELCS + IMPCSGPS + IMPCSGIR)) * (1 - positif(PREVSOCNET))) ;

CPASPSOLREST = max(0,min(min(abs(PRELPSOL + IMPPSOLPS + IMPPSOLIR) , abs(PREVSOCNET)) , abs(IREST) - CRESTACI - CPASIRREST - CPASCSGREST)
               * (1 - positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)))
               * (1 - positif(IRPSNET)) * (1 - positif(PRELPSOL + IMPPSOLPS + IMPPSOLIR)) * (1 - positif(PREVSOCNET))) ;

FRAISPASPS = arr(CPASCSGREST * TX041/100) ;

NETFRSPASPS = CPASCSGREST - FRAISPASPS ;

COMPENSPAD = min(abs(PASIR) , min(PREVSOCNET - COMPENSACD , abs(NAPTIRNET))) * positif(PREVSOCNET) * (1 - positif(PASIR)) * (1 - positif(NAPTIRNET)) ;

IRPSREST = abs(min(0 , IRPSNET)) ;

CRESTACID = min(abs(max(NAPTIRNET2 , NAPTIRNET) + COMPENSACD) , abs(IRPSREST)) * (1 - positif(IRPSNET)) * (1 - positif(NAPTIRNET)) * (1 - positif(NAPTIRNET2 + COMPENSACD)) ;

CPASIRRESD = min(min(abs(PASIR + COMPENSPAD) , abs(NAPTIRNET + COMPENSPAD)) , abs(IRPSREST) - CRESTACID) 
             * (1 - positif(IRPSNET)) * (1 - positif(NAPTIRNET)) * (1 - positif(PASIR + COMPENSPAD)) ;

CPASCSGRED = min(min(abs(PRELCS + IMPCSGPSD + IMPCSGIRD) , abs(PREVSOCNET)) , abs(IRPSREST) - CRESTACID - CPASIRRESD) 
             * (1 - positif(IRPSNET)) * (1 - positif(PRELCS + IMPCSGPSD + IMPCSGIRD)) * (1 - positif(PREVSOCNET)) ;

CPASPSOLRD = min(min(abs(PRELPSOL + IMPPSOLPSD + IMPPSOLIRD) , abs(PREVSOCNET)) , abs(IRPSREST) - CRESTACID - CPASIRRESD - CPASCSGRED) 
             * (1 - positif(IRPSNET)) * (1 - positif(PRELPSOL + IMPPSOLPSD + IMPPSOLIRD)) * (1 - positif(PREVSOCNET)) ;

FRAISPASPD = arr(CPASCSGRED * TX041/100) ;

NETFRSPASD = CPASCSGRED - FRAISPASPD ;

VARNAPTIR = NAPTIRNET - V_NAPTIRNETANT + V_NAPTIRNTNANT ;

VARPREVSOC = PREVSOCNET - V_PREVSOCANT + V_PREVSOCNANT ;

regle 101015:
application : iliad ;

RECUMIRPAS = min(RECUMIR , max(0 , RECUMIR - PASIR - AVRICIIR)) ;

NONRESTPS = max(0 , min(NONREST , V_ANTCR - V_NANTCR - NAPCR61)) ;

NONRESTIR = NONREST - NONRESTPS ;

NONMERPS = max(0 , min(NONMER , NAPCR61 - V_ANTCR + V_NANTCR)) ;

NONMERIR = NONMER - NONMERPS ;

regle 101016:
application : iliad ;

IRCOMP = max(TOTIRCUM - RECUMIR + NONRESTIR - (NONMERIR - COMPANVIR) , -max(0 , NAPCR61 + NONRESTPS - (NONMERPS - COMPANVPS))) * positif(14 - V_NOTRAIT)
         + (TOTIRCUM - RECUMIR + NONRESTIR - (NONMERIR - COMPANVIR)) * positif(V_NOTRAIT - 20) ;

IRCOMPANT = (V_TOTIRANT - V_TOTIRNANT - V_ANTREIR - (V_NONMERANT - V_NONMERPSANT) + (V_NONRESTANT - V_NONRESTPSANT)) * positif(V_NOTRAIT - 20) ;

regle 101017:
application : iliad ;

CSGCOMP = max(PRELCS + NONRESTPS - (NONMERPS - COMPANVPS) , -max(0 , max(0 , TOTIRCUM - RECUMIR + NONRESTIR - (NONMERIR - COMPANVIR)) + NAPCR61 - (PRELCS + NONRESTPS - (NONMERPS - COMPANVPS)))) * positif(14 - V_NOTRAIT)
          + (PRELCS + NONRESTPS - (NONMERPS - COMPANVPS)) * positif(V_NOTRAIT - 20) ;

CSGCOMPANT = (V_PRELCSANT - V_PRELCSNANT + V_NONRESTPSANT - V_NONMERPSANT) * positif(V_NOTRAIT - 20) ;

PSOLCOMP = max(PRELPSOL , -max(0 , max(0 , TOTIRCUM - RECUMIR + NONRESTIR - (NONMERIR - COMPANVIR) + NAPCR61 - PRELPSOL))) * positif(14 - V_NOTRAIT)
           + PRELPSOL * positif(V_NOTRAIT - 20) ;

PSOLCOMPANT = (V_PRELPSOLANT - V_PRELPSOLNANT) * positif(V_NOTRAIT - 20) ;

regle 101018:
application : iliad ;

IMPCSGPS3 = IMPCSGPS * positif(20 - V_NOTRAIT) + max(0 , min(min(0 , CSGCOMPANT) - min(0 , CSGCOMP) , IMPCSGPS - V_IMPCSGPSANT)) * positif(V_NOTRAIT - 20) ;

IMPPSOLPS3 = IMPPSOLPS * positif(20 - V_NOTRAIT) + max(0 , min(min(0 , PSOLCOMPANT) - min(0 , PSOLCOMP) , IMPPSOLPS - V_IMPPSOLPSANT)) * positif(V_NOTRAIT - 20) ;

regle 101019:
application : iliad ;


NAPT = NAPTEMPCX - TOTIRPSANT ;

NAPTIR = IRNET + TAXANET + PCAPNET + HAUTREVNET - IRESTITIR ;

NAPTIR61 = NAPTIR * positif_ou_nul(IAMD1 - COD8EA - COD8EB - SEUIL_61) ; 

NAPTIR1 = IRCUM + TAXACUM + PCAPCUM + HAUTREVCUM - IRESTITIR ; 

regle 101020:
application : iliad ;



NAPCOROLIR = (TOTIRCUM - NONMER -RECUMIR + NONREST) * positif(20 - V_NOTRAIT)

             + ( 0 * null( 2 - null(VARPS) - positif(1 - NATIMP))
                + max(0,NAPT+COMPENSPS-VARPS)* (1 - null( 2 - null(VARPS) - positif(1 - NATIMP)))
               ) * (1 - positif(20 - V_NOTRAIT)) ;

NAPCOROLCS = max(0, NAPCR61 - TOTCRA) ;

regle 101030:
application : iliad ;

RC1 = positif_ou_nul(NAPINI - V_ANTIR - IRCUM_A + RECUMBIS - SEUIL_12) ;

regle 101040:
application : iliad ;


IAVIMBIS = IRB + PIR + BRASAR + NRINET ;

IAVIMO = (max(0 , max(ID11 - ADO1 , IMI) - RED) + ITP + REI + PIR + BRASAR + NRINET) * V_CNR ;

regle 101050:
application : bareme , iliad ;


NAPI = IRD + PIRD - IRANT + TAXASSUR + IPCAPTAXT + IHAUTREVT + CHRPVIMP + BRASAR ;

regle 101060:
application : iliad ;


INTMS = inf( MOISAN / 10000 );
INTAN = (( MOISAN/10000 - INTMS )*10000)  * present(MOISAN) ;
TXINT = max(0, (INTAN - (ANNEEREV+1) )* 12 + INTMS - 6 ) * TXMOISRETARD2 * present(MOISAN);

COPETO = (NOMBRE10 * positif(null(CMAJ - 7) + null(CMAJ - 10) + null(CMAJ - 17) + null(CMAJ - 18))
          + NOMBRE40 * positif(null(CMAJ - 8) + null(CMAJ - 11))
	  + NOMBRE80 * (1 - positif(null(CMAJ - 7) + null(CMAJ - 10) + null(CMAJ - 17) + null(CMAJ - 18) + null(CMAJ - 8) + null(CMAJ - 11)))) * positif(CMAJ) ;


regle 101070:
application : iliad;

CSTOTSSPENA = max(0,CSGC + RDSC + MPSOL + CVNN + MCSG820 + CDIS + CGLOA + RSE1N + RSE2N + RSE3N + RSE4N
                  + RSE5N + RSE6N+ RSE8N);
PTOIR = arr(BTO * COPETO / 100)                
	 + arr(BTO * COPETO /100) * positif(null(CMAJ-10)+null(CMAJ-17))
         + arr((BTO) * TXINT / 100) ;

PTOTAXA= arr(max(0,TAXASSUR- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT)) * COPETO / 100)
	 + arr(max(0,TAXASSUR- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT)) * COPETO /100) * positif(null(CMAJ-10)+null(CMAJ-17))
         + arr(max(0,TAXASSUR- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT)-TAXA9YI) * TXINT / 100) ;

PTOTPCAP= arr(max(0,IPCAPTAXT- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))+min(0,IRN - IRANT+TAXASSUR)) * COPETO / 100)
	 + arr(max(0,IPCAPTAXT- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))+min(0,IRN - IRANT+TAXASSUR)) * COPETO /100) * positif(null(CMAJ-10)+null(CMAJ-17))
         + arr(max(0,IPCAPTAXT- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))+min(0,IRN - IRANT+TAXASSUR)-CAP9YI) * TXINT / 100) ;

PTOTCHR= arr(max(0,IHAUTREVT+CHRPVIMP+min(0,IRN - IRANT+TAXASSUR+IPCAPTAXT)) * COPETO / 100)
	 + arr(max(0,IHAUTREVT+CHRPVIMP+min(0,IRN - IRANT+TAXASSUR+IPCAPTAXT)) * COPETO /100) * positif(null(CMAJ-10)+null(CMAJ-17))
         + arr(max(0,IHAUTREVT+CHRPVIMP+min(0,IRN - IRANT+TAXASSUR+IPCAPTAXT)-CHR9YI) * TXINT / 100) ;

PTOCSG =( arr(max(0,CSGC-CSGIM) * COPETO / 100)                
         + arr(max(0,CSGC-CSGIM-CS9YP) * TXINT / 100) ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTOPSOL =( arr(max(0,MPSOL-PRSPROV) * COPETO / 100)                
         + arr(max(0,MPSOL-PRSPROV - max(0,PS9YP)) * TXINT / 100) ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTORDS =( arr(max(0,RDSC-CRDSIM) * COPETO / 100)                
         + arr(max(0,RDSC-CRDSIM-RD9YP) * TXINT / 100) ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTOCVN = (arr(max(0,CVNN - COD8YT) * COPETO / 100) + arr(max(0,CVNN - COD8YT-CVN9YP) * TXINT / 100))                
         * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTOCDIS = (arr(max(0,CDIS-CDISPROV) * COPETO / 100) + arr(max(0,CDISC-CDISPROV-CDIS9YP) * TXINT / 100)) 
          * positif_ou_nul(CSTOTSSPENA - SEUIL_61);
PTOCSG820 = (arr(MCSG820  * COPETO / 100) + arr(max(0,MCSG820-C8209YP) * TXINT / 100)) 
          * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTOGLOA = (arr(max(0,GLOBASE) * COPETO / 100) + arr(max(0,GLOBASE-GLO9YP) * TXINT / 100)) 
          * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTORSE1 = (arr(max(0,RSE1 -CIRSE1 -CSPROVYD) * COPETO / 100) 
               + arr(max(0,RSE1 -CIRSE1 -CSPROVYD-RSE19YP) * TXINT / 100)
          ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTORSE2 = (arr(max(0,RSE2 -CIRSE2 -CSPROVRSE2) * COPETO / 100) 
               + arr(max(0,RSE2 -CIRSE2 -CSPROVRSE2-RSE29YP) * TXINT / 100)
          ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTORSE3 = (arr(max(0,RSE3 -CIRSE3 -CSPROVYG) * COPETO / 100) 
               + arr(max(0,RSE3 -CIRSE3 -CSPROVYG-RSE39YP) * TXINT / 100)
          ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTORSE4 = (arr(max(0,RSE4 -CIRSE4 -CSPROVRSE4) * COPETO / 100)
               + arr(max(0,RSE4 -CIRSE4 -CSPROVRSE4-RSE49YP) * TXINT / 100)
          ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTORSE5 = (arr(max(0,RSE5 -CIRSE5 -CSPROVYE) * COPETO / 100) 
               + arr(max(0,RSE5 -CIRSE5 -CSPROVYE-RSE59YP) * TXINT / 100)
          ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTORSE6 = (arr(max(0,RSE6BASE) * COPETO / 100) 
               + arr(max(0,RSE6BASE - RSE69YP) * TXINT / 100)
          ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);

PTORSE8 = (arr(max(0,RSE8BASE) * COPETO / 100)
               + arr(max(0,RSE8BASE -RSE89YP) * TXINT / 100)
          ) * positif_ou_nul(CSTOTSSPENA - SEUIL_61);


regle 101080:
application : iliad ;


BINRIR = (1-V_CNR) * (max( 0 ,IRN-IRANT)
          + max(0,TAXASSUR- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT))
            + max(0,IPCAPTAXT- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))
                                +min(0,IRN - IRANT+TAXASSUR))
           + max(0,IHAUTREVT+CHRPVIMP+min(0,IRN - IRANT+TAXASSUR+IPCAPTAXT)))
       + (V_CNR+0)* (max( 0 ,IRN-IRANT)
          + max(0,TAXASSUR- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT))
            + max(0,IPCAPTAXT- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))
                                +min(0,IRN - IRANT+TAXASSUR))
           + max(0,IHAUTREVT+CHRPVIMP+min(0,IRN - IRANT+TAXASSUR+IPCAPTAXT))) * INDTXMIN 
	        * positif_ou_nul(max( 0 ,IRN-IRANT)
                               + max(0,TAXASSUR- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))+min(0,IRN - IRANT))
                               + max(0,IPCAPTAXT- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))
                                                                             +min(0,IRN - IRANT+TAXASSUR))
                               + max(0,IHAUTREVT+CHRPVIMP+min(0,IRN - IRANT+TAXASSUR+IPCAPTAXT))- SEUIL_TXMIN)
       ;

BINRPS = max(0,CSGC-CICSG-CSGIM)+ max(0,RDSC-CIRDS-CRDSIM) + max(0,MPSOL-CIPSOL-PRSPROV) + max(0,CVNN - COD8YT) + max(0,CDIS - CDISPROV)
          + max(0,CGLOA-CIGLOA-COD8YL) + max(0,RSE1N-CSPROVYD) + max(0,RSE2N-CSPROVRSE2) + max(0,RSE3N-CSPROVYG)
                  + max(0,RSE4N-CSPROVRSE4) + max(0,RSE5N-CSPROVYE) + max(0,RSE6N-COD8YQ) + max(0,MCSG820-COD8ZH) +  max(0,RSE8N);
regle 101083:
application : iliad ;

SOM9YIINTER = (1-positif(ACODELAISINR)) * max(0,COD8HV+COD8HW+COD8HX-COD8HY - COD8HZ + COD8IV + COD8IW + COD8IX - COD8IY - COD8IZ
                               + COD8JV +COD8JW + COD8JX - COD8JY - COD8JZ + COD8KV +COD8KW + COD8KX - COD8KY - COD8KZ
                                + COD8LV +COD8LW + COD8LX - COD8LY - COD8LZ + COD8MV +COD8MW + COD8MX - COD8MY - COD8MZ)
          + positif(ACODELAISINR) * ACODELAISINR;
SOM9YI= (SOM9YIINTER + 0) * (1-positif(V_NOTRAIT -20));
VAR9YIIR= arr(SOM9YI * BINRIR/(BINRIR+BINRPS)) * (1-positif(V_NOTRAIT -20));
VAR9YIPS = max(0,SOM9YI - VAR9YIIR) * (1-positif(V_NOTRAIT -20));
IR9YI  = arr(VAR9YIIR * max( 0 , IRN - IRANT )/BINRIR);
TAXA9YI = positif(IPCAPTAXT + IHAUTREVT + CHRPVIMP) * arr(VAR9YIIR * max(0,TAXASSUR- min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))
                          +min(0,IRN - IRANT))/BINRIR)
                + (1 - positif(IPCAPTAXT + IHAUTREVT + CHRPVIMP)) * max(0 , VAR9YIIR - IR9YI) ;
CAP9YI = positif(IHAUTREVT + CHRPVIMP) * arr(VAR9YIIR * max(0,IPCAPTAXT- min(IPCAPTAXT+0,max(0,INE-IRB+AVFISCOPTER-TAXASSUR))
                          +min(0,IRN - IRANT+TAXASSUR))/BINRIR)
         + (1 - positif(IHAUTREVT + CHRPVIMP)) * max(0,VAR9YIIR - IR9YI - TAXA9YI);

CHR9YI = max(0 , VAR9YIIR - IR9YI - TAXA9YI - CAP9YI) ;

CS9YP = positif(RDSC+MPSOL+CVNN+CDIS+CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (CSGC-CICSG-CSGIM)/BINRPS) 
        + (1-positif(RDSC+MPSOL+ CVNN+CDIS+CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820)) * VAR9YIPS;

RD9YP = positif(MPSOL + CVNN+CDIS+CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (RDSC-CIRDS-CRDSIM)/BINRPS) 
        + (1-positif(MPSOL + CVNN+CDIS+CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP);

PS9YP = positif(CVNN+CDIS+CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (MPSOL-CIPSOL-PRSPROV)/BINRPS)
       + (1-positif(CVNN+CDIS+CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP);  

CVN9YP = positif(CDIS+CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (CVNN - COD8YT)/BINRPS)
       +(1-positif(CDIS+CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP);

CDIS9YP = positif(CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (CDIS - CDISPROV)/BINRPS)
       +(1-positif(CGLOA+RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP);

GLO9YP = positif(RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (CSGLOA-COD8YL ) /BINRPS)
       +(1-positif(RSE1+RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP-CDIS9YP);

RSE19YP = positif(RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (RSE1N-CSPROVYD)/BINRPS)
       +(1-positif(RSE2+RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP-CDIS9YP-GLO9YP);

RSE29YP = positif(RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (RSE2N-CSPROVRSE2)/BINRPS)
       +(1-positif(RSE3+RSE4+RSE5+RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP-CDIS9YP-GLO9YP-RSE19YP);

RSE39YP = positif(RSE4+RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (RSE3N-CSPROVYG)/BINRPS)
       +(1-positif(RSE4+RSE5+RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP-CDIS9YP-GLO9YP-RSE19YP-RSE29YP);

RSE49YP = positif(RSE5+RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (RSE4N-CSPROVRSE4)/BINRPS)
       +(1-positif(RSE5+RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP-CDIS9YP-GLO9YP-RSE19YP-RSE29YP-RSE39YP);

RSE59YP = positif(RSE6+RSE8+MCSG820) * arr(VAR9YIPS * (RSE5N-CSPROVYE)/BINRPS)
       +(1-positif(RSE6+RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP-CDIS9YP-GLO9YP-RSE19YP-RSE29YP-RSE39YP-RSE49YP);

RSE69YP = positif(RSE8+MCSG820) * arr(VAR9YIPS * (RSE6N-COD8YQ)/BINRPS)
       +(1-positif(RSE8+MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP-CDIS9YP-GLO9YP-RSE19YP-RSE29YP-RSE39YP-RSE49YP-RSE59YP);

RSE89YP = positif(MCSG820) * arr(VAR9YIPS *  RSE8N/BINRPS)
       +(1-positif(MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP-CDIS9YP-GLO9YP-RSE19YP-RSE29YP-RSE39YP-RSE49YP-RSE59YP-RSE69YP);

C8209YP = positif(MCSG820) * arr(VAR9YIPS *(MCSG820-COD8ZH)/BINRPS)
       +(1-positif(MCSG820)) * max(0,VAR9YIPS-CS9YP-RD9YP-PS9YP-CVN9YP-CDIS9YP-GLO9YP-RSE19YP-RSE29YP-RSE39YP-RSE49YP-RSE59YP-RSE69YP-RSE89YP);

regle 101090:
application : bareme , iliad ;

IAMD28EA = IAD11 + IBATMARG + ITP + PVMTS + REI + AVFISCOPTER + VERSLIB + AUTOVERSSUP + IMPETAL19 + IMPETAL20 + IMPETAL21 + COD8UA + COD8UB ;

IAN8EA = max(0 , (IAMD28EA - AVFISCOPTER
                  - ((CIRCMAVFT + IRETS + ICREREVET + CIGLO + CIDONENTR + CICORSE + CIRECH + CICOMPEMPL) * (1 - positif(RE168 + TAX1649)))
                  + min(TAXASSUR+0 , max(0,INE-IRB+AVFISCOPTER))
                  + min(IPCAPTAXTOT+0 , max(0,INE-IRB+AVFISCOPTER - min(TAXASSUR+0,max(0,INE-IRB+AVFISCOPTER))))
                 )) ;
IRN8EA = min(0 , IAN8EA + AVFISCOPTER - IRE) + max(0 , IAN8EA + AVFISCOPTER - IRE) * positif(IAMD1 + 1 - SEUIL_61) ;

BTO = max(0 , IRN - IR9YI - IRANT) * (positif(IAMD1 + 1 - SEUIL_61) * (1 - positif(V_CNR2)) + positif(IAMD1 + 1 - SEUIL_TXMIN) * positif(V_CNR2)) ;

regle 101092:
application : bareme ,iliad ;


IRD = IRN * (positif(5 - V_IND_TRAIT)
              +
              (1-positif(5-V_IND_TRAIT)) * (
              positif_ou_nul(IRN+PIR-SEUIL_12) 
             + (1 - positif(IRN + PIR))
             ));

regle 101100:
application : iliad ;



CSGD = NAPCS - (V_CSANT - V_CSNANT) ;

RDSD = NAPRD - V_RDANT ;

CVND = NAPCVN - V_CVNANT ;

CGLOAD = NAPGLOA - V_GLOANT ;

CDISD = NAPCDIS - V_CDISANT ;
CSG820D = NAPCSG820 - V_CSG820ANT ;
CRSE1D = NAPRSE1 - V_RSE1ANT ;
CRSE2D = NAPRSE2 - V_RSE2ANT ;
CRSE3D = NAPRSE3 - V_RSE3ANT ;
CRSE4D = NAPRSE4 - V_RSE4ANT ;
CRSE5D = NAPRSE5 - V_RSE5ANT ;
CRSE6D = NAPRSE6 - V_RSE6ANT ;
CRSE8D = NAPRSE8 - V_RSE8ANT ;

regle 101110:
application : iliad;


CSBRUT = max(0,(CSGC + PCSG - CICSG - CSGIM)) ;

RDBRUT = max(0,(RDSC + PRDS - CIRDS - CRDSIM));

PSOLBRUT = max(0,MPSOL + PPSOL - CIPSOL - PRSPROV)  ;


CSNET = max(0 , CSGC + PCSG - CICSG - CSGIM );

RDNET = max(0,RDSC + PRDS - CIRDS - CRDSIM);

regle 101112:
application : iliad;

PSOLNET = max(0,MPSOL + PPSOL - CIPSOL - PRSPROV);
regle 101114:
application : iliad;


CVNNET  =  max(0,(CVNSALC + PCVN - CICVN - COD8YT));

CDISNET = max(0,(CDISC + PCDIS - CDISPROV))  ;
CSG820NET = max(0,(MCSG820 - COD8ZH  + PCSG820))  ;

CGLOANET = max(0,(CGLOA - CIGLOA - COD8YL + PGLOA))  ;

regle 101120:
application : iliad ;

RSE1NET = max(0,(RSE1 + PRSE1 - CIRSE1 - CSPROVYD))  ;

RSE2NET = max(0, RSE8TV - CIRSE8TV - CSPROVYF) 
        + max(0, RSE8SA - CIRSE8SA - CSPROVYN) + PRSE2 ;

RSE3NET = max(0,(RSE3 + PRSE3 - CIRSE3 - CSPROVYG))  ;

RSE4NET = max(0,(RSE4 + PRSE4 - CIRSE4 - CSPROVRSE4))  ;

RSE5NET = max(0,(RSE5 + PRSE5 - CIRSE5 - CSPROVYE))  ;

RSE6NET = max(0,(RSE6 - COD8YQ - CIRSE6 + PRSE6))  ;

RSE8NET = max(0,(RSE8 - CIRSE8 - COD8YV - COD8YX + PRSE8))  ;


RSENETTOT = RSE1NET + RSE2NET + RSE3NET + RSE4NET + RSE5NET + RSE8NET ;

regle 101122:
application : iliad ;
RASACOPS = COD8HX + COD8IX + COD8JX + COD8KX + COD8LX + COD8MX ;

RASACOCS = arr(RASACOPS * T_CSGCRDS/TXPASPS) ;

RASACOPSOL = RASACOPS - RASACOCS ;

RASCTXPS = COD8LJ + COD8LK + COD8LL + COD8HZ + COD8IZ + COD8JZ + COD8KZ + COD8LZ + COD8MZ ;

RASCTXCS = arr(RASCTXPS * T_CSGCRDS/TXPASPS) ;

RASCTXPSOL = RASCTXPS - RASCTXCS ;

AREMBPS = min(max(0 , COD8VL - COD8PB) , arr(COD8IE * TXPASPS/100)) ;

EPSOL = min(max(0 , COD8VL - COD8PB) , arr(COD8IF * TXPSOL/100)) ;

RESTITCS = arr((AREMBPS/TXPASPS)*T_CSGCRDS) ;

RESTITPSOL = max(0,AREMBPS - RESTITCS) + EPSOL ;

RESTITPS = RESTITCS + RESTITPSOL ;

PREVAUTSOC = (RSE1NET + RSE2NET + RSE3NET + RSE4NET + RSE5NET + RSE6NET + RSE8NET + CSG820NET + CGLOANET + RDNET + CVNNET + CDISNET)
	     * positif_ou_nul(CSNET + PSOLNET + RSE1NET + RSE2NET + RSE3NET + RSE4NET + RSE5NET + RSE6NET + RSE8NET + CSG820NET + CGLOANET + RDNET + CVNNET + CDISNET - SEUIL_61) ; 

CSNET1 = CSNET * (positif_ou_nul(CSNET + PSOLNET + RSE1NET + RSE2NET + RSE3NET + RSE4NET + RSE5NET + RSE6NET + RSE8NET + CSG820NET + CGLOANET + RDNET + CVNNET + CDISNET - SEUIL_61) 
                  + (1 - positif(NAPCRP))) ;
PSOLNET1 = PSOLNET * (positif_ou_nul(CSNET + PSOLNET + RSE1NET + RSE2NET + RSE3NET + RSE4NET + RSE5NET + RSE6NET + RSE8NET + CSG820NET + CGLOANET + RDNET + CVNNET + CDISNET - SEUIL_61) 
                      + (1 - positif(NAPCRP))) ;
PSNET = CSNET1 + PSOLNET1 ;

PRELCS = CSNET1 - RASACOCS + RASCTXCS - RESTITCS ;
PRELPSOL = PSOLNET1 - RASACOPSOL + RASCTXPSOL - RESTITPSOL ;
PRELPS = PRELCS + PRELPSOL ;

PREVSOCNET = PRELCS + PRELPSOL + PREVAUTSOC ;

TOTPSNET = CSNET1 + PSOLNET1 + PREVAUTSOC ;

regle 101130:
application : iliad;

TOTCRBRUT = max(0,CSGC+PCSG-CICSG-CSGIM + RDSC+PRDS-CIRDS-CRDSIM + MPSOL + PPSOL -CIPSOL-PRSPROV
                       + CVNSALC+PCVN-CICVN-COD8YT + CDISC+PCDIS-CDISPROV + CGLOA+PGLOA-COD8YL + MCSG820-COD8ZH 
                       + RSE1+PRSE1-CIRSE1-CSPROVYD + RSE2+PRSE2-CIRSE2-CSPROVRSE2 
                       + RSE3+PRSE3-CIRSE3-CSPROVYG + RSE4+PRSE4-CIRSE4-CSPROVRSE4
                       + RSE5+PRSE5-CIRSE5-CSPROVYE + RSE6+PRSE6-CIRSE6+ RSE8 + PRSE8-COD8YV - COD8YX);
TOTCRNET = CSNET + RDNET + PSOLNET + CVNNET + CDISNET + CSG820NET
          + CGLOANET + RSE1NET + RSE2NET + RSE3NET + RSE4NET 
          + RSE5NET + RSE6NET + RSE8NET ;

regle 101140:
application : iliad ;


IARD = IAR - IAR_A ;

regle 101150:
application : iliad ;


PIRD = PIR * (positif(5 - V_IND_TRAIT)
              +
              (1-positif(5-V_IND_TRAIT)) * (
              positif_ou_nul(IRN+PIR-SEUIL_12) 
              + 
              (1-positif(IRN+PIR)) 
             ))
    - 
              PIR_A * ( positif_ou_nul(PIR_A-SEUIL_12) 
               + 
              (1-positif(PIR_A))  
              );
PCSGD = PCSG* CSREC - PCSG_A * CSRECA ;
PRDSD = PRDS * CSREC - PRDS_A * CSRECA;
PTOTD = PIRD ;

regle 101160:
application : iliad;

BPSOL = arr(( RDRFPS + max(0,NPLOCNETSF)) * V_CNR
        + max( 0, RDRV + RDRCM01 + RDRFPS + COD8XK + COD8YK + RDNP + RDNCP + RDPTP + ESFP + R1649 + PREREV+ PVTERPS
              ) * (1-V_CNR) 
      ) * (1 - positif(present(RE168) + present(TAX1649)))
      + (RE168 + TAX1649) * (1-V_CNR) ;


regle 101170:
application : iliad;


MPSOL = arr(max(0,(BPSOL-(PVTERPS * (1-V_CNR)))) * TXPSOL/100 + (PVTERPS *TX068/100) * (1-V_CNR))* (1 - positif(ANNUL2042)) ;
regle 101190:
application : iliad;


CSGC = arr(((BCSG - (PVTERPS * (1-V_CNR))) * T_CSGCRDS / 100) + (PVTERPS * T_RDS/100) * (1-V_CNR)) * (1 - positif(ANNUL2042)) ;

regle 101200:
application : iliad ;

RSE1 = arr(BRSE1 * TXTQ/100) * (1 - positif(ANNUL2042)) ;

BRSE8TV = ALLECS * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;
BRSE8SA = COD8SA * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

RSE2 = (arr(BRSE8TV * TXTV/100) + arr(BRSE8SA * TXTV/100)) * (1 - positif(ANNUL2042)) ;

RSE3 = arr(BRSE3 * TXTW/100) * (1 - positif(ANNUL2042)) ;

RSE4 = arr(BRSE4 * TXTX/100) * (1 - positif(ANNUL2042)) ;
BRSE8TX = PENECS * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;
BRSE8SB = COD8SB * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

RSE5 = arr(BRSE5 * TXTR/100) * (1 - positif(ANNUL2042)) ;

RSE6 = arr(BRSE6 * TXCASA/100) * (1 - positif(ANNUL2042)) ;

RSE8 = arr(BRSE8 * TX066/100) * (1 - positif(ANNUL2042)) ;

RSETOT = RSE1 + RSE2 + RSE3 + RSE4 + RSE5 + RSE8 ;

regle 101210:
application : iliad;

CSG = max(0 , CSGC - CICSG ) ;
RDSN = max(0 , RDSC - CIRDS ) ;
PSOL = max(0,MPSOL - CIPSOL );
CVNN = max (0,CVNSALC - CICVN) ;
CSGLOA = CGLOA - CIGLOA;
CICPQ = CSGLOA;

RSE1N = max(0,RSE1 - CIRSE1) ;
RSE2N = max(0,RSE2 - CIRSE2) ;
RSE3N = max(0,RSE3 - CIRSE3) ;
RSE4N = max(0,RSE4 - CIRSE4) ;
RSE5N = max(0,RSE5 - CIRSE5) ;
RSE6N = max(0,RSE6 - CIRSE6) ;
RSE8N = max(0,RSE8 - CIRSE8) ;

regle 101220:
application : iliad;

RDRF = max(0 , RFCF + RFMIC + CODRBE - MICFR - RFDANT) * (1 - positif(ART1731BIS))
      + max(0 , RFCF + RFMIC + CODRBE - MICFR + DEFRF4BC)  * positif(ART1731BIS);

RDRFPS = max(0 , RFCFPS + RFMIC + CODRBE - MICFR - MICFRQ - RFDANT) * (1 - positif(ART1731BIS))
         + (max(0 , RFCFPS + RFMIC + CODRBE - MICFR - MICFRQ - RFDANT + DEFRF4BC)+DEFRFNONI) * positif(ART1731BIS) ;

RDRFAPS = max(0 , RFCFAPS + RFMIC + COD4BK - MICFR - RFDANT) * (1 - positif(ART1731BIS))
          + max(0 , RFCFAPS + RFMIC + COD4BK - MICFR + DEFRF4BD + DEFRF4BC) * positif(ART1731BIS) ;

RDRCM1 = max(0 , TRCMABD + DRTNC + RCMAV + PROVIE + RCMNAB + RTCAR + REGPRIV + RESTUC
                 + COD2VV + COD2WW + COD2YY + COD2ZZ + COD2VN + COD2VO + COD2VP + RCMIMPAT + CODRYY 
                 + COD2TQ + COD2TT + COD2TZ + COD2RB + COD2RC + COD2RD - RCMRDS - RCMSOC - COD2DF - COD2DG) ;

RDRCM1NEG = min(0 , RDRCM1) ;
RDRCM1NEGPLAF = abs(RDRCM1) ;
RDRCM1BIS = (1-positif(RDRCM1)) * RDRCM1NEGPLAF * (-1)
           + positif_ou_nul(RDRCM1) * RDRCM1;

RDRCM01 = RDRCM1BIS ;


RDRCM2 = COD2DI ;

RDRCM = RDRCM1 + RDRCM2 ;

RDRCM1APS = max(0 , RCMABD + RCMTNC + RCMAV + RCMHAD + RCMHAB + REGPRIV
                    + COD2VV + COD2WW + COD2YY + COD2ZZ + COD2VN + COD2VO + COD2VP
	            + RCMIMPAT + COD2TQ + CODRYY +  COD2TT + COD2TZ + COD2RB + COD2RC + COD2RD - RCMSOC - RCMRDS
	            - COD2DF - COD2DG) ;
		       
RDRCM1NEGAPS = min(0,RDRCM1APS);
RDRCM1NEGPLAFAPS = abs(RDRCM1APS);
RDRCM1BISAPS = (1-positif(RDRCM1APS)) * RDRCM1NEGPLAFAPS * (-1)
           + positif_ou_nul(RDRCM1APS) * RDRCM1APS;
RDRCMAPS = RDRCM1BISAPS ;
RDRV =   arr((RVB1 + COD1AR + RENTAX+CODRAR) * TXRVT1 / 100)
       + arr((RVB2 + COD1BR + RENTAX5+CODRBR) * TXRVT2 / 100)
       + arr((RVB3 + COD1CR + RENTAX6+CODRCR) * TXRVT3 / 100)
       + arr((RVB4 + COD1DR + RENTAX7+CODRDR) * TXRVT4 / 100);


RDNP = ( RCSV + RCSC + RCSP + max(0,NPLOCNETSF)) * (1-V_CNR) 
        + max(0,NPLOCNETSF) * V_CNR ;

        
PVTAUXPS = BPV18V + BPCOPTV + BPV40V + BPCOSAV + BPCOSAC + BPVSJ + BPVSK + COD3PI + COD3AN ;

RDNCP = PVBARPS + PVTAUXPS + PVTZPS ;



RDPTPA = max(0,BAF1AV-COD5XN)+ BA1AV + max(0,MIB1AV - MIBDEV) + BI1AV + max(0,MIBNP1AV - MIBNPDEV) + BI2AV + max(0,BNCPRO1AV - BNCPRODEV) + BN1AV + max(0,BNCNP1AV - BNCNPDEV) + PVINVE + PVSOCV + COD5QJ ; 


RDPTPB = max(0,BAF1AC-COD5YN) +BA1AC + max(0,MIB1AC - MIBDEC) + BI1AC + max(0,MIBNP1AC - MIBNPDEC) + BI2AC + max(0,BNCPRO1AC - BNCPRODEC) + BN1AC + max(0,BNCNP1AC - BNCNPDEC) + PVINCE + PVSOCC + COD5RJ ; 

RDPTPC = max(0,BAF1AP-COD5ZN) + BA1AP + max(0,MIB1AP - MIBDEP) + BI1AP + max(0,MIBNP1AP - MIBNPDEP) + BI2AP + max(0,BNCPRO1AP - BNCPRODEP) + BN1AP + max(0,BNCNP1AP - BNCNPDEP) + PVINPE + COD5SJ ;

RDPTP = RDPTPA + RDPTPB + RDPTPC ;


RGLOA = GLDGRATV + GLDGRATC;


regle 101225:
application : iliad ;

REVNONASSU = (1 - positif(COD8SH + COD8SI)) * (CODZRU + CODZRV)
             + positif(COD8SH) * positif(V_0AC + V_0AD + V_0AV) * (RDRV + RDRCM01 + RDRFPS + COD8XK + COD8YK + RDNP + RDNCP + RDPTP) * (1-V_CNR)
             + positif(COD8SH) * positif(V_0AC + V_0AD + V_0AV) * (RDRFPS + max(0,NPLOCNETSF) + CODZRV) * (V_CNR)

             + positif(COD8SH) * positif(COD8SI) * BOOL_0AM * (RDRV + RDRCM01 + RDRFPS + COD8XK + COD8YK + RDNP + RDNCP + RDPTP) * (1-V_CNR)
             + positif(COD8SH) * positif(COD8SI) * BOOL_0AM * (RDRFPS + max(0,NPLOCNETSF) + CODZRV) * (V_CNR)

             + positif(COD8SH) * (1 - positif(COD8SI)) * BOOL_0AM * (min(COD8RV,RDRV) + min(COD8RC,RDRCM01) + min(COD8RF,RDRFPS + COD8XK + COD8YK) + RDPTPA + min(COD8RM,RDNCP)+ min((RCSV+max(0,NPLOCNETSV)),RDNP) + (CODZRU + CODZRV))* (1-V_CNR)
             + positif(COD8SH) * (1 - positif(COD8SI)) * BOOL_0AM * (min(COD8RF,RDRFPS) + max(0,NPLOCNETSV) + CODZRV) * (V_CNR)

             + positif(COD8SI) * (1 - positif(COD8SH)) * BOOL_0AM * (min(COD8RV,RDRV) + min(COD8RC,RDRCM01) + min(COD8RF,RDRFPS + COD8XK + COD8YK) +RDPTPB +min(COD8RM,RDNCP)+ min((RCSC+max(0,NPLOCNETSC)),RDNP) + (CODZRU + CODZRV))* (1-V_CNR)
             + positif(COD8SI) * (1 - positif(COD8SH)) * BOOL_0AM * (min(COD8RF,RDRFPS) + max(0,NPLOCNETSC) + CODZRV) * (V_CNR)
           ;

regle 101226:
application : iliad;

BCSG = arr(max(0 , RDRFPS +  max(0 , NPLOCNETSF) - REVNONASSU) * V_CNR
           + max(0 , RDRV + RDRCM01 + RDRCM2 + RDRFPS + COD8XK + COD8YK + RDNP + RDNCP + RDPTP + PVTERPS + ESFP + R1649 + PREREV - REVNONASSU) * (1 - V_CNR))
           * (1 - positif(present(RE168) + present(TAX1649)))
           + (RE168 + TAX1649) * (1 - V_CNR) ; 

BCSGNORURV = (1-positif(COD8SH))*(
             arr(RDRFPS + max(0,NPLOCNETSF) -REVNONASSU * V_CNR
                 + ( RDRV + RDRCM01 + RDRFPS + COD8XK + COD8YK + RDNP + RDNCP + RDPTP + ESFP + R1649 + PREREV - REVNONASSU ) * (1-V_CNR)
                ) * (1 - positif(present(RE168) + present(TAX1649)))
             + (RE168 + TAX1649) * (1-V_CNR) 
	     )
	     + positif(COD8SH)*0 ;

BRSE1 = SALECS * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BRSE2 = (ALLECS + COD8SA) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BRSE3 = (INDECS + COD8SW) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BRSE4 = (PENECS + COD8SB + COD8SX) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BRSE5 = (SALECSG + COD8SC)  * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BRSE6 = (ALLECS + COD8SA + COD8SC + COD8TH + COD8SD) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BRSE8 = (COD8TH + COD8SD) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BRSETOT = BRSE1 + BRSE2 + BRSE3 + BRSE4 + BRSE5 + BRSE8 ;

regle 101230:
application : iliad;


RETRSETOT = RETRSE1 + RETRSE2 + RETRSE3 + RETRSE4 + RETRSE5 + RETRSE8;
RSEPROVTOT = CSPROVYD + CSPROVYE + CSPROVYF + CSPROVYN + CSPROVYG + CSPROVYH + CSPROVYP + COD8YV + COD8YX ;
NMAJRSE1TOT = NMAJRSE11 + NMAJRSE21 + NMAJRSE31 + NMAJRSE41 + NMAJRSE51 + NMAJRSE81 ;
NMAJRSE4TOT = NMAJRSE14 + NMAJRSE24 + NMAJRSE34 + NMAJRSE44 + NMAJRSE54 + NMAJRSE84 ;

regle 101240:
application : iliad;


BDCSG = (min (BCSG , max (0, RDRV + positif(COD2OP)* (max(0,RDRCM1+ RDRCM2)+ PVBARPS)  + RDRFPS + RDNP +(1-positif(COD2OP))*COD3XN - IPPNCS - 8SGAUTO - REVNONASSU + (1-positif(COD2OP))*(min(COD8RC,RDRCM1)+min(COD8RM,RDNCP)))) 
                    * (1 - positif (ABDETPLUS + COD3SL + CODRVA + CODRSL + COD1TZ + COD1UZ + COD1VZ + COD1WZ))

       + min ( BCSG  , BDCSG3VAVB)     
                     * positif( ABDETPLUS + COD3SL + CODRVA + CODRSL + COD1TZ + COD1UZ + COD1VZ + COD1WZ)  
         ) * (1 - positif(present(RE168)+present(TAX1649))) * (1-V_CNR);


BDCSGNORUYT = ( min ( BCSGNORURV , max( 0, RDRFPS+RDRV +RDNP+ positif(COD2OP)* (max(0,RDRCM1 + RDRCM2) + PVBARPS) - IPPNCS - 8SGAUTO))
                   * (1- positif(ABDETPLUS + COD1TZ + COD1UZ + COD1VZ+COD3UA + COD3SL))

        + min ( BCSGNORURV  , BDCSG3VAVB)
                   * positif(ABDETPLUS + COD1TZ + COD1UZ + COD1VZ+COD3UA + COD3SL)

        ) * (1 - positif(present(RE168)+present(TAX1649))) * (1-V_CNR) ;


regle 101250:
application : iliad;

DGLOD = positif(CSREC + V_GLOANT) * max(0 , arr((BGLOA - COD8XM - (COD8YL/0.075)) * TX068/100)) * (1 - positif(present(RE168) + present(TAX1649))) * positif(NAPC61) ;

IDGLO = max(0 , arr((BGLOA - COD8XM - (COD8YL/0.075))* TX068/ 100)) * positif(CSREC) * null(V_IND_TRAIT - 4)
        + abs(DGLOD - V_IDGLOANT) * (1 - null(V_IND_TRAIT - 4)) ;

CSGDED3UA =  positif (COD2OP)*((( COD3UA + CODRUA + 0) * TX068/100)* ((COD3UA + CODRUA - COD3SL - CODRSL - CODRVA - ABDETPLUS) /(COD3UA + CODRUA + 0))) ; 
CSGDEDAUTRE = positif (COD2OP)*(arr((PVBARPS - (COD3UA + CODRUA)+ 0
                                                    ) * TX068/100
                                                  )) ; 

CSGDED = positif(COD2OP)* max(0 , CSGDED3UA + CSGDED3UB + CSGDED3UO + CSGDED3UP + CSGDED3UY + CSGDEDAUTRE) ;
CSGDED1TZ =  (PVTZPS * TX068/100) * (COD1TZ / (COD1TZ + COD1UZ + COD1VZ + COD1WZ));

PVTZBIS = arr((CSGDED1TZ/TX068)*100)+0; 
PVBAR3VAVB = positif (COD2OP) * positif(CSGDED)* arr( CSGDED * 100/TX068) ;

BDCSG3VAVB = max(0, RDRV + positif(COD2OP)* max(0,RDRCM1 + RDRCM2) + RDRFPS + RDNP + PVBAR3VAVB + PVTZBIS + (1-positif(COD2OP))* COD3XN - IPPNCS - 8SGAUTO - REVNONASSU+(1-positif(COD2OP))* min(COD8RC,RDRCM1)+min(COD8RM,RDNCP) ) * (1-V_CNR) * (1 - positif(present(RE168)+present(TAX1649)));

regle 101260:
application : iliad ;


BDRSE1 = max(0,SALECS - min( SALECS,REVCSXA ) - arr(CSPROVYD/(TX092/100))
            ) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BDRSE2 = max(0,ALLECS - min( ALLECS,REVCSXC ) - arr(CSPROVYF/(TXTV/100))
            ) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BDRSE3 = max(0,INDECS + COD8SW - min( INDECS + COD8SW,REVCSXD ) - arr(CSPROVYG/(TXTW/100))
            ) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BDRSE4 = max(0,PENECS + COD8SX - min( PENECS + COD8SX,REVCSXE ) - arr(CSPROVYH/(TXTX/100))
            ) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;

BDRSE5 = max(0,SALECSG + COD8SC - min( SALECSG + COD8SC,REVCSXB ) - arr(CSPROVYE/(TX092/100))
            ) * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;


BDRSE8 = max(0,COD8TH - min(COD8XO,COD8TH) - arr(COD8YV/(TX066/100)))
            * (1 - positif(present(RE168) + present(TAX1649))) * (1-V_CNR) ;


DRSED = (arr(BDRSE1 * TXTQDED/100) + arr(BDRSE2 * TXTVDED/100)
	   + arr(BDRSE3 * TXTWDED/100) + arr(BDRSE4 * TXTXDED/100) + arr(BDRSE5 * TXTRDED/100)
           + arr(BDRSE8 * TX042/100)) * positif(CSREC+V_IDRSEANT) * positif(NAPC61) ;

IDRSE = (arr(BDRSE1 * TXTQDED/100) + arr(BDRSE2 * TXTVDED/100) + arr(BDRSE3 * TXTWDED/100) + arr(BDRSE4 * TXTXDED/100) + arr(BDRSE5 * TXTRDED/100) + arr(BDRSE8 * TX042/100)) * positif(CSREC) * null(V_IND_TRAIT - 4)
        + abs(DRSED - V_IDRSEANT) * (1 - null(V_IND_TRAIT - 4)) ;

regle 101270:
application : iliad ;


DCSGD = positif(CSRECB+V_IDANT) * max( 0, arr((BDCSG * TX068 / 100)+ (COD3WN * TX051/100)) - DCSGIM)  * positif(NAPC61);

IDCSG = max(0 , arr(BDCSG * T_IDCSG / 100) + (COD3WN * TX051/100) - DCSGIM) * positif(CSRECB) * null(V_IND_TRAIT - 4)
        + abs(DCSGD - V_IDANT) * (1 - null(V_IND_TRAIT - 4)) ;

regle 101280:
application : iliad;


REVREMP = SALECS + SALECSG + ALLECS + INDECS + PENECS + COD8SA + COD8SB + COD8SC + COD8SW + COD8SX + COD8TH + COD8SD ;

	                  
BRDS = (1 - positif(COD8RP + COD8RQ)) * (REVREMP + RGLOA ) * (1 - V_CNR) 
        + positif(COD8RP) * (1 - positif(COD8RQ)) * (REVREMP + GLDGRATC ) * (1 - V_CNR) 
        + positif(COD8RQ) * (1 - positif(COD8RP)) * (REVREMP + GLDGRATV ) * (1 - V_CNR)
	+ positif(COD8RP * COD8RQ) * 0 * (1 - V_CNR)
        + 0 ;
        

regle 101290:
application : iliad;


RDSC = arr( BRDS * T_RDS / 100 ) * (1 - positif(ANNUL2042)) ;

regle 101310:                                                             
application : iliad;                               

                                                                          
CSRTF = (RDPTP + PVINVE+PVINCE+PVINPE 
         + somme(i=V,C,P:BN1Ai + BI1Ai                          
         + BI2Ai + BA1Ai )); 
RDRTF = CSRTF ;                                                          
PSRTF = CSRTF ;                                                          

regle 101320:
application : iliad;

BASSURV3 = max(0,CESSASSV - LIM_ASSUR3);
BASSURV2 = max(0,CESSASSV - BASSURV3 - LIM_ASSUR2);
BASSURV1 = max(0,CESSASSV - BASSURV3 - BASSURV2 - LIM_ASSUR1);
BASSURC3 = max(0,CESSASSC - LIM_ASSUR3);
BASSURC2 = max(0,(CESSASSC -BASSURC3) - LIM_ASSUR2);
BASSURC1 = max(0,(CESSASSC - BASSURC3 -BASSURC2) - LIM_ASSUR1);
BASSURV = CESSASSV;
BASSURC = CESSASSC;

TAXASSURV = arr(BASSURV1 * TX_ASSUR1/100 + BASSURV2 * TX_ASSUR2/100 + BASSURV3 * TX_ASSUR3/100) * (1 - positif(RE168 + TAX1649));
TAXASSURC = arr(BASSURC1 * TX_ASSUR1/100 + BASSURC2 * TX_ASSUR2/100 + BASSURC3 * TX_ASSUR3/100) * (1 - positif(RE168 + TAX1649));
TAXASSUR = TAXASSURV + TAXASSURC ;

regle 101330:
application : iliad;

BCVNSAL = (1-positif(COD8SH +COD8SI + COD8RP + COD8RQ))* (CVNSALAV + GLDGRATV + GLDGRATC) * (1-positif(present(TAX1649)+present(RE168)))
        +  positif(COD8SH +COD8RP)* positif(V_0AC + V_0AD + V_0AV) * (0)
	+ positif(positif(COD8SH)* (1-positif(COD8SI)) + positif(COD8RP)*(1-positif(COD8RQ))) * positif(V_0AM + V_0AO) * (CVNSALAV + GLDGRATC)
	+ positif(positif(COD8SI)* (1-positif(COD8SH)) + positif(COD8RQ)*(1-positif(COD8RP))) * positif(V_0AM + V_0AO) * (CVNSALAV + GLDGRATV)  
	+ positif((COD8SH * COD8SI) + (COD8RP*COD8RQ)) * positif(V_0AM + V_0AO) * (0);  
	
CVNSALC = arr( BCVNSAL * TX10 / 100 ) * (1 - positif(ANNUL2042));

B3SVN  = CVNSALAV * (1 - positif(present(TAX1649) + present(RE168)));

BGLOA = (1-positif(COD8RP + COD8RQ)) * RGLOA * (1-V_CNR) * (1-positif(present(TAX1649)+present(RE168)))
      + positif(COD8RP)* (1-positif(COD8RQ)) * GLDGRATC * (1-V_CNR) * (1-positif(present(TAX1649)+present(RE168))) 
      + positif(COD8RQ) * (1-positif(COD8RP)) * GLDGRATV * (1-V_CNR) * (1-positif(present(TAX1649)+present(RE168))) ; 

CGLOA = arr( BGLOA * T_CSG / 100 ) * (1 - positif(ANNUL2042));

BGLOACNR = (GLDGRATV+GLDGRATC) * V_CNR * (1-positif(present(TAX1649)+present(RE168)));

regle 101345:
application : iliad;

BCDIS = (GSALV + GSALC) * (1 - V_CNR)* (1-positif(present(TAX1649)+present(RE168))) ;

CDISC = arr(BCDIS * TCDIS / 100) * (1 - positif(ANNUL2042)) ;

CDIS = CDISC ;
regle 101350:
application : iliad;

BCSG820 = PVTERPS * (1 - V_CNR)* (1-positif(present(TAX1649)+present(RE168))) ;

MCSG820 = arr(BCSG820 * TX082 / 100) * (1 - positif(ANNUL2042)) ;









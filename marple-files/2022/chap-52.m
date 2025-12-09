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
regle 521000:
application : bareme , iliad ;
 

TXBAR0 = TX_MOD0 * FLAG_BAREM + TX_BAR0 * (1 - FLAG_BAREM) ;
TXBAR1 = TX_MOD1 * FLAG_BAREM + TX_BAR1 * (1 - FLAG_BAREM) ;
TXBAR2 = TX_MOD2 * FLAG_BAREM + TX_BAR2 * (1 - FLAG_BAREM) ;
TXBAR3 = TX_MOD3 * FLAG_BAREM + TX_BAR3 * (1 - FLAG_BAREM) ;
TXBAR4 = TX_MOD4 * FLAG_BAREM + TX_BAR4 * (1 - FLAG_BAREM) ;
TXBAR5 = TX_MOD5 * FLAG_BAREM + TX_BAR5 * (1 - FLAG_BAREM) ;
TXBAR6 = TX_MOD6 * FLAG_BAREM + TX_BAR6 * (1 - FLAG_BAREM) ;

TAUX1 = positif(TXBAR1) * (TXBAR1 - TXBAR0) ;
TAUX2 = positif(TXBAR2) * (TXBAR2 - TXBAR1) ;
TAUX3 = positif(TXBAR3) * (TXBAR3 - TXBAR2) ;
TAUX4 = positif(TXBAR4) * (TXBAR4 - TXBAR3) ;
TAUX5 = positif(TXBAR5) * (TXBAR5 - TXBAR4) ;
TAUX6 = positif(TXBAR6) * (TXBAR6 - TXBAR5) ;

LIMINFBAR1 = LIM_INF_MOD1 * FLAG_BAREM + LIM_INF_BAR1 * (1 - FLAG_BAREM) ;
LIMINFBAR2 = LIM_INF_MOD2 * FLAG_BAREM + LIM_INF_BAR2 * (1 - FLAG_BAREM) ;
LIMINFBAR3 = LIM_INF_MOD3 * FLAG_BAREM + LIM_INF_BAR3 * (1 - FLAG_BAREM) ;
LIMINFBAR4 = LIM_INF_MOD4 * FLAG_BAREM + LIM_INF_BAR4 * (1 - FLAG_BAREM) ;
LIMINFBAR5 = LIM_INF_MOD5 * FLAG_BAREM + LIM_INF_BAR5 * (1 - FLAG_BAREM) ;
LIMINFBAR6 = LIM_INF_MOD6 * FLAG_BAREM + LIM_INF_BAR6 * (1 - FLAG_BAREM) ;

LIMSUPBAR1 = LIM_SUP_MOD1 * FLAG_BAREM + LIM_SUP_BAR1 * (1 - FLAG_BAREM) ;
LIMSUPBAR2 = LIM_SUP_MOD2 * FLAG_BAREM + LIM_SUP_BAR2 * (1 - FLAG_BAREM) ;
LIMSUPBAR3 = LIM_SUP_MOD3 * FLAG_BAREM + LIM_SUP_BAR3 * (1 - FLAG_BAREM) ;
LIMSUPBAR4 = LIM_SUP_MOD4 * FLAG_BAREM + LIM_SUP_BAR4 * (1 - FLAG_BAREM) ;
LIMSUPBAR5 = LIM_SUP_MOD5 * FLAG_BAREM + LIM_SUP_BAR5 * (1 - FLAG_BAREM) ;
LIMSUPBAR6 = LIM_SUP_MOD6 * FLAG_BAREM + LIM_SUP_BAR6 * (1 - FLAG_BAREM) ;

regle 521010:
application : bareme , iliad ;

pour x=0,5;y=1,2;z=1,2:
DSxyz = somme(i=1..6: max(QFxyz - LIMINFBARi, 0) * (TAUXi / 100)) ;

regle 521020:
application : iliad ;

WTXMARJ = (RB51) / ( NBPT * null(PLAFQF) + (1 + BOOL_0AM + BOOL_0AZ * V_0AV) *null(1-PLAFQF)) ;


VARTMPTAB1[NOMBRE0] = 0;
pour i=1-NB_TRANCHES_BAR:
VARTMPTAB1[NOMBREi] = positif(WTXMARJ - LIMINFBARi) * TXBARi;

VARTMP1 = max(0, multimax(NB_TRANCHES_BAR+1, VARTMPTAB1));

TXMARJ = VARTMP1
          * ( 1 - positif ( 
                              present ( NRBASE ) 
                            + present ( NRINET ) 
                            + present ( BASRET )
                            + present ( IMPRET )
                          )              
             )
          * (1 - V_CNR * (1 - positif(present(IPTXMO) + present(RMOND))))
  * positif(IN01+IPQ1001);


TXMARJBA = VARTMP1
  * positif(IN01+IPQ1001);

VARTMP1 = 0 ;
regle 521030:
application : bareme , iliad ;
 

pour y=1,2:
DS0y4 = somme(i=1..6: max(QF0y4 - LIMINFBARi, 0) * (TAUXi / 100)) ;

pour x=0,5;y=1,2:
DSxy5 = somme(i=1..6: max(QFxy5 - LIMINFBARi, 0) * (TAUXi / 100)) ;

pour y=1,2:
DS0y6 = somme(i=1..6: max(QF0y6 - LIMINFBARi, 0) * (TAUXi / 100)) ;

regle 521040:
application : bareme , iliad ;

NBYV1 = NBPT;
NBYV2 = 1 + BOOL_0AM + BOOL_0AZ * V_0AV;

regle 521050:
application : bareme , iliad ;
QF011 = arr(RB01) / NBYV1;
QF021 = arr(RB01) / NBYV2;
QF012 = arr(RB02) / NBYV1;
QF022 = arr(RB02) / NBYV2;
QF511 = arr(RB51) / NBYV1;
QF521 = arr(RB51) / NBYV2;
QF512 = arr(RB52) / NBYV1;
QF522 = arr(RB52) / NBYV2;
QF014 = arr(RB04) / NBYV1;
QF024 = arr(RB04) / NBYV2;
QF015 = arr(RB05) / NBYV1;
QF515 = arr(RB55) / NBYV1;
QF025 = arr(RB05) / NBYV2;
QF525 = arr(RB55) / NBYV2;
QF016 = arr(RB06) / NBYV1;
QF026 = arr(RB06) / NBYV2;

regle corrective 521070:
application : iliad ;

CFRIAHP = ARESTIMO + ALOGDOM + ADUFREP + APIREPAI
         + APIREPBI + APIREPCI + APIREPDI + APIREPCZ + APIREPEZ 
         + ACELREPYP + ACELREPYO
         + ACELREPYN + ACELREPYM + ACELREPYW + ACELREPYV
         + ACELREPYU + ACELREPYT 
	 + AILMPF + AILMPK + AILMPG + AILMPL 
	 + AILMPH + AILMPM + AILMPI + AILMPN + AILMPJ + AILMPO 
	 + AILMJY + AILMJX + AILMJW + AILMJV + AILMOE + AILMOD + AILMOC + AILMOB + AILMOA
         + AILMOJ + AILMOI + AILMOH + AILMOG + AILMOF + AILMOO + AILMON + AILMOM + AILMOL + AILMOK
         + ARESIVIEU + ARESINEUV + ALOCIDEFG + ACODJTJU + ACODOU + ACODOW + ACODOX + ACODOY + ACODPZ;

CFRIRHP =  RRESTIMO + RLOGDOM + RDUFREP + RPIREPBI
          + RPIREPDI + RPIREPCZ + RPIREPEZ
          + RCELREPYP + RCELREPYO
          + RCELREPYN + RCELREPYM + RCELREPYW + RCELREPYV 
	  + RCELREPYU + RCELREPYT 
	  + RILMPF + RILMPK + RILMPG + RILMPL 
	  + RILMPH + RILMPM + RILMPI + RILMPN + RILMPJ + RILMPO 
          + RILMJY + RILMJX + RILMJW + RILMJV 
          + RILMOJ + RILMOI + RILMOH + RILMOG + RILMOF + RILMOO + RILMON + RILMOM + RILMOL + RILMOK
	  + RILMOT + RILMOS + RILMOR + RILMOQ + RILMOP
          + RRESIVIEU + RRESINEUV + RLOCIDEFG + RCODJTJU + RCODOU + RCODOV + RCODOW + RCODOX + RCODOY + RCODPZ;
CFRIADON = AREPA + ADONS;
CFRIRDON = RREPA + RDONS;
CFRIAENF = APRESCOMP;
CFRIRENF = RPRESCOMP + RRETU;
CFRIADEP = AHEBE ;
CFRIRDEP = RHEBE ;
CFRIAFOR = BFCPI + ACOMP + AFOREST + AFORET + ANOUV + ALOCENT + ALOGSOC + ACOLENT + ACOTFOR + ADOMSOC1 + AFIPDOM;
CFRIRFOR = RINNO + RCOMP + RFOREST + RFORET + RNOUV + RLOCENT + RLOGSOC + RCOLENT + RCOTFOR + RDOMSOC1 + RFIPDOM;
CFRIAVIE = ASURV;
CFRIRVIE = RSURV;
CFRIAAUTRE = AFIPC + ADIFAGRI + ASOCREPR + ASOUFIP + ARIRENOV + APATNAT ;
CFRIRAUTRE = RFIPC + RDIFAGRI + RSOCREPR + RSOUFIP + RRIRENOV + RPATNAT ;
CFCIDIV = CIGARD + CISYND + CIADCRE + CIFORET;

TOTDEFRCM = DFRCM1 + DFRCM2+DFRCM3 +DFRCM4 +DFRCM5 +DFRCMN;
TOTDEFLOC = DEFLOC1 + DEFLOC2 +DEFLOC3 +DEFLOC4 +DEFLOC5 +DEFLOC6 +DEFLOC7 +DEFLOC8 +DEFLOC9 +DEFLOC10;
TOTMIBV = MIBRNETV + MIBNPRNETV + MIBNPPVV + MIBPVV - BICPMVCTV - MIBNPDCT;
TOTMIBC = MIBRNETC + MIBNPRNETC + MIBNPPVC + MIBPVC - BICPMVCTC -COD5RZ;
TOTMIBP = MIBRNETP + MIBNPRNETP + MIBNPPVP + MIBPVP - BICPMVCTP - COD5SZ;
TOTBNCV = SPENETPV + SPENETNPV + BNCPROPVV - BNCPMVCTV + BNCNPPVV - BNCNPDCT;
TOTBNCC = SPENETPC + SPENETNPC + BNCPROPVC - BNCPMVCTC + BNCNPPVC;
TOTBNCP = SPENETPP + SPENETNPP + BNCPROPVP - BNCPMVCTP + BNCNPPVP;
TOTSPEREP = SPEDREPV + SPEDREPC +SPEDREPP;
TOTSPEREPNP = SPEDREPNPV + SPEDREPNPC +SPEDREPNPP;
IRNINCFIR = IAN+AVFISCOPTER-IRE;


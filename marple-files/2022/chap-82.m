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
regle 821000:
application : iliad ;



RCMORDTOT = positif(COD2OP) * (RCMABD + RCMTNC + RCMAV + RCMHAD + RCMHAB + REGPRIV + COD2TT + COD2VV + COD2WW + COD2YY + COD2ZZ + COD2VN + COD2VO + COD2VP + COD2TQ + COD2RB + COD2RC + COD2RD + COD2TZ) 
          + (1-positif(COD2OP)) *  (RCMAV + COD2YY + COD2VN + COD2RB) ;


RCMQUOTOT = positif(COD2OP)*( REVACT + REVPEA + PROVIE + DISQUO + INTERE + RESTUC + CODRYY)
          + (1-positif(COD2OP)) * (PROVIE + CODRYY );

RCMTOT = positif(COD2OP)*( RCMORDTOT + RCMQUOTOT);

regle 821001:
application : iliad ;

RCMRABD = positif(COD2OP)* (arr(RCMABD * 40/100)) ;
2RCMRABD = arr(REVACT * 40/100) ;

TRCMRABD = RCMRABD + 2RCMRABD ;
RCMRTNC = positif(COD2OP)*( arr(RCMTNC * 40/100)) ;

2RCMRTNC = positif(COD2OP)*( arr(REVPEA * 40/100)) ;

TRCMRTNC = RCMRTNC + 2RCMRTNC ;


RCMRNABD = positif(COD2OP)*(max(0,RCMABD - RCMRABD)) ;

2RCMRNABD = positif(COD2OP)*( max(0,REVACT - 2RCMRABD)) ;
RCMRNTNC = positif(COD2OP)*( max(0,RCMTNC - RCMRTNC)) ;

2RCMRNTNC = positif(COD2OP)*( max(0,REVPEA - 2RCMRTNC)) ;


REGPRIVM = positif(COD2OP)*(arr(REGPRIV * MAJREV)) ;

2REGPRIVM = positif(COD2OP)*(arr(RESTUC * MAJREV)) ;

regle 821002:
application : iliad ;


REVPER = COD2RA + COD2RB + COD2RC + COD2RD ; 

REVPERNOR =  positif(REVPER - ABTAV) ;

EXO2RB =  min(ABTAV , COD2RB) ;
        
NETEXO2RB = positif(REVPERNOR) * max(0 , COD2RB - EXO2RB )
          + (1-positif(REVPERNOR)) * 0;

EXO2RA = min(max(0,(ABTAV-EXO2RB)) , COD2RA) ; 

NETEXO2RA = positif(REVPERNOR) * max(0 , COD2RA - EXO2RA ) 
          + (1-positif(REVPERNOR)) * 0;

EXO2RC = min(max(0,(ABTAV - EXO2RB - EXO2RA)) , COD2RC) ;

NETEXO2RC = positif(REVPERNOR) * max(0 , COD2RC - EXO2RC )
          + (1-positif(REVPERNOR)) * 0; 

EXO2RD = min(max(0,(ABTAV - EXO2RB - EXO2RA -EXO2RC)) , COD2RD) ;

NETEXO2RD = positif(REVPERNOR) * max(0 , COD2RD - EXO2RD ) 
          + (1-positif(REVPERNOR)) * 0 ;



RAVC = positif(COD2OP)*(RCMAV + PROVIE + RCMLIB + COD2VV + COD2WW + NETEXO2RA + NETEXO2RB + NETEXO2RC + NETEXO2RD) ;
regle 821003:
application : iliad ;

RAVCNORM1 = positif(COD2OP) * positif(RAVC - ABTAV) ;

RND2CHRCH = positif(COD2OP) * positif(RAVCNORM1) * (RCMAV + PROVIE) ;

ABT2CHRCH = positif(COD2OP) * positif(RAVCNORM1) * min(RND2CHRCH , ABTAV) ;

ABT2CH = positif(COD2OP) * positif(RAVCNORM1) * arr(ABT2CHRCH * (RCMAV / RND2CHRCH)) ;

ABTRCH = positif(COD2OP) * positif(RAVCNORM1) * (ABT2CHRCH - ABT2CH) ;

RNET2CH = positif(COD2OP) * positif(RAVCNORM1) * (RCMAV - ABT2CH) ;

RNETRCH = positif(COD2OP) * positif(RAVCNORM1) * (PROVIE - ABTRCH) ;

ABT2RB = positif(COD2OP) * positif(RAVCNORM1) * min(NETEXO2RB , (ABTAV - ABT2CHRCH) ) ;

RNET2RB = positif(COD2OP) * positif(RAVCNORM1) * max(0 ,(NETEXO2RB - ABT2RB)) ;  

RNET2DH = positif(COD2OP) * positif(RAVCNORM1) * max(0 ,RCMLIB - (ABTAV - ABT2CHRCH - ABT2RB)) ;

ABT2DH = positif(COD2OP) * positif(RAVCNORM1) * (RCMLIB - RNET2DH) + (1 - positif(RAVCNORM1)) * RCMLIB ;

ABT2RA = positif(COD2OP) * min ( NETEXO2RA , (ABTAV - ABT2CHRCH - ABT2RB - ABT2DH)) ;

RNET2RA = positif(COD2OP) * positif(RAVCNORM1) * max(0 , NETEXO2RA - ABT2RA) ; 

RNET2VV2WW = positif(COD2OP) * positif(RAVCNORM1) * positif(COD2VV + COD2WW + NETEXO2RC + NETEXO2RD) * max(0 , COD2VV + COD2WW + NETEXO2RC + NETEXO2RD - (ABTAV - ABT2CHRCH - ABT2RB - ABT2DH - ABT2RA)) ;

regle 821004:
application : iliad ;


RCMORDTOTNET = positif(COD2OP) * (RCMRNABD + RCMRNTNC + RNET2CH + REGPRIVM + RCMHAB + RCMHAD + COD2TT + RNET2RB + RNET2VV2WW + COD2YY + COD2ZZ + COD2VN + COD2VO + COD2VP + COD2TQ + COD2TZ) ;
RCMQUOTOTNET = positif(COD2OP) * (2RCMRNABD + 2RCMRNTNC + RNETRCH + 2REGPRIVM + INTERE + DISQUO +CODRYY) ;

REPRCM2TU = COD2TU + COD2TV + COD2TW + COD2TX +COD2TY ;
TRCMABD = RCMABD + REVACT ;

RCMAB = RCMRNABD + 2RCMRNABD ;

DRTNC = RCMTNC + REVPEA ;

RTNC = RCMRNTNC + 2RCMRNTNC ;

ABRCM2 = min( ABTAV , RAVC) ;
ABACH  =( positif(RCMAV)) * arr( ABRCM2 * RCMAV / RAVC ) ;
RCMRNCH = max(0,RCMAV - ABACH) ;

2ABACH =( positif(PROVIE)) * (min(arr( ABRCM2 * PROVIE / RAVC ) , max(0,ABRCM2 - ABACH))) ;
2RCMRNCH = max(0,PROVIE - 2ABACH) ;

TRCMRNCH = RCMRNCH + 2RCMRNCH ;

RCMNAB = RCMHAD + DISQUO ;

RTCAR = RCMHAB + INTERE ;

RCMPRIV = REGPRIV + RESTUC ;

RCMPRIVM = REGPRIVM + 2REGPRIVM ;

INDRCMDEF = positif(RCMFR - RCMORDTOT-RCMQUOTOT) ;
regle 821020:
application : iliad ;

DEFRCMN = positif(COD2OP)*(positif(RCMFR-(RCMORDTOTNET+RCMQUOTOTNET))) * ((RCMORDTOTNET + RCMQUOTOTNET) - RCMFR) ;

RCMFRORDI = positif(COD2OP) * (1-positif(DEFRCMN)) * (arr(RCMORDTOT*RCMFR / RCMTOT)) ;
RCMFRQUOT = positif(COD2OP) * (1-positif(DEFRCMN)) * ( max(0,RCMFR - RCMFRORDI)) ;


INDRCMDEFQ = positif(RCMQUOTOTNET - RCMFRQUOT1) ;
regle 821022:
application : iliad ;

RCMORDNETB = positif(COD2OP) * (positif(RCMFRQUOT-RCMQUOTOTNET)) * max(0,(RCMORDTOTNET - RCMFRORDI)+(RCMQUOTOTNET-RCMFRQUOT)) ;

regle 821030:
application : iliad ;

RCMORDNETA = positif(COD2OP) * (1-positif(RCMFRQUOT-RCMQUOTOTNET)) * max(0,(RCMORDTOTNET - RCMFRORDI))*(1-positif(DEFRCMN)) ;

RCMORDNET = RCMORDNETA + RCMORDNETB ;

2RCMFRDC = positif(COD2OP) * (positif(RCMQUOTOTNET-RCMFRQUOT)) * (arr(RCMFRQUOT*(REVACT/RCMQUOTOT))) ;
2RCMFRFU = positif(COD2OP) * (positif(RCMQUOTOTNET-RCMFRQUOT)) * (arr(RCMFRQUOT*(REVPEA/RCMQUOTOT))) ;
2RCMFRCH = positif(COD2OP) * (positif(RCMQUOTOTNET-RCMFRQUOT)) * (arr(RCMFRQUOT*(PROVIE/RCMQUOTOT))) ;
2RCMFRTR = positif(COD2OP) * (positif(RCMQUOTOTNET-RCMFRQUOT)) * (arr(RCMFRQUOT*(INTERE/RCMQUOTOT))) ;
2RCMFRYY = positif(COD2OP) * (positif(RCMQUOTOTNET-RCMFRQUOT)) * (arr(RCMFRQUOT*(CODRYY/RCMQUOTOT))) ;
2RCMFRTS = positif(COD2OP) * (positif(RCMQUOTOTNET-RCMFRQUOT)) * (arr(RCMFRQUOT*(DISQUO/RCMQUOTOT))) ;
2RCMFRGO = positif(COD2OP) * (positif(RCMQUOTOTNET-RCMFRQUOT)) * (arr(RCMFRQUOT-2RCMFRDC-2RCMFRFU-2RCMFRCH-2RCMFRTR-2RCMFRYY-2RCMFRTS)) ;
regle 821040:
application : iliad ;

2RCMDCNET = positif(COD2OP) * (max(0,2RCMRNABD - 2RCMFRDC)) ;
2RCMFUNET = positif(COD2OP) * (max(0,2RCMRNTNC - 2RCMFRFU));
2RCMCHNET = positif(COD2OP) * (max(0,RNETRCH - 2RCMFRCH)) ;
2RCMTRNET = positif(COD2OP) * (max(0,INTERE - 2RCMFRTR));
2RCMYYNET = positif(COD2OP) * (max(0,CODRYY - 2RCMFRYY));
2RCMTSNET = positif(COD2OP) * (max(0,DISQUO - 2RCMFRTS));
2RCMGONET = positif(COD2OP) * (max(0,2REGPRIVM- 2RCMFRGO));
RCMQNET = positif(COD2OP) * (2RCMDCNET + 2RCMFUNET + 2RCMCHNET + 2RCMGONET + 2RCMTRNET + 2RCMTSNET + 2RCMYYNET) ;
RCMTOTNET = positif(COD2OP) * ( RCMQNET + RCMORDNET) ;

regle 821050:
application : iliad ;

RCMFRTEMP = min(RCMAB + RTNC + TRCMRNCH + RCMNAB + RTCAR + RCMPRIVM+COD2TT,RCMFR) ;

regle 821060:
application : iliad ;

BRCMBIS = RCMAB + RTNC + TRCMRNCH + RCMNAB + RTCAR + RCMPRIVM ;

DEFRCMI = BRCMBISB1 + BRCMBISQ1 ;

BRCMBISB = RCMRNABD + RCMRNTNC + RNET2CH + REGPRIVM + RCMHAB + RCMHAD + COD2TT + RNET2RB + RNET2VV2WW + COD2YY + COD2ZZ + COD2VN + COD2VO + COD2VP + COD2TQ + COD2TZ ;

BRCMBISQ = 2RCMRNABD + 2RCMRNTNC + RNETRCH + 2REGPRIVM + INTERE + DISQUO + CODRYY ;
regle 821070:
application : iliad ;


DEFRCMIMPU = positif(null(PREM8_11)*positif(SOMMERCM_2)* positif(BRCMBISB + BRCMBISQ-RCMFR))
           * (max(0,REPRCM - max(REPRCMB1731,max(REPRCMB_P,REPRCMBP2)) - max(0,REPRCMB-REPRCMBP3)))
           + PREM8_11 * positif(BRCMBISB + BRCMBISQ-RCMFR) * REPRCM 
           + 0 ;

regle 821080:
application : iliad ;

RCMFRART1731 = RCMFRTEMP ;

regle 821100:
application : iliad ;


DFRCMNBIS = positif(RCMTOT)*(min(0,RCMORDTOTNET - RCMFRORDI + RCMQUOTOTNET - 2RCMDCNET-2RCMFUNET-2RCMCHNET-2RCMTRNET-2RCMTSNET-2RCMGONET-2RCMYYNET) * (-1))
          + (1-positif(RCMTOT))*max(0,RCMFR) ;          

DFRCMN = positif(COD2OP)*(DFRCMNBIS * null(V_IND_TRAIT-4) + (RCMFR - RCMFRART1731) *  null(V_IND_TRAIT-5)) ;

regle 821110:
application : iliad ;


1RCM_I = RCMORDNET * (1 - positif(DFRCMNBIS)) * positif(COD2OP) * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

2RCM_I = 2RCMDCNET * (1 - positif(DFRCMNBIS)) * positif(COD2OP) * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

3RCM_I = 2RCMFUNET * (1 - positif(DFRCMNBIS)) * positif(COD2OP) * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

4RCM_I = 2RCMCHNET * (1 - positif(DFRCMNBIS)) * positif(COD2OP) * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

5RCM_I = 2RCMTRNET * (1 - positif(DFRCMNBIS)) * positif(COD2OP) * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

6RCM_I = 2RCMYYNET * (1 - positif(DFRCMNBIS)) * positif(COD2OP) * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

7RCM_I = 2RCMTSNET * (1 - positif(DFRCMNBIS)) * positif(COD2OP) * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

8RCM_I = 2RCMGONET * (1 - positif(DFRCMNBIS)) * positif(COD2OP) * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

RCM_I = positif(COD2OP) * (1RCM_I + 2RCM_I + 3RCM_I + 4RCM_I + 5RCM_I + 6RCM_I + 7RCM_I + 8RCM_I) ;

regle 82014:
application : iliad ;


DEFFRCM = min (DEFRCM,RCMTOTNET) ;


REPRCM = (DEFRCM + DEFRCM2 + DEFRCM3 + DEFRCM4 + DEFRCM5 + DEFRCM6) ;


REPRCMAL = null(DEFFRCM-RCMTOTNET)*DEFRCM2
         + positif(RCMTOTNET-DEFFRCM)*(max(0,DEFRCM2-(RCMTOTNET-DEFFRCM))) ;

REPRCMAM = positif((DEFFRCM + DEFRCM2)-RCMTOTNET) * DEFRCM3
         + (1-positif((DEFFRCM + DEFRCM2)-RCMTOTNET)) * (max(0,DEFRCM3-(RCMTOTNET-(DEFFRCM+DEFRCM2)))) ;

REPRCMAN = positif((DEFFRCM+DEFRCM2+DEFRCM3)-RCMTOTNET) * DEFRCM4
         + (1-positif((DEFFRCM + DEFRCM2+DEFRCM3)-RCMTOTNET)) * (max(0,DEFRCM4-(RCMTOTNET-(DEFFRCM+DEFRCM2+DEFRCM3)))) ;

REPRCMAQ = positif((DEFFRCM+DEFRCM2+DEFRCM3+DEFRCM4)-RCMTOTNET) * DEFRCM5
         + (1-positif((DEFFRCM + DEFRCM2+DEFRCM3+DEFRCM4)-RCMTOTNET)) * (max(0,DEFRCM5-(RCMTOTNET-(DEFFRCM+DEFRCM2+DEFRCM3+DEFRCM4)))) ;

REPRCMAR = positif((DEFFRCM+DEFRCM2+DEFRCM3+DEFRCM4+DEFRCM5)-RCMTOTNET) * DEFRCM6
          +(1-positif((DEFFRCM + DEFRCM2+DEFRCM3+DEFRCM4 + DEFRCM5)-RCMTOTNET)) * (max(0,DEFRCM6-(RCMTOTNET-(DEFFRCM+DEFRCM2+DEFRCM3+DEFRCM4+DEFRCM5)))) ;


REPRCMTO = REPRCMAL + REPRCMAM + REPRCMAN + REPRCMAQ + REPRCMAR ; 
regle 8201402:
application : iliad ;
REPRCMB = max(0,BRCMBISB + BRCMBISQ - RCMFRTEMP) ;
regle 8201404:
application : iliad ;
REPRCMBIS = positif(COD2OP)*( positif(positif(SOMMERCM_2)*null(PREM8_11) * positif(BRCMBISB + BRCMBISQ-RCMFR) + PREM8_11 * positif(BRCMBISB + BRCMBISQ-RCMFR))
          * max(0,REPRCM - DEFRCMIMPU)
          + (1-positif(positif(SOMMERCM_2)*null(PREM8_11) * positif(BRCMBISB + BRCMBISQ-RCMFR) + PREM8_11 * positif(BRCMBISB + BRCMBISQ-RCMFR)))
          * min(REPRCM,REPRCMB) + 0) ;


REPRCM1 = positif(COD2OP) * (positif(REPRCMBIS) * arr((REPRCMBIS * 1RCM_I)/ RCM_I)
        + (1 - positif(REPRCMBIS)) * (positif(RCMTOTNET-REPRCM) * arr(REPRCM*RCMORDNET/RCMTOTNET) + positif(REPRCM-RCMTOTNET) * max(0,REPRCM-REPRCMTO)) * (1-positif((SOMMERCM_2)*null(PREM8_11) + PREM8_11))
        + (positif(SOMMERCM_2)*null(PREM8_11) + PREM8_11) * 0) ;


REPRCM2 = positif(COD2OP) * (positif(REPRCMBIS) * min(arr((REPRCMBIS * 2RCM_I)/ RCM_I), REPRCMBIS - REPRCM1)
        + (1 - positif(REPRCMBIS)) * (positif(RCMTOTNET-REPRCM)) * (arr(REPRCM * 2RCMDCNET/RCMTOTNET)) * (1-positif((SOMMERCM_2) * null(PREM8_11) + PREM8_11)) + (positif(SOMMERCM_2) * null(PREM8_11) + PREM8_11) * 0) ;


REPRCM3 = positif(COD2OP) * (positif(REPRCMBIS) * min(arr((REPRCMBIS * 3RCM_I)/ RCM_I), REPRCMBIS - REPRCM1 - REPRCM2)
        + (1 - positif(REPRCMBIS)) * (positif(RCMTOTNET-REPRCM)) * (arr(REPRCM*2RCMFUNET/RCMTOTNET)) * (1-positif((SOMMERCM_2) * null(PREM8_11) + PREM8_11)) + (positif(SOMMERCM_2) * null(PREM8_11) + PREM8_11) * 0) ;


REPRCM4 = positif(COD2OP) * (positif(REPRCMBIS) * min(arr((REPRCMBIS * 4RCM_I)/ RCM_I), REPRCMBIS - REPRCM1 - REPRCM2 - REPRCM3)
        + (1 - positif(REPRCMBIS)) * (positif(RCMTOTNET-REPRCM)) * (arr(REPRCM*2RCMCHNET/RCMTOTNET)) * (1-positif((SOMMERCM_2) * null(PREM8_11) + PREM8_11)) + (positif(SOMMERCM_2) * null(PREM8_11) + PREM8_11) * 0) ;


REPRCM5 = positif(COD2OP) *(positif(REPRCMBIS) * min(arr((REPRCMBIS * 5RCM_I)/ RCM_I), REPRCMBIS - REPRCM1 - REPRCM2 - REPRCM3 - REPRCM4)
        + (1 - positif(REPRCMBIS)) * (positif(RCMTOTNET-REPRCM)) * (arr(REPRCM*2RCMTRNET/RCMTOTNET)) * (1-positif((SOMMERCM_2) * null(PREM8_11) + PREM8_11)) + (positif(SOMMERCM_2) * null(PREM8_11) + PREM8_11) * 0) ;


REPRCM6 = positif(COD2OP) * (positif(REPRCMBIS) * min(arr((REPRCMBIS * 6RCM_I)/ RCM_I), REPRCMBIS - REPRCM1 - REPRCM2 - REPRCM3 - REPRCM4-REPRCM5)
        + (1 - positif(REPRCMBIS)) * (positif(RCMTOTNET-REPRCM))*(arr(REPRCM*2RCMYYNET/RCMTOTNET)) * (1-positif((SOMMERCM_2) * null(PREM8_11) + PREM8_11)) + (positif(SOMMERCM_2) * null(PREM8_11) + PREM8_11) * 0) ;


REPRCM7 = positif(COD2OP) * (positif(REPRCMBIS) * min(arr((REPRCMBIS * 7RCM_I)/ RCM_I), REPRCMBIS - REPRCM1 - REPRCM2 - REPRCM3 - REPRCM4 - REPRCM5-REPRCM6)
        + (1 - positif(REPRCMBIS)) * (positif(RCMTOTNET-REPRCM)) * (arr(REPRCM*2RCMTSNET/RCMTOTNET)) * (1-positif((SOMMERCM_2) * null(PREM8_11) + PREM8_11)) + (positif(SOMMERCM_2) * null(PREM8_11) + PREM8_11) * 0) ;


REPRCM8 = positif(COD2OP) * (positif(REPRCMBIS) * max(0,REPRCMBIS - REPRCM1 -REPRCM2 - REPRCM3 - REPRCM4 - REPRCM5  - REPRCM6 - REPRCM7 )
        + (1 - positif(REPRCMBIS)) * (positif(RCMTOTNET-REPRCM)) * (arr(REPRCM*2RCMGONET/RCMTOTNET)) * (1-positif((SOMMERCM_2) * null(PREM8_11) + PREM8_11)) + (positif(SOMMERCM_2) * null(PREM8_11) + PREM8_11) * 0) ;
regle 82015:
application : iliad ;


DFRCMM5 =  null(4-V_IND_TRAIT) * min(DEFRCM6,REPRCM - REPRCMBIS)
        + null(5-V_IND_TRAIT) * min(DEFRCM6,REPRCM - REPRCMBIS) ;
regle 821140:
application : iliad ;


DFRCMM4 = null(4-V_IND_TRAIT) * min(DEFRCM5,REPRCM - REPRCMBIS - DFRCMM5 )
        + null(5-V_IND_TRAIT) * min(DEFRCM5,REPRCM - REPRCMBIS - DFRCMM5 ) ;
regle 821150:
application : iliad ;


DFRCMM3 = null(4-V_IND_TRAIT) * min(DEFRCM4,REPRCM - REPRCMBIS - DFRCMM5 - DFRCMM4 )
        + null(5-V_IND_TRAIT) * min(DEFRCM4,REPRCM - REPRCMBIS - DFRCMM5 - DFRCMM4 ) ;
regle 821160:
application : iliad ;

DFRCMM2 = null(4-V_IND_TRAIT) * min(DEFRCM3,REPRCM - REPRCMBIS - DFRCMM5 - DFRCMM4-DFRCMM3)
        + null(5-V_IND_TRAIT) * min(DEFRCM3,REPRCM - REPRCMBIS - DFRCMM5 - DFRCMM4-DFRCMM3) ;

regle 821170:
application : iliad ;


DFRCMM1 = null(4-V_IND_TRAIT) * min(DEFRCM2,REPRCM-REPRCMBIS-DFRCMM5-DFRCMM4-DFRCMM3-DFRCMM2)
        + null(5-V_IND_TRAIT) * min(DEFRCM2,REPRCM-REPRCMBIS-DFRCMM5-DFRCMM4-DFRCMM3-DFRCMM2) ;

regle 821175:
application : iliad ;


DFRCM1 = null(4-V_IND_TRAIT) * (positif(COD2OP) * (positif(positif_ou_nul(RCMORDNET-REPRCM1) * positif(RCMTOT) + (COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ)) * REPRCMAL
       + positif(positif(positif(REPRCM1-RCMORDNET) + (1-positif(RCMTOT))) * (1-positif(COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ))) * DEFRCM2)
       + (1-positif(COD2OP)) * positif(DEFRCM2) * DEFRCM2)
       + null(5-V_IND_TRAIT) * min(DEFRCM2 , REPRCM-REPRCMBIS-DFRCM5-DFRCM4-DFRCM3-DFRCM2) ;

DFRCM2 = null(4-V_IND_TRAIT) *  (positif(COD2OP) * (positif(positif_ou_nul(RCMORDNET-REPRCM1)* positif(RCMTOT) + (COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ)) * REPRCMAM
       + positif(positif(positif(REPRCM1-RCMORDNET) + (1-positif(RCMTOT))) * (1-positif(COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ))) * DEFRCM3)
       + (1-positif(COD2OP)) * positif(DEFRCM3) * DEFRCM3)
       + null(5-V_IND_TRAIT) * min(DEFRCM3 , REPRCM - REPRCMBIS - DFRCM5 - DFRCM4-DFRCM3) ;

DFRCM3 = null(4-V_IND_TRAIT) *  (positif(COD2OP) * (positif(positif_ou_nul(RCMORDNET-REPRCM1) * positif(RCMTOT) + (COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ)) * REPRCMAN
       + positif(positif(positif(REPRCM1-RCMORDNET) + (1-positif(RCMTOT))) * (1-positif(COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ))) * DEFRCM4)
       + (1-positif(COD2OP))*positif(DEFRCM4) * DEFRCM4)
       + null(5-V_IND_TRAIT) * min(DEFRCM4 , REPRCM - REPRCMBIS - DFRCM5 - DFRCM4 ) ;

DFRCM4 = null(4-V_IND_TRAIT) *  (positif(COD2OP) * (positif(positif_ou_nul(RCMORDNET-REPRCM1) * positif(RCMTOT) + (COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ)) * REPRCMAQ
       + positif(positif(positif(REPRCM1-RCMORDNET) + (1-positif(RCMTOT))) * (1-positif(COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ))) * DEFRCM5)
       + (1-positif(COD2OP)) * positif(DEFRCM5) * DEFRCM5)
       + null(5-V_IND_TRAIT) * min(DEFRCM5,REPRCM - REPRCMBIS - DFRCM5 ) ;

DFRCM5 = null(4-V_IND_TRAIT) * (positif(COD2OP) * (positif(positif_ou_nul(RCMORDNET-REPRCM1) * positif(RCMTOT) +(COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ)) * REPRCMAR
       + positif(positif(positif(REPRCM1-RCMORDNET) + (1-positif(RCMTOT))) * (1-positif(COD2YY + COD2ZZ + COD2VN + COD2VO + COD2TQ + COD2TZ))) * DEFRCM6)
       + (1-positif(COD2OP)) * positif(DEFRCM6) * DEFRCM6)
       + null(5-V_IND_TRAIT) * min(DEFRCM6,REPRCM - REPRCMBIS) ;

regle 821180:
application : iliad ;


RCM11 = positif(COD2OP) * (1-V_CNR) * (max(0,(1RCM_I-REPRCM1)) +0) ;

2RCM = positif(COD2OP) * (1-V_CNR) * (
       (1-positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET)) *( max(0,(2RCM_I-REPRCM2)) +0)
     + positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET)*0
                                    ) ;
3RCM = positif(COD2OP) * (1-V_CNR) * (
       (1-positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET)) * ( max(0,(3RCM_I-REPRCM3)) +0)
     + positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET)*0
                                    ) ; 
4RCM = positif(COD2OP)* (1-V_CNR) * (
       (1-positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET)) * ( max(0,(4RCM_I-REPRCM4)) +0)
     + positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET) * 0
                                    ) ;
5RCM = positif(COD2OP)* (1-V_CNR) * (
       (1-positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET)) * ( max(0,(5RCM_I-REPRCM5)) +0)
     + positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET) * 0
                                    ) ;
6RCM = positif(COD2OP)* (1-V_CNR) * (
       (1-positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET)) * ( max(0,(6RCM_I-REPRCM6)) +0)
     + positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET) * 0
                                    ) ;
7RCM = positif(COD2OP)* (1-V_CNR) * (
       (1-positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET)) * ( max(0,(7RCM_I-REPRCM7)) +0)
     + positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET) * 0
                                    ) ;
8RCM = positif(COD2OP)* (1-V_CNR) * (
       (1-positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET)) * ( max(0,(8RCM_I-REPRCM8)) +0)
     + positif_ou_nul(RCMFRQUOT-RCMQUOTOTNET) * 0
                                    ) ;

DFRCM = (DFRCMN + DFRCM1 + DFRCM2 + DFRCM3 + DFRCM4 + DFRCM5) * (1-V_CNR) ;
RCMEXCREF = max(0,TRCMRABD + TRCMRTNC) * (1 - V_CNR) ;

regle 8211812:
application : iliad ;

RCM1 = arr(RCM11 + RCM11B) ;

regle 821190:
application : iliad ;


ABTAV = PLAF_RCMAV1 * (1 + BOOL_0AM) ;

regle 821200:
application : iliad ;


BPLIB = positif(COD2OP)*(ABT2DH + ABT2RA + EXO2RA) * (1 - V_CNR)
      + (1-positif(COD2OP))*(ABT2DH2 + ABT2RA2 + EXO2RA) * (1-V_CNR) ;


PLIBE = RCMLIB + COD2RA ;
regle 821210:
application : iliad ;


EPAV = arr(BPLIB * TX_PREVLIB/100) ;

regle 821212:
application : iliad ;


REVBRUTASSU = (1-positif(COD2OP)) * (RCMAV + PROVIE + RCMLIB + COD2VV + COD2WW + NETEXO2RA + NETEXO2RB + NETEXO2RC + NETEXO2RD ) ;

REVNET2CH = (1-positif(COD2OP)) * positif(ABTAV-REVBRUTASSU)*(0) ;

REVNET2VV = (1-positif(COD2OP)) * positif(ABTAV-REVBRUTASSU)*(0) ;

REVNET2WW = (1-positif(COD2OP)) * positif(ABTAV-REVBRUTASSU)*(0) ;


RND2CHRCH2 = (1-positif(COD2OP)) * (RCMAV + PROVIE) ;

ABT2CHRCH2 = (1-positif(COD2OP)) * min(ABTAV , RND2CHRCH2);

ABT2CH2  = (1-positif(COD2OP)) * arr(ABT2CHRCH2 * (RCMAV/RND2CHRCH2)) ;

ABTRCH2 = (1-positif(COD2OP)) * (ABT2CHRCH2 - ABT2CH2);

RNABT2CH = (1-positif(COD2OP)) * (RCMAV - ABT2CH2) ;

RNABTRCH = (1-positif(COD2OP)) * (PROVIE - ABTRCH2) ; 

ABT2RB2 = (1-positif(COD2OP)) * min( ABTAV - ABT2CHRCH2 , NETEXO2RB ) ;

RNET2RB2 = (1-positif(COD2OP)) * max (0, NETEXO2RB - ABT2RB2 ) ;

RNABT2DH = (1-positif(COD2OP)) * max(0,RCMLIB-(ABTAV-ABT2CHRCH2 - ABT2RB2)) ;

ABT2DH2 = (1-positif(COD2OP)) * (RCMLIB-RNABT2DH);

ABT2RA2 = (1-positif(COD2OP)) * min( ABTAV - ABT2CHRCH2 - ABT2RB2 - ABT2DH2 , NETEXO2RA ) ;

RNET2RA2 = (1-positif(COD2OP)) * max (0, NETEXO2RA - ABT2RA2 ) ;

RNABT2VV = (1-positif(COD2OP)) * max(0, COD2VV-(ABTAV-(ABT2CHRCH2+ ABT2RB2 + ABT2DH2 + ABT2RA2))) ;

ABT2VV = (1-positif(COD2OP)) * (COD2VV-RNABT2VV);

ABT2RC = (1-positif(COD2OP)) * min( ABTAV - (ABT2CHRCH2 + ABT2RB2 + ABT2DH2 + ABT2RA2 + ABT2VV) , NETEXO2RC ) ;

RNET2RC = (1-positif(COD2OP)) * max (0, NETEXO2RC - ABT2RC ) ;

RNABT2WW = (1-positif(COD2OP)) * max(0,COD2WW-(ABTAV-(ABT2CHRCH2 + ABT2RB2 + ABT2DH2 + ABT2RA2 + ABT2VV + ABT2RC))) ;

ABT2WW = (1-positif(COD2OP)) * (COD2WW-RNABT2WW) ;

ABT2RD = (1 - positif(COD2OP)) * min(ABTAV - (ABT2CHRCH2 + ABT2RB2 + ABT2DH2 + ABT2RA2 + ABT2RC + ABT2VV + ABT2WW) , NETEXO2RD) ; 

RNET2RD = (1-positif(COD2OP)) * max (0, NETEXO2RD - ABT2RD ) ;
regle 821214:
application : iliad ;

REGPRIVMB = (1-positif(COD2OP)) * arr(REGPRIV * MAJREV) ;


RCMIMPTN = (1-positif(COD2OP)) * (1-positif( null(V_REGCO-2))) * arr(RCMABD + RCMTNC + RCMHAD + RCMHAB + COD2TT + REGPRIVMB + RNABT2WW + COD2ZZ + COD2VP + COD2TQ +  RNET2RD + COD2TZ) ;


RCMIMPTR = (1-positif(COD2OP)) * (1-positif( null(V_REGCO-2) ))*(RNABT2VV + COD2VO + RNET2RC) ;


RCMBAR = (1-positif(COD2OP)) * (RNABT2CH + COD2YY + COD2VN + RNET2RB2) ;

RCMBARQUOT = (1-positif(COD2OP)) * (RNABTRCH + CODRYY) ;

RCM11B = (1-positif(COD2OP)) * RCMBAR * (1-positif( null(V_REGCO-2) )) ;


IMPOT128 = (1-positif(COD2OP)) * (RCMIMPTN*TX128/100) ;

IMPOT75 = (1-positif(COD2OP)) * (RCMIMPTR*TX075/100) ;


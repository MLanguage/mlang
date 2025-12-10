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
                                                                         #####
  ####   #    #    ##    #####      #     #####  #####   ######         #     #
 #    #  #    #   #  #   #    #     #       #    #    #  #                    #
 #       ######  #    #  #    #     #       #    #    #  #####           #####
 #       #    #  ######  #####      #       #    #####   #                    #
 #    #  #    #  #    #  #          #       #    #   #   #              #     #
  ####   #    #  #    #  #          #       #    #    #  ###### #######  #####
 #
 #
 #
 #
 #
 #
 #
 #                       CALCUL DE L'IMPOT NET
 #
 #
 #
 #
 #
 #
regle 301000:
application : bareme , iliad ;

IRN = min(0 , IAN + AVFISCOPTER - IRE) + max(0 , IAN + AVFISCOPTER - IRE) * positif( IAMD1 + 1 - SEUIL_61) ;


regle 301005:
application : bareme , iliad ;
IRNAF = min( 0, IAN - IRE) + max( 0, IAN - IRE) * positif( IAMD1AF + 1 - SEUIL_61) ;

regle 301010:
application : bareme , iliad ;


IAR = min( 0, IAN + AVFISCOPTER - IRE) + max( 0, IAN + AVFISCOPTER - IRE) ;

regle 301015:
application : bareme , iliad ;

IARAF = min(0 , IANAF - IREAF) + max(0 , IANAF - IREAF) + NEGIANAF ;

regle 301020:
application : iliad ;

CREREVET = min(arr((BPTP3 + BPTPD + BPTPG) * TX128/100),arr(CIIMPPRO * TX128/100 ))
	   + min(arr(BPTP19 * TX19/100),arr(CIIMPPRO2 * TX19/100 ))
	   + min (arr(RCMIMPTR * TX075/100),arr(COD8XX * TX075/100)) 
	   + min (arr(BPTP10 * TX10/100),arr(COD8XV * TX10/100)) 
	   ;

CIIMPPROTOT = CIIMPPRO + CIIMPPRO2 + COD8XX + COD8XX + COD8XV;

regle 301030:
application : iliad ;

ICI8XFH = min(arr(BPTP18 * TX18/100),arr(COD8XF * TX18/100 ))
      + min(arr(BPTP4I * TX30/100),arr(COD8XG * TX30/100 ))
      + min(arr(BPTP40 * TX41/100),arr(COD8XH * TX41/100 ));
ICIGLO = min(arr(BPTP18 * TX18/100),arr(COD8XF * TX18/100 ))
      + min(arr(BPTP4I * TX30/100),arr(COD8XG * TX30/100 ))
      + min(arr(BPTP40 * TX41/100),arr(COD8XH * TX41/100 ));

CIGLOTOT = COD8XF + COD8XG + COD8XH; 
regle 301032:
application : iliad ;

CI8XFH = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER - CIRCMAVFT - IRETS - ICREREVET , ICI8XFH)) ;

CIGLO = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER - CIRCMAVFT - IRETS - ICREREVET , ICIGLO)) ;

regle 301035:
application : iliad ;

CI8XFHAF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT - CIRCMAVFTAF - IRETSAF - ICREREVETAF , ICI8XFH)) ;

CIGLOAF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT - CIRCMAVFTAF - IRETSAF - ICREREVETAF , ICIGLO)) ;

regle 301040:
application : iliad ;


ICREREVET = max(0 , min(IAD11 + ITP - CIRCMAVFT - IRETS , min(ITP , CREREVET))) ;

regle 301045:
application : iliad ;

ICREREVETAF = max(0 , min(IAD11 + ITP - CIRCMAVFTAF - IRETSAF , min(ITP , CREREVET))) ;

regle 301050:
application : iliad , bareme ;

INE = (CIRCMAVFT + IRETS + ICREREVET + CIGLO + CIDONENTR + CICORSE + CIRECH + CICOMPEMPL) * (1 - positif(RE168 + TAX1649)) ;

IAN = max(0 , (IRB - AVFISCOPTER - INE
               + min(TAXASSUR + 0 , max(0 , INE - IRB + AVFISCOPTER)) 
               + min(IPCAPTAXTOT + 0 , max(0 , INE - IRB + AVFISCOPTER - min(TAXASSUR + 0 , max(0 , INE - IRB + AVFISCOPTER))))
	      )
         ) ;
IANINR = max(0 , (IRBINR - AVFISCOPTER - INE
               + min(TAXASSUR + 0 , max(0 , INE - IRBINR + AVFISCOPTER)) 
               + min(IPCAPTAXTOT + 0 , max(0 , INE - IRBINR + AVFISCOPTER - min(TAXASSUR + 0 , max(0 , INE - IRBINR + AVFISCOPTER))))
	      )
         ) ;

regle 301055:
application : iliad , bareme ;

INEAF = (CIRCMAVFTAF + IRETSAF + ICREREVETAF + CIGLOAF + CIDONENTRAF + CICORSEAF + CIRECHAF + CICOMPEMPLAF)
            * (1-positif(RE168+TAX1649));
regle 301057:
application : iliad , bareme ;

IANAF = max( 0, (IRBAF  + ((- CIRCMAVFTAF
				     - IRETSAF
                                     - ICREREVETAF
                                     - CIGLOAF
                                     - CIDONENTRAF
                                     - CICORSEAF
				     - CIRECHAF
                                     - CICOMPEMPLAF)
                                   * (1 - positif(RE168 + TAX1649)))
                  + min(TAXASSUR+0 , max(0,INEAF-IRBAF)) 
                  + min(IPCAPTAXTOT+0 , max(0,INEAF-IRBAF - min(TAXASSUR+0,max(0,INEAF-IRBAF))))
	      )
         ) ;

NEGIANAF = -1 * (min(TAXASSUR+0 , max(0,INEAF-IRBAF))
                 + min(IPCAPTAXTOT+0 , max(0,INEAF-IRBAF - min(TAXASSUR+0,max(0,INEAF-IRBAF))))) ;

regle 301060:
application : iliad ;


IRE = (EPAV + CRICH + CICORSENOW + CIGE + CITEC + CICA + CIGARD + CISYND 
       + CIPRETUD + CIADCRE + CIHABPRIN + CREFAM + COD8WK + CREAGRIBIO + CRESINTER 
       + CREFORMCHENT + CREARTS + CICONGAGRI + AUTOVERSLIB + CIPAP
       + CI2CK + CIFORET + CIHJA + COD8TE + CIVHELEC+ CREAGRIHVE+CREAGRIGLY
       + COD8TL) * (1 - positif(RE168 + TAX1649 + 0)) ;

IREAF = (EPAV + CRICH + CICORSENOW + CIGE + CITEC + CICA + CIGARD + CISYND 
       + CIPRETUD + CIADCRE + CIHABPRIN + CREFAM + COD8WK + CREAGRIBIO + CRESINTER 
       + CREFORMCHENT + CREARTS + CICONGAGRI + AUTOVERSLIB + CIPAP
       + CI2CK + CIFORET + CIHJA + COD8TE + CIVHELEC+ CREAGRIHVE+CREAGRIGLY
       + COD8TL) * (1 - positif(RE168 + TAX1649 + 0)) ;

IRE2 = IRE ; 

regle 301065:
application : iliad ;

CIHJA = CODHJA * (1 - positif(RE168 + TAX1649)) * (1 - V_CNR) ;

regle 301070:
application : iliad ;

CRICH =  IPRECH * (1 - positif(RE168+TAX1649));

regle 301080:
application : iliad ;


CIRCMAVFT = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER , RCMAVFT * (1 - V_CNR)));

regle 301085:
application : iliad ;

CIRCMAVFTAF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT , RCMAVFT * (1 - V_CNR)));

regle 301100:
application : iliad;
CI2CK = COD2CK * (1 - positif(RE168 + TAX1649)) * (1 - V_CNR);

regle 301110:
application : iliad;


CICA =  arr(BAILOC98 * TX_BAIL / 100) * (1 - positif(RE168 + TAX1649)) ;

regle 301130:
application : iliad ;


IPAE = COD8VL + COD8VM + COD8WM + COD8UM ;

RASIPSOUR =  IPSOUR * positif( null(V_REGCO-2) + null(V_REGCO-3) ) * ( 1 - positif(RE168+TAX1649) );

RASIPAE = COD8VM + COD8WM + COD8UM ;

regle 301133:
application : iliad ;

IRETS1 = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER - CIRCMAVFT , RASIPSOUR)) ;

IRETS21 = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER - CIRCMAVFT - IRETS1 , min(COD8PB , COD8VL) * present(COD8PB) + COD8VL * (1 - present(COD8PB)))) 
          * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

IRETS2 = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER - CIRCMAVFT - IRETS1 - IRETS21 , min(COD8PA , RASIPAE) * present(COD8PA) + RASIPAE * (1 - present(COD8PA)))) 
         * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) + IRETS21 ;
	
IRETS = IRETS1 + IRETS2 ;

regle 301135:
application : iliad ;

IRETS1AF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT - CIRCMAVFTAF , RASIPSOUR)) ;

IRETS21AF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT + - CIRCMAVFTAF - IRETS1AF , min(COD8PB , COD8VL) * present(COD8PB) + COD8VL * (1 - present(COD8PB))))
            * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;

IRETS2AF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT - CIRCMAVFTAF - IRETS1AF - IRETS21AF , min(COD8PA , RASIPAE) * present(COD8PA) + RASIPAE * (1 - present(COD8PA))))
           * positif(null(V_REGCO - 1) + null(V_REGCO - 3) + null(V_REGCO - 5) + null(V_REGCO - 6)) + IRETS21AF ;
	
IRETSAF = IRETS1AF + IRETS2AF ;

regle 301170:
application : iliad ;

BCIDONENTR = RDMECENAT * (1-V_CNR) ;

regle 301172:
application : iliad ;

CIDONENTR = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER - CIRCMAVFT - REI - IRETS - ICREREVET - CIGLO , BCIDONENTR)) ;

regle 301175:
application : iliad ;

CIDONENTRAF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT - CIRCMAVFTAF - REI - IRETSAF - ICREREVETAF - CIGLOAF , BCIDONENTR)) ;

regle 301180:
application : iliad ;

CICORSE = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER - CIRCMAVFT - IPPRICORSE - IRETS - ICREREVET - CIGLO - CIDONENTR , CIINVCORSE + IPREPCORSE)) ;

CICORSEAVIS = CICORSE + CICORSENOW ;

TOTCORSE = CIINVCORSE + IPREPCORSE + CICORSENOW ;

regle 301185:
application : iliad ;

CICORSEAF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT - CIRCMAVFTAF - IPPRICORSE - IRETSAF - ICREREVETAF - CIGLOAF - CIDONENTRAF , CIINVCORSE + IPREPCORSE)) ;

CICORSEAVISAF = CICORSEAF + CICORSENOW ;

regle 301190:
application : iliad ;

CIRECH = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER - CIRCMAVFT - IRETS - ICREREVET - CIGLO - CIDONENTR - CICORSE , IPCHER)) ;

regle 301195:
application : iliad ;

CIRECHAF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT - CIRCMAVFTAF - IRETSAF - ICREREVETAF - CIGLOAF - CIDONENTRAF - CICORSEAF , IPCHER)) ;
regle 301200:
application : iliad ;

CICOMPEMPL = max(0 , min(IRB + TAXASSUR + IPCAPTAXT - AVFISCOPTER - CIRCMAVFT - IRETS - ICREREVET - CIGLO - CIDONENTR - CICORSE - CIRECH , COD8UW)) ;

DIEMPLOI = (COD8UW + COD8TL) * (1 - positif(RE168+TAX1649)) ;

CIEMPLOI = (CICOMPEMPL + COD8TL) * (1 - positif(RE168+TAX1649)) ;

IRECR = abs(min(0 ,IRB+TAXASSUR+IPCAPTAXT -AVFISCOPTER-CIRCMAVFT-IRETS-ICREREVET-CIGLO-CIDONENTR-CICORSE-CIRECH-CICOMPEMPL));

regle 301205:
application : iliad ;

CICOMPEMPLAF = max(0 , min(IRBAF + TAXASSUR + IPCAPTAXT - CIRCMAVFTAF - IRETSAF - ICREREVETAF - CIGLOAF - CIDONENTRAF - CICORSEAF - CIRECHAF , COD8UW)) ;

CIEMPLOIAF = (CICOMPEMPLAF + COD8TL) * (1 - positif(RE168+TAX1649)) ;

IRECRAF = abs(min(0 ,IRBAF+TAXASSUR+IPCAPTAXT -CIRCMAVFTAF-IRETSAF-ICREREVETAF-CIGLOAF-CIDONENTRAF-CICORSEAF-CIRECHAF-CICOMPEMPLAF));

regle 301210:
application : iliad ;
  
REPCORSE = abs(CIINVCORSE+IPREPCORSE-CICORSE) ;
REPRECH = abs(IPCHER - CIRECH) ;
REPCICE = abs(COD8UW - CICOMPEMPL) ;

regle 301220:
application : iliad ;

CICONGAGRI = CRECONGAGRI * (1-V_CNR) ;

regle 301230:
application : iliad ;

BCICAP = arr(PRELIBXT * TX90/100 * T_PCAPTAX/100) ;

regle 301233:
application : iliad ;

BCICAPAVIS = arr(PRELIBXT * TX90/100) ;

CICAP = max(0 , min(IPCAPTAXTOT , BCICAP)) ;

regle 301235:
application : iliad ;

CICAPAF = CICAP ;

regle 301240:
application : iliad ;

BCICHR = arr(CHRAPRES * ((REGCIAUTO+CIIMPPRO)*(1-present(COD8XY))+COD8XY+0) / (REVKIREHR - TEFFHRC+COD8YJ));
regle 301242:
application : iliad ;

CICHR = max(0,min(IRB + TAXASSUR + IPCAPTAXT +CHRAPRES - AVFISCOPTER ,min(CHRAPRES,BCICHR)));
regle 301245:
application : iliad ;

CICHRAF = max(0,min(IRBAF + TAXASSUR + IPCAPTAXT +CHRAPRES ,min(CHRAPRES,BCICHR)));
regle 301247:
application : iliad ;

BCICHR3WH = arr(CHRAPRES3WH * (REGCIAUTO*(1-present(COD8XY))+COD8XY+0) / (REVKIREHR+PVREPORT - TEFFHRC+COD8YJ));

regle 301249:
application : iliad ;

CICHR3WH = max(0,min(IRB + TAXASSUR + IPCAPTAXT +CHRAPRES3WH - AVFISCOPTER ,min(CHRAPRES3WH,BCICHR3WH)));

regle 301252:
application : iliad ;

CICHR3WHAF = max(0,min(IRBAF + TAXASSUR + IPCAPTAXTOT +CHRAPRES3WH -CICAPAF ,min(CHRAPRES3WH,BCICHR3WH)));

regle 301257:
application : iliad ;



DSYND = RDSYVO + RDSYCJ + RDSYPP ;


SOMBCOSV = TSHALLOV + COD1AA + CARTSV + ALLOV + REMPLAV + COD1GB + COD1GF + COD1GG + COD1AF  
           + CODRAF + COD1AG + CODRAG + PRBRV + CARPEV + PALIV + PENSALV + CODDAJ + CODEAJ 
	   + PENINV + CODRAZ + COD1AL + CODRAL + COD1AM + CODRAM + COD1TP + GLDGRATV 
	   + COD1TZ + COD1NX + max(0,COD1GH - LIM5000);

SOMBCOSC = TSHALLOC + COD1BA + CARTSC + ALLOC + REMPLAC + COD1HB + COD1HF + COD1HG + COD1BF 
           + CODRBF + COD1BG + CODRBG + PRBRC + CARPEC + PALIC + PENSALC + CODDBJ + CODEBJ 
	   + PENINC + CODRBZ + COD1BL + CODRBL + COD1BM + CODRBM + COD1UP + GLDGRATC + COD1OX + max(0,COD1HH - LIM5000);

SOMBCOSP = TSHALLO1 + TSHALLO2 + TSHALLO3 + TSHALLO4 + COD1CA + COD1DA + COD1EA + COD1FA   
           + CARTSP1 + CARTSP2 + CARTSP3 + CARTSP4 + ALLO1 + ALLO2 + ALLO3 + ALLO4    
           + REMPLAP1 + REMPLAP2 + REMPLAP3 + REMPLAP4 + COD1IB + COD1IF + COD1JB   
           + COD1JF + COD1KF + COD1LF + COD1CF + COD1DF + COD1EF + COD1FF   
           + CODRCF + CODRDF + CODREF + CODRFF + COD1CG + COD1DG + COD1EG + COD1FG    
           + CODRCG + CODRDG + CODRGG + CODRFG + PRBR1 + PRBR2 + PRBR3 + PRBR4     
           + CARPEP1 + CARPEP2 + CARPEP3 + CARPEP4 + PALI1 + PALI2 + PALI3 + PALI4     
           + PENSALP1 + PENSALP2 + PENSALP3 + PENSALP4 + PENIN1 + PENIN2 + PENIN3 + PENIN4    
           + CODRCZ + CODRDZ + CODREZ + CODRFZ + COD1CL + COD1DL + COD1EL + COD1FL    
           + CODRCL + CODRDL + CODREL + CODRFL + COD1CM + COD1DM + COD1EM + COD1FM    
           + CODRCM + CODRDM + CODREM + CODRFM + COD1IG + COD1JG + COD1KG + COD1LG 
	   + max(0,COD1IH - LIM5000)+ max(0,COD1JH - LIM5000)+ max(0,COD1KH - LIM5000)+ max(0,COD1LH - LIM5000);


BCOS = min(RDSYVO+0,arr(TX_BASECOTSYN/100*SOMBCOSV*IND_10V))
      +min(RDSYCJ+0,arr(TX_BASECOTSYN/100*SOMBCOSC*IND_10C))                             
      +min(RDSYPP+0,arr(TX_BASECOTSYN/100*SOMBCOSP*IND_10P));

ASYND = BCOS * (1-V_CNR) ;


CISYND = arr(TX_REDCOTSYN/100 * BCOS) * (1 - V_CNR) ;

regle 301260:
application : iliad ;


IAVF = IRE - EPAV + CICORSE + CIRCMAVFT ;


DIAVF2 = (IPRECH + IPCHER + RCMAVFT ) * (1 - positif(RE168+TAX1649)) + CIRCMAVFT * positif(RE168+TAX1649);


IAVF2 = (IPRECH + CIRECH + CIRCMAVFT + 0) * (1 - positif(RE168 + TAX1649))
        + CIRCMAVFT * positif(RE168 + TAX1649) ;

IAVFGP = IAVF2 + CREFAM ;

regle 301270:
application : iliad ;


I2DH = EPAV ;

regle 301280:
application : iliad ;


BTANTGECUM   = (V_BTGECUM * (1 - positif(present(COD7ZZ)+present(COD7ZY)+present(COD7ZX)+present(COD7ZW))) + COD7ZZ+COD7ZY+COD7ZX+COD7ZW);

BTANTGECUMWL =   V_BTPRT7 * (1 - present(COD7WK)) + COD7WK 
               + V_BTPRT6 * (1 - present(COD7WQ)) + COD7WQ 
	       + V_BTPRT5 * (1- present (COD7WH)) + COD7WH
	       + V_BTPRT4 * (1- present (COD7WS)) + COD7WS
	       + V_BTPRT3 * (1- present (COD7XZ)) + COD7XZ
	       + V_BTPRT2 * (1- present (COD7XR)) + COD7XR
	       + V_BTPRT1 * (1- present (COD7XV)) + COD7XV
	       ;

P2GE = max( (   PLAF_GE2 * (1 + BOOL_0AM)
             + PLAF_GE2_PACQAR * (V_0CH + V_0DP)
             + PLAF_GE2_PAC * (V_0CR + V_0CF + V_0DJ + V_0DN)  
              ) - BTANTGECUM
             , 0
             ) ;
BGEPAHA = min(RDEQPAHA +COD7WI, P2GE) * (1 - V_CNR);

P2GEWL = max(0,PLAF20000 - BTANTGECUMWL);
BGTECH = min(RDTECH , P2GEWL) * (1 - V_CNR) ;

BGEDECL = RDTECH + RDEQPAHA + COD7WI ;
TOTBGE = BGTECH + BGEPAHA ;

RGEPAHA =  (BGEPAHA * TX25 / 100 );
RGTECH = (BGTECH * TX40 / 100); 
CIGE = arr (RGTECH + RGEPAHA ) * (1 - positif(RE168 + TAX1649));


DEPENDPDC = BGEPAHA ;


DEPENPPRT = BGTECH ;

GECUM = min(P2GE,BGEPAHA)+(V_BTGECUM * (1 - positif(present(COD7ZY)+present(COD7ZX)+present(COD7ZW) + present(COD7ZZ))) + COD7ZZ + COD7ZW +COD7ZX + COD7ZY);
GECUMWL = max(0,BGTECH + BTANTGECUMWL) ;

BADCRE = min(max(0,CREAIDE-COD7DR) , min((LIM_AIDOMI * (1 - positif(PREMAIDE)) + LIM_PREMAIDE * positif(PREMAIDE)
                            + MAJSALDOM * (positif_ou_nul(ANNEEREV-V_0DA-65) + positif_ou_nul(ANNEEREV-V_0DB-65) * BOOL_0AM
                                           + V_0CF + V_0DJ + V_0DN + (V_0CH + V_0DP)/2+ASCAPA)
                           ) , LIM_AIDOMI3 * (1 - positif(PREMAIDE)) + LIM_PREMAIDE2 * positif(PREMAIDE) ) * (1-positif(INAIDE + 0))
                               +  LIM_AIDOMI2 * positif(INAIDE + 0)) ;

DAIDC = max(0 , CREAIDE - COD7DR) ;
AAIDC = BADCRE * (1 - V_CNR) ;
CIADCRE = max(0 , arr(BADCRE * TX_AIDOMI /100)) * (1 - positif(RE168 + TAX1649)) * (1 - V_CNR) ;
CIADCREB3 = COD7HB ;

regle 301310:
application : iliad ;

DTEC = RISKTEC;
ATEC = positif(DTEC) * DTEC;
CITEC = arr (ATEC * TX40/100);

regle 301320:
application : iliad ;

DPRETUD = PRETUD + PRETUDANT ;
APRETUD = max(min(PRETUD,LIM_PRETUD) + min(PRETUDANT,LIM_PRETUD*CASEPRETUD),0) * (1-V_CNR) ;

CIPRETUD = arr(APRETUD*TX_PRETUD/100) * (1 - positif(RE168 + TAX1649)) * (1-V_CNR) ;

regle 301325:
application : iliad ;


DPAP = COD7PA + COD7PB;
PLAFAPAP = positif_ou_nul((LIM24000 + (LIM6000 * (NSM + NPA + NIN + NSP-1) + (LIM6000 / 2) * NBQAR) * 2) - (V_BTRFRN2 * (1- present(RFRN2)) + RFRN2)) * positif(V_BTRFRN2 * (1- present(RFRN2)) + RFRN2);

APAP = (COD7PA + PLAFAPAP * COD7PB+0) * (1 - V_CNR) ;

CIPAP = arr(APAP * TX30/100) * (1 - V_CNR) ;

regle 301330:
application : iliad ;



EM7AVRICI = somme (i=0..7: min (1 , max(0 , V_0Fi + AG_LIMFG - ANNEEREV+1)))
         + somme (j=0..5: min (1 , max(0 , V_0Nj + AG_LIMFG - ANNEEREV+1)))
      + (1 - positif(somme(i=0..7:V_0Fi) + somme(i=0..5: V_0Ni) + 0)) * (V_0CF + V_0DN) ;

EM7QARAVRICI = somme (i=0..5: min (1 , max(0 , V_0Hi + AG_LIMFG - ANNEEREV+1)))
         + somme (j=0..3: min (1 , max(0 , V_0Pj + AG_LIMFG - ANNEEREV+1)))
         + (1 - positif(somme(i=0..5: V_0Hi) + somme(j=0..3: V_0Pj) + 0)) * (V_0CH + V_0DP) ;

EM7 = somme (i=0..7: min (1 , max(0 , V_0Fi + AG_LIMFG - ANNEEREV)))
         + somme (j=0..5: min (1 , max(0 , V_0Nj + AG_LIMFG - ANNEEREV)))
       + ((1 - positif(somme(i=0..7:V_0Fi) + 0)) * V_0CF + (1 - positif(somme(j=0..5: V_0Nj)+ 0)) * V_0DN); 

EM7QAR = somme (i=0..5: min (1 , max(0 , V_0Hi + AG_LIMFG - ANNEEREV)))
         + somme (j=0..5: min (1 , max(0 , V_0Pj + AG_LIMFG - ANNEEREV)))
         + ((1 - positif(somme(i=0..5:V_0Hi) + 0)) * V_0CH + (1 - positif(somme(j=0..5: V_0Pj)+ 0)) * V_0DP); 
	 
BRFG = min(RDGARD1,PLAF_REDGARD) + min(RDGARD2,PLAF_REDGARD)
       + min(RDGARD3,PLAF_REDGARD) + min(RDGARD4,PLAF_REDGARD)
       + min(RDGARD1QAR,PLAF_REDGARDQAR) + min(RDGARD2QAR,PLAF_REDGARDQAR)
       + min(RDGARD3QAR,PLAF_REDGARDQAR) + min(RDGARD4QAR,PLAF_REDGARDQAR)
       ;
RFG1 = arr( (BRFG + CODFGR + CODFHR) * TX_REDGARD /100 ) * (1 -V_CNR) ;
DGARD = somme(i=1..4:RDGARDi)+somme(i=1..4:RDGARDiQAR) + CODFGD + CODFHD ;
AGARD = (BRFG + CODFGR + CODFHR) * (1-V_CNR) ;
CIGARD = RFG1 * (1 - positif(RE168 + TAX1649)) ;

regle 301340:
application : iliad ;


PREHAB = PREHABT + PREHABT2 + PREHABTN2 + PREHABTVT ;

BCIHP = max(( PLAFHABPRIN * (1 + BOOL_0AM) * (1+positif(V_0AP+V_0AF+V_0CG+V_0CI+V_0CR))
                 + (PLAFHABPRINENF/2) * (V_0CH + V_0DP)
                 + PLAFHABPRINENF * (V_0CR + V_0CF + V_0DJ + V_0DN)
                  )
             ,0);

BCIHABPRIN1 = min(BCIHP , PREHABT) * (1 - V_CNR) ;
BCIHABPRIN5 = min(max(0,BCIHP-BCIHABPRIN1),PREHABT2) * (1 - V_CNR);
BCIHABPRIN6 = min(max(0,BCIHP-BCIHABPRIN1-BCIHABPRIN5),PREHABTN2) * (1 - V_CNR);
BCIHABPRIN7 = min(max(0,BCIHP-BCIHABPRIN1-BCIHABPRIN5-BCIHABPRIN6),PREHABTVT) * (1 - V_CNR);

BCIHABPRIN = BCIHABPRIN1 + BCIHABPRIN5 + BCIHABPRIN6 + BCIHABPRIN7 ;

CIHABPRIN = arr((BCIHABPRIN1 * TX40/100)
                + (BCIHABPRIN5 * TX20/100)
                + (BCIHABPRIN6 * TX15/100)
                + (BCIHABPRIN7 * TX10/100))
                * (1 - positif(RE168 + TAX1649)) * (1 - V_CNR);

regle 301350:
application : iliad ;


BDCIFORET = COD7VQ + COD7TE + COD7VR + COD7TF + COD7VM + COD7TA + COD7VN + COD7TB + COD7VH + COD7TV + COD7VI + COD7TW + COD7VJ + COD7TT + COD7VK + COD7TU + COD7TR + COD7TS + COD7TP + COD7TQ + COD7TM + COD7TO
            + REPSINFOR5 + COD7TK +RDFORESTRA + SINISFORET + COD7UA + COD7UB + RDFORESTGES + COD7UI ;

BCIFORETTK = max(0 , min(COD7TK , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)))) * (1-V_CNR) ;
VARTMP1 = BCIFORETTK ;

BCIFORETTJ = max(0 , min(REPSINFOR5 , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTJ ;

BCIFORETTO = max(0 , min(COD7TO , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTO ;

BCIFORETTM = max(0 , min(COD7TM , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTM ;

BCIFORETTQ = max(0 , min(COD7TQ , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTQ ;

BCIFORETTP = max(0 , min(COD7TP , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTP ;

BCIFORETTS = max(0 , min(COD7TS , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTS ;

BCIFORETTR = max(0 , min(COD7TR , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTR ;

BCIFORETVK = max(0 , min(COD7VK , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETVK ;

BCIFORETTU = max(0 , min(COD7TU , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTU ;

BCIFORETVJ = max(0 , min(COD7VJ , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETVJ ;

BCIFORETTT = max(0 , min(COD7TT , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTT ;

BCIFORETVI = max(0 , min(COD7VI , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETVI ;

BCIFORETTW = max(0 , min(COD7TW , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTW ;

BCIFORETVH = max(0 , min(COD7VH , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETVH ;

BCIFORETTV = max(0 , min(COD7TV , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTV ;

BCIFORETVN = max(0 , min(COD7VN , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETVN ;

BCIFORETTB = max(0 , min(COD7TB , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTB ;

BCIFORETVM = max(0 , min(COD7VM , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETVM ;

BCIFORETTA = max(0 , min(COD7TA , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTA ;

BCIFORETVR = max(0 , min(COD7VR , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETVR ;

BCIFORETTF = max(0 , min(COD7TF , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTF ;

BCIFORETVQ = max(0 , min(COD7VQ , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETVQ ;

BCIFORETTE = max(0 , min(COD7TE , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETTE ;

BCIFORETUA = max(0 , min(COD7UA , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETUA ;

BCIFORETUB = max(0 , min(COD7UB , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETUB ;

BCIFORETUP = max(0 , min(RDFORESTRA , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = VARTMP1 + BCIFORETUP ;

BCIFORETUT = max(0 , min(SINISFORET , max(0 , PLAF_FOREST1 * (1 + BOOL_0AM)-VARTMP1))) * (1-V_CNR) ;
VARTMP1 = 0 ;

BCIFORETUI = max(0 , min(COD7UI , max(0 , PLAF_FOREST2 * (1 + BOOL_0AM)))) * (1-V_CNR) ;
BCIFORETUQ = max(0 , min(RDFORESTGES , max(0 , PLAF_FOREST2 * (1 + BOOL_0AM)-BCIFORETUI))) * (1-V_CNR) ;

BCIFORET = BCIFORETTK + BCIFORETTJ + BCIFORETTO + BCIFORETTM + BCIFORETTQ + BCIFORETTP + BCIFORETVL + BCIFORETTS + BCIFORETVS + BCIFORETTR + BCIFORETVK + BCIFORETTU + BCIFORETVJ + BCIFORETTT + BCIFORETVI
         + BCIFORETTW + BCIFORETVH + BCIFORETTV + BCIFORETVN + BCIFORETTB + BCIFORETVM + BCIFORETTA + BCIFORETVR + BCIFORETTF + BCIFORETVQ + BCIFORETTE + BCIFORETUA + BCIFORETUB + BCIFORETUP + BCIFORETUT + BCIFORETUI + BCIFORETUQ ;

CIFORET = arr((BCIFORETTJ + BCIFORETTM + BCIFORETTP + BCIFORETVS + BCIFORETTR + BCIFORETVJ + BCIFORETTT + BCIFORETVH + BCIFORETTV + BCIFORETVM + BCIFORETTA + BCIFORETVQ + BCIFORETTE + BCIFORETUP + BCIFORETUT + BCIFORETUQ
                                                                  ) * TX18/100
        + (BCIFORETTK + BCIFORETTO + BCIFORETTQ + BCIFORETTS + BCIFORETVK + BCIFORETTU + BCIFORETVI + BCIFORETTW + BCIFORETVN + BCIFORETTB + BCIFORETVR +BCIFORETTF + BCIFORETUA + BCIFORETUB + BCIFORETUI
                                                                  ) * TX25/100) ;

regle 301360:
application : iliad ;



REPCIFADSSN7 = (positif_ou_nul(COD7TK + REPSINFOR5 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TO
                + (1 - positif_ou_nul(COD7TK + REPSINFOR5 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TO - (PLAF_FOREST1 * (1 + BOOL_0AM) - COD7TK - REPSINFOR5))) * (1 - V_CNR) ;

REPCIFSN7 = (positif_ou_nul(COD7TK + REPSINFOR5 + COD7TO - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TM
             + (1 - positif_ou_nul(COD7TK + REPSINFOR5 + COD7TO - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TM - (PLAF_FOREST1 * (1 + BOOL_0AM) - COD7TK - REPSINFOR5 + COD7TO))) * (1 - V_CNR) ;

VARTMP1 = COD7TK + REPSINFOR5 + COD7TO + COD7TM ;

REPCIFADSSN6 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TQ
                + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TQ - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TQ ;

REPCIFSN6 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TP
            + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TP - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TP ;
							   

REPCIFADSSN5 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TS
             + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TS - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TS ;

REPCIFSN5 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TR
          + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TR - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TR + COD7VK ;
			     

REPCIFADSSN4 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TU
                + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TU - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TU + COD7VJ ;

REPCIFSN4 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TT
             + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TT - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TT ;


REPCIFADHSN3 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7VI
            + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7VI - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7VI ;
							   
REPCIFADSSN3 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TW
               + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TW - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TW ;
							   
REPCIFHSN3 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7VH
          + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7VH - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7VH ;
							   
REPCIFSN3 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TV
             + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TV - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TV ;



REPCIFADHSN2 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7VN
            + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7VN - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7VN ;

REPCIFADSSN2 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TB
               + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TB - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TB ;

REPCIFHSN2 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7VM
             + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7VM - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7VM ;

REPCIFSN2 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TA
            + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TA - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TA ;


REPCIFADHSN1 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7VR
            + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7VR - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7VR ;

REPCIFADSSN1 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TF
               + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TF - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7TF ;

REPCIFHSN1 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7VQ
             + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7VQ - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7VQ ;

REPCIFSN1 = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7TE
            + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7TE - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
	    VARTMP1 = VARTMP1 + COD7TE ;


REPCIFAD = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7UA
            + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7UA - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7UA ;
							   
REPCIFADSIN = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * COD7UB
               + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , COD7UB - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + COD7UB ;
							   
REPCIF = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * RDFORESTRA
          + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , RDFORESTRA - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + RDFORESTRA ;
							   
REPCIFSIN = (positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM)) * SINISFORET
             + (1 - positif_ou_nul(VARTMP1 - PLAF_FOREST1 * (1 + BOOL_0AM))) * max(0 , SINISFORET - (PLAF_FOREST1 * (1 + BOOL_0AM) - VARTMP1))) * (1 - V_CNR) ;
VARTMP1 = 0 ;


regle 301365:
application : iliad ;

REP7UP = REPCIF + REPCIFHSN1 + REPCIFHSN2 + REPCIFHSN3 + REPCIFHSN4;
REP7UA = REPCIFAD + REPCIFADHSN1 + REPCIFADHSN2 + REPCIFADHSN3 + REPCIFADHSN4;
REP7UT = REPCIFSIN + REPCIFSN1 + REPCIFSN2 + REPCIFSN3 + REPCIFSN4 + REPCIFSN5 + REPCIFSN6 + REPCIFSN7;
REP7UB = REPCIFADSIN + REPCIFADSSN1 + REPCIFADSSN2 + REPCIFADSSN3 + REPCIFADSSN4 + REPCIFADSSN5 +REPCIFADSSN6 + REPCIFADSSN7; 

regle 301370:
application : iliad ;


CICSG = min(CSGC , arr((IPPNCS + 8SGAUTO) * T_CSGCRDS/100)) ;

CIRDS = min(RDSC , arr((min(REVCSXA , SALECS) + min(REVCSXB , SALECSG + COD8SC)
                        + min(REVCSXC , ALLECS) + min(REVCSXD , INDECS + COD8SW)
                        + min(REVCSXE , PENECS + COD8SX) + min(COD8XI , COD8SA)
                        + min(COD8XJ , COD8SB) + min (COD8XM , GLDGRATV + GLDGRATC)
			+ min(COD8XO , COD8TH) + min (COD8XN , COD8SD)
                       ) * T_RDS/100)) ;

CIPSOL = min(MPSOL , arr((IPPNCS + 8SGAUTO) * TXPSOL/100)) ;

CICVN = min( CVNSALC , arr( min(BCVNSAL,COD8XL) * TX10/100 )) ;

CIGLOA = min( CGLOA , arr ( min(GLDGRATV+GLDGRATC,COD8XM) * T_CSG/100));


CIRSE1 = min( RSE1 , arr( min(SALECS,REVCSXA) * TXTQ/100 ));

RSE8TV = arr(BRSE8TV * TXTV/100) * (1 - positif(ANNUL2042));
RSE8SA = arr(BRSE8SA * TXTV/100) * (1 - positif(ANNUL2042));
CIRSE8TV = min( RSE8TV , arr( min(ALLECS,REVCSXC) * TXTV/100 )) ;
CIRSE8SA = min( RSE8SA , arr(min(COD8SA,COD8XI) * TXTV/100 )) ;
CIRSE2 = min(RSE2, arr(min(ALLECS,REVCSXC)* TXTV/100 + min(COD8SA,COD8XI) * TXTV/100));

CIRSE3 = min( RSE3 , arr( min(COD8SW+INDECS,REVCSXD * TXTW/100 )));

RSE8TX = arr(BRSE8TX * TXTX/100) * (1 - positif(ANNUL2042));
RSE8SB = arr(BRSE8SB * TXTX/100) * (1 - positif(ANNUL2042));
CIRSE8TX = min( RSE8TX , arr( REVCSXE * TXTX/100 )) ;
CIRSE8SB = min( RSE8SB , arr( COD8XJ * TXTX/100 ));
CIRSE4 =  min(RSE4, arr(min(PENECS+COD8SX,REVCSXE)* TXTX/100 + min(COD8XJ,COD8SB) * TXTX/100));

CIRSE5 = min( RSE5 , arr( min(SALECSG+COD8SC,REVCSXB) * TXTR/100 ));

CIRSE6 = min( RSE6 , arr(( min( REVCSXB , SALECSG+COD8SC ) +
                           min( REVCSXC , ALLECS ) +
                           min( COD8XI , COD8SA ) +
			   min( COD8XN, COD8SD )  +
                           min( COD8XO, COD8TH ) 
                         ) * TXCASA/100 ));
			 
CIRSE8 =  arr((min(COD8XN ,COD8SD) + min(COD8XO , COD8TH)) * TX066/100) ;

CIRSETOT = CIRSE1 + CIRSE2 + CIRSE3 + CIRSE4 + CIRSE5 + CIRSE8 ;

regle 301380:
application : iliad ;

CRESINTER = PRESINTER ;

regle 301385:
application : iliad ;



BDCIVHELEC = COD7ZQ + COD7ZR + COD7ZS + COD7ZT + COD7ZU + COD7ZV ; 


BCIVHELEC = BDCIVHELEC * (1-V_CNR) ; 


CI7ZQ = min((COD7ZQ * NOMBRE75/100),LIM300) ;
CI7ZR = min((COD7ZR * NOMBRE75/100),LIM300) ;
CI7ZS = min((COD7ZS * NOMBRE75/100),LIM300) ;
CI7ZT = min((COD7ZT * NOMBRE75/100),LIM300) ;
CI7ZU = min((COD7ZU * NOMBRE75/100),LIM300) ;
CI7ZV = min((COD7ZV * NOMBRE75/100),LIM300) ;

CIVHELEC = arr(CI7ZQ + CI7ZR + CI7ZS + CI7ZT + CI7ZU + CI7ZV)  * (1-V_CNR);

regle 301390:
application : iliad ;

REST = positif(IRE) * positif(CRESTACID) ;
VARTMP1 = 0 ;

LIBREST = positif(REST) * max(0 , min(AUTOVERSLIB , CRESTACID - VARTMP1)) ;
LIBIMP = positif_ou_nul(LIBREST) * (AUTOVERSLIB - LIBREST) ;
VARTMP1 = VARTMP1 + AUTOVERSLIB ;

8TEREST = positif(REST) * max(0 , min(COD8TE , CRESTACID - VARTMP1)) ;
8TEIMP = positif_ou_nul(8TEREST) * (COD8TE - 8TEREST) ;
VARTMP1 = VARTMP1 + COD8TE ;

AGRREST = positif(REST) * max(0 , min(CICONGAGRI , CRESTACID - VARTMP1)) ;
AGRIMP = positif_ou_nul(AGRREST) * (CICONGAGRI - AGRREST) ;
VARTMP1 = VARTMP1 + CICONGAGRI ;

ARTREST = positif(REST) * max(0 , min(CREARTS , CRESTACID - VARTMP1)) ;
ARTIMP = positif_ou_nul(ARTREST) * (CREARTS - ARTREST) ;
VARTMP1 = VARTMP1 + CREARTS ;

FORREST = positif(REST) * max(0 , min(CREFORMCHENT , CRESTACID - VARTMP1)) ;
FORIMP = positif_ou_nul(FORREST) * (CREFORMCHENT - FORREST) ;
VARTMP1 = VARTMP1 + CREFORMCHENT ;

PSIREST = positif(REST) * max(0 , min(CRESINTER , CRESTACID - VARTMP1)) ;
PSIIMP = positif_ou_nul(PSIREST) * (CRESINTER - PSIREST) ;
VARTMP1 = VARTMP1 + CRESINTER ;

HVEREST = positif(REST) * max(0 , min(CREAGRIHVE , CRESTACID - VARTMP1)) ;
HVEIMP = positif_ou_nul(HVEREST) * (CREAGRIHVE - HVEREST) ;
VARTMP1 = VARTMP1 + CREAGRIHVE ;

GLYREST = positif(REST) * max(0 , min(CREAGRIGLY , CRESTACID - VARTMP1)) ;
GLYIMP = positif_ou_nul(GLYREST) * (CREAGRIGLY - GLYREST) ;
VARTMP1 = VARTMP1 + CREAGRIGLY ;

BIOREST = positif(REST) * max(0 , min(CREAGRIBIO , CRESTACID - VARTMP1)) ;
BIOIMP = positif_ou_nul(BIOREST) * (CREAGRIBIO - BIOREST) ;
VARTMP1 = VARTMP1 + CREAGRIBIO ;

8WKREST = positif(REST) * max(0 , min(COD8WK , CRESTACID - VARTMP1)) ;
8WKIMP = positif_ou_nul(8WKREST) * (COD8WK - 8WKREST) ;
VARTMP1 = VARTMP1 + COD8WK ;

FAMREST = positif(REST) * max(0 , min(CREFAM , CRESTACID - VARTMP1)) ;
FAMIMP = positif_ou_nul(FAMREST) * (CREFAM - FAMREST) ;
VARTMP1 = VARTMP1 + CREFAM ;

PAPREST = positif(REST) * max(0 , min(CIPAP , CRESTACID - VARTMP1)) ;
PAPIMP = positif_ou_nul(PAPREST) * (CIPAP - PAPREST) ;
VARTMP1 = VARTMP1 + CIPAP ;

HABREST = positif(REST) * max(0 , min(CIHABPRIN , CRESTACID - VARTMP1)) ;
HABIMP = positif_ou_nul(HABREST) * (CIHABPRIN - HABREST) ;
VARTMP1 = VARTMP1 + CIHABPRIN ;

ROFREST = positif(REST) * max(0 , min(CIFORET , CRESTACID - VARTMP1)) ;
ROFIMP = positif_ou_nul(ROFREST) * (CIFORET - ROFREST) ;
VARTMP1 = VARTMP1 + CIFORET ;

SALREST = positif(REST) * max(0 , min(CIADCRE , CRESTACID - VARTMP1)) ;
SALIMP = positif_ou_nul(SALREST) * (CIADCRE - SALREST) ;
VARTMP1 = VARTMP1 + CIADCRE ;

PREREST = positif(REST) * max(0 , min(CIPRETUD , CRESTACID - VARTMP1)) ;
PREIMP = positif_ou_nul(PREREST) * (CIPRETUD - PREREST) ;
VARTMP1 = VARTMP1 + CIPRETUD ;

SYNREST = positif(REST) * max(0 , min(CISYND , CRESTACID - VARTMP1)) ;
SYNIMP = positif_ou_nul(SYNREST) * (CISYND - SYNREST) ;
VARTMP1 = VARTMP1 + CISYND ;

GARREST = positif(REST) * max(0 , min(CIGARD , CRESTACID - VARTMP1)) ;
GARIMP = positif_ou_nul(GARREST) * (CIGARD - GARREST) ;
VARTMP1 = VARTMP1 + CIGARD ;

BAIREST = positif(REST) * max(0 , min(CICA , CRESTACID - VARTMP1)) ;
BAIIMP = positif_ou_nul(BAIREST) * (CICA - BAIREST) ;
VARTMP1 = VARTMP1 + CICA ;

VEHREST = positif(REST) * max(0 , min(CIVHELEC , CRESTACID - VARTMP1)) ;
VEHIMP = positif_ou_nul(CIVHELEC) * (CIVHELEC - VEHREST) ;
VARTMP1 = VARTMP1 + CIVHELEC ;

TECREST = positif(REST) * max(0 , min(CITEC , CRESTACID - VARTMP1)) ;
TECIMP = positif_ou_nul(TECREST) * (CITEC - TECREST) ;
VARTMP1 = VARTMP1 + CITEC ;

AIDREST = positif(REST) * max(0 , min(CIGE , CRESTACID - VARTMP1)) ;
AIDIMP = positif_ou_nul(AIDREST) * (CIGE - AIDREST) ;
VARTMP1 = VARTMP1 + CIGE ;

HJAREST = positif(REST) * max(0 , min(CIHJA , CRESTACID - VARTMP1)) ;
HJAIMP = positif_ou_nul(HJAREST) * (CIHJA - HJAREST) ;
VARTMP1 = VARTMP1 + CIHJA ;

CORREST = positif(REST) * max(0 , min(CICORSENOW , CRESTACID - VARTMP1)) ;
CORIMP = positif_ou_nul(CORREST) * (CICORSENOW - CORREST) ;
VARTMP1 = VARTMP1 + CICORSENOW ;

EMPREST = positif(REST) * max(0 , min(COD8TL , CRESTACID - VARTMP1)) ;
EMPIMP = positif_ou_nul(EMPREST) * (COD8TL - EMPREST) ;
VARTMP1 = VARTMP1 + COD8TL ;

RECREST = positif(REST) * max(0 , min(IPRECH , CRESTACID - VARTMP1)) ;
RECIMP = positif_ou_nul(RECREST) * (IPRECH - RECREST) ;
VARTMP1 = VARTMP1 + IPRECH ;

ASSREST = positif(REST) * max(0 , min(I2DH , CRESTACID - VARTMP1)) ;
ASSIMP = positif_ou_nul(ASSREST) * (I2DH - ASSREST) ;
VARTMP1 = VARTMP1 + I2DH ;

2CKREST = positif(REST) * max(0 , min(CI2CK , CRESTACID - VARTMP1)) ;
2CKIMP = positif_ou_nul(2CKREST) * (CI2CK - 2CKREST) ;
VARTMP1 = 0 ;



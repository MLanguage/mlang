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


regle 871000:
application : iliad  ;

XBAMIC = COD5XA + COD5YA + COD5ZA; 

regle 871020:
application : iliad  ;

XBAV = BAHEXV + BAEXV ;
XBAC = BAHEXC + BAEXC ;
XBAP = BAHEXP + BAEXP ;
XBIPV = BIHEXV + BICEXV;
XBIPC = BIHEXC + BICEXC;
XBIPP = BIHEXP + BICEXP;
XBINPV = BICNPHEXV + BICNPEXV;
XBINPC = BICNPHEXC + BICNPEXC;
XBINPP = BICNPHEXP + BICNPEXP;
XBNV = BNHEXV + BNCEXV ;
XBNC = BNHEXC + BNCEXC ;
XBNP = BNHEXP + BNCEXP ;
XBNNPV = BNCNPREXV+BNCNPREXAAV ;
XBNNPC = BNCNPREXC+BNCNPREXAAC ;
XBNNPP = BNCNPREXP+BNCNPREXAAP ;

regle 871030:
application : iliad  ;

XBICHDV = (BICEXV + BICNOV)  ;
XBICHDC = (BICEXC + BICNOC)  ;
XBICHDP = (BICEXP + BICNOP)  ;
XBICNETV = XBICHDV - BICDNV;
XBICNETC = XBICHDC - BICDNC;
XBICNETP = XBICHDP - BICDNP;
XBICSV =  XBICNETV + BA1AV ;
XBICSC =  XBICNETC + BA1AC ;
XBICSP =  XBICNETP + BA1AP ;
XBICNPHDV = BICNPEXV + BICREV ;
XBICNPHDC = BICNPEXC + BICREC ;
XBICNPHDP = BICNPEXP + BICREP ;
XBICNPNETV = XBICNPHDV - BICDEV;
XBICNPNETC = XBICNPHDC - BICDEC;
XBICNPNETP = XBICNPHDP - BICDEP;
XBICNPSV =  XBICNPNETV + BI2AV ;
XBICNPSC =  XBICNPNETC + BI2AC ;
XBICNPSP =  XBICNPNETP + BI2AP ;
XBITV = max (0 , XBICNETV + max (0,XBICNPNETV )); 
XBITC = max (0 , XBICNETC + max (0,XBICNPNETC )); 
XBITP = max (0 , XBICNETP + max (0,XBICNPNETP )); 
XBISV = positif(max(0,XBICNETV + max(0,XBICNPNETV)))
        * ( BI2AV  + BI1AV  );
XBISC = positif(max(0,XBICNETC + max(0,XBICNPNETC)))
        * ( BI2AC  + BI1AC  );
XBISP = positif(max(0,XBICNETP + max(0,XBICNPNETP)))
        * ( BI2AP  + BI1AP  );

XBICIMPV =  XBICHDV + XBICNPHDV ;
XBICIMPC =  XBICHDC + XBICNPHDC ;
XBICIMPP =  XBICHDP + XBICNPHDP ;

regle 871040:
application : iliad  ;
 


COD1GHLIM = min(LIM7500,COD1GH);
COD1HHLIM = min(LIM7500,COD1HH);
COD1IHLIM = min(LIM7500,COD1IH);
COD1JHLIM = min(LIM7500,COD1JH);
COD1KHLIM = min(LIM7500,COD1KH);
COD1LHLIM = min(LIM7500,COD1LH);

COD1ADLIM = min(LIM3000 * (1+positif(COD1AV)),COD1AD);
COD1BDLIM = min(LIM3000 * (1+positif(COD1BV)),COD1BD);
COD1CDLIM = min(LIM3000 * (1+positif(COD1CV)),COD1CD);
COD1DDLIM = min(LIM3000 * (1+positif(COD1DV)),COD1DD);
COD1EDLIM = min(LIM3000 * (1+positif(COD1EV)),COD1ED);
COD1FDLIM = min(LIM3000 * (1+positif(COD1FV)),COD1FD);

XHEURSUPP = COD1IH + COD1JH + COD1KH + COD1LH ;

XTSBV = TSBNV + BPCOSAV + TSASSUV + XETRANV + EXOCETV + GLDGRATV + COD1GHLIM + COD1ADLIM+COD1PB;
XTSBC = TSBNC + BPCOSAC + TSASSUC + XETRANC + EXOCETC + GLDGRATC + COD1HHLIM + COD1BDLIM+COD1PC;

XTSB1 = TSBN1 + COD1IHLIM + COD1CDLIM+COD1PD;
XTSB2 = TSBN2 + COD1JHLIM + COD1DDLIM+COD1PE;
XTSB3 = TSBN3 + COD1KHLIM + COD1EDLIM+COD1PF;
XTSB4 = TSBN4 + COD1LHLIM + COD1FDLIM+COD1PG;

XEXTSV = XTSBV + CARTSV + REMPLAV ;
XEXTSC = XTSBC + CARTSC + REMPLAC ;
XEXTS1 = XTSB1 + CARTSP1 + REMPLAP1 ;
XEXTS2 = XTSB2 + CARTSP2 + REMPLAP2 ;
XEXTS3 = XTSB3 + CARTSP3 + REMPLAP3 ;
XEXTS4 = XTSB4 + CARTSP4 + REMPLAP4 ;
XEXTSP = XEXTS1+XEXTS2+XEXTS3+XEXTS4;
regle 871050:
application : iliad  ;
 
XTPS10V = arr (XEXTSV * TX_DEDFORFTS /100) ;
XTPS10C = arr (XEXTSC * TX_DEDFORFTS /100) ;
XTPS101 = arr (XEXTS1 * TX_DEDFORFTS /100) ;
XTPS102 = arr (XEXTS2 * TX_DEDFORFTS /100) ;
XTPS103 = arr (XEXTS3 * TX_DEDFORFTS /100) ;
XTPS104 = arr (XEXTS4 * TX_DEDFORFTS /100) ;
XDFNV =  min( PLAF_DEDFORFTS , XTPS10V ) ;
XDFNC =  min( PLAF_DEDFORFTS , XTPS10C ) ;
XDFN1 =  min( PLAF_DEDFORFTS , XTPS101 ) ;
XDFN2 =  min( PLAF_DEDFORFTS , XTPS102 ) ;
XDFN3 =  min( PLAF_DEDFORFTS , XTPS103 ) ;
XDFN4 =  min( PLAF_DEDFORFTS , XTPS104 ) ;
 
regle 871060:
application : iliad  ;
 
X10MINSV = max( min(XEXTSV,DEDMINV) , XDFNV );
X10MINSC = max( min(XEXTSC,DEDMINC) , XDFNC );
X10MINS1 = max( min(XEXTS1,DEDMIN1) , XDFN1 );
X10MINS2 = max( min(XEXTS2,DEDMIN2) , XDFN2 );
X10MINS3 = max( min(XEXTS3,DEDMIN3) , XDFN3 );
X10MINS4 = max( min(XEXTS4,DEDMIN4) , XDFN4 );
XIND_10V = positif_ou_nul(X10MINSV-FRNV);
XIND_10C = positif_ou_nul(X10MINSC-FRNC);
XIND_101 = positif_ou_nul(X10MINS1-FRN1);
XIND_102 = positif_ou_nul(X10MINS2-FRN2);
XIND_103 = positif_ou_nul(X10MINS3-FRN3);
XIND_104 = positif_ou_nul(X10MINS4-FRN4);
XIND_10P = positif(XIND_101+XIND_102+XIND_103+XIND_104);
XDFV = X10MINSV  ;
XDFC = X10MINSC  ;
XDF1 = X10MINS1  ;
XDF2 = X10MINS2  ;
XDF3 = X10MINS3  ;
XDF4 = X10MINS4  ;
XFPTV = XDFV * XIND_10V + FRDV * (1 - XIND_10V);
XFPTC = XDFC * XIND_10C + FRDC * (1 - XIND_10C);
XFPT1 = XDF1 * XIND_101 + FRD1 * (1 - XIND_101);
XFPT2 = XDF2 * XIND_102 + FRD2 * (1 - XIND_102);
XFPT3 = XDF3 * XIND_103 + FRD3 * (1 - XIND_103);
XFPT4 = XDF4 * XIND_104 + FRD4 * (1 - XIND_104);
XTSNTV =  XEXTSV - XFPTV ;
XTSNTC =  XEXTSC - XFPTC ;
XTSNT1 =  XEXTS1 - XFPT1 ;
XTSNT2 =  XEXTS2 - XFPT2 ;
XTSNT3 =  XEXTS3 - XFPT3 ;
XTSNT4 =  XEXTS4 - XFPT4 ;
 
regle 871070:
application : iliad  ;
 
XTSNV = positif (-XTSNTV) * min (0 , XTSNTV)
        + positif_ou_nul (XTSNTV) * XTSNTV ;
XTSNC = positif (-XTSNTC) * min (0 , XTSNTC)
        + positif_ou_nul (XTSNTC) * XTSNTC ;
XTSN1 = positif (-XTSNT1) * min (0 , XTSNT1)
        + positif_ou_nul (XTSNT1) * XTSNT1 ;
XTSN2 = positif (-XTSNT2) * min (0 , XTSNT2)
        + positif_ou_nul (XTSNT2) * XTSNT2 ;
XTSN3 = positif (-XTSNT3) * min (0 , XTSNT3)
        + positif_ou_nul (XTSNT3) * XTSNT3 ;
XTSN4 = positif (-XTSNT4) * min (0 , XTSNT4)
        + positif_ou_nul (XTSNT4) * XTSNT4 ;
XTSNP = XTSN1+XTSN2+XTSN3+XTSN4;
 
regle 871080:
application : iliad  ;
 
XTSV = XTSNV ;
XTSC = XTSNC ;
XTSP = XTSN1+XTSN2+XTSN3+XTSN4 ;
XTSNNV = arr( positif(XTSNV) * 
         XTSNV  
         * (TSASSUV/XEXTSV)) * XIND_10V 
	 + (1-XIND_10V) * TSASSUV;
XTSNNC = arr( positif(XTSNC) * 
         XTSNC  
         * (TSASSUC/XEXTSC)) * XIND_10C 
	 + (1-XIND_10C) * TSASSUC;
XETSNNV = arr( positif(XTSNV) * 
         XTSNV  
         * (XETRANV/XEXTSV)) * XIND_10V
	 + (1-XIND_10V) * XETRANV;
XETSNNC = arr( positif(XTSNC) * 
         XTSNC  
         * (XETRANC/XEXTSC)) * XIND_10C
	 + (1-XIND_10C) * XETRANC;
XEXOCETV = arr( positif(XTSNV) * 
         XTSNV  
         * (EXOCETV/XEXTSV)) * XIND_10V 
	 + (1-XIND_10V) * EXOCETV;
XEXOCETC = arr( positif(XTSNC) * 
         XTSNC  
         * (EXOCETC/XEXTSC)) * XIND_10C 
	 + (1-XIND_10C) * EXOCETC;
XEXOCET = somme(i=V,C:XEXOCETi);
XHSUPV = arr( positif(XTSNV) *
         XTSNV
          * (COD1GHLIM/XEXTSV)) * XIND_10V
           + (1-XIND_10V) * COD1GHLIM;
XHSUPC = arr( positif(XTSNC) *
            XTSNC
          * (COD1HHLIM/XEXTSC)) * XIND_10C
           + (1-XIND_10C) * COD1HHLIM;
XHSUP1 = arr( positif(XTSN1) *
            XTSN1
          * ((COD1IHLIM)/XEXTS1)) * XIND_101
           + (1-XIND_101) * (COD1IHLIM);
XHSUP2 = arr( positif(XTSN2) *
            XTSN2
          * ((COD1JHLIM)/XEXTS2)) * XIND_102
           + (1-XIND_102) * (COD1JHLIM);
XHSUP3 = arr( positif(XTSN3) *
            XTSN3
          * ((COD1KHLIM)/XEXTS3)) * XIND_103
           + (1-XIND_103) * (COD1KHLIM);
XHSUP4 = arr( positif(XTSN4) *
            XTSN4
          * ((COD1LHLIM)/XEXTS4)) * XIND_104
           + (1-XIND_104) * (COD1LHLIM);
XHSUPP = XHSUP1 + XHSUP2 + XHSUP3 + XHSUP4;
XHSUPTOT = XHSUPV+XHSUPC+XHSUP1+XHSUP2+XHSUP3+XHSUP4 ;
XPRIMV = arr( positif(XTSNV) *
         XTSNV
          * (COD1ADLIM/XEXTSV)) * XIND_10V
           + (1-XIND_10V) * COD1ADLIM;
XPRIMC = arr( positif(XTSNC) *
            XTSNC
          * (COD1BDLIM/XEXTSC)) * XIND_10C
           + (1-XIND_10C) * COD1BDLIM;
XPRIM1 = arr( positif(XTSN1) *
            XTSN1
          * ((COD1CDLIM)/XEXTS1)) * XIND_101
           + (1-XIND_101) * (COD1CDLIM);
XPRIM2 = arr( positif(XTSN2) *
            XTSN2
          * ((COD1DDLIM)/XEXTS2)) * XIND_102
           + (1-XIND_102) * (COD1DDLIM);
XPRIM3 = arr( positif(XTSN3) *
            XTSN3
          * ((COD1EDLIM)/XEXTS3)) * XIND_103
           + (1-XIND_103) * (COD1EDLIM);
XPRIM4 = arr( positif(XTSN4) *
            XTSN4
          * ((COD1FDLIM)/XEXTS4)) * XIND_104
           + (1-XIND_104) * (COD1FDLIM);
XPRIMP = XPRIM1 + XPRIM2 + XPRIM3 + XPRIM4;
XPRIMTOT = XPRIMV+XPRIMC+XPRIM1+XPRIM2+XPRIM3+XPRIM4 ;
XPOURV = arr( positif(XTSNV) *
         XTSNV
          * (COD1PB/XEXTSV)) * XIND_10V
           + (1-XIND_10V) * COD1PB;
XPOURC = arr( positif(XTSNC) *
            XTSNC
          * (COD1PC/XEXTSC)) * XIND_10C
           + (1-XIND_10C) * COD1PC;
XPOUR1 = arr( positif(XTSN1) *
            XTSN1
          * ((COD1PD)/XEXTS1)) * XIND_101
           + (1-XIND_101) * (COD1PD);
XPOUR2 = arr( positif(XTSN2) *
            XTSN2
          * ((COD1PE)/XEXTS2)) * XIND_102
           + (1-XIND_102) * (COD1PE);
XPOUR3 = arr( positif(XTSN3) *
            XTSN3
          * ((COD1PF)/XEXTS3)) * XIND_103
           + (1-XIND_103) * (COD1PF);
XPOUR4 = arr( positif(XTSN4) *
            XTSN4
          * ((COD1PG)/XEXTS4)) * XIND_104
           + (1-XIND_104) * (COD1PG);
XPOURP = XPOUR1 + XPOUR2 + XPOUR3 + XPOUR4;
XPOURTOT = XPOURV+XPOURC+XPOUR1+XPOUR2+XPOUR3+XPOUR4 ;

regle 871100:
application : iliad  ;
 

PVTAUX =COD3AN + BPVSJ + BPVSK + BPV18V + BPCOPTV + BPV40V + COD3WI + COD3WJ+COD3PI ;



regle 871110:
application : iliad  ;
 
GLN3NET = arr(GLN3 * GL3 / REV3);
QUOKIRE =   TEGL3 + TERPQ4
            + somme (x=V,C,1..4 : TERPQPRRx+TERPQPRRZx+ TEGLFx+ TERPQTSx+ TERPQTSREMPx+TERPQPALIMx)
            + TERPQRF1 + TEGLRF2 + TERPQRF3 + TERPQRCMDC + TERPQRCMFU + TERPQRCMCH + TERPQRCMYY
            + TERPQRCMTS + TERPQRCMGO + TERPQRCMTR + TERPQRVO + TERPQRVO5 + TERPQRVO6 + TERPQRVO7
            + TERPQRVOR + TERPQRVO5R + TERPQRVO6R + TERPQRVO7R 
            + TERPQRAF + TERPQRBF + TERPQRCF + TERPQRDF + TERPQREF + TERPQRFF + TERPQRAG + TERPQRBG
            + TERPQRCG + TERPQRDG + TERPQRGG + TERPQRFG + TERPQRAL + TERPQRBL + TERPQRCL + TERPQRDL
            + TERPQREL + TERPQRFL + TERPQRAM + TERPQRBM + TERPQRCM + TERPQRDM + TERPQREM + TERPQRFM 
	    + TERPQRAI + TERPQRBI + TERPQRCK;

regle 871118:
application : iliad  ;


RI1RFR =     positif(COD2OP) * positif(RB01-PVTAXSB-COD3SZ) * max(0,(RB01-PVTAXSB-COD3SZ) - min(ABIMPMV,BTPM3VG+BTPM3UA+BTPM3TJ+BPTPVT))
           + (1-positif(COD2OP)) * RB01;

SOLDE3VR1 = min(max(0,RB01-PVTAXSB-COD3SZ- min(ABIMPMV,BTPM3VG+BTPM3UA+BTPM3TJ+BPTPVT)),max(0,ABIMPMV-BTPM3VG-BTPM3UA-BTPM3TJ-BPTPVT))* positif(RB01-PVTAXSB-COD3SZ- min(ABIMPMV,BTPM3VG+BTPM3UA+BTPM3TJ+BPTPVT))
           + max(0,ABIMPMV-min(ABIMPMV,BTPM3VG+BTPM3UA+BTPM3TJ+BPTPVT) -max(0,RB01-PVTAXSB-COD3SZ)) * (1 - positif(RB01-PVTAXSB-COD3SZ- min(ABIMPMV,BTPM3VG+BTPM3UA+BTPM3TJ+BPTPVT)));

PVTXEFF2 = arr(max(0, GLRVG + GLRUA -SOLDE3VR1 )/4);
PVTXEFF2HR = arr(max(0, GLRVG +GLRUA -SOLDE3VR1 ));
SOLDE3VR2 = max(0, SOLDE3VR1-GLRVG-GLRUA );


SOLDE3VR3 = max(0, (COD3SG+COD3SL+ABDETPLUS+CODRSG+CODRSL+CODRVA)-SOLDE3VR2);

regle 871120:
application : iliad  ;

PVTXEFF =    positif(COD2OP) * (PVTXEFF2 + SOLDE3VR3+ABIMPPV*null(present(ABIMPMV))) + (1-positif(COD2OP)) * max(0,BPVRCM+COD3UA+COD3TJ+GAINPEA-ABIMPMV+ABIMPPV);
PVTXEFFHR =  positif(COD2OP) * (PVTXEFF2HR + SOLDE3VR3+ABIMPPV*null(present(ABIMPMV))) + (1-positif(COD2OP)) * max(0,BPVRCM+COD3UA+COD3TJ+GAINPEA-ABIMPMV+ABIMPPV);


regle 71122:
application : iliad  ;

VARREVKIRE = BPCAPTAXV + BPCAPTAXC + BPCAPTAXP
             + XBAMIC
	     + somme( i=V,C,P: XBAi+XBIPi+XBINPi+XBNi+XBNNPi)
             + somme (i=V,C,P: MIBEXi + MIBNPEXi + BNCPROEXi + XSPENPi)
             + somme (i=V,C,P: BNCCRi)
             + somme (i=V,C,P: BNCCRFi)
             + somme (i=V,C: XETSNNi)
             + somme (i=V,C: XEXOCETi)
             + somme (i=V,C: XTSNNi)
	     + somme (i=V,C,P: XHSUPi)
	     + somme (i=V,C,P: XPRIMi)
	     + somme (i=V,C,P: XPOURi)
             + RCMLIB + PPLIB  + COD2XX + COD2VM + COD2RA
             + GAINABDET
             + RCMEXCREF
             + RCM2FA
             + RCMIMPAT
             + PVIMMO
             + PVMOBNR
	     + COD3WN + COD3XN
             + PVTITRESOC
	     + COD3TK * positif(COD2OP)
             + RCMIMPTN
             + BATMARGTOT
	     + RCMIMPTR
             + max(0 , BTP3A + BPTP10)
             + (1 - positif(present(TAX1649) + present(RE168))) * PVTAUX 
             + COD1UZ + COD1WZ + COD1VZ
             + (APERPV + APERPC + APERPP)* (1 - V_CNR) * (1-INDTEFF)
             + (APERPVTEF + APERPCTEF + APERPPTEF)* (1 - V_CNR) * INDTEFF
	   ;
regle 871125:
application : iliad  ;


REVKIRE = (1-null(IND_TDR)) * (arr (
       max ( 0, RI1RFR * (1-present(IND_TDR))
              + max(0,IND_TDR-PVTAXSB-COD3SZ)
              +  VARREVKIRE
              +  PVTXEFF
	      + QUOKIRE
	      + (V_8ZT + CODZRE + CODZRF) * V_CNR
                     )))
       ;

BNCCREAV = BNCCRV + BNCCRFV ;
BNCCREAC = BNCCRC + BNCCRFC ;
BNCCREAP = BNCCRP + BNCCRFP ;
QUOKIREHR =   TGL1 + TGL2 + TGL3 + TGL4
             + somme (x=V,C,1..4 : TGLPRRx+TGLPRRZx+ TGLFx+ TGLTSx+ TGLTSREMPx+TGLPALIMx)
             + TGLRF1 + TGLRF2 + TGLRF3 + TGLRCMDC + TGLRCMFU + TGLRCMCH + TGLRCMYY
             + TGLRCMTS + TGLRCMGO + TGLRCMTR + TGLRVO + TGLRVO5 + TGLRVO6 + TGLRVO7
             + TGLRVOR + TGLRVO5R + TGLRVO6R + TGLRVO7R
             + TGLRAF + TGLRBF + TGLRCF + TGLRDF + TGLREF + TGLRFF + TGLRAG + TGLRBG
             + TGLRCG + TGLRDG + TGLRGG + TGLRFG + TGLRAL + TGLRBL + TGLRCL + TGLRDL
             + TGLREL + TGLRFL + TGLRAM + TGLRBM + TGLRCM + TGLRDM + TGLREM + TGLRFM 
	     + TGLRAI + TGLRBI + TGLRCK;
REVKIREHR = (1-null(IND_TDR)) * (arr (
       max ( 0, RI1RFR  * (1-present(IND_TDR))
              + IND_TDR
              +  VARREVKIRE  - COD3WN - COD3XN
              +  PVTXEFFHR
	      + QUOKIREHR
	      + (V_8ZT + CODZRE + CODZRF) * V_CNR
              )) * (1-present(COD8YM)) + COD8YM );
regle 871130 :
application :  iliad ;

PPE_DATE_DEB = positif(V_0AV+0) * positif(V_0AZ+0) * (V_0AZ+0)
              + positif(DATRETETR+0) * (DATRETETR+0) * null(V_0AZ+0) ;

PPE_DATE_FIN = positif(BOOL_0AM) * positif(V_0AZ+0) * (V_0AZ+0)
               + positif(DATDEPETR+0) * (DATDEPETR+0) * null(V_0AZ+0) ;

PPE_DEBJJMMMM =  PPE_DATE_DEB + (01010000+ANNEEREV) * null(PPE_DATE_DEB+0);
PPE_DEBJJMM = arr( (PPE_DEBJJMMMM - ANNEEREV)/10000);
PPE_DEBJJ =  inf(PPE_DEBJJMM/100);
PPE_DEBMM =  PPE_DEBJJMM -  (PPE_DEBJJ*100);
PPE_DEBUT = PPE_DEBJJ + (PPE_DEBMM - 1 ) * 30 ;

PPE_FINJJMMMM =  PPE_DATE_FIN + (30120000+ANNEEREV) * null(PPE_DATE_FIN+0);
PPE_FINJJMM = arr( (PPE_FINJJMMMM - ANNEEREV)/10000);
PPE_FINJJ =  inf(PPE_FINJJMM/100);
PPE_FINMM =  PPE_FINJJMM -  (PPE_FINJJ*100);
PPE_FIN = PPE_FINJJ + (PPE_FINMM - 1 ) * 30 - positif (PPE_DATE_FIN) ;

CDEVDUR_NBJ = max(1, arr(min(360 , PPE_FIN - PPE_DEBUT + 1))) ;
CKIREDUR = arr(REVKIRE * 360/CDEVDUR_NBJ);
REVKIREDUR2 = CKIREDUR ;

regle 871140 :
application : bareme ;
REVKIRE =  V_9ZZ;
regle 871150 :
application : iliad ;
REVINDIV1 = max(0 , REVORDI * (1 - INDTEFFPAS) + TREVORDI * INDTEFFPAS - CHARGES) * null(1 - TXPASMIN);
REVINDIV2 = max(0 , REVORDI * (1 - INDTEFFPAS) + TREVORDI * INDTEFFPAS - CHARGES) * null(2 - TXPASMIN);
REVKIRE1 = (1-null(IND_TDR)) * (arr (
               max(0,
               REVINDIV1 - arr((PVTAXSB)/2)
	      + arr(RCMEXCREF/2)
              + TERPQPRRV+TERPQPRRZV+ TEGLFV+ TERPQTSV+ TERPQTSREMPV+TERPQPALIMV+TERPQ4V
              + arr((TEGL3 + TERPQRF1 + TEGLRF2 + TERPQRCMDC + TERPQRCMFU + TERPQRCMCH + TERPQRCMTS 
	             + TERPQRCMGO + TERPQRCMTR + TERPQRVO + TERPQRVO5 + TERPQRVO6 + TERPQRVO7 
		     + TERPQRVOR + TERPQRVO5R + TERPQRVO6R + TERPQRVO7R + TERPQRCF + TERPQRDF 
		     + TERPQREF + TERPQRFF + TERPQRCG + TERPQRDG + TERPQRGG + TERPQRFG + TERPQRCL 
		     + TERPQRDL + TERPQREL + TERPQRFL + TERPQRCM + TERPQRDM + TERPQREM + TERPQRFM)/2)
	      + arr(PVTXEFF/2)
	      + arr((RCM2FA+PVTAUX)/2)
	      + max(0,BAF1AV - COD5XN)+ BA1AV
              + max(0,MIB1AV -MIBDEV+MIBNP1AV-MIBNPDEV)+BI1AV+BI2AV
	      + max(0,BNCPRO1AV-BNCPRODEV+BNCNP1AV-BNCNPDEV)+BN1AV+PVINVE+INVENTV
	      + (APERPV + APERPC + APERPP)/2
	      + BATMARGV
	      + TERPQRAF + TERPQRAG + TERPQRAL + TERPQRAM + COD5XA + BAEXV + BAHEXV
              + MIBEXV +BICEXV +BIHEXV +MIBNPEXV +BICNPEXV +BICNPHEXV
              + BNCPROEXV +BNCEXV +BNHEXV +XSPENPV +BNCNPREXAAV +BNCNPREXV
	      + XETSNNV + XEXOCETV + XTSNNV
	      + BNCCRV + BNCCRFV
	      + (GAINABDET +PVTITRESOC +RCMIMPAT +PVIMMO +PVMOBNR +COD1UZ + COD1WZ +COD1VZ +COD3WN+COD3XN+RCMIMPTN)/2
	      + (PPLIB + RCMLIB + COD2XX + COD2VM)/2
	      + BPCAPTAXV + arr(BPCAPTAXP / 2)
	      + COD3SA/2
	      + arr((max(0 , BAF1AP - COD5ZN) + BA1AP) / 2)
	      + arr((max(0 , MIB1AP - MIBDEP + MIBNP1AP - MIBNPDEP) + BI1AP + BI2AP) / 2)
	      + arr((max(0 , BNCPRO1AP - BNCPRODEP + BNCNP1AP - BNCNPDEP) + BN1AP + PVINPE + INVENTP) / 2)
	      + arr((COD5ZA + BAEXP + BAHEXP) / 2)
	      + arr((MIBEXP + BICEXP + BIHEXP + MIBNPEXP + BICNPEXP + BICNPHEXP) / 2) 
	      + arr((BNCPROEXP + BNCEXP + BNHEXP + XSPENPP + BNCNPREXAAP + BNCNPREXP) / 2)  
	      + arr((BNCCRP + BNCCRFP) / 2)
	      + arr((BAQCP + (DEFNIBAQ * BAQCP / (BAQCV+BAQCC+BAQCP)))/2)  )
               ));
REVKIRE2 = (1-null(IND_TDR)) * (arr (
               max(0,
               REVINDIV2 - arr((PVTAXSB)/2)
	      + arr(RCMEXCREF/2)
              + TERPQPRRC+TERPQPRRZC+ TEGLFC+ TERPQTSC+ TERPQTSREMPC+TERPQPALIMC+ TERPQ4C
              + arr((TEGL3 + TERPQRF1 + TEGLRF2 + TERPQRCMDC + TERPQRCMFU + TERPQRCMCH + TERPQRCMTS 
	             + TERPQRCMGO + TERPQRCMTR + TERPQRVO + TERPQRVO5 + TERPQRVO6 + TERPQRVO7
                     + TERPQRVOR + TERPQRVO5R + TERPQRVO6R + TERPQRVO7R + TERPQRCF + TERPQRDF 
		     + TERPQREF + TERPQRFF + TERPQRCG + TERPQRDG + TERPQRGG + TERPQRFG + TERPQRCL 
		     + TERPQRDL + TERPQREL + TERPQRFL + TERPQRCM + TERPQRDM + TERPQREM + TERPQRFM)/2)
	      + arr(PVTXEFF/2)
	      + arr((RCM2FA+PVTAUX)/2)
	      + max(0,BAF1AC - COD5YN)+ BA1AC
              + max(0,MIB1AC -MIBDEC+MIBNP1AC-MIBNPDEC)+BI1AC+BI2AC
	      + max(0,BNCPRO1AC-BNCPRODEC+BNCNP1AC-BNCNPDEC)+BN1AC+PVINCE+INVENTC
	      + (APERPV + APERPC + APERPP)/2
	      + BATMARGC
	      + TERPQRBF + TERPQRBG + TERPQRBL + TERPQRBM + COD5YA + BAEXC + BAHEXC
              + MIBEXC +BICEXC +BIHEXC +MIBNPEXC +BICNPEXC +BICNPHEXC
              + BNCPROEXC +BNCEXC +BNHEXC +XSPENPC +BNCNPREXAAC +BNCNPREXC
	      + XETSNNC + XEXOCETC + XTSNNC
	      + BNCCRC + BNCCRFC
	      + (GAINABDET +PVTITRESOC +RCMIMPAT +PVIMMO +PVMOBNR +COD1UZ + COD1WZ +COD1VZ  +COD3WN+COD3XN+RCMIMPTN)/2
	      + (PPLIB + RCMLIB + COD2XX + COD2VM)/2
	      + BPCAPTAXC + arr(BPCAPTAXP / 2)
	      + COD3SA/2
	      + arr((max(0 , BAF1AP - COD5ZN) + BA1AP) / 2)
	      + arr((max(0 , MIB1AP - MIBDEP + MIBNP1AP - MIBNPDEP) + BI1AP + BI2AP) / 2)
	      + arr((max(0 , BNCPRO1AP - BNCPRODEP + BNCNP1AP - BNCNPDEP) + BN1AP + PVINPE + INVENTP) / 2)
	      + arr((COD5ZA + BAEXP + BAHEXP) / 2)
	      + arr((MIBEXP + BICEXP + BIHEXP + MIBNPEXP + BICNPEXP + BICNPHEXP) / 2) 
	      + arr((BNCPROEXP + BNCEXP + BNHEXP + XSPENPP + BNCNPREXAAP + BNCNPREXP) / 2)  
	      + arr((BNCCRP + BNCCRFP) / 2)
	      + arr((BAQCP + (DEFNIBAQ * BAQCP / (BAQCV+BAQCC+BAQCP)))/2)  )
               ));
     

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
 #####   ######   ####    #####     #     #####   ########
 #    #  #       #          #       #       #     #      #
 #    #  #####    ####      #       #       #      #    #
 #####   #            #     #       #       #       ####
 #   #   #       #    #     #       #       #      #    #
 #    #  ######   ####      #       #       #     ########
 #
 #
 #
 #
 #                 RES-SER1.m
 #                 ===========
 #
 #
 #                      zones restituees par l'application
 #
 #
 #
 #
 #
 #
regle 111000:
application : iliad , bareme  ;

MCDV = 1 * positif(V_0AM + 0)
     + 2 * positif(V_0AC + 0)
     + 3 * positif(V_0AD + 0)
     + 4 * positif(V_0AV + 0)
     + 5 * positif(V_0AO + 0);

SFM = positif(BOOL_0AM) * (positif(V_0AP) * (2 - positif(V_0AF)) + 3 * (1 - positif(V_0AP)) * positif(V_0AF) 
                           + 4 * (1 - positif(V_0AP)) * (1 - positif(V_0AF)) * positif(V_0AS) * positif(positif_ou_nul(AGV - LIM_AGE_LET_S) + positif_ou_nul(AGC - LIM_AGE_LET_S))) ; 

regle 111010:
application :  iliad , bareme ;


BOOL_V = positif(V_0AV+0) * positif(1 - BOOL_0AZ) 
			  * ((1 - positif(PAC + V_0CH + 0))
			     + positif(PAC + V_0CH + 0) * (3 - null(EAC + V_0CH + 0))) ;

BOOL_CDV = positif( BOOL_V + V_0AC + V_0AD + 0);

BOOL_PACSFL = 1 - positif( PAC +V_0CH + 0);

BOOL_W = positif(V_0AW + 0) * positif_ou_nul( AGV - LIM_AGE_LET_S );

SFCD1 = ( 15 * positif(V_0AN + 0) * (1 - positif(V_0AP + 0)) * (1 - positif(V_0AG + 0)) * (1 - BOOL_W)         
 
       + 2 * positif(V_0AP + 0) * (1-positif(V_0AL+0))          


       + 14 * positif(V_0AG + 0) * (1 - positif(V_0AP + 0)) * (1 - BOOL_W)                   

       + 7 * BOOL_W * (1 - positif(V_0AP + 0)))
       
       * (1-positif(V_0AL+0)) * BOOL_CDV * BOOL_PACSFL;


regle 111020:
application :  iliad , bareme ;

SFL = positif (V_0AL + 0) * BOOL_CDV * BOOL_PACSFL *

      ( 2 * positif(V_0AP + 0) 

      + 9 * ( 1 - BOOL_W ) * positif( 1- V_0AP + 0) * positif(1-(V_0AG + 0)) * positif (1-(V_0AN+0))  

      + 7 * BOOL_W * positif(1-(V_0AP + 0)) 

      + 15 * positif (V_0AN +0) * ( 1 - BOOL_W ) * positif(1-(V_0AG + 0)) * positif(1-(V_0AP + 0)) 

      + 14 * positif (V_0AG +0) * ( 1 - BOOL_W ) * positif(1-(V_0AP + 0))) ;

regle 111030:
application :  iliad , bareme ;



SFCD2 = positif(PAC+V_0CH) * positif(V_0AC + V_0AD + null(2- BOOL_V)) *
	(
		positif(V_0AP+0) * ( 10 * positif(V_0BT+0) * (1-positif(V_0AV))
 			            + 2 * positif(V_0AV)
                                    + 2 * (1 - positif(V_0AV)) *(1 - positif(V_0BT+0)))
          + (1-positif(V_0AP+0)) * ( 11 * positif(V_0BT+0)) * (1-positif(V_0AV+0))
	);

regle 111040:
application :  iliad , bareme ;


SFV1 = 2 * positif(V_0AP + 0) * null(BOOL_V - 3) ;

regle 111050:
application :  iliad , bareme ;


SFV2 = positif(V_0AV * BOOL_0AZ) * (positif(V_0AP) * (2 - positif(V_0AF)) + (1 - positif(V_0AP)) * (3 * positif(V_0AF) + 7 * positif(V_0AW) * (1 - positif(V_0AF)))) ;

regle 111060:
application :  iliad , bareme ;


BOOL_0AM = positif(positif(V_0AM + 0) + positif(V_0AO + 0)) ;


BOOL_0AMN1 = positif(positif(V_BT0AM + 0) + positif(V_BT0AO + 0)); 

regle 111070:
application :  iliad , bareme ;



SFUTILE = SFM + SFCD1 + SFCD2 + SFV1 + SFV2 + SFL ;

regle 111080:
application :  iliad ;


NATPENA = (positif(null(CMAJ - 7) + null(CMAJ - 8) + null(CMAJ - 9) + null(CMAJ - 10) + null(CMAJ - 11) + null(CMAJ - 12) + null(CMAJ - 17) + null(CMAJ - 18))
           + 2 * null(CMAJ - 2)
	   + 4 * positif(null(CMAJ - 3) + null(CMAJ - 4) + null(CMAJ - 5) + null(CMAJ - 6))) * positif(APPLI_COLBERT + APPLI_ILIAD) ;

regle 111090:
application : iliad ;


TSALV = TSBNV ;
TSALC = TSBNC ;

regle 111100:
application : iliad  ;


TSALP = TSBN1 + TSBN2 + TSBN3 + TSBN4 ;

regle 111110:
application : iliad  ;


F10AV = positif(null(IND_10V) + positif(IND_10V)*null(IND_10MIN_0V)+PREM8_11*positif(DEDMINV-TSBV)* positif (FRNV - 10MINSV)) * max(FRDAV,DFNV);
F10AC = positif(null(IND_10C) + positif(IND_10C)*null(IND_10MIN_0C)+PREM8_11*positif(DEDMINC-TSBC)* positif (FRNC - 10MINSC)) * max(FRDAC,DFNC);
F10A1 = positif(null(IND_101) + positif(IND_101)*null(IND_10MIN_01)+PREM8_11*positif(DEDMIN1-TSB1)* positif (FRN1 - 10MINS1)) * max(FRDA1,DFN1);
F10A2 = positif(null(IND_102) + positif(IND_102)*null(IND_10MIN_02)+PREM8_11*positif(DEDMIN2-TSB2)* positif (FRN2 - 10MINS2)) * max(FRDA2,DFN2);
F10A3 = positif(null(IND_103) + positif(IND_103)*null(IND_10MIN_03)+PREM8_11*positif(DEDMIN3-TSB3)* positif (FRN3 - 10MINS3)) * max(FRDA3,DFN3);
F10A4 = positif(null(IND_104) + positif(IND_104)*null(IND_10MIN_04)+PREM8_11*positif(DEDMIN4-TSB4)* positif (FRN4 - 10MINS4)) * max(FRDA4,DFN4);

regle 111120:
application : iliad  ;

F10AP = somme(i=1..4:F10Ai) ;  

regle 111130:
application : iliad  ;


F10BV = positif(positif(IND_10V)*positif(IND_10MIN_0V)*(1-positif(PREM8_11*positif(DEDMINV-TSBV)*positif (FRNV - 10MINSV)))) * 10MINSV ;
F10BC = positif(positif(IND_10C)*positif(IND_10MIN_0C)*(1-positif(PREM8_11*positif(DEDMINC-TSBC)*positif (FRNC - 10MINSC)))) * 10MINSC ;
F10B1 = positif(positif(IND_101)*positif(IND_10MIN_01)*(1-positif(PREM8_11*positif(DEDMIN1-TSB1)*positif (FRN1 - 10MINS1)))) * 10MINS1 ;
F10B2 = positif(positif(IND_102)*positif(IND_10MIN_02)*(1-positif(PREM8_11*positif(DEDMIN2-TSB2)*positif (FRN2 - 10MINS2)))) * 10MINS2 ;
F10B3 = positif(positif(IND_103)*positif(IND_10MIN_03)*(1-positif(PREM8_11*positif(DEDMIN3-TSB3)*positif (FRN3 - 10MINS3)))) * 10MINS3 ;
F10B4 = positif(positif(IND_104)*positif(IND_10MIN_04)*(1-positif(PREM8_11*positif(DEDMIN4-TSB4)*positif (FRN4 - 10MINS4)))) * 10MINS4 ;

regle 111140:
application : iliad  ;

F10BP = somme(i=1..4:F10Bi) ;

regle 111150:
application : iliad  ;


DEDSV =  (10MINSV - DFNV) * (1-positif(F10BV)) * IND_10V ;
DEDSC =  (10MINSC - DFNC) * (1-positif(F10BC)) * IND_10C ;
DEDS1 =  (10MINS1 - DFN1) * (1-positif(F10B1)) * IND_101 ;
DEDS2 =  (10MINS2 - DFN2) * (1-positif(F10B2)) * IND_102 ;
DEDS3 =  (10MINS3 - DFN3) * (1-positif(F10B3)) * IND_103 ;
DEDS4 =  (10MINS4 - DFN4) * (1-positif(F10B4)) * IND_104 ;

regle 111160:
application : iliad  ;

DEDSP = somme( i=1..4: DEDSi ) ;

regle 111170:
application : iliad  ;


PRV = PRBRV ;
PRC = PRBRC ;
PRP = PRBR1 + PRBR2 + PRBR3 + PRBR4 ;

PRZV = PENINV ;
PRZC = PENINC ;
PRZP = PENIN1 + PENIN2 + PENIN3 + PENIN4 ;
PALIP = PALI1 + PALI2 + PALI3 + PALI4 ;

regle 111180:
application : iliad  ;


AB10V = APRV ;
AB10C = APRC ;
AB10P = APR1 + APR2 + APR3 + APR4 ;

regle 111190:
application : iliad  ;


TSPRT =  (TSNNV + PRRV
        + TSNNC + PRRC
        + TSNN1 + PRR1
        + TSNN2 + PRR2
        + TSNN3 + PRR3
        + TSNN4 + PRR4) ;

regle 111210:
application : iliad  ;


BIPNV = BIPTAV + BIHTAV ;                        
BIPNC = BIPTAC + BIHTAC ;                        
BIPNP = BIPTAP + BIHTAP ;                        
BIPN  = BIPNV + BIPNC + BIPNP ;                          
                                                         
BIPNQV = BIPTAQV + BIHTAQV ;                        
BIPNQC = BIPTAQC + BIHTAQC ;                        
BIPNQP = BIPTAQP + BIHTAQP ;                        
BIPNQ  = BIPNQV + BIPNQC + BIPNQP ;                          
                                                         

MIBRV = MIBVENV + MIBPRESV ;
MIBRC = MIBVENC + MIBPRESC ;
MIBRP = MIBVENP + MIBPRESP ;
MIBR = somme(i=V,P,C: MIBRi);
MLOCDECV = MIBGITEV + LOCGITV + MIBMEUV + COD5NW ;
MLOCDECC = MIBGITEC + LOCGITC + MIBMEUC + COD5OW ;
MLOCDECP = MIBGITEP + LOCGITP + MIBMEUP + COD5PW ;
MIBRABV = MIB_ABVV + MIB_ABPV ;
MIBRABC = MIB_ABVC + MIB_ABPC ;
MIBRABP = MIB_ABVP + MIB_ABPP ;
MLOCABV = MIB_ABNPVLV + MIB_ABNPPLV ;
MLOCABC = MIB_ABNPVLC + MIB_ABNPPLC ;
MLOCABP = MIB_ABNPVLP + MIB_ABNPPLP ;
MIBRNETV = max (0,MIBRV - MIBRABV );
MIBRNETC = max (0,MIBRC - MIBRABC );
MIBRNETP = max (0,MIBRP - MIBRABP );
MIBRNET = somme(i=V,C,P:MIBRNETi);
MLOCNETV = max (0,MLOCDECV - MLOCABV );
MLOCNETC = max (0,MLOCDECC - MLOCABC );
MLOCNETP = max (0,MLOCDECP - MLOCABP );
MLOCNET = somme(i=V,C,P:MLOCNETi);
MIBNPRV = MIBNPVENV + MIBNPPRESV ;
MIBNPRC = MIBNPVENC + MIBNPPRESC ;
MIBNPRP = MIBNPVENP + MIBNPPRESP ;
MIBNPRABV = MIB_ABNPVV + MIB_ABNPPV ;
MIBNPRABC = MIB_ABNPVC + MIB_ABNPPC ;
MIBNPRABP = MIB_ABNPVP + MIB_ABNPPP ;
MIBNPRNETV = max (0,MIBNPRV - MIBNPRABV );
MIBNPRNETC = max (0,MIBNPRC - MIBNPRABC );
MIBNPRNETP = max (0,MIBNPRP - MIBNPRABP );

regle 111230:
application : iliad  ;


BINNV = BINTAV + BINHTAV;
BINNC = BINTAC + BINHTAC;
BINNP = BINTAP + BINHTAP;

regle 111270:
application : iliad  ;

BRCM = positif(COD2OP) * (RCMABD + RCMTNC + RCMAV + RCMHAD + RCMHAB + REGPRIV + COD2TT + COD2VV + COD2WW + COD2YY + COD2ZZ + COD2VN + COD2VO + COD2VP + COD2TQ + COD2RB + COD2RC + COD2RD + COD2TZ)
       + (1 - positif(COD2OP)) * (RCMAV + COD2YY + COD2VN + COD2RB) ;

regle 111280:
application : iliad  ;

BRCMQ =positif(COD2OP)*( REVACT + REVPEA + PROVIE + DISQUO + RESTUC + INTERE + CODRYY)
      + (1-positif(COD2OP)) * (PROVIE + CODRYY) ;
BRCMTOT = RCMAB + RTNC + RAVC + RCMNAB + RTCAR + RCMPRIVM   ;

regle 111290:
application : iliad  ;


RRCM = max(0,RCM1) ;

regle 111300:
application : iliad  ;


B1FIS = max(RCM1 + 2RCM + 3RCM + 4RCM + 5RCM + 6RCM + 7RCM , 0) ;

regle 111310:
application : iliad  ;

DRFRP = (abs (DFCE+DFCG) * (1-positif(RFMIC))
         + positif(RFMIC) * abs(min(0 , RFMIC + CODRBE - MICFR - MICFRQ - RFDANT))) *  null(4-V_IND_TRAIT)
        + (((positif(RFMIC) *  abs(min(0,RFMIC - MICFR - RFDANT)- DEFRFNONI)
	            + (1-positif(RFMIC)) * 
		   ((1-positif_ou_nul(RFDORD+RFDHIS+RFDANT -RFDORD1731-RFDHIS1731-RFDANT1731)) 
                      * (max(0,abs(DFCE1731+DFCG1731) - max(0,RFDANT1731 - RFDANT)* positif((-1)*(DFCE1731+DFCG1731)*positif_ou_nul((-1)*(DFCE+DFCG)))))
		       + DEFRFNONI * positif_ou_nul(RFDORD+RFDHIS+RFDANT -RFDORD1731-RFDHIS1731-RFDANT1731)
		       + abs(DFCE+DFCG) * (1-positif(DFCG))* (1-positif(DFCG1731))*(1-positif(DFCE))* (1-positif(DFCE1731)))
		    )) * (1-PREM8_11)
                   + RFDANT* PREM8_11) * null(5 - V_IND_TRAIT);
regle 111320:
application : iliad  ;


DLMRN1TXM = - min(0,MIB_NETCT *(1-positif(MIBNETPTOT))
                          +SPENETCT * (1 - positif(SPENETPF)));


DLMRN1=null(4-V_IND_TRAIT)*abs(min(0,BICNPOCF+BICNPQCF))+ 
       null(5-V_IND_TRAIT)*( max(0,DEFBICNPF-DEFNPI) * positif(DEFBICNPF)+(max(0,-(BINNV+BINNC+BINNP+MIBNETNPTOT))) * null(DEFBICNPF));



regle 111330:
application : iliad  ;

 	 
DLMRN2=null(4-V_IND_TRAIT)*(min(DEFBIC1+0,BICNPREPN1-DLMRN1))+
       null(5-V_IND_TRAIT)*(null(DEFBICNPF) * min(DEFBIC1,DEFNP - DEFNPI)+ positif(DEFBICNPF) * min(DEFBIC1,DEFBICNPF + DEFNP - DEFNPI - DLMRN1));

regle 111340:
application : iliad  ;



DLMRN3=null(4-V_IND_TRAIT)*(min(DEFBIC2+0,BICNPREPN1-DLMRN2-DLMRN1))+ 
       null(5-V_IND_TRAIT)*(null(DEFBICNPF) * min(DEFBIC2,DEFNP - DEFNPI -DLMRN2)+positif(DEFBICNPF) * min(DEFBIC2,DEFBICNPF +DEFNP - DEFNPI- DLMRN1-DLMRN2));
		  
regle 111350:
application : iliad  ;


DLMRN4=null(4-V_IND_TRAIT)*(min(DEFBIC3+0,BICNPREPN1-DLMRN3-DLMRN2-DLMRN1))+
       null(5-V_IND_TRAIT)*(null(DEFBICNPF)*min(DEFBIC3,DEFNP-DEFNPI-DLMRN2-DLMRN3)+positif(DEFBICNPF)*min(DEFBIC3,DEFBICNPF+DEFNP-DEFNPI-DLMRN1-DLMRN2-DLMRN3));


		  
regle 111360:
application : iliad  ;


DLMRN5=null(4-V_IND_TRAIT)*(min(DEFBIC4+0,BICNPREPN1-DLMRN4-DLMRN3-DLMRN2-DLMRN1))+
        null(5-V_IND_TRAIT)*(null(DEFBICNPF)*min(DEFBIC4,DEFNP-DEFNPI-DLMRN2-DLMRN3-DLMRN4)+positif(DEFBICNPF)*min(DEFBIC4,DEFBICNPF+DEFNP-DEFNPI-DLMRN1-DLMRN2-DLMRN3-DLMRN4));

		  
		  
regle 111370:
application : iliad  ;


DLMRN6=null(4-V_IND_TRAIT)*(min(DEFBIC5+0,BICNPREPN1-DLMRN5-DLMRN4-DLMRN3-DLMRN2-DLMRN1))+
       null(5-V_IND_TRAIT)*(null(DEFBICNPF) * min(DEFBIC5,DEFNP - DEFNPI - DLMRN2-DLMRN3-DLMRN4-DLMRN5)+positif(DEFBICNPF) * min(DEFBIC5,DEFBICNPF +DEFNP - DEFNPI- DLMRN1-DLMRN2-DLMRN3-DLMRN4-DLMRN5));

		  

		  
		  
		  #
regle 9030961 :
application :  iliad   ;
DLMRN6P =  positif(DEFBIC5) *
                  ((1-positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)) * DEFBIC5
                  + positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)
                  * min(max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6,0)-DEFBIC5,DEFBIC5)*(-1)
                  * positif_ou_nul(DEFBIC5-max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6,0)))*null(4-V_IND_TRAIT);
DLMRN5P = positif(DEFBIC4) *
                  ((1-positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)) * DEFBIC4
                  + positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)
                  * min(max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6-DEFBIC5,0)-DEFBIC4,DEFBIC4)*(-1)
                  * positif_ou_nul(DEFBIC4-max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6-DEFBIC5,0)))*null(4-V_IND_TRAIT);
DLMRN4P = positif(DEFBIC3) *
                  ((1-positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT))  * DEFBIC3
                  + positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)
                  * min(max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6-DEFBIC5-DEFBIC4,0)-DEFBIC3,DEFBIC3)*(-1)
                  * positif_ou_nul(DEFBIC3-max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6-DEFBIC5-DEFBIC4,0)))*null(4-V_IND_TRAIT);
DLMRN3P = positif(DEFBIC2) *
                  ((1-positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT))* DEFBIC2
                  + positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)
                    * min(max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6-DEFBIC5-DEFBIC4-DEFBIC3,0)-DEFBIC2,DEFBIC2)*(-1)
                  * positif_ou_nul(DEFBIC2-max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6-DEFBIC5-DEFBIC4-DEFBIC3,0)))*null(4-V_IND_TRAIT);
DLMRN2P = positif(DEFBIC1) *
                 ((1-positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT))* DEFBIC1
                 + positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)
                 * min(max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6-DEFBIC5-DEFBIC4-DEFBIC3-DEFBIC2,0)-DEFBIC1,DEFBIC1)*(-1)
                 * positif_ou_nul(DEFBIC1-max(somme(i=V,C,P:BICNPi)+MIB_NETNPCT-DEFBIC6-DEFBIC5-DEFBIC4-DEFBIC3-DEFBIC2,0)))*null(4-V_IND_TRAIT);
regle 903096 :
application :  iliad   ;

DEFBA7 = min(DAGRI6,DAGRI1 + DAGRI2 + DAGRI3 + DAGRI4 + DAGRI5 + DAGRI6- DBAIP- DEFBA2- DEFBA3- DEFBA4- DEFBA5 - DEFBA6);
DLMRN7 = min(DEFBIC6,DEFNP- DEFNPI- DLMRN2- DLMRN3- DLMRN4- DLMRN5 - DLMRN6);
DEFLOC11 = min(LNPRODEF10,TOTDEFLOCNP- DNPLOCIMPU- DEFLOC2BIS- DEFLOC3BIS- DEFLOC4BIS- DEFLOC5BIS- DEFLOC6BIS- DEFLOC7BIS- DEFLOC8BIS- DEFLOC9BIS - DEFLOC10BIS);
BNCDF7 = min(DABNCNP6,DABNCNP- DIDABNCNP- BNCDF2- BNCDF3- BNCDF4- BNCDF5 - BNCDF6);
RNIDF6 = min(DEFAA5,RNIDF- RNIDF2- RNIDF3- RNIDF4 - RNIDF5);
regle 111380:
application : iliad  ;

DLMRN = max(0, DEFNP - BICNPV*positif(BICNPV)-BICNPC*positif(BICNPC)-BICNPP*positif(BICNPP)
                     + abs(BICNPV)*(1-positif(BICNPV))*null(DLMRN1)
                     + abs(BICNPC)*(1-positif(BICNPC))*null(DLMRN1)
                     + abs(BICNPP)*(1-positif(BICNPP))*null(DLMRN1)) + DLMRN1;
TOTDLMRN = somme(i=1..6:DLMRNi);
DLMRNTXM = max(0,DEFNP - BICNPV*positif(BICNPV)-BICNPC*positif(BICNPC)-BICNPP*positif(BICNPP)
         +MIB_NETCT+MIB_NETNPCT+SPENETCT+SPENETNPCT + DLMRN1 
               );

regle 111390: 
application : iliad  ;


DRCVM = DPVRCM ;

regle 111400:
application : iliad  ;


BALNP = max(0,NOCEPIMP) ;


regle 111410:
application : iliad  ;



DALNP = (BNCDF1 + BNCDF2 + BNCDF3 + BNCDF4 + BNCDF5 + BNCDF6) ;

regle 111420:
application : iliad  ;


DESV = REPSOF ;

regle 111440:
application : iliad  ;


DFANT = (DEFAA5 + DEFAA4 + DEFAA3 + DEFAA2 + DEFAA1 + DEFAA0);
DFANTPROV = (RGPROV - DAR) + SOMDEFICIT;

regle 90432 :
application : iliad   ;
DFANTIMPUBAR = arr(DFANTIMPU * ((SHBA+REVTP+BA1+BAHQCF)/(SHBA+REVTP+BA1+BAQCF+BAHQCF+BICPROQF + BICNPQTF+max(0,REVQTOTQHTPROV-REV4PROV)))) * positif(REVQTOTQHTPROV)
              + DFANTIMPU   * (1-positif(REVQTOTQHTPROV));
regle 90433 :
application : iliad   ;
DFANTIMPUQUO = max(0,DFANTIMPU - DFANTIMPUBAR);
regle 111450:
application : iliad  ;

DFANTIMPU =    (( 1-PREM8_11) * (
                                (1-positif(RGPROV)) * (
                                            (1-positif(DAR)) * (1-positif_ou_nul(TSPRT+RCM1)) * (-1) * RGPROV
                                              + positif(DAR) * ( (1-positif_ou_nul(TSPRT+RCM1)) * ( (-1) * TSPRT + DAR))
                                                  )
                                           + positif(RGPROV) * (((1-positif(DAR)) * 0
                                                       +  positif(DAR) * max(0,DAR - RGPROV1731)) * positif_ou_nul(RGPROV1731))
                                         + (1-positif(RGPROV1731))* ((1-positif_ou_nul(BANOR1731)) * abs(BANOR1731) * positif(INDSEUILBA)
                                                 + min(RFDHIS,abs(RGPROV1731)))
                                        )
                 + PREM8_11 *  ((-1) * TSPRT * (1-positif(TSPRT))+(-1) * RCM1 *(1-positif(RCM1))+DAR
                                    + (-1) * BIN * (1-positif(BIN)) + (-1) * BNN * (1-positif(BNN)) + (-1) * NPLOCNETF * (1-positif(NPLOCNETF))
                                          + (1-positif(RGPROV1731))* ((1-positif_ou_nul(BANOR1731)) * abs(BANOR1731)
                                              + min(RFDHIS,abs(RGPROV1731)))
                                     )
                      ) * null(V_IND_TRAIT - 5) * (1-positif(COD9ZA))  * positif(ART1731BIS);
DAR_REP =  somme (i=0..4:DEFAAi ) ;

regle 111460:
application : iliad  ;

SOMDFANTIMPU = DFANTIMPU+TSPRT + RBAT + MIBNETPTOT+BIPN+PLOCNETF+BICNPF+NPLOCNETF+SPENETPF+BNRTOT+BNCIF+RVTOT+RRCM+RRFI+DESV+ESFP+RE168+TAX1649+PREREV+R1649;

regle 111463:
application : iliad  ;

PRORADFANT = (RGPROVHQ / RGPROV);
regle 111465:
application : iliad  ;

RRBGPROV = (RGPROV - DAR ) ;
regle 111470:
application : iliad  ;


RRBG = (RG - DAR ) ;
RRRBG = max(0 , RRBG) ;
DRBG = min(0 , RRBG) ;

regle 111480:
application : iliad  ;


RCMSOCDED = RCMSOC + COD2DF ;

DDCSG = ((V_BTCSGDED * (1-present(DCSG)) + DCSG) 
        + positif(RCMSOC+ COD2DF +0) * (1 - V_CNR)
                            * (1 - positif(present(RE168)+present(TAX1649)))*
          ((positif(COD2OP) * arr(min(RCMSOC + COD2DF , RCMABD + REVACT + RCMAV + PROVIE + RCMHAD + DISQUO + RCMHAB + INTERE + COD2TZ + COD2TT + RCMIMPAT 
	                                                + COD2VV + COD2WW + COD2YY +CODRYY + COD2ZZ + COD2VN + COD2VO + COD2VP + COD2RB + COD2RC + COD2RD ) 
                                  * (T_IDCSG/100))) 
          +((1-positif(COD2OP))* arr(min(COD2DF , RCMAV+ PROVIE + COD2YY + CODRYY + COD2VN + COD2RB )*(T_IDCSG/100))))); 

RDCSG = max (min(DDCSG , RBG1 + TOTALQUO - SDDD) , 0) ;

regle 111490:
application : iliad  ;


DPALE =  somme(i=1..4:CHENFi+NCHENFi)+COD6GX+COD6EX ;
RPALE = max(0,min(somme(i=1..4:min(NCHENFi,LIM_PENSALENF)+min(arr(CHENFi*MAJREV),LIM_PENSALENF))+COD6GZ+COD6EZ,
                RBG1-DDCSG+TOTALQUO-SDDD)) *(1-V_CNR) ;

regle 111500:
application : iliad  ;


DNETU = somme(i=1..4: CHENFi)+COD6GX;
RNETU = max(0,min(somme(i=1..4:min(CHENFi,LIM_PENSALENF))+COD6GZ,
                RBG1+TOTALQUO-SDDD-RPALE)) *(1-V_CNR);

regle 111510:
application : iliad  ;


DPREC = CHNFAC ;

regle 111520:
application : iliad  ;


DFACC = CHRFAC ;
RFACC = max( min(DDFA,RBG1 - RPALE - RPALP  - DDCSG + TOTALQUO - SDDD) , 0) ;

regle 111530:
application : iliad ;


TRANSFERT = R1649 + PREREV + RE168 + TAX1649 ;

regle 111540:
application : iliad  ;


RPALP = max( min(TOTPA,RBG1 - RPALE - DDCSG + TOTALQUO - SDDD) , 0 ) * (1 - V_CNR) ;
DPALP = PAAV + PAAP ;

regle 111550:
application : iliad  ;


DEDIV = (1-positif(RE168+TAX1649))*CHRDED ;
RDDIV = max( min(DEDIV * (1 - V_CNR),RBG1 - RPALE - RPALP - RFACC - DDCSG + TOTALQUO - SDDD ) , 0 ) ;

regle 111555:
application : iliad  ;


DED6DG = (1-positif(RE168+TAX1649))* (1 - V_CNR) * COD6DG;
RD6DG = max( min(DED6DG * (1 - V_CNR),RBG1 - RPALE - RPALP - RFACC - DDCSG + TOTALQUO - SDDD -RDDIV) , 0 ) ;

regle 111560:
application : iliad  ;


NUPROPT = REPGROREP12 + REPGROREP13 + REPGROREP14 + COD6HP + COD6HQ + COD6HR ;

NUREPAR = min(NUPROPT , max(0 , min(PLAF_NUREPAR, RRBG - RPALE - RPALP - RFACC - RDDIV - APERPV - APERPC - APERPP - DDCSG + TOTALQUO - SDDD-RD6DG))) 
	  * ((V_REGCO + 0) dans (1,3,5,6)) ;

REPNUREPART = max(NUPROPT - NUREPAR , 0) ;
 

REPAR4 = (positif_ou_nul(REPGROREP12 - NUREPAR) * REPGROREP13
	  + (1 - positif_ou_nul(REPGROREP12 - NUREPAR)) 
              * max(REPGROREP13 + REPGROREP12 - NUREPAR , 0)) * ((V_REGCO+0) dans (1,3,5,6)) ;

REPAR3 = (positif_ou_nul(REPGROREP12 + REPGROREP13 - NUREPAR) * REPGROREP14
	  + (1 - positif_ou_nul(REPGROREP12 + REPGROREP13 - NUREPAR)) 
              * max(REPGROREP14 + REPGROREP13 + REPGROREP12 - NUREPAR , 0)) * ((V_REGCO+0) dans (1,3,5,6)) ;

REPAR2 = (positif_ou_nul(REPGROREP12 + REPGROREP13 + REPGROREP14 - NUREPAR) * COD6HP
	  + (1 - positif_ou_nul(REPGROREP12 + REPGROREP13 + REPGROREP14 - NUREPAR)) 
              * max(COD6HP + REPGROREP14 + REPGROREP13 + REPGROREP12 - NUREPAR , 0)) * ((V_REGCO+0) dans (1,3,5,6)) ;

REPAR1 = (positif_ou_nul(REPGROREP12 + REPGROREP13 + REPGROREP14 + COD6HP - NUREPAR) * COD6HQ
	  + (1 - positif_ou_nul(REPGROREP12 + REPGROREP13 + REPGROREP14 + COD6HP - NUREPAR)) 
              * max(COD6HQ + COD6HP + REPGROREP14 + REPGROREP13 + REPGROREP12 - NUREPAR , 0)) * ((V_REGCO+0) dans (1,3,5,6)) ;

REPAR = (positif_ou_nul(REPGROREP12 + REPGROREP13 + REPGROREP14 + COD6HP + COD6HQ - NUREPAR) * COD6HR
	  + (1 - positif_ou_nul(REPGROREP12 + REPGROREP13 + REPGROREP14 + COD6HP + COD6HQ - NUREPAR)) 
              * max(COD6HR + COD6HQ + COD6HP + REPGROREP14 + REPGROREP13 + REPGROREP12 - NUREPAR , 0)) * ((V_REGCO+0) dans (1,3,5,6)) ;

REPNUREPAR = REPAR5 + REPAR4 + REPAR3 + REPAR2 + REPAR1 + REPAR ;

regle 111570:
application : iliad  ;


CHTOT = max( 0 , 
   min( DDPA + DDFA + (1-positif(RE168+TAX1649))*CHRDED +DED6DG+ APERPV + APERPC + APERPP + NUREPAR , RBG1
       - DDCSG + TOTALQUO - SDDD) 
           )  * (1-V_CNR) ;

regle 111580:
application : iliad  ;


ABMAR = min(ABTMA,  max(RNG + TOTALQUO - SDDD - SDCC - ABTPA , 0)) ;

regle 111590:
application : iliad  ;


ABVIE = min(ABTPA,max(RNG+TOTALQUO-SDDD-SDCC,0)) ;

regle 111600:
application : iliad  ;


RNI = arr(RI1)*(1 - positif(ANNUL2042)) ;

regle 111610:
application : iliad  ;

TOTALQUORET = min(TOTALQUO,max(TOTALQUO1731,max(TOTALQUO_P,TOTALQUOP2)));

RNIDF = (1 - positif_ou_nul( RG-DAR+TOTALQUO))
         * (
           (1 - positif_ou_nul(RG + TOTALQUO)) *
              (((RG + TOTALQUO) * (-1)) + DAR_REP)
                    + null(RG+TOTALQUO) * (DAR_REP)
                  + positif(RG + TOTALQUO) *
                   (positif(RG + TOTALQUO - DEFAA5) * (DAR - RG - TOTALQUO)
               + (1 -positif(RG + TOTALQUO - DEFAA5)) * DAR_REP)
                      ) * (1-positif(DFANTIMPU))
                     +  positif(DFANTIMPU) * RNIDF1731;
RNIDF1 = ((1-positif_ou_nul(RG + TOTALQUO)) * DEFAA0
        + positif_ou_nul(RG + TOTALQUO) *
        min(max(RG+TOTALQUO-DEFAA5 -DEFAA4 -DEFAA3 -DEFAA2 -DEFAA1,0) -DEFAA0, DEFAA0)*(-1)
     * positif_ou_nul(DEFAA0 -max(RG+TOTALQUO-DEFAA5 -DEFAA4 -DEFAA3 -DEFAA2 -DEFAA1,0)))
          * null(4-V_IND_TRAIT)
         + null(5-V_IND_TRAIT) * min(DEFAA0,RNIDF);
RNIDF2 = ((1 - positif_ou_nul(RG + TOTALQUO)) * (DEFAA1) * (1-PREM8_11)
        + positif_ou_nul(RG + TOTALQUO) *
        min(max(RG+TOTALQUO-DEFAA5-DEFAA4-DEFAA3-DEFAA2,0)-DEFAA1,DEFAA1)*(-1)
        * positif_ou_nul(DEFAA1-max(RG+TOTALQUO-DEFAA5-DEFAA4-DEFAA3-DEFAA2,0)))
          * null(4-V_IND_TRAIT)
         + null(5-V_IND_TRAIT) * min(DEFAA1,RNIDF - RNIDF1);
RNIDF3 = ((1 - positif_ou_nul(RG + TOTALQUO)) * (DEFAA2)
        + positif_ou_nul(RG + TOTALQUO) *
        min(max(RG+TOTALQUO-DEFAA5 -DEFAA4 -DEFAA3,0) -DEFAA2, DEFAA2)*(-1)
     * positif_ou_nul(DEFAA2 -max(RG+TOTALQUO-DEFAA5 -DEFAA4 -DEFAA3,0)))
          * null(4-V_IND_TRAIT)
         + null(5-V_IND_TRAIT) * min(DEFAA2,RNIDF - RNIDF1 - RNIDF2);

RNIDF4 = ((1 - positif_ou_nul(RG + TOTALQUO)) * (DEFAA3)
        + positif_ou_nul(RG + TOTALQUO) *
        min(max(RG+TOTALQUO-DEFAA5 -DEFAA4,0) -DEFAA3, DEFAA3)*(-1)
     * positif_ou_nul(DEFAA3 -max(RG+TOTALQUO-DEFAA5 -DEFAA4,0)))
          * null(4-V_IND_TRAIT)
         + null(5-V_IND_TRAIT) *  min(DEFAA3,RNIDF - RNIDF1 - RNIDF2 - RNIDF3);
RNIDF5 = ((1 - positif_ou_nul(RG + TOTALQUO)) * (DEFAA4)
        + positif_ou_nul(RG + TOTALQUO) *
        min(max(RG+TOTALQUO-DEFAA5,0) -DEFAA4, DEFAA4)*(-1) * positif_ou_nul(DEFAA4 -max(RG+TOTALQUO-DEFAA5,0)))
          * null(4-V_IND_TRAIT)
         + null(5-V_IND_TRAIT) *  min(DEFAA4,RNIDF - RNIDF1 - RNIDF2 - RNIDF3 - RNIDF4);
RNIDF0 = ((1-positif(RG + TOTALQUO)) * (RG + TOTALQUO) * (-1)) * null(4-V_IND_TRAIT)
         + null(5-V_IND_TRAIT) * (RNIDF - RNIDF1 - RNIDF2 - RNIDF3 - RNIDF4 - RNIDF5) ;

regle 111620:
application :  iliad ;


RNICOL = (RNI + RNIDF) ;

regle 111630:
application : iliad  ;

 
TTPVQ = TONEQUOHT * (1 - positif(ANNUL2042));

regle 111640:
application : iliad  ;


TEFF = IPTEFP - IPTEFN + TEFFREVTOT ; 
TEFFP_1 = max(0, TEFF);
TEFFP_2 = (max(0, TEFF) * null(SOMMEMOND_2+0) * null(PREM8_11)
        + positif(positif(SOMMEMOND_2)+positif(PREM8_11))  *  max(0,(1-INDTEFF)*IPTEFP+TEFFREVTOT*INDTEFF+DEFZU-IPTEFN));
TEFFP = TEFFP_2;
TEFFN_1 = ((1-positif_ou_nul(TEFF)) * ( min(0, TEFF) * (-1) ) + 0);
TEFFN_2 = ((1-positif_ou_nul(TEFF)) * ( min(0, TEFF) * (-1) ) + 0) * null(SOMMEMOND_2+0) * null(PREM8_11) + 0;
TEFFN = TEFFN_2;
RDMO = TEFF + (VARRMOND * positif(SOMMEMOND_2) + RMOND * (1 - positif(SOMMEMOND_2*PREM8_11)))
                           - (VARDMOND * positif(SOMMEMOND_2) + DMOND * (1 - positif(SOMMEMOND_2*PREM8_11)));

RMONDT = positif(RMOND + DEFZU - DMOND) * (RMOND + DEFZU - DMOND) ;

DMONDT = max(0 , RMOND + DEFZU - DMOND) ;
RMOND_1 =  RMOND;
RMOND_2 =  max(0,RMOND + DEFZU - DMOND) * positif(positif(SOMMEMOND_2) + positif(PREM8_11))
           + RMOND *  null(SOMMEMOND_2+0) * null(PREM8_11);
DMOND_1 =  DMOND;
DMOND_2 =  DMOND *  null(SOMMEMOND_2+0)* null(PREM8_11) +0;

regle 111650:
application : iliad  ;


FRF = somme (i=V,C,1,2,3,4: FRDi * (1-IND_10i))*(1-positif(APPLI_COLBERT+APPLI_OCEANS)) ;

regle 111670:
application : iliad ;


TX_CSG = T_CSG * (1-positif(APPLI_OCEANS));
TX_RDS = T_RDS * (1-positif(APPLI_OCEANS));
TX_PREL_SOC = (positif(V_EAG + V_EAD) * (TX023)
              + positif(( 1-V_EAD ) * ( 1-V_EAG )) * (TX068))
	      * (1-V_CNR) * (1-positif(APPLI_OCEANS));
TX_IDCSG = T_IDCSG * (1-positif(APPLI_OCEANS));

regle 111680:
application :  iliad ;

SURIMP = IPSURSI ;

REPPLU = CREDPVREP + V_BTPVREP * (1-present(CREDPVREP)) ;

regle 111685:
application : iliad ;


INDM13 = positif((IREST-LIM_INFRESTIT)*(LIM_RESTIT-IREST))
         * positif_ou_nul((IPRECH + COD8TL + CREFAM + COD8WK + CREAGRIBIO + PRESINTER + CREFORMCHENT + CREARTS + CRECONGAGRI + AUTOVERSLIB)-IREST)
	 * (1-positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)));
regle 111690:
application : iliad ;



INDM14 = positif_ou_nul(IREST - LIM_RESTIT2) * (1-positif(APPLI_OCEANS))
       * (1-positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)));
regle 111695:
application : iliad ;

INDM15 = positif_ou_nul(IINET - LIM_RESTNET)
       * (1-positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63))); 
regle 111700:
application : iliad ;

INDDEFICIT = positif(RNIDF1 + DEFBA6 + DEFBA5 + DEFBA4 + DEFBA3 + DEFBA2 +DEFBA1
		   + DRFRP + DLMRN1 + DALNP + IRECR + DPVRCM + MIBDREPV + MIBDREPC
                   + MIBDREPP + MIBDREPNPV + MIBDREPNPC + MIBDREPNPP + SPEDREPV + SPEDREPC
                   + SPEDREPP + SPEDREPNPV + SPEDREPNPC + SPEDREPNPP) * (1-positif(APPLI_OCEANS)) ;

regle 111710:
application : iliad  ;

RP = somme (i=V,C: TSNNi + TSNN2i + BICIMPi + BICNPi + BNNi +  max(0,BANi) + BAEi)
                 + (min (0,BANV) + min (0,BANC)) *
                 (1 - positif(1 + SEUIL_IMPDEFBA - SHBA - REVQTOTQHT
                 - (REVTP - BA1)  ))
                 + max(0 , ANOCEP - ((min(DNOCEP,DNOCEP1731+0) * positif(DEFRI) + DNOCEP * (1 - positif(DEFRI)))
		 +(min(DABNCNP6,DABNCNP61731+0) * positif(DEFRI) + DABNCNP6 * (1 - positif(DEFRI)))
		 +(min(DABNCNP5,DABNCNP51731+0) * positif(DEFRI) + DABNCNP5 * (1 - positif(DEFRI)))
		 +(min(DABNCNP4,DABNCNP41731+0) * positif(DEFRI) + DABNCNP4 * (1 - positif(DEFRI)))
		 +(min(DABNCNP3,DABNCNP31731+0) * positif(DEFRI) + DABNCNP3 * (1 - positif(DEFRI)))
		 +(min(DABNCNP2,DABNCNP21731+0) * positif(DEFRI) + DABNCNP2 * (1 - positif(DEFRI)))
		 +(min(DABNCNP1,DABNCNP11731+0) * positif(DEFRI) + DABNCNP1 * (1 - positif(DEFRI)))
		 ) ) + GLN3 ;

regle 111720:
application : iliad  ;


AGREPAPER = PALIV + PALIC + PALIP + PENSALV + PENSALC + PENSALP ;

AGREPARET = RPALE + RPALP ;


AGREDTARDIF = positif(RETIR + RETTAXA + RETPCAP + RETHAUTREV + RETCS + RETRD + RETPSOL
                      + RETCVN + RETCDIS + RETGLOA + RETRSE1 + RETRSE2 + RETRSE3 + RETRSE4 
                      + RETRSE5 + RETRSE6 + RETCSG820
                      + RETARPRIM + FLAG_RETARD + 0) ;
AGABAT = ABVIE + ABMAR ;
AGREVTP = REVTP ;
AGREI = REI ;
AGRECI = max(0,INE + IRE + CICAP + CICHR) ;
AGRECITOT = INE + IRE + CICAP + CICHR ;
AGRRCM = RRCM + 2RCM;
AGRCVM = BPVRCM + COD3SG + CODRVG;
AGRRF = RRFI + REVRF;
AGRBAR = RBAT + BAQTOTAVIS;
INDTAZ = ((INDIRN1
           * null((1 - V_BTINDIR) * (1 - positif(COD8OT)))
	   * positif((RFRPARQF * NBPT) - REVKIRE)
	   * positif(IDRS4)
	   * (1 - V_MODUL)) * FLAG_BAREM
	  + V_BARINDTAZ * (1 - FLAG_BAREM)) * (1 - null(2 - INDPAS)) ;

AVRICIPROV = positif(null(1-V_REGCO)+null(5-V_REGCO)+null(6-V_REGCO)+null(3-V_REGCO)*null(0-V_INDVB31)) * 
	                   ( max(0 , CIGARD 
			   + RHEBE + RDUFREP + RPINELTOT + RNORMAN + RNORMREP
	                   + RCELTOT 
                       + RILMNP1 + RILMNP3 + RILMNP4 + RLOCANAH +  RREPA + RDONDJ + RDONS + CISYND + RLOGHVH + RLOGHVI + RLOGHVJ + RLOGHVK + RLOGHVL
		        + (RLOG33 + RLOG34 + RLOG35 + RLOG36 + RLOG37 + RLOG38 + RLOG39) * positif(CODHHC)
		        + (RLOG40 + RLOG41 + RLOG42 + RLOG43 + RLOG44 + RLOG45 + RLOG46) * positif(CODHIC)
		        + (RLOG47 + RLOG48 + RLOG49 + RLOG50 + RLOG51 + RLOG52 + RLOG53) * positif(CODHJC)
		        + (RLOG54 + RLOG55 + RLOG56 + RLOG57 + RLOG58 + RLOG59 + RLOG60) * positif(CODHKC)))
          + positif(null(2-V_REGCO)+null(3-V_REGCO)*null(1-V_INDVB31)) 
	                * max(0,  RPIQA + RPIQB + RPIQC + RPIQD + RPIQI + RPIQJ + RPIQK + RPIQL + RPIQQ + RPIQY + RPIQW + RPIQX 
			         + RPIJN + RPIJO + RPIJP + RPIJQ + RPIREPJM + RPIREPKM + RPIREPLM + RPIREPMM
				 + RPIQM + RPIQN + RPIQO + RPIQP + RPIJV + RPIJW + RPIJX + RPIJY 
				 + RNONM + RNONN + RNOPF + RNOPG 
				 + RNORMLG + RNORMLH + RNORMLI + RNORMLJ 
				 + RNONA + RNONB + RNONC + RNOND + RNONE + RNONF + RNONG + RNONH + RNONI + RNONJ 
				 + RNONK + RNONL + RNORMJA + RNORMJB + RNORMJC + RNORMJD + RNORMJR + RNORMJS + RNORMJT + RNORMJU 
				 + RLOCANAH);

AVRICISAPPROV = positif(null(1-V_REGCO)+null(5-V_REGCO)+null(6-V_REGCO)+null(3-V_REGCO)*null(0-V_INDVB31)) 
                  * (max(0 , arr(BADCRE * TX_AIDOMI /100) - COD7HB) * (1 - positif(RE168 + TAX1649)) * (1 - V_CNR)) + 0 ;

AVRICI = null(0-INDTAZ) * AVRICIPROV
        + null(1-INDTAZ) * positif(AVRICIPROV+AVRICISAPPROV-IDRS4) * arr((AVRICIPROV+AVRICISAPPROV-IDRS4)*AVRICIPROV/(AVRICIPROV+AVRICISAPPROV)) +0;
AVRICISAP = null(0-INDTAZ) * AVRICISAPPROV 
        + null(1-INDTAZ) * max(0,AVRICIPROV+AVRICISAPPROV-IDRS4 - AVRICI)+0; 
regle 111730:
application : iliad  ;



IRRAP = min(COD8EA,IRNET);
IRPUR = max(0, IRNET - IRRAP) ;


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

regle isf 77280:
application : iliad ;

INDCODIFI = positif(present(COD9AA) + present(COD9AB) + present(COD9AC) + present(COD9AD) + present(COD9BA) + present(COD9BB) + present(COD9CA) 
                    + present(COD9GF) + present(COD9GH) + present(COD9GL) + present(COD9GM) + present(COD9GN) 
		    + present(COD9GY) + present(COD9NC) + present(COD9NG) 
		    + present(COD9PR) + present(COD9PX) + present(COD9RS) + 0) ;


DACTBOIS = arr( COD9AC*TX25/100);


TR1_BOIS =arr(max(0,min(LIM_TRANCHEBOISGFA , COD9AD))*(TX25/100)) ;
TR2_BOIS =arr(max(0,(COD9AD - LIM_TRANCHEBOISGFA ))*(TX50/100)) ;
	   
DACTBRUR =arr(TR1_BOIS+TR2_BOIS) ;


TR1_GFA =arr(max(0,min(LIM_TRANCHEBOISGFA , COD9BA))*(TX25/100));
TR2_GFA =arr(max(0,(COD9BA - LIM_TRANCHEBOISGFA ))*(TX50/100));
 
DACTGFA =arr(TR1_GFA + TR2_GFA) ;

IFIACT =(COD9AA + COD9AB + DACTBOIS + DACTBRUR + DACTGFA + COD9BB + COD9CA) ;


IFIPAS =(COD9GF + COD9GH) ;

IFIPAT = max(0, IFIACT - IFIPAS)*(1 - positif(ANNUL2042)) ;

DIFIBASE = IFIPAT ;

regle isf 77290:
application : iliad  ;


TR2_IFI = arr( max(0, min( IFIPAT , LIM_TR2_IFI ) - (LIM_TR1_IFI)) * (TX_TR2_IFI/10000)) ;
TR3_IFI = arr( max(0, min(IFIPAT ,LIM_TR3_IFI ) - (LIM_TR2_IFI)) * (TX_TR3_IFI/10000)) ;
TR4_IFI = arr( max(0, min(IFIPAT ,LIM_TR4_IFI ) - (LIM_TR3_IFI)) * (TX_TR4_IFI/100)) ;
TR5_IFI = arr( max(0, min(IFIPAT ,LIM_TR5_IFI) - (LIM_TR4_IFI)) * (TX_TR5_IFI/10000)) ;
TR6_IFI = arr( max(0,IFIPAT -LIM_TR5_IFI) * (TX_TR6_IFI/1000)) ;

IFI1 = (TR2_IFI + TR3_IFI +  TR4_IFI + TR5_IFI + TR6_IFI) ;

regle isf 77300:
application : iliad ;

IFIDEC = arr((17500 - ( (TX_TR5_IFI/10000) *IFIPAT))
                  * positif(IFIPAT-LIM_IFIINF)*positif(LIM_IFIDEC -IFIPAT))
		           * positif(IFI1);
DECIFI = IFIDEC;

regle isf 77310:
application : iliad ;

IFI2 = arr((IFI1 - IFIDEC) * positif( LIM_IFIDEC - 1 - IFIPAT)
       + IFI1 * (1-positif(LIM_IFIDEC - 1 - IFIPAT))) ;
 
regle isf 77314:
application : iliad ;

IFI1731 = positif(positif(SOMMERI_2)+ null(CODE_2042-8) +null(CODE_2042-11));
regle isf 77315:
application : iliad ;

ART1731BISIFI = positif((positif(SOMMERI_2)+PREM8_11)*IFI1731 + FLAG_RETARD*null(CODE_2042-8)+null( CMAJ_ISF - 8)+null( CMAJ_ISF - 11)+null( CMAJ_ISF -34)) * (1-positif(STR_TR09))
               + positif(STR_TR09) * positif(STR_TR03+STR_TR05+STR_TR06+STR_TR07+STR_TR08+STR_TR09+STR_TR10+STR_TR11+STR_TR12+STR_TR13+STR_TR14);
regle isf 77330:
application : iliad ;

PLAF_IFIRED = 50000 * positif(COD9NC+COD9NG);

AIFIDONF =arr(COD9NC * (TX75/100)) ;
AIFIDONCEE = arr(COD9NG * (TX75/100)) ;

RIFIIDONF_1 = min(PLAF_IFIRED ,AIFIDONF);
RIFIDONCEE_1 = max(0, min( PLAF_IFIRED -RIFIIDONF_1, AIFIDONCEE));

regle isf 77350:
application : iliad ;


RIFIDONF_1 = min(PLAF_IFIRED,RIFIIDONF_1);
RIFIDONCEE_2 = max(0, min(PLAF_IFIRED -RIFIIDONF_1,RIFIDONCEE_1));


RDONIFI_1= max( min( RIFIDONF_1, IFI2) , 0);
RDONIFI1 = positif(null(V_IND_TRAIT-4)+positif(COD9ZA)) * (RDONIFI_1) * (1-positif(null(8-CMAJ_ISF)+null(11-CMAJ_ISF)+null(34-CMAJ_ISF)))
          + (max(0,min(RDONIFI_1,max(RDONIFI_P,RDONIFI11731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
	  

RDONIFI2_1 = max( min( RIFIDONCEE_2,IFI2 -RDONIFI1), 0);

RDONIFI2 = positif(null(V_IND_TRAIT-4)+positif(COD9ZA)) * (RDONIFI2_1) * (1-positif(null(8-CMAJ_ISF)+null(11-CMAJ_ISF)+null(34-CMAJ_ISF)))
         + (max(0,min(RDONIFI2_1,max(RDONIFI2_P,RDONIFI21731)))*positif(1-COD9ZA)*(1-positif(PREM8_11)))* null(V_IND_TRAIT-5)+0;




regle isf 77360:
application : iliad ;


IFITRED = RDONIFI1 + RDONIFI2; 


regle isf 77370:
application : iliad ;

IFI3 = max(0, IFI2 - IFITRED);

IFITOTIMPO = present (COD9PR)*(COD9PR + IFI3) ;
IFIREVPROD = COD9PX * TX75/100 ;

IFIPLAF  =arr( max (0, IFITOTIMPO - IFIREVPROD)) ;

PLAFIFI =positif(COD9PR+COD9PX)*(arr( max(0,  IFI3 * positif(IFIPLAF - IFI3)
              + IFIPLAF * (1-positif(IFIPLAF - IFI3)))));
regle isf 77375:
application : iliad ;

IFI4 = max(0, IFI3 - PLAFIFI) ;

regle isf 77376:
application : iliad ;

IFIETR  = positif(IFIPAT)*positif(COD9RS)*(min(IFI4 ,COD9RS));

regle isf 77380:
application : iliad ;

IFITOT = max(0, IFI4 - IFIETR);

regle isf 77390:
application : iliad   ;




COPETOIFI = si (CMAJ_ISF = 7 ou CMAJ_ISF = 10 ou CMAJ_ISF = 17 ou CMAJ_ISF = 18 )
            alors (10)
            sinon
           ( si (CMAJ_ISF = 8 ou CMAJ_ISF = 34 ou CMAJ_ISF = 11 )
            alors (40)
            sinon
            ( si (CMAJ_ISF = 22)
             alors (0)
                   finsi)
                   finsi)
                   finsi;



COPETOIFI2 =si ( CMAJ_ISF = 55 ou CMAJ_ISF = 3)
            alors (40)
            sinon
           ( si (CMAJ_ISF = 4 ou CMAJ_ISF = 5 ou CMAJ_ISF = 35)
            alors (80)
           finsi)
                finsi;






regle isf 77395:
application : iliad   ;


NMAJIFI1 = max (0, MAJO1728IFI + arr(IFITOT * COPETOIFI/100) * positif_ou_nul(IFITOT - SEUIL_12) 
                + FLAG_TRTARDIF * MAJOIFITARDIF_D
               + FLAG_TRTARDIF_F * MAJOIFITARDIF_D
              - FLAG_TRTARDIF_F * ( positif(FLAG_RECTIFMAJO) * MAJOIFITARDIF_R
                                   + (1 - positif(FLAG_RECTIFMAJO)) * MAJOIFITARDIF_A)
              );
  


	      

NMAJIFI4 =  max (0, MAJO1728IFI2+arr(IFITOT * COPETOIFI2/100)*positif_ou_nul(IFITOT - SEUIL_12) 
                ) ;
		



TXPF1728IFI= si (V_CODPF1728ISF = 7 ou V_CODPF1728ISF = 10 ou V_CODPF1728ISF = 17 ou V_CODPF1728ISF = 18 )
            alors (10)
           sinon
               ( si (V_CODPF1728ISF = 8 ou V_CODPF1728ISF = 34 ou V_CODPF1728ISF = 11 ou V_CODPF1728ISF = 55 ou V_CODPF1728ISF = 3)
                    alors (40)
           sinon
               ( si (V_CODPF1728ISF = 4 ou V_CODPF1728ISF = 35 ou V_CODPF1728ISF = 5 ou V_CODPF1728ISF = 55)
                    alors (80)
           sinon
               ( si (V_CODPF1728ISF = 22)
                    alors (0)
                finsi)
                   finsi)
                      finsi)
            finsi;






MAJTXIFI1 = (1 - positif(V_NBCOD1728ISF))
             * ((1 - positif(CMAJ_ISF)) * positif(NMAJIFI1) * TXPF1728IFI + positif(CMAJ_ISF) * COPETOIFI)
             + positif(V_NBCOD1728ISF) * (-1) ;

MAJTXIFI4 = (1 - positif(V_NBCOD1728ISF))
             * ((1 - positif(CMAJ_ISF)) * positif(NMAJIFI4) * TXPF1728IFI + positif(CMAJ_ISF) * COPETOIFI2)
	                  + positif(V_NBCOD1728ISF) * (-1) ;

regle isf 77400:
application : iliad   ;


INTMSIFI = inf( MOISAN_ISF / 10000 );
INTANIFI = arr(( MOISAN_ISF/10000 - INTMSIFI )*10000)  * present(MOISAN_ISF) ;

TXINTIFI1 = (1-null(CMAJ_ISF-22))*(positif((V_ANREV+1)-INTANIFI))*max(0,(((INTANIFI - (V_ANREV+1))*12)+INTMSIFI-6)*TXMOISRETARD);

TXINTIFI2 = (1-null(CMAJ_ISF-22))*(positif_ou_nul(INTANIFI-(V_ANREV+1)))*max(0,(((INTANIFI - (V_ANREV+1))*12)+INTMSIFI-6)*TXMOISRETARD2);

TXINTIFIR1 =(null(CMAJ_ISF-22))*(positif((V_ANREV+1)-INTANIFI))*max(0,(((INTANIFI - (V_ANREV+1))*12)+INTMSIFI-6)*TXMOISRETARD *TXMOISRED);

TXINTIFIR2 =(null(CMAJ_ISF-22))*(positif_ou_nul(INTANIFI-(V_ANREV+1)))*max(0,(((INTANIFI - (V_ANREV+1))*12)+INTMSIFI-6)*TXMOISRETARD2 *TXMOISRED);

TXINTIFI = TXINTIFI1+TXINTIFI2;
TXINTIFI22 = TXINTIFIR1+TXINTIFIR2; 



PTOIFI =arr(IFITOT * COPETOIFI / 100) + arr(IFITOT * TXINTIFI / 100) ;
PTOIFI22 =(null(CMAJ_ISF-22))* arr(IFITOT * COPETOIFI / 100) + arr(IFITOT * TXINTIFI22 / 100) ;
RETIFI =(RETIFI2 + arr(IFITOT * TXINTIFI/100))* positif_ou_nul(IFI4BIS - SEUIL_12) ;
RETXIFI = positif(CMAJ_ISF) * TXINTIFI
       + (1-positif(DO_INR_IFI2))* (
                                      TXINRISF * positif(INRIFI_NET) * (1-positif(INRIFI_NET_1+INRIFI_NET1ADEF+INRIFI_NETADEF))
                            + TINR_1 * positif(INRIFI_NET_1) * (1-positif(INRIFI_NET+INRIFI_NETADEF+INRIFI_NET1ADEF))
                                  + (-1) * positif(INRIFI_NET_1+INRIFI_NETADEF+INRIFI_NET1ADEF)
                                             * positif(INRIFI_NET+INRIFI_NETADEF+INRIFI_NET1ADEF)
                                         )
           + positif(DO_INR_IFI2)* (
                          (-1) * positif(INRIR_NET1ADEF) * positif(INRIFI_NETADEF)
                                + TINR_1_A * positif(INRIFI_NET1ADEF) * (1-positif(INRIFI_NETADEF))
            + TINR_A * (1-positif(INRIFI_NET1ADEF)) * positif(INRIFI_NETADEF)
                             )
                                 ;

NATMAJIFINOR =(positif(positif(RETIFI) * positif(NMAJIFI1)+positif(NMAJIFI1))
           + 2 * positif(RETIFI) * (1-positif(NMAJIFI1)));


NATMAJIFIRED=positif (NATMAJIFINOR)*0 +
            (1-positif(NATMAJIFINOR))*(( positif(positif(RETIFI) * positif(NMAJIFI4)+positif(NMAJIFI4))
       	   + 2 * positif(RETIFI) * (1-positif(NMAJIFI4))));



RETIFIRED = RETIFI22+RETIFI24;

NATMAJIFI = NATMAJIFINOR + NATMAJIFIRED;
regle isf 77410:
application : iliad ;

ILI_SYNT_IFI = (1 - positif_ou_nul(IFI4BIS - SEUIL_12)) * null(V_ANTIFI+0) * IFI4BIS
               + positif_ou_nul(IFI4BIS - SEUIL_12) * IFI4BIS ;

regle isf 77420:
application : iliad ;


IFI4BIS= max( 0, IFITOT )*(1-positif(ANNUL2042)) ;
regle isf 77425:
application : iliad   ;




PIFI = INCIFI_NET   
        + NMAJIFI1+ NMAJIFI4
         + arr(IFITOT * TXINTIFI / 100) * (1-positif(FLAG_PRIM+FLAG_RETARD+FLAG_DEFAUT))
	;
regle isf 77430 :
application : iliad  ;

NAPIFITOT = IFITOT + PIFI ;

regle isf 77440:
application : iliad  ;


IFINET = NAPIFITOT * (1-positif(ANNUL2042)) ;

regle isf 77450:
application : iliad  ;

IFINAP = IFICUM - V_ANTIFI ;
regle isf 77460:
application : iliad  ;

IFIRECOUVR = max(0,IFITOT) ;


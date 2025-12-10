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
regle 80000:
application : iliad   ;


INDCT = positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63));

RFRH1DEF = max(0,RFRH1 - REFRH1_P) ;
RFRH2DEF = max(0,RFRH2 - REFRH2_P) ;

HRBT1 = (RFRH1 * (1-positif(positif_ou_nul(RFRH1DEF) * positif(REFRH1_P)))  * (1-positif(positif(V_0AM * V_0AX* (1-V_0AB))   + positif(V_0AO * V_0AX *(1-V_0AB))  + positif((V_0AD + V_0AC + V_0AV + V_0AM) * V_0AY)  +  positif((V_0AD + V_0AC + V_0AV ) * V_0AZ) + positif((V_0AD +V_0AC + V_0AV + V_0AM) * V_ZDC)))) * null(V_IND_TRAIT - 5) ;
HRBT2 = (RFRH2 * (1-positif(positif_ou_nul(RFRH2DEF) * positif(REFRH2_P)))  * (1-positif(positif(V_0AM * V_0AX* (1-V_0AB))   + positif(V_0AO * V_0AX *(1-V_0AB))  + positif((V_0AD + V_0AC + V_0AV + V_0AM) * V_0AY)  +  positif((V_0AD + V_0AC + V_0AV ) * V_0AZ) + positif((V_0AD +V_0AC + V_0AV + V_0AM) * V_ZDC)))) * null(V_IND_TRAIT - 5) ;


HRBTRFR1 = ((V_BTRFRHR1 * (1-positif_ou_nul(RFRH1)) + RFRH1)
         * (1-positif(positif(V_0AM * V_0AX* (1-V_0AB))   + positif(V_0AO * V_0AX *(1-V_0AB))  + positif(V_0AD * V_0AY)  +  positif(V_0AV * V_0AZ)))) * null(V_IND_TRAIT - 4)
         + (RFRH1 * (1-positif(positif(RFRH1DEF) * positif(REFRH1_P))) * (1-positif(positif(V_0AM * V_0AX* (1-V_0AB))   + positif(V_0AO * V_0AX *(1-V_0AB))  + positif((V_0AD + V_0AC + V_0AV + V_0AM) * V_0AY)  +  positif((V_0AD + V_0AC + V_0AV ) * V_0AZ) + positif((V_0AD +V_0AC + V_0AV + V_0AM) * V_ZDC)))) * null(V_IND_TRAIT - 5)
         + RFRH1DEF * (1-positif(HRBT1)) * positif(positif(V_0AC + V_0AD + V_0AX + V_0AY + V_0AZ+V_ZDC) + positif(V_0AM * V_0AZ)) * null(V_IND_TRAIT - 5);

HRBTRFR2 = ((V_BTRFRHR2 * (1-positif_ou_nul(RFRH2)) + RFRH2)
        * (1-positif(positif(V_0AM * V_0AX* (1-V_0AB))   + positif(V_0AO * V_0AX *(1-V_0AB))  + positif(V_0AD * V_0AY)  +  positif(V_0AV * V_0AZ)))) * null(V_IND_TRAIT - 4)
         + (RFRH2 * (1-positif(positif(RFRH2DEF) * positif(REFRH2_P))) * (1-positif(positif(V_0AM * V_0AX* (1-V_0AB))   + positif(V_0AO * V_0AX *(1-V_0AB))  + positif((V_0AD + V_0AC + V_0AV + V_0AM) * V_0AY)  +  positif((V_0AD + V_0AC + V_0AV ) * V_0AZ) + positif((V_0AD +V_0AC + V_0AV + V_0AM) * V_ZDC)))) * null(V_IND_TRAIT - 5)
          + RFRH2DEF * (1-positif(HRBT2)) * positif(positif(V_0AC + V_0AD + V_0AX + V_0AY + V_0AZ+V_ZDC) + positif(V_0AM * V_0AZ)) * null(V_IND_TRAIT - 5);


HRNBTRFR = positif_ou_nul(V_BTRFRHR1 * (1-positif(RFRH1)) + RFRH1) + positif_ou_nul(V_BTRFRHR2 * (1-positif(RFRH2)) + RFRH2);
HRMOYBTRFR = arr((HRBTRFR1 + HRBTRFR2) /2);
HRLIM15 = positif_ou_nul(REVKIREHR - (1.5 * HRMOYBTRFR));
HRLIMBTRFR2 = positif_ou_nul(LIMHR1 * (1+BOOL_0AM) - HRBTRFR2);
HRLIMBTRFR1 = positif_ou_nul(LIMHR1 * (1+BOOL_0AM) - HRBTRFR1);

HRCONDTHEO = positif(null(2-HRNBTRFR)*positif(HRLIM15)*positif(HRLIMBTRFR1*HRLIMBTRFR2)* (1-positif(CASECHR+0)))
         * (1-positif(positif(V_0AM * V_0AX* (1-V_0AB))   + positif(V_0AO * V_0AX *(1-V_0AB))  + positif(V_0AD * V_0AY)  +  positif(V_0AV * V_0AZ)))* null(V_IND_TRAIT - 4)
         +   positif(null(2-HRNBTRFR)*positif(HRLIM15)*positif(HRLIMBTRFR1*HRLIMBTRFR2)* (1-positif(CASECHR+0)))
	          * (1-positif(positif(V_0AM * (V_0AX+V_0AY + V_0AZ + V_ZDC)* (1-V_0AB))   + positif(V_0AO * (V_0AX+V_0AY + V_0AZ+V_ZDC) *(1-V_0AB))  + positif(V_0AD * (V_0AY+V_0AX+V_0AZ+V_ZDC))  +  positif(V_0AV * (V_0AY+V_0AX+V_0AZ+V_ZDC)) +positif(V_0AX + V_0AY + V_0AZ+V_ZDC))) * null(V_IND_TRAIT - 5)
		           + positif(null(2-HRNBTRFR)*positif(HRLIM15)*positif(HRLIMBTRFR1*HRLIMBTRFR2)* (1-positif(CASECHR+0)))
			            * positif(V_0AX + V_0AY + V_0AZ+V_ZDC)*positif(HRBTRFR1+HRBTRFR2) * null(V_IND_TRAIT - 5)
				    ;
HRBASEFRAC = arr((REVKIREHR - HRMOYBTRFR) / 2);
HRBASELISSE = HRBASEFRAC + HRMOYBTRFR;
CHRREEL1 = positif_ou_nul(LIMHRTX1 * (1+BOOL_0AM)-REVKIREHR) * ((REVKIREHR - LIMHR1 * (1+BOOL_0AM))*TXHR1/100)
                       + (LIMHR1 * (1+BOOL_0AM) * TXHR1/100) * positif(REVKIREHR - LIMHRTX1 * (1+BOOL_0AM));
CHRREEL2 = max(0,(REVKIREHR - LIMHR2*(1+BOOL_0AM))*TXHR2/100);
CHRREELTOT = arr(max(0,CHRREEL1 + CHRREEL2));
CHRTHEO11 = arr(positif_ou_nul(LIMHRTX1 * (1+BOOL_0AM)-HRBASELISSE) * ((HRBASELISSE - LIMHR1 * (1+BOOL_0AM))*TXHR1/100)
                        + (LIMHR1 * (1+BOOL_0AM) * TXHR1/100)* positif(HRBASELISSE - LIMHRTX1 * (1+BOOL_0AM)));
CHRTHEO21 = arr(max(0,(HRBASELISSE - LIMHR2*(1+BOOL_0AM))*TXHR2/100));
CHRTHEOTOT = arr(max(0,CHRTHEO11 + CHRTHEO21)*2);
BHAUTREV = max(0 , REVKIREHR - LIMHR1 * (1 + BOOL_0AM)) ;
CHRAVANT = (max(0,min(CHRREELTOT,CHRTHEOTOT)) * HRCONDTHEO
                     + CHRREELTOT * (1-HRCONDTHEO) ) ;
CHRTEFF = arr(CHRAVANT * (REVKIREHR - TEFFHRC+COD8YJ)/ REVKIREHR);
CHRAPRES = (CHRAVANT * (1-positif(positif(IPMOND)+positif(INDTEFF))) + CHRTEFF * positif(positif(IPMOND)+positif(INDTEFF))) * (1 - positif(RE168 + TAX1649));
regle 80010:
application : iliad   ;
REVKIRE3WH = REVKIREHR+PVREPORT;
HRLIM153WH = positif_ou_nul(REVKIRE3WH - (1.5 * HRMOYBTRFR));
HRCONDTHEO3WH = positif(null(2-HRNBTRFR)*positif(HRLIM153WH)*positif(HRLIMBTRFR1*HRLIMBTRFR2)* (1-positif(CASECHR+0)))
         * (1-positif(positif(V_0AM * V_0AX* (1-V_0AB))   + positif(V_0AO * V_0AX *(1-V_0AB))  + positif(V_0AD * V_0AY)  +  positif(V_0AV * V_0AZ)));
HRBASEFRAC3WH = arr((REVKIRE3WH - HRMOYBTRFR) / 2);
HRBASELISSE3WH = HRBASEFRAC3WH + HRMOYBTRFR;
CHRREEL13WH = positif_ou_nul(LIMHRTX1 * (1+BOOL_0AM)-REVKIRE3WH) * ((REVKIRE3WH - LIMHR1 * (1+BOOL_0AM))*TXHR1/100)
                       + (LIMHR1 * (1+BOOL_0AM) * TXHR1/100) * positif(REVKIRE3WH - LIMHRTX1 * (1+BOOL_0AM));
CHRREEL23WH = max(0,(REVKIRE3WH - LIMHR2*(1+BOOL_0AM))*TXHR2/100);
CHRREELTOT3WH = arr(max(0,CHRREEL13WH + CHRREEL23WH));
CHRTHEO113WH = arr(positif_ou_nul(LIMHRTX1 * (1+BOOL_0AM)-HRBASELISSE3WH) * ((HRBASELISSE3WH - LIMHR1 * (1+BOOL_0AM))*TXHR1/100)
                        + (LIMHR1 * (1+BOOL_0AM) * TXHR1/100)* positif(HRBASELISSE3WH - LIMHRTX1 * (1+BOOL_0AM)));
CHRTHEO213WH = arr(max(0,(HRBASELISSE3WH - LIMHR2*(1+BOOL_0AM))*TXHR2/100));
CHRTHEOTOT3WH = arr(max(0,CHRTHEO113WH + CHRTHEO213WH)*2);
BHAUTREV3WH = max(0 , REVKIRE3WH - LIMHR1 * (1 + BOOL_0AM)) ;
CHRAVANT3WH = (max(0,min(CHRREELTOT3WH,CHRTHEOTOT3WH)) * HRCONDTHEO3WH
                     + CHRREELTOT3WH * (1-HRCONDTHEO3WH) ) ;
CHRTEFF3WH = arr(CHRAVANT3WH * (REVKIRE3WH - TEFFHRC+COD8YJ)/ REVKIRE3WH);
CHRAPRES3WH = (CHRAVANT3WH * (1-positif(positif(IPMOND)+positif(INDTEFF))) + CHRTEFF3WH * positif(positif(IPMOND)+positif(INDTEFF))) * (1 - positif(RE168 + TAX1649));
regle 80020:
application : iliad   ;
IHAUTREVT = max(0,CHRAPRES - CICHR);
IHAUTREVT3WH = max(0,CHRAPRES3WH - CICHR3WH);
regle 80025:
application : iliad   ;
THCHRRO  = arr((max(0,CHRAPRES3WH - CHRAPRES)/PVREPORT)*10000)/100;
regle 80030:
application : iliad   ;
BCHRPVIMP = COD3WN ;
CHRPVIMP = COD3WT ;

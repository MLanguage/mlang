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
regle 841000:
application : iliad ;


BAMICNV = positif_ou_nul(COD5XB) * (max(0 , COD5XB - max(MIN_MBIC , COD5XB*TX87/100))) ;
BAMICNC = positif_ou_nul(COD5YB) * (max(0 , COD5YB - max(MIN_MBIC , COD5YB*TX87/100))) ;
BAMICNP = positif_ou_nul(COD5ZB) * (max(0 , COD5ZB - max(MIN_MBIC , COD5ZB*TX87/100))) ;

BAMIC1V = max(0 , COD5XM - max(MIN_MBIC , COD5XM*TX87/100)) * present(COD5XM)
          + max(0 , V_BTBAMICV2 - max(MIN_MBIC , V_BTBAMICV2*TX87/100)) * present(V_BTBAMICV2) * (1 - present(COD5XM)) ;

BAMIC1C = max(0 , COD5YT - max(MIN_MBIC , COD5YT*TX87/100)) * present(COD5YT)
          + max(0 , V_BTBAMICC2 - max(MIN_MBIC , V_BTBAMICC2*TX87/100)) * present(V_BTBAMICC2) * (1 - present(COD5YT)) ;

BAMIC1P = max(0 , COD5ZT - max(MIN_MBIC , COD5ZT*TX87/100)) * present(COD5ZT)
	  + max(0 , V_BTBAMICP2 - max(MIN_MBIC , V_BTBAMICP2*TX87/100)) * present(V_BTBAMICP2) * (1 - present(COD5ZT)) ;

BAMIC2V = max(0 , COD5YM - max(MIN_MBIC , COD5YM*TX87/100)) * present(COD5YM)
          + max(0 , V_BTBAMICV1 - max(MIN_MBIC , V_BTBAMICV1*TX87/100)) * present(V_BTBAMICV1) * (1 - present(COD5YM)) ;

BAMIC2C = max(0 , COD5YU - max(MIN_MBIC , COD5YU*TX87/100)) * present(COD5YU)
	  + max(0 , V_BTBAMICC1 - max(MIN_MBIC , V_BTBAMICC1*TX87/100)) * present(V_BTBAMICC1) * (1 - present(COD5YU)) ;

BAMIC2P = max(0 , COD5ZU - max(MIN_MBIC , COD5ZU*TX87/100)) * present(COD5ZU)
	  + max(0 , V_BTBAMICP1 - max(MIN_MBIC , V_BTBAMICP1*TX87/100)) * present(V_BTBAMICP1) * (1 - present(COD5ZU)) ;


BAMICV = arr((BAMICNV + BAMIC1V + BAMIC2V) / (1 + positif(present(COD5XM) + present(V_BTBAMICV2)) * (1 + positif(present(COD5YM) + present(V_BTBAMICV1))))) * positif_ou_nul(BAMICNV) ;

BAMICC = arr((BAMICNC + BAMIC1C + BAMIC2C) / (1 + positif(present(COD5YT) + present(V_BTBAMICC2)) * (1 + positif(present(COD5YU) + present(V_BTBAMICC1))))) * positif_ou_nul(BAMICNC) ;

BAMICP = arr((BAMICNP + BAMIC1P + BAMIC2P) / (1 + positif(present(COD5ZT) + present(V_BTBAMICP2)) * (1 + positif(present(COD5ZU) + present(V_BTBAMICP1))))) * positif_ou_nul(BAMICNP) ;
		   
regle 841006:
application : iliad ;


BAMICRSV = COD5XB ;
BAMICRSC = COD5YB ;
BAMICRSP = COD5ZB ;

BAMICABAV = max(min(COD5XB , MIN_MBIC) , COD5XB * TX87/100) ;
BAMICABAC = max(min(COD5YB , MIN_MBIC) , COD5YB * TX87/100) ;
BAMICABAP = max(min(COD5ZB , MIN_MBIC) , COD5ZB * TX87/100) ;

BAABAV = COD5XB - BAMICV ;
BAABAC = COD5YB - BAMICC ;

regle 841010:
application : iliad ;

BARSV = BAHREV + 4BAHREV + COD5AL - BAHDEV ; 
BARSREVV = BAHREV + 4BAHREV + COD5AL ;
BARSC = BAHREC + 4BAHREC + COD5BL - BAHDEC ;
BARSREVC = BAHREC + 4BAHREC + COD5BL ;
BARSP = BAHREP + 4BAHREP + COD5CL - BAHDEP ;
BARSREVP = BAHREP + 4BAHREP + COD5CL ;
BARAV = BACREV + 4BACREV + COD5AK - BACDEV ;
BARREVAV = BACREV + 4BACREV + COD5AK ;
BARAC = BACREC + 4BACREC + COD5BK - BACDEC ;
BARREVAC = BACREC + 4BACREC + COD5BK ;
BARAP = BACREP + 4BACREP + COD5CK - BACDEP ;
BARREVAP = BACREP + 4BACREP + COD5CK ;

RBAV = BARAV + BARSV ;
RBAC = BARAC + BARSC ;
RBAP = BARAP + BARSP ;

regle 841020:
application : iliad ;


DEFBACREV = positif(BARREVAV) * (present(BACDEV) * arr(BACDEV * BACREV / BARREVAV));
DEFBACREC = positif(BARREVAC) * (present(BACDEC) * arr(BACDEC * BACREC / BARREVAC));
DEFBACREP = positif(BARREVAP) * (present(BACDEP) * arr(BACDEP * BACREP / BARREVAP));

4DEFBACREV = positif(BARREVAV) * present(4BACREV) * ( present(COD5AK) * arr(BACDEV * 4BACREV / BARREVAV) + (1-present(COD5AK)) * (BACDEV - DEFBACREV));
4DEFBACREC = positif(BARREVAC) * present(4BACREC) * ( present(COD5BK) * arr(BACDEC * 4BACREC / BARREVAC) + (1-present(COD5BK)) * (BACDEC - DEFBACREC));
4DEFBACREP = positif(BARREVAP) * present(4BACREP) * ( present(COD5CK) * arr(BACDEP * 4BACREP / BARREVAP) + (1-present(COD5CK)) * (BACDEP - DEFBACREP));

EDEFBACREV = positif(BARREVAV) * present(COD5AK) * (BACDEV - DEFBACREV - 4DEFBACREV) ;
				   
EDEFBACREC = positif(BARREVAC) * present(COD5BK) * (BACDEC - DEFBACREC - 4DEFBACREC) ;

EDEFBACREP = positif(BARREVAP) * present(COD5CK) * (BACDEP - DEFBACREP - 4DEFBACREP) ;
				  

regle 841030:
application : iliad ;

R15HC = (BACREV - DEFBACREV)  * positif_ou_nul(BARAV)
        +  BARAV * (1-positif_ou_nul(BARAV)) ;
R15IC = (BACREC - DEFBACREC)  * positif_ou_nul(BARAC)
        +  BARAC * (1-positif_ou_nul(BARAC)) ;
R15JC = (BACREP - DEFBACREP)  * positif_ou_nul(BARAP)
        +  BARAP * (1-positif_ou_nul(BARAP)) ;
BANV = ((BACREV - DEFBACREV) +(COD5AK - EDEFBACREV)) * positif_ou_nul(BARAV)
	+  BARAV * (1-positif_ou_nul(BARAV)) ;

BANC = ((BACREC - DEFBACREC) +(COD5BK - EDEFBACREC)) * positif_ou_nul(BARAC)
	+ BARAC * (1-positif_ou_nul(BARAC)) ;

BANP = ( (BACREP - DEFBACREP) + (COD5CK - EDEFBACREP) ) * positif_ou_nul(BARAP)
	+ BARAP * (1-positif_ou_nul(BARAP)) ;

BAEV = (4BACREV - 4DEFBACREV) * positif_ou_nul(BARAV) + 0 ;

BAEC = (4BACREC - 4DEFBACREC) * positif_ou_nul(BARAC) + 0 ;

BAEP = (4BACREP - 4DEFBACREP) * positif_ou_nul(BARAP) + 0 ;

regle 841040:
application : iliad ;

DEFBAHREV = positif(BARSREVV) * (present(BAHREV) * arr(BAHDEV * BAHREV / BARSREVV));
DEFBAHREC = positif(BARSREVC) * (present(BAHREC) * arr(BAHDEC * BAHREC / BARSREVC));
DEFBAHREP = positif(BARSREVP) * (present(BAHREP) * arr(BAHDEP * BAHREP / BARSREVP));

4DEFBAHREV = positif( BARSREVV) * present(4BAHREV) * ( present(COD5AL) * arr(BAHDEV * 4BAHREV / BARSREVV) + (1-present(COD5AL)) * ( BAHDEV - DEFBAHREV));
4DEFBAHREC = positif( BARSREVC) * present(4BAHREC) * ( present(COD5BL) * arr(BAHDEC * 4BAHREC / BARSREVC) + (1-present(COD5BL)) * ( BAHDEC - DEFBAHREC));
4DEFBAHREP = positif( BARSREVP) * present(4BAHREP) * ( present(COD5CL) * arr(BAHDEP * 4BAHREP / BARSREVP) + (1-present(COD5CL)) * ( BAHDEP - DEFBAHREP));


EDEFBAHREV = positif(BARSREVV) * present(COD5AL) * (BAHDEV - DEFBAHREV - 4DEFBAHREV);

EDEFBAHREC = positif(BARSREVC) * present(COD5BL) * (BAHDEC - DEFBAHREC - 4DEFBAHREC);

EDEFBAHREP = positif(BARSREVP) * present(COD5CL) * (BAHDEP - DEFBAHREP - 4DEFBAHREP);


regle 841050:
application : iliad ;

BAMV = arr(((BAHREV - DEFBAHREV)+(COD5AL - EDEFBAHREV)) * MAJREV20) * positif_ou_nul(BARSV) 
	+ BARSV * (1-positif(BARSV)) ;
R2MAJ5HI = arr((BAHREV - DEFBAHREV)*MAJREV20)*positif_ou_nul(BARSV) + BARSV * (1-positif_ou_nul(BARSV));

BAMC = arr(((BAHREC - DEFBAHREC)+(COD5BL - EDEFBAHREC)) * MAJREV20) * positif_ou_nul(BARSC)
	+ BARSC * (1-positif(BARSC)) ;
R2MAJ5II = arr((BAHREC - DEFBAHREC)*MAJREV20)*positif_ou_nul(BARSC) + BARSC * (1-positif_ou_nul(BARSC));	

BAMP = arr(((BAHREP - DEFBAHREP)+(COD5CL - EDEFBAHREP)) * MAJREV20)  * positif_ou_nul(BARSP)
	+ BARSP * (1-positif(BARSP)) ;
R2MAJ5JI = arr((BAHREP - DEFBAHREP)*MAJREV20)*positif_ou_nul(BARSP) + BARSP * (1-positif_ou_nul(BARSP));

BAEMV = (arr((4BAHREV - 4DEFBAHREV) * MAJREV20)) * positif_ou_nul(BARSV) + 0;

BAEMC = (arr((4BAHREC - 4DEFBAHREC) * MAJREV20)) * positif_ou_nul(BARSC) + 0;

BAEMP = (arr((4BAHREP - 4DEFBAHREP) * MAJREV20)) * positif_ou_nul(BARSP) + 0;


IBAMICV = BAMICV + BAFPVV - COD5XO ;
IBAMICC = BAMICC + BAFPVC - COD5YO ;
IBAMICP = BAMICP + BAFPVP - COD5ZO ;

IBAMICF = IBAMICV + IBAMICC + IBAMICP ;

regle 841070:
application : iliad ;


BAHDEF = BAFORESTV + BAFPVV + BACREV + BAHREV * MAJREV20 + BAFORESTC + BAFPVC + BACREC + BAHREC * MAJREV20
       + BAFORESTP + BAFPVP + BACREP + BAHREP * MAJREV20 + 4BACREV + 4BAHREV * MAJREV20 + 4BACREC + 4BAHREC * MAJREV20 + 4BACREP + 4BAHREP * MAJREV20+0;

BAHQNODEFV = (BANV + BAMV + IBAMICV + BAFORESTV);
BAHQNODEFC = (BANC + BAMC + IBAMICC + BAFORESTC);
BAHQNODEFP = (BANP + BAMP + IBAMICP + BAFORESTP);

BAHQNODEFF = BAHQNODEFV + BAHQNODEFC + BAHQNODEFP;

BAHQAVISV = BANV + BAMV ;
BAHQAVISC = BANC + BAMC ;
BAHQAVISP = BANP + BAMP ;

regle 841080:
application : iliad ;



BAQNODEFV = BAEV + BAEMV ; 
BAQNODEFC = BAEC + BAEMC ;
BAQNODEFP = BAEP + BAEMP ;

BAQNODEFF = BAQNODEFV + BAQNODEFC + BAQNODEFP;

BAQAVISV = max(0,BAEV + BAEMV + ( BAHQNODEFV * (1-positif(BAHQNODEFV)))) ;
BAQAVISC = max(0,BAEC + BAEMC + ( BAHQNODEFC * (1-positif(BAHQNODEFC)))) ;
BAQAVISP = max(0,BAEP + BAEMP + ( BAHQNODEFP * (1-positif(BAHQNODEFP)))) ;

regle 841090:
application : iliad ;


DEFANTBAF = somme (i=1..6:DAGRIi);

BAHQCV = (1-positif(BAHQNODEFV)) * positif_ou_nul(BAQNODEFV) * min(0,BAHQNODEFV + BAQNODEFV) + positif_ou_nul(BAHQNODEFV) * BAHQNODEFV;
BAHQCC = (1-positif(BAHQNODEFC)) * positif_ou_nul(BAQNODEFC) * min(0,BAHQNODEFC + BAQNODEFC) + positif_ou_nul(BAHQNODEFC) * BAHQNODEFC;
BAHQCP = (1-positif(BAHQNODEFP)) * positif_ou_nul(BAQNODEFP) * min(0,BAHQNODEFP + BAQNODEFP) + positif_ou_nul(BAHQNODEFP) * BAHQNODEFP;

BAHQCF = BAHQCV + BAHQCC + BAHQCP;

regle 841092:
application : iliad ;

BAQCV =   positif(BAHQNODEFV) * (BAQNODEFV)
           + (1-positif(BAHQNODEFV))* max(0,BAQNODEFV+BAHQNODEFV);
BAQCC =   positif(BAHQNODEFC) * (BAQNODEFC)
           + (1-positif(BAHQNODEFC)) * max(0,BAQNODEFC+BAHQNODEFC);
BAQCP =   positif(BAHQNODEFP) * (BAQNODEFP)
           + (1-positif(BAHQNODEFP)) * max(0,BAQNODEFP+BAHQNODEFP);

BAQCF = BAQCV + BAQCC + BAQCP;
regle 841095:
application : iliad ;

DEFANTBA = max(0,min(DEFANTBAF,BAHQCF+ BAQCF));
DEFANTBAV = arr(DEFANTBA * (BAHQCV+BAQCV)/(max(0,BAHQCV+BAQCV)+max(0,BAHQCC+BAQCC)+max(0,BAHQCP+BAQCP)))*positif(BAHQCV+BAQCV);
DEFANTBAC = arr(DEFANTBA * (BAHQCC+BAQCC)/(max(0,BAHQCV+BAQCV)+max(0,BAHQCC+BAQCC)+max(0,BAHQCP+BAQCP)))*positif(BAHQCC+BAQCC) * positif(abs(BAHQCP+BAQCP))
           + max(0,DEFANTBA - DEFANTBAV) * (1-positif(abs(BAHQCP+BAQCP)));
DEFANTBAP = max(0, DEFANTBA - DEFANTBAV - DEFANTBAC)*positif(BAHQCP+BAQCP);
DEFANTIMPHQ = max(0 , min(BAHQCF, DEFANTBAF)) ;
DEFANTIMPQ = max(0 , min(BAQCF, DEFANTBAF -DEFANTIMPHQ)) ;
DEFANTIMP = max(0 , min(BAHQCF + BAQCF, DEFANTBAF)) ;

BAHQF = (1-positif(BAHQCF)) * positif_ou_nul(BAQCF) * min(0,BAHQCF-DEFANTIMPHQ+BAQCF)
          + positif(BAHQCF) * (BAHQCF-DEFANTIMPHQ);
BAQF = (1-positif(BAHQCF)) * positif_ou_nul(BAQCF) * max(0,BAHQCF+BAQCF-DEFANTIMPQ)
	            + positif(BAHQCF) * max(0,BAQCF-DEFANTIMPQ);
regle 841097:
application : iliad ;

BAHQV = max(0 , BAHQCV - DEFANTBAV) * positif(BAHQCV) + BAHQCV * (1 - positif(BAHQCV));
BAHQC = max(0 , BAHQCC - DEFANTBAC) * positif(BAHQCC) + BAHQCC * (1 - positif(BAHQCC));
BAHQP = max(0 , BAHQCP - DEFANTBAP) * positif(BAHQCP) + BAHQCP * (1 - positif(BAHQCP));


regle 841098:
application : iliad ;

BAQV =  max(0,BAQCV - max(0,(DEFANTBAV-BAHQCV)));
BAQC =  max(0,BAQCC - max(0,(DEFANTBAC-BAHQCC)));
BAQP =  max(0,BAQCP - max(0,(DEFANTBAP-BAHQCP)));

regle 841110:
application : iliad ;

BA1V = BA1AV + max(0,(BAF1AV -COD5XN));
BA1C = BA1AC + max(0,(BAF1AC - COD5YN));
BA1P = BA1AP + max(0,(BAF1AP - COD5ZN));

regle 841120:
application : iliad ;

BAHQT = max(0,BANOR+DEFBANIF-DEFNIBAQ);

regle 841125:
application : iliad ;
BAQT = BAQV + BAQC + BAQP;

regle 841180:
application : iliad ;


BA1 = max(0 , BA1V) + max(0 , BA1C) + max(0 , BA1P) ; 

regle 841190:
application : iliad ;

BANOR =  BAHQF;

regle 841200:
application : iliad ;


BAGF1A = BANV + BAMV + BANC + BAMC + BANP + BAMP  
         + (max(0,(4BACREV - 4DEFBACREV))*positif_ou_nul(BARAV)+arr(max(0,(4BAHREV - 4DEFBAHREV))*MAJREV20) * positif_ou_nul(BARSV))
         + (max(0,(4BACREC - 4DEFBACREC))*positif_ou_nul(BARAC)+arr(max(0,(4BAHREC - 4DEFBAHREC))*MAJREV20) * positif_ou_nul(BARSC))
         + (max(0,(4BACREP - 4DEFBACREP))*positif_ou_nul(BARAP)+arr(max(0,(4BAHREP - 4DEFBAHREP))*MAJREV20) * positif_ou_nul(BARSP)) ;

DEFBA1 = ((1-positif(BAHQNODEFF+BAQNODEFF)) * (abs(BAHQNODEFF+BAQNODEFF) - (DEFIBA))
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 positif_ou_nul(DAGRI5+DAGRI4+DAGRI3+DAGRI2+DAGRI1-BAHQNODEFF-BAQNODEFF-BAEV - BAEMV-BAEC - BAEMC-BAEP - BAEMP)
                 * (DAGRI5+DAGRI4+DAGRI3+DAGRI2+DAGRI1-BAHQNODEFF-BAQNODEFF-BAEV - BAEMV-BAEC - BAEMC-BAEP - BAEMP)
                  * null(DEFBA2P+DEFBA3P+DEFBA4P+DEFBA5P+DEFBA6P)) * null(4 - V_IND_TRAIT)
                 +  (positif(SHBA-SEUIL_IMPDEFBA) * positif(DEFBANIF) * max(0,DEFBANIF - DBAIP) * positif(DEFBANIF+0)
                 + positif(SHBA-SEUIL_IMPDEFBA) * max(0,-(BAHQV+BAHQC+BAHQP+4BAQV+4BAQC+4BAQP))* (1-positif(DEFBANIF+0))) * null(5 - V_IND_TRAIT);

regle 860:
application : iliad  ;
DEFBA2 = ((1-positif(BAHQNODEFF+BAQNODEFF)) * (DAGRI1)
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4-DAGRI3-DAGRI2,0)-DAGRI1,DAGRI1))
                 * positif_ou_nul(DAGRI1-max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4-DAGRI3-DAGRI2,0))) * null(4-V_IND_TRAIT)
                  + (positif(DEFBANIF) * min(DAGRI1,DEFBANIF+DEFANTBAF-DBAIP-DEFBA1)
                    + null(DEFBANIF) * min(DAGRI1,DEFANTBAF-DBAIP))  * null(5-V_IND_TRAIT);
regle 862:
application : iliad  ;
DEFBA3 = ((1-positif(BAHQNODEFF+BAQNODEFF)) * DAGRI2
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4-DAGRI3,0)-DAGRI2,DAGRI2))
                 * positif_ou_nul(DAGRI2-max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4-DAGRI3,0))) * null(4-V_IND_TRAIT)
                  + (null(DEFBANIF) * min(DAGRI2,DEFANTBAF-DBAIP-DEFBA2)
                    + positif(DEFBANIF) * min(DAGRI2,DEFBANIF+DEFANTBAF-DBAIP-DEFBA1- DEFBA2))  * null(5-V_IND_TRAIT);
regle 864:
application : iliad  ;
DEFBA4 = ((1-positif(BAHQNODEFF+BAQNODEFF)) * (DAGRI3)
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4,0)-DAGRI3,DAGRI3))
                 * positif_ou_nul(DAGRI3-max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4,0))) * null(4-V_IND_TRAIT)
                  + (null(DEFBANIF) * min(DAGRI3,DEFANTBAF-DBAIP-DEFBA2-DEFBA3)
                    + positif(DEFBANIF) * min(DAGRI3,DEFBANIF+DEFANTBAF-DBAIP-DEFBA1- DEFBA2-DEFBA3))  * null(5-V_IND_TRAIT);
regle 866:
application : iliad  ;
DEFBA5 = ((1-positif(BAHQNODEFF+BAQNODEFF)) * (DAGRI4)
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5,0)-DAGRI4,DAGRI4))
                 * positif_ou_nul(DAGRI4-max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5,0))) * null(4-V_IND_TRAIT)
                  + (null(DEFBANIF) * min(DAGRI4,DEFANTBAF-DBAIP-DEFBA2-DEFBA3-DEFBA4)
                    + positif(DEFBANIF) * min(DAGRI4,DEFBANIF+DEFANTBAF-DBAIP-DEFBA1- DEFBA2-DEFBA3-DEFBA4))  * null(5-V_IND_TRAIT);
regle 868:
application : iliad  ;
DEFBA6 = ((1-positif(BAHQNODEFF+BAQNODEFF)) * (DAGRI5)
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6,0)-DAGRI5,DAGRI5))
                 * positif_ou_nul(DAGRI5-max(BAHQNODEFF+BAQNODEFF-DAGRI6,0))) * null(4-V_IND_TRAIT)
                  + (null(DEFBANIF) * min(DAGRI5,DEFANTBAF-DBAIP-DEFBA2-DEFBA3-DEFBA4-DEFBA5)
                    + positif(DEFBANIF) * min(DAGRI5,DEFBANIF+DEFANTBAF-DBAIP-DEFBA1- DEFBA2-DEFBA3-DEFBA4-DEFBA5))  * null(5-V_IND_TRAIT);
regle 870:
application : iliad  ;
DEFBA2P = ((1-positif(BAHQNODEFF+BAQNODEFF)) * (DAGRI1)
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4-DAGRI3-DAGRI2,0)-DAGRI1,DAGRI1))
                 * positif_ou_nul(DAGRI1-max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4-DAGRI3-DAGRI2,0)));
regle 87202:
application : iliad  ;
DEFBA3P = ((1-positif(BAHQNODEFF+BAQNODEFF)) * DAGRI2
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4-DAGRI3,0)-DAGRI2,DAGRI2))
                 * positif_ou_nul(DAGRI2-max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4-DAGRI3,0)));
regle 874:
application : iliad  ;
DEFBA4P = ((1-positif(BAHQNODEFF+BAQNODEFF)) * (DAGRI3)
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4,0)-DAGRI3,DAGRI3))
                 * positif_ou_nul(DAGRI3-max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5-DAGRI4,0)));
regle 87602:
application : iliad  ;
DEFBA5P = ((1-positif(BAHQNODEFF+BAQNODEFF)) * (DAGRI4)
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5,0)-DAGRI4,DAGRI4))
                 * positif_ou_nul(DAGRI4-max(BAHQNODEFF+BAQNODEFF-DAGRI6-DAGRI5,0)));
regle 878:
application : iliad  ;
DEFBA6P = ((1-positif(BAHQNODEFF+BAQNODEFF)) * (DAGRI5)
                 + positif(BAHQNODEFF+BAQNODEFF) *
                 abs(min(max(BAHQNODEFF+BAQNODEFF-DAGRI6,0)-DAGRI5,DAGRI5))
                 * positif_ou_nul(DAGRI5-max(BAHQNODEFF+BAQNODEFF-DAGRI6,0)));
regle 841210:
application : iliad ;


DEFIBAANT = positif_ou_nul(BAQT+BAHQTOT-DAGRI1 - DAGRI2 - DAGRI3 - DAGRI4 - DAGRI5 - DAGRI6 )
            * (DAGRI1 - DAGRI2 - DAGRI3 - DAGRI4 - DAGRI5 - DAGRI6)
            + positif_ou_nul(DAGRI1 + DAGRI2 + DAGRI3 + DAGRI4 + DAGRI5 + DAGRI6 -BAQT-BAHQTOT)
            * (BAQT+BAHQTOT);

regle 841230:
application : iliad ;

BAQTOTAVIS = BAQF+DEFNIBAQ;
regle 841240:
application : iliad ;


SOMDEFBANI = max(0,BAFORESTV+BAFPVV+BACREV+BAHREV*MAJREV20+BAFORESTC+BAFPVC+BACREC+BAHREC*MAJREV20+BAFORESTP+BAFPVP+BACREP+BAHREP*MAJREV20
                 + 4BACREV + 4BAHREV * MAJREV20 + 4BACREC + 4BAHREC * MAJREV20 + 4BACREP + 4BAHREP * MAJREV20 
		 + COD5AK+arr(COD5AL*MAJREV20)+ COD5BK+arr(COD5BL*MAJREV20)+ COD5CK+arr(COD5CL*MAJREV20)
		  - BAHQPROV) ;

regle 841250:
application : iliad ;

BAHQPROV = BANV + BAMV + BANC + BAMC + BANP + BAMP 
        + BAFORESTV+BAFPVV+BAFORESTC+BAFPVC+BAFORESTP+BAFPVP
        +(max(0,(4BACREV - 4DEFBACREV))*positif_ou_nul(BARAV)+arr(max(0,(4BAHREV - 4DEFBAHREV))*MAJREV20) * positif_ou_nul(BARSV))
        +(max(0,(4BACREC - 4DEFBACREC))*positif_ou_nul(BARAC)+arr(max(0,(4BAHREC - 4DEFBAHREC))*MAJREV20) * positif_ou_nul(BARSC))
        +(max(0,(4BACREP - 4DEFBACREP))*positif_ou_nul(BARAP)+arr(max(0,(4BAHREP - 4DEFBAHREP))*MAJREV20) * positif_ou_nul(BARSP)) 
	;

regle 841260:
application : iliad ;

DEFBANI = max(0,BAFORESTV+BAFPVV+BACREV+arr(BAHREV*MAJREV20)+BAFORESTC+BAFPVC+BACREC+arr(BAHREC*MAJREV20)+BAFORESTP+BAFPVP+BACREP+arr(BAHREP*MAJREV20)
                 +4BACREV + arr(4BAHREV * MAJREV20) + 4BACREC + arr(4BAHREC * MAJREV20) + 4BACREP + arr(4BAHREP * MAJREV20) 
		 + COD5AK+arr(COD5AL*MAJREV20)+ COD5BK+arr(COD5BL*MAJREV20)+ COD5CK+arr(COD5CL*MAJREV20)
                 + min(0,BAHQV+BAHQC+BAHQP+4BAQV+4BAQC+4BAQP) * (1-positif(SHBA-SEUIL_IMPDEFBA))) ;

DEFBANIH470 = max(0,BAFORESTV+BAFPVV+BACREV+arr(BAHREV*MAJREV20)+BAFORESTC+BAFPVC+BACREC+arr(BAHREC*MAJREV20)+BAFORESTP+BAFPVP+BACREP+arr(BAHREP*MAJREV20)
                 +4BACREV + arr(4BAHREV * MAJREV20) + 4BACREC + arr(4BAHREC * MAJREV20) + 4BACREP + arr(4BAHREP * MAJREV20))
		 + COD5AK+arr(COD5AL*MAJREV20)+ COD5BK+arr(COD5BL*MAJREV20)+ COD5CK+arr(COD5CL*MAJREV20)
		 ;

DEFBANI470 =  max(0,-BAHQV-BAHQC-BAHQP-4BAQV-4BAQC-4BAQP) * (1-positif(SHBA-SEUIL_IMPDEFBA)) ;
DEFBANI470BIS =  max(0,-BAHQV-BAHQC-BAHQP-4BAQV-4BAQC-4BAQP) * positif(SHBA-SEUIL_IMPDEFBA)* (1-APPLI_BATCH);
regle 841800:
application : iliad ;
BACIFBIS =  BAHQNODEFF +BAQNODEFF;
BACIFBISQ =  abs(min(0 , BAQNODEFF -max(0,BAHQT-DAGRI1-DAGRI2-DAGRI3-DAGRI4-DAGRI5-DAGRI6))) ;
regle 841265:
application : iliad ;


DEFBANIFBIS = ((1-PREM8_11) * (positif(BACIFBIS1731) * positif(BAHQNODEFF +BAQNODEFF)
                                          * positif(BAHQNODEFF +BAQNODEFF -(BACIFBIS1731))
                                                 * max(0,DAGRIIMP - max(0,SOMBADF1731 - SOMBADF) - BACIFBIS1731)
                           + (1-positif(BACIFBIS1731)) * positif(BAHQNODEFF +BAQNODEFF)
                                          * max(0,DAGRIIMP - max(0,SOMBADF1731 - SOMBADF)+ (-1) * (BACIFBIS1731)-DEFIBA1731)
                      )
              + PREM8_11 * max(0, DAGRIIMP)
	       )
                                    * null(V_IND_TRAIT - 5);
regle 841270:
application : iliad ;


DEFBANIF = (max(0,DEFBANIFBIS )* (1-positif(positif(BAHQNODEFF1731+BAQNODEFF1731)+PREM8_11))
                + DEFBANIFBIS * positif(positif(BAHQNODEFF1731+BAQNODEFF1731)+PREM8_11))   * positif(ART1731BIS);
regle 841280:
application : iliad ;


PRORATABA = (4BACREV + 4BACREC +4BACREP +arr(4BAHREV*MAJREV20) +arr(4BAHREC*MAJREV20) +arr(4BAHREP*MAJREV20)-max(0,4BAQV+4BAQC+4BAQP+min(0,BAHQNODEFF)))/SOMDEFBANI+0;
regle 841290:
application : iliad ;


DEFNIBAQ = (DEFNIBAQ1+max(0,arr((DEFBANIF - DBAIP) * PRORATABA))) * positif(DEFBANIF-DBAIP)
         + DEFNIBAQ1 * (1-positif(DEFBANIF-DBAIP));
regle 8509355:
application : iliad  ;



DEFNIBAQ1=positif(DBAIP-max(0,BAHQAVISV+BAHQAVISC+BAHQAVISP))*max(0,min(DEFBANIF,min(4BAQV+4BAQC+4BAQP,DBAIP-max(0,BAHQV+BAHQC+BAHQP))));


regle 8509365:
application : iliad  ;
DAGRIIMP = min(max(0,BAHQNODEFF + BAQNODEFF),DEFANTBAF);


regle 8509375:
application : iliad  ;
DBAIP = DAGRIIMP;

regle 841300:
application : iliad ;


BATMARGV = COD5XT + arr(COD5XV * MAJREV20) ;
BATMARGC = COD5XU + arr(COD5XW * MAJREV20) ;
BATMARGTOT =  BATMARGV + BATMARGC ;

regle 841310:
application : iliad ;


IBATMARG = arr(BATMARGTOT * TXMARJBA/100) ;

regle 841315:
application : iliad ;


4BAQV = BAEV + BAEMV;
4BAQC = BAEC + BAEMC;
4BAQP = BAEP + BAEMP;

PASBACV = ((BACREV - DEFBACREV) * positif_ou_nul(BARAV) +  BARAV * (1-positif_ou_nul(BARAV))) - COD5AQ + COD5AY;
PASBACC = ((BACREC - DEFBACREC) * positif_ou_nul(BARAC) +  BARAC * (1-positif_ou_nul(BARAC))) - COD5BQ + COD5BY;
PASBACP = ((BACREP - DEFBACREP) * positif_ou_nul(BARAP) +  BARAP * (1-positif_ou_nul(BARAP))) - COD5CU + COD5CV;

R25HIV = (positif(BARSV) * (BAHREV - DEFBAHREV) + (1-positif(BARSV)) * (BARSV)) - COD5AR + COD5AZ;
R25HIC = (positif(BARSC) * (BAHREC - DEFBAHREC) + (1-positif(BARSC)) * (BARSC)) - COD5BR + COD5BZ;
R25HIP = (positif(BARSP) * (BAHREP - DEFBAHREP) + (1-positif(BARSP)) * (BARSP)) - COD5CY + COD5CZ;

PASBAHV = positif(R25HIV) * arr(R25HIV * MAJREV20) + (1-positif(R25HIV)) * R25HIV;
PASBAHC = positif(R25HIC) * arr(R25HIC * MAJREV20) + (1-positif(R25HIC)) * R25HIC;
PASBAHP = positif(R25HIP) * arr(R25HIP * MAJREV20) + (1-positif(R25HIP)) * R25HIP;



BAETALV = COD5EA + COD5EC ;
BAETALC = COD5EI + COD5EQ ;
BAETALP = COD5EU + COD5EV ;

RN6DEC1 = COD5EA + (COD5EC * MAJREV20) ; 
RN6DEC2 = COD5EI + (COD5EQ * MAJREV20) ; 
RN6DEC3 = COD5EU + (COD5EV * MAJREV20) ;

RN6FOY = RN6DEC1 + RN6DEC2 + RN6DEC3 ;

regle 841330:
application : iliad;

AVRICIIR = COD8EA + COD8EB ; 

RASSALIR = COD8HV + COD8IV + COD8JV + COD8KV + COD8LV + COD8MV ; 

RASACOIR = COD8HW + COD8IW + COD8JW + COD8KW + COD8LW + COD8MW ; 

RASCTXIR = COD8LG + COD8LH + COD8LI + COD8HY + COD8IY + COD8JY + COD8KY + COD8LY + COD8MY ; 

regle 841335:
application : iliad;


IMPDRFRAC = max(0 , IRNET + TAXANET + PCAPNET + HAUTREVNET - IRESTITIR + AVRICIIR + CIADCREB3 - RASSALIR - RASACOIR + RASCTXIR + RASPENAC ) ; 
regle 841340:
application : iliad;


RON1 = BANV + BANC + BANP ;  
RON2 = BAMV + BAMC + BAMP ;
REN1 = BAEV + BAEC + BAEP ;
REN2 = BAEMV + BAEMC + BAEMP ;


RONDEC1 = BANV + BAMV ;
RONDEC2 = BANC + BAMC ;
RONDECPAC = BANP + BAMP ;
RENDEC1 = BAEV + BAEMV ; 
RENDEC2 = BAEC + BAEMC ; 
RENDEC3 = BAEP + BAEMP ; 


DEFANTBADECLA = DEFANTBAV + DEFANTBAC + DEFANTBAP ;

RONTOTDEC1 = max(0,RONDEC1 + RENDEC1 - DEFANTBAV);
RONTOTDEC2 = max(0,RONDEC2 + RENDEC2 - DEFANTBAC);
RONTOTDEC3 = max(0,RONDECPAC + RENDEC3 - DEFANTBAP);




REVFRACDEC1 = positif_ou_nul(RONTOTDEC1-RN6DEC1) * RN6DEC1
            +  positif(RN6DEC1-RONTOTDEC1) * max(0,(BANV + BAMV + BAEV + BAEMV - DEFANTBAV)) ;

REVFRACDEC2 = positif_ou_nul(RONTOTDEC2-RN6DEC2) * RN6DEC2
            +  positif(RN6DEC2-RONTOTDEC2) * max(0,(BANC + BAMC + BAEC + BAEMC - DEFANTBAC)) ;       

REVFRACPAC = positif_ou_nul(RONTOTDEC3-RN6DEC3) * RN6DEC3
           +  positif(RN6DEC3 - RONTOTDEC3) * max(0,(BANP + BAMP  + BAEP + BAEMP - DEFANTBAP)) ;

REVFRACFOY = REVFRACDEC1 + REVFRACDEC2 + REVFRACPAC ;	   
regle 841350:
application : iliad;



PVINDUSBA = COD5HA + COD5IA + COD5JA ;

regle 841360:
application : iliad;

REVNET = RNI + TTPVQ + BATMARGTOT + RCMIMPTR + BPTP3 + BPTP10 + BPTP18 + BPT19 +  BPTP24 + BPTP4 + BPTP40 + BPTPPI + BPTPD + BPTPG + BPCAPTAXV + BPCAPTAXC + BPCAPTAXP ; 

regle 841370:
application : iliad;

IMPETAL = arr(IMPDRFRAC * REVFRACFOY/REVNET) ; 

IMPETAL5 = arr(IMPETAL/NOMBRE5) ;


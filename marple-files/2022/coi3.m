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
verif 302:
application : iliad ;

si
V_IND_TRAIT > 0
 et
(
 null(TSHALLOV - (COD1AF + COD1AG)) = 1
 ou
 null(TSHALLOC - (COD1BF + COD1BG)) = 1
 ou
 null(TSHALLO1 - (COD1CF + COD1CG)) = 1
 ou
 null(TSHALLO2 - (COD1DF + COD1DG)) = 1
 ou
 null(TSHALLO3 - (COD1EF + COD1EG)) = 1
 ou
 null(TSHALLO4 - (COD1FF + COD1FG)) = 1
) 

alors erreur IM02 ;
verif 309:
application : iliad ;


si
   (APPLI_OCEANS + 0) < 1
   et
   positif(V_BTCO2044P + 0) = 1
   et
   present(CO2044P) = 0
   et
   V_IND_TRAIT = 4

alors erreur IM09 ;
verif 3111:
application : iliad ;


si
   (APPLI_ILIAD=1 et positif(APPLI_COLBERT + 0) != 1)
   et
   V_0CF+0 != somme (i = 0..7: positif(V_0Fi+0))

alors erreur IM1101 ;
verif 3113:
application : iliad ;


si
   (APPLI_ILIAD=1 et positif(APPLI_COLBERT + 0) != 1)
   et
   V_0CH != somme (i = 0..5: positif(V_0Hi+0))

alors erreur IM1102 ;
verif 3116:
application : iliad ;


si
   (APPLI_ILIAD=1 et positif(APPLI_COLBERT + 0) != 1)

   et
   V_0DJ != somme (i = 0..5: positif(V_0Ji+0))

alors erreur IM1103 ;
verif 313:
application : iliad ;

si
  IREST > LIM_INFRESTIT
  et
  IREST <= LIM_RESTIT3
  et 
  (IPRECH + COD8TL + CREFAM + CREAGRIBIO + PRESINTER + CREFORMCHENT + COD8WG + COD8WH +  CREARTS + CRECONGAGRI + COD8WK + AUTOVERSLIB) >= IREST
  et
  positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)) = 0

alors erreur IM13 ; 
verif 315:
application : iliad ;

si
  IINET >= LIM_RESTNET
   et
  positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)) = 0

alors erreur IM15 ;
verif 3161:
application : iliad ;



si
   (APPLI_OCEANS + 0) < 1
   et
   V_ZDC+0 = 0
   et
   V_BTMUL+0 = 0
   et
   V_0AX+0 = 0 et V_0AY+0 = 0 et V_0AZ+0 = 0 et V_0AO+0 = 0
   et
   V_BTRNI > LIM_BTRNI
   et
   RNI > V_BTRNI * 9
   et
   RNI < 100000
   et
   V_IND_TRAIT = 4

alors erreur IM1601 ;
verif 3162:
application : iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_ZDC+0 = 0
   et
   V_BTMUL+0 = 0
   et
   V_0AX+0 = 0 et V_0AY+0 = 0 et V_0AZ+0 = 0 et V_0AO+0 = 0
   et
   V_BTRNI > LIM_BTRNI
   et
   RNI > V_BTRNI * 5
   et
   RNI >= 100000
   et
   V_IND_TRAIT = 4

alors erreur IM1602 ;
verif 319:
application : iliad ;


si
   (APPLI_OCEANS + 0) < 1
   et
   (V_IND_TRAIT = 4
    et V_BT0CF + 0 = somme(i=0..5:positif(V_BT0Fi+0))
    et V_BT0CH + 0 = somme(i=0..5:positif(V_BT0Hi+0))
    et V_0CF + 0 = somme(i=0..5:positif(V_0Fi+0))
    et V_0CH + 0 = somme(i=0..5:positif(V_0Hi+0))
    et
     (
       V_BT0CH + V_BT0CF + 0 > V_0CH + V_0CF
       ou
       (V_BT0CF = 1 et V_0CF =1 et V_0CH + 0 = 0 et pour un i dans 0,1: V_0Fi = ANNEEREV )
       ou
       (V_BT0CF = 1 et V_0CH =1 et V_0CF + 0 = 0 et pour un i dans 0,1: V_0Hi = ANNEEREV )
       ou
       (V_BT0CH = 1 et V_0CH =1 et V_0CF + 0 = 0 et pour un i dans 0,1: V_0Hi = ANNEEREV )
       ou
       (V_BT0CH = 1 et V_0CF =1 et V_0CH + 0 = 0 et pour un i dans 0,1: V_0Fi = ANNEEREV )
     )
   )

alors erreur IM19 ;
verif 32001:
application : iliad ;

si
V_IND_TRAIT > 0
et
(
(FRNV > (TSHALLOV + COD1PM + COD1TP + COD1NX + COD1AF + COD1AG + ALLOV + BPCOSAV + GLDGRATV + COD1GB + COD1AA + COD1GG + COD1GF))
ou
(FRNC > (TSHALLOC + COD1QM + COD1UP + COD1OX + COD1BF + COD1BG + ALLOC + BPCOSAC + GLDGRATC + COD1HB + COD1BA + COD1HF + COD1HG )) 
ou
(FRN1 > (TSHALLO1 + COD1CF + COD1CG + ALLO1 + COD1IB + COD1CA + COD1IF +COD1IG))
ou
(FRN2 > (TSHALLO2 + COD1DF + COD1DG + ALLO2 + COD1JB + COD1DA + COD1JF +COD1JG))
ou
(FRN3 > (TSHALLO3 + COD1EF + COD1EG + ALLO3 + COD1EA + COD1KF + COD1KG))
ou
(FRN4 >  (TSHALLO4 +COD1FF +COD1FG + ALLO4 + COD1FA + COD1LF + COD1LG))
 )

alors erreur IM2001 ;
verif 32002:
application : iliad ;

si
V_IND_TRAIT > 0
et
(
(FRNV > 2 * (TSHALLOV + COD1PM + COD1TP + COD1NX + COD1AF + COD1AG + ALLOV + BPCOSAV + GLDGRATV + COD1GB + COD1AA + COD1GG + COD1GF))
ou
(FRNC > 2 * (TSHALLOC + COD1QM + COD1UP + COD1OX + COD1BF + COD1BG + ALLOC + BPCOSAC + GLDGRATC + COD1HB + COD1BA + COD1HF + COD1HG ))
ou
(FRN1 > 2 * (TSHALLO1 + COD1CF + COD1CG + ALLO1 + COD1IB + COD1CA + COD1IF +COD1IG))
ou
(FRN2 > 2 * (TSHALLO2 + COD1DF + COD1DG + ALLO2 + COD1JB + COD1DA + COD1JF +COD1JG))
ou
(FRN3 > 2 * (TSHALLO3 + COD1EF + COD1EG + ALLO3 + COD1EA + COD1KF + COD1KG))
ou
(FRN4 > 2 * (TSHALLO4 +COD1FF +COD1FG + ALLO4 + COD1FA + COD1LF + COD1LG))
 )

alors erreur IM2002 ;
verif 32101:
application : iliad ;

si 

 V_IND_TRAIT > 0
 et
 SOMMEM210 >= (NOMBRE80/100) * REVKIRE
 et 
 REVKIRE >= PLAF20000

alors erreur IM2101 ;
verif 32102:
application : iliad ;

si

 V_IND_TRAIT > 0
 et
 SOMMEM210 > (NOMBRE80/100) * REVKIRE
 et
  REVKIRE <= PLAF20000
 et
(
 positif(NOMBRE65 - (ANNEEREV - V_0DA)) = 1
 ou
 positif(NOMBRE65 - (ANNEEREV - V_0DB)) = 1
)

alors erreur IM2102 ;
verif 340:
application : iliad ;


si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 5
   et
   positif(ANNUL2042) = 1

alors erreur IM40 ;
verif corrective 342:
application : iliad ;


si
                    (FLAGDERNIE+0 = 1) et  ((DEFRI = 1)  et (PREM8_11+TOTSTR >=1))

alors erreur IM42 ;
verif 3421:
application : iliad ;


si
                    (FLAGDERNIE+0 = 1) et  ((DEFRI = 1)  et (PREM8_11=0) et (VARR10+0=0) et ( TOTSTR+0 = 1))

alors erreur IM42 ;
verif 343:
application : iliad ;


si
                      ((DEFRI = 0)  et (DEFRIMAJ = 1))

alors erreur IM43 ;
verif 3441:
application : iliad ;

si
  V_IND_TRAIT = 5
  et
  INDCODIFI = 0
  et
  positif(V_FLAG8OT + 0) = 0 
  et
  positif(COD8OT + 0) = 0
  et
  positif(IITAZIR) = 0
  et
  positif(IDRS4) = 1
  et
  positif((RFRPARQF * NBPT) - REVKIRE) = 1
  et
  positif(ANNUL2042 + 0) = 0

alors erreur IM4401 ;
verif 3442:
application : iliad ;

si
  V_IND_TRAIT = 5
  et
  INDCODIFI = 0
  et
  positif(V_FLAG8OT + 0) = 1
  et
  positif(COD8OT + 0) = 1
  et
  positif(IITAZIR) = 0
  et
  positif(IDRS4) = 1
  et
  positif((RFRPARQF * NBPT) - REVKIRE) = 1
  et
  positif(ANNUL2042 + 0) = 0

alors erreur IM4402 ;
verif 3450:
application : iliad ;

si
  V_IND_TRAIT = 5
   et
  INDCODCO = 1

alors erreur IM55 ;

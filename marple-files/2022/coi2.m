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
verif 2011:
application : iliad  ;
si
   (APPLI_OCEANS + 0) < 1
   et
   IND_10V = 0
   et
   RDSYVO > 0

alors erreur I00101 ;
verif 2012:
application : iliad  ;
si
   (APPLI_OCEANS + 0) < 1
   et
   IND_10C = 0
   et
   RDSYCJ > 0

alors erreur I00102 ;
verif 2013:
application : iliad ;
si
(
  (APPLI_OCEANS + 0) < 1
   et
  (
    IND_101 = 0
   ou
    IND_102 = 0
   ou
    IND_103 = 0
   ou
    IND_104 = 0
  ) et RDSYPP > 0
)
alors erreur I00103 ;
verif 208:
application :  iliad ;

si
   positif(APPLI_COLBERT + APPLI_BATCH + APPLI_ILIAD) = 1
   et
   CHRFAC > 0
   et
   CHNFAC + 0 = 0
   et
   positif(NATIMP) = 1
   et
   V_CNR = 0

alors erreur I008 ;
verif 209:
application :  iliad ;

si
   positif(APPLI_COLBERT + APPLI_BATCH + APPLI_ILIAD) = 1
   et
   RDCOM > 0
   et
   NBACT + 0  = 0
   et
   positif(NATIMP) = 1

alors erreur I009 ;
verif 211:
application : iliad  ;


si
   (APPLI_OCEANS + 0 ) < 1
   et
   (V_0AM + V_0AO + 0 = 1) et V_0AS = 1 et V_0AP+0 = 0 et V_0AF+0 = 0
   et
   ANNEEREV - V_0DA < 74
   et
   ANNEEREV - V_0DB < 74

alors erreur I011 ;
verif 212:
application :  iliad ;


si
   (APPLI_OCEANS + 0) < 1
   et
   (V_0AM + V_0AO + 0 = 0 )
   et
   V_0AZ + 0 = 0
   et
   V_0AP + 0 = 0
   et
   V_0AW = 1
   et
   ANNEEREV - V_0DA < 74
  
alors erreur I012 ;
verif 214:
application :  iliad ;


si 
   (APPLI_OCEANS + APPLI_COLBERT + 0) < 1 
   et
    (
       V_BT0CF >0
          et V_0CH >0
              et positif(V_0CF+0) != 1
                   et V_BT0CF + 0 = somme(i=0..5:positif(V_BT0Fi+0))
                     et V_BT0CH + 0 = somme(i=0..5:positif(V_BT0Hi+0))
                       et V_0CF + 0 = somme(i=0..5:positif(V_0Fi+0))
                         et V_0CH + 0 = somme(i=0..5:positif(V_0Hi+0))
                           et ((     V_0CH < V_BT0CF   )
                                ou
                               (     V_0CH = V_BT0CF
                                  et somme(i=0..5:V_0Hi+0) != somme(i=0..5:V_BT0Fi+0)         )
                                ou
                               (     V_0CH = V_BT0CF
                                  et somme(i=0..5:V_0Hi+0) = somme(i=0..5:V_BT0Fi+0)
                                  et somme(i=0..5: (1/V_0Hi)) != somme(i=0..5: (1/V_BT0Fi))   )
                               ou
                               (     V_0CH > V_BT0CF
        et somme(i=0..5:positif(somme(j=0..5:null(V_0Hj - V_BT0Fi)))*V_BT0Fi) != somme(i=0..5:V_BT0Fi)
                               )
                               ou
                               (     V_0CH > V_BT0CF
        et somme(i=0..5:positif(somme(j=0..5:null(V_0Hj - V_BT0Fi)))*V_BT0Fi) = somme(i=0..5:V_BT0Fi)
        et somme(i=0..5:positif(somme(j=0..5:null(V_0Hi - V_BT0Fj)))*V_0Hi) < somme(i=0..5:V_BT0Fi)
                               )
                              )
    )
   et
   V_IND_TRAIT = 4

alors erreur I014 ;
verif 215:
application :  iliad ;


si
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   (
    DEFRCM + 0 > V_BTDFRCM1 + PLAF_PRECONS * (1 - positif(V_BTDFRCM1))
    ou
    DEFRCM2 + 0 > V_BTDFRCM2 + PLAF_PRECONS * (1 - positif(V_BTDFRCM2))
    ou
    DEFRCM3 + 0 > V_BTDFRCM3 + PLAF_PRECONS * (1 - positif(V_BTDFRCM3))
    ou
    DEFRCM4 + 0 > V_BTDFRCM4 + PLAF_PRECONS * (1 - positif(V_BTDFRCM4))
    ou
    DEFRCM5 + 0 > V_BTDFRCM5 + PLAF_PRECONS * (1 - positif(V_BTDFRCM5))
    ou
    DEFRCM6 + 0 > V_BTDFRCM6 + PLAF_PRECONS * (1 - positif(V_BTDFRCM6)))

alors erreur I015 ;
verif 216:
application :  iliad ;

si
   V_IND_TRAIT > 0
   et
   V_CNR + 0 != 1
   et
   positif(PVSURSI + PVIMPOS + CODRWA + CODRWB + COD3TA + COD3TB + COD3XM + COD3XA + COD3XD + COD3YA + 0) = 1

alors erreur I016 ;
verif 217:
application : iliad ;

si
   V_IND_TRAIT = 5
   et
   null(5 - LIGI017) = 1

alors erreur I017;

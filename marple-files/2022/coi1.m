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
verif 2:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   1 - V_CNR > 0
   et
   positif(COD7CH + COD7CI) = 1
   et
   positif(COD7EN + CINE1 + CINE2) = 1

alors erreur DD02 ;
verif 5:
application :  iliad ;

si
   (APPLI_COLBERT + APPLI_OCEANS + 0) < 1
   et
   V_ZDC + 0 = 0
   et
   V_BTMUL = 0
   et
   V_0AX+0 = 0 et V_0AY+0 = 0 et V_0AZ+0= 0
   et
   V_BTRNI > LIM_BTRNI10
   et
   RNI < V_BTRNI/5
   et
   V_BTANC + 0 = 1
   et
   ((V_BTNI1 + 0) non dans (50,92))
   et
   V_IND_TRAIT = 4

alors erreur DD05 ;
verif 11: 
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   RFMIC > 0
   et
   RFDANT> 0

alors erreur DD11 ;
verif 15:
application : iliad  ;

si (APPLI_OCEANS + 0) < 1 
   et (
          (
                ( RDPRESREPORT +0  > V_BTPRESCOMP  +  LIM_REPORT )
           ou
                ( PRESCOMP2000 + PRESCOMPJUGE  +0 > LIM_REPORT  et
                   V_BTPRESCOMP  + 0> 0 )
           ou
                ( RDPRESREPORT +0  > LIM_REPORT et V_BTPRESCOMP+0 = 0 )
          )
          et
          (
              1 - V_CNR > 0
          )
          et
          (
              RPRESCOMP > 0
          )
         et
          (((APPLI_ILIAD = 1 et positif(APPLI_COLBERT + 0) != 1) et V_NOTRAIT+0 < 16)
             ou APPLI_COLBERT = 1
             ou ((V_BTNI1+0) non dans (50,92) et APPLI_BATCH = 1))
                       )
alors erreur DD15 ;
verif 16:
application :  iliad ;

si
   APPLI_BATCH + APPLI_ILIAD  + APPLI_OCEANS = 1
   et
   APPLI_COLBERT = 0
   et
   1 - V_CNR > 0
   et
   CHRFAC > 0
   et
   V_0CR > 0
   et
   RFACC != 0

alors erreur DD16 ;
verif 18:
application :  iliad ;


si
   (APPLI_COLBERT + APPLI_OCEANS + 0) < 1
   et
   DAR > LIM_CONTROLE
   et
   V_BTRNI > 0
   et
   ((V_BTNI1+0) non dans (50,92))
   et
   V_IND_TRAIT = 4

alors erreur DD18 ;
verif 20:
application :  iliad ;


si
  (APPLI_COLBERT + APPLI_OCEANS + 0) < 1
   et
   V_BTANC = 1
   et
   DAGRI1 + DAGRI2 + DAGRI3 + DAGRI4 + DAGRI5 + DAGRI6 > LIM_CONTROLE + V_BTDBA
   et
   V_IND_TRAIT = 4

alors erreur DD20 ;
verif 21:
application :  iliad ;

si
  (APPLI_OCEANS + 0) < 1
   et
   1 - V_CNR > 0
   et
   (CREAIDE + 0) > (LIM_AIDOMI3 * (1 - positif(PREMAIDE)) + LIM_PREMAIDE2 * positif(PREMAIDE))
   et
   INAIDE = 1
   et
   (positif(V_0AP+0)=0
    et positif(V_0AF+0)=0
    et positif(V_0CG+0)=0
    et positif(V_0CI+0)=0
    et positif(V_0CR+0)=0
   )

alors erreur DD21 ;
verif 22:
application :  iliad ;


si
  (APPLI_COLBERT + APPLI_OCEANS + 0) < 1
   et
   (V_BTCSGDED * (1-present(DCSG)) + DCSG) > V_BTCSGDED +  LIM_CONTROLE
   et
   1 - V_CNR > 0
   et
   RDCSG > 0
   et
   (((APPLI_ILIAD =  1 et positif(APPLI_COLBERT + 0) != 1) et V_NOTRAIT+0 < 16)
    ou
    ((V_BTNI1+0) non dans (50,92) et APPLI_BATCH = 1))

alors erreur DD22 ;
verif 26:
application :  iliad ;

si
   positif(APPLI_COLBERT + APPLI_BATCH + APPLI_ILIAD) = 1
   et
   RFORDI + FONCI + REAMOR + RFDORD + RFDHIS + RFDANT > LIM_BTREVFONC
   et
   V_BTANC = 1
   et
   V_BTIRF = 0
   et
   V_IND_TRAIT = 4

alors erreur DD26 ;
verif 27:
application :  iliad ;


si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   (1 - V_CNR) > 0
   et
   (
    COD7CR > LIM_CONTROLE + V_BTITENT4
    ou
    COD7CV > LIM_CONTROLE + V_BTITENT3
    ou
    COD7CX > LIM_CONTROLE + V_BTITENT2A 
    ou
    COD7CS > LIM_CONTROLE + V_BTITENT2B
    ou
    COD7CT > LIM_CONTROLE + V_BTITENT1A
    ou
    COD7DC > LIM_CONTROLE + V_BTITENT1B
    )
   et
   positif(NATIMP + 0) = 1

alors erreur DD27 ;
verif 29:
application :  iliad ;


si
  (APPLI_OCEANS + 0) < 1
   et
   V_CNR + 0 = 0
   et
   positif(NATIMP) = 1
   et
   ((REPDON03 > LIM_CONTROLE + V_BTDONS5)
    ou
    (REPDON04 > LIM_CONTROLE + V_BTDONS4)
    ou
    (REPDON05 > LIM_CONTROLE + V_BTDONS3)
    ou
    (REPDON06 > LIM_CONTROLE + V_BTDONS2)
    ou
    (REPDON07 > LIM_CONTROLE + V_BTDONS1))
   et
   V_IND_TRAIT = 4

alors erreur DD29 ;
verif 34:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et 
   V_CNR + 0 != 1
   et
   positif(FIPCORSE+0) = 1
   et
   positif(FFIP + FCPI + COD7FT + COD7GR) = 1
                         
alors erreur DD34 ;
verif 39:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   REPGROREP12 + REPGROREP13 + REPGROREP14 + COD6HP + COD6HQ + COD6HR > LIM_CONTROLE + V_BTNUREPAR

alors erreur DD39 ;
verif 40:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   CELRREDLQ + COD7LA  + COD7MS + COD7MO + COD7MA + COD7MI  > LIM_CONTROLE + V_BTRRCEL4

alors erreur DD40 ;
verif 41:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   COD7PP + COD7PU + COD7HO + COD7HT  + COD7HD + COD7KE > LIM_CONTROLE + V_BTRILMNP5

alors erreur DD41 ;
verif 48:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   CELRREDLR + COD7LB + COD7MT + COD7MP + COD7MB + COD7MJ > LIM_CONTROLE + V_BTRRCEL3

alors erreur DD48 ;
verif 49:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   COD7PQ + COD7PV + COD7HP + COD7HU + COD7HE + COD7KF > LIM_CONTROLE + V_BTRILMNP4

alors erreur DD49 ;
verif 52:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
    LNPRODEF10 + LNPRODEF9 + LNPRODEF8 + LNPRODEF7 + LNPRODEF6 + LNPRODEF5
    + LNPRODEF4 + LNPRODEF3 + LNPRODEF2 + LNPRODEF1 > LIM_CONTROLE + V_BTDEFNPLOC

alors erreur DD52 ;
verif 53:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   DEFBIC6 + DEFBIC5 + DEFBIC4 + DEFBIC3 + DEFBIC2 + DEFBIC1 > LIM_CONTROLE + V_BTBICDF

alors erreur DD53 ;
verif 57:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   CELRREDLU + COD7LC + COD7MU + COD7MQ + COD7MC + COD7MK > LIM_CONTROLE + V_BTRRCEL2

alors erreur DD57 ;
verif 58:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   COD7PR + COD7PW + COD7HQ + COD7HV + COD7HF + COD7KG > LIM_CONTROLE + V_BTRILMNP3

alors erreur DD58 ;
verif 64:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   CELRREDLV + COD7LY + COD7MV + COD7MR + COD7MD + COD7ML > LIM_CONTROLE + V_BTRRCEL1

alors erreur DD64 ;
verif 65:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   COD7PS + COD7PX + COD7HR + COD7HW + COD7HG + COD7KH  > LIM_CONTROLE + V_BTRILMNP2

alors erreur DD65 ;
verif 66:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   COD7PT + COD7PY + COD7HS + COD7HX + COD7HH + COD7KI > LIM_CONTROLE + V_BTRILMNP1

alors erreur DD66 ;
verif 67:
application :  iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT = 4
   et
   V_CNR + 0 != 1
   et
   COD7CY + COD7DY + COD7EY + COD7FY + COD7GY > LIM_CONTROLE + V_BTPLAFPME1

alors erreur DD67 ;

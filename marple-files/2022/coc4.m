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
verif 1700:
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   RDCOM > 0
   et
   SOMMEA700 = 0

alors erreur A700 ;
verif 1702:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
    ((V_REGCO +0) dans (1,3,5,6)
    ou
    (VALREGCO+0)  non dans (2))
   et
   INTDIFAGRI * positif(INTDIFAGRI) + 0 > RCMHAB * positif(RCMHAB) + 0

alors erreur A702 ;
verif 1703:
application :  iliad ;

si
 (V_MODUL+0) < 1
   et
 (
  ( (positif(PRETUD+0) = 1 ou positif(PRETUDANT+0) = 1)
   et
    V_0DA < 1985
   et
    positif(BOOL_0AM+0) = 0 )
  ou
  ( (positif(PRETUD+0) = 1 ou positif(PRETUDANT+0) = 1)
   et
   positif(BOOL_0AM+0) = 1
   et
   V_0DA < 1985
   et
   V_0DB < 1985 )
  )
alors erreur A703 ;
verif 1704:
application :  iliad ;


si
   (V_MODUL+0) < 1
     et
   (positif(CASEPRETUD + 0) = 1 et positif(PRETUDANT + 0) = 0)
   ou
   (positif(CASEPRETUD + 0) = 0 et positif(PRETUDANT + 0) = 1)

alors erreur A704 ;
verif 17071:
application :  iliad ;


si
   (V_MODUL+0) < 1
     et
   RDENS + RDENL + RDENU > V_0CF + V_0DJ + V_0DN + 0

alors erreur A70701 ;
verif 17072:
application :  iliad ;


si
   (V_MODUL+0) < 1
     et
   RDENSQAR + RDENLQAR + RDENUQAR > V_0CH + V_0DP + 0

alors erreur A70702 ;
verif 17111:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
   et
   INAIDE > 0
   et
   positif( CREAIDE + 0) = 0

alors erreur A71101 ;
verif 17112:
application :  iliad ;
si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
   et
   ASCAPA >0
   et 
   positif (CREAIDE + 0) = 0

alors erreur A71102 ;
verif 17113:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
  et
   PREMAIDE > 0
   et
   positif(CREAIDE + 0) = 0

alors erreur A71103 ;
verif 17114:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
     et
   COD7DR > 0
     et
   positif(CREAIDE + 0) = 0

alors erreur A71104 ;
verif 17115:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
     et
   COD7HB > 0
     et
   positif(CREAIDE + 0) = 0

alors erreur A71105 ;
verif 17121:
application :  iliad ;


si
   (V_MODUL+0) < 1
     et
   PRESCOMP2000 + 0 > PRESCOMPJUGE
   et
   positif(PRESCOMPJUGE) = 1

alors erreur A712 ;
verif non_auto_cc 1713:
application :  iliad ;


si
   (V_MODUL+0) < 1
     et
   (PRESCOMPJUGE + 0 > 0 et PRESCOMP2000 + 0 = 0)
   ou
   (PRESCOMPJUGE + 0 = 0 et PRESCOMP2000 + 0 > 0)

alors erreur A713 ;
verif 1714:
application :  iliad ;


si
   (V_MODUL+0) < 1
     et
   RDPRESREPORT + 0 > 0
   et
   PRESCOMPJUGE + PRESCOMP2000 + 0 > 0

alors erreur A714 ;
verif 1715:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
   et
   RDPRESREPORT + 0 > LIM_REPCOMPENS

alors erreur A715 ;
verif 1716:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
   et
   ((SUBSTITRENTE < PRESCOMP2000 + 0)
    ou
    (SUBSTITRENTE > 0 et present(PRESCOMP2000) = 0))

alors erreur A716 ;
verif 1719:
application :  iliad ;


si
   (V_MODUL+0) < 1
     et
   RDMECENAT > 0
   et
   SOMMEA719 = 0

alors erreur A719 ;
verif 1731:
application :  iliad ;


si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
   et
   CASEPRETUD + 0 > 5

alors erreur A731 ;
verif 17366:
application :  iliad ;

si
   (V_MODUL+0) < 1
    et
   V_IND_TRAIT > 0
   et
   positif(COD7QW) + positif(COD7QX) + positif(COD7QY) + positif(COD7QQ) + positif(COD7NA) + positif(COD7NB) + positif(COD7NC) + positif(COD7ND) + 0 > 2

alors erreur A73606 ;
verif 17367:
application :  iliad ;

si
   (V_MODUL+0) < 1
    et
   V_IND_TRAIT > 0
    et
   positif(COD7QA) + positif(COD7QB) + positif(COD7QC) + positif(COD7QD) + positif(COD7NE) + positif(COD7NF) + positif(COD7NG) + positif(COD7NH) + 0 > 2

alors erreur A73607 ;
verif 17368:
application :  iliad ;

si
   (V_MODUL+0) < 1
    et
   V_IND_TRAIT > 0
    et
   positif(COD7QI) + positif(COD7QJ) + positif(COD7QK) + positif(COD7QL) + positif(COD7NI) + positif(COD7NJ) + positif(COD7NK) + positif(COD7NL) + 0 > 2

alors erreur A73608 ;
verif 17369:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   positif(COD7QM) + positif(COD7QN) + positif(COD7QO) + positif(COD7QP) + positif(COD7NM) + positif(COD7NN) + positif(COD7PF) + positif(COD7PG) + 0 > 2

alors erreur A73609 ;
verif 17371:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (COD7YE + 0) < 1
   et
   positif(COD7SR) + positif(COD7YZ) + 0 > 1

alors erreur A73701 ;
verif 17372:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (COD7YE + 0) < 1
   et
   positif(COD7SL) + positif(COD7SQ) + positif(COD7YX) + positif(COD7YY) + 0 > 1

alors erreur A73702 ;
verif 17373:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (COD7YE + 0) < 1
   et
   positif(COD7SH) + positif(COD7SI) + positif(COD7SJ) + positif(COD7SK) + positif(COD7XH) + positif(COD7XI) + positif(COD7XJ) + positif(COD7XK)
   + positif(COD7IA) + positif(COD7IB) + positif(COD7IC) + positif(COD7IE) + positif(COD7KJ) + positif(COD7KL) + positif(COD7KN) + 0 > 1

alors erreur A73703 ;
verif 17374:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (COD7YE + 0) < 1
   et
   positif(COD7SD) + positif(COD7SE) + positif(COD7SF) + positif(COD7SG) + positif(COD7WD) + positif(COD7WE) + positif(COD7WF) + positif(COD7WG) 
   + positif(COD7IF) + positif(COD7IG) + positif(COD7IH) + positif(COD7IO) + positif(COD7KO) + positif(COD7KQ) + positif(COD7KR) +  0 > 1

alors erreur A73704 ;
verif 17375:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (COD7YE + 0) < 1
   et   
   positif(COD7IP) + positif(COD7WC) + positif(COD7KS) + 0 > 1

alors erreur A73705 ;
verif 17376:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (COD7YE + 0) < 1
   et   
   positif(COD7LD) + positif(COD7LE) + positif(COD7LF) + positif(COD7LN) + positif(COD7BA) + positif(COD7BB) 
   + positif(COD7BC) + positif(COD7BD) + 0 > 1

alors erreur A73706 ;
verif 17377:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (COD7YE + 0) < 1
   et
   positif(COD7MH) + positif(COD7BJ) + 0 > 1 

alors erreur A73707 ;
verif 17378:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (COD7YE + 0) = 0
   et
   positif(COD7HL) + positif(COD7HM) + 0 > 1  

alors erreur A73708 ;
verif 17379:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (COD7YE + 0) = 0
   et
   positif(COD7LT) + positif(COD7LX) + positif(COD7LZ) + positif(COD7MG) + positif(COD7BE) + positif(COD7BF) + positif(COD7BG) + positif(COD7BH) + 0 > 1

alors erreur A73709 ;
verif 17401:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
   et
   (CODHFR + CODHFW + CODHER + CODHEW + CODHDM + CODHDR + CODHDW + CODHGW + CODHHW + CODHIW  + 0) > PLAF_INVDOM6

alors erreur A74001 ;
verif 1744:
application : iliad  ;

si
 (V_MODUL+0) < 1
   et
 present(COD7WS) + present(COD7WH) + present(COD7WK) + present(COD7WQ) + present(COD7XR) +  present(COD7XZ) +  present(COD7XV) > 0
 et
 present(COD7WS) + present(COD7WH) + present(COD7WK) + present(COD7WQ)+ present(COD7XR) + present(COD7XZ) + present(COD7XV) < 7

alors erreur A744;
verif 1745:
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
   et
   positif_ou_nul(COD7ZW + COD7ZX + COD7ZY + COD7ZZ) = 1
   et
   positif_ou_nul(COD7ZW) + positif_ou_nul(COD7ZX) + positif_ou_nul(COD7ZY) + positif_ou_nul(COD7ZZ) < 4

alors erreur A745 ;
verif 176001:
application : iliad  ;

si
 V_IND_TRAIT > 0
 et
 CODFGD > 0
 et 
 present(RDGARD1) + present(RDGARD2) + present(RDGARD3) + present(RDGARD4) < 4

 alors erreur A76001 ;
verif 176002:
application : iliad  ;

si
 V_IND_TRAIT > 0
 et 
 CODFGD > 0
 et 
 present(CODFGR) = 0

alors erreur A76002 ; 
verif 176003:
application : iliad  ;

si
 V_IND_TRAIT > 0
 et
 CODFHD > 0
 et
 present(RDGARD1QAR) + present(RDGARD2QAR) + present(RDGARD3QAR) + present(RDGARD4QAR) < 4

alors erreur A76003 ;
verif 176004:
application : iliad  ;

si
 V_IND_TRAIT > 0
  et
  CODFHD > 0
  et
  present(CODFHR) = 0


alors erreur A76004 ;
verif 1761: 
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   (APPLI_OCEANS + 0) < 1
   et
  (
  (CIGARD > 0
  et
  1 - V_CNR > 0
  et
  positif(RDGARD1) + positif(RDGARD2) + positif(RDGARD3) + positif(RDGARD4) + positif(CODFGD) > EM7 + 0)
  ou
 (CIGARD > 0
  et
  1 - V_CNR > 0
  et
  positif(RDGARD1QAR) + positif(RDGARD2QAR) + positif(RDGARD3QAR) + positif(RDGARD4QAR) + positif(CODFHD) > EM7QAR + 0)
  )

alors erreur A761 ;

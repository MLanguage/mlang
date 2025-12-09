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
verif 10011:
application :  iliad ;


si
  (V_MODUL+0) < 1
   et
   positif(APPLI_COLBERT + APPLI_ILIAD) = 1
   et
   V_IND_TRAIT > 0
   et
   CHNFAC > 9
 
alors erreur A00101 ;
verif 10012:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   positif(APPLI_COLBERT + APPLI_ILIAD) = 1
   et
   V_IND_TRAIT > 0
   et
   NBACT > 9

alors erreur A00102 ;
verif 10014:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   positif(APPLI_COLBERT + APPLI_ILIAD) = 1
   et
   V_IND_TRAIT > 0
   et
   ASCAPA > 9

alors erreur A00104 ;
verif 10015:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT > 0
   et
   ((RBG1 > 9999999999)
    ou
    (BCSG > 9999999999)
    ou
    (BRDS > 9999999999)
    ou
    (BPRS > 9999999999)
    ou
    (GSALV > 9999999999)
    ou
    (GSALC > 9999999999)
    ou
    (CVNSALAV > 9999999999)
    ou
    (IFIPAT > 9999999999))

alors erreur A00105 ;
verif isf 100171:
application : iliad ; 

si
(V_MODUL+0) < 1
et
(APPLI_OCEANS + 0) < 1
et
V_IND_TRAIT > 0
   et
(COD9AA > 9999999999)
ou
(COD9AB > 9999999999)
ou
(COD9AC > 9999999999)
ou
(COD9AD > 9999999999)
ou
(COD9BA > 9999999999)
ou
(COD9BB > 9999999999)
ou
(COD9CA > 9999999999)
ou
(COD9GF > 9999999999)
ou
(COD9GH > 9999999999)
ou
(COD9NC > 9999999999)
ou
(COD9NG > 9999999999)
ou
(COD9PR > 9999999999)
ou
(COD9PX > 9999999999)
ou
(COD9RS > 9999999999)

alors erreur A00107;
verif 10004:
application : iliad ;


si
  (V_MODUL+0) < 1
  et
   V_IND_TRAIT > 0
   et
   APPLI_BATCH=1
   et
   positif(V_0AB + 0) = 1
   et
   (positif(V_0AX + 0) = 0
    ou
    positif(V_0AM + V_0AO + 0) = 1)

alors erreur A004 ;
verif 10005:
application : iliad ;


si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   (APPLI_BATCH + APPLI_COLBERT) = 1
   et
   positif(V_0AX + 0) = 1
   et
   positif(V_0AC + V_0AD + V_0AV + 0) = 1
   et
   positif(V_0AB + 0) = 0

alors erreur A005 ;
verif 10101:
application : iliad  ;

si
  (V_MODUL+0) < 1
   et
   V_0AM + 0 = 1
   et
   V_0AG + V_0AN + V_0AW + V_0AL + 0 > 0

alors erreur A01001 ;
verif 10102:
application : iliad  ;
si
   (V_MODUL+0) < 1
   et
   V_0AO + 0 = 1
   et
   V_0AG + V_0AL + V_0AN + V_0AW + V_0AU + 0 > 0

alors erreur A01002 ;
verif 10103:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_0AV + 0 = 1
   et
   BOOL_0AZ != 1
   et
   V_0AF + V_0AS + V_0AU + 0 > 0

alors erreur A01003 ;
verif 10104:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_0AC + 0 = 1
   et
   V_0AF + V_0AS + V_0AU + 0 > 0

alors erreur A01004 ;
verif 10105:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_0AD + 0 = 1
   et
   V_0AF + V_0AS + V_0AU + 0 > 0

alors erreur A01005 ;
verif 10106:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_0AC = 1
   et
   V_0AG = 1

alors erreur A01006 ;
verif 10107:
application : iliad  ;

si
  (V_MODUL+0) < 1
  et
   V_0AD = 1
   et
   V_0AG = 1

alors erreur A01007 ;
verif 10108:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_0AV = 1
   et
   V_INDG = 1
   et
   V_0AG = 1
   ou
   (present(V_0AZ) = 1 et V_0AV = 1 et BOOL_0AZ != 1 et V_INDG = 1 et V_0AG = 1)

alors erreur A01008 ;
verif 10109:
application : iliad  ;

si
  (V_MODUL+0) < 1
  et
   V_0AM + 0 = 1
   et
   V_0BT + 0 = 1

alors erreur A01009 ;
verif 101010:
application : iliad  ;

si
  (V_MODUL+0) < 1
  et
   V_0AO + 0 = 1
   et
   V_0BT+0 = 1

alors erreur A01010 ;
verif 101011:
application : iliad  ;

si
  (V_MODUL+0) < 1
  et
   V_0AP + V_0AF + V_0AS + V_0AW + V_0AL + V_0AN + V_0AG + V_0BT + 0 > 0
   et
   positif(V_0AM + V_0AO + V_0AC + V_0AD + V_0AV + 0) != 1

alors erreur A01011 ;
verif 1011:
application : iliad  ;


si
   V_IND_TRAIT > 0
   et
   V_0DN + V_0DP + 0 = 1

alors erreur A011 ;
verif 10121:
application : iliad  ;


si
   (V_MODUL+0) < 1
   et
   V_0CF + 0 < V_0CG

alors erreur A01201 ;
verif 10122:
application : iliad  ;


si
   (V_MODUL+0) < 1
   et
   V_0CI + 0 > V_0CH +0

alors erreur A01202 ;
verif 1013:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   (V_IND_TRAIT = 4
    et
    (V_0DA < (ANNEEREV - 129) ou V_0DA > ANNEEREV ou V_0DB < (ANNEEREV - 129) ou V_0DB > ANNEEREV))
   ou
   (V_IND_TRAIT = 5
    et
    ((positif(V_0DB) = 1 et ( V_0DB < (ANNEEREV - 129) ou V_0DB > ANNEEREV ) )
     ou
     (V_0DA < (ANNEEREV - 129) ou V_0DA > ANNEEREV)))

alors erreur A013 ;
verif 10171:
application : iliad ;


si
   (V_MODUL+0) < 1
   et
   (APPLI_OCEANS + 0) < 1
   et
   V_ZDC = 1
   et
   somme(i=X,Y,Z: positif(V_0Ai)) > 1

alors erreur A01701 ;
verif 10172:
application : iliad  ;


si
   (V_MODUL+0) < 1
   et
   (APPLI_OCEANS + 0) < 1
   et
   (APPLI_BATCH + APPLI_ILIAD ) = 1
   et
   V_ZDC = 4
   et
   (positif(V_0AZ + 0) = 0
    ou
    V_0AM + V_0AO + (V_0AC + V_0AD + V_0AV) * V_0AB + 0 = 0)

alors erreur A01702 ;
verif 10173:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   (APPLI_OCEANS + 0) < 1
   et
   V_ZDC = 1
   et
   positif(V_0AX) = 1
   et
   V_0AM + V_0AO + 0 = 0

alors erreur A01703 ;
verif 10174:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   (APPLI_OCEANS + 0) < 1
   et
   V_ZDC = 1
   et
   positif(V_0AY) = 1
   et
   V_0AD + 0 = 0

alors erreur A01704 ;
verif 10175:
application :  iliad;


si
   (V_MODUL+0) < 1
   et
   ( APPLI_OCEANS + 0) < 1
   et
   V_ZDC = 1
   et
   positif(V_0AZ) = 1
   et
   V_0AV + V_0AM + 0 = 0

alors erreur A01705 ;
verif 1018:
application : iliad ;


si 
   (V_MODUL+0) < 1
   et
   APPLI_COLBERT = 0
   et
   null(10 - V_NOTRAIT) = 1
   et
   V_ZDC + 0 = 0
   et
   positif(V_0AZ) = 1
   et
   V_0AV + V_0AM + V_0AO + 0 = 0

alors erreur A018 ;
verif 1019:
application : iliad  ;


si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT > 0
   et
   (V_0CF > 25 ou V_0CG > 25 ou V_0CH > 25 ou V_0CI > 25 ou V_0CR > 25 ou V_0DJ > 25 ou V_0DN > 25 ou V_0DP > 25)

alors erreur A019 ;
verif 1021:
application :  iliad ;


si
   (APPLI_OCEANS + 0) < 1
   et
   ((positif(V_0CF + 0) != 1
     et
     (pour un i dans 0..7: positif(V_0Fi + 0) = 1))
    ou
    (positif(V_0CH + 0) != 1
     et
     (pour un i dans 0..5: positif(V_0Hi) = 1)))

alors erreur A021 ;
verif 1022:
application :  iliad ;


si
   (V_MODUL+0) < 1
   et
   (APPLI_OCEANS + APPLI_COLBERT + 0) < 1
   et
   V_NOTRAIT = 10
   et
   (pour un i dans 0..5: V_BT0Fi = ANNEEREV - 18)
   et
   (pour un i dans 0..5: V_0Ji = ANNEEREV - 18)

alors erreur A022 ;
verif 10231:
application :  iliad ;


si
   (V_MODUL+0) < 1
   et
   (APPLI_OCEANS + 0) < 1
   et
   V_NOTRAIT+0 < 20
   et
   V_IND_TRAIT + 0 = 4
   et
   (
    (present(V_0AX) = 1
     et
     (inf( ( V_0AX - ANNEEREV ) / 1000000) > 31
      ou
      inf( ( V_0AX - ANNEEREV ) / 1000000) = 0))
    ou
    (present(V_0AY) = 1
     et
     (inf( ( V_0AY - ANNEEREV ) / 1000000) > 31
      ou
      inf( ( V_0AY - ANNEEREV ) / 1000000) = 0))
    ou
    (present(V_0AZ) = 1
     et
     (inf( ( V_0AZ - ANNEEREV ) / 1000000) > 31
      ou
      inf( ( V_0AZ - ANNEEREV ) / 1000000) = 0))
   )

alors erreur A02301 ;
verif 10232:
application :  iliad ;


si
   (V_MODUL+0) < 1
   et
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT > 0
   et
   V_IND_TRAIT + 0 = 4
   et
   (
    (present(V_0AX) = 1
     et
     (
     (    inf ( V_0AX / 10000) * 10000
        - inf ( V_0AX / 1000000)* 1000000
     )/10000 > 12
   ou
     (    inf ( V_0AX / 10000) * 10000
        - inf ( V_0AX / 1000000)* 1000000
     )/10000 =0
   )
)
ou
(  present(V_0AY) =1
 et
  (
     (    inf ( V_0AY / 10000) * 10000
        - inf ( V_0AY / 1000000)* 1000000
     )/10000 > 12
   ou
     (    inf ( V_0AY / 10000) * 10000
        - inf ( V_0AY / 1000000)* 1000000
     )/10000 =0
   )
)
ou
(  present(V_0AZ) =1
 et
  (
     (    inf ( V_0AZ / 10000) * 10000
        - inf ( V_0AZ / 1000000)* 1000000
     )/10000 > 12
   ou
     (    inf ( V_0AZ / 10000) * 10000
        - inf ( V_0AZ / 1000000)* 1000000
     )/10000 =0
   )
)
)

alors erreur A02302 ;
verif 10233:
application :  iliad ;


si
   (V_MODUL+0) < 1
   et
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT > 0
   et
   V_IND_TRAIT + 0 = 4
   et
   (
    (present(V_0AX) = 1
     et
     (V_0AX - inf(V_0AX/ 10000) * 10000) != V_ANREV
     et
     (V_0AX - inf(V_0AX/ 10000) * 10000) != V_ANREV - 1)
    ou
    (present(V_0AY) = 1
     et
     (V_0AY - inf(V_0AY/ 10000) * 10000) != V_ANREV
     et
     (V_0AY - inf(V_0AY/ 10000) * 10000) != V_ANREV - 1)
    ou
    (present(V_0AZ) = 1
     et
     (V_0AZ - inf(V_0AZ/ 10000) * 10000) != V_ANREV
      et
     (V_0AZ - inf(V_0AZ/ 10000) * 10000) != V_ANREV - 1)
   )

alors erreur A02303 ;
verif 1024:
application : iliad ;


si
 
   (V_MODUL+0) < 1
   et
   (V_IND_TRAIT +0 = 4
      et
   (
   (positif(V_0AX) = 1
    et
   V_0AX + 0 < 1010000 + (V_ANREV - 1))
    ou
  (positif(V_0AY) = 1
    et
  V_0AY + 0 < 1010000 + V_ANREV)
    ou
 (positif(V_0AZ) = 1
   et
 V_0AZ + 0 < 1010000 + V_ANREV)
      )
   ou
  V_IND_TRAIT +0 = 5
      et
       (
        (positif(V_0AX) = 1
          et
     V_0AX + 0 < 1010000 + (V_ANREV - 1))
    ou
  (positif(V_0AY) = 1
    et
    V_0AY + 0 < 1010000 + V_ANREV)
      ou
     (positif(V_0AZ) = 1
      et
     V_0AZ + 0 < 1010000 + V_ANREV)
         )
      )

alors erreur A024 ;
verif 1030:
application :  iliad ;


si
   (V_MODUL+0) < 1
   et
   V_0CF + V_0CG + V_0CH + V_0CI + V_0CR + V_0DJ + V_0DN + V_0DP + 0 = 0
   et
   SOMMEA030 > 0

alors erreur A030 ;
verif 1031:
application :  iliad ;


si
   (V_MODUL+0) < 1
   et
   V_0AC + V_0AD + V_0AV + 0 > 0
   et
   SOMMEA031 > 0

alors erreur A031 ;
verif 1063:
application : bareme ;


si
   (V_MODUL+0) < 1
   et
   V_9VV < 2
   et
   V_0AM + V_0AO + 0 = 1

alors erreur A063 ;
verif 1064:
application : bareme ;


si
   (V_MODUL+0) < 1
   et
   (V_9VV < 1.25
    et
    (V_0AC = 1 ou V_0AD = 1)
    et
    V_9XX = 1)
   ou
   (V_9VV < 2.25
    et
    (BOOL_0AM = 1 ou V_0AV = 1)
    et
    V_9XX = 1)
   ou
   (V_9VV = 1.25
    et
    V_0BT = 1
    et
    V_9XX=1)

alors erreur A064 ;
verif 10651:
application : bareme ;


si
   (V_MODUL+0) < 1
   et 
   ((V_9VV / 0.25) - arr(V_9VV / 0.25)) != 0

alors erreur A06501 ;
verif 10652:
application : bareme ;


si
   (V_MODUL+0) < 1
   et
   (V_9VV < 1
   ou
   V_9VV > 99.75)

alors erreur A06502 ;
verif 1066:
application : bareme ;


si
   (V_MODUL+0) < 1
   et
   V_9VV < 2
   et
   V_0AV + V_0AZ = 2

alors erreur A066 ;
verif 1075:
application :  iliad ;


si
  present(VALREGCO) = 0
  et  
  V_CNR2 + 0 = 1
  et 
  present(V_0DA) = 1
  et
  V_NOTRAIT < 14

alors erreur A075 ;
verif 1077:
application :  iliad ;

si
  (V_MODUL+0) < 1
  et
   positif(COD8XK + 0) = 1
   et
   V_REGCO + 0 != 3

alors erreur A077 ;
verif 1078:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   positif(COD8YK + 0) = 1
   et
   ((V_REGCO+0) dans (2,3)
   ou
   (VALREGCO) dans (2,3))

alors erreur A078 ;
verif 1079:
application :  iliad ;

si
   positif_ou_nul(BRAS + CODZRE + CODZRF ) >= 0
   et
   V_CNR2 + 0 != 1

alors erreur A079 ;
verif 1080:
application :  iliad ;


si
   (V_NOTRAIT + 0 < 20
    et
   ( present(BRAS) +present( CODZRE) +present( CODZRF)) = 1 et V_CNR + 0 != 1 et V_CNR2+0 = 1)
   ou
   (V_NOTRAIT >= 20
    et
   ( positif(BRAS)+positif(CODZRE) +positif(CODZRF)) = 1 et V_CNR + 0 != 1 et V_CNR2+0 = 1)

alors erreur A080 ;
verif 1082:
application :  iliad ;


si
   (APPLI_OCEANS + 0) < 1
   et
   V_IND_TRAIT > 0
   et
   VALREGCO non dans (2,3)

alors erreur A082 ;
verif 1083:
application :  iliad ;


si
   (APPLI_OCEANS + 0) < 1
   et
   V_NOTRAIT+0 = 10
   et
   (VALREGCO = 2 ou VALREGCO = 3) 
   et
   V_CNR2 + 0 != 1

alors erreur A083 ;
verif 1084:
application : iliad  ;



si
   positif(present(CODZRB) + present(CODZRA)) = 1
      et
         present(CODZRB) + present(CODZRA) < 2


alors erreur A084;
verif 1085:
application : iliad  ;


si
    (V_MODUL+0) < 1
    et
   (V_NOTRAIT + 0 < 20
    et
    (NRBASE >= 0 ou NRINET >= 0)
    et
     (VALREGCO+0) !=3) 
   ou
   (V_NOTRAIT + 0 > 20
    et
    (NRBASE > 0 ou NRINET > 0)
    et
    (VALREGCO+0) !=3)
    

alors erreur A085 ;
verif 1086:
application : iliad  ;


si
   (V_MODUL+0) < 1
   et
   positif(present(NRBASE) + present(NRINET)) = 1
   et
   present(NRBASE) + present(NRINET) < 2

alors erreur A086 ;
verif 1087:
application : iliad  ;


si
   (V_MODUL+0) < 1
   et 
   (V_NOTRAIT + 0 < 20
    et
    (V_CNR + 0 = 1
    ou
    (VALREGCO) dans (2))
    et
    IND_TDR >= 0)
   ou
   (V_NOTRAIT + 0 > 20
    et
   ( V_CNR + 0 = 1
   ou
   (VALREGCO) dans(2))
    et
    IND_TDR > 0)

alors erreur A087 ;
verif 1088:
application : iliad  ;


si
   (IPTEFP + IPTEFN
    + SALEXTV + SALEXTC + SALEXT1 + SALEXT2 + SALEXT3 + SALEXT4
    + COD1AH + COD1BH + COD1CH + COD1DH + COD1EH + COD1FH
    + CODDAJ + CODDBJ + CODEAJ + CODEBJ + 0) > 0
   et
   (V_CNR + 0 = 1
   ou
   (VALREGCO) dans(2))

alors erreur A088 ;
verif 1089:
application : iliad ;


si
   ((APPLI_OCEANS = 1 et (V_8ZT + CODZRE + CODZRF) > 0)
    ou
    (APPLI_BATCH = 1 et (V_8ZT + CODZRE + CODZRF) >= 0))
   et
   V_CNR + 0 != 1

alors erreur A089 ;
verif 10891:
application : iliad ;


si
   (APPLI_OCEANS + 0) < 1
   et
   (( ( V_8ZT + CODZRE + CODZRF)  >= 0 et V_CNR+0 != 1 et V_NOTRAIT + 0 < 20)
    ou
    ( (V_8ZT + CODZRE + CODZRF)  > 0 et V_CNR+0 != 1 et V_NOTRAIT >= 20))

alors erreur A089 ;
verif 1090:
application : iliad  ;


si
    V_IND_TRAIT + 0 > 0
    et
   ( V_8ZT + CODZRE + CODZRF)  > ( somme(i=V,C,1..4: TPRi)
              + GLN3V + GLN3C
              + RVTOT + T2RV
              + COD1TZ + COD1GG + COD1HG + COD1IG + COD1JG + COD1KG + COD1LG
	      + CODRAI + CODRBI + CODRCK 
	      + 2 )

alors erreur A090 ;
verif 1091:
application : iliad  ;


si
   (V_NOTRAIT + 0 < 20
    et
    (present(RMOND) = 1 ou present(DMOND) = 1)
    et V_CNR + 0 != 1)
   ou
   (V_NOTRAIT >= 20
    et
    (positif(RMOND) = 1 ou positif(DMOND) = 1)
    et V_CNR + 0 != 1)

alors erreur A091 ;
verif 1092:
application : iliad ;


si
   (V_NOTRAIT + 0 < 20
    et
    ((positif(IPTXMO) = 1 et present(DMOND) != 1 et present(RMOND) != 1)
     ou
     ((present(RMOND) = 1 ou present(DMOND) = 1) et positif(IPTXMO+0) != 1)))
   ou
   (V_NOTRAIT >= 20
    et
    ((positif(IPTXMO) = 1 et positif(DMOND) != 1 et positif(RMOND) != 1)
     ou
     ((positif(RMOND) = 1 ou positif(DMOND) = 1) et positif(IPTXMO+0) != 1)))

alors erreur A092 ;
verif 1093:
application : iliad  ;


si
   (V_NOTRAIT + 0 < 20
    et
    present(RMOND) = 1
    et
    present(DMOND) = 1)
   ou
   (V_NOTRAIT >= 20
    et
    positif(RMOND) = 1
    et
    positif(DMOND) = 1)

alors erreur A093 ;
verif 1094:
application :  iliad ;

si
   (V_NOTRAIT + 0 < 20
    et
    present(IPSOUR) = 1 
    et 
    ((V_REGCO + 0) dans(1,5,6)
     ou
     (VALREGCO ) non dans (2,3))
   )	
   ou
   (V_NOTRAIT >= 20
    et
    positif(IPSOUR) = 1 
    et
    ((V_REGCO + 0) dans(1,5,6)
    ou 
    (VALREGCO ) non dans (2,3)) 
   )	


alors erreur A094 ;
verif 1096:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   ((V_REGCO) dans (2,3)
   ou
   (VALREGCO) dans (2,3))
   et
   ((V_IND_TRAIT = 4 et BASRET >= 0 et IMPRET >= 0)
    ou
    (V_IND_TRAIT = 5 et BASRET > 0 et IMPRET > 0))

alors erreur A096 ;
verif 1097:
application :  iliad ;


si
   (V_MODUL+0) < 1
   et
   present(PERPIMPATRIE) = 1
   et
   (V_CNR = 1
   ou
   VALREGCO = 2)

alors erreur A097 ;
verif 10981:
application :  iliad ;

si
   (V_MODUL+0) < 1
   et
   positif(PVMOBNR + 0) = 1
   et
   V_CNR != 1

alors erreur A098 ;

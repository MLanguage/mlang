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
verif 1800:
application : iliad  ;


si
   RG + 2 < PRODOM + PROGUY

alors erreur A800 ;
verif 18021:
application : iliad  ;


si
   (V_NOTRAIT >= 20
    et
    IPTEFP > 0
    et
    IPTEFN > 0)
   ou
   (V_NOTRAIT + 0 < 20
    et
    IPTEFP >= 0
    et
    IPTEFN >= 0
    et
    V_ROLCSG+0 < 40)

alors erreur A80201 ;
verif 18022:
application : iliad  ;


si
   (
    V_NOTRAIT + 0 < 20
    et
    IPTEFP + IPTEFN >= 0
    et
    PRODOM + PROGUY + CODDAJ + CODDBJ + CODEAJ + CODEBJ >= 0
   )
   ou
   (
    V_NOTRAIT >= 20
    et
    IPTEFP + IPTEFN > 0
    et
    PRODOM + PROGUY + CODDAJ + CODDBJ + CODEAJ + CODEBJ > 0
   )

alors erreur A80202 ;
verif 18023:
application : iliad  ;

si
   (
    V_NOTRAIT + 0 < 20
    et
    SOMMEA802 > 0
    et
    CODDAJ + CODDBJ + CODEAJ + CODEBJ > 0
    )
    ou

   (
    V_NOTRAIT >= 20
    et
    SOMMEA802 > 0
    et
    CODDAJ + CODDBJ + CODEAJ + CODEBJ > 0
    )

alors erreur A80203 ;
verif 1803:
application : iliad  ;

si
   V_IND_TRAIT > 0
   et
   positif(CODDAJ + CODDBJ + CODEAJ + CODEBJ + 0) = 1
   et
   V_REGCO + 0 != 1

alors erreur A803 ;
verif 1804:
application : iliad  ;

si
   PROGUY + PRODOM + CODDAJ + CODEAJ + CODDBJ + CODEBJ+ 0 > 0
   et
   SOMMEA804 > 0

alors erreur A804 ;
verif 1805:
application : iliad  ;


si
   V_IND_TRAIT > 0
   et
   positif(TREVEX) = 1
   et
   SOMMEA805 = 0

alors erreur A805 ;
verif 1806:
application : iliad  ;

si
   V_IND_TRAIT > 0
   et
   positif(PROGUY + PRODOM + CODDAJ + CODEAJ + CODDBJ + CODEBJ + 0) = 1
   et
   ((positif(CARTSNBAV + 0) = 1
     et    
     null(CARTSNBAV - 4) = 0)
   ou
    (positif(CARTSNBAC + 0) = 1
     et 
     null(CARTSNBAC - 4) = 0))

alors erreur A806 ;
verif 1807:
application : iliad  ;

si
   V_IND_TRAIT > 0
   et
   positif(PRELIBXT + 0) = 1
   et
   positif(PCAPTAXV + PCAPTAXC + COD1CT + COD1DT + COD1ET + COD1FT + 0) = 0

alors erreur A807 ;
verif 1808:
application : iliad  ;

si
   positif(COD5XT + COD5XV + COD5XU + COD5XW + 0) = 1
   et
   positif(PRODOM + PROGUY + CODEAJ + CODEBJ + CODDAJ + CODDBJ + 0) = 1

alors erreur A808 ;
verif 18101:
application : iliad  ;

si
   ((V_REGCO+0) dans (1,5,6)
   ou (VALREGCO+0) non dans (2,3)) 
   et	
   positif(COD1AF + COD1BF + COD1CF  + COD1DF + COD1EF  + COD1FF + CODRAF + CODRBF + CODRCF+ CODRDF + CODREF + CODRFF + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81001 ;
verif 18102:
application : iliad  ;

si
   ((V_REGCO+0) dans (1,5,6)
      ou (VALREGCO+0) non dans (2,3))
   et
   positif(COD1AL + COD1BL + COD1CL + COD1DL + COD1EL  + COD1FL + CODRAL + CODRBL + CODRCL + CODRDL + CODREL + CODRFL + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81002 ;
verif 18103:
application : iliad  ;

si
  ((V_REGCO+0) dans (1,5,6)
     ou (VALREGCO+0) non dans (2,3))
   et
   positif(COD1AR + COD1BR + COD1CR + COD1DR + CODRAR + CODRBR + CODRCR + CODRDR + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81003 ;
verif 18104:
application : iliad  ;

si 
((V_REGCO+0) dans (1,5,6)
   ou (VALREGCO+0) non dans (2,3))
   et
   positif(COD4BK + COD4BL + CODRBK + CODRBL + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81004 ;
verif 18105:
application : iliad  ;

si
((V_REGCO+0) dans (1,5,6)
   ou (VALREGCO+0) non dans (2,3))
  et
   positif(COD5AK + COD5AL + COD5BK + COD5BL + COD5CK + COD5CL + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81005 ;
verif 18106:
application : iliad  ;

si
   ((V_REGCO+0) dans (1,5,6)
      ou (VALREGCO+0) non dans (2,3))
   et
   positif(COD5DF + COD5DG + COD5EF + COD5EG + COD5FF + COD5FG + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81006 ;
verif 18107:
application : iliad  ;

si
   ((V_REGCO+0) dans (1,5,6)
      ou (VALREGCO+0) non dans (2,3))
   et
   positif(COD5EY + COD5EZ + COD5FY + COD5FZ + COD5GY + COD5GZ + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81007 ;
verif 18108:
application : iliad  ;

si
  ((V_REGCO+0) dans (1,5,6)
   ou (VALREGCO+0) non dans (2,3))  
   et
   positif(COD5UR + COD5US + COD5VR + COD5VS + COD5WR + COD5WS + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81008 ;
verif 18109:
application : iliad  ;

si
   ((V_REGCO+0) dans (1,5,6)
      ou (VALREGCO+0) non dans (2,3))
   et
   positif(COD5XJ + COD5XK + COD5YJ + COD5YK + COD5ZJ + COD5ZK + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81009 ;
verif 18110:
application : iliad  ;

si
   ((V_REGCO+0) dans (1,5,6)
      ou (VALREGCO+0) non dans (2,3))
   et
   positif(COD5XS + COD5XX + COD5YS + COD5YX + COD5ZS + COD5ZX + 0) = 1
   et
   present(IPBOCH) = 0

alors erreur A81010 ;
verif 18111:
application : iliad  ;

si 
  ((V_REGCO+0) dans(2)
   ou (VALREGCO) dans (2))
   et	
   positif(COD1AF + COD1BF + COD1CF  + COD1DF + COD1EF  + COD1FF + CODRAF + CODRBF + CODRCF + CODRDF + CODREF+ CODRFF + 0) = 1
   et
   (present(IPSOUR) + present(V_8ZT)+ present(CODZRE) + present(CODZRF)) = 0

alors erreur A81101 ;
verif 18112:
application : iliad  ;

si
  ((V_REGCO+0) dans(2)
   ou (VALREGCO) dans (2))  
   et
   positif(COD1AL + COD1BL + COD1CL + COD1DL + COD1EL  + COD1FL + CODRAL + CODRBL + CODRCL + CODRDL + CODREL + CODRFL + 0) = 1
   et
   (present(IPSOUR) + present(V_8ZT)+ present(CODZRE) + present(CODZRF)) = 0

alors erreur A81102 ;
verif 18113:
application : iliad  ;

si
   ((V_REGCO+0) dans(2)
   ou (VALREGCO) dans (2))
   et
   positif(COD1AR + COD1BR + COD1CR + COD1DR + CODRAR + CODRBR + CODRCR + CODRDR + 0) = 1
   et
   (present(IPSOUR) + present(V_8ZT) + present(CODZRE) + present(CODZRF)) = 0

alors erreur A81103 ;
verif 1821:
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   (V_IND_TRAIT > 0 )
   et
   present(BASRET) + present(IMPRET) = 1

alors erreur A821 ;
verif 1830:
application : iliad  ;

si
  ((V_REGCO) dans(2)
  ou (VALREGCO) dans (2))
   et
   positif(COD8VL + COD8VM + COD8WM + COD8UM) = 1

alors erreur A830 ;
verif 1831:
application : iliad  ;

si
  positif(COD8VL) = 1
   et
   (present(RCMABD) +present( REVACT) + present(RCMHAD) + present(DISQUO) + present(RCMHAB) + present(INTERE)
   + present(REGPRIV) + present(RESTUC)
   + present(RCMIMPAT) + present(BPCOSAV) 
   + present(BPCOSAC) + present(BPVSJ) + present(BPVSK) + present(BPV18V)
   + present(BPV40V) + present(BPCOPTV) + present(BPVRCM) + present(CODRVG) + present(COD3TJ)  + present(RCMTNC)) = 0

alors erreur A831 ;
verif 1858:
application : iliad  ;

si
   COD8TL + COD8UW + 0 > 0
   et
   SOMMEA858 = 0

alors erreur A858 ;
verif 1859:
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   PRESINTER > 0
   et
   SOMMEA859 = 0

alors erreur A859 ;
verif 1862:
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   AUTOVERSLIB > 0
   et
   SOMMEA862 = 0

alors erreur A862 ;
verif corrective 1863:
application : iliad ;

si
   (APPLI_OCEANS + 0) < 1
   et
   positif(AUTOVERSSUP + 0) = 1
   et
   positif(AUTOBICVV + AUTOBICPV + AUTOBNCV
           + AUTOBICVC + AUTOBICPC + AUTOBNCC
           + AUTOBICVP + AUTOBICPP + AUTOBNCP + 0) = 0

alors erreur A863 ;
verif 1864:
application : iliad  ;

si
  (V_MODUL+0) < 1
    et
   V_IND_TRAIT > 0
   et
   COD8YL + 0 > CGLOA + 0

alors erreur A864 ;
verif 1865:
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT > 0
   et
   COD8YT + 0 > CVNN + 0

alors erreur A865 ;
verif 18661:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   CSPROVYD + 0 > max(0 , RSE1 + PRSE1 - CIRSE1) + 0
   et
   (V_GESTPAS+0) < 1

alors erreur A86601 ;
verif 18662:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   CSPROVYE + 0 > max(0 , RSE5 + PRSE5 - CIRSE5) + 0
   et
   (V_GESTPAS+0) < 1 

alors erreur A86602 ;
verif 18663:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   CSPROVYF + 0 > max(0 , RSE8TV + arr(max(0 , RSE8TV - CIRSE8TV - CSPROVYF) * TXINT/100) - CIRSE8TV) + 0
   et
   (V_GESTPAS+0) < 1

alors erreur A86603 ;
verif 18664:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   CSPROVYG + 0 > max(0 , RSE3 + PRSE3 - CIRSE3) + 0
   et
   (V_GESTPAS+0) < 1

alors erreur A86604 ;
verif 18665:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   CSPROVYH + 0 > max(0 , RSE8TX + arr(COD8SX*TXTX/100)+ arr(max(0 , RSE8TX + arr(COD8SX*TXTX/100) - CIRSE8TX - CSPROVYH) * TXINT/100) - CIRSE8TX) + 0
   et
   (V_GESTPAS+0) < 1

alors erreur A86605 ;
verif 18666:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   CSPROVYN + 0 > max(0 , RSE8SA + arr(max(0 , RSE8SA - CIRSE8SA - CSPROVYN) * TXINT/100) - CIRSE8SA) + 0
   et
   (V_GESTPAS+0) < 1

alors erreur A86606 ;
verif 18667:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   CSPROVYP + 0 > max(0 , RSE8SB + arr(max(0 , RSE8SB - CIRSE8SB - CSPROVYP) * TXINT/100) - CIRSE8SB) + 0
   et
   (V_GESTPAS+0) < 1

alors erreur A86607 ;
verif 18668:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   COD8YQ + 0 > max(0 ,  RSE6 + PRSE6 - CIRSE6) + 0
   et
   (V_GESTPAS+0) < 1

alors erreur A86608 ;
verif 186610:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   COD8ZH + 0 > max(0 ,MCSG820 ) + 0
   et
   (V_GESTPAS+0) < 1

alors erreur A86610 ;
verif 186611:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   COD8YV + 0 > max(0 , (COD8TH * TX066/100) + (PRSE8 * positif(COD8TH)) - arr(min(COD8XO , COD8TH) * TX066/100))  + 0
   et
  (V_GESTPAS+0) < 1

alors erreur A86611 ;
verif 186612:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   V_IND_TRAIT > 0
   et
   COD8YX + 0 > max(0 , (COD8SD * TX066/100)  + (PRSE8 * positif(COD8SD)) -  arr(min(COD8XN ,COD8SD) * TX066/100)) + 0
   et
  (V_GESTPAS+0) < 1

alors erreur A86612 ;
verif 1868:
application :  iliad ;

si
   V_IND_TRAIT + 0 > 0
   et
   (CDISPROV + 0 > CDIS + 0
    ou
    (positif(CDISPROV + 0) = 1 et positif(GSALV + GSALC + 0) = 0))

alors erreur A868 ;
verif 1870:
application :  iliad ;


si
   (V_MODUL+0) < 1
     et
   positif(DCSGIM) = 1 
   et 
   positif(CSGIM + 0) != 1
    
alors erreur A870 ;
verif 1871:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   CRDSIM > RDSN

alors erreur A871 ;
verif 1872:
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   V_IND_TRAIT + 0 > 0
   et
   PRSPROV > PSOL

alors erreur A872 ;
verif 1873:
application : iliad  ;

si
  (V_MODUL+0) < 1
    et
   (APPLI_OCEANS + 0) < 1
   et
   CSGIM > CSG
    
alors erreur A873 ;
verif 1874:
application : iliad  ;


si
   (V_MODUL+0) < 1
     et
   IPSOUR >= 0
   et
   SOMMEA874 = 0
   et
   (V_GESTPAS+0) < 1

alors erreur A874 ;
verif 1875:
application : iliad  ;

si
  (V_MODUL+0) < 1
et
  max(0 ,IRB + TAXASSUR + IPCAPTAXTOT + CHRAPRES + CHRPVIMP - IRE) < IRANT 
alors erreur A875 ;  
verif 1877:
application : iliad  ;


si
   (V_MODUL+0) < 1
    et 
   (IPRECH + 0 > 0 ou IPCHER + 0 > 0)
   et
   SOMMEA877 = 0

alors erreur A877 ;
verif 1878:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et 
   COD8SG +0 > 0
   et 
   SOMMEA878 = 0

alors erreur A878 ;
verif 1879:
application : iliad  ;


si
   (V_MODUL+0) < 1
     et
   (CIINVCORSE + CICORSENOW + 0) > 0
   et
   SOMMEA879 = 0

alors erreur A879 ;
verif 1880:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
   COD8TE > 0
   et
   SOMMEA880 = 0

alors erreur A880 ;
verif 1881:
application : iliad  ;


si
   (V_MODUL+0) < 1
     et
   CREFAM > 0
   et
   SOMMEA881 = 0

alors erreur A881 ;
verif 18821:
application : iliad  ;


si
  (
   IPMOND > 0
   et
   (present(IPTEFP) = 0 et present(IPTEFN) = 0)
  )
  ou
  (
   (present(IPTEFP) = 1 ou present(IPTEFN) = 1)
   et
   present(IPMOND) = 0
  )

alors erreur A88201 ;
verif 18822:
application : iliad  ;

si
   (present(IPMOND)
    + present(SALEXTV) + present(SALEXTC) + present(SALEXT1) + present(SALEXT2) + present(SALEXT3) + present(SALEXT4)
    + present(COD1AH) + present(COD1BH) + present(COD1CH) + present(COD1DH) + present(COD1EH) + present(COD1FH)) = 0
   et
   positif_ou_nul(TEFFHRC + COD8YJ) = 1

alors erreur A88202 ;
verif 1883:
application : iliad  ;


si
   (V_MODUL+0) < 1
    et
   IPBOCH > 0
   et
   (CIIMPPRO + CIIMPPRO2 + positif_ou_nul(REGCI) + positif(IND8XRAUTO) + PRELIBXT + COD8XF + COD8XG + COD8XH + COD8XX + COD8XY + 0) = 0
   et
  (V_GESTPAS+0) < 1

alors erreur A883 ;
verif 1884:
application : iliad  ;


si
  (V_MODUL+0) < 1
   et
   REGCI + COD8XY > 0
   et
   SOMMEA884 = 0
   et
  (V_GESTPAS+0) < 1

alors erreur A884 ;
verif 18851:
application : iliad  ;


si
  (V_MODUL+0) < 1
  et
   positif(CIIMPPRO2 + 0) = 1
   et
   present(BPVSJ) = 0
    et
  (V_GESTPAS+0) < 1 

alors erreur A88501 ;
verif 18852:
application : iliad  ;

si
   (V_MODUL+0) < 1
    et
   positif(COD8XX + 0) = 1
   et
   positif(present(COD2VV) + present(COD2VO) + present(COD2RC)) = 0
    et
   (V_GESTPAS+0) < 1

alors erreur A88502 ;
verif 18853:
application : iliad  ;

si
  (V_MODUL+0) < 1
   et
   positif(CIIMPPRO + 0) = 1
   et
   somme(i=V,C,P:present(BA1Ai) + present(BAF1Ai) + present(BI1Ai) + present(BN1Ai)) + present(MIB1AV) 
   + present(MIB1AC) + present(MIB1AP) + present(BNCPRO1AV) + present(BNCPRO1AC) + present(BNCPRO1AP) 
   + present(COD2WW) + present(COD2YY) + present(COD2ZZ) + present(RCMABD) + present(RCMTNC) + present(RCMHAD)
   + present(RCMHAB) + present(COD2TT) + present(COD2VP) + present(REGPRIV) + present(COD2VN) + present(BPVRCM) + present(COD2RD) = 0
    et
   (V_GESTPAS+0) < 1

alors erreur A88503 ;
verif 18854:
application : iliad  ;

si
  (V_MODUL+0) < 1
   et
   positif(COD8XF + 0) = 1
   et
   present(BPV18V) = 0
   et
  (V_GESTPAS+0) < 1

alors erreur A88504 ;
verif 18855:
application : iliad  ;

si
   (V_MODUL+0) < 1
    et
   positif(COD8XG + 0) = 1
   et
   present(BPCOPTV) = 0
   et
   (V_GESTPAS+0) < 1

alors erreur A88505 ;
verif 18856:
application : iliad  ;

si
   (V_MODUL+0) < 1
    et
   positif(COD8XH + 0) = 1
   et
   present(BPV40V) = 0
   et
   (V_GESTPAS+0) < 1

alors erreur A88506 ;
verif 18857:
application : iliad  ;

si
   (V_MODUL+0) < 1
    et
    positif(COD8XV + 0) = 1
    et
    present(COD5HA) + present(COD5IA)+ present(COD5JA)
    + present(COD5UI) + present(COD5VI) + present(COD5WI) + present(COD5TF) + present(COD5UF) + present(COD5VF)
    + present(COD5QA) + present(COD5RA) + present(COD5SA) + present(COD5QJ) + present(COD5RJ) + present(COD5SJ) = 0
    et
   (V_GESTPAS+0) < 1

alors erreur A88507 ;
verif 1886:
application : iliad  ;


si
  (V_MODUL+0) < 1
      et
   IPPNCS > 0
   et
   positif(REGCI + CIIMPPRO + CIIMPPRO2 + COD8XX + COD8XF + COD8XG + COD8XH + COD8PA + COD8XV + 0) != 1
   et
   (V_GESTPAS+0) < 1

alors erreur A886 ;
verif 1887:
application : iliad  ;


si
   (V_MODUL+0) < 1
         et
   (APPLI_OCEANS + 0) < 1
   et
   REGCI + 0 > IPBOCH + 0
   et
   (V_GESTPAS+0) < 1
   
alors erreur A887 ;
verif 1888:
application : iliad  ;


si
  positif(COD8PB + 0) = 1
  et
  positif(COD8VL + 0) = 0

alors erreur A888 ;
verif 1889:
application : iliad  ;


si
   (V_MODUL+0) < 1
     et
   (APPLI_OCEANS + 0) < 1
   et
   REVFONC + 0 > IND_TDR + 0
   et
   present(IND_TDR) = 0

alors erreur A889 ;
verif 1890:
application : iliad  ;

si
   (V_MODUL+0) < 1
   et
 COD8WK > 0
     et
 SOMMEA890 = 0

alors erreur A890 ;
verif 1891:
application : iliad  ;

si
   (V_MODUL+0) < 1
    et
   COD8WG > 0
    et
   SOMMEA891 = 0

alors erreur A891 ;
verif 1892:
application : iliad  ;

si
   (V_MODUL+0) < 1
    et
   COD8WH > 0
    et
   SOMMEA892 = 0

alors erreur A892 ;
verif 1893:
application : iliad  ;


si
   (V_MODUL+0) < 1
     et
   CREFORMCHENT > 0
   et
   SOMMEA893 = 0

alors erreur A893 ;
verif 1894:
application : iliad  ;

si
  IPBOCH > 0
  et
  positif(COD4BL + COD4BK + CODRBK + CODRBT) > 0
   et
 (positif_ou_nul(COD8SG) + positif(IND8XRAUTO) + 0) = 0
   et
 (V_GESTPAS+0) < 1

alors erreur A894 ;
verif 1895:
application : iliad  ;


si
   (V_MODUL+0) < 1
     et
   CREAGRIBIO > 0
   et
   SOMMEA895 = 0

alors erreur A895 ;
verif 1896:
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   CREARTS > 0
   et
   SOMMEA896 = 0

alors erreur A896 ;
verif 1898:
application : iliad  ;

si
   (V_MODUL+0) < 1
     et
   CRECONGAGRI > 0
   et
   SOMMEA898 = 0

alors erreur A898 ;
verif 1901:
application : iliad  ;

si
 (V_MODUL+0) < 1
  et
  positif(ANNUL2042) = 1
  et  
  V_NOTRAIT < 20

alors erreur A901 ;
verif 1911:
application : iliad  ;


si
(V_MODUL+0) < 1
  et
(positif(COD8PC +COD8PF+COD8PV)>0) 

alors erreur A911 ; 
verif 192001:
application : iliad  ;

si
 V_IND_TRAIT > 0
 et
 positif(CODSAA + CODSAC + CODSAE) = 1
 et
 SOMMEA9201 < 3

alors erreur A92001 ;
verif 192002:
application : iliad  ;

si
 V_IND_TRAIT > 0
  et
 positif(CODSAB + CODSAD + CODSAF) = 1
 et 
 SOMMEA9202 < 3  

alors erreur A92002 ;
verif 192101:
application : iliad  ;

si
 V_IND_TRAIT > 0
   et
 ((CODSCA)=1 ou (CODSCB) = 1 ou (CODSCC) = 1 ou (CODSCD)=1 ou (CODSCE) = 1 ou (CODSCF) = 1 ou (CODSCG)=1 ou (CODSCH) = 1 ou (CODSCI)=1 ou (CODSCJ) = 1 ou (CODSCK) = 1 ou (CODSCL)= 1 ou (CODSCM) = 1 ou (CODSCN) = 1 ou (CODSCO) = 1)
   et
 positif_ou_nul(CODSAG) + positif_ou_nul(CODSAI) + positif_ou_nul(CODSAK) + positif_ou_nul(CODSAO) + positif_ou_nul(CODSAQ) + positif_ou_nul(CODSAS) + positif_ou_nul(CODSAU) + positif_ou_nul(CODSAW) < 8

alors erreur A92101 ;
verif 192102:
application : iliad  ;

si
 V_IND_TRAIT > 0
  et
((CODSDA) =1 ou (CODSDB)=1 ou (CODSDC)=1 ou (CODSDD) = 1 ou (CODSDE)=1 ou (CODSDF)=1 ou (CODSDG)=1 ou (CODSDH)=1 ou (CODSDI)=1 ou (CODSDJ)=1 ou (CODSDK)=1 ou (CODSDL) =1 ou (CODSDM) = 1 ou (CODSDN) = 1 ou (CODSDO) =1 ou (CODSAH) = 1 ou (CODSAJ)=1 ou (CODSAL)=1 ou (CODSAN)=1 ou (CODSAP) = 1 ou (CODSAR)=1 ou (CODSAT) = 1 ou (CODSAV) = 1 ou (CODSAX) = 1)
  et
 positif_ou_nul(CODSAH) + positif_ou_nul(CODSAJ) + positif_ou_nul(CODSAL) + positif_ou_nul(CODSAP) + positif_ou_nul(CODSAR) + positif_ou_nul(CODSAT) + positif_ou_nul(CODSAV) + positif_ou_nul(CODSAX) < 8

alors erreur A92102 ;
verif 1922011:
application : iliad ;

si
  V_IND_TRAIT > 0
  et

((CODSCA)!= 1 ou (CODSCB)!= 1 ou (CODSCC) != 1 ou (CODSCD) != 1 ou (CODSCE) != 1 ou (CODSCF)!= 1 ou (CODSCG)!= 1 ou
 (CODSCH) != 1 ou (CODSCI) != 1 ou (CODSCJ) != 1 ou (CODSCK) != 1 ou (CODSCL) != 1 ou (CODSCM)!= 1 ou (CODSCN)!= 1 ou 
 (CODSCO)!= 1 ou (CODSDA)!= 1 ou (CODSDB)!= 1 ou (CODSDC)!= 1 ou (CODSDD)!= 1 ou (CODSDE)!= 1 ou (CODSDF)!= 1 ou 
 (CODSDG)!= 1 ou (CODSDH)!= 1 ou (CODSDI)!= 1 ou (CODSDJ)!= 1 ou (CODSDK)!= 1 ou (CODSDL)!= 1 ou (CODSDM)!= 1 ou (CODSDN)!= 1 ou (CODSDO)!= 1)
  et
  positif(ANNUL2042 + 0) = 0

alors erreur A922 ;


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
verif 19611:
application : iliad  ;


si
        (V_MODUL+0) < 1
	  et
	(( 
	CMAJ != 7 et ((APPLI_BATCH = 1 et positif(APPLI_COLBERT + 0)!=1)
                    ou APPLI_OCEANS = 1
                    ou ((APPLI_ILIAD = 1 et positif(APPLI_COLBERT +0) != 1) et
                          non ( V_CALCULIR+0=1
                               ou (V_NOTRAIT+0) dans (16,23,26,33,36,43,46,53,56)
                              )
                        )
	             )
	)
	ou
	( 
	CMAJ2 != 7 et CMAJ2 != 0 et ((APPLI_BATCH = 1 et positif(APPLI_COLBERT + 0) != 1)
                                   ou APPLI_OCEANS = 1
                                   ou ((APPLI_ILIAD = 1 et positif(APPLI_COLBERT + 0) != 1) et
                                       non ( V_CALCULIR+0=1
                                            ou (V_NOTRAIT+0) dans (16,23,26,33,36,43,46,53,56)
                              )
                        )
	             )
        ))
        
alors erreur A96101 ;
verif 19612:
application :  iliad ;


si
       (V_MODUL+0) < 1
         et
       ((
       CMAJ non dans ( 7..8 ) et CMAJ non dans (10..11) et CMAJ non dans (17..18)  
     et (  ((APPLI_ILIAD = 1 et positif(APPLI_COLBERT + 0) != 1) et
                   ( V_CALCULIR + 0 = 1 ou (V_NOTRAIT + 0) dans (16,23,26,33,36,43,46,53,56))
           )
           ou APPLI_COLBERT = 1)
       )
       ou
       (
       CMAJ2 non dans ( 7..8 ) et CMAJ2 non dans (10..11) et CMAJ2 non dans (17..18) et CMAJ2 != 0
     et (  ((APPLI_ILIAD = 1 et positif(APPLI_COLBERT +0) != 1) et
                   ( V_CALCULIR + 0 = 1 ou (V_NOTRAIT + 0) dans (16,23,26,33,36,43,46,53,56))
           )
           ou APPLI_COLBERT = 1)
       ))

alors erreur A96102 ;
verif isf 19613:
application :  iliad ;
si
   (V_MODUL+0) < 1
     et
   ((CMAJ_ISF non dans ( 7,8,10,11,17,18,34 )  et V_IND_TRAIT+0 = 4 )
   ou
   (CMAJ_ISF non dans ( 0,7,8,10,11,17,18,34 )  et V_IND_TRAIT+0 = 5 ))
        
alors erreur A96103 ;
verif 19621:
application : iliad  ;


si
   (V_MODUL+0) < 1
     et
   (( present(CMAJ)=1 et present(MOISAN)=0 )
   ou
   ( present(CMAJ2)=1 et present(MOISAN2)=0 ))

alors erreur A96201 ;
verif 19622:
application : iliad  ;


si
   (V_MODUL+0) < 1
     et
   (( present(CMAJ)=0 et present(MOISAN)=1)
   ou
   ( present(CMAJ2)=0 et present(MOISAN2)=1))

alors erreur A96202 ;
verif isf 19623:
application : iliad  ;
si
   (V_MODUL+0) < 1
     et
          ( present(CMAJ_ISF)=1 et present(MOISAN_ISF)=0 )

alors erreur A96203 ;
verif isf 19624:
application : iliad  ;
si
   (V_MODUL+0) < 1
     et
   ( present(CMAJ_ISF)=0 et present(MOISAN_ISF)=1)

alors erreur A96204 ;
verif 19631:
application : iliad  ;


si
      (V_MODUL+0) < 1
        et
        (V_IND_TRAIT > 0 )
       et
        (
        inf(MOISAN/10000) non dans (01..12)
        ou
        inf(MOISAN2/10000) non dans (00..12)
        )
alors erreur A96301 ;
verif 19632:
application : iliad ;


si 
 (V_MODUL+0) < 1
  et 
   (((positif(APPLI_COLBERT + 0)!=1) et (APPLI_ILIAD=1) et 
	V_IND_TRAIT !=5)
et(
   (
 arr( (MOISAN/10000 - inf(MOISAN/10000))*10000 ) != ANNEEREV +1
et
 arr( (MOISAN/10000 - inf(MOISAN/10000))*10000 ) != ANNEEREV +2
et
 arr( (MOISAN/10000 - inf(MOISAN/10000))*10000 ) != ANNEEREV +3
et
 arr( (MOISAN/10000 - inf(MOISAN/10000))*10000 ) != ANNEEREV +4
et
 arr( (MOISAN/10000 - inf(MOISAN/10000))*10000 ) != ANNEEREV +5
et
 arr( (MOISAN/10000 - inf(MOISAN/10000))*10000 ) != ANNEEREV +6
   )
   ou
   (
 arr( (MOISAN2/10000 - inf(MOISAN2/10000))*10000 ) != ANNEEREV +1
et
 arr( (MOISAN2/10000 - inf(MOISAN2/10000))*10000 ) != ANNEEREV +2
et
 arr( (MOISAN2/10000 - inf(MOISAN2/10000))*10000 ) != ANNEEREV +3
et
 arr( (MOISAN2/10000 - inf(MOISAN2/10000))*10000 ) != ANNEEREV +4
et
 arr( (MOISAN2/10000 - inf(MOISAN2/10000))*10000 ) != ANNEEREV +5
et
 arr( (MOISAN2/10000 - inf(MOISAN2/10000))*10000 ) != ANNEEREV +6
et
 arr( (MOISAN2/10000 - inf(MOISAN2/10000))*10000 ) != 0
   )))
alors erreur A96302 ;
verif 196321:
application : iliad ;


si
   (V_MODUL+0) < 1
     et
    V_IND_TRAIT !=5
   et
   (
    arr( (MOISAN/10000 - inf(MOISAN/10000))*10000 ) != ANNEEREV + 1 
    et
    arr( (MOISAN/10000 - inf(MOISAN/10000))*10000 ) != ANNEEREV + 2
    et
    arr( (MOISAN/10000 - inf(MOISAN/10000))*10000 ) != ANNEEREV + 3
   )

alors erreur A96302 ;
verif isf 19633:
application : iliad  ;
si
         (V_MODUL+0) < 1
	   et
        (
                (V_IND_TRAIT+0 = 4 et inf(MOISAN_ISF/10000) non dans (01..12) )
                ou
                (V_IND_TRAIT+0 = 5 et inf(MOISAN_ISF/10000) non dans (01..12) et MOISAN_ISF != 0 )
        )
alors erreur A96303 ;
verif isf 19634:
application : iliad ;
si
(V_MODUL+0) < 1
  et
(APPLI_OCEANS + 0 < 1) et (
   (
   	V_IND_TRAIT !=5 et(
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +1
et
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +2
et
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +3
et
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +4
   ))
   ou
   (
   	V_IND_TRAIT = 5 et(
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +1
et
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +2
et
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +3
et
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +4
et
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +5
et
 arr( (MOISAN_ISF/10000 - inf(MOISAN_ISF/10000))*10000 ) != V_ANREV +6
 ))
   
   )

alors erreur A96304 ;
verif 19641:
application : iliad  ;


si
        (V_MODUL+0) < 1
	  et
	(
       ((inf( DATDEPETR/1000000 ) non dans (01..31)) et V_IND_TRAIT+0 = 4)
ou
       ((inf( DATDEPETR/1000000 ) non dans (01..31)) et V_IND_TRAIT = 5 et DATDEPETR != 0)
	)

alors erreur A96401;
verif 19642:
application : iliad  ;


si
        (V_MODUL+0) < 1
	  et
	(
       ((inf( (DATDEPETR/1000000 - inf(DATDEPETR/1000000))*100 ) non dans (01..12)) et (V_IND_TRAIT+0= 4))
ou
       ((inf( (DATDEPETR/1000000 - inf(DATDEPETR/1000000))*100 ) non dans (01..12)) 
		et V_IND_TRAIT = 5 et DATDEPETR != 0)
	)

alors erreur A96402;
verif 19643:
application : iliad  ;


si
(V_MODUL+0) < 1
  et
(
 ((arr( (DATDEPETR/10000 - inf(DATDEPETR/10000))*10000 ) != ANNEEREV ) et V_IND_TRAIT+0 = 4)
ou
 ((arr( (DATDEPETR/10000 - inf(DATDEPETR/10000))*10000 ) != ANNEEREV ) 
		et V_IND_TRAIT = 5 et DATDEPETR != 0)
   ) 

alors erreur A96403;
verif 19651:
application : iliad  ;


si
        (V_MODUL+0) < 1
	  et
	(
       ((inf( DATRETETR/1000000) non dans (01..31)) et V_IND_TRAIT+0 = 4)
	ou
       ((inf( DATRETETR/1000000) non dans (01..31)) et V_IND_TRAIT = 5 et DATRETETR != 0)
	)

alors erreur A96501;
verif 19652:
application : iliad  ;


si
        (V_MODUL+0) < 1
	  et
	(
       ((inf( (DATRETETR/1000000 - inf(DATRETETR/1000000))*100 ) non dans (01..12)) et V_IND_TRAIT+0 = 4)
	ou
       ((inf( (DATRETETR/1000000 - inf(DATRETETR/1000000))*100 ) non dans (01..12)) 
	et V_IND_TRAIT = 5 et DATRETETR != 0)
	)

alors erreur A96502 ;
verif 19653:
application : iliad  ;


si
(V_MODUL+0) < 1
  et
(
 ((arr( (DATRETETR/10000 - inf(DATRETETR/10000))*10000 ) != ANNEEREV ) et V_IND_TRAIT+0 = 4)
ou
 ((arr( (DATRETETR/10000 - inf(DATRETETR/10000))*10000 ) != ANNEEREV ) 
            et V_IND_TRAIT = 5 et DATRETETR != 0)
   )

alors erreur A96503 ;
verif 1966:
application : iliad  ;


si
   (V_MODUL+0) < 1
     et
   DATDEPETR > 0 
   et 
   DATRETETR > 0
	 
alors erreur A966 ;
verif isf 1967:
application : iliad  ;
 si
   (V_MODUL+0) < 1
     et
   positif(V_ZDC) > 0
   et
   positif(V_0AM + V_0AO) = 1
   et
   positif(V_0AZ + 0) = 1
   et
   positif(IFIPAT + 0) = 1

alors erreur A967 ;
verif isf 1968:
application : iliad  ;
 si
    (V_MODUL+0) < 1
    et
    V_ZDC = 1
    et
    positif(V_0AC + V_0AD + V_0AV + 0) = 1
    et
    positif(IFIPAT + 0) = 1

alors erreur A968 ;
verif isf 1979:
application : iliad  ;

si
 (V_MODUL+0) < 1
   et
 present(COD9PR)+ present (COD9PX) = 1

alors erreur A979;
verif isf 19801:
application : iliad  ;
 si
   (V_MODUL+0) < 1
     et
   V_NOTRAIT + 0 < 14
   et
   V_IND_TRAIT + 0 = 4
   et	    
   (IFIPAT+0) <= LIM_IFIINF
  et
 positif(present(COD9AA)+present(COD9AB)+present(COD9AC)+present(COD9AD)+present(COD9BA)+present(COD9BB)+present(COD9CA)+present(COD9GF)+present(COD9GH)) > 0
  et
 present(COD9GN) = 0 

alors erreur A980 ;
verif isf 19803:
application : iliad  ;
 si
   ( V_NOTRAIT + 0 = 14 ou V_NOTRAIT + 0 = 16 )
    et
    V_IND_TRAIT = 5
    et
    IFIPAT <= LIM_IFIINF
    et
 positif(present(COD9AA)+present(COD9AB)+present(COD9AC)+present(COD9AD)+present(COD9BA)+present(COD9BB)+present(COD9CA)+present(COD9GF)+present(COD9GH))>0    

    alors erreur A98003 ;
verif isf 19804:
application : iliad  ;
 si
     V_NOTRAIT + 0 > 20 
    et
    IFIPAT <= LIM_IFIINF
    et
    IFIPAT != 0

   alors erreur A98004 ;
verif isf 1982:
application : iliad ;

si
 (V_MODUL+0) < 1
   et
 (V_REGCO = 2 ou V_REGCO = 3)
 et 
  VAR9GN = 1
  et
 INDREV1A8BIS = 1

alors erreur A982 ;
verif isf 1983:
application :  iliad ;
 si
 (V_MODUL+0) < 1
   et
 (APPLI_OCEANS + 0) < 1
   et
 (
  (V_IND_TRAIT + 0 = 4)
   et
     (
      positif(COD9GL + 0 ) = 1
   et
         (positif(V_0AM + V_0AO + 0 ) = 1
          ou
           (positif(V_0AC + V_0AD + V_0AV + 0 )=1
   et
   positif(V_0AB + 0)= 1
           )
         )
      )
  )

alors erreur A983 ;
verif isf 1984:
application :  iliad ;
 si
 (V_MODUL+0) < 1
   et
      (
                  ((V_IND_TRAIT + 0 = 4) ou (V_IND_TRAIT + 0 = 5)) 
                  et
                  (
                  positif(COD9GM + 0 ) = 1
                  et
                        (positif(V_0AM + V_0AO + 0 ) = 1
                         ou
                                (positif(V_0AC + V_0AD + V_0AV + 0 )=1
                                 et
                                 positif(V_0AB + 0)= 0
                                )
                        )
                   )
        )

alors erreur A984 ;
verif isf 1985:
application :  iliad ;
 si
 (V_MODUL+0) < 1
   et
      positif(COD9GY + 0) = 1
      et
       IFIPAT>LIM_IFIINF

alors erreur A985 ;
verif isf 1986:
application :  iliad ;

si
(V_MODUL+0) < 1
  et
    (((APPLI_ILIAD) = 1 et positif(APPLI_COLBERT+ 0) != 1)  ou (APPLI_BATCH) = 1)
    et
    V_NOTRAIT + 0 < 14
    et
    (V_REGCO = 1 ou V_REGCO = 5 ou V_REGCO = 6)
    et
   (positif(COD9GN + 0) = 1)

alors erreur A986 ;
verif isf 19871:
application :  iliad ;
si
   (APPLI_OCEANS + 0) < 1
   et
   V_NOTRAIT + 0 >= 14 
   et
   V_ETCVL + 0 = 1
   et
   COD9GL + COD9GM + 0 = 0

alors erreur A98701 ;
verif isf 19872:
application :  iliad ;
si
   (APPLI_OCEANS + 0) < 1
   et
   V_NOTRAIT + 0 >= 14
   et
   present(V_ETCVL) = 1
   et
   V_ETCVL + 0 = 0
   et
   COD9GL + COD9GM + 0 > 0

alors erreur A98702 ;
verif isf 19873:
application :  iliad ;

si
   (APPLI_OCEANS  + 0) < 1
   et
   V_NOTRAIT + 0 >= 14
   et
   COD9GL + 0 > 0
  et
   COD9GM + 0 > 0


alors erreur A98703 ;
verif isf 1988:
application : iliad ;

si 
  (V_MODUL+0) < 1
    et
  APPLI_BATCH = 1
  et 
  ((V_REGCO = 2 ou V_REGCO = 3))
  et 
  VAR9GN = 1
  et
  INDTELEIR = 0

alors erreur A988;
verif 1991:
application : iliad ;


si
   positif(FLAGDERNIE+0) = 1
   et
   positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)) = 1
   et
   positif(IDEGR + IREST) = 1
   et
   NAPCR61 > NAPCR61_A


alors erreur A991 ;
verif corrective 99310000:
application : iliad ;
si positif(4BACREC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 4BACREC;
si positif(4BACREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 4BACREP;
si positif(4BACREV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 4BACREV;
si positif(4BAHREC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 4BAHREC;
si positif(4BAHREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 4BAHREP;
si positif(4BAHREV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 4BAHREV;
si positif(ABDETPLUS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ABDETPLUS;
si positif(ABIMPMV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ABIMPMV;
si positif(ABIMPPV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ABIMPPV;
si positif(ABPVNOSURSIS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ABPVNOSURSIS;
si positif(ACODELAISINR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ACODELAISINR;
si positif(ALLECS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ALLECS;
si positif(ALLO1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ALLO1;
si positif(ALLO2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ALLO2;
si positif(ALLO3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ALLO3;
si positif(ALLO4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ALLO4;
si positif(ALLOC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ALLOC;
si positif(ALLOV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ALLOV;
si positif(ANOCEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ANOCEP;
si positif(ANOPEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ANOPEP;
si positif(ANOVEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ANOVEP;
si positif(ASCAPA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ASCAPA;
si positif(AUTOBICPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOBICPC;
si positif(AUTOBICPP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOBICPP;
si positif(AUTOBICPV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOBICPV;
si positif(AUTOBICVC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOBICVC;
si positif(AUTOBICVP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOBICVP;
si positif(AUTOBICVV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOBICVV;
si positif(AUTOBNCC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOBNCC;
si positif(AUTOBNCP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOBNCP;
si positif(AUTOBNCV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOBNCV;
si positif(AUTOVERSLIB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOVERSLIB;
si positif(AUTOVERSSUP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AUTOVERSSUP;
si positif(AVETRAN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 AVETRAN;
si positif(BA1AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BA1AC;
si positif(BA1AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BA1AP;
si positif(BA1AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BA1AV;
si positif(BACDEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BACDEC;
si positif(BACDEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BACDEP;
si positif(BACDEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BACDEV;
si positif(BACREC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BACREC;
si positif(BACREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BACREP;
si positif(BACREV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BACREV;
si positif(BAEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAEXC;
si positif(BAEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAEXP;
si positif(BAEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAEXV;
si positif(BAF1AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAF1AC;
si positif(BAF1AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAF1AP;
si positif(BAF1AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAF1AV;
si positif(BAFORESTC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAFORESTC;
si positif(BAFORESTP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAFORESTP;
si positif(BAFORESTV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAFORESTV;
si positif(BAFPVC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAFPVC;
si positif(BAFPVP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAFPVP;
si positif(BAFPVV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAFPVV;
si positif(BAHDEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAHDEC;
si positif(BAHDEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAHDEP;
si positif(BAHDEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAHDEV;
si positif(BAHEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAHEXC;
si positif(BAHEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAHEXP;
si positif(BAHEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAHEXV;
si positif(BAHREC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAHREC;
si positif(BAHREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAHREP;
si positif(BAHREV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAHREV;
si positif(BAILOC98 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAILOC98;
si positif(BANOCGAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BANOCGAC;
si positif(BANOCGAP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BANOCGAP;
si positif(BANOCGAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BANOCGAV;
si positif(BAPERPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAPERPC;
si positif(BAPERPP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAPERPP;
si positif(BAPERPV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BAPERPV;
si positif(BASRET ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BASRET;
si positif(BI1AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BI1AC;
si positif(BI1AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BI1AP;
si positif(BI1AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BI1AV;
si positif(BI2AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BI2AC;
si positif(BI2AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BI2AP;
si positif(BI2AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BI2AV;
si positif(BICDEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICDEC;
si positif(BICDEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICDEP;
si positif(BICDEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICDEV;
si positif(BICDNC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICDNC;
si positif(BICDNP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICDNP;
si positif(BICDNV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICDNV;
si positif(BICEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICEXC;
si positif(BICEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICEXP;
si positif(BICEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICEXV;
si positif(BICHDEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICHDEC;
si positif(BICHDEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICHDEP;
si positif(BICHDEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICHDEV;
si positif(BICHREC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICHREC;
si positif(BICHREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICHREP;
si positif(BICHREV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICHREV;
si positif(BICNOC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICNOC;
si positif(BICNOP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICNOP;
si positif(BICNOV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICNOV;
si positif(BICNPEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICNPEXC;
si positif(BICNPEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICNPEXP;
si positif(BICNPEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICNPEXV;
si positif(BICNPHEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICNPHEXC;
si positif(BICNPHEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICNPHEXP;
si positif(BICNPHEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICNPHEXV;
si positif(BICPMVCTC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICPMVCTC;
si positif(BICPMVCTP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICPMVCTP;
si positif(BICPMVCTV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICPMVCTV;
si positif(BICREC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICREC;
si positif(BICREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICREP;
si positif(BICREV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BICREV;
si positif(BIGREST ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIGREST;
si positif(BIHDNC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIHDNC;
si positif(BIHDNP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIHDNP;
si positif(BIHDNV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIHDNV;
si positif(BIHEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIHEXC;
si positif(BIHEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIHEXP;
si positif(BIHEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIHEXV;
si positif(BIHNOC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIHNOC;
si positif(BIHNOP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIHNOP;
si positif(BIHNOV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BIHNOV;
si positif(BN1AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BN1AC;
si positif(BN1AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BN1AP;
si positif(BN1AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BN1AV;
si positif(BNCAABC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCAABC;
si positif(BNCAABP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCAABP;
si positif(BNCAABV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCAABV;
si positif(BNCAADC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCAADC;
si positif(BNCAADP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCAADP;
si positif(BNCAADV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCAADV;
si positif(BNCCRC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCCRC;
si positif(BNCCRFC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCCRFC;
si positif(BNCCRFP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCCRFP;
si positif(BNCCRFV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCCRFV;
si positif(BNCCRP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCCRP;
si positif(BNCCRV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCCRV;
si positif(BNCDEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCDEC;
si positif(BNCDEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCDEP;
si positif(BNCDEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCDEV;
si positif(BNCEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCEXC;
si positif(BNCEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCEXP;
si positif(BNCEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCEXV;
si positif(BNCNP1AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNP1AC;
si positif(BNCNP1AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNP1AP;
si positif(BNCNP1AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNP1AV;
si positif(BNCNPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPC;
si positif(BNCNPDCT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPDCT;
si positif(BNCNPDEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPDEC;
si positif(BNCNPDEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPDEP;
si positif(BNCNPDEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPDEV;
si positif(BNCNPP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPP;
si positif(BNCNPPVC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPPVC;
si positif(BNCNPPVP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPPVP;
si positif(BNCNPPVV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPPVV;
si positif(BNCNPREXAAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPREXAAC;
si positif(BNCNPREXAAP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPREXAAP;
si positif(BNCNPREXAAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPREXAAV;
si positif(BNCNPREXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPREXC;
si positif(BNCNPREXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPREXP;
si positif(BNCNPREXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPREXV;
si positif(BNCNPV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCNPV;
si positif(BNCPMVCTC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPMVCTC;
si positif(BNCPMVCTP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPMVCTP;
si positif(BNCPMVCTV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPMVCTV;
si positif(BNCPRO1AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPRO1AC;
si positif(BNCPRO1AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPRO1AP;
si positif(BNCPRO1AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPRO1AV;
si positif(BNCPROC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPROC;
si positif(BNCPRODEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPRODEC;
si positif(BNCPRODEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPRODEP;
si positif(BNCPRODEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPRODEV;
si positif(BNCPROEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPROEXC;
si positif(BNCPROEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPROEXP;
si positif(BNCPROEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPROEXV;
si positif(BNCPROP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPROP;
si positif(BNCPROPVC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPROPVC;
si positif(BNCPROPVP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPROPVP;
si positif(BNCPROPVV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPROPVV;
si positif(BNCPROV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCPROV;
si positif(BNCREC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCREC;
si positif(BNCREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCREP;
si positif(BNCREV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNCREV;
si positif(BNHDEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNHDEC;
si positif(BNHDEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNHDEP;
si positif(BNHDEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNHDEV;
si positif(BNHEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNHEXC;
si positif(BNHEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNHEXP;
si positif(BNHEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNHEXV;
si positif(BNHREC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNHREC;
si positif(BNHREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNHREP;
si positif(BNHREV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BNHREV;
si positif(BPCOPTV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BPCOPTV;
si positif(BPCOSAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BPCOSAC;
si positif(BPCOSAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BPCOSAV;
si positif(BPV18V ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BPV18V;
si positif(BPV40V ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BPV40V;
si positif(BPVRCM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BPVRCM;
si positif(BPVSJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BPVSJ;
si positif(BPVSK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BPVSK;
si positif(BRAS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 BRAS;
si positif(CARPEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPEC;
si positif(CARPENBAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPENBAC;
si positif(CARPENBAP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPENBAP1;
si positif(CARPENBAP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPENBAP2;
si positif(CARPENBAP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPENBAP3;
si positif(CARPENBAP4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPENBAP4;
si positif(CARPENBAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPENBAV;
si positif(CARPEP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPEP1;
si positif(CARPEP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPEP2;
si positif(CARPEP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPEP3;
si positif(CARPEP4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPEP4;
si positif(CARPEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARPEV;
si positif(CARTSC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSC;
si positif(CARTSNBAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSNBAC;
si positif(CARTSNBAP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSNBAP1;
si positif(CARTSNBAP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSNBAP2;
si positif(CARTSNBAP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSNBAP3;
si positif(CARTSNBAP4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSNBAP4;
si positif(CARTSNBAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSNBAV;
si positif(CARTSP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSP1;
si positif(CARTSP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSP2;
si positif(CARTSP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSP3;
si positif(CARTSP4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSP4;
si positif(CARTSV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CARTSV;
si positif(CASECHR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CASECHR;
si positif(CASEPRETUD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CASEPRETUD;
si positif(CBETRAN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CBETRAN;
si positif(CELREPWT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPWT;
si positif(CELREPWU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPWU;
si positif(CELREPWV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPWV;
si positif(CELREPWW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPWW;
si positif(CELREPYM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPYM;
si positif(CELREPYN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPYN;
si positif(CELREPYO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPYO;
si positif(CELREPYP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPYP;
si positif(CELREPYT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPYT;
si positif(CELREPYU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPYU;
si positif(CELREPYV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPYV;
si positif(CELREPYW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELREPYW;
si positif(CELRREDLQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELRREDLQ;
si positif(CELRREDLR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELRREDLR;
si positif(CELRREDLU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELRREDLU;
si positif(CELRREDLV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CELRREDLV;
si positif(CESSASSC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CESSASSC;
si positif(CESSASSV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CESSASSV;
si positif(CHENF1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHENF1;
si positif(CHENF2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHENF2;
si positif(CHENF3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHENF3;
si positif(CHENF4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHENF4;
si positif(COD6GX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHENF4;
si positif(COD6EX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHENF4;
si positif(COD6GZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHENF4;
si positif(COD6EZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHENF4;
si positif(CHNFAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHNFAC;
si positif(COD6DG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6DG;
si positif(CHRDED ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHRDED;
si positif(CHRFAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CHRFAC;
si positif(CICORSENOW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CICORSENOW;
si positif(CIIMPPRO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CIIMPPRO;
si positif(CIIMPPRO2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CIIMPPRO2;
si positif(CIINVCORSE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CIINVCORSE;
si positif(CINE1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CINE1;
si positif(CINE2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CINE2;
si positif(CMAJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CMAJ;
si positif(CMAJ_ISF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CMAJ_ISF;
si positif(CO2044P ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CO2044P;
si positif(CO2047 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CO2047;
si positif(COD1AA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1AA;
si positif(COD1AE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1AE;
si positif(COD1AF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1AF;
si positif(COD1AG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1AG;
si positif(COD1AH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1AH;
si positif(COD1AI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1AI;
si positif(COD1AL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1AL;
si positif(COD1AM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1AM;
si positif(COD1AR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1AR;
si positif(COD1BA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1BA;
si positif(COD1BE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1BE;
si positif(COD1BF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1BF;
si positif(COD1BG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1BG;
si positif(COD1BH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1BH;
si positif(COD1BI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1BI;
si positif(COD1BL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1BL;
si positif(COD1BM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1BM;
si positif(COD1BR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1BR;
si positif(COD1CA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CA;
si positif(COD1CE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CE;
si positif(COD1CF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CF;
si positif(COD1CG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CG;
si positif(COD1CH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CH;
si positif(COD1CI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CI;
si positif(COD1CL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CL;
si positif(COD1CM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CM;
si positif(COD1CR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CR;
si positif(COD1CT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1CT;
si positif(COD1DA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DA;
si positif(COD1DE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DE;
si positif(COD1DF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DF;
si positif(COD1DG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DG;
si positif(COD1DH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DH;
si positif(COD1DI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DI;
si positif(COD1DL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DL;
si positif(COD1DM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DM;
si positif(COD1DR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DR;
si positif(COD1DT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1DT;
si positif(COD1EA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1EA;
si positif(COD1EE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1EE;
si positif(COD1EF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1EF;
si positif(COD1EG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1EG;
si positif(COD1EH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1EH;
si positif(COD1EI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1EI;
si positif(COD1EL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1EL;
si positif(COD1EM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1EM;
si positif(COD1ET ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1ET;
si positif(COD1FA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1FA;
si positif(COD1FE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1FE;
si positif(COD1FF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1FF;
si positif(COD1FG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1FG;
si positif(COD1FH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1FH;
si positif(COD1FI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1FI;
si positif(COD1FL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1FL;
si positif(COD1FM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1FM;
si positif(COD1FT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1FT;
si positif(COD1GA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GA;
si positif(COD1GB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GB;
si positif(COD1GE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GE;
si positif(COD1GF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GF;
si positif(COD1GG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GG;
si positif(COD1GH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GH;
si positif(COD1GK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GK;
si positif(COD1GL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GL;
si positif(COD1GP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GP;
si positif(COD1GQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GQ;
si positif(COD1GR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GR;
si positif(COD1GS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1GS;
si positif(COD1HA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HA;
si positif(COD1HB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HB;
si positif(COD1HE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HE;
si positif(COD1HF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HF;
si positif(COD1HG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HG;
si positif(COD1HH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HH;
si positif(COD1HK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HK;
si positif(COD1HL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HL;
si positif(COD1HP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HP;
si positif(COD1HQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HQ;
si positif(COD1HR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HR;
si positif(COD1HS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1HS;
si positif(COD1IA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1IA;
si positif(COD1IB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1IB;
si positif(COD1IE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1IE;
si positif(COD1IF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1IF;
si positif(COD1IG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1IG;
si positif(COD1IH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1IH;
si positif(COD1JA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1JA;
si positif(COD1JB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1JB;
si positif(COD1JE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1JE;
si positif(COD1JF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1JF;
si positif(COD1JG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1JG;
si positif(COD1JH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1JH;
si positif(COD1KA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1KA;
si positif(COD1KE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
              alors erreur A99301 COD1KE;
si positif(COD1KF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1KF;
si positif(COD1KG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1KG;
si positif(COD1KH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1KH;
si positif(COD1LA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1LA;
si positif(COD1LE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1LE;
si positif(COD1LF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1LF;
si positif(COD1LG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1LG;
si positif(COD1LH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1LH;
si positif(COD1NX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1NX;
si positif(COD1OX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1OX;
si positif(COD1PM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1PM;
si positif(COD1QM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1QM;
si positif(COD1TP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1TP;
si positif(COD1TZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1TZ;
si positif(COD1UP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1UP;
si positif(COD1UZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1UZ;
si positif(COD1VZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1VZ;
si positif(COD1WZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD1WZ;
si positif(COD2CK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2CK;
si positif(COD2DF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2DF;
si positif(COD2DG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2DG;
si positif(COD2DI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
              alors erreur A99301 COD2DI;
si positif(COD2OP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2OP;
si positif(COD2RA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2RA;
si positif(COD2RB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2RB;
si positif(COD2RC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2RC;
si positif(COD2RD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2RD;
si positif(COD2TQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2TQ;
si positif(COD2TT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2TT;
si positif(COD2TU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2TU;
si positif(COD2TV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2TV;
si positif(COD2TW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2TW;
si positif(COD2TX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2TX;
si positif(COD2UU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2UU;
si positif(COD2VM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2VM;
si positif(COD2VN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2VN;
si positif(COD2VO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2VO;
si positif(COD2VP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2VP;
si positif(COD2VQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2VQ;
si positif(COD2VR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2VR;
si positif(COD2VV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2VV;
si positif(COD2WW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2WW;
si positif(COD2XX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2XX;
si positif(COD2YY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2YY;
si positif(COD2ZZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2ZZ;
si positif(COD3AN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3AN;
si positif(COD3BN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3BN;
si positif(COD3PI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3PI;
si positif(COD3SA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3SA;
si positif(COD3SG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3SG;
si positif(COD3SL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3SL;
si positif(COD3SZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3SZ;
si positif(COD3TA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3TA;
si positif(COD3TB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3TB;
si positif(COD3TJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3TJ;
si positif(COD3TK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3TK;
si positif(COD3UA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3UA;
si positif(COD3WG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3WG;
si positif(COD3WI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3WI;
si positif(COD3WJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3WJ;
si positif(COD3WM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3WM;
si positif(COD3WN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3WN;
si positif(COD3WP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3WP;
si positif(COD3WR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3WR;
si positif(COD3WT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3WT;
si positif(COD3WZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3WZ;
si positif(COD3XA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3XA;
si positif(COD3XD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3XD;
si positif(COD3XM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3XM;
si positif(COD3XN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3XN;
si positif(COD3YA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD3YA;
si positif(COD4BK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD4BK;
si positif(COD4BL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD4BL;
si positif(COD4BN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD4BN;
si positif(COD5AD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AD;
si positif(COD5AF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AF;
si positif(COD5AH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AH;
si positif(COD5AI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AI;
si positif(COD5AK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AK;
si positif(COD5AL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AL;
si positif(COD5AN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AN;
si positif(COD5AO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AO;
si positif(COD5AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AP;
si positif(COD5AQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AQ;
si positif(COD5AR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AR;
si positif(COD5AY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AY;
si positif(COD5AZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5AZ;
si positif(COD5BD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BD;
si positif(COD5BF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BF;
si positif(COD5BH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BH;
si positif(COD5BI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BI;
si positif(COD5BK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BK;
si positif(COD5BL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BL;
si positif(COD5BN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BN;
si positif(COD5BO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BO;
si positif(COD5BP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BP;
si positif(COD5BQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BQ;
si positif(COD5BR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BR;
si positif(COD5BY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BY;
si positif(COD5BZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5BZ;
si positif(COD5CD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CD;
si positif(COD5CF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CF;
si positif(COD5CI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CI;
si positif(COD5CK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CK;
si positif(COD5CL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CL;
si positif(COD5CM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CM;
si positif(COD5CN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CN;
si positif(COD5CQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CQ;
si positif(COD5CR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CR;
si positif(COD5CU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CU;
si positif(COD5CV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CV;
si positif(COD5CY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CY;
si positif(COD5CZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5CZ;
si positif(COD5DB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5DB;
si positif(COD5DD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5DD;
si positif(COD5DF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5DF;
si positif(COD5DG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5DG;
si positif(COD5DK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5DK;
si positif(COD5DL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5DL;
si positif(COD5DM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5DM;
si positif(COD5DN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5DN;
si positif(COD5EA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EA;
si positif(COD5EB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EB;
si positif(COD5EC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EC;
si positif(COD5ED ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ED;
si positif(COD5EF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EF;
si positif(COD5EG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EG;
si positif(COD5EI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EI;
si positif(COD5EK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EK;
si positif(COD5EL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EL;
si positif(COD5EM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EM;
si positif(COD5EN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EN;
si positif(COD5EQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EQ;
si positif(COD5EU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EU;
si positif(COD5EV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EV;
si positif(COD5EY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EY;
si positif(COD5EZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5EZ;
si positif(COD5FB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FB;
si positif(COD5FD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FD;
si positif(COD5FF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FF;
si positif(COD5FG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FG;
si positif(COD5FK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FK;
si positif(COD5FL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FL;
si positif(COD5FM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FM;
si positif(COD5FN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FN;
si positif(COD5FY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FY;
si positif(COD5FZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5FZ;
si positif(COD5GY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5GY;
si positif(COD5GZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5GZ;
si positif(COD5HA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5HA;
si positif(COD5IA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5IA;
si positif(COD5JA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5JA;
si positif(COD5LD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5LD;
si positif(COD5MD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5MD;
si positif(COD5NW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5NW;
si positif(COD5OW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5OW;
si positif(COD5PW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5PW;
si positif(COD5QA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5QA;
si positif(COD5QJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5QJ;
si positif(COD5RA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5RA;
si positif(COD5RJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5RJ;
si positif(COD5RZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5RZ;
si positif(COD5SA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5SA;
si positif(COD5SJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5SJ;
si positif(COD5SZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5SZ;
si positif(COD5TF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5TF;
si positif(COD5TP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5TP;
si positif(COD5UF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5UF;
si positif(COD5UI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5UI;
si positif(COD5UP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5UP;
si positif(COD5UR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5UR;
si positif(COD5US ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5US;
si positif(COD5UT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5UT;
si positif(COD5UU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5UU;
si positif(COD5UY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5UY;
si positif(COD5UZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5UZ;
si positif(COD5VF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VF;
si positif(COD5VI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VI;
si positif(COD5VM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VM;
si positif(COD5VN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VN;
si positif(COD5VP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VP;
si positif(COD5VQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VQ;
si positif(COD5VR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VR;
si positif(COD5VS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VS;
si positif(COD5VT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VT;
si positif(COD5VU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VU;
si positif(COD5VV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VV;
si positif(COD5VW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VW;
si positif(COD5VX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VX;
si positif(COD5VY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VY;
si positif(COD5VZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5VZ;
si positif(COD5WI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5WI;
si positif(COD5WM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5WM;
si positif(COD5WN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5WN;
si positif(COD5WR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5WR;
si positif(COD5WS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5WS;
si positif(COD5XA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XA;
si positif(COD5XB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XB;
si positif(COD5XH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XH;
si positif(COD5XI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XI;
si positif(COD5XJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XJ;
si positif(COD5XK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XK;
si positif(COD5XL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XL;
si positif(COD5XM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XM;
si positif(COD5XN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XN;
si positif(COD5XO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XO;
si positif(COD5XP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XP;
si positif(COD5XQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XQ;
si positif(COD5XR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XR;
si positif(COD5XS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XS;
si positif(COD5XT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XT;
si positif(COD5XU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XU;
si positif(COD5XV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XV;
si positif(COD5XW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XW;
si positif(COD5XX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XX;
si positif(COD5XY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XY;
si positif(COD5XZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5XZ;
si positif(COD5YA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YA;
si positif(COD5YB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YB;
si positif(COD5YH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YH;
si positif(COD5YI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YI;
si positif(COD5YJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YJ;
si positif(COD5YK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YK;
si positif(COD5YL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YL;
si positif(COD5YM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YM;
si positif(COD5YN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YN;
si positif(COD5YO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YO;
si positif(COD5YP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YP;
si positif(COD5YQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YQ;
si positif(COD5YR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YR;
si positif(COD5YS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YS;
si positif(COD5YT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YT;
si positif(COD5YU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YU;
si positif(COD5YX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YX;
si positif(COD5YY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YY;
si positif(COD5YZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5YZ;
si positif(COD5ZA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZA;
si positif(COD5ZB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZB;
si positif(COD5ZH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZH;
si positif(COD5ZI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZI;
si positif(COD5ZJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZJ;
si positif(COD5ZK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZK;
si positif(COD5ZL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZL;
si positif(COD5ZM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZM;
si positif(COD5ZN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZN;
si positif(COD5ZO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZO;
si positif(COD5ZP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZP;
si positif(COD5ZQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZQ;
si positif(COD5ZR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZR;
si positif(COD5ZS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZS;
si positif(COD5ZT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZT;
si positif(COD5ZU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZU;
si positif(COD5ZW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZW;
si positif(COD5ZX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZX;
si positif(COD5ZY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZY;
si positif(COD5ZZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD5ZZ;
si positif(COD6HP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6HP;
si positif(COD6HQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6HQ;
si positif(COD6HR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6HR;
si positif(COD6NS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6NS;
si positif(COD6NT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6NT;
si positif(COD6NU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6NU;
si positif(COD6OS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6OS;
si positif(COD6OT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6OT;
si positif(COD6OU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6OU;
si positif(COD7AA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AA;
si positif(COD7BS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BS;
si positif(COD7BK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BK;
si positif(COD7BL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BL;
si positif(COD7BM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BM;
si positif(COD7BN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BN;
si positif(COD7BO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BO;
si positif(COD7CH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7CH;
si positif(COD7CI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7CI;
si positif(COD7CR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7CR;
si positif(COD7CS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7CS;
si positif(COD7CV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7CV;
si positif(COD7CX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7CX;
si positif(COD7CY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7CY;
si positif(COD7DY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7DY;
si positif(COD7EN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7EN;
si positif(COD7EY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7EY;
si positif(COD7FW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7FW;
si positif(COD7FX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7FX;
si positif(COD7FY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7FY;
si positif(COD7GW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7GW;
si positif(COD7GY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7GY;
si positif(COD7HO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HO;
si positif(COD7HP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HP;
si positif(COD7HQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HQ;
si positif(COD7HR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HR;
si positif(COD7HS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HS;
si positif(COD7LA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LA;
si positif(COD7LB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LB;
si positif(COD7LC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LC;
si positif(COD7LY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LY;
si positif(COD7MS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MS;
si positif(COD7MT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MT;
si positif(COD7MU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MU;
si positif(COD7MV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MV;
si positif(COD7MX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MX;
si positif(COD7MY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MY;
si positif(COD7NA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NA;
si positif(COD7NB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NB;
si positif(COD7NC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NC;
si positif(COD7ND ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ND;
si positif(COD7NX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NX;
si positif(COD7NY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NY;
si positif(COD7OF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OF;
si positif(COD7OG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OG;
si positif(COD7OH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OH;
si positif(COD7OI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OI;
si positif(COD7OJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OJ;
si positif(COD7OK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OK;
si positif(COD7OL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OL;
si positif(COD7OM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OM;
si positif(COD7ON ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ON;
si positif(COD7OO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OO;
si positif(COD7OP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OP;
si positif(COD7OQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OQ;
si positif(COD7OR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OR;
si positif(COD7OS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OS;
si positif(COD7OT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OT;
si positif(COD7OW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OW;
si positif(COD7OX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OX;
si positif(COD7OY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OY;
si positif(COD7OZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7OZ;
si positif(COD7PP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PP;
si positif(COD7PQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PQ;
si positif(COD7PR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PR;
si positif(COD7PS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PS;
si positif(COD7PT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PT;
si positif(COD7PU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PU;
si positif(COD7PV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PV;
si positif(COD7PW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PW;
si positif(COD7PX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PX;
si positif(COD7PY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PY;
si positif(COD7PZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PZ;
si positif(COD7QQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QQ;
si positif(COD7QV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QV;
si positif(COD7QW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QW;
si positif(COD7QX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QX;
si positif(COD7QY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QY;
si positif(COD7RA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RA;
si positif(COD7RB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RB;
si positif(COD7RC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RC;
si positif(COD7RD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RD;
si positif(COD7RE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RE;
si positif(COD7RF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RF;
si positif(COD7RG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RG;
si positif(COD7RH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RH;
si positif(COD7RT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RT;
si positif(COD7RU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RU;
si positif(COD7SA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SA;
si positif(COD7SB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SB;
si positif(COD7SC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SC;
si positif(COD7SN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SN;
si positif(COD7SO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SO;
si positif(COD7SU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SU;
si positif(COD7TA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TA;
si positif(COD7TB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TB;
si positif(COD7TE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TE;
si positif(COD7TF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TF;
si positif(COD7TK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TK;
si positif(COD7TM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TM;
si positif(COD7TO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TO;
si positif(COD7TP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TP;
si positif(COD7TQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TQ;
si positif(COD7TR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TR;
si positif(COD7TS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TS;
si positif(COD7TT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TT;
si positif(COD7TU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TU;
si positif(COD7TV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TV;
si positif(COD7TW) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TW;
si positif(COD7TX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TX;
si positif(COD7TY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TY;
si positif(COD7UA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UA;
si positif(COD7UB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UB;
si positif(COD7UH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UH;
si positif(COD7UI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UI;
si positif(COD7VH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VH;
si positif(COD7VI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VI;
si positif(COD7VJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VJ;
si positif(COD7VK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VK;
si positif(COD7VM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VM;
si positif(COD7VN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VN;
si positif(COD7VQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VQ;
si positif(COD7VR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VR;
si positif(COD7WH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WH;
si positif(COD7WI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WI;
si positif(COD7WK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WK;
si positif(COD7WQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WQ;
si positif(COD7WS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WS;
si positif(COD7XO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XO;
si positif(COD7XP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XP;
si positif(COD7XQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XQ;
si positif(COD7XX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XX;
si positif(COD7YE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YE;
si positif(COD7YI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YI;
si positif(COD7YJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YJ;
si positif(COD7YK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YK;
si positif(COD7YL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YL;
si positif(COD7ZO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZO;
si positif(COD7ZP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZP;
si positif(COD7ZQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZQ;
si positif(COD7ZR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZR;
si positif(COD7ZS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZS;
si positif(COD7ZT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZT;
si positif(COD7ZU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZU;
si positif(COD7ZV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZV;
si positif(COD7ZW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZW;
si positif(COD7ZX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZX;
si positif(COD7ZY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZY;
si positif(COD7ZZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZZ;
si positif(COD8AA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AA;
si positif(COD8AB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AB;
si positif(COD8AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AC;
si positif(COD8AD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AD;
si positif(COD8AE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AE;
si positif(COD8AF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AF;
si positif(COD8AG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AG;
si positif(COD8AH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AH;
si positif(COD8AI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AI;
si positif(COD8AJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AJ;
si positif(COD8AK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AK;
si positif(COD8AL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AL;
si positif(COD8AM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AM;
si positif(COD8AN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AN;
si positif(COD8AO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AO;
si positif(COD8AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AP;
si positif(COD8AQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AQ;
si positif(COD8AR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AR;
si positif(COD8AS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AS;
si positif(COD8AT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AT;
si positif(COD8AU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AU;
si positif(COD8AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AV;
si positif(COD8AW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AW;
si positif(COD8AX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AX;
si positif(COD8AY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AY;
si positif(COD8AZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8AZ;
si positif(COD8BA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8BA;
si positif(COD8BB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8BB;
si positif(COD8BC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8BC;
si positif(COD8BD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8BD;
si positif(COD8EA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8EA;
si positif(COD8EB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8EB;
si positif(COD8HV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8HV;
si positif(COD8HW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8HW;
si positif(COD8HX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8HX;
si positif(COD8HY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8HY;
si positif(COD8HZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8HZ;
si positif(COD8IE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8IE;
si positif(COD8IF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8IF;
si positif(COD8IV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8IV;
si positif(COD8IW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8IW;
si positif(COD8IX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8IX;
si positif(COD8IY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8IY;
si positif(COD8IZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8IZ;
si positif(COD8JV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8JV;
si positif(COD8JW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8JW;
si positif(COD8JX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8JX;
si positif(COD8JY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8JY;
si positif(COD8JZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8JZ;
si positif(COD8KV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8KV;
si positif(COD8KW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8KW;
si positif(COD8KX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8KX;
si positif(COD8KY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8KY;
si positif(COD8KZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8KZ;
si positif(COD8LG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LG;
si positif(COD8LH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LH;
si positif(COD8LI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LI;
si positif(COD8LJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LJ;
si positif(COD8LK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LK;
si positif(COD8LL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LL;
si positif(COD8LV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LV;
si positif(COD8LW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LW;
si positif(COD8LX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LX;
si positif(COD8LY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LY;
si positif(COD8LZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8LZ;
si positif(COD8MM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8MM;
si positif(COD8MT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8MT;
si positif(COD8MV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8MV;
si positif(COD8MW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8MW;
si positif(COD8MX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8MX;
si positif(COD8MY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8MY;
si positif(COD8MZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8MZ;
si positif(COD8NA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8NA;
si positif(COD8NB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8NB;
si positif(COD8OU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8OU;
si positif(COD8OV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8OV;
si positif(COD8PA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8PA;
si positif(COD8PB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8PB;
si positif(COD8PC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8PC;
si positif(COD8PF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8PF;
si positif(COD8PV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8PV;
si positif(COD8RC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8RC;
si positif(COD8RF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8RF;
si positif(COD8RM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8RM;
si positif(COD8RP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8RP;
si positif(COD8RQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8RQ;
si positif(COD8RV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8RV;
si positif(COD8SA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8SA;
si positif(COD8SB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8SB;
si positif(COD8SC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8SC;
si positif(COD8SD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8SD;
si positif(COD8SH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8SH;
si positif(COD8SI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8SI;
si positif(COD8SW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8SW;
si positif(COD8SX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8SX;
si positif(COD8TH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8TH;
si positif(COD8TL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8TL;
si positif(COD8UM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8UM;
si positif(COD8UW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8UW;
si positif(COD8VL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8VL;
si positif(COD8VM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8VM;
si positif(COD8WM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8WM;
si positif(COD8XF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XF;
si positif(COD8XG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XG;
si positif(COD8XH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XH;
si positif(COD8XI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XI;
si positif(COD8XJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XJ;
si positif(COD8XK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XK;
si positif(COD8XL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XL;
si positif(COD8XM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XM;
si positif(COD8XN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XN;
si positif(COD8XO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XO;
si positif(COD8XV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XV;
si positif(COD8XX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XX;
si positif(COD8XY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8XY;
si positif(COD8YJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8YJ;
si positif(COD8YK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8YK;
si positif(COD8YM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8YM;
si positif(COD8YQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8YQ;
si positif(COD8ZH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8ZH;
si positif(COD9AA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9AA;
si positif(COD9AB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9AB;
si positif(COD9AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9AC;
si positif(COD9AD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9AD;
si positif(COD9AE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9AE;
si positif(COD9BA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9BA;
si positif(COD9BB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9BB;
si positif(COD9BZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9BZ;
si positif(COD9CA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9CA;
si positif(COD9GF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9GF;
si positif(COD9GH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9GH;
si positif(COD9GL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9GL;
si positif(COD9GM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9GM;
si positif(COD9GN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9GN;
si positif(COD9GY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9GY;
si positif(COD9NC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9NC;
si positif(COD9NG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9NG;
si positif(COD9PR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9PR;
si positif(COD9PX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9PX;
si positif(COD9RS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9RS;
si positif(COD9ZA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD9ZA;
si positif(CODBIS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODBIS;
si positif(CODCCI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCCI;
si positif(CODCDI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCDI;
si positif(CODCHA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCHA;
si positif(CODCJG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCJG;
si positif(CODCKC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCKC;
si positif(CODCKI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCKI;
si positif(CODCLC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCLC;
si positif(CODCLI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCLI;
si positif(CODCMC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCMC;
si positif(CODCMI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCMI;
si positif(CODCNC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCNC;
si positif(CODCNI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCNI;
si positif(CODCNS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCNS;
si positif(CODCOA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOA;
si positif(CODCOB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOB;
si positif(CODCOC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOC;
si positif(CODCOD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOD;
si positif(CODCOE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOE;
si positif(CODCOF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOF;
si positif(CODCOG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOG;
si positif(CODCOH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOH;
si positif(CODCOI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOI;
si positif(CODCOJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOJ;
si positif(CODCOK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOK;
si positif(CODCOL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOL;
si positif(CODCOM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOM;
si positif(CODCON ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCON;
si positif(CODCOO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOO;
si positif(CODCOP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOP;
si positif(CODCOQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOQ;
si positif(CODCOR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOR;
si positif(CODCOS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOS;
si positif(CODCOT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOT;
si positif(CODCOU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOU;
si positif(CODCOV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOV;
si positif(CODCOX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCOX;
si positif(CODCPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCPC;
si positif(CODCPI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCPI;
si positif(CODCQC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCQC;
si positif(CODCQI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCQI;
si positif(CODCRC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCRC;
si positif(CODCRF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCRF;
si positif(CODCRI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCRI;
si positif(CODCSC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCSC;
si positif(CODCSF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCSF;
si positif(CODCSI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCSI;
si positif(CODCSN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODCSN;
si positif(CODDAJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODDAJ;
si positif(CODDBJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODDBJ;
si positif(CODEAJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODEAJ;
si positif(CODEBJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODEBJ;
si positif(CODHDI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDI;
si positif(CODHDJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDJ;
si positif(CODHDK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDK;
si positif(CODHDM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDM;
si positif(CODHDN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDN;
si positif(CODHDO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDO;
si positif(CODHDP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDP;
si positif(CODHDR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDR;
si positif(CODHDS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDS;
si positif(CODHDT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDT;
si positif(CODHDU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDU;
si positif(CODHDW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHDW;
si positif(CODHEN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHEN;
si positif(CODHEO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHEO;
si positif(CODHEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHEP;
si positif(CODHER ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHER;
si positif(CODHES ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHES;
si positif(CODHET ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHET;
si positif(CODHEU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHEU;
si positif(CODHEW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHEW;
si positif(CODHFN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHFN;
si positif(CODHFO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHFO;
si positif(CODHFP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHFP;
si positif(CODHFR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHFR;
si positif(CODHFS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHFS;
si positif(CODHFT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHFT;
si positif(CODHFU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHFU;
si positif(CODHFW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHFW;
si positif(CODHHC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHHC;
si positif(CODHIC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHIC;
si positif(CODHJA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHJA;
si positif(CODHJC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHJC;
si positif(CODHKC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHKC;
si positif(CODHOD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHOD;
si positif(CODHOE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHOE;
si positif(CODHOF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHOF;
si positif(CODHOG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHOG;
si positif(CODHOX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHOX;
si positif(CODHOY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHOY;
si positif(CODHOZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHOZ;
si positif(CODHUA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUA;
si positif(CODHUB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUB;
si positif(CODHUC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUC;
si positif(CODHUD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUD;
si positif(CODHUE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUE;
si positif(CODHUF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUF;
si positif(CODHUG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUG;
si positif(CODHUH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUH;
si positif(CODHUI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUI;
si positif(CODHUJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUJ;
si positif(CODHUK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUK;
si positif(CODHUL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUL;
si positif(CODHUM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUM;
si positif(CODHUN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUN;
si positif(CODHUO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUO;
si positif(CODHUP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUP;
si positif(CODHUQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUQ;
si positif(CODHUR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUR;
si positif(CODHUS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUS;
si positif(CODHUT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUT;
si positif(CODHUU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHUU;
si positif(CODHVA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVA;
si positif(CODHVB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVB;
si positif(CODHVC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVC;
si positif(CODHVD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVD;
si positif(CODHVE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVE;
si positif(CODHVF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVF;
si positif(CODHVG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVG;
si positif(CODHVH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVH;
si positif(CODHVI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVI;
si positif(CODHXQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHXQ;
si positif(CODHXR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHXR;
si positif(CODHXS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHXS;
si positif(CODHXT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHXT;
si positif(CODHXU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHXU;
si positif(CODHYA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHYA;
si positif(CODHYB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHYB;
si positif(CODHYC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHYC;
si positif(CODHYD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHYD;
si positif(CODMODUL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODMODUL;
si positif(CODNAF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNAF;
si positif(CODNAG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNAG;
si positif(CODNAL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNAL;
si positif(CODNAM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNAM;
si positif(CODNAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNAR;
si positif(CODNAZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNAZ;
si positif(CODNBE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNBE;
si positif(CODNBF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNBF;
si positif(CODNBG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNBG;
si positif(CODNBL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNBL;
si positif(CODNBM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNBM;
si positif(CODNBR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNBR;
si positif(CODNBZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNBZ;
si positif(CODNCF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNCF;
si positif(CODNCG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNCG;
si positif(CODNCL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNCL;
si positif(CODNCM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNCM;
si positif(CODNCR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNCR;
si positif(CODNCZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNCZ;
si positif(CODNDF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNDF;
si positif(CODNDG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNDG;
si positif(CODNDL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNDL;
si positif(CODNDM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNDM;
si positif(CODNDR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNDR;
si positif(CODNDZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNDZ;
si positif(CODNEF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNEF;
si positif(CODNEL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNEL;
si positif(CODNEM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNEM;
si positif(CODNEZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNEZ;
si positif(CODNFF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNFF;
si positif(CODNFG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNFG;
si positif(CODNFL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNFL;
si positif(CODNFM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNFM;
si positif(CODNFZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNFZ;
si positif(CODNGG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNGG;
si positif(CODNUA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNUA;
si positif(CODNVG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNVG;
si positif(CODNWA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNWA;
si positif(CODNWB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODNWB;
si positif(CODRAF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRAF;
si positif(CODRAG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRAG;
si positif(CODRAL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRAL;
si positif(CODRAM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRAM;
si positif(CODRAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRAR;
si positif(CODRAZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRAZ;
si positif(CODRBE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRBE;
si positif(CODRBF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRBF;
si positif(CODRBG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRBG;
si positif(CODRBK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRBK;
si positif(CODRBL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRBL;
si positif(CODRBM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRBM;
si positif(CODRBR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRBR;
si positif(CODRBT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRBT;
si positif(CODRBZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRBZ;
si positif(CODRCF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRCF;
si positif(CODRCG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRCG;
si positif(CODRCI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRCI;
si positif(CODRCL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRCL;
si positif(CODRCM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRCM;
si positif(CODRCR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRCR;
si positif(CODRCZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRCZ;
si positif(CODRDF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRDF;
si positif(CODRDG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRDG;
si positif(CODRDI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRDI;
si positif(CODRDL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRDL;
si positif(CODRDM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRDM;
si positif(CODRDR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRDR;
si positif(CODRDZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRDZ;
si positif(CODREF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODREF;
si positif(CODREL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODREL;
si positif(CODREM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODREM;
si positif(CODREZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODREZ;
si positif(CODRFF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRFF;
si positif(CODRFG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRFG;
si positif(CODRFL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRFL;
si positif(CODRFM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRFM;
si positif(CODRFZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRFZ;
si positif(CODRGG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRGG;
si positif(CODRSG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRSG;
si positif(CODRSL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRSL;
si positif(CODRUA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRUA;
si positif(CODRVA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRVA;
si positif(CODRVG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRVG;
si positif(CODRWA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRWA;
si positif(CODRWB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODRWB;
si positif(CODSAA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAA;
si positif(CODSAB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAB;
si positif(CODSAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAC;
si positif(CODSAD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAD;
si positif(CODSAE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAE;
si positif(CODSAF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAF;
si positif(CODSAG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAG;
si positif(CODSAH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAH;
si positif(CODSAI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAI;
si positif(CODSAJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAJ;
si positif(CODSAK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAK;
si positif(CODSAL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAL;
si positif(CODSAM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAM;
si positif(CODSAN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAN;
si positif(CODSAO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAO;
si positif(CODSAP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAP;
si positif(CODSAQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAQ;
si positif(CODSAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAR;
si positif(CODSAS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAS;
si positif(CODSAT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAT;
si positif(CODSAU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAU;
si positif(CODSAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAV;
si positif(CODSAW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAW;
si positif(CODSAX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAX;
si positif(CODSAY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAY;
si positif(CODSAZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSAZ;
si positif(CODSCA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCA;
si positif(CODSCB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCB;
si positif(CODSCC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCC;
si positif(CODSCD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCD;
si positif(CODSCE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCE;
si positif(CODSCF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCF;
si positif(CODSCG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCG;
si positif(CODSCH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCH;
si positif(CODSCI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCI;
si positif(CODSCJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCJ;
si positif(CODSCK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCK;
si positif(CODSCL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCL;
si positif(CODSCM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCM;
si positif(CODSCN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCN;
si positif(CODSCO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSCO;
si positif(CODSDA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDA;
si positif(CODSDB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDB;
si positif(CODSDC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDC;
si positif(CODSDD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDD;
si positif(CODSDE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDE;
si positif(CODSDF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDF;
si positif(CODSDG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDG;
si positif(CODSDH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDH;
si positif(CODSDI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDI;
si positif(CODSDJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDJ;
si positif(CODSDK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDK;
si positif(CODSDL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDL;
si positif(CODSDM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDM;
si positif(CODSDN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDN;
si positif(CODSDO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDO;
si positif(CODSDP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDP;
si positif(CODSDQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSDQ;
si positif(CODSIR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSIR;
si positif(CODSN1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSN1;
si positif(CODSN2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSN2;
si positif(CODSN3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSN3;
si positif(CODSN4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSN4;
si positif(CODSN5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSN5;
si positif(CODSN6 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSN6;
si positif(CODSN7 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODSN7;
si positif(CODZRA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODZRA;
si positif(CODZRB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODZRB;
si positif(CODZRE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODZRE;
si positif(CODZRF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODZRF;
si positif(CODZRS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODZRS;
si positif(CODZRU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODZRU;
si positif(CODZRV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODZRV;
si positif(COTF1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COTF1;
si positif(COTF2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COTF2;
si positif(COTF3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COTF3;
si positif(COTF4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COTF4;
si positif(COTFC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COTFC;
si positif(COTFORET ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COTFORET;
si positif(COTFV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COTFV;
si positif(CREAGRIBIO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CREAGRIBIO;
si positif(CREAIDE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CREAIDE;
si positif(CREARTS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CREARTS;
si positif(CRECONGAGRI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CRECONGAGRI;
si positif(CREDPVREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CREDPVREP;
si positif(CREFAM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CREFAM;
si positif(CREFORMCHENT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CREFORMCHENT;
si positif(CVNSALAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CVNSALAV;
si positif(DABNCNP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DABNCNP1;
si positif(DABNCNP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DABNCNP2;
si positif(DABNCNP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DABNCNP3;
si positif(DABNCNP4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DABNCNP4;
si positif(DABNCNP5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DABNCNP5;
si positif(DABNCNP6 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DABNCNP6;
si positif(DAGRI1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DAGRI1;
si positif(DAGRI2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DAGRI2;
si positif(DAGRI3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DAGRI3;
si positif(DAGRI4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DAGRI4;
si positif(DAGRI5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DAGRI5;
si positif(DAGRI6 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DAGRI6;
si positif(DATOCEANS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DATOCEANS;
si positif(DCSG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DCSG;
si positif(DEFAA0 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFAA0;
si positif(DEFAA1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFAA1;
si positif(DEFAA2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFAA2;
si positif(DEFAA3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFAA3;
si positif(DEFAA4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFAA4;
si positif(DEFAA5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFAA5;
si positif(DEFBIC1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFBIC1;
si positif(DEFBIC2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFBIC2;
si positif(DEFBIC3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFBIC3;
si positif(DEFBIC4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFBIC4;
si positif(DEFBIC5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFBIC5;
si positif(DEFBIC6 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFBIC6;
si positif(DEFRCM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFRCM;
si positif(DEFRCM2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFRCM2;
si positif(DEFRCM3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFRCM3;
si positif(DEFRCM4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFRCM4;
si positif(DEFRCM5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFRCM5;
si positif(DEFRCM6 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFRCM6;
si positif(DEFZU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DEFZU;
si positif(DISQUO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DISQUO;
si positif(DISQUONB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DISQUONB;
si positif(DNOCEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DNOCEP;
si positif(DNOCEPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DNOCEPC;
si positif(DNOCEPP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DNOCEPP;
si positif(DONAUTRE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DONAUTRE;
si positif(DONETRAN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DONETRAN;
si positif(DPVRCM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DPVRCM;
si positif(DUFLOFK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DUFLOFK;
si positif(DUFLOFR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DUFLOFR;
si positif(DUFLOFV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 DUFLOFV;
si positif(ESFP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 ESFP;
si positif(EXOCETC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 EXOCETC;
si positif(EXOCETV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 EXOCETV;
si positif(FCPI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FCPI;
si positif(FFIP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FFIP;
si positif(FIPCORSE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FIPCORSE;
si positif(FIPDOMCOM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FIPDOMCOM;
si positif(FONCI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FONCI;
si positif(FONCINB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FONCINB;
si positif(FORET ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FORET;
si positif(FRN1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FRN1;
si positif(FRN2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FRN2;
si positif(FRN3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FRN3;
si positif(FRN4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FRN4;
si positif(FRNC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FRNC;
si positif(FRNV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 FRNV;
si positif(GAINABDET ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 GAINABDET;
si positif(GAINPEA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 GAINPEA;
si positif(GLDGRATC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 GLDGRATC;
si positif(GLDGRATV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 GLDGRATV;
si positif(GSALC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 GSALC;
si positif(GSALV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 GSALV;
si positif(IMPRET ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IMPRET;
si positif(INAIDE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INAIDE;
si positif(INDECS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INDECS;
si positif(INDJNONIMPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INDJNONIMPC;
si positif(INDJNONIMPV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INDJNONIMPV;
si positif(INDPVSURSI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INDPVSURSI;
si positif(IND_TDR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IND_TDR;
si positif(INTDIFAGRI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INTDIFAGRI;
si positif(INTERE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INTERE;
si positif(INTERENB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INTERENB;
si positif(INVENTC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVENTC;
si positif(INVENTP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVENTP;
si positif(INVENTV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVENTV;
si positif(INVLGAUTRE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVLGAUTRE;
si positif(INVLGDEB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVLGDEB;
si positif(INVLGDEB2009 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVLGDEB2009;
si positif(INVLGDEB2010 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVLGDEB2010;
si positif(INVLOG2008 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVLOG2008;
si positif(INVLOG2009 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVLOG2009;
si positif(INVNPROF1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVNPROF1;
si positif(INVOMLOGOA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOA;
si positif(INVOMLOGOB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOB;
si positif(INVOMLOGOC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOC;
si positif(INVOMLOGOH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOH;
si positif(INVOMLOGOI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOI;
si positif(INVOMLOGOJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOJ;
si positif(INVOMLOGOK ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOK;
si positif(INVOMLOGOL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOL;
si positif(INVOMLOGOM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOM;
si positif(INVOMLOGON ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGON;
si positif(INVOMLOGOO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOO;
si positif(INVOMLOGOP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOP;
si positif(INVOMLOGOQ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOQ;
si positif(INVOMLOGOR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOR;
si positif(INVOMLOGOS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOS;
si positif(INVOMLOGOT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOT;
si positif(INVOMLOGOU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOU;
si positif(INVOMLOGOV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOV;
si positif(INVOMLOGOW ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 INVOMLOGOW;
si positif(IPBOCH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPBOCH;
si positif(IPCHER ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPCHER;
si positif(IPMOND ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPMOND;
si positif(IPPNCS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPPNCS;
si positif(IPPRICORSE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPPRICORSE;
si positif(IPRECH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPRECH;
si positif(IPREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPREP;
si positif(IPREPCORSE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPREPCORSE;
si positif(IPSOUR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPSOUR;
si positif(IPSURSI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPSURSI;
si positif(IPTEFN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPTEFN;
si positif(IPTEFP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPTEFP;
si positif(IPTXMO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 IPTXMO;
si positif(LNPRODEF1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF1;
si positif(LNPRODEF10 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF10;
si positif(LNPRODEF2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF2;
si positif(LNPRODEF3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF3;
si positif(LNPRODEF4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF4;
si positif(LNPRODEF5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF5;
si positif(LNPRODEF6 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF6;
si positif(LNPRODEF7 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF7;
si positif(LNPRODEF8 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF8;
si positif(LNPRODEF9 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LNPRODEF9;
si positif(LOCDEFNPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCDEFNPC;
si positif(LOCDEFNPCGAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCDEFNPCGAC;
si positif(LOCDEFNPCGAPAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCDEFNPCGAPAC;
si positif(LOCDEFNPCGAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCDEFNPCGAV;
si positif(LOCDEFNPPAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCDEFNPPAC;
si positif(LOCDEFNPV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCDEFNPV;
si positif(LOCGITC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCGITC;
si positif(LOCGITCC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCGITCC;
si positif(LOCGITCP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCGITCP;
si positif(LOCGITCV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCGITCV;
si positif(LOCGITHCC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCGITHCC;
si positif(LOCGITHCP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCGITHCP;
si positif(LOCGITHCV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCGITHCV;
si positif(LOCGITP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCGITP;
si positif(LOCGITV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCGITV;
si positif(LOCMEUBID ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCMEUBID;
si positif(LOCMEUBII ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCMEUBII;
si positif(COD7JZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JZ;
si positif(LOCNPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCNPC;
si positif(LOCNPCGAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCNPCGAC;
si positif(LOCNPCGAPAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCNPCGAPAC;
si positif(LOCNPCGAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCNPCGAV;
si positif(LOCNPPAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCNPPAC;
si positif(LOCNPV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCNPV;
si positif(LOCRESINEUV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 LOCRESINEUV;
si positif(MIB1AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIB1AC;
si positif(MIB1AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIB1AP;
si positif(MIB1AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIB1AV;
si positif(MIBDEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBDEC;
si positif(MIBDEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBDEP;
si positif(MIBDEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBDEV;
si positif(MIBEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBEXC;
si positif(MIBEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBEXP;
si positif(MIBEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBEXV;
si positif(MIBGITEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBGITEC;
si positif(MIBGITEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBGITEP;
si positif(MIBGITEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBGITEV;
si positif(MIBMEUC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBMEUC;
si positif(MIBMEUP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBMEUP;
si positif(MIBMEUV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBMEUV;
si positif(MIBNP1AC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNP1AC;
si positif(MIBNP1AP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNP1AP;
si positif(MIBNP1AV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNP1AV;
si positif(MIBNPDCT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPDCT;
si positif(MIBNPDEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPDEC;
si positif(MIBNPDEP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPDEP;
si positif(MIBNPDEV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPDEV;
si positif(MIBNPEXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPEXC;
si positif(MIBNPEXP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPEXP;
si positif(MIBNPEXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPEXV;
si positif(MIBNPPRESC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPPRESC;
si positif(MIBNPPRESP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPPRESP;
si positif(MIBNPPRESV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPPRESV;
si positif(MIBNPPVC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPPVC;
si positif(MIBNPPVP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPPVP;
si positif(MIBNPPVV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPPVV;
si positif(MIBNPVENC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPVENC;
si positif(MIBNPVENP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPVENP;
si positif(MIBNPVENV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBNPVENV;
si positif(MIBPRESC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBPRESC;
si positif(MIBPRESP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBPRESP;
si positif(MIBPRESV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBPRESV;
si positif(MIBPVC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBPVC;
si positif(MIBPVP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBPVP;
si positif(MIBPVV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBPVV;
si positif(MIBVENC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBVENC;
si positif(MIBVENP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBVENP;
si positif(MIBVENV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MIBVENV;
si positif(MOISAN ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MOISAN;
si positif(MOISAN_ISF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 MOISAN_ISF;
si positif(NBACT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 NBACT;
si positif(NCHENF1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 NCHENF1;
si positif(NCHENF2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 NCHENF2;
si positif(NCHENF3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 NCHENF3;
si positif(NCHENF4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 NCHENF4;
si positif(NRBASE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 NRBASE;
si positif(NRINET ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 NRINET;
si positif(PAAP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PAAP;
si positif(PAAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PAAV;
si positif(PALI1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PALI1;
si positif(PALI2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PALI2;
si positif(PALI3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PALI3;
si positif(PALI4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PALI4;
si positif(PALIC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PALIC;
si positif(PALIV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PALIV;
si positif(PCAPTAXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PCAPTAXC;
si positif(PCAPTAXV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PCAPTAXV;
si positif(PEBF1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PEBF1;
si positif(PEBF2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PEBF2;
si positif(PEBF3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PEBF3;
si positif(PEBF4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PEBF4;
si positif(PEBFC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PEBFC;
si positif(PEBFV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PEBFV;
si positif(PENECS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENECS;
si positif(PENIN1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENIN1;
si positif(PENIN2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENIN2;
si positif(PENIN3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENIN3;
si positif(PENIN4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENIN4;
si positif(PENINC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENINC;
si positif(PENINV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENINV;
si positif(PENSALC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALC;
si positif(PENSALNBC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALNBC;
si positif(PENSALNBP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALNBP1;
si positif(PENSALNBP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALNBP2;
si positif(PENSALNBP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALNBP3;
si positif(PENSALNBP4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALNBP4;
si positif(PENSALNBV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALNBV;
si positif(PENSALP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALP1;
si positif(PENSALP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALP2;
si positif(PENSALP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALP3;
si positif(PENSALP4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALP4;
si positif(PENSALV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PENSALV;
si positif(PERPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPC;
si positif(PERPIMPATRIE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPIMPATRIE;
si positif(PERPMUTU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPMUTU;
si positif(PERPP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPP;
si positif(PERPPLAFCC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFCC;
si positif(PERPPLAFCP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFCP;
si positif(PERPPLAFCV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFCV;
si positif(PERPPLAFNUC1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFNUC1;
si positif(PERPPLAFNUC2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFNUC2;
si positif(PERPPLAFNUC3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFNUC3;
si positif(PERPPLAFNUP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFNUP1;
si positif(PERPPLAFNUP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFNUP2;
si positif(PERPPLAFNUP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFNUP3;
si positif(PERPPLAFNUV1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFNUV1;
si positif(PERPPLAFNUV2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFNUV2;
si positif(PERPPLAFNUV3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPPLAFNUV3;
si positif(PERPV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERPV;
si positif(PERP_COTC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERP_COTC;
si positif(PERP_COTP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERP_COTP;
si positif(PERP_COTV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PERP_COTV;
si positif(PINELBI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PINELBI;
si positif(PINELCZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PINELCZ;
si positif(PINELDI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PINELDI;
si positif(PINELEZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PINELEZ;
si positif(PINELRZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PINELRZ;
si positif(PINELTZ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PINELTZ;
si positif(PLAF_PERPC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PLAF_PERPC;
si positif(PLAF_PERPP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PLAF_PERPP;
si positif(PLAF_PERPV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PLAF_PERPV;
si positif(PPLIB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PPLIB;
si positif(PRBR1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRBR1;
si positif(PRBR2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRBR2;
si positif(PRBR3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRBR3;
si positif(PRBR4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRBR4;
si positif(PRBRC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRBRC;
si positif(PRBRV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRBRV;
si positif(PREHABT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PREHABT;
si positif(PREHABT2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PREHABT2;
si positif(PREHABTN2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PREHABTN2;
si positif(PREHABTVT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PREHABTVT;
si positif(PRELIBXT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRELIBXT;
si positif(PREMAIDE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PREMAIDE;
si positif(PREREV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PREREV;
si positif(PRESCOMP2000 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRESCOMP2000;
si positif(PRESCOMPJUGE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRESCOMPJUGE;
si positif(PRESINTER ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRESINTER;
si positif(PRETUD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRETUD;
si positif(PRETUDANT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRETUDANT;
si positif(PRODOM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PRODOM;
si positif(PROGUY ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PROGUY;
si positif(PROVIE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PROVIE;
si positif(PROVIENB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PROVIENB;
si positif(PVEXOSEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVEXOSEC;
si positif(PVIMMO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVIMMO;
si positif(PVIMPOS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVIMPOS;
si positif(PVINCE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVINCE;
si positif(PVINPE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVINPE;
si positif(PVINVE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVINVE;
si positif(PVMOBNR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVMOBNR;
si positif(PVREP8 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVREP8;
si positif(PVREPORT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVREPORT;
si positif(PVSOCC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVSOCC;
si positif(PVSOCV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVSOCV;
si positif(PVSURSI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVSURSI;
si positif(PVTAXSB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVTAXSB;
si positif(PVTITRESOC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 PVTITRESOC;
si positif(R1649 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 R1649;
si positif(RCCURE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCCURE;
si positif(RCMABD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMABD;
si positif(RCMAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMAV;
si positif(RCMAVFT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMAVFT;
si positif(RCMFR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMFR;
si positif(RCMHAB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMHAB;
si positif(RCMHAD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMHAD;
si positif(RCMIMPAT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMIMPAT;
si positif(RCMLIB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMLIB;
si positif(RCMRDS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMRDS;
si positif(RCMSOC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMSOC;
si positif(RCMTNC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCMTNC;
si positif(RCSC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCSC;
si positif(RCSP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCSP;
si positif(RCSV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RCSV;
si positif(RDCOM ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDCOM;
si positif(RDDOUP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDDOUP;
si positif(RDENL ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDENL;
si positif(RDENLQAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDENLQAR;
si positif(RDENS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDENS;
si positif(RDENSQAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDENSQAR;
si positif(RDENU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDENU;
si positif(RDENUQAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDENUQAR;
si positif(RDEQPAHA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDEQPAHA;
si positif(RDFOREST ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDFOREST;
si positif(RDFORESTGES ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDFORESTGES;
si positif(RDFORESTRA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDFORESTRA;
si positif(RDGARD1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDGARD1;
si positif(RDGARD1QAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDGARD1QAR;
si positif(RDGARD2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDGARD2;
si positif(RDGARD2QAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDGARD2QAR;
si positif(RDGARD3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDGARD3;
si positif(RDGARD3QAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDGARD3QAR;
si positif(RDGARD4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDGARD4;
si positif(RDGARD4QAR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDGARD4QAR;
si positif(RDMECENAT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDMECENAT;
si positif(RDPRESREPORT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDPRESREPORT;
si positif(RDREP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDREP;
si positif(RDRESU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDRESU;
si positif(RDSYCJ ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDSYCJ;
si positif(RDSYPP ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDSYPP;
si positif(RDSYVO ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDSYVO;
si positif(RDTECH ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RDTECH;
si positif(RE168 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RE168;
si positif(REAMOR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REAMOR;
si positif(REAMORNB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REAMORNB;
si positif(REGCI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REGCI;
si positif(REGPRIV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REGPRIV;
si positif(REMPLAC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLAC;
si positif(REMPLANBC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLANBC;
si positif(REMPLANBP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLANBP1;
si positif(REMPLANBP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLANBP2;
si positif(REMPLANBP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLANBP3;
si positif(REMPLANBP4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLANBP4;
si positif(REMPLANBV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLANBV;
si positif(REMPLAP1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLAP1;
si positif(REMPLAP2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLAP2;
si positif(REMPLAP3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLAP3;
si positif(REMPLAP4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLAP4;
si positif(REMPLAV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REMPLAV;
si positif(RENTAX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RENTAX;
si positif(RENTAX5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RENTAX5;
si positif(RENTAX6 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RENTAX6;
si positif(RENTAX7 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RENTAX7;
si positif(RENTAXNB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RENTAXNB;
si positif(RENTAXNB5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RENTAXNB5;
si positif(RENTAXNB6 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RENTAXNB6;
si positif(RENTAXNB7 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RENTAXNB7;
si positif(REPDON03 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPDON03;
si positif(REPDON04 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPDON04;
si positif(REPDON05 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPDON05;
si positif(REPDON06 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPDON06;
si positif(REPDON07 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPDON07;
si positif(REPGROREP12 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPGROREP12;
si positif(REPGROREP13 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPGROREP13;
si positif(REPGROREP14 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPGROREP14;
si positif(REPSINFOR5 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPSINFOR5;
si positif(REPSOF ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REPSOF;
si positif(RESIVIEU ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RESIVIEU;
si positif(RESTUC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RESTUC;
si positif(RESTUCNB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RESTUCNB;
si positif(REVACT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVACT;
si positif(REVACTNB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVACTNB;
si positif(REVCSXA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVCSXA;
si positif(REVCSXB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVCSXB;
si positif(REVCSXC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVCSXC;
si positif(REVCSXD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVCSXD;
si positif(REVCSXE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVCSXE;
si positif(REVFONC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVFONC;
si positif(REVMAR1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVMAR1;
si positif(REVMAR2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVMAR2;
si positif(REVMAR3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVMAR3;
si positif(REVPEA ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVPEA;
si positif(REVPEANB ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 REVPEANB;
si positif(RFDANT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFDANT;
si positif(RFDHIS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFDHIS;
si positif(RFDORD ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFDORD;
si positif(RFMIC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFMIC;
si positif(RFORDI ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFORDI;
si positif(RFRH1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFRH1;
si positif(RFRH2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFRH2;
si positif(RFRN1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFRN1;
si positif(RFRN2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFRN2;
si positif(RFROBOR ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RFROBOR;
si positif(RIRENOV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RIRENOV;
si positif(RISKTEC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RISKTEC;
si positif(RMOND ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RMOND;
si positif(RSOCREPRISE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RSOCREPRISE;
si positif(RVB1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RVB1;
si positif(RVB2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RVB2;
si positif(RVB3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RVB3;
si positif(RVB4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RVB4;
si positif(RVCURE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 RVCURE;
si positif(SALECS ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SALECS;
si positif(SALECSG ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SALECSG;
si positif(SALEXT1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SALEXT1;
si positif(SALEXT2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SALEXT2;
si positif(SALEXT3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SALEXT3;
si positif(SALEXT4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SALEXT4;
si positif(SALEXTC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SALEXTC;
si positif(SALEXTV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SALEXTV;
si positif(SINISFORET ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SINISFORET;
si positif(SUBSTITRENTE ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 SUBSTITRENTE;
si positif(TAX1649 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TAX1649;
si positif(TEFFHRC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TEFFHRC;
si positif(TREVEX ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TREVEX;
si positif(TSASSUC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TSASSUC;
si positif(TSASSUV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TSASSUV;
si positif(TSHALLO1 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TSHALLO1;
si positif(TSHALLO2 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TSHALLO2;
si positif(TSHALLO3 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TSHALLO3;
si positif(TSHALLO4 ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TSHALLO4;
si positif(TSHALLOC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TSHALLOC;
si positif(TSHALLOV ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 TSHALLOV;
si positif(V_8ZT ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 V_8ZT;
si positif(XETRANC ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 XETRANC;
si positif(XETRANV) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 XETRANV;
si positif(XSPENPC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 XSPENPC;
si positif(XSPENPP) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 XSPENPP;
si positif(XSPENPV) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 XSPENPV;
si positif(COD2VT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD2VT ;
si positif(CODBJS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODBJS ;
si positif(CODBKS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODBKS ;
si positif(COD7BS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BS ;
si positif(COD7HA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HA ;
si positif(COD7HD) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HD ;
si positif(COD7HE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HE ;
si positif(COD7HF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HF ;
si positif(COD7HG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HG ;
si positif(COD7HH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HH ;
si positif(COD7HJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HJ ;
si positif(COD7HK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HK ;
si positif(COD7HN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HN ;
si positif(COD7HY) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HY ;
si positif(COD7IA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IA ;
si positif(COD7IB) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IB ;
si positif(COD7IC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IC ;
si positif(COD7IE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IE ;
si positif(COD7IF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IF ;
si positif(COD7IG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IG ;
si positif(COD7IH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IH ;
si positif(COD7IK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IK ;
si positif(COD7IL) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IL ;
si positif(COD7IO) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IO ;
si positif(COD7IP) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IP ;
si positif(COD7IQ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7IQ ;
si positif(COD7JN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JN ;
si positif(COD7JO) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JO ;
si positif(COD7JP) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JP ;
si positif(COD7JQ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JQ ;
si positif(COD7JR) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JR ;
si positif(COD7JS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JS ;
si positif(COD7JT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JT ;
si positif(COD7JU) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JU ;
si positif(COD7KW) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KW ;
si positif(COD7LD) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LD ;
si positif(COD7LE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LE ;
si positif(COD7LF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LF ;
si positif(COD7LN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LN ;
si positif(COD7LT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LT ;
si positif(COD7LX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LX ;
si positif(COD7LZ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LZ ;
si positif(COD7MA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MA ;
si positif(COD7MB) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MB ;
si positif(COD7MC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MC ;
si positif(COD7MD) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MD ;
si positif(COD7MG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MG ;
si positif(COD7MH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MH ;
si positif(COD7MI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MI ;
si positif(COD7MJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MJ ;
si positif(COD7MK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MK ;
si positif(COD7ML) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ML ;
si positif(COD7MW) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MW ;
si positif(COD7NI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NI ;
si positif(COD7NJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NJ ;
si positif(COD7NK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NK ;
si positif(COD7NL) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NL ;
si positif(COD7PA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PA ;
si positif(COD7PC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PC ;
si positif(COD7PD) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PD ;
si positif(COD7PE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PE ;
si positif(COD7QI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QI ;
si positif(COD7QJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QJ ;
si positif(COD7QK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QK ;
si positif(COD7QL) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QL ;
si positif(COD7QM) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QM ;
si positif(COD7QN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QN ;
si positif(COD7QO) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QO ;
si positif(COD7QP) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QP ;
si positif(COD7JV) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JV ;
si positif(COD7JW) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JW ;
si positif(COD7JX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JX ;
si positif(COD7JY) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7JY ;
si positif(COD7RP) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RP ;
si positif(COD7RQ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RQ ;
si positif(COD7RI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RI ;
si positif(COD7RJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RJ ;
si positif(COD7UY) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UY ;
si positif(COD7UZ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UZ ;
si positif(COD7RX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RX ;
si positif(COD7RY) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RY ;
si positif(COD7NM) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NM ;
si positif(COD7NN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NN ;
si positif(COD7PF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PF ;
si positif(COD7PG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PG ;
si positif(COD7LG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LG ;
si positif(COD7LH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LH ;
si positif(COD7LI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LI ;
si positif(COD7LJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7LJ ;
si positif(COD7SD) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SD ;
si positif(COD7SE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SE ;
si positif(COD7SF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SF ;
si positif(COD7SG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SG ;
si positif(COD7SH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SH ;
si positif(COD7SI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SI ;
si positif(COD7SJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SJ ;
si positif(COD7SK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SK ;
si positif(COD7SL) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SL ;
si positif(COD7SM) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SM ;
si positif(COD7SQ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SQ ;
si positif(COD7SR) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SR ;
si positif(COD7SW) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SW ;
si positif(COD7SX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SX ;
si positif(COD7SY) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SY ;
si positif(COD7TA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TA ;
si positif(COD7TB) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7TB ;
si positif(COD7UG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UG ;
si positif(COD7UJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UJ ;
si positif(COD7UU) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UU ;
si positif(COD7UV) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UV ;
si positif(COD7UW) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UW ;
si positif(COD7UX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7UX ;
si positif(COD7VM) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VM ;
si positif(COD7VN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7VN ;
si positif(COD7XA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XA ;
si positif(COD7XB) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XB ;
si positif(COD7XC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XC ;
si positif(COD7XL) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XL ;
si positif(COD7XM) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XM ;
si positif(COD7XN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XN ;
si positif(COD7XR) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XR ;
si positif(COD7YA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YA ;
si positif(COD7YC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YC ;
si positif(COD7YG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YG ;
si positif(COD7YR) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YR ;
si positif(COD7YS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YS ;
si positif(COD7ZQ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZQ ;
si positif(COD7ZR) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZR ;
si positif(COD7ZS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZS ;
si positif(COD7ZT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZT ;
si positif(COD7ZU) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZU ;
si positif(COD7ZV) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZV ;
si positif(CODHHS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHHS ;
si positif(CODHHT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHHT ;
si positif(CODHHU) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHHU ;
si positif(CODHHW) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHHW ;
si positif(CODHVK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVK ;
si positif(CODHVL) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHVL ;
si positif(CODHYF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHYF ;
si positif(CODHYG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHYG ;
si positif(COD8UA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8UA ;
si positif(COD8UB) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8UB ;
si positif(COD8UC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8UC ;
si positif(COD8WG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8WG ;
si positif(COD8WH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8WH ;
si positif(COD8ZQ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8ZQ ;
si positif(COD8ZX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8ZX ;
si positif(COD6EX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6EX ;
si positif(COD6EZ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6EZ ;
si positif(COD6GX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6GX ;
si positif(COD6GZ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD6GZ ;
si positif(COD7AB) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AB ;
si positif(COD7AD) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AD ;
si positif(COD7AF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AF ;
si positif(COD7AH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AH ;
si positif(COD7AI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AI ;
si positif(COD7AP) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AP ;
si positif(COD7AR) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AR ;
si positif(COD7AS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AS ;
si positif(COD7AT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AT ;
si positif(COD7AU) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7AU ;
si positif(COD7BA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BA ;
si positif(COD7BB) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BB ;
si positif(COD7BC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BC ;
si positif(COD7BD) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BD ;
si positif(COD7BE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BE ;
si positif(COD7BF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BF ;
si positif(COD7BG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BG ;
si positif(COD7BH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BH ;
si positif(COD7BJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BJ ;
si positif(COD7BT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7BT ;
si positif(COD7CA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7CA ;
si positif(COD7CT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7CT ;
si positif(COD7DC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7DC ;
si positif(COD7EK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7EK ;
si positif(COD7FT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7FT ;
si positif(COD7GR) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7GR ;
si positif(COD7GS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7GS ;
si positif(COD7GU) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7GU ;
si positif(COD7GX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7GX ;
si positif(COD7HL) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HL ;
si positif(COD7HM) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HM ;
si positif(COD7HZ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7HZ ;
si positif(COD7KE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KE ;
si positif(COD7KF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KF ;
si positif(COD7KG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KG ;
si positif(COD7KH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KH ;
si positif(COD7KI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KI ;
si positif(COD7KJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KJ ;
si positif(COD7KL) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KL ;
si positif(COD7KN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KN ;
si positif(COD7KO) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KO ; 
si positif(COD7KQ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KQ ;
si positif(COD7KR) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KR ;
si positif(COD7KS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KS ;
si positif(COD7KT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KT ;
si positif(COD7KU) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KU ;
si positif(COD7KV) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KV ;
si positif(COD7KZ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7KZ ;
si positif(COD7MN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7MN ;
si positif(COD7NE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NE ;
si positif(COD7NF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NF ;
si positif(COD7NG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NG ;
si positif(COD7NH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7NH ;
si positif(COD7PB) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PB ;
si positif(COD7PI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PI ;
si positif(COD7PJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7PJ ;
si positif(COD7QA) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QA ;
si positif(COD7QB) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QB ;
si positif(COD7QC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QC ;
si positif(COD7QD) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QD ;
si positif(COD7QE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7QE ;
si positif(COD7RK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RK ;
si positif(COD7RL) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RL ;
si positif(COD7RM) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RM ;
si positif(COD7RN) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7RN ;
si positif(COD7SS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SS ;
si positif(COD7SV) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7SV ;
si positif(COD7WC) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WC ;
si positif(COD7WD) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WD ;
si positif(COD7WE) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WE ;
si positif(COD7WF) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WF ;
si positif(COD7WG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WG ;
si positif(COD7WX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WX ;
si positif(COD7WY) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WY ;
si positif(COD7WZ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7WZ ;
si positif(COD7XH) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XH ;
si positif(COD7XI) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XI ;
si positif(COD7XJ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XJ ;
si positif(COD7XK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XK ;
si positif(COD7XV) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7XV ;
si positif(COD7YX) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YX ;
si positif(COD7YY) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YY ;
si positif(COD7YZ) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7YZ ;
si positif(COD7ZM) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD7ZM ;
si positif(CODHIS) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHIS ;
si positif(CODHIT) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHIT ;
si positif(CODHIU) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHIU ;
si positif(CODHIV) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHIV ;
si positif(CODHIW) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 CODHIW ;
si positif(COD8SG) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8SG ;
si positif(COD8WK) = 1 et positif(ANNUL2042) = 1 et (APPLI_OCEANS + 0) < 1
               alors erreur A99301 COD8WK ;

verif 19941:
application : iliad ;

si 
   positif(PREM8_11 + positif(null(CMAJ_ISF-8) + null(CMAJ_ISF-11) +null(CMAJ_ISF-34))) = 1
   et
   positif(COD9ZA + 0) = 1

alors erreur A99401 ;
verif 19942:
application : iliad ;

si 
   positif(PREM8_11 + positif(null(CMAJ_ISF-8) + null(CMAJ_ISF-11) +null(CMAJ_ISF-34))) = 0
   et
   positif(COD9ZA + 0) = 1
   et
   PENA994 = 0

alors erreur A99402 ;
verif isf 1995:
application : iliad ;

si
(V_MODUL+0) < 1
  et
IFIPAT=0
et
positif((COD9NC)+(COD9NG)+(COD9PR)+(COD9PX)+(COD9RS)+(COD9GL)+(COD9GM)) > 0
et
present(ANNUL2042) = 0

alors erreur A995;
verif isf 1997:
application : iliad ;

si
(V_MODUL+0) < 1
     et
(V_IND_TRAIT = 4
et
present(COD9GN) = 1
et
IFIPAT <= LIM_IFIINF)
ou
(V_IND_TRAIT = 5
et
positif(COD9GN) = 1
et
IFIPAT <= LIM_IFIINF)

alors erreur A997 ; 
verif 101:
application :  iliad ;

si
  (V_MODUL+0) < 1
    et
   positif(APPLI_COLBERT + APPLI_ILIAD) = 1
   et
   V_ZDC + 0 = 0
   et
   V_0AC = 1
   et
   V_0AZ + 0 > 0

alors erreur AS0101 ;
verif 102:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   positif(APPLI_COLBERT + APPLI_ILIAD) = 1
   et
   V_ZDC + 0 = 0
   et
   BOOL_0AM = 1
   et
   V_0AX + 0 > 0
   et
   V_0AB + 0 > 0

alors erreur AS0102 ;
verif 103:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
  positif(APPLI_COLBERT + APPLI_ILIAD) = 1
   et
   V_ZDC + 0 = 0
   et
   V_0AC + V_0AD + V_0AV + 0 = 1
   et
   V_0AX + 0 > 0
   et
   positif(V_0AB + 0) = 0

alors erreur AS0103 ;
verif 104:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
  positif(APPLI_COLBERT + APPLI_ILIAD) = 1
   et
   V_ZDC + 0 = 0
   et
   BOOL_0AM = 1
   et
   V_0AY + 0 > 0

alors erreur AS0104 ;
verif 105:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
   positif(APPLI_COLBERT + APPLI_ILIAD) = 1
   et
   V_ZDC + 0 = 0
   et
   V_0AM = 1
   et
   V_0AY + 0 > 0
   et
   V_0AZ + 0 > 0

alors erreur AS0105 ;
verif 106:
application :  iliad ;

si
   (V_MODUL+0) < 1
     et
  positif(APPLI_COLBERT + APPLI_ILIAD) = 1
   et
   V_ZDC + 0 = 0
   et
   V_0AD = 1
   et
   V_0AZ + 0 > 0

alors erreur AS0106 ;
verif 107:
application : iliad  ;


si
   (V_MODUL+0) < 1
    et 
   (APPLI_OCEANS+APPLI_COLBERT + 0) < 1 et 
   (( pour un i dans 0, 1, 2, 3, 4, 5, 6, 7: V_0Fi + 0 > ANNEEREV  )
   ou
   ( pour un j dans G, J, N, H, I, P et un i dans 0, 1, 2, 3: V_0ji + 0 > ANNEEREV  ))
 ou (APPLI_COLBERT+APPLI_OCEANS=1) et
   (( pour un i dans 0, 1, 2, 3, 4, 5, 6, 7: V_0Fi + 0 > ANNEEREV  )
   ou
   ( pour un j dans 0, 1, 2, 3: V_0Hj + 0 > ANNEEREV  ))

alors erreur AS02;
verif 108:
application : iliad ;

si
   (V_MODUL+0) < 1
     et
   APPLI_COLBERT = 1
   et
   positif(V_IND_TRAIT + 0) = 1
   et
   V_NOTRAIT + 0 < 14
   et
   present(V_ANTIR) = 0
   et
   positif(V_0DA + 0) = 0

alors erreur AS11 ;

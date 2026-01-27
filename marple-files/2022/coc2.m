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
verif 11391:
application : iliad  ;


si
   ((V_IND_TRAIT = 4 )
     et
    (
     CARPENBAV < 2 ou CARPENBAV > 45
     ou
     CARPENBAC < 2 ou CARPENBAC > 45
     ou
     CARPENBAP1 < 2 ou CARPENBAP1 > 45
     ou
     CARPENBAP2 < 2 ou CARPENBAP2 > 45
     ou
     CARPENBAP3 < 2 ou CARPENBAP3 > 45
     ou
     CARPENBAP4 < 2 ou CARPENBAP4 > 45
     ou
     PENSALNBV < 2 ou PENSALNBV > 45
     ou
     PENSALNBC < 2 ou PENSALNBC > 45
     ou
     PENSALNBP1 < 2 ou PENSALNBP1 > 45
     ou
     PENSALNBP2 < 2 ou PENSALNBP2 > 45
     ou
     PENSALNBP3 < 2 ou PENSALNBP3 > 45
     ou
     PENSALNBP4 < 2 ou PENSALNBP4 > 45
     ou
     RENTAXNB < 2 ou RENTAXNB > 45
     ou
     RENTAXNB5 < 2 ou RENTAXNB5 > 45
     ou
     RENTAXNB6 < 2 ou RENTAXNB6 > 45
     ou
     RENTAXNB7 < 2 ou RENTAXNB7 > 45
     ou
     CODNAZ < 2 ou CODNAZ > 45
     ou
     CODNBZ < 2 ou CODNBZ > 45
     ou
     CODNCZ < 2 ou CODNCZ > 45
     ou
     CODNDZ < 2 ou CODNDZ > 45
     ou
     CODNEZ < 2 ou CODNEZ > 45
     ou
     CODNFZ < 2 ou CODNFZ > 45
     ou
     CODNAL < 2 ou CODNAL > 45
     ou
     CODNAM < 2 ou CODNAM > 45
     ou
     CODNBL < 2 ou CODNBL > 45
     ou
     CODNBM < 2 ou CODNBM > 45
     ou
     CODNCL < 2 ou CODNCL > 45
     ou
     CODNCM < 2 ou CODNCM > 45
     ou
     CODNDL < 2 ou CODNDL > 45
     ou
     CODNDM < 2 ou CODNDM > 45
     ou
     CODNEL < 2 ou CODNEL > 45
     ou
     CODNEM < 2 ou CODNEM > 45
     ou
     CODNFL < 2 ou CODNFL > 45
     ou
     CODNFM < 2 ou CODNFM > 45
     ou
     CODNAR < 2 ou CODNAR > 45
     ou
     CODNBR < 2 ou CODNBR > 45
     ou
     CODNCR < 2 ou CODNCR > 45
     ou
     CODNDR < 2 ou CODNDR > 45
     ou
     CODNAI < 2 ou CODNAI > 45
     ou
     CODNBI < 2 ou CODNBI > 45
     ou
     CODNCK < 2 ou CODNCK > 45
    )
   )
   ou
   ((V_IND_TRAIT = 5 )
     et
    (
     CARPENBAV = 1 ou CARPENBAV > 45
     ou
     CARPENBAC = 1 ou CARPENBAC > 45
     ou
     CARPENBAP1 = 1 ou CARPENBAP1 > 45
     ou
     CARPENBAP2 = 1 ou CARPENBAP2 > 45
     ou
     CARPENBAP3 = 1 ou CARPENBAP3 > 45
     ou
     CARPENBAP4 = 1 ou CARPENBAP4 > 45
     ou
     PENSALNBV = 1 ou PENSALNBV > 45
     ou
     PENSALNBC = 1 ou PENSALNBC > 45
     ou
     PENSALNBP1 = 1 ou PENSALNBP1 > 45
     ou
     PENSALNBP2 = 1 ou PENSALNBP2 > 45
     ou
     PENSALNBP3 = 1 ou PENSALNBP3 > 45
     ou
     PENSALNBP4 = 1 ou PENSALNBP4 > 45
     ou
     RENTAXNB = 1 ou RENTAXNB > 45
     ou
     RENTAXNB5 = 1 ou RENTAXNB5 > 45
     ou
     RENTAXNB6 = 1 ou RENTAXNB6 > 45
     ou
     RENTAXNB7 = 1 ou RENTAXNB7 > 45
     ou
     CODNAZ = 1 ou CODNAZ > 45
     ou
     CODNBZ = 1 ou CODNBZ > 45
     ou
     CODNCZ = 1 ou CODNCZ > 45
     ou
     CODNDZ = 1 ou CODNDZ > 45
     ou
     CODNEZ = 1 ou CODNEZ > 45
     ou
     CODNFZ = 1 ou CODNFZ > 45
     ou
     CODNAL = 1 ou CODNAL > 45
     ou
     CODNAM = 1 ou CODNAM > 45
     ou
     CODNBL = 1 ou CODNBL > 45
     ou
     CODNBM = 1 ou CODNBM > 45
     ou
     CODNCL = 1 ou CODNCL > 45
     ou
     CODNCM = 1 ou CODNCM > 45
     ou
     CODNDL = 1 ou CODNDL > 45
     ou
     CODNDM = 1 ou CODNDM > 45
     ou
     CODNEL = 1 ou CODNEL > 45
     ou
     CODNEM = 1 ou CODNEM > 45
     ou
     CODNFL = 1 ou CODNFL > 45
     ou
     CODNFM = 1 ou CODNFM > 45
     ou
     CODNAR = 1 ou CODNAR > 45
     ou
     CODNBR = 1 ou CODNBR > 45
     ou
     CODNCR = 1 ou CODNCR > 45
     ou
     CODNDR = 1 ou CODNDR > 45
     ou
     CODNAI = 1 ou CODNAI > 45
     ou
     CODNBI = 1 ou CODNBI > 45
     ou
     CODNCK = 1 ou CODNCK > 45
    )
   )
alors erreur A13901 ;
verif 11392:
application : iliad  ;


si
  (V_IND_TRAIT = 4
    et
    (
     (positif(CARPEV) + present(CARPENBAV) = 1)
     ou
     (positif(CARPEC) + present(CARPENBAC) = 1)
     ou
     (positif(CARPEP1) + present(CARPENBAP1) = 1)
     ou
     (positif(CARPEP2) + present(CARPENBAP2) = 1)
     ou
     (positif(CARPEP3) + present(CARPENBAP3) = 1)
     ou
     (positif(CARPEP4) + present(CARPENBAP4) = 1)
     ou
     (positif(PENSALV) + present(PENSALNBV) = 1)
     ou
     (positif(PENSALC) + present(PENSALNBC) = 1)
     ou
     (positif(PENSALP1) + present(PENSALNBP1) = 1)
     ou
     (positif(PENSALP2) + present(PENSALNBP2) = 1)
     ou
     (positif(PENSALP3) + present(PENSALNBP3) = 1)
     ou
     (positif(PENSALP4) + present(PENSALNBP4) = 1)
     ou
     (positif(RENTAX) + present(RENTAXNB) = 1)
     ou
     (positif(RENTAX5) + present(RENTAXNB5) = 1)
     ou
     (positif(RENTAX6) + present(RENTAXNB6) = 1)
     ou
     (positif(RENTAX7) + present(RENTAXNB7) = 1)
     ou
     (positif(CODRAZ) + present(CODNAZ) = 1)
     ou
     (positif(CODRBZ) + present(CODNBZ) = 1)
     ou
     (positif(CODRCZ) + present(CODNCZ) = 1)
     ou
     (positif(CODRDZ) + present(CODNDZ) = 1)
     ou
     (positif(CODREZ) + present(CODNEZ) = 1)
     ou
     (positif(CODRFZ) + present(CODNFZ) = 1)
     ou
     (positif(CODRAL) + present(CODNAL) = 1)
     ou
     (positif(CODRAM) + present(CODNAM) = 1)
     ou
     (positif(CODRBL) + present(CODNBL) = 1)
     ou
     (positif(CODRBM) + present(CODNBM) = 1)
     ou
     (positif(CODRCL) + present(CODNCL) = 1)
     ou
     (positif(CODRCM) + present(CODNCM) = 1)
     ou
     (positif(CODRDL) + present(CODNDL) = 1)
     ou
     (positif(CODRDM) + present(CODNDM) = 1)
     ou
     (positif(CODREL) + present(CODNEL) = 1)
     ou
     (positif(CODREM) + present(CODNEM) = 1)
     ou
     (positif(CODRFL) + present(CODNFL) = 1)
     ou
     (positif(CODRFM) + present(CODNFM) = 1)
     ou
     (positif(CODRAR) + present(CODNAR) = 1)
     ou
     (positif(CODRBR) + present(CODNBR) = 1)
     ou
     (positif(CODRCR) + present(CODNCR) = 1)
     ou
     (positif(CODRDR) + present(CODNDR) = 1)
     ou
     (positif(CODRAI) + present(CODNAI) = 1)
     ou
     (positif(CODRCK) + present(CODNCK) = 1)
     ou
     (positif(CODRBI) + present(CODNBI) = 1)

    )
  )
  ou
  (V_IND_TRAIT = 5
    et
    (
     (positif(CARPEV) + positif(CARPENBAV) = 1)
     ou
     (positif(CARPEC) + positif(CARPENBAC) = 1)
     ou
     (positif(CARPEP1) + positif(CARPENBAP1) = 1)
     ou
     (positif(CARPEP2) + positif(CARPENBAP2) = 1)
     ou
     (positif(CARPEP3) + positif(CARPENBAP3) = 1)
     ou
     (positif(CARPEP4) + positif(CARPENBAP4) = 1)
     ou
     (positif(PENSALV) + positif(PENSALNBV) = 1)
     ou
     (positif(PENSALC) + positif(PENSALNBC) = 1)
     ou
     (positif(PENSALP1) + positif(PENSALNBP1) = 1)
     ou
     (positif(PENSALP2) + positif(PENSALNBP2) = 1)
     ou
     (positif(PENSALP3) + positif(PENSALNBP3) = 1)
     ou
     (positif(PENSALP4) + positif(PENSALNBP4) = 1)
     ou
     (positif(RENTAX) + positif(RENTAXNB) = 1)
     ou
     (positif(RENTAX5) + positif(RENTAXNB5) = 1)
     ou
     (positif(RENTAX6) + positif(RENTAXNB6) = 1)
     ou
     (positif(RENTAX7) + positif(RENTAXNB7) = 1)
     ou
     (positif(CODRAZ) + positif(CODNAZ) = 1)
     ou
     (positif(CODRBZ) + positif(CODNBZ) = 1)
     ou
     (positif(CODRCZ) + positif(CODNCZ) = 1)
     ou
     (positif(CODRDZ) + positif(CODNDZ) = 1)
     ou
     (positif(CODREZ) + positif(CODNEZ) = 1)
     ou
    (positif(CODRFZ) + positif(CODNFZ) = 1)
    ou
    (positif(CODRAL) + positif(CODNAL) = 1)
    ou
    (positif(CODRAM) + positif(CODNAM) = 1)
    ou
    (positif(CODRBL) + positif(CODNBL) = 1)
    ou
    (positif(CODRBM) + positif(CODNBM) = 1)
    ou
   (positif(CODRCL) + positif(CODNCL) = 1)
    ou
   (positif(CODRCM) + positif(CODNCM) = 1)
    ou
   (positif(CODRDL) + positif(CODNDL) = 1)
   ou
   (positif(CODRDM) + positif(CODNDM) = 1)
   ou
   (positif(CODREL) + positif(CODNEL) = 1)
   ou
  (positif(CODREM) + positif(CODNEM) = 1)
   ou
   (positif(CODRFL) + positif(CODNFL) = 1)
   ou
  (positif(CODRFM) + positif(CODNFM) = 1)
   ou
   (positif(CODRAR) + positif(CODNAR) = 1)
   ou
   (positif(CODRBR) + positif(CODNBR) = 1)
   ou
   (positif(CODRCR) + positif(CODNCR) = 1)
   ou
   (positif(CODRDR) +positif(CODNDR) = 1)
   ou
   (positif(CODRAI) + positif(CODNAI) = 1)
   ou
   (positif(CODRCK) + positif(CODNCK) = 1)
   ou
   (positif(CODRBI) + positif(CODNBI) = 1)

    )
  )
alors erreur A13902 ;
verif 11401:
application : iliad  ;

si
   ((V_IND_TRAIT = 4 )
     et
    (
     CARTSNBAV < 2 ou CARTSNBAV > 45
     ou
     CARTSNBAC < 2 ou CARTSNBAC > 45
     ou
     CARTSNBAP1 < 2 ou CARTSNBAP1 > 45
     ou
     CARTSNBAP2 < 2 ou CARTSNBAP2 > 45
     ou
     CARTSNBAP3 < 2 ou CARTSNBAP3 > 45
     ou
     CARTSNBAP4 < 2 ou CARTSNBAP4 > 45
     ou
     REMPLANBV < 2 ou REMPLANBV > 45
     ou
     REMPLANBC < 2 ou REMPLANBC > 45
     ou
     REMPLANBP1 < 2 ou REMPLANBP1 > 45
     ou
     REMPLANBP2 < 2 ou REMPLANBP2 > 45
     ou
     REMPLANBP3 < 2 ou REMPLANBP3 > 45
     ou
     REMPLANBP4 < 2 ou REMPLANBP4 > 45
     ou
     CODNAF < 2 ou CODNAF > 45
     ou
     CODNAG < 2 ou CODNAG > 45
     ou
     CODNBF < 2 ou CODNBF > 45
     ou
     CODNBG < 2 ou CODNBG > 45
     ou
     CODNCF < 2 ou CODNCF > 45
     ou
     CODNCG < 2 ou CODNCG > 45
     ou
     CODNDF < 2 ou CODNDF > 45
     ou
     CODNDG < 2 ou CODNDG > 45
     ou
     CODNEF < 2 ou CODNEF > 45
     ou
     CODNGG < 2 ou CODNGG > 45
     ou
     CODNFF < 2 ou CODNFF > 45
     ou
     CODNFG < 2 ou CODNFG > 45
    )
   )
   ou
   ((V_IND_TRAIT = 5 )
     et
    (
     CARTSNBAV = 1 ou CARTSNBAV > 45
     ou
     CARTSNBAC = 1 ou CARTSNBAC > 45
     ou
     CARTSNBAP1 = 1 ou CARTSNBAP1 > 45
     ou
     CARTSNBAP2 = 1 ou CARTSNBAP2 > 45
     ou
     CARTSNBAP3 = 1 ou CARTSNBAP3 > 45
     ou
     CARTSNBAP4 = 1 ou CARTSNBAP4 > 45
     ou
     REMPLANBV = 1 ou REMPLANBV > 45
     ou
     REMPLANBC = 1 ou REMPLANBC > 45
     ou
     REMPLANBP1 = 1 ou REMPLANBP1 > 45
     ou
     REMPLANBP2 = 1 ou REMPLANBP2 > 45
     ou
     REMPLANBP3 = 1 ou REMPLANBP3 > 45
     ou
     REMPLANBP4 = 1 ou REMPLANBP4 > 45
     ou
     CODNAF = 1 ou CODNAF > 45
     ou
     CODNAG = 1 ou CODNAG > 45
     ou
     CODNBF = 1 ou CODNBF > 45
     ou
     CODNBG = 1 ou CODNBG > 45
     ou
     CODNCF = 1 ou CODNCF > 45
     ou
     CODNCG = 1 ou CODNCG > 45
     ou
     CODNDF = 1 ou CODNDF > 45
     ou
     CODNDG = 1 ou CODNDG > 45
     ou
     CODNEF = 1 ou CODNEF > 45
     ou
     CODNGG = 1 ou CODNGG > 45
     ou
     CODNFF = 1 ou CODNFF > 45
     ou
     CODNFG = 1 ou CODNFG > 45
    )
   )
alors erreur A14001 ;
verif 11402:
application : iliad  ;


si
  (V_IND_TRAIT = 4
    et
    (
     (positif(CARTSV) + present(CARTSNBAV) = 1)
     ou
     (positif(CARTSC) + present(CARTSNBAC) = 1)
     ou
     (positif(CARTSP1) + present(CARTSNBAP1) = 1)
     ou
     (positif(CARTSP2) + present(CARTSNBAP2) = 1)
     ou
     (positif(CARTSP3) + present(CARTSNBAP3) = 1)
     ou
     (positif(CARTSP4) + present(CARTSNBAP4) = 1)
     ou
     (positif(REMPLAV) + present(REMPLANBV) = 1)
     ou
     (positif(REMPLAC) + present(REMPLANBC) = 1)
     ou
     (positif(REMPLAP1) + present(REMPLANBP1) = 1)
     ou
     (positif(REMPLAP2) + present(REMPLANBP2) = 1)
     ou
     (positif(REMPLAP3) + present(REMPLANBP3) = 1)
     ou
     (positif(REMPLAP4) + present(REMPLANBP4) = 1)
     ou
     (positif(CODRAF) + present(CODNAF) = 1)
     ou
     (positif(CODRAG) + present(CODNAG) = 1)
     ou
     (positif(CODRBF) + present(CODNBF) = 1)
     ou
     (positif(CODRBG) + present(CODNBG) = 1)
     ou
     (positif(CODRCF) + present(CODNCF) = 1)
     ou
     (positif(CODRCG) + present(CODNCG) = 1)
     ou
     (positif(CODRDF) + present(CODNDF) = 1)
     ou
     (positif(CODRDG) + present(CODNDG) = 1)
     ou
     (positif(CODREF) + present(CODNEF) = 1)
     ou
     (positif(CODRGG) + present(CODNGG) = 1)
     ou
     (positif(CODRFF) + present(CODNFF) = 1)
     ou
     (positif(CODRFG) + present(CODNFG) = 1)
    )
  )
  ou
  (V_IND_TRAIT = 5
    et
    (
     (positif(CARTSV) + positif(CARTSNBAV) = 1)
     ou
     (positif(CARTSC) + positif(CARTSNBAC) = 1)
     ou
     (positif(CARTSP1) + positif(CARTSNBAP1) = 1)
     ou
     (positif(CARTSP2) + positif(CARTSNBAP2) = 1)
     ou
     (positif(CARTSP3) + positif(CARTSNBAP3) = 1)
     ou
     (positif(CARTSP4) + positif(CARTSNBAP4) = 1)
     ou
     (positif(REMPLAV) + positif(REMPLANBV) = 1)
     ou
     (positif(REMPLAC) + positif(REMPLANBC) = 1)
     ou
     (positif(REMPLAP1) + positif(REMPLANBP1) = 1)
     ou
     (positif(REMPLAP2) + positif(REMPLANBP2) = 1)
     ou
     (positif(REMPLAP3) + positif(REMPLANBP3) = 1)
     ou
     (positif(REMPLAP4) + positif(REMPLANBP4) = 1)
     ou
     (positif(CODRAF) + positif(CODNAF) = 1)
     ou
     (positif(CODRAG) + positif(CODNAG) = 1)
     ou
     (positif(CODRBF) + positif(CODNBF) = 1)
     ou
     (positif(CODRBG) + positif(CODNBG) = 1)
    ou
     (positif(CODRCF) + positif(CODNCF) = 1)
    ou
     (positif(CODRCG) + positif(CODNCG) = 1)
    ou
    (positif(CODRDF) + positif(CODNDF) = 1)
    ou
    (positif(CODRDG) + positif(CODNDG) = 1)
    ou
   (positif(CODREF) + positif(CODNEF) = 1)
    ou
   (positif(CODRGG) + positif(CODNGG) = 1)
   ou
   (positif(CODRFF) + positif(CODNFF) = 1)
   ou
  (positif(CODRFG) + positif(CODNFG) = 1)
    )
  )
alors erreur A14002 ;
verif 11411:
application : iliad  ;


si
   V_IND_TRAIT > 0
   et
   (COTFV + 0 > 25
    ou
    COTFC + 0 > 25
    ou
    COTF1 + 0 > 25
    ou
    COTF2 + 0 > 25
    ou
    COTF3 + 0 > 25
    ou
    COTF4 + 0 > 25)

alors erreur A14101 ;
verif 11412:
application : iliad  ;


si
   (V_IND_TRAIT = 4
    et
    (
     (positif(PEBFV) + present(COTFV) = 1)
     ou
     (positif(PEBFC) + present(COTFC) = 1)
     ou
     (positif(PEBF1) + present(COTF1) = 1)
     ou
     (positif(PEBF2) + present(COTF2) = 1)
     ou
     (positif(PEBF3) + present(COTF3) = 1)
     ou
     (positif(PEBF4) + present(COTF4) = 1)
     ou
     (positif(COTFV) + present(PEBFV) = 1)
     ou
     (positif(COTFC) + present(PEBFC) = 1)
     ou
     (positif(COTF1) + present(PEBF1) = 1)
     ou
     (positif(COTF2) + present(PEBF2) = 1)
     ou
     (positif(COTF3) + present(PEBF3) = 1)
     ou
     (positif(COTF4) + present(PEBF4) = 1)
    )
   )
   ou
   (V_IND_TRAIT = 5
    et
    (
     (positif(PEBFV) + positif(COTFV) = 1)
     ou
     (positif(PEBFC) + positif(COTFC) = 1)
     ou
     (positif(PEBF1) + positif(COTF1) = 1)
     ou
     (positif(PEBF2) + positif(COTF2) = 1)
     ou
     (positif(PEBF3) + positif(COTF3) = 1)
     ou
     (positif(PEBF4) + positif(COTF4) = 1)
     ou
     (positif(COTFV) + positif(PEBFV) = 1)
     ou
     (positif(COTFC) + positif(PEBFC) = 1)
     ou
     (positif(COTF1) + positif(PEBF1) = 1)
     ou
     (positif(COTF2) + positif(PEBF2) = 1)
     ou
     (positif(COTF3) + positif(PEBF3) = 1)
     ou
     (positif(COTF4) + positif(PEBF4) = 1)
    )
   )

alors erreur A14102 ;
verif 1143:
application : iliad  ;


si
    (
 ( FRNV + COD1AE > 0 et (present(TSHALLOV) + present(ALLOV) + present(SALEXTV) + present(COD1AF)+ present(COD1AG) + present(COD1GB) + present (COD1AA) + present (COD1GF) + present (COD1GG)) = 0 )
     ou
 ( FRNC + COD1BE > 0 et (present(TSHALLOC) + present(ALLOC) + present(SALEXTC) + present(COD1BF)+ present(COD1BG) + present(COD1HB) + present (COD1BA)+ present (COD1HF) + present (COD1HG)) = 0 )
     ou
 ( FRN1 + COD1CE > 0 et (present(TSHALLO1) + present(ALLO1) + present(SALEXT1) + present(COD1CF)+ present(COD1CG) + present(COD1IB) + present (COD1CA)+ present (COD1IF) + present (COD1IG)) = 0 )
     ou
 ( FRN2 + COD1DE > 0 et (present(TSHALLO2) + present(ALLO2) + present(SALEXT2) + present(COD1DF)+ present(COD1DG) + present(COD1JB) + present (COD1DA) + present (COD1JF) + present (COD1JG)) = 0 )
     ou
 ( FRN3 + COD1EE > 0 et (present(TSHALLO3) + present(ALLO3) + present(SALEXT3) + present(COD1EF)+ present(COD1EG) + present (COD1EA) + present (COD1KF) + present (COD1KG)) = 0 )
     ou
 ( FRN4 + COD1FE > 0 et (present(TSHALLO4) + present(ALLO4) + present(SALEXT4) + present(COD1FF)+ present(COD1FG) + present (COD1FA) + present (COD1LF) + present (COD1LG)) = 0 )
    )
alors erreur A143 ;
verif 11441:
application : iliad  ;


si
   COD1NX + 0 < GSALV + 0
   et
   GSALV + 0 > 0

alors erreur A14401 ;
verif 11442:
application : iliad  ;


si
   COD1OX + 0 < GSALC + 0
   et
   GSALC + 0 > 0

alors erreur A14402 ;
verif 11451:
application : iliad  ;

si
 positif(COD1AL) = 1
 et
 present(BRAS) = 1
 et
 present(V_8ZT) = 0

alors erreur A14501;
verif 11452:
application : iliad  ;

si
 positif(V_0AM + V_0AO) = 1
 et
 positif(COD1BL) = 1
 et
 present(BRAS) = 1
 et
 present(CODZRE) = 0

alors erreur A14502;
verif 11453:
application : iliad  ;

si 
 positif(present(COD1CL) + present(COD1DL) + present(COD1EL) + present(COD1FL)) = 1
 et
present(BRAS) = 1
et
present(CODZRF) = 0

alors erreur A14503;
verif 12231:
application : iliad ;

si
  ((V_IND_TRAIT = 4 )
   et
   (
    REVACTNB < 2 ou REVACTNB > 20
    ou
    REVPEANB < 2 ou REVPEANB > 20
    ou
    PROVIENB < 2 ou PROVIENB > 20
    ou
    DISQUONB < 2 ou DISQUONB > 20
    ou
    RESTUCNB < 2 ou RESTUCNB > 20
    ou
    INTERENB < 2 ou INTERENB > 20
    ou
      CODNYY < 2 ou CODNYY > 20
   )
  )
  ou
  ((V_IND_TRAIT = 5 )
   et
   (
    REVACTNB = 1 ou REVACTNB > 20
    ou
    REVPEANB = 1 ou REVPEANB > 20
    ou
    PROVIENB = 1 ou PROVIENB > 20
    ou
    DISQUONB = 1 ou DISQUONB > 20
    ou
    RESTUCNB = 1 ou RESTUCNB > 20
    ou
    INTERENB = 1 ou INTERENB > 20
    ou
    CODNYY = 1 ou CODNYY > 20
   )
  )
alors erreur A22301 ;
verif 12232:
application : iliad  ;

si
   (V_IND_TRAIT = 4
    et
    (
     positif(REVACT) + present(REVACTNB) = 1
     ou
     positif(REVPEA) + present(REVPEANB) = 1
     ou
     positif(PROVIE) + present(PROVIENB) = 1
     ou
     positif(DISQUO) + present(DISQUONB) = 1
     ou
     positif(RESTUC) + present(RESTUCNB) = 1
     ou
     positif(INTERE) + present(INTERENB) = 1
     ou
     positif(CODRYY) + present(CODNYY) = 1
    )
   )
   ou
   (V_IND_TRAIT = 5
    et
    (
     positif(REVACT) + positif(REVACTNB) = 1
     ou
     positif(REVPEA) + positif(REVPEANB) = 1
     ou
     positif(PROVIE) + positif(PROVIENB) = 1
     ou
     positif(DISQUO) + positif(DISQUONB) = 1
     ou
     positif(RESTUC) + positif(RESTUCNB) = 1
     ou
     positif(INTERE) + positif(INTERENB) = 1
     ou
     positif(CODRYY) + positif(CODNYY) = 1
    )
   )
alors erreur A22302 ;
verif 1227:
application : iliad  ;

si
   positif(COD2TT)> 0 
   et (positif(COD2TU)+ positif (COD2TV) + positif(COD2TW)+ positif(COD2TX) + positif(COD2TY))>0
   
alors erreur A227 ;
verif 1228:
application : iliad ;

si
  positif(COD2UU) > 0
  et
  ((COD2VV + COD2WW + COD2RC + COD2RD < COD2UU)
   ou
   (present(COD2VV) + present(COD2WW) + present(COD2RC) + present(COD2RD) + 0 = 0))

alors erreur A228 ;  
verif 1229:
application : iliad ;

si
  present(COD2OP) = 0
  et
  positif(REVACT + REVPEA + DISQUO + INTERE + RESTUC) = 1

alors erreur A229 ;

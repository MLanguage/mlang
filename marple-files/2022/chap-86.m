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
regle 861000:
application : iliad  ;


BNCV = BNHREV + COD5XK + BNCREV + COD5XJ + CODCQC + CODCQI - BNHDEV - BNCDEV ;
BNCC = BNHREC + COD5YK + BNCREC + COD5YJ + CODCRC + CODCRI - BNHDEC - BNCDEC ;
BNCP = BNHREP + COD5ZK + BNCREP + COD5ZJ + CODCSC + CODCSI - BNHDEP - BNCDEP ;

BNNAV = BNCREV + COD5XJ + CODCQC - BNCDEV ;

DEF5QC = arr(BNCDEV * BNCREV / (BNCREV + COD5XJ + CODCQC)) * positif(COD5XJ + CODCQC) + BNCDEV * (1 - positif(COD5XJ + CODCQC)) ;

DEF5XJ = positif_ou_nul(BNNAV) * (positif(CODCQC) * arr(BNCDEV*COD5XJ/(BNCREV + COD5XJ+CODCQC))
                                                             + (1-positif(CODCQC)) * (BNCDEV-DEF5QC));
DEFCQC = positif_ou_nul(BNNAV) * max(0 , BNCDEV - DEF5QC - DEF5XJ) ;

R15QC = max(0 , BNCREV - DEF5QC) * positif_ou_nul(BNNAV) + BNNAV * (1 - positif_ou_nul(BNNAV)) ;

R15XJ = max(0 , COD5XJ - DEF5XJ) ;
BNNAMNV = positif_ou_nul(BNNAV) * arr(R15QC + R15XJ) + (1 - positif_ou_nul(BNNAV)) * BNNAV ;
R1CQC = max(0 , CODCQC - DEFCQC) * positif_ou_nul(BNNAV)+0;

PASBNNAV = R15QC - COD5XP + COD5XH ;

BNNAC = BNCREC + COD5YJ + CODCRC - BNCDEC ;

DEF5RC = arr(BNCDEC * BNCREC / (BNCREC + COD5YJ + CODCRC)) * positif(COD5YJ + CODCRC) + BNCDEC * (1 - positif(COD5YJ+CODCRC)) ;

DEF5YJ = positif_ou_nul(BNNAC) * (positif(CODCRC) * arr(BNCDEC*COD5YJ/(BNCREC + COD5YJ+CODCRC))
                                                             + (1-positif(CODCRC)) * (BNCDEC-DEF5RC));
DEFCRC = positif_ou_nul(BNNAC) * max(0 , BNCDEC - DEF5RC - DEF5YJ) ;
R15RC = max(0 , BNCREC - DEF5RC) * positif_ou_nul(BNNAC) + BNNAC * (1 - positif_ou_nul(BNNAC)) ;

R15YJ = max(0 , COD5YJ - DEF5YJ) ;
BNNAMNC = positif_ou_nul(BNNAC) * arr(R15RC + R15YJ) + (1 - positif_ou_nul(BNNAC)) * BNNAC ;
R1CRC = max(0 , CODCRC - DEFCRC) * positif_ou_nul(BNNAC)+0;

PASBNNAC = R15RC - COD5YP + COD5YH ;

BNNAP = BNCREP + COD5ZJ + CODCSC - BNCDEP ;

DEF5SC = arr(BNCDEP * BNCREP / (BNCREP + COD5ZJ + CODCSC)) * positif(COD5ZJ + CODCSC) + BNCDEP * (1 - positif(COD5ZJ + CODCSC)) ;

DEF5ZJ = positif_ou_nul(BNNAP) * (positif(CODCSC) * arr(BNCDEP*COD5ZJ/(BNCREP + COD5ZJ+CODCSC))
                                                             + (1-positif(CODCSC)) * (BNCDEP-DEF5SC));
DEFCSC = positif_ou_nul(BNNAP) * max(0 , BNCDEP - DEF5SC - DEF5ZJ) ;
R15SC = max(0 , BNCREP - DEF5SC) * positif_ou_nul(BNNAP) + BNNAP * (1 - positif_ou_nul(BNNAP)) ;

R15ZJ = max(0 , COD5ZJ - DEF5ZJ) ;
BNNAMNP = positif_ou_nul(BNNAP) * (R15SC + R15ZJ) + (1 - positif_ou_nul(BNNAP)) * BNNAP ;
R1CSC = max(0 , CODCSC - DEFCSC) * positif_ou_nul(BNNAP)+0;

PASBNNAP = R15SC - COD5ZP + COD5ZH ;


BNNSV = BNHREV + COD5XK + CODCQI - BNHDEV ;
DEF5QI = positif_ou_nul(BNNSV) * (positif(COD5XK+CODCQI) * arr(BNHDEV*BNHREV/(BNHREV + COD5XK+CODCQI))
                                  + (1-positif(COD5XK+CODCQI)) * BNHDEV) ;
DEF5XK = positif_ou_nul(BNNSV) * (positif(CODCQI) * arr(BNHDEV*COD5XK/(BNHREV + COD5XK+CODCQI))
                                  + (1-positif(CODCQI)) * (BNHDEV-DEF5QI)) ;
DEFCQI = positif_ou_nul(BNNSV) * max(0 , BNHDEV - DEF5QI - DEF5XK) ;
BNNS5QI = max(0,BNHREV - DEF5QI);
R2MAJ5QI = arr(BNNS5QI * MAJREV20) * positif(BNNSV) + BNNSV * (1 - positif(BNNSV)) ;
BNNS5XK = max(0,COD5XK - DEF5XK);
R2MAJCQI = arr(max(0,CODCQI - DEFCQI)* MAJREV20)*positif_ou_nul(BNNSV)+0;
BNNSMNV = positif_ou_nul(BNNSV) * arr((BNNS5QI + BNNS5XK) * MAJREV20) + (1 - positif_ou_nul(BNNSV)) * BNNSV ;

PASBNNSV = arr(((BNNS5QI * positif(BNNSV) + BNNSV * (1 - positif(BNNSV))) - COD5XQ + COD5XL) * MAJREV20) * positif((BNNS5QI * positif(BNNSV) + BNNSV * (1 - positif(BNNSV))) - COD5XQ + COD5XL)
            + ((BNNS5QI * positif(BNNSV) + BNNSV * (1 - positif(BNNSV))) - COD5XQ + COD5XL) * (1 - positif((BNNS5QI * positif(BNNSV) + BNNSV * (1 - positif(BNNSV))) - COD5XQ + COD5XL)) ;



BNNSC = BNHREC + COD5YK + CODCRI - BNHDEC ;
DEF5RI = positif_ou_nul(BNNSC) * (positif(COD5YK+CODCRI) * arr(BNHDEC*BNHREC/(BNHREC + COD5YK+CODCRI))
                                  + (1-positif(COD5YK+CODCRI)) * BNHDEC);
DEF5YK = positif_ou_nul(BNNSC) * (positif(CODCRI) * arr(BNHDEC*COD5YK/(BNHREC + COD5YK+CODCRI))
                                  + (1-positif(CODCRI)) * (BNHDEC-DEF5RI));
DEFCRI = positif_ou_nul(BNNSC) * max(0,BNHDEC-DEF5RI-DEF5YK);
BNNS5RI = max(0,BNHREC - DEF5RI);
R2MAJ5RI = arr(BNNS5RI * MAJREV20) * positif(BNNSC) + BNNSC * (1 - positif(BNNSC)) ;
BNNS5YK = max(0,COD5YK - DEF5YK);
R2MAJCRI = arr(max(0,CODCRI - DEFCRI)* MAJREV20)*positif_ou_nul(BNNSC)+0;
BNNSMNC = positif_ou_nul(BNNSC) * arr((BNNS5RI + BNNS5YK) * MAJREV20) + (1 - positif_ou_nul(BNNSC)) * BNNSC ;

PASBNNSC = arr(((BNNS5RI * positif(BNNSC) + BNNSC * (1 - positif(BNNSC))) - COD5YQ + COD5YL) * MAJREV20) * positif((BNNS5RI * positif(BNNSC) + BNNSC * (1 - positif(BNNSC))) - COD5YQ + COD5YL)
            + ((BNNS5RI * positif(BNNSC) + BNNSC * (1 - positif(BNNSC))) - COD5YQ + COD5YL) * (1 - positif((BNNS5RI * positif(BNNSC) + BNNSC * (1 - positif(BNNSC))) - COD5YQ + COD5YL)) ;


BNNSP = BNHREP + COD5ZK + CODCSI - BNHDEP ;
DEF5SI = positif_ou_nul(BNNSP) * (positif(COD5ZK+CODCSI) * arr(BNHDEP*BNHREP/(BNHREP + COD5ZK+CODCSI))
                                  + (1-positif(COD5ZK+CODCSI)) * BNHDEP);
DEF5ZK = positif_ou_nul(BNNSP) * (positif(CODCSI) * arr(BNHDEP*COD5ZK/(BNHREP + COD5ZK+CODCSI))
                                  + (1-positif(CODCSI)) * (BNHDEP-DEF5SI));
DEFCSI = positif_ou_nul(BNNSP) * max(0,BNHDEP-DEF5SI-DEF5ZK);
BNNS5SI = max(0,BNHREP - DEF5SI);
R2MAJ5SI = arr(BNNS5SI * MAJREV20) * positif(BNNSP) + BNNSP * (1 - positif(BNNSP)) ;
BNNS5ZK = max(0,COD5ZK - DEF5ZK);
R2MAJCSI = arr(max(0,CODCSI - DEFCSI)* MAJREV20)*positif_ou_nul(BNNSP)+0;
BNNSMNP = positif_ou_nul(BNNSP) * arr((BNNS5SI + BNNS5ZK) * MAJREV20) + (1 - positif_ou_nul(BNNSP)) * BNNSP ;
PASBNNSP = arr(((BNNS5SI * positif(BNNSP) + BNNSP * (1 - positif(BNNSP))) - COD5ZQ + COD5ZL) * MAJREV20) * positif((BNNS5SI * positif(BNNSP) + BNNSP * (1 - positif(BNNSP))) - COD5ZQ + COD5ZL)
            + ((BNNS5SI * positif(BNNSP) + BNNSP * (1 - positif(BNNSP))) - COD5ZQ + COD5ZL) * (1 - positif((BNNS5SI * positif(BNNSP) + BNNSP * (1 - positif(BNNSP))) - COD5ZQ + COD5ZL)) ;

regle 861010:
application : iliad  ;



BNNAAV = BNCAABV + COD5XS + CODCJG - BNCAADV ;
DEF5JG = positif_ou_nul(BNCAABV + COD5XS + CODCJG- BNCAADV) * (positif(COD5XS+CODCJG) * arr(BNCAADV*BNCAABV/(BNCAABV + COD5XS+CODCJG))
                                                             + (1-positif(COD5XS+CODCJG)) * BNCAADV);
DEF5XS = positif_ou_nul(BNCAABV + COD5XS + CODCJG- BNCAADV) * (positif(CODCJG) * arr(BNCAADV*COD5XS/(BNCAABV + COD5XS+CODCJG))
                                                             + (1-positif(CODCJG)) * (BNCAADV-DEF5JG));
DEFCJG = positif_ou_nul(BNCAABV + COD5XS + CODCJG- BNCAADV) * max(0,BNCAADV-DEF5JG-DEF5XS);
R15JG = max(0 , BNCAABV - DEF5JG) * positif(BNNAAV) + BNNAAV * (1 - positif(BNNAAV)) ;
R15XS = max(0,COD5XS - DEF5XS);
R1CJG = max(0,CODCJG - DEFCJG)*positif_ou_nul(BNCAABV + COD5XS + CODCJG- BNCAADV)+0;
BNNAANV = positif_ou_nul(BNCAABV + COD5XS + CODCJG- BNCAADV) * (R15JG+R15XS) + (1-positif_ou_nul(BNCAABV + COD5XS + CODCJG- BNCAADV)) * BNNAAV;

PASBNNAAV = R15JG - COD5XY + COD5VM ;

BNNAAC = BNCAABC + COD5YS + CODCRF - BNCAADC ;
DEF5RF = positif_ou_nul(BNCAABC + COD5YS + CODCRF- BNCAADC) * (positif(COD5YS+CODCRF) * arr(BNCAADC*BNCAABC/(BNCAABC + COD5YS+CODCRF))
                                                             + (1-positif(COD5YS+CODCRF)) * BNCAADC);
DEF5YS = positif_ou_nul(BNCAABC + COD5YS + CODCRF- BNCAADC) * (positif(CODCRF) * arr(BNCAADC*COD5YS/(BNCAABC + COD5YS+CODCRF))
                                                             + (1-positif(CODCRF)) * (BNCAADC-DEF5RF));
DEFCRF = positif_ou_nul(BNCAABC + COD5YS + CODCRF- BNCAADC) * max(0,BNCAADC-DEF5RF-DEF5YS);
R15RF = max(0 , BNCAABC - DEF5RF) * positif(BNNAAC) + BNNAAC * (1 - positif(BNNAAC)) ;
R15YS = max(0,COD5YS - DEF5YS);
R1CRF = max(0,CODCRF - DEFCRF)*positif_ou_nul(BNCAABC + COD5YS + CODCRF- BNCAADC)+0;
BNNAANC = positif_ou_nul(BNCAABC + COD5YS + CODCRF- BNCAADC) * (R15RF+R15YS) + (1-positif_ou_nul(BNCAABC + COD5YS + CODCRF- BNCAADC)) * BNNAAC;

PASBNNAAC = R15RF - COD5YY + COD5WM ;

BNNAAP = BNCAABP + COD5ZS + CODCSF - BNCAADP ;

DEF5SF = positif_ou_nul(BNNAAP) * (positif(COD5ZS + CODCSF) * arr(BNCAADP * BNCAABP / (BNCAABP + COD5ZS +CODCSF))
                                   + (1 - positif(COD5ZS + CODCSF)) * BNCAADP) ;
DEF5ZS = positif_ou_nul(BNNAAP) * (positif(CODCSF) * arr(BNCAADP * COD5ZS / (BNCAABP + COD5ZS + CODCSF))
                                   + (1 - positif(CODCSF)) * (BNCAADP - DEF5SF)) ;
DEFCSF = positif_ou_nul(BNNAAP) * max(0 , BNCAADP - DEF5SF - DEF5ZS) ;

R15SF = max(0 , BNCAABP - DEF5SF) * positif(BNNAAP) + BNNAAP * (1 - positif(BNNAAP)) ;
R15ZS = max(0,COD5ZS - DEF5ZS);
R1CSF = max(0,CODCSF - DEFCSF)*positif_ou_nul(BNCAABP + COD5ZS + CODCSF- BNCAADP)+0;

BNNAANP = positif_ou_nul(BNCAABP + COD5ZS + CODCSF- BNCAADP) * (R15SF+R15ZS) + (1-positif_ou_nul(BNCAABP + COD5ZS + CODCSF- BNCAADP)) * BNNAAP;

PASBNNAAP = R15SF - COD5ZY + COD5ZM ;

NOCEPV = BNCAABV + COD5XS + CODCJG - BNCAADV + ANOCEP + COD5XX + CODCSN - DNOCEP ;

NOCEPC = BNCAABC + COD5YS + CODCRF - BNCAADC + ANOVEP + COD5YX + CODCNS - DNOCEPC ; 

NOCEPP = BNCAABP + COD5ZS + CODCSF - BNCAADP + ANOPEP + COD5ZX + CODCOS - DNOCEPP ; 

NOCEPIMPV = ANOCEP + COD5XX + CODCSN - DNOCEP ;

DEF5SN = positif_ou_nul(NOCEPIMPV) * (positif(COD5XX + CODCSN) * arr(DNOCEP * ANOCEP / (ANOCEP + COD5XX + CODCSN))
                                      + (1 - positif(COD5XX + CODCSN)) * DNOCEP) ;
DEF5XX = positif_ou_nul(NOCEPIMPV) * (positif(CODCSN) * arr(DNOCEP * COD5XX / (ANOCEP + COD5XX + CODCSN))
                                      + (1 - positif(CODCSN)) * (DNOCEP - DEF5SN)) ;
DEFCSN = positif_ou_nul(NOCEPIMPV) * max(0 , DNOCEP - DEF5SN - DEF5XX) ;

NOCEPIMP5SN = max(0 , ANOCEP - DEF5SN) ;
R2MAJ5SN = NOCEPIMP5SN * MAJREV20 * positif_ou_nul(NOCEPIMPV) + NOCEPIMPV * (1 - positif_ou_nul(NOCEPIMPV)) ;

NOCEPIMP5XX = max(0 , COD5XX - DEF5XX) * positif_ou_nul(NOCEPIMPV) ;
R2MAJ5XX = NOCEPIMP5XX * MAJREV20 ;
R2MAJCSN = arr(max(0 , CODCSN - DEFCSN) * MAJREV20) * positif_ou_nul(NOCEPIMPV) + 0 ;

NOCEPIMPNV = positif_ou_nul(NOCEPIMPV) * arr((NOCEPIMP5SN + NOCEPIMP5XX) * MAJREV20) + (1 - positif_ou_nul(NOCEPIMPV)) * NOCEPIMPV ;

R25SN = NOCEPIMP5SN * positif_ou_nul(NOCEPIMPV) + NOCEPIMPV * (1 - positif_ou_nul(NOCEPIMPV)) ;
PASNOCEPIMPV = arr((R25SN - (COD5XZ - COD5VN)) * MAJREV20) * positif(R25SN - (COD5XZ - COD5VN))
               + (R25SN - (COD5XZ - COD5VN)) * (1 - positif(R25SN - (COD5XZ - COD5VN))) ;


NOCEPIMPQV = R1CJG + R2MAJCSN ;

NOCEPIMPC = ANOVEP + COD5YX + CODCNS - DNOCEPC ;

DEF5NS = positif_ou_nul(NOCEPIMPC) * (positif(COD5YX + CODCNS) * arr(DNOCEPC * ANOVEP / (ANOVEP + COD5YX + CODCNS))
                                      + (1 - positif(COD5YX + CODCNS)) * DNOCEPC) ;
DEF5YX = positif_ou_nul(NOCEPIMPC) * (positif(CODCNS) * arr(DNOCEPC * COD5YX / (ANOVEP + COD5YX + CODCNS))
                                      + (1 - positif(CODCNS)) * (DNOCEPC - DEF5NS)) ;
DEFCNS = positif_ou_nul(NOCEPIMPC) * max(0 , DNOCEPC - DEF5NS - DEF5YX) ;

NOCEPIMP5NS = max(0 , ANOVEP - DEF5NS) ;
R2MAJ5NS = NOCEPIMP5NS * MAJREV20 * positif_ou_nul(NOCEPIMPC) + NOCEPIMPC * (1 - positif_ou_nul(NOCEPIMPC)) ;

NOCEPIMP5YX = max(0 , COD5YX - DEF5YX) * positif_ou_nul(NOCEPIMPC) ;
R2MAJ5YX = NOCEPIMP5YX * MAJREV20;
R2MAJCNS = arr(max(0 , CODCNS - DEFCNS) * MAJREV20) * positif_ou_nul(NOCEPIMPC) + 0 ;

NOCEPIMPNC = positif_ou_nul(NOCEPIMPC) * arr((NOCEPIMP5NS + NOCEPIMP5YX) * MAJREV20) + (1 - positif_ou_nul(NOCEPIMPC)) * NOCEPIMPC ;

R25NS = NOCEPIMP5NS * positif_ou_nul(NOCEPIMPC) + NOCEPIMPC * (1 - positif_ou_nul(NOCEPIMPC)) ;
PASNOCEPIMPC = arr((R25NS - (COD5YZ - COD5WN)) * MAJREV20) * positif(R25NS - (COD5YZ - COD5WN)) 
               + (R25NS - (COD5YZ - COD5WN)) * (1 - positif(R25NS - (COD5YZ - COD5WN))) ;


NOCEPIMPQC = R1CRF + R2MAJCNS ;

NOCEPIMPP = ANOPEP + COD5ZX + CODCOS - DNOCEPP ;

DEF5OS = positif_ou_nul(NOCEPIMPP) * (positif(COD5ZX + CODCOS) * arr(DNOCEPP * ANOPEP / (ANOPEP + COD5ZX + CODCOS))
                                      + (1 - positif(COD5ZX + CODCOS)) * DNOCEPP) ;
DEF5ZX = positif_ou_nul(NOCEPIMPP) * (positif(CODCOS) * arr(DNOCEPP * COD5ZX / (ANOPEP + COD5ZX + CODCOS))
                                      + (1 - positif(CODCOS)) * (DNOCEPP - DEF5OS)) ;
DEFCOS = positif_ou_nul(NOCEPIMPP) * max(0 , DNOCEPP - DEF5OS - DEF5ZX) ;

NOCEPIMP5OS = max(0 , ANOPEP - DEF5OS) ;
R2MAJ5OS = NOCEPIMP5OS * MAJREV20 * positif_ou_nul(NOCEPIMPP) + NOCEPIMPP * (1 - positif_ou_nul(NOCEPIMPP)) ;

NOCEPIMP5ZX = max(0 , COD5ZX - DEF5ZX) * positif_ou_nul(NOCEPIMPP) ;
R2MAJ5ZX = NOCEPIMP5ZX * MAJREV20 ;
R2MAJCOS = arr(max(0 , CODCOS - DEFCOS) * MAJREV20) * positif_ou_nul(NOCEPIMPP) + 0 ;

NOCEPIMPNP = positif_ou_nul(NOCEPIMPP) * arr((NOCEPIMP5OS + NOCEPIMP5ZX) * MAJREV20) + (1 - positif_ou_nul(NOCEPIMPP)) * NOCEPIMPP ;

R25OS = NOCEPIMP5OS * positif_ou_nul(NOCEPIMPP) + NOCEPIMPP * (1 - positif_ou_nul(NOCEPIMPP)) ;
PASNOCEPIMPP = arr((R25OS - (COD5ZW - COD5ZZ)) * MAJREV20 ) * positif(R25OS - (COD5ZW - COD5ZZ)) 
               + (R25OS - (COD5ZW - COD5ZZ)) * (1 - positif(R25OS - (COD5ZW - COD5ZZ))) ;


NOCEPIMPQP = R1CSF + R2MAJCOS ;

NOCEPIMP = NOCEPIMPNV + NOCEPIMPNC + NOCEPIMPNP ;

TOTDABNCNP = (DABNCNP6 + DABNCNP5 + DABNCNP4 + DABNCNP3 + DABNCNP2 + DABNCNP1);
regle 861020:
application : iliad  ;


NOCEPIMPQ = max(0,NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP + min(0,DEFORDIBNCNP)) ;
NOCEPIMPQREP = min(0,NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP + min(0,DEFORDIBNCNP)) ;
regle 861025:
application : iliad  ;


BNN = BNCPHQF + BNCIF;

regle 861040:
application : iliad  ;

BNRV = BNNSMNV + BNNAMNV;
BNRC = BNNSMNC + BNNAMNC;
BNRP = BNNSMNP + BNNAMNP;
BNRPROV = (somme(i=V,C,P: (positif(BNHREi - BNHDEi) * arr((BNHREi-BNHDEi)*MAJREV20)
                       + (1-positif_ou_nul(BNHREi-BNHDEi)) *(BNHREi-BNHDEi))
                              + (BNCREi - BNCDEi)));

regle 861050:
application : iliad  ;


BN1 = somme(i=V,C,P:BN1i) ;

regle 861060:
application : iliad  ;


BN1V = BN1AV + PVINVE ;
BN1C = BN1AC + PVINCE ;
BN1P = BN1AP + PVINPE ;

regle 861065:
application : iliad  ;




PVINDUSPRO = COD5QA + COD5RA + COD5SA ; 


PVINDUSNONPRO1 = COD5QJ + INVENTV ; 
PVINDUSNONPRO2 = COD5RJ + INVENTC ; 
PVINDUSNONPRO3 = COD5SJ + INVENTP ;


PVINDUSNONPRO = PVINDUSNONPRO1 + PVINDUSNONPRO2 + PVINDUSNONPRO3 ; 


PVINDUSPBNC = PVINDUSPRO + PVINDUSNONPRO ;  


regle 861070:                                                                    
application : iliad  ;                          
                                                                               
SPETOTV = BNCPROV + BNCNPV ;
SPETOTC = BNCPROC + BNCNPC ;
SPETOTP = BNCPROP + BNCNPP ;

regle 861080:
application : iliad  ;                          
                                                                   

SPEBASABV =SPETOTV ;
SPEBASABC =SPETOTC ;
SPEBASABP =SPETOTP ;
                                                                               
SPEABV = arr((max(MIN_SPEBNC,(SPEBASABV * SPETXAB/100))) * 
                       positif_ou_nul(SPETOTV - MIN_SPEBNC)) +
          arr((min(MIN_SPEBNC,SPEBASABV )) * 
                       positif(MIN_SPEBNC - SPETOTV)) ; 
SPEABC = arr((max(MIN_SPEBNC,(SPEBASABC * SPETXAB/100))) * 
                       positif_ou_nul(SPETOTC - MIN_SPEBNC)) +
          arr((min(MIN_SPEBNC,SPEBASABC )) * 
                       positif(MIN_SPEBNC - SPETOTC)) ; 
SPEABP = arr((max(MIN_SPEBNC,(SPEBASABP * SPETXAB/100))) * 
                       positif_ou_nul(SPETOTP - MIN_SPEBNC)) +
          arr((min(MIN_SPEBNC,SPEBASABP )) * 
                       positif(MIN_SPEBNC - SPETOTP)) ; 

regle 861090:
application : iliad  ;                          
                                                                               
                                                                               
SPEABPV = arr((SPEABV * BNCPROV)/SPETOTV) ; 
SPEABPC = arr((SPEABC * BNCPROC)/SPETOTC) ; 
SPEABPP = arr((SPEABP * BNCPROP)/SPETOTP) ; 
                                                                               
SPEABNPV = SPEABV - SPEABPV ; 
SPEABNPC = SPEABC - SPEABPC ; 
SPEABNPP = SPEABP - SPEABPP ; 

regle 861100:
application : iliad  ;                          
                                                                        
SPENETPV = max (0,(BNCPROV - SPEABPV)) ; 
SPENETPC = max (0,(BNCPROC - SPEABPC)) ; 
SPENETPP = max (0,(BNCPROP - SPEABPP)) ; 
                                                                               
SPENETNPV = max (0,(BNCNPV - SPEABNPV)) ;
SPENETNPC = max (0,(BNCNPC - SPEABNPC)) ;
SPENETNPP = max (0,(BNCNPP - SPEABNPP)) ;

SPENETV = SPENETPV + SPENETNPV ;
SPENETC = SPENETPC + SPENETNPC ;
SPENETP = SPENETPP + SPENETNPP ;
                                                                               
SPENET = somme(i=V,C,P:(SPENETi)) ;

regle 861110:
application : iliad  ;                          
SPENETCTV = BNCPROPVV - BNCPMVCTV ;
SPENETCTC = BNCPROPVC - BNCPMVCTC ;
SPENETCTP = BNCPROPVP - BNCPMVCTP ;
SPENETCT = BNCPROPVV + BNCPROPVC + BNCPROPVP - BNCPMVCTV - BNCPMVCTC - BNCPMVCTP ;
                                                                               
SPENETNPNCTV = min (0,(BNCNPPVV - BNCNPDCT)) ;
SPENETNPNCTC = min (0,(BNCNPPVC - COD5LD)) ;
SPENETNPNCTP = min (0,(BNCNPPVP - COD5MD)) ;
SPENETNPCT = BNCNPPVV + BNCNPPVC + BNCNPPVP - BNCNPDCT - COD5LD - COD5MD  ;

regle 861111:
application : iliad  ;                          



BNCR2TOTALV = BNCNPHQCV + BNCNPQCV;

BNCR2TOTALC = BNCNPHQCC + BNCNPQCC;

BNCR2TOTALP = BNCNPHQCP + BNCNPQCP;

BNCR2TOTALF = somme(i=V,C,P: BNCR2TOTALi) ; 
regle 861115:
application : iliad  ;                          

DEFORDIBNCNPV = BNNAANV + NOCEPIMPNV + SPENETNPNCTV + SPENETCTV;
DEFORDIBNCNPC = BNNAANC + NOCEPIMPNC + SPENETNPNCTC + SPENETCTC;
DEFORDIBNCNPP = BNNAANP + NOCEPIMPNP + SPENETNPNCTP + SPENETCTP;
DEFORDIBNCNP = min(0,DEFORDIBNCNPV + DEFORDIBNCNPC + DEFORDIBNCNPP);
regle 861120:
application : iliad  ; 
BNCPHQNCV = BNNAMNV + BNNSMNV + SPENETPV + SPENETCTV;
BNCPHQNCC = BNNAMNC + BNNSMNC + SPENETPC + SPENETCTC;
BNCPHQNCP = BNNAMNP + BNNSMNP + SPENETPP + SPENETCTP;
BNCPHQNCF = BNCPHQNCV + BNCPHQNCC + BNCPHQNCP;

regle 861123:
application : iliad  ; 
BNCPQNCV = max(0,R1CQC + R2MAJCQI);
BNCPQNCC = max(0,R1CRC + R2MAJCRI);
BNCPQNCP = max(0,R1CSC + R2MAJCSI);
BNCPQUOTF = BNCPQNCV+BNCPQNCC+BNCPQNCP;

BNCPHQCV = ((1-positif(BNCPHQNCV))* positif_ou_nul(BNCPQNCV)) * min(0,BNCPHQNCV+BNCPQNCV) + positif_ou_nul(BNCPHQNCV)* BNCPHQNCV;
BNCPHQCC = ((1-positif(BNCPHQNCC))* positif_ou_nul(BNCPQNCC)) * min(0,BNCPHQNCC+BNCPQNCC) + positif_ou_nul(BNCPHQNCC)* BNCPHQNCC;
BNCPHQCP = ((1-positif(BNCPHQNCP))* positif_ou_nul(BNCPQNCP)) * min(0,BNCPHQNCP+BNCPQNCP) + positif_ou_nul(BNCPHQNCP)* BNCPHQNCP;
BNCPQCV =   positif(BNCPHQNCV) * (R1CQC+R2MAJCQI)
           + (1-positif(BNCPHQNCV)) * max(0,R1CQC+R2MAJCQI+BNCPHQNCV);
BNCPQCC =   positif(BNCPHQNCC) * (R1CRC+R2MAJCRI)
           + (1-positif(BNCPHQNCC)) * max(0,R1CRC+R2MAJCRI+BNCPHQNCC);
BNCPQCP =   positif(BNCPHQNCP) * (R1CSC+R2MAJCSI)
           + (1-positif(BNCPHQNCP)) * max(0,R1CSC+R2MAJCSI+BNCPHQNCP);
BNCPHQCF = BNCPHQCV+BNCPHQCC+BNCPHQCP;
BNCPQCF = BNCPQCV +  BNCPQCC + BNCPQCP;
BNCPHQF = (1-positif(BNCPHQCF)) * positif_ou_nul(BNCPQCF) * min(0,BNCPHQCF+BNCPQCF)
          + positif(BNCPHQCF) * BNCPHQCF;
BNCPQF = (1-positif(BNCPHQCF)) * positif_ou_nul(BNCPQCF) * max(0,BNCPHQCF+BNCPQCF)
          + positif(BNCPHQCF) * BNCPQCF;
BNRTOT = BNCPHQF ;
BNRTOTQ = BNCPQF;
BNCNPHQNCV = BNNAANV + NOCEPIMPNV + SPENETNPV + BNCNPPVV - BNCNPDCT ;
BNCNPHQNCC = BNNAANC + NOCEPIMPNC + SPENETNPC + BNCNPPVC - COD5LD ;
BNCNPHQNCP = BNNAANP + NOCEPIMPNP + SPENETNPP + BNCNPPVP - COD5MD ;
BNCNPQNCV = R1CJG+R2MAJCSN;
BNCNPQNCC = R1CRF+R2MAJCNS;
BNCNPQNCP = R1CSF+R2MAJCOS;
BNCNPHQCV = (1-positif(BNCNPHQNCV)) * positif_ou_nul(BNCNPQNCV) * min(0,BNCNPHQNCV+BNCNPQNCV)+ positif_ou_nul(BNCNPHQNCV)*BNCNPHQNCV;
BNCNPHQCC = (1-positif(BNCNPHQNCC)) * positif_ou_nul(BNCNPQNCC) * min(0,BNCNPHQNCC+BNCNPQNCC)+ positif_ou_nul(BNCNPHQNCC)*BNCNPHQNCC;
BNCNPHQCP = (1-positif(BNCNPHQNCP)) * positif_ou_nul(BNCNPQNCP) * min(0,BNCNPHQNCP+BNCNPQNCP)+ positif_ou_nul(BNCNPHQNCP)*BNCNPHQNCP;
BNNNV = BNCNPHQCV;
BNNNC = BNCNPHQCC;
BNNNP = BNCNPHQCP;
BNCNPHQCF = BNCNPHQCV+BNCNPHQCC+BNCNPHQCP;
BNCNPQCV =   positif(BNCNPHQNCV) * (R1CJG+R2MAJCSN)
           + (1-positif(BNCNPHQNCV))* max(0,R1CJG+R2MAJCSN+BNCNPHQNCV);
BNCNPQCC =   positif(BNCNPHQNCC) * (R1CRF+R2MAJCNS)
           + (1-positif(BNCNPHQNCC)) * max(0,R1CRF+R2MAJCNS+BNCNPHQNCC);
BNCNPQCP =   positif(BNCNPHQNCP) * (R1CSF+R2MAJCOS)
           + (1-positif(BNCNPHQNCP)) * max(0,R1CSF+R2MAJCOS+BNCNPHQNCP);
BNCNPQCF = BNCNPQCV + BNCNPQCC + BNCNPQCP;
regle 861125:
application : iliad  ; 
BNCNPHQF = (1-positif(BNCNPHQCF)) * positif_ou_nul(BNCNPQCF) * min(0,max(0,BNCNPHQCF-min(DEFBNCNPF,DIDABNCNPHQ))+BNCNPQCF)
          + positif(BNCNPHQCF) * max(0,BNCNPHQCF-DIDABNCNPHQ+DEFBNCNPF) *(1-PREM8_11) + max(0,BNCNPHQCF-DIDABNCNPHQ+DEFBNCNPF-BNCNPDIBIS) * PREM8_11;

BNCNPQF = (1-positif(BNCNPHQCF)) * positif_ou_nul(BNCNPQCF) * max(0,BNCNPHQCF+BNCNPQCF-DIDABNCNPQ+DEFNIBNCQ)
          + positif(BNCNPHQCF) * max(0,BNCNPQCF-DIDABNCNPQ+DEFNIBNCQ)  * (1-PREM8_11) + max(0,BNCNPQCF-DIDABNCNPQ+DEFNIBNCQ+BNCNPDIBIS) * PREM8_11;

regle 861126:
application : iliad  ;
BNCNPHQF1 = (1-positif(BNCNPHQCF)) * positif_ou_nul(BNCNPQCF) * min(0,max(0,BNCNPHQCF-DIDABNCNPHQ)+BNCNPQCF)
             + positif(BNCNPHQCF) * max(0,BNCNPHQCF-DIDABNCNPHQ);
regle 861127:
application : iliad  ;
BNCNPQF1 = (1-positif(BNCNPHQCF)) * positif_ou_nul(BNCNPQCF) * max(0,BNCNPHQCF+BNCNPQCF-DIDABNCNPQ+DEFNIBNCQ1)
                   + positif(BNCNPHQCF) * max(0,BNCNPQCF-DIDABNCNPQ+DEFNIBNCQ1);
regle 861128:
application : iliad  ; 

SPENETPF = somme(i=V,C,P:SPENETPi) + SPENETCT ;

SPENETNPF = somme(i=V,C,P:SPENETNPi) + SPENETNPCT ;                                    
BNCNPTOT = SPENETPF + SPENETNPF ;

regle 861130:
application : iliad  ;                          
SPEPVPV = BNCPRO1AV - BNCPRODEV;
SPEPVPC = BNCPRO1AC - BNCPRODEC;
SPEPVPP = BNCPRO1AP - BNCPRODEP;

SPEPVNPV = BNCNP1AV - BNCNPDEV;
SPEPVNPC = BNCNP1AC - BNCNPDEC;
SPEPVNPP = BNCNP1AP - BNCNPDEP;
                                                                               
SPEPV = somme(i=V,C,P:max(0,SPEPVPi + SPEPVNPi)) ;

regle 861135:
application : iliad  ;                          


PVBNCFOYER = BN1 +SPEPV ;

regle 861140:
application : iliad  ;                          

DCTSPE = positif_ou_nul(BNRTOT+SPENETPF) * BNCPMVCTV
        + ( 1 - positif_ou_nul(BNRTOT+SPENETPF)) * (BNCPMVCTV-abs(BNRTOT+SPENETPF))
        + ( 1 - positif_ou_nul(BNRTOT+SPENETPF)) * null(BNCPMVCTV-abs(BNRTOT+SPENETPF))* BNCPMVCTV
	;
DCTSPENP = positif_ou_nul(NOCEPIMP+SPENETNPF) * BNCNPDCT
        + ( 1 - positif_ou_nul(NOCEPIMP+SPENETNPF)) * (BNCNPDCT-abs(NOCEPIMP+SPENETNPF))
        + ( 1 - positif_ou_nul(NOCEPIMP+SPENETNPF)) * null(BNCNPDCT-abs(NOCEPIMP+SPENETNPF)) * BNCNPDCT
	;
regle 861150:
application : iliad  ;

BNCDF1 = ((1-positif_ou_nul(BNCNPHQCF + BNCNPQCF)) * abs(BNCNPHQCF + BNCNPQCF)
                + positif_ou_nul(BNCNPHQCF + BNCNPQCF)
                * positif_ou_nul(DABNCNP5+DABNCNP4+DABNCNP3+DABNCNP2+DABNCNP1-BNCNPHQCF - BNCNPQCF)
                * (DABNCNP5+DABNCNP4+DABNCNP3+DABNCNP2+DABNCNP1-BNCNPHQCF - BNCNPQCF)
                * null(BNCDF6P+BNCDF5P+BNCDF4P+BNCDF3P+BNCDF2P)) * null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(DEFBNCNPF) * max(0,DEFBNCNPF-DIDABNCNP)
              + (1-positif(DEFBNCNPF)) *  max(0,-(NOCEPIMPNV +NOCEPIMPNC +NOCEPIMPNP+BNNAAV +BNNAAC +BNNAAP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)));

regle 861160:
application : iliad  ;                          
                                                                               
BNCDF2 = ((1-positif_ou_nul(BNCNPHQCF + BNCNPQCF)) * (DABNCNP1)
                + positif_ou_nul(BNCNPHQCF + BNCNPQCF)
                * min(max(BNCNPHQCF + BNCNPQCF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3-DABNCNP2,0)-DABNCNP1,DABNCNP1)*(-1)
                * positif_ou_nul(DABNCNP1-max(BNCNPHQCF + BNCNPQCF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3-DABNCNP2,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(DEFBNCNPF) * min(DABNCNP1,DEFBNCNPF+DABNCNP-DIDABNCNP-BNCDF1)
              + (1-positif(DEFBNCNPF)) *  min(DABNCNP1,DABNCNP-DIDABNCNP));

regle 861170:
application : iliad  ;                          

BNCDF3 = ((1 - positif_ou_nul(BNCNPHQCF + BNCNPQCF)) * (DABNCNP2)
                 + positif_ou_nul(BNCNPHQCF + BNCNPQCF)
                 * min(max(BNCNPHQCF + BNCNPQCF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3,0)-DABNCNP2,DABNCNP2)*(-1)
                 * positif_ou_nul(DABNCNP2-max(BNCNPHQCF + BNCNPQCF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(DEFBNCNPF) * min(DABNCNP2,DEFBNCNPF+DABNCNP-DIDABNCNP-BNCDF1-BNCDF2)
              + (1-positif(DEFBNCNPF)) *  min(DABNCNP2,DABNCNP-DIDABNCNP-BNCDF2));
regle 861180:
application : iliad  ;                          
                                                                               
BNCDF4 = ((1 - positif_ou_nul(BNCNPHQCF + BNCNPQCF)) * (DABNCNP3)
                 + positif_ou_nul(BNCNPHQCF + BNCNPQCF)
                 * min(max(BNCNPHQCF + BNCNPQCF-DABNCNP6-DABNCNP5-DABNCNP4,0)-DABNCNP3,DABNCNP3)*(-1)
                 * positif_ou_nul(DABNCNP3-max(BNCNPHQCF + BNCNPQCF-DABNCNP6-DABNCNP5-DABNCNP4,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(DEFBNCNPF) * min(DABNCNP3,DEFBNCNPF+DABNCNP-DIDABNCNP-BNCDF1-BNCDF2-BNCDF3)
              + (1-positif(DEFBNCNPF)) *  min(DABNCNP3,DABNCNP-DIDABNCNP-BNCDF2-BNCDF3));
regle 861190:
application : iliad  ;                          

BNCDF5 = ((1 - positif_ou_nul(BNCNPHQCF + BNCNPQCF)) * (DABNCNP4)
                 + positif_ou_nul(BNCNPHQCF + BNCNPQCF)
                 * min(max(BNCNPHQCF + BNCNPQCF-DABNCNP6-DABNCNP5,0)-DABNCNP4,DABNCNP4)*(-1)
                 * positif_ou_nul(DABNCNP4-max(BNCNPHQCF + BNCNPQCF-DABNCNP6-DABNCNP5,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(DEFBNCNPF) * min(DABNCNP4,DEFBNCNPF+DABNCNP-DIDABNCNP-BNCDF1-BNCDF2-BNCDF3-BNCDF4)
              + (1-positif(DEFBNCNPF)) *  min(DABNCNP4,DABNCNP-DIDABNCNP-BNCDF2-BNCDF3-BNCDF4));
regle 861200:
application : iliad ;                          

BNCDF6 = ((1 - positif_ou_nul(BNCNPHQCF + BNCNPQCF)) * (DABNCNP5)
                 + positif_ou_nul(BNCNPHQCF + BNCNPQCF)
                 * min(max(BNCNPHQCF + BNCNPQCF-DABNCNP6,0)-DABNCNP5,DABNCNP5)*(-1)
                 * positif_ou_nul(DABNCNP5-max(BNCNPHQCF + BNCNPQCF-DABNCNP6,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(DEFBNCNPF) * min(DABNCNP5,DEFBNCNPF+DABNCNP-DIDABNCNP-BNCDF1-BNCDF2-BNCDF3-BNCDF4-BNCDF5)
              + (1-positif(DEFBNCNPF)) *  min(DABNCNP5,DABNCNP-DIDABNCNP-BNCDF2-BNCDF3-BNCDF4-BNCDF5));

regle 86917:
application : iliad ;

BNCDF2P = ((1-positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)) * (DABNCNP1)
                + positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)
                * min(max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3-DABNCNP2,0)-DABNCNP1,DABNCNP1)*(-1)
                * positif_ou_nul(DABNCNP1-max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3-DABNCNP2,0)));
BNCDF3P = ((1 - positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)) * (DABNCNP2)
                 + positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)
                 * min(max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3,0)-DABNCNP2,DABNCNP2)*(-1)
                 * positif_ou_nul(DABNCNP2-max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3,0)));
BNCDF4P = ((1 - positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)) * (DABNCNP3)
                 + positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)
                 * min(max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6-DABNCNP5-DABNCNP4,0)-DABNCNP3,DABNCNP3)*(-1)
                 * positif_ou_nul(DABNCNP3-max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6-DABNCNP5-DABNCNP4,0)));
BNCDF5P = ((1 - positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)) * (DABNCNP4)
                 + positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)
                 * min(max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6-DABNCNP5,0)-DABNCNP4,DABNCNP4)*(-1)
                 * positif_ou_nul(DABNCNP4-max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6-DABNCNP5,0)));
BNCDF6P = ((1 - positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)) * (DABNCNP5)
                 + positif_ou_nul(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP)
                 * min(max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6,0)-DABNCNP5,DABNCNP5)*(-1)
                 * positif_ou_nul(DABNCNP5-max(NOCEPIMP+SPENETNPF+NOCEPIMPQV + NOCEPIMPQC + NOCEPIMPQP-DABNCNP6,0)));

regle 861205:
application : iliad ;


DIDABNCNPHQ = max(0 , min(BNCNPHQCF, DABNCNP1+DABNCNP2+DABNCNP3+DABNCNP4+DABNCNP5+DABNCNP6)) ;
DIDABNCNPQ = max(0 ,min(BNCNPQCF, DABNCNP1+DABNCNP2+DABNCNP3+DABNCNP4+DABNCNP5+DABNCNP6-DIDABNCNPHQ)) ;
DIDABNCNP = max(0 , min(BNCNPHQCF + BNCNPQCF, DABNCNP1+DABNCNP2+DABNCNP3+DABNCNP4+DABNCNP5+DABNCNP6)) ;

regle 861209:
application : iliad  ;



BNCIFBIS =  abs(min(0 , BNCNPHQCF +BNCNPQCF -DABNCNP1-DABNCNP2-DABNCNP3-DABNCNP4-DABNCNP5-DABNCNP6))
                      * (1-positif(BNCNPHQCF +BNCNPQCF))
                             + max(0,DABNCNP1+DABNCNP2+DABNCNP3+DABNCNP4+DABNCNP5+DABNCNP6-BNCNPHQCF -BNCNPQCF)
                        * positif(BNCNPHQCF +BNCNPQCF);
BNCIFBISQ =  abs(min(0 , BNCNPQCF -max(0,BNCNPHQCF-DABNCNP1-DABNCNP2-DABNCNP3-DABNCNP4-DABNCNP5-DABNCNP6))) ;
regle 861207:
application : iliad ;


BNCIF =  max(0 , BNCNPHQF) ;

regle 861210:
application : iliad ;

DABNCNP = DABNCNP1 + DABNCNP2 + DABNCNP3 + DABNCNP4 + DABNCNP5 + DABNCNP6 ;


DABNCNPF =  max(0 , min(BNCNPHQCF + BNCNPQCF, DABNCNP1+DABNCNP2+DABNCNP3+DABNCNP4+DABNCNP5+DABNCNP6)) ; 


DABNCNPV = arr(DABNCNPF * (BNCNPHQCV + BNCNPQCV)/(max(0,BNCNPHQCV)+BNCNPQCV+max(0,BNCNPHQCC)+BNCNPQCC+max(0,BNCNPHQCP)+BNCNPQCP))*positif(BNCNPHQCV + BNCNPQCV);
DABNCNPC = arr(DABNCNPF * (BNCNPHQCC + BNCNPQCC)/(max(0,BNCNPHQCV)+BNCNPQCV+max(0,BNCNPHQCC)+BNCNPQCC+max(0,BNCNPHQCP)+BNCNPQCP))*positif(BNCNPHQCC + BNCNPQCC);
DABNCNPP = arr(DABNCNPF * (BNCNPHQCP + BNCNPQCP)/(max(0,BNCNPHQCV)+BNCNPQCV+max(0,BNCNPHQCC)+BNCNPQCC+max(0,BNCNPHQCP)+BNCNPQCP))*positif(BNCNPHQCP + BNCNPQCP);

BNCHQV = max(0,BNCNPHQCV - DABNCNPV) ;
BNCHQC = max(0,BNCNPHQCC - DABNCNPC) ;
BNCHQP = max(0,BNCNPHQCP - DABNCNPP) ;

BNCQV = max(0 , BNCNPQCV - max(0 , DABNCNPV - BNCNPHQCV)) ;
BNCQC = max(0 , BNCNPQCC - max(0 , DABNCNPC - BNCNPHQCC)) ;
BNCQP = max(0 , BNCNPQCP - max(0 , DABNCNPP - BNCNPHQCP)) ;

regle 861218:
application : iliad ;


BNCNPRBIS=BNCAABV+arr(ANOCEP*MAJREV20)+BNCAABC+arr(ANOVEP*MAJREV20)+BNCAABP+arr(ANOPEP*MAJREV20)+COD5XS+arr(COD5XX*MAJREV20)+COD5YS+arr(COD5YX*MAJREV20)+COD5ZS+arr(COD5ZX*MAJREV20)+
          CODCJG+arr(CODCNS*MAJREV20)+CODCOS+arr(CODCRF*MAJREV20)+CODCSF+arr(CODCSN*MAJREV20);



BNCNPFBIS = BNCNPRBIS - (BNCNPDCT + COD5LD + COD5MD + BNCNPHQCV + BNCNPHQCC + BNCNPHQCP + BNCNPQCV + BNCNPQCC + BNCNPQCP) ;


BNCNPNBIS = BNCNPDCT + COD5LD + COD5MD + DIDABNCNP ;

SOMDBNC = BNHDEV + BNCDEV+BNHDEC + BNCDEC+BNHDEC + BNCDEC+BNCAADV+BNCAADC+BNCAADP+DNOCEP+DNOCEPC+DNOCEPP+BNCNPDCT+COD5LD+COD5MD;
DEFBNCNP = arr(SPENETNPV+SPENETNPC+SPENETNPP+BNCNPPVV+BNCNPPVC+BNCNPPVP+BNCAABV+ANOCEP*MAJREV20+BNCAABC+ANOVEP*MAJREV20+BNCAABP+ANOPEP*MAJREV20
               + COD5XS + COD5XX*MAJREV20 +COD5YS + COD5YX*MAJREV20 + COD5ZS + COD5ZX*MAJREV20 + CODCJG + CODCNS*MAJREV20 + CODCOS + CODCRF*MAJREV20 + CODCSF + CODCSN*MAJREV20) ;
DEFBNCNPH470 = (SPENETNPV+SPENETNPC+SPENETNPP+BNCNPPVV+BNCNPPVC+BNCNPPVP+BNCAABV+ANOCEP*MAJREV20+BNCAABC+ANOVEP*MAJREV20+BNCAABP+ANOPEP*MAJREV20
               + COD5XS + COD5XX*MAJREV20 + COD5YS + COD5YX*MAJREV20 + COD5ZS + COD5ZX*MAJREV20 + CODCJG + CODCNS*MAJREV20 + CODCOS + CODCRF*MAJREV20 + CODCSF + CODCSN*MAJREV20) ;
DEFBNCNP470 =  max(0,-BNCNPHQCV-BNCNPHQCC-BNCNPHQCP-BNCNPQCV-BNCNPQCC-BNCNPQCP);

regle 861220:
application : iliad ;

DEFBNCNPFBIS = ((1-PREM8_11) * (positif(BNCNPHQCF1731+BNCNPQCF1731) * positif(BNCNPHQCF+BNCNPQCF)
                                   * positif(BNCNPHQCF+BNCNPQCF-(BNCNPHQCF1731+BNCNPQCF1731))
                                        * max(0,DIDABNCNP- max(0,SOMBNCDF1731 - SOMBNCDF)-BNCNPHQCF1731-BNCNPQCF1731)
                            + (1-positif(BNCNPHQCF1731+BNCNPQCF1731)) * positif(BNCNPHQCF+BNCNPQCF)
                                      * max(0,DIDABNCNP- max(0,SOMBNCDF1731 - SOMBNCDF)+(-1)*(BNCNPHQCF1731+BNCNPQCF1731))
                               )
              + PREM8_11 * max(0,((-1)*(BNCNPHQCF1731+BNCNPQCF1731)-(-1)*(BNCNPHQCF+BNCNPQCF) * (1-positif(BNCNPHQCF+BNCNPQCF)) + DIDABNCNP -BNCNPDCT - COD5LD - COD5MD)
	                                                      * (1-positif(BNCNPHQCF1731+BNCNPQCF1731))
                		+ (DIDABNCNP + SOMBNCDF1731) * positif(BNCNPHQCF1731+BNCNPQCF1731)
							      )
                            )    * null(V_IND_TRAIT - 5);
DEFBNCNPF = (arr(max(0,DEFBNCNPFBIS * (1-BNCNPDPROBIS)))* (1-positif(positif(BNCNPHQCF1731+BNCNPQCF1731)+PREM8_11))
               + DEFBNCNPFBIS * positif(positif(BNCNPHQCF1731+BNCNPQCF1731)+PREM8_11)) * null(V_IND_TRAIT - 5) * positif(ART1731BIS);
regle 861230:
application : iliad ;


BNCNPDIBIS = CODCJG + arr(CODCNS*MAJREV20) + CODCOS + arr(CODCRF*MAJREV20) + CODCSF + arr(CODCSN*MAJREV20)
             - max(0 , BNCNPQCV + BNCNPQCC + BNCNPQCP + min(0 , BNCNPHQNCV + BNCNPHQNCC + BNCNPHQNCP)) ;


BNCNPDPROBIS = BNCNPDIBIS/BNCNPFBIS ;

regle 861240:
application : iliad ;


DEFNIBNCQ1=max(0,DIDABNCNP-max(0,BNCNPQCV+BNCNPQCC+BNCNPQCP));


DEFNIBNCQ = max(0,DEFBNCNPFBIS - DEFBNCNPF);
regle 861250:
application : iliad ;

MICROBNCV = SPENETPV + BNCPROPVV - BNCPMVCTV ;
MICROBNCC = SPENETPC + BNCPROPVC - BNCPMVCTC ; 
MICROBNCP = SPENETPP + BNCPROPVP - BNCPMVCTP ;

regle 862000:
application : iliad ;


IMPUT5JU = positif(SPENETNPV+SPENETNPC+SPENETNPP+BNCNPPVV+BNCNPPVC+BNCNPPVP+max(0,BNCNPHQCV+BNCNPHQCC+BNCNPHQCP+BNCNPQCV+BNCNPQCC+BNCNPQCP))*
           min(SPENETNPV+SPENETNPC+SPENETNPP+BNCNPPVV+BNCNPPVC+BNCNPPVP+max(0,BNCNPHQCV+BNCNPHQCC+BNCNPHQCP+BNCNPQCV+BNCNPQCC+BNCNPQCP) , BNCNPDCT+COD5LD+COD5MD) ;


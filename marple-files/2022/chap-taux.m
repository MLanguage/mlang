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
regle 201600:
application : iliad ;

INDPASV = positif( TSHALLOV + COD1GA + ALLOV + FRNV + CARTSV + REMPLAV + COD1GE + COD1AA + COD1GH + COD1AD
                   + PRBRV + PENINV + CARPEV + CODRAZ + PEBFV + COD1AI + CODRAI
                   + COD1AG + CODRAG + COD1GB + COD1GF + COD1GG + PALIV + PENSALV + COD1AM + CODRAM
                   + COD5XB + BAFORESTV + BACREV + BACDEV
                   + BAHREV + BAHDEV + 4BACREV + 4BAHREV + BAFPVV + COD5XO + COD5XT + COD5XV
                   + MIBVENV + MIBPRESV + MIBPVV + BICPMVCTV + BICNOV + BICDNV + BIHNOV + BIHDNV + MIBMEUV 
                   + MIBGITEV + LOCGITV + COD5NW + LOCNPCGAV + LOCGITCV + LOCDEFNPCGAV + COD5WE + COD5WF
		   + LOCNPV + LOCGITHCV + LOCDEFNPV + MIBNPVENV + MIBNPPRESV + MIBNPPVV + MIBNPDCT + BICREV 
		   + BICDEV + BICHREV + BICHDEV + CODCKC + CODCKI + CODCNC + CODCNI
                   + BNCPROV + BNCPROPVV + BNCPMVCTV + BNCREV + BNCDEV + BNHREV + BNHDEV + BNCNPV + BNCNPPVV
		   + BNCNPDCT + BNCAABV + BNCAADV + ANOCEP + DNOCEP + CODCQC + CODCQI + CODCJG + CODCSN + 0) ; 

INDPASC = positif( TSHALLOC + COD1HA + ALLOC + FRNC + CARTSC + REMPLAC + COD1HE + COD1BA + COD1HH + COD1BD
                   + PRBRC + PENINC + CARPEC + CODRBZ + PEBFC + COD1BI + CODRBI
                   + COD1BG + CODRBG + COD1HB + COD1HF + COD1HG + PALIC + PENSALC + COD1BM + CODRBM
                   + COD5YB + BAFORESTC + BACREC + BACDEC 
                   + BAHREC + BAHDEC + 4BACREC + 4BAHREC + BAFPVC + COD5YO + COD5XU + COD5XW
                   + MIBVENC + MIBPRESC + MIBPVC + BICPMVCTC + BICNOC + BICDNC + BIHNOC + BIHDNC + MIBMEUC 
                   + MIBGITEC + LOCGITC + COD5OW + LOCNPCGAC + LOCGITCC + LOCDEFNPCGAC + COD5XE + COD5XF
		   + LOCNPC + LOCGITHCC + LOCDEFNPC + MIBNPVENC + MIBNPPRESC + MIBNPPVC + COD5RZ + BICREC 
		   + BICDEC + BICHREC + BICHDEC + CODCLC + CODCLI + CODCOC + CODCOI
                   + BNCPROC + BNCPROPVC + BNCPMVCTC + BNCREC + BNCDEC + BNHREC + BNHDEC + BNCNPC + BNCNPPVC
		   + COD5LD + BNCAABC + BNCAADC + ANOVEP + DNOCEPC + CODCRC + CODCRI + CODCRF + CODCNS + 0) ;

INDPASP = positif( TSHALLO1 + TSHALLO2 + TSHALLO3 + TSHALLO4 + ALLO1 + ALLO2 + ALLO3 + ALLO4
                   + COD1IE + COD1JE + COD1KE + COD1LE + COD1CA + COD1DA + COD1EA + COD1FA
		   + COD1IH + COD1JH + COD1KH + COD1LH + COD1CD + COD1DD + COD1ED + COD1FD
                   + FRN1 + FRN2 + FRN3 + FRN4 + CARTSP1 + CARTSP2 + CARTSP3 + CARTSP4
		   + REMPLAP1 + REMPLAP2 + REMPLAP3 + REMPLAP4 + COD1IA + COD1JA + COD1KA + COD1LA
		   + PRBR1 + PRBR2 + PRBR3 + PRBR4 + PENIN1 + PENIN2 + PENIN3 + PENIN4
		   + CARPEP1 + CARPEP2 + CARPEP3 + CARPEP4 + CODRCZ + CODRDZ + CODREZ + CODRFZ
		   + PEBF1 + PEBF2 + PEBF3 + PEBF4 + COD1CI + COD1DI + COD1EI + COD1FI + CODRCK
		   + COD1CG + COD1DG + COD1EG + COD1FG
		   + COD1IG + COD1JG + COD1KG + COD1LG
		   + CODRCG + CODRDG + CODRGG + CODRFG + COD1IB + COD1JB 
		   + COD1IF + COD1JF + COD1KF + COD1LF
		   + PALI1 + PALI2 + PALI3 + PALI4 + PENSALP1 + PENSALP2 + PENSALP3 + PENSALP4
		   + COD1CM + COD1DM + COD1EM + COD1FM + CODRCM + CODRDM + CODREM + CODRFM
		   + COD5ZB + BAFORESTP + BACREP + BACDEP + BAHREP + BAHDEP + 4BACREP + 4BAHREP
		   + BAFPVP + COD5ZO
		   + MIBVENP + MIBPRESP + MIBPVP + BICPMVCTP + BICNOP + BICDNP + BIHNOP + BIHDNP + MIBMEUP + MIBGITEP
		   + LOCGITP + COD5PW + LOCNPCGAPAC + LOCGITCP + LOCDEFNPCGAPAC + LOCNPPAC + COD5YE + COD5YF
		   + LOCGITHCP + LOCDEFNPPAC + MIBNPVENP + MIBNPPRESP + MIBNPPVP + COD5SZ + BICREP + BICDEP + BICHREP
		   + BICHDEP + CODCMC + CODCMI + CODCPC + CODCPI
		   + BNCPROP + BNCPROPVP + BNCPMVCTP + BNCREP + BNCDEP + BNHREP + BNHDEP + BNCNPP + BNCNPPVP + COD5MD
		   + BNCAABP + BNCAADP + ANOPEP + DNOCEPP + CODCSC + CODCSI + CODCSF + CODCOS) ;

INDPASF = positif( RVB1 + RVB2 + RVB3 + RVB4 + RENTAX + RENTAX5 + RENTAX6 + RENTAX7
                   + DAGRI6 + DAGRI5 + DAGRI4 + DAGRI3 + DAGRI2 + DAGRI1
                   + LNPRODEF10 + LNPRODEF9 + LNPRODEF8 + LNPRODEF7 + LNPRODEF6
                   + LNPRODEF5 + LNPRODEF4 + LNPRODEF3 + LNPRODEF2 + LNPRODEF1
	           + DEFBIC6 + DEFBIC5 + DEFBIC4 + DEFBIC3 + DEFBIC2 + DEFBIC1
                   + DABNCNP6 + DABNCNP5 + DABNCNP4 + DABNCNP3 + DABNCNP2 + DABNCNP1
                   + RFORDI + RFDORD + RFDHIS + RFDANT + RFMIC + FONCI + REAMOR + CODRBE + 0) ;

INDPAS = ((2 * (1 - positif(INDPASV + INDPASC + INDPASP + INDPASF)) * positif(IDRS4 + IBATMARG)) + (1 - ((1 - positif(INDPASV + INDPASC + INDPASP + INDPASF)) * positif(IDRS4 + IBATMARG)))) * (1 - positif(ANNUL2042))
          + 2 * positif(ANNUL2042) ;

IRTOTAL = (IDRS3 + IBATMARG - IDEC) * ((1 - V_CNR * (1 - INDTXMOY)) * positif_ou_nul(IDRS3 + IBATMARG - IDEC - SEUIL_61) 
	                                         + V_CNR * (1 - INDTXMOY) * positif_ou_nul(IDRS3 + IBATMARG - IDEC - SEUIL_TXMIN)) ;

regle 201620:
application : iliad ;

TSRASF = PASTSNTV + PASPRNV + PASTSNTC + PASPRNC + PASTSNTP + PASPRNP ;

TSPETAUXF = PASTSN1AG + PASTSN1BG + max(0 , TSN1CG + TSNRCG) + max(0 , TSN1DG + TSNRDG) + max(0 , TSN1EG + TSNRGG) + max(0 , TSN1FG + TSNRFG)
            + PASPRNAOM + PASPRNBOM + max(0 , PRN1CO + PRN1CM + PRNRCO + PRNRCM) + max(0 , PRN1DO + PRN1DM + PRNRDO + PRNRDM)
	    + max(0 , PRN1EO + PRN1EM + PRNREO + PRNREM) + max(0 , PRN1FO + PRN1FM + PRNRFO + PRNRFM) ;

BAPASV = max(0 , arr(IBAMICV + BAFORESTV + R15HC + R2MAJ5HI
                     - arr(DEFANTBAV * ((IBAMICV + BAFORESTV + R15HC + R2MAJ5HI) / BAHQNODEFV)))) + BAQV + BATMARGV ;

BAPASC = max(0 , arr(IBAMICC + BAFORESTC + R15IC + R2MAJ5II
                     - arr(DEFANTBAC * ((IBAMICC + BAFORESTC + R15IC + R2MAJ5II) / BAHQNODEFC)))) + BAQC + BATMARGC ;

BAPASP = max(0 , arr(IBAMICP + BAFORESTP + R15JC + R2MAJ5JI
                     - arr(DEFANTBAP * ((IBAMICP + BAFORESTP + R15JC + R2MAJ5JI) / BAHQNODEFP)))) + BAQP ;

BATAUXF = BAPASV + BAPASC + BAPASP ;

BICPASVP = MIB_NETVV + MIB_NETPV + MIBPVV - BICPMVCTV + R15KC + R2MAJ5KI + BIPTAQV + BIHTAQV ;
BICPASVNP = max(0 , max(0 , MIB_NETNPVV + MIB_NETNPPV + MIBNPPVV - MIBNPDCT
                            + arr(R15NC + R2MAJ5NI - (DEFANTBICNPV * (MIB_NETNPVV + MIB_NETNPPV + MIBNPPVV - MIBNPDCT + R15NC + R2MAJ5NI) / BICNPONCV)))
	            + BICQV) ;
BICPASVLNP = max(0 , MLOCNETV + SNPLOCPASV + RNPLOCPASV - arr(DEFANTLOCV * (SNPLOCPASV + RNPLOCPASV + MLOCNETV) / RNPILOCV)) ;
BICPASCP = MIB_NETVC + MIB_NETPC + MIBPVC - BICPMVCTC + R15LC + R2MAJ5LI + BIPTAQC + BIHTAQC ;
BICPASCNP = max(0 , max(0 , MIB_NETNPVC + MIB_NETNPPC + MIBNPPVC - COD5RZ
                            + arr(R15OC + R2MAJ5OI - (DEFANTBICNPC * (MIB_NETNPVC + MIB_NETNPPC + MIBNPPVC - COD5RZ + R15OC + R2MAJ5OI) / BICNPONCC)))
	            + BICQC) ;
BICPASCLNP = max(0 , MLOCNETC + SNPLOCPASC + RNPLOCPASC - arr(DEFANTLOCC * (SNPLOCPASC + RNPLOCPASC + MLOCNETC) / RNPILOCC)) ;
BICPASPP = MIB_NETVP + MIB_NETPP + MIBPVP - BICPMVCTP + R15MC + R2MAJ5MI + BIPTAQP + BIHTAQP ;
BICPASPNP = max(0 , max(0 , MIB_NETNPVP + MIB_NETNPPP + MIBNPPVP - COD5SZ
                            + arr(R15PC + R2MAJ5PI - (DEFANTBICNPP * (MIB_NETNPVP + MIB_NETNPPP + MIBNPPVP - COD5SZ + R15PC + R2MAJ5PI) / BICNPONCP)))
	            + BICQP) ;
BICPASPLNP = max(0 , MLOCNETP + SNPLOCPASP + RNPLOCPASP - arr(DEFANTLOCP * (SNPLOCPASP + RNPLOCPASP + MLOCNETP) / RNPILOCP)) ;

BICPROTAUX = max(0 , BICPASVP + BICPASCP + BICPASPP) ;
BICNPROTAUX = max(0 , BICPASVNP + BICPASCNP + BICPASPNP) ;
LOCNPROTAUX = max(0 , BICPASVLNP + BICPASCLNP + BICPASPLNP) ;

BICPASV = max(0 , (MIB_NETVV + MIB_NETPV + MIBPVV - BICPMVCTV + R15KC + R2MAJ5KI + BIPTAQV + BIHTAQV) 
                  + max(0 , max(0 , MIB_NETNPVV + MIB_NETNPPV + MIBNPPVV - MIBNPDCT 
		                    + arr(R15NC + R2MAJ5NI - (DEFANTBICNPV * (MIB_NETNPVV + MIB_NETNPPV + MIBNPPVV - MIBNPDCT + R15NC + R2MAJ5NI) / BICNPONCV)))
                            + BICQV) 
                  + max(0 , MLOCNETV + SNPLOCPASV + RNPLOCPASV - arr(DEFANTLOCV * (SNPLOCPASV + RNPLOCPASV + MLOCNETV) / RNPILOCV))) ;

BICPASC = max(0 , (MIB_NETVC + MIB_NETPC + MIBPVC - BICPMVCTC + R15LC + R2MAJ5LI + BIPTAQC + BIHTAQC) 
                  + max(0 , max(0 , MIB_NETNPVC + MIB_NETNPPC + MIBNPPVC - COD5RZ 
		                    + arr(R15OC + R2MAJ5OI - (DEFANTBICNPC * (MIB_NETNPVC + MIB_NETNPPC + MIBNPPVC - COD5RZ + R15OC + R2MAJ5OI) / BICNPONCC)))
			    + BICQC) 
                  + max(0 , MLOCNETC + SNPLOCPASC + RNPLOCPASC - arr(DEFANTLOCC * (SNPLOCPASC + RNPLOCPASC + MLOCNETC) / RNPILOCC))) ;

BICPASP = max(0 , (MIB_NETVP + MIB_NETPP + MIBPVP - BICPMVCTP + R15MC + R2MAJ5MI + BIPTAQP + BIHTAQP) 
                  + max(0 , max(0 , MIB_NETNPVP + MIB_NETNPPP + MIBNPPVP - COD5SZ 
		                    + arr(R15PC + R2MAJ5PI - (DEFANTBICNPP * (MIB_NETNPVP + MIB_NETNPPP + MIBNPPVP - COD5SZ + R15PC + R2MAJ5PI) / BICNPONCP)))
			    + BICQP) 
                  + max(0 , MLOCNETP + SNPLOCPASP + RNPLOCPASP - arr(DEFANTLOCP * (SNPLOCPASP + RNPLOCPASP + MLOCNETP) / RNPILOCP))) ;

BICTAUXF = BICPASV + BICPASC + BICPASP ;

INDBICTAUX = null(BICTAUXF - BICPROTAUX - BICNPROTAUX - LOCNPROTAUX) * positif(BICTAUXF) * positif(BICPROTAUX + BICNPROTAUX + LOCNPROTAUX) ;

BNCPASVP = MICROBNCV + R15QC + R2MAJ5QI + R1CQC + R2MAJCQI ;
BNCPASVNP = max(0 , max(0 , SPENETNPV + BNCNPPVV - BNCNPDCT
                            + arr(R15JG + R2MAJ5SN - arr(DABNCNPV * (SPENETNPV + BNCNPPVV - BNCNPDCT + R15JG + R2MAJ5SN) / BNCNPHQNCV)))
	            + BNCNPQCV) ;
BNCPASCP = MICROBNCC + R15RC + R2MAJ5RI + R1CRC + R2MAJCRI ;
BNCPASCNP = max(0 , max(0 , SPENETNPC + BNCNPPVC - COD5LD
                            + arr(R15RF + R2MAJ5NS - arr(DABNCNPC * (SPENETNPC + BNCNPPVC - COD5LD + R15RF + R2MAJ5NS) / BNCNPHQNCC)))
	            + BNCNPQCC) ;
BNCPASPP = MICROBNCP + R15SC + R2MAJ5SI + R1CSC + R2MAJCSI ;
BNCPASPNP = max(0 , max(0 , SPENETNPP + BNCNPPVP - COD5MD
                            + arr(R15SF + R2MAJ5OS - arr(DABNCNPP * (SPENETNPP + BNCNPPVP - COD5MD + R15SF + R2MAJ5OS) / BNCNPHQNCP)))
                    + BNCNPQCP) ;

BNCPROTAUX = max(0 , BNCPASVP + BNCPASCP + BNCPASPP) ;
BNCNPROTAUX = max(0 , BNCPASVNP + BNCPASCNP + BNCPASPNP) ;

BNCPASV = max(0 , (MICROBNCV + R15QC + R2MAJ5QI + R1CQC + R2MAJCQI) 
                  + max(0 , max(0 , SPENETNPV + BNCNPPVV - BNCNPDCT 
		                    + arr(R15JG + R2MAJ5SN - arr(DABNCNPV * (SPENETNPV + BNCNPPVV - BNCNPDCT + R15JG + R2MAJ5SN) / BNCNPHQNCV))) 
			    + max(0 , BNCNPQCV - max(0 , DABNCNPV - BNCNPHQCV)))) ; 

BNCPASC = max(0 , (MICROBNCC + R15RC + R2MAJ5RI + R1CRC + R2MAJCRI) 
                  + max(0 , max(0 , SPENETNPC + BNCNPPVC - COD5LD 
		                    + arr(R15RF + R2MAJ5NS - arr(DABNCNPC * (SPENETNPC + BNCNPPVC - COD5LD + R15RF + R2MAJ5NS) / BNCNPHQNCC))) 
			    + max(0 , BNCNPQCC - max(0 , DABNCNPC - BNCNPHQCC)))) ;

BNCPASP = max(0 , (MICROBNCP + R15SC + R2MAJ5SI + R1CSC + R2MAJCSI) 
                  + max(0 , max(0 , SPENETNPP + BNCNPPVP - COD5MD 
		                    + arr(R15SF + R2MAJ5OS - arr(DABNCNPP * (SPENETNPP + BNCNPPVP - COD5MD + R15SF + R2MAJ5OS) / BNCNPHQNCP))) 
			    + max(0 , BNCNPQCP - max(0 , DABNCNPP - BNCNPHQCP)))) ;

BNCTAUXF = BNCPASV + BNCPASC + BNCPASP ;

INDBNCTAUX = null(BNCTAUXF - BNCPROTAUX - BNCNPROTAUX) * positif(BNCTAUXF) * positif(BNCPROTAUX + BNCNPROTAUX) ; 

AGASSUR = max(0 , TSN1GF) + max(0 , TSN1HF) + max(0 , TSN1IF) + max(0 , TSN1JF) + max(0 , TSN1KF) + max(0 , TSN1LF) 
          + max(0 , TSN1GG) + max(0 , TSN1HG) + max(0 , TSN1IG) + max(0 , TSN1JG) + max(0 , TSN1KG) + max(0 , TSN1LG) ;

GERANT = max(0 , TSN1GB) + max(0 , TSN1HB) + max(0 , TSN1IB) + max(0 , TSN1JB) ;

RFTAUXF = max(0 , R34BA - R4BL + R3RBA - RRBT + R3SBA + PASRFASS) ;

RVTOTAUXF = RV1 + RV2 + RV3 + RV4 + 2RV1 + 2RV2 + 2RV3 + 2RV4 ;

REVACOMP = TSPETAUXF + RVTOTAUXF + AGASSUR + GERANT + BATAUXF + BICTAUXF + BNCTAUXF + RFTAUXF ;

SALAGGE1 = PASTSN1AG + max(0 , TSN1GF) + max(0 , TSN1GG) + max(0 , TSN1GB) ;
SALAGGE2 = PASTSN1BG + max(0 , TSN1HF) + max(0 , TSN1HG) + max(0 , TSN1HB) ;
SALAGGEP = max(0 , TSN1CG + TSNRCG) + max(0 , TSN1DG + TSNRDG) + max(0 , TSN1EG + TSNRGG) + max(0 , TSN1FG + TSNRFG)
           + max(0 , TSN1IF) + max(0 , TSN1JF) + max(0 , TSN1KF) + max(0 , TSN1LF) + max(0 , TSN1IB) + max(0 , TSN1JB) 
	   + max(0 , TSN1IG) + max(0 , TSN1JG) + max(0 , TSN1KG) + max(0 , TSN1LG) ;

PASPRNPAC = max(0 , PRN1CO + PRN1CM + PRNRCO + PRNRCM) + max(0 , PRN1DO + PRN1DM + PRNRDO + PRNRDM)
	    + max(0 , PRN1EO + PRN1EM + PRNREO + PRNREM) + max(0 , PRN1FO + PRN1FM + PRNRFO + PRNRFM) ;

BAPAS1 = max(0 , BAHQV) + BAQV ;

BAPAS2 = max(0 , BAHQC) + BAQC ;

BAPAS3 = max(0 , BAHQP) + BAQP ;

BICPAS1 = max(0 , BICPROOCV + max(0 , BICHQV) + max(0 , PASRNPLOCFV)) + BICQV + BICPROQCV ;

BICPAS2 = max(0 , BICPROOCC + max(0 , BICHQC) + max(0 , PASRNPLOCFC)) + BICQC + BICPROQCC ;

BICPAS3 = max(0 , BICPROOCP + max(0 , BICHQP) + max(0 , PASRNPLOCFP)) + BICQP + BICPROQCP ;

BNCPAS1 = max(0 , BNCPHQCV + max(0 , BNCHQV)) + BNCQV + BNCPQCV ;

BNCPAS2 = max(0 , BNCPHQCC + max(0 , BNCHQC)) + BNCQC + BNCPQCC ;

BNCPAS3 = max(0 , BNCPHQCP + max(0 , BNCHQP)) + BNCQP + BNCPQCP ;

TONE1 = max(0, TOTALQUOHT - (SDDD + SDCC + SDVV + SDMM) * (1- positif(VARIPTEFP + VARIPTEFN+ TEFFREVTOT*INDTEFF)) - VARIPTEFN ) ;

REVRVGUA = positif(COD2OP) * inf(TONE1 * max(0 , CODRVG - CODRSG) / TOTALQUOHT)
           + (1 - positif(COD2OP)) * inf(TONE1 * CODRVG / TOTALQUOHT)
           + positif(COD2OP) * inf(TONE1 * max(0 , CODRUA - CODRVA - CODRSL) / TOTALQUOHT) ;

REVTOT = max(0 , TSPRV) + TSNN2VAFF + PENSTOTV + BAPAS1 + BICPAS1 + BNCPAS1 + PRNRAI
         + max(0 , TSPRC) + TSNN2CAFF + PENSTOTC + BAPAS2 + BICPAS2 + BNCPAS2 + PRNRBI
	 + max(0 , TSPR1) + max(0 , TSPR2) + max(0 , TSPR3) + max(0 , TSPR4) + TSNN2PAFF + PENSTOTP + BAPAS3 + BICPAS3 + BNCPAS3
         + PRNFAS + PRNFBS + PRNFCS + PRNFDS + PRNFES + PRNFFS + GLN3 + COD1TZ + RVTOT + T2RV + PRNRCK
	 + RRCM + REVRCM + PVBAR3VG + PVRVGRUA + max(0 , RRFI + 0) + REVRF + DESV + ESFP + RE168 + TAX1649 + PREREV + R1649
	 - (V_8ZT + CODZRE + CODZRF) * null(2 - V_REGCO) 
	 + BATMARGTOT ;

regle taux 201630:
application : iliad ;

NUMBADEC1 = PASTSNTV + PASPRNV + RASAUSA1 + RASASSO1 + PASPRNAOM + PASTSN1AG + PASPRN1AL ;
NUMBADEC2 = PASTSNTC + PASPRNC + RASAUSA2 + RASASSO2 + PASPRNBOM + PASTSN1BG + PASPRN1BL ;
NUMBAPAC = PASTSNTP + PASPRNP + RASGASSUP + RASASSOP + PASPRNPAC + PASTSN1PAC + PASPRN1PAC ;
NUMBA = TSRASF + AGASSUR + GERANT + TSPETAUXF ;

regle taux 201640:
application : iliad ;
 
TSTAUXRASV = TSHALLOV + COD1GA + ALLOV + (SALEXTV * positif(COD1GE)) + COD1AA + COD1GHRET + COD1ADRET 
             + CARTSV + REMPLAV + CODDAJ + CODEAJ + PRBRV + PENINV + COD1AI + CARPEV + CODRAZ + PEBFV + CODRAI ;

TSTAUXRASC = TSHALLOC + COD1HA + ALLOC + (SALEXTC * positif(COD1HE)) + COD1BA + COD1HHRET + COD1BDRET
             + CARTSC + REMPLAC + CODDBJ + CODEBJ + PRBRC + PENINC + COD1BI + CARPEC + CODRBZ + PEBFC + CODRBI ;

TSTAUXRASP = TSHALLO1 + COD1IA + ALLO1 + (SALEXT1 * positif(COD1IE)) + COD1CA + COD1IHRET + COD1CDRET + CARTSP1 + REMPLAP1 
             + TSHALLO2 + COD1JA + ALLO2 + (SALEXT2 * positif(COD1JE)) + COD1DA + COD1JHRET + COD1DDRET + CARTSP2 + REMPLAP2
	     + TSHALLO3 + COD1KA + ALLO3 + (SALEXT3 * positif(COD1KE)) + COD1EA + COD1KHRET + COD1EDRET + CARTSP3 + REMPLAP3 
	     + TSHALLO4 + COD1LA + ALLO4 + (SALEXT4 * positif(COD1LE)) + COD1FA + COD1LHRET + COD1FDRET + CARTSP4 + REMPLAP4
	     + PRBR1 + PENIN1 + COD1CI + CARPEP1 + CODRCZ + PEBF1 + PRBR2 + PENIN2 + COD1DI + CARPEP2 + CODRDZ + PEBF2
	     + PRBR3 + PENIN3 + COD1EI + CARPEP3 + CODREZ + PEBF3 + PRBR4 + PENIN4 + COD1FI + CARPEP4 + CODRFZ + PEBF4 
	     + CODRCK ;

TSTAUXRAS = TSTAUXRASV + TSTAUXRASC + TSTAUXRASP ;

PASDENF = TSTAUXRAS + REVACOMP ;

CIPAS = (COD8VM + COD8WM + COD8UM) * (1 - positif_ou_nul(COD8PA)) + min(COD8VM + COD8WM + COD8UM , COD8PA) * positif_ou_nul(COD8PA) ;

PASNUMF = max(0 , arr((IRTOTAL * min(1 , (TSRASF + REVACOMP) / REVTOT)) - CIPAS)) * (1 - null(REVTOT)) ;

RASTXFOYER = min(80 , arr(((PASNUMF / PASDENF) * 100 * (1 - null(PASDENF))) * 10) / 10 * (1 - null(2 - INDPAS))) * (1 - INDTAZ) ;

regle taux 201660:
application : iliad ;

RASTSPR1 = TSTAUXRASV * (1 - null(2 - INDPAS)) ;

RASTSPR2 = TSTAUXRASC * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASTSPE1 = (max(0 , TSN1AG) * (1 - positif(COD1GK)) + max(0 , PRN1AM + PRN1AO) * (1 - positif(COD1HK))) * (1 - null(2 - INDPAS)) ;

RASTSPE2 = (max(0 , TSN1BG) * (1 - positif(COD1GL)) + max(0 , PRN1BM + PRN1BO) * (1 - positif(COD1HL))) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASTSPEP = (max(0 , TSN1CG) * (1 - positif(COD1GP)) + max(0 , TSN1DG) * (1 - positif(COD1GQ)) + max(0 , TSN1EG) * (1 - positif(COD1GR)) + max(0 , TSN1FG) * (1 - positif(COD1GS))
            + max(0 , PRN1CO + PRN1CM) * (1 - positif(COD1HP)) + max(0 , PRN1DO + PRN1DM) * (1 - positif(COD1HQ)) + max(0 , PRN1EO + PRN1EM) * (1 - positif(COD1HR)) + max(0 , PRN1FO + PRN1FM)) * (1 - positif(COD1HS))
	   * (1 - null(2 - INDPAS)) ;

RASTSPE1N = (max(0 , TSN1AG) + max(0 , PRN1AM + PRN1AO)) * (1 - null(2 - INDPAS)) ;

RASTSPE2N = (max(0 , TSN1BG) + max(0 , PRN1BM + PRN1BO)) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASTSPEPN = (max(0 , TSN1CG) + max(0 , TSN1DG) + max(0 , TSN1EG) + max(0 , TSN1FG) 
             + max(0 , PRN1CO + PRN1CM) + max(0 , PRN1DO + PRN1DM) + max(0 , PRN1EO + PRN1EM) + max(0 , PRN1FO + PRN1FM)) * (1 - null(2 - INDPAS)) ;

RASRVTO = (RV1 + RV2 + RV3 + RV4) * (1 - null(2 - INDPAS)) ;

RASRVTO1 = arr(RASRVTO / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) ;

RASRVTO2 = (RASRVTO - RASRVTO1) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASAUSA1 = (max(0 , TSN1GF) + max(0 , TSN1GG)) * (1 - positif(COD1GK)) * (1 - null(2 - INDPAS)) ;

RASAUSA2 = (max(0 , TSN1HF) + max(0 , TSN1HG)) * (1 - positif(COD1GL)) * (1 - null(2 - INDPAS)) ;

RASGASSUP = ((max(0 , TSN1IF) + max(0 , TSN1IG)) * (1 - positif(COD1GP)) + (max(0 , TSN1JF) + max(0 , TSN1JG)) * (1 - positif(COD1GQ))
             + (max(0 , TSN1KF) + max(0 , TSN1KG)) * (1 - positif(COD1GR)) + (max(0 , TSN1LF) + max(0 , TSN1LG)) * (1 - positif(COD1GS))) * (1 - null(2 - INDPAS)) ;

RASAUSA1N = (max(0 , TSN1GF) + max(0 , TSN1GG)) * (1 - null(2 - INDPAS)) ;

RASAUSA2N = (max(0 , TSN1HF) + max(0 , TSN1HG)) * (1 - null(2 - INDPAS)) ;

RASGASSUPN = (max(0 , TSN1IF) + max(0 , TSN1IG) + max(0 , TSN1JF) + max(0 , TSN1JG)
             + max(0 , TSN1KF) + max(0 , TSN1KG) + max(0 , TSN1LF) + max(0 , TSN1LG)) * (1 - null(2 - INDPAS)) ;

RASASSO1 = max(0 , TSN1GB) * (1 - positif(COD1GK)) * (1 - null(2 - INDPAS)) ;

RASASSO2 = max(0 , TSN1HB) * (1 - positif(COD1GL)) * (1 - null(2 - INDPAS)) ;

RASASSOP = (max(0 , TSN1IB) * (1 - positif(COD1GP)) + max(0 , TSN1JB) * (1 - positif(COD1GQ))) * (1 - null(2 - INDPAS)) ;

RASASSO1N = max(0 , TSN1GB) * (1 - null(2 - INDPAS)) ;

RASASSO2N = max(0 , TSN1HB) * (1 - null(2 - INDPAS)) ;

RASASSOPN = (max(0 , TSN1IB) + max(0 , TSN1JB)) * (1 - null(2 - INDPAS)) ;

RASRF = max(0 , R34BA - R4BL + RMFN - R4BK) * (1 - positif(COD4BN)) * (1 - null(2 - INDPAS)) ;

RASRFN = max(0 , R34BA - R4BL + RMFN - R4BK) * (1 - null(2 - INDPAS)) ;
                            
RASRF1 = arr(RASRF / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) ;

RASRF2 = (RASRF - RASRF1) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASBA1 = (max(0 , arr((BAMICV + BAFORESTV + PASBACV + PASBAHV) * 12 / min(12 , COD5AD + 12 * null(COD5AD + 0)))
                  - arr(DEFANTBAV * ((IBAMICV + BAFORESTV + R15HC + R2MAJ5HI) / BAHQNODEFV))) + BATMARGV)
         * (1 - positif(COD5AF)) * (1 - null(2 - INDPAS)) ;

RASBA2 = (max(0 , arr((BAMICC + BAFORESTC + PASBACC + PASBAHC) * 12 / min(12 , COD5BD + 12 * null(COD5BD + 0)))
                  - arr(DEFANTBAC * ((IBAMICC + BAFORESTC + R15IC + R2MAJ5II) / BAHQNODEFC))) + BATMARGC)
         * (1 - positif(COD5AI)) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASBAP = max(0 , arr((BAMICP + BAFORESTP + PASBACP + PASBAHP) * 12 / min(12 , COD5ED + 12 * null(COD5ED + 0)))
                 - arr(DEFANTBAP * ((IBAMICP + BAFORESTP + R15JC + R2MAJ5JI) / BAHQNODEFP)))
         * (1 - positif(COD5AH)) * (1 - null(2 - INDPAS)) ;

RASBA1N = (max(0 , (BAMICV + BAFORESTV + PASBACV + PASBAHV) 
                   - arr(DEFANTBAV * ((IBAMICV + BAFORESTV + R15HC + R2MAJ5HI) / BAHQNODEFV))) + BATMARGV)
          * (1 - null(2 - INDPAS)) ;

RASBA2N = (max(0 , (BAMICC + BAFORESTC + PASBACC + PASBAHC) 
                   - arr(DEFANTBAC * ((IBAMICC + BAFORESTC + R15IC + R2MAJ5II) / BAHQNODEFC))) + BATMARGC)
          * BOOL_0AM * (1 - null(2 - INDPAS)) ;
          
RASBAPN = max(0 , (BAMICP + BAFORESTP + PASBACP + PASBAHP) 
                  - arr(DEFANTBAP * ((IBAMICP + BAFORESTP + R15JC + R2MAJ5JI) / BAHQNODEFP)))
          * (1 - null(2 - INDPAS)) ;
          
RASBIC1 = BICASSV * (1 - null(2 - INDPAS)) ;

RASBIC2 = BICASSC * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASBICP = BICASSP * (1 - null(2 - INDPAS)) ;

RASBIC1N = BICASSVN * (1 - null(2 - INDPAS)) ;

RASBIC2N = BICASSCN * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASBICPN = BICASSPN * (1 - null(2 - INDPAS)) ;

RASBNC1 = max(0 , arr((SPENETPV + PASBNNAV + PASBNNSV) * 12 / min(12 , COD5XI + 12 * null(COD5XI + 0))) * (1 - positif(COD5AO))
                  + max(0 , arr((SPENETNPV + PASBNNAAV + PASNOCEPIMPV) * 12 / min(12 , COD5XR + 12 * null(COD5XR + 0)))
                            - arr(DABNCNPV * (SPENETNPV + BNCNPPVV - BNCNPDCT + R15JG + R2MAJ5SN) / BNCNPHQNCV)) * (1 - positif(COD5AP)))
          * (1 - null(2 - INDPAS)) ;

RASBNC2 = max(0 , arr((SPENETPC + PASBNNAC + PASBNNSC) * 12 / min(12 , COD5YI + 12 * null(COD5YI + 0))) * (1 - positif(COD5BO))
                  + max(0 , arr((SPENETNPC + PASBNNAAC + PASNOCEPIMPC) * 12 / min(12 , COD5YR + 12 * null(COD5YR + 0)))
                            - arr(DABNCNPC * (SPENETNPC + BNCNPPVC - COD5LD + R15RF + R2MAJ5NS) / BNCNPHQNCC)) * (1 - positif(COD5BP)))
          * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASBNCP = max(0 , arr((SPENETPP + PASBNNAP + PASBNNSP) * 12 / min(12 , COD5ZI + 12 * null(COD5ZI + 0))) * (1 - positif(COD5CQ))
                  + max(0 , arr((SPENETNPP + PASBNNAAP + PASNOCEPIMPP) * 12 / min(12 , COD5ZR + 12 * null(COD5ZR + 0)))
                            - arr(DABNCNPP * (SPENETNPP + BNCNPPVP - COD5MD + R15SF + R2MAJ5OS) / BNCNPHQNCP)) * (1 - positif(COD5CR)))
          * (1 - null(2 - INDPAS)) ;

RASBNC1N = max(0 , (SPENETPV + PASBNNAV + PASBNNSV) 
                   + max(0 , (SPENETNPV + PASBNNAAV + PASNOCEPIMPV) 
		             - arr(DABNCNPV * (SPENETNPV + BNCNPPVV - BNCNPDCT + R15JG + R2MAJ5SN) / BNCNPHQNCV)))
	   * (1 - null(2 - INDPAS)) ;

RASBNC2N = max(0 , (SPENETPC + PASBNNAC + PASBNNSC) 
                   + max(0 , (SPENETNPC + PASBNNAAC + PASNOCEPIMPC) 
		             - arr(DABNCNPC * (SPENETNPC + BNCNPPVC - COD5LD + R15RF + R2MAJ5NS) / BNCNPHQNCC)))
           * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASBNCPN = max(0 , (SPENETPP + PASBNNAP + PASBNNSP) 
                   + max(0 , (SPENETNPP + PASBNNAAP + PASNOCEPIMPP) 
	 	             - arr(DABNCNPP * (SPENETNPP + BNCNPPVP - COD5MD + R15SF + R2MAJ5OS) / BNCNPHQNCP)))
 	   * (1 - null(2 - INDPAS)) ;

RASPAC = RASBAP + RASBICP + RASBNCP + RASTSPEP + RASGASSUP + RASASSOP ;

RASPAC1 = arr(RASPAC / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) ;

RASPAC2 = (RASPAC - RASPAC1) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

regle 201680:
application : iliad ;

REVDEC1 = PASTSNTV + PASPRNV + PASTSN1AG + PASPRNAOM + BAPASV + BICPASV + BNCPASV + max(0 , TSN1GB) + max(0 , TSN1GF) + max(0 , TSN1GG) ;

REVDEC2 = PASTSNTC + PASPRNC + PASTSN1BG + PASPRNBOM + BAPASC + BICPASC + BNCPASC + max(0 , TSN1HB) + max(0 , TSN1HF) + max(0 , TSN1HG) ;

REVTSI = (PASTSNTV + PASPRNV + PASTSN1AG + PASPRNAOM + max(0 , TSN1GB) + max(0 , TSN1GF) + max(0 , TSN1GG)) * null(1 - TXPASMIN)
         + (PASTSNTC + PASPRNC + PASTSN1BG + PASPRNBOM + max(0 , TSN1HB) + max(0 , TSN1HF) + max(0 , TSN1HG)) * null(2 - TXPASMIN) ;

REVACOI = (BAPASV + BICPASV + BNCPASV) * null(1 - TXPASMIN) + (BAPASC + BICPASC + BNCPASC) * null(2 - TXPASMIN) ;  

REVDECP = PASTSNTP + PASPRNP + max(0 , TSN1CG + TSNRCG) + max(0 , TSN1DG + TSNRDG) + max(0 , TSN1EG + TSNRGG) + max(0 , TSN1FG + TSNRFG)
          + max(0 , PRN1CO + PRN1CM + PRNRCO + PRNRCM) + max(0 , PRN1DO + PRN1DM + PRNRDO + PRNRDM)
	  + max(0 , PRN1EO + PRN1EM + PRNREO + PRNREM) + max(0 , PRN1FO + PRN1FM + PRNRFO + PRNRFM)
	  + BAPASP + BICPASP + BNCPASP + max(0 , TSN1IB) + max(0 , TSN1JB) 
	  + max(0 , TSN1IF) + max(0 , TSN1JF) + max(0 , TSN1KF) + max(0 , TSN1LF) 
	  + max(0 , TSN1IG) + max(0 , TSN1JG) + max(0 , TSN1KG) + max(0 , TSN1LG) ;

TXPASMIN = positif(REVDEC2 - REVDEC1) + 2 * positif(REVDEC1 - REVDEC2) + 4 * null(REVDEC1 - REVDEC2) ;

INDTEFFPAS = positif(positif(SALEXTV + COD1AE + COD1AH + AUTOBICVV + AUTOBICPV + AUTOBNCV ) * null(1 - TXPASMIN)
                     + positif(SALEXTC + COD1BE + COD1BH + AUTOBICVC + AUTOBICPC + AUTOBNCC ) * null(2 - TXPASMIN) 
	             + positif(SALEXT1 + SALEXT2 + SALEXT3 + SALEXT4 + COD1CH + COD1DH + COD1EH + COD1FH + AUTOBICVP + AUTOBICPP + AUTOBNCP )) 
	     * (1 - null(2 - VALREGCO)) ;

SHBAPAS = (TSPRV + PENSTOTV + max(0 , BNCPHQCV + max(0 , BNCHQV)) + BICPROOCV + max(0 , BICHQV) + max(0 , PASRNPLOCFV)
           + TSNN2VAFF + PENSTOTV + PENFV + GLN3V + BAQV + BICPROQCV + BICQV + BNCQV + BNCPQCV) * null(1 - TXPASMIN)
          + (TSPRC + PENSTOTC + max(0 , BNCPHQCC + max(0 , BNCHQC)) + BICPROOCC + max(0 , BICHQC) + max(0 , PASRNPLOCFC)
	     + TSNN2CAFF + PENSTOTC + PENFC + GLN3C + BAQC + BICPROQCC + BICQC + BNCQC + BNCPQCC) * null(2 - TXPASMIN)
          + arr((TSPRP + PENSTOTP + max(0 , BNCPHQCP + max(0 , BNCHQP)) + BICPROOCP + max(0 , BICHQP) + max(0 , PASRNPLOCFP)) / 2)
          + arr(COD1TZ/2) + arr(RVTOT/2) + arr(RRCM/2) + arr(PVBAR3VG/2) + arr(RRFI/2) + arr(DESV/2) + arr((ESFP + RE168 + TAX1649 + PREREV + R1649)/2)
	  + arr(TSNN2PAFF / 2) + arr(PENSTOTP / 2) + arr((BNCQP + BNCPQCP) / 2) + arr(PENFP / 2) + arr(BAQP / 2)
	  + arr((BICPROQCP + BICQP) / 2) + arr(T2RV/2) + arr(REVRCM/2) + arr(PVRVGRUA/2) + arr(REVRF/2) ;

REVORDI = (TSPRV + BNCPHQCV + BNCHQV + (BAHQV + arr(BAHQP/2)) * positif(BAHQV + arr(BAHQP/2)) + (BAHQV + arr(BAHQP/2)) * ((1 - positif(BAHQV + arr(BAHQP/2))) * positif(SEUIL_IMPDEFBA - SHBAPAS)) 
           + BICPROOCV + BICHQV + max(0 , PASRNPLOCFV)) * null(1 - TXPASMIN)
          + (TSPRC + BNCPHQCC + BNCHQC + (BAHQC + arr(BAHQP/2)) * positif(BAHQC + arr(BAHQP/2)) + (BAHQC + arr(BAHQP/2)) * ((1 - positif(BAHQC + arr(BAHQP/2))) * positif(SEUIL_IMPDEFBA - SHBAPAS)) 
	     + BICPROOCC + BICHQC + max(0 , PASRNPLOCFC)) * null(2 - TXPASMIN)
          + arr((TSPRP + BNCPHQCP + BNCHQP + BICPROOCP + BICHQP + max(0 , PASRNPLOCFP)) / 2)
          + arr(COD1TZ/2) + arr(RVTOT/2) + arr(RRCM/2) + arr(PVBAR3VG/2) + arr(RRFI/2) + arr(DESV/2) + arr((ESFP + RE168 + TAX1649 + PREREV + R1649)/2) ;

TREVORDI = (TTSPRV + TBNCPHQCV + max(0 , TBNCNPHQCV - DABNCNPV) + TBICPROOCV + max(0 , TBICNPOCV - DEFANTBICNPV) + max(0 , PASRNPLOCFV)
            + (BAHQV + arr(BAHQP/2)) * positif(BAHQV + arr(BAHQP/2)) + (BAHQV + arr(BAHQP/2)) * ((1 - positif(BAHQV + arr(BAHQP/2))) * positif(SEUIL_IMPDEFBA - SHBAPAS))) * null(1 - TXPASMIN)
           + (TTSPRC + TBNCPHQCC + max(0 , TBNCNPHQCC - DABNCNPC) + TBICPROOCC + max(0 , TBICNPOCC - DEFANTBICNPC) + max(0 , PASRNPLOCFC)
	    + (BAHQC + arr(BAHQP/2)) * positif(BAHQC + arr(BAHQP/2)) + (BAHQC + arr(BAHQP/2)) * ((1 - positif(BAHQC + arr(BAHQP/2))) * positif(SEUIL_IMPDEFBA - SHBAPAS))) * null(2 - TXPASMIN)
           + arr((TTSPRP + TBNCPHQCP + max(0 , TBNCNPHQCP - DABNCNPP) + TBICPROOCP + max(0 , TBICNPOCP - DEFANTBICNPP) + max(0 , PASRNPLOCFP)) / 2)
           + arr(COD1TZ/2) + arr(RVTOT/2) + arr(RRCM/2) + arr(PVBAR3VG/2) + arr(RRFI/2) + arr(DESV/2) + arr((ESFP + RE168 + TAX1649 + PREREV + R1649)/2) ;

REVQUOT = (TSNN2VAFF + PENSTOTV + PENFV + GLN3V + BAQV + BICPROQCV + BICQV + BNCQV + BNCPQCV + PRNRAI) * null(1 - TXPASMIN)
	  + (TSNN2CAFF + PENSTOTC + PENFC + GLN3C + BAQC + BICPROQCC + BICQC + BNCQC + BNCPQCC + PRNRBI) * null(2 - TXPASMIN)
	  + arr(TSNN2PAFF / 2) + arr(PENSTOTP / 2) + arr((BNCQP + BNCPQCP) / 2) + arr(PENFP / 2) + arr(BAQP / 2)
	  + arr((BICPROQCP + BICQP) / 2) + arr(T2RV/2) + arr(REVRCM/2) + arr(PVRVGRUA/2) + arr(REVRF/2) + arr(PRNRCK / 2) ;

REVQUOTEFF = (TTSNRAJ + TTSNRAP + TTSNRAF + TTSNRAG + TPRNNRAS + TPRNNRAZ + TPRNNRAO + TPRNNRAL + TPRNNRAM + TPRNNFAS + BAQV + BICPROQCV + BICQV + BNCQV + BNCPQCV + TPRNRAI) * null(1 - TXPASMIN)
             + (TTSNRBJ + TTSNRBP + TTSNRBF + TTSNRBG + TPRNNRBS + TPRNNRBZ + TPRNNRBO + TPRNNRBL + TPRNNRBM + TPRNNFBS + BAQC + BICPROQCC + BICQC + BNCQC + BNCPQCC + TPRNRBI) * null(2 - TXPASMIN)
             + arr(TTSNRCJ / 2) + arr(TTSNRDJ / 2) + arr(TTSNREJ / 2) + arr(TTSNRFJ / 2) + arr(TTSNRCP / 2) + arr(TTSNRDP / 2) + arr(TTSNREP / 2) + arr(TTSNRFP / 2) + arr(TTSNRCF / 2) 
	     + arr(TTSNRDF / 2) + arr(TTSNREF / 2) + arr(TTSNRFF / 2) + arr(TTSNRCG / 2) + arr(TTSNRDG / 2) + arr(TTSNRGG / 2) + arr(TTSNRFG / 2) + arr(TPRNNRCS / 2) + arr(TPRNNRDS / 2) 
	     + arr(TPRNNRES / 2) + arr(TPRNNRFS / 2) + arr(TPRNNRCZ / 2) + arr(TPRNNRDZ / 2) + arr(TPRNNREZ / 2) + arr(TPRNNRFZ / 2) + arr(TPRNNRCO / 2) + arr(TPRNNRDO / 2) + arr(TPRNNREO / 2) 
	     + arr(TPRNNRFO / 2) + arr(TPRNNRCL / 2) + arr(TPRNNRDL / 2) + arr(TPRNNREL / 2) + arr(TPRNNRFL / 2) + arr(TPRNNRCM / 2) + arr(TPRNNRDM / 2) + arr(TPRNNREM / 2) + arr(TPRNNRFM / 2) 
	     + arr(TPRNNFCS / 2) + arr(TPRNNFDS / 2) + arr(TPRNNFES / 2) + arr(TPRNNFFS / 2) + arr(BAQP / 2) + arr((BICPROQCP + BICQP) / 2) + arr((BNCQP + BNCPQCP) / 2)
             + arr(T2RV/2) + arr(REVRCM/2) + arr(PVRVGRUA/2) + arr(REVRF/2) + arr(TPRNRCK / 2) ; 

REVTEFF = TREVORDI + REVQUOTEFF ;

CHARGES = arr(DFANT/2) + arr(RDCSG/2) + arr(CHTOT/2) + arr(ABVIE/2) + arr(ABMAR/2) 
          + (V_8ZT * null(1 - TXPASMIN) + CODZRE * null(2 - TXPASMIN) + arr(CODZRF/2)) * null(2 - V_REGCO) ;

REVINDIV = max(0 , REVORDI * (1 - INDTEFFPAS) + TREVORDI * INDTEFFPAS - CHARGES) ;

REVINDIVD = (REVORDI * (1 - INDTEFFPAS) + TREVORDI * INDTEFFPAS) * (1 - positif(REVORDI * (1 - INDTEFFPAS) + TREVORDI * INDTEFFPAS)) ;

NBPTPAS = NBPT / 2 ;

regle 201700:
application : iliad ;

IRINDPAS1 = somme(i=2..6: positif(arr(REVINDIV/NBPTPAS) - LIMINFBARi) * positif_ou_nul(LIMSUPBARi - arr(REVINDIV/NBPTPAS)) * arr((REVINDIV * TXBARi / 100) - (NBPTPAS * PLABARi))) ;

IRINDPAS2 = somme(i=2..6: positif(REVINDIV - LIMINFBARi) * positif_ou_nul(LIMSUPBARi - REVINDIV) * arr((REVINDIV * TXBARi/100) - PLABARi - (2 * PLAF_DEMIPART * (NBPTPAS - 1)))) ;

IRINDPAS = max(IRINDPAS1 , IRINDPAS2) - max(0 , min(IRINDPAS2 - IRINDPAS1 , arr(PLAF_PLACOMP/2 * (V_0AP + V_0AF + V_0AS * (1 - positif(V_0AP + V_0AF + 0)) + V_0AW + V_0AG + V_0CR + V_0CG + V_0CI/2)))) ;

IRINDPASV = IRINDPAS * null(1 - TXPASMIN) ;

IRINDPASC = IRINDPAS * null(2 - TXPASMIN) ;

RASTONPAS = (RASTONEQUO1V * null(1 - TXPASMIN)) + (RASTONEQUO1C * null(2 - TXPASMIN)) ; 

IRINDPASQ1 = somme(i=2..6: positif(arr((REVINDIV + RASTONPAS)/NBPTPAS) - LIMINFBARi) * positif_ou_nul(LIMSUPBARi - arr((REVINDIV + RASTONPAS)/NBPTPAS)) 
                                                                                     * arr(((REVINDIV + RASTONPAS) * TXBARi / 100) - (NBPTPAS * PLABARi))) ;

IRINDPASQ2 = somme(i=2..6: positif((REVINDIV + RASTONPAS) - LIMINFBARi) * positif_ou_nul(LIMSUPBARi - (REVINDIV + RASTONPAS)) 
                                                                        * arr(((REVINDIV + RASTONPAS) * TXBARi/100) - PLABARi - (2 * PLAF_DEMIPART * (NBPTPAS - 1)))) ;

IRINDPASQ = max(IRINDPASQ1 , IRINDPASQ2) - max(0 , min(IRINDPASQ2 - IRINDPASQ1 , arr(PLAF_PLACOMP/2 * (V_0AP + V_0AF + V_0AS * (1 - positif(V_0AP + V_0AF + 0)) + V_0AW + V_0AG + V_0CR + V_0CG + V_0CI/2)))) ;

IRINDPASQV = IRINDPASQ * null(1 - TXPASMIN) ;

IRINDPASQC = IRINDPASQ * null(2 - TXPASMIN) ;

regle taux 201720:
application : iliad ;


RASTXMARJ = positif(arr((REVINDIV + RASTONPAS)/NBPTPAS) - LIMSUPBAR1) * positif(LIMSUPBAR2 - arr((REVINDIV + RASTONPAS)/NBPTPAS)) * TXBAR2 
            + positif(arr((REVINDIV + RASTONPAS)/NBPTPAS) - LIMSUPBAR2) * positif(LIMSUPBAR3 - arr((REVINDIV + RASTONPAS)/NBPTPAS)) * TXBAR3 
            + positif(arr((REVINDIV + RASTONPAS)/NBPTPAS) - LIMSUPBAR3) * positif(LIMSUPBAR4 - arr((REVINDIV + RASTONPAS)/NBPTPAS)) * TXBAR4 
	    + positif(arr((REVINDIV + RASTONPAS)/NBPTPAS) - LIMSUPBAR4) * positif(LIMSUPBAR5 - arr((REVINDIV + RASTONPAS)/NBPTPAS)) * TXBAR5 ;

DECINDIV = arr(max( 0 , IRINDPAS + RASIPQ1001V * null(1 - TXPASMIN) + RASIPQ1001C * null(2 - TXPASMIN) - RECOMPAS)
              * ((max(0 , REVORDI + REVQUOT - CHARGES) / max(0 , TREVORDI + REVQUOTEFF - CHARGES) * positif(INDTEFFPAS)) + null(INDTEFFPAS + 0))) ;

IRINDIV = arr(max( 0 , IRINDPAS + RASIPQ1001V * null(1 - TXPASMIN) + RASIPQ1001C * null(2 - TXPASMIN) - RECOMPAS)
              * ((max(0 , REVORDI + REVQUOT - CHARGES) / max(0 , TREVORDI + REVQUOTEFF - CHARGES) * positif(INDTEFFPAS)) + null(INDTEFFPAS + 0))) 
	  + arr(BATMARGV * RASTXMARJ/100) * null(1 - TXPASMIN) + arr(BATMARGC * RASTXMARJ/100) * null(2 - TXPASMIN) ;

RFRVTOTAUX = arr((RFTAUXF + RVTOTAUXF)/2) ;

TSTAUXINDIV = (TSTAUXRASV + PASTSN1AG + PASPRNAOM + max(0 , TSN1GB) + max(0 , TSN1GF) + max(0 , TSN1GG)) * null(1 - TXPASMIN)
              + (TSTAUXRASC + PASTSN1BG + PASPRNBOM + max(0 , TSN1HB) + max(0 , TSN1HF) + max(0 , TSN1HG)) * null(2 - TXPASMIN) ;

REVACOINDIV = (BAPASV + BICPASV + BNCPASV) * null(1 - TXPASMIN) + (BAPASC + BICPASC + BNCPASC) * null(2 - TXPASMIN) ;

DENOMPAC2 = arr((TSTAUXRASP + max(0 , TSN1CG + TSNRCG) + max(0 , TSN1DG + TSNRDG) + max(0 , TSN1EG + TSNRGG) + max(0 , TSN1FG + TSNRFG)
                 + max(0 , PRN1CO + PRN1CM + PRNRCO + PRNRCM) + max(0 , PRN1DO + PRN1DM + PRNRDO + PRNRDM)
	         + max(0 , PRN1EO + PRN1EM + PRNREO + PRNREM) + max(0 , PRN1FO + PRN1FM + PRNRFO + PRNRFM)
	         + BAPASP + BICPASP + BNCPASP + max(0 , TSN1IB) + max(0 , TSN1JB) 
	         + max(0 , TSN1IF) + max(0 , TSN1JF) + max(0 , TSN1KF) + max(0 , TSN1LF)
		 + max(0 , TSN1IG) + max(0 , TSN1JG) + max(0 , TSN1KG) + max(0 , TSN1LG)) / 2) ;

PASDENI1 = TSTAUXINDIV + REVACOINDIV + DENOMPAC2 + RFRVTOTAUX ;

ABADOPAS = min(arr(IRINDIV * (TX_RABDOM / 100) * (V_EAD + 0)) , PLAF_RABDOM) ;

ABAGUPAS = min(arr(IRINDIV * (TX_RABGUY / 100) * (V_EAG + 0)) , PLAF_RABGUY) ;

DECPAS = min(max(arr(SEUIL_DECOTE1 - ((DECINDIV - (ABADOPAS + ABAGUPAS)) * 45.25/100)) , 0) , (DECINDIV - (ABADOPAS + ABAGUPAS))) ;

INDPASMIN = positif(((min(LIM_INF_MOD3 , REVINDIV + REVQUOT) * TX_MIN_MET + max(0 , REVINDIV + REVQUOT - LIM_INF_MOD3) * TX_MIN_MET2) / 100) - IRINDIV + 0) * null(2 - V_REGCO) ;

REVTOTINDIV = (max(0 , TSPRV) + max(0 , BNCPHQCV + max(0 , BNCHQV)) + max(0 , BAHQV) * positif(BAHQV + arr(BAHQP/2)) + max(0 , BICPROOCV + max(0 , BICHQV) + max(0 , PASRNPLOCFV))
	       + TSNN2VAFF + PENSTOTV + PENFV + GLN3V + BAQV + BICQV + BICPROQCV + BNCQV + BNCPQCV + BATMARGV + PRNRAI) * null(1 - TXPASMIN)
              + (max(0 , TSPRC) + max(0 , BNCPHQCC + max(0 , BNCHQC)) + max(0 , BAHQC) * positif(BAHQC + arr(BAHQP/2)) + max(0 , BICPROOCC + max(0 , BICHQC) + max(0 , PASRNPLOCFC))
	         + TSNN2CAFF + PENSTOTC + PENFC + GLN3C + BAQC + BICQC + BICPROQCC + BNCQC + BNCPQCC + BATMARGC + PRNRBI) * null(2 - TXPASMIN)
	      + arr((max(0 , TSPR1) + max(0 , TSPR2) + max(0 , TSPR3) + max(0 , TSPR4) + max(0 , BNCPHQCP + max(0 , BNCHQP)) + max(0 , BAHQP) + max(0 , BICPROOCP + max(0 , BICHQP) + max(0 , PASRNPLOCFP))) / 2)
              + arr(COD1TZ/2) + arr(RVTOT/2) + arr(RRCM/2) + arr(PVBAR3VG/2) + arr(max(0 , RRFI)/2) + arr(DESV/2) + arr((ESFP + RE168 + TAX1649 + PREREV + R1649)/2)  
	      + arr(TSNN2PAFF / 2) + arr(PENSTOTP / 2) + arr(PENFP / 2) + arr(BAQP / 2) + arr((BICQP + BICPROQCP) / 2) + arr((BNCQP + BNCPQCP) / 2) 
	      + arr(T2RV/2) + arr(REVRCM/2) + arr(PVRVGRUA/2) + arr(REVRF/2) + arr(PRNRCK/2) 
	      - (V_8ZT * null(1 - TXPASMIN) + CODZRE * null(2 - TXPASMIN) + arr(CODZRF/2)) * null(2 - V_REGCO) ;

CIPASIND = (((COD8VM + arr(COD8UM/2)) * (1 - positif_ou_nul(COD8PA)) + min((COD8VM + arr(COD8UM/2)) , (COD8VM + arr(COD8UM/2)) * COD8PA / max(1 , COD8VM + COD8WM + COD8UM)) * positif_ou_nul(COD8PA)) * null(1 - TXPASMIN) 
	   + ((COD8WM + arr(COD8UM/2)) * (1 - positif_ou_nul(COD8PA)) + min((COD8WM + arr(COD8UM/2)) , (COD8WM + arr(COD8UM/2)) * COD8PA / max(1 , COD8VM + COD8WM + COD8UM)) * positif_ou_nul(COD8PA)) * null(2 - TXPASMIN)) ;

REVPASIND = REVDEC1 * null(1 - TXPASMIN) + REVDEC2 * null(2 - TXPASMIN) + arr(REVDECP / 2) + RFRVTOTAUX ;

IRINDIV1 = max(0 , (IRINDIV - (ABADOPAS + ABAGUPAS) - DECPAS) * ((1 - V_CNR) * positif_ou_nul(IRINDIV - (ABADOPAS + ABAGUPAS) - DECPAS - SEUIL_61)
                                                                             + V_CNR * positif(IRINDIV - (ABADOPAS + ABAGUPAS) - DECPAS - SEUIL_61))) * (1 - INDPASMIN)
           + (arr(min(LIM_INF_MOD3 , REVINDIV + REVQUOT) * TX_MIN_MET / 100) + arr(max(0 , REVINDIV + REVQUOT - LIM_INF_MOD3) * TX_MIN_MET2 / 100))
	     * positif_ou_nul(arr(min(LIM_INF_MOD3 , REVINDIV + REVQUOT) * TX_MIN_MET / 100) - SEUIL_TXMIN) * INDPASMIN ;

PASNUMI1 = max(0 , arr((IRINDIV1 * min(1 , REVPASIND / REVTOTINDIV)) - CIPASIND)) ;

TXINDIV1 = arr((PASNUMI1 / PASDENI1) * 1000 * (1 - null(PASDENI1))) / 10 ;

IRPREMIER = arr(((TSTAUXRASV + RASTSPE1N + RASAUSA1N + RASASSO1N + RASBA1N + RASBIC1N + RASBNC1N) * null(1 - TXPASMIN)
                 + (TSTAUXRASC + RASTSPE2N + RASAUSA2N + RASASSO2N + RASBA2N + RASBIC2N + RASBNC2N) * null(2 - TXPASMIN)) * TXINDIV1/100) ;

IRCOMMUN = arr((TSTAUXRASP + RASRFN + RASRVTO + RASTSPEPN + RASGASSUPN + RASASSOPN + RASBAPN + RASBICPN + RASBNCPN) * RASTXFOYER/100) ;

PASNUMI2 = max(0 , PASNUMF - IRPREMIER - IRCOMMUN) ;

PASDENI2 = (TSTAUXRASV + PASTSN1AG + PASPRNAOM + BAPASV + BICPASV + BNCPASV + max(0 , TSN1GB) + max(0 , TSN1GF) + max(0 , TSN1GG)) * null(2 - TXPASMIN)
           + (TSTAUXRASC + PASTSN1BG + PASPRNBOM + BAPASC + BICPASC + BNCPASC + max(0 , TSN1HB) + max(0 , TSN1HF) + max(0 , TSN1HG)) * null(1 - TXPASMIN) ;

TXINDIV2 = arr(((PASNUMI2 / PASDENI2) * 100 * (1 - null(PASDENI2))) * 10) / 10 ; 

RASTXDEC1 = min(80 , (TXINDIV1 * null(1 - TXPASMIN) + TXINDIV2 * null(2 - TXPASMIN) + RASTXFOYER * null(4 - TXPASMIN))
                      * positif(RASTXFOYER) * BOOL_0AM * (1 - null(2 - INDPAS)) * (1 - positif(RMOND + DMOND)) 
	             + RASTXFOYER * BOOL_0AM * positif(RMOND + DMOND)) ;

RASTXDEC2 = min(80 , (TXINDIV1 * null(2 - TXPASMIN) + TXINDIV2 * null(1 - TXPASMIN) + RASTXFOYER * null(4 - TXPASMIN))
                      * positif(RASTXFOYER) * BOOL_0AM * (1 - null(2 - INDPAS)) * (1 - positif(RMOND + DMOND)) 
	             + RASTXFOYER * BOOL_0AM * positif(RMOND + DMOND)) ;

REVMENSPAC1 = arr(arr(((max(0 , TSN1CG) * (1 - positif(COD1GP)) + max(0 , PRN1CO + PRN1CM) * (1 - positif(COD1HP))) * (1 - null(2 - INDPAS)) 
                       + (max(0 , TSN1IF) + max(0 , TSN1IG) + max(0 , TSN1IB)) * (1 - positif(COD1GP))) * 1.11) / 12) ;

REVMENSPAC2 = arr(arr(((max(0 , TSN1DG) * (1 - positif(COD1GQ)) + max(0 , PRN1DO + PRN1DM) * (1 - positif(COD1HQ))) * (1 - null(2 - INDPAS)) 
                       + (max(0 , TSN1JF) + max(0 , TSN1JG) + max(0 , TSN1JB)) * (1 - positif(COD1GQ))) * 1.11) / 12) ;

REVMENSPAC3 = arr(arr(((max(0 , TSN1EG) * (1 - positif(COD1GR)) + max(0 , PRN1EO + PRN1EM) * (1 - positif(COD1HR))) * (1 - null(2 - INDPAS)) 
                       + (max(0 , TSN1KF) + max(0 , TSN1KG)) * (1 - positif(COD1GR))) * 1.11) / 12) ;

REVMENSPAC4 = arr(arr(((max(0 , TSN1FG) * (1 - positif(COD1GS)) + max(0 , PRN1FO + PRN1FM) * (1 - positif(COD1HS))) * (1 - null(2 - INDPAS)) 
                       + (max(0 , TSN1LF) + max(0 , TSN1LG)) * (1 - positif(COD1GS))) * 1.11) / 12) ;

REVMENSPAC5 = arr(arr(RASBAPN * 1.11) / 12) ;

REVMENSPAC6 = arr(arr(RASBICPN * 1.11) / 12) ;

REVMENSPAC7 = arr(arr(RASBNCPN * 1.11) / 12) ;

pour i = 1..7 :
RASTXPACi = positif_ou_nul(3 - V_REGCO) * (positif(1518 - REVMENSPACi) * 0
                                           + positif_ou_nul(REVMENSPACi - 1518) * positif(1577 - REVMENSPACi) * TXPAC005
                                           + positif_ou_nul(REVMENSPACi - 1577) * positif(1678 - REVMENSPACi) * TXPAC013
                                           + positif_ou_nul(REVMENSPACi - 1678) * positif(1791 - REVMENSPACi) * TXPAC021
                                           + positif_ou_nul(REVMENSPACi - 1791) * positif(1914 - REVMENSPACi) * TXPAC029
                                           + positif_ou_nul(REVMENSPACi - 1914) * positif(2016 - REVMENSPACi) * TXPAC035
                                           + positif_ou_nul(REVMENSPACi - 2016) * positif(2150 - REVMENSPACi) * TXPAC041
                                           + positif_ou_nul(REVMENSPACi - 2150) * positif(2544 - REVMENSPACi) * TXPAC053
                                           + positif_ou_nul(REVMENSPACi - 2544) * positif(2912 - REVMENSPACi) * TXPAC075
                                           + positif_ou_nul(REVMENSPACi - 2912) * positif(3317 - REVMENSPACi) * TXPAC099
                                           + positif_ou_nul(REVMENSPACi - 3317) * positif(3734 - REVMENSPACi) * TXPAC119
                                           + positif_ou_nul(REVMENSPACi - 3734) * positif(4357 - REVMENSPACi) * TXPAC138
                                           + positif_ou_nul(REVMENSPACi - 4357) * positif(5224 - REVMENSPACi) * TXPAC158
                                           + positif_ou_nul(REVMENSPACi - 5224) * positif(6537 - REVMENSPACi) * TXPAC179
                                           + positif_ou_nul(REVMENSPACi - 6537) * positif(8165 - REVMENSPACi) * TXPAC20
                                           + positif_ou_nul(REVMENSPACi - 8165) * positif(11333 - REVMENSPACi) * TXPAC24
                                           + positif_ou_nul(REVMENSPACi - 11333) * positif(15349 - REVMENSPACi) * TXPAC28
                                           + positif_ou_nul(REVMENSPACi - 15349) * positif(24094 - REVMENSPACi) * TXPAC33
                                           + positif_ou_nul(REVMENSPACi - 24094) * positif(51611 - REVMENSPACi) * TXPAC38
                                           + positif_ou_nul(REVMENSPACi - 51611) * TXPAC43)
                    + null(5 - V_REGCO) * (positif(1741 - REVMENSPACi) * 0
                                           + positif_ou_nul(REVMENSPACi - 1741) * positif(1847 - REVMENSPACi) * TXPAC005
                                           + positif_ou_nul(REVMENSPACi - 1847) * positif(2035 - REVMENSPACi) * TXPAC013
                                           + positif_ou_nul(REVMENSPACi - 2035) * positif(2222 - REVMENSPACi) * TXPAC021
                                           + positif_ou_nul(REVMENSPACi - 2222) * positif(2454 - REVMENSPACi) * TXPAC029
                                           + positif_ou_nul(REVMENSPACi - 2454) * positif(2588 - REVMENSPACi) * TXPAC035
                                           + positif_ou_nul(REVMENSPACi - 2588) * positif(2677 - REVMENSPACi) * TXPAC041
                                           + positif_ou_nul(REVMENSPACi - 2677) * positif(2945 - REVMENSPACi) * TXPAC053
                                           + positif_ou_nul(REVMENSPACi - 2945) * positif(3641 - REVMENSPACi) * TXPAC075
                                           + positif_ou_nul(REVMENSPACi - 3641) * positif(4659 - REVMENSPACi) * TXPAC099
                                           + positif_ou_nul(REVMENSPACi - 4659) * positif(5292 - REVMENSPACi) * TXPAC119
                                           + positif_ou_nul(REVMENSPACi - 5292) * positif(6130 - REVMENSPACi) * TXPAC138
                                           + positif_ou_nul(REVMENSPACi - 6130) * positif(7344 - REVMENSPACi) * TXPAC158
                                           + positif_ou_nul(REVMENSPACi - 7344) * positif(8165 - REVMENSPACi) * TXPAC179
                                           + positif_ou_nul(REVMENSPACi - 8165) * positif(9280 - REVMENSPACi) * TXPAC20
                                           + positif_ou_nul(REVMENSPACi - 9280) * positif(12761 - REVMENSPACi) * TXPAC24
                                           + positif_ou_nul(REVMENSPACi - 12761) * positif(16956 - REVMENSPACi) * TXPAC28
                                           + positif_ou_nul(REVMENSPACi - 16956) * positif(25880 - REVMENSPACi) * TXPAC33
                                           + positif_ou_nul(REVMENSPACi - 25880) * positif(56568 - REVMENSPACi) * TXPAC38
                                           + positif_ou_nul(REVMENSPACi - 56568) * TXPAC43)
                    + null(6 - V_REGCO) * (positif(1865 - REVMENSPACi) * 0
                                           + positif_ou_nul(REVMENSPACi - 1865) * positif(2016 - REVMENSPACi) * TXPAC005
                                           + positif_ou_nul(REVMENSPACi - 2016) * positif(2248 - REVMENSPACi) * TXPAC013
                                           + positif_ou_nul(REVMENSPACi - 2248) * positif(2534 - REVMENSPACi) * TXPAC021
                                           + positif_ou_nul(REVMENSPACi - 2534) * positif(2632 - REVMENSPACi) * TXPAC029
                                           + positif_ou_nul(REVMENSPACi - 2632) * positif(2722 - REVMENSPACi) * TXPAC035
                                           + positif_ou_nul(REVMENSPACi - 2722) * positif(2811 - REVMENSPACi) * TXPAC041
                                           + positif_ou_nul(REVMENSPACi - 2811) * positif(3123 - REVMENSPACi) * TXPAC053
                                           + positif_ou_nul(REVMENSPACi - 3123) * positif(4310 - REVMENSPACi) * TXPAC075
                                           + positif_ou_nul(REVMENSPACi - 4310) * positif(5578 - REVMENSPACi) * TXPAC099
                                           + positif_ou_nul(REVMENSPACi - 5578) * positif(6291 - REVMENSPACi) * TXPAC119
                                           + positif_ou_nul(REVMENSPACi - 6291) * positif(7300 - REVMENSPACi) * TXPAC138
                                           + positif_ou_nul(REVMENSPACi - 7300) * positif(8031 - REVMENSPACi) * TXPAC158
                                           + positif_ou_nul(REVMENSPACi - 8031) * positif(8897 - REVMENSPACi) * TXPAC179
                                           + positif_ou_nul(REVMENSPACi - 8897) * positif(10325 - REVMENSPACi) * TXPAC20
                                           + positif_ou_nul(REVMENSPACi - 10325) * positif(13891 - REVMENSPACi) * TXPAC24
                                           + positif_ou_nul(REVMENSPACi - 13891) * positif(17669 - REVMENSPACi) * TXPAC28
                                           + positif_ou_nul(REVMENSPACi - 17669) * positif(28317 - REVMENSPACi) * TXPAC33
                                           + positif_ou_nul(REVMENSPACi - 28317) * positif(59770 - REVMENSPACi) * TXPAC38
                                           + positif_ou_nul(REVMENSPACi - 59770) * TXPAC43)
		  ;

regle taux 201740:
application : iliad ;


RASRVTOA = arr(RASRVTO * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASRFA = arr(RASRF * RASTXFOYER / 100 / 12) ;

RASPACA = arr((somme (i=1..4 : REVMENSPACi * RASTXPACi) + arr(arr(RASBAP * 1.11) / 12) * RASTXPAC5 
              + arr(arr(RASBICP * 1.11) / 12) * RASTXPAC6 + arr(arr(RASBNCP * 1.11) / 12) * RASTXPAC7) / 100)
	  * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ; 


RASPSRF = max(0 , RASRF - COD8RF * BOOL_0AM * null(1 - COD8SH - COD8SI)) * (1 - COD8SH * (1 - BOOL_0AM + COD8SI)) ;

RASPSRF1 = arr(RASPSRF / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) ;

RASPSRF2 = (RASPSRF - RASPSRF1) * BOOL_0AM * (1 - null(2 - INDPAS)) ; 

RASPSRFA = arr(RASPSRF * TXPASPS / 100 / 12) * (1 - null(2 - INDPAS)) ;

RASPSRVTO = max(0 , RASRVTO - COD8RV * BOOL_0AM * null(1 - COD8SH - COD8SI)) * (1 - COD8SH * (1 - BOOL_0AM + COD8SI)) * (1 - V_CNR) ;

RASPSRVTO1 = arr(RASPSRVTO / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) ;

RASPSRVTO2 = (RASPSRVTO - RASPSRVTO1) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASPSRVTOA = arr(RASPSRVTO * TXPASPS / 100 / 12) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPSBIC1 = LOCNPASSPSV * (1 - COD5CF) * (1 - COD8SH) * (1 - null(2 - INDPAS)) ;

RASPSBIC2 = LOCNPASSPSC * (1 - COD5CI) * (1 - COD8SI) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASPSBICP = LOCNPASSPSP * (1 - COD5CM) * (1 - positif(COD8SH + COD8SI)) * (1 - null(2 - INDPAS)) ;

RASPS5HY = RCSV * (1 - positif(COD5AF + COD5BF + COD5AN + COD5CF + COD5AO + COD5AP)) * (1 - COD8SH) * (1 - null(2 - INDPAS)) * (1 - V_CNR) ;

RASPS5IY = RCSC * (1 - positif(COD5AI + COD5BI + COD5BN + COD5CI + COD5BO + COD5BP)) * (1 - COD8SI) * (1 - null(2 - INDPAS)) * (1 - V_CNR) ;

RASPS5JY = RCSP * (1 - positif(COD5AH + COD5BH + COD5CN + COD5CM + COD5CQ + COD5CR)) * (1 - positif(COD8SH + COD8SI)) * (1 - null(2 - INDPAS)) * (1 - V_CNR) ;

RASPSPAC = RASPSBICP + RASPS5JY ;

RASPSPAC1 = arr(RASPSPAC / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) ;

RASPSPAC2 = (RASPSPAC - RASPSPAC1) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASPSPACA = arr(RASPSPAC * TXPASPS / 100 / 12) * (1 - null(2 - INDPAS)) ;

regle taux 201760:
application : iliad ;

RASBICFM1 = arr(RASBIC1 * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBICFM2 = arr(RASBIC2 * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBICFT1 = RASBICFM1 * 3 ;

RASBICFT2 = RASBICFM2 * 3 ;

RASBICIM1 = arr(RASBIC1 * RASTXDEC1 / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBICIM2 = arr(RASBIC2 * RASTXDEC2 / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBICIT1 = RASBICIM1 * 3 ;

RASBICIT2 = RASBICIM2 * 3 ;

RASBNCFM1 = arr(RASBNC1 * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBNCFM2 = arr(RASBNC2 * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBNCFT1 = RASBNCFM1 * 3 ;

RASBNCFT2 = RASBNCFM2 * 3 ;

RASBNCIM1 = arr(RASBNC1 * RASTXDEC1 / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBNCIM2 = arr(RASBNC2 * RASTXDEC2 / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBNCIT1 = RASBNCIM1 * 3 ;

RASBNCIT2 = RASBNCIM2 * 3 ;

RASBAFM1 = arr(RASBA1 * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBAFM2 = arr(RASBA2 * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBAFT1 = RASBAFM1 * 3 ;

RASBAFT2 = RASBAFM2 * 3 ;

RASBAIM1 = arr(RASBA1 * RASTXDEC1 / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBAIM2 = arr(RASBA2 * RASTXDEC2 / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASBAIT1 = RASBAIM1 * 3 ;

RASBAIT2 = RASBAIM2 * 3 ;

RASRVTOFM1 = arr(arr(RASRVTO * RASTXFOYER / 100 / 12) / (2 - null(BOOL_0AM))) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASRVTOFM2 = (arr(RASRVTO * RASTXFOYER / 100 / 12) - RASRVTOFM1) * BOOL_0AM * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASRVTOFT1 = RASRVTOFM1 * 3 ;

RASRVTOFT2 = RASRVTOFM2 * 3 ;

RASRFFM1 = arr(arr(RASRF * RASTXFOYER / 100 / 12) / (2 - null(BOOL_0AM))) ;

RASRFFM2 = (arr(RASRF * RASTXFOYER / 100 / 12) - RASRFFM1) * BOOL_0AM ;

RASRFFT1 = RASRFFM1 * 3 ;

RASRFFT2 = RASRFFM2 * 3 ;

RASTSPEFM1 = arr(RASTSPE1 * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASTSPEFM2 = arr(RASTSPE2 * RASTXFOYER / 100 / 12) * BOOL_0AM * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASTSPEFT1 = RASTSPEFM1 * 3 ;

RASTSPEFT2 = RASTSPEFM2 * 3 ;

RASTSPEIM1 = arr(RASTSPE1 * RASTXDEC1 / 100 / 12) * (1 - INDTAZ) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASTSPEIM2 = arr(RASTSPE2 * RASTXDEC2 / 100 / 12) * BOOL_0AM * (1 - INDTAZ) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASTSPEIT1 = RASTSPEIM1 * 3 ;

RASTSPEIT2 = RASTSPEIM2 * 3 ;

RASPACFM1 = arr(RASPACA / (2 - null(BOOL_0AM))) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPACFM2 = (RASPACA - RASPACFM1) * BOOL_0AM * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPACFT1 = RASPACFM1 * 3 ;

RASPACFT2 = RASPACFM2 * 3 ;

RASLEMPFM1 = arr(RASLEMP1 * RASTXFOYER / 100 / 4) ;

RASLEMPFM2 = arr(RASLEMP2 * RASTXFOYER / 100 / 4) * BOOL_0AM ;

RASLEMPIM1 = arr(RASLEMP1 * RASTXDEC1 / 100 / 4) * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASLEMPIM2 = arr(RASLEMP2 * RASTXDEC2 / 100 / 4) * BOOL_0AM * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASLEMPFM = RASLEMPFM1 + RASLEMPFM2 ;

RASASSOFM1 = arr(RASASSO1 * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASASSOFM2 = arr(RASASSO2 * RASTXFOYER / 100 / 12) * BOOL_0AM * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASASSOFT1 = RASASSOFM1 * 3 ;

RASASSOFT2 = RASASSOFM2 * 3 ;

RASASSOIM1 = arr(RASASSO1 * RASTXDEC1 / 100 / 12) * (1 - INDTAZ) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASASSOIM2 = arr(RASASSO2 * RASTXDEC2 / 100 / 12) * BOOL_0AM * (1 - INDTAZ) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASASSOIT1 = RASASSOIM1 * 3 ;

RASASSOIT2 = RASASSOIM2 * 3 ;

RASAUSAFM1 = arr(RASAUSA1 * RASTXFOYER / 100 / 12) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASAUSAFM2 = arr(RASAUSA2 * RASTXFOYER / 100 / 12) * BOOL_0AM * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASAUSAFT1 = RASAUSAFM1 * 3 ;

RASAUSAFT2 = RASAUSAFM2 * 3 ;

RASAUSAIM1 = arr(RASAUSA1 * RASTXDEC1 / 100 / 12) * (1 - INDTAZ) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASAUSAIM2 = arr(RASAUSA2 * RASTXDEC2 / 100 / 12) * BOOL_0AM * (1 - INDTAZ) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASAUSAIT1 = RASAUSAIM1 * 3 ;

RASAUSAIT2 = RASAUSAIM2 * 3 ;


RASBAF1 = arr(RASBA1 * RASTXFOYER / 100) ;

RASBAF2 = arr(RASBA2 * RASTXFOYER / 100) ;

RASBAI1 = arr(RASBA1 * RASTXDEC1 / 100) ;

RASBAI2 = arr(RASBA2 * RASTXDEC2 / 100) ;

RASRBAMF1 = arr(max(0 , RASBAF1 - COD8AA) / (null(COD8MM) + min(12 , COD8MM))) ; 

RASRBAMF2 = arr(max(0 , RASBAF2 - COD8AB) / (null(COD8MM) + min(12 , COD8MM))) ; 

RASRBAMI1 = arr(max(0 , RASBAI1 - COD8AA) / (null(COD8MM) + min(12 , COD8MM))) ; 

RASRBAMI2 = arr(max(0 , RASBAI2 - COD8AB) / (null(COD8MM) + min(12 , COD8MM))) ; 

RASRBATF1 = arr(max(0 , RASBAF1 - COD8AA) / (null(COD8MT) + min(4 , COD8MT))) ; 

RASRBATF2 = arr(max(0 , RASBAF2 - COD8AB) / (null(COD8MT) + min(4 , COD8MT))) ; 

RASRBATI1 = arr(max(0 , RASBAI1 - COD8AA) / (null(COD8MT) + min(4 , COD8MT))) ; 

RASRBATI2 = arr(max(0 , RASBAI2 - COD8AB) / (null(COD8MT) + min(4 , COD8MT))) ; 

RASBICF1 = arr(RASBIC1 * RASTXFOYER / 100) ;

RASBICF2 = arr(RASBIC2 * RASTXFOYER / 100) ;

RASBICI1 = arr(RASBIC1 * RASTXDEC1 / 100) ;

RASBICI2 = arr(RASBIC2 * RASTXDEC2 / 100) ;

RASRBICMF1 = arr(max(0 , RASBICF1 - COD8AC) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRBICMF2 = arr(max(0 , RASBICF2 - COD8AD) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRBICMI1 = arr(max(0 , RASBICI1 - COD8AC) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRBICMI2 = arr(max(0 , RASBICI2 - COD8AD) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRBICTF1 = arr(max(0 , RASBICF1 - COD8AC) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRBICTF2 = arr(max(0 , RASBICF2 - COD8AD) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRBICTI1 = arr(max(0 , RASBICI1 - COD8AC) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRBICTI2 = arr(max(0 , RASBICI2 - COD8AD) / (null(COD8MT) + min(4 , COD8MT))) ;

RASBNCF1 = arr(RASBNC1 * RASTXFOYER / 100) ;

RASBNCF2 = arr(RASBNC2 * RASTXFOYER / 100) ;

RASBNCI1 = arr(RASBNC1 * RASTXDEC1 / 100) ;

RASBNCI2 = arr(RASBNC2 * RASTXDEC2 / 100) ;

RASRBNCMF1 = arr(max(0 , RASBNCF1 - COD8AE) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRBNCMF2 = arr(max(0 , RASBNCF2 - COD8AF) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRBNCMI1 = arr(max(0 , RASBNCI1 - COD8AE) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRBNCMI2 = arr(max(0 , RASBNCI2 - COD8AF) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRBNCTF1 = arr(max(0 , RASBNCF1 - COD8AE) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRBNCTF2 = arr(max(0 , RASBNCF2 - COD8AF) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRBNCTI1 = arr(max(0 , RASBNCI1 - COD8AE) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRBNCTI2 = arr(max(0 , RASBNCI2 - COD8AF) / (null(COD8MT) + min(4 , COD8MT))) ;

RASTSPEF1 = arr(RASTSPE1 * RASTXFOYER / 100) ;

RASTSPEF2 = arr(RASTSPE2 * RASTXFOYER / 100) * BOOL_0AM ;

RASTSPEI1 = arr(RASTSPE1 * RASTXDEC1 / 100) * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASTSPEI2 = arr(RASTSPE2 * RASTXDEC2 / 100) * BOOL_0AM * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASRTSPEMF1 = arr(max(0 , RASTSPEF1 - COD8AG) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRTSPEMF2 = arr(max(0 , RASTSPEF2 - COD8AH) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRTSPEMI1 = arr(max(0 , RASTSPEI1 - COD8AG) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRTSPEMI2 = arr(max(0 , RASTSPEI2 - COD8AH) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRTSPETF1 = arr(max(0 , RASTSPEF1 - COD8AG) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRTSPETF2 = arr(max(0 , RASTSPEF2 - COD8AH) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRTSPETI1 = arr(max(0 , RASTSPEI1 - COD8AG) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRTSPETI2 = arr(max(0 , RASTSPEI2 - COD8AH) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRVTOF = arr(RASRVTO * RASTXFOYER / 100) ;

RASRVTOF1 = arr(RASRVTOF / (2 - null(BOOL_0AM))) ; 

RASRVTOF2 = (RASRVTOF - RASRVTOF1) * BOOL_0AM ;

RASRRVTOM1 = arr(max(0 , RASRVTOF1 - COD8AI) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRRVTOM2 = arr(max(0 , RASRVTOF2 - COD8AJ) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRRVTOT1 = arr(max(0 , RASRVTOF1 - COD8AI) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRRVTOT2 = arr(max(0 , RASRVTOF2 - COD8AJ) / (null(COD8MT) + min(4 , COD8MT))) ;

RASLEMPF1 = arr(RASLEMP1 * RASTXFOYER / 100) ;

RASLEMPF2 = arr(RASLEMP2 * RASTXFOYER / 100) * BOOL_0AM ;

RASLEMPI1 = arr(RASLEMP1 * RASTXDEC1 / 100) * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASLEMPI2 = arr(RASLEMP2 * RASTXDEC2 / 100) * BOOL_0AM * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASASSOF1 = arr(RASASSO1 * RASTXFOYER / 100) ;

RASASSOF2 = arr(RASASSO2 * RASTXFOYER / 100) * BOOL_0AM ;

RASASSOI1 = arr(RASASSO1 * RASTXDEC1 / 100) * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASASSOI2 = arr(RASASSO2 * RASTXDEC2 / 100) * BOOL_0AM * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASRASSOMF1 = arr(max(0 , RASASSOF1 - COD8BA) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRASSOMF2 = arr(max(0 , RASASSOF2 - COD8BB) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRASSOMI1 = arr(max(0 , RASASSOI1 - COD8BA) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRASSOMI2 = arr(max(0 , RASASSOI2 - COD8BB) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRASSOTF1 = arr(max(0 , RASASSOF1 - COD8BA) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRASSOTF2 = arr(max(0 , RASASSOF2 - COD8BB) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRASSOTI1 = arr(max(0 , RASASSOI1 - COD8BA) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRASSOTI2 = arr(max(0 , RASASSOI2 - COD8BB) / (null(COD8MT) + min(4 , COD8MT))) ;

RASAUSAF1 = arr(RASAUSA1 * RASTXFOYER / 100) ;

RASAUSAF2 = arr(RASAUSA2 * RASTXFOYER / 100) * BOOL_0AM ;

RASAUSAI1 = arr(RASAUSA1 * RASTXDEC1 / 100) * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASAUSAI2 = arr(RASAUSA2 * RASTXDEC2 / 100) * BOOL_0AM * (1 - INDTAZ) * (1 - null(2 - INDPAS)) ;

RASRAUSAMF1 = arr(max(0 , RASAUSAF1 - COD8AU) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRAUSAMF2 = arr(max(0 , RASAUSAF2 - COD8AV) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRAUSAMI1 = arr(max(0 , RASAUSAI1 - COD8AU) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRAUSAMI2 = arr(max(0 , RASAUSAI2 - COD8AV) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRAUSATF1 = arr(max(0 , RASAUSAF1 - COD8AU) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRAUSATF2 = arr(max(0 , RASAUSAF2 - COD8AV) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRAUSATI1 = arr(max(0 , RASAUSAI1 - COD8AU) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRAUSATI2 = arr(max(0 , RASAUSAI2 - COD8AV) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRFF = arr(RASRF * RASTXFOYER / 100) ;

RASRFF1 = arr(RASRFF / (2 - null(BOOL_0AM))) ;

RASRFF2 = (RASRFF - RASRFF1) * BOOL_0AM ;

RASRRFM1 = arr(max(0 , RASRFF1 - COD8AK) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRRFM2 = arr(max(0 , RASRFF2 - COD8AL) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRRFT1 = arr(max(0 , RASRFF1 - COD8AK) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRRFT2 = arr(max(0 , RASRFF2 - COD8AL) / (null(COD8MT) + min(4 , COD8MT))) ;

RASPACF = arr(((max(0 , TSN1CG) + max(0 , PRN1CO + PRN1CM)) 
               + max(0 , TSN1IF) + max(0 , TSN1IG) + max(0 , TSN1IB)) * 1.11 * RASTXPAC1 / 100)
          + arr(((max(0 , TSN1DG) + max(0 , PRN1DO + PRN1DM)) 
                 + max(0 , TSN1JF) + max(0 , TSN1JG) + max(0 , TSN1JB)) * 1.11 * RASTXPAC2 / 100)
          + arr(((max(0 , TSN1EG) + max(0 , PRN1EO + PRN1EM)) 
                 + max(0 , TSN1KF) + max(0 , TSN1KG)) * 1.11 * RASTXPAC3 / 100)
          + arr(((max(0 , TSN1FG) + max(0 , PRN1FO + PRN1FM)) 
                 + max(0 , TSN1LF) + max(0 , TSN1LG)) * 1.11 * RASTXPAC4 / 100)
          + arr(RASBAP * 1.11 * RASTXPAC5 / 100) + arr(RASBICP * 1.11 * RASTXPAC6 / 100) + arr(RASBNCP * 1.11 * RASTXPAC7 / 100) ;

RASPACF1 = arr(RASPACF / (2 - null(BOOL_0AM))) ;

RASPACF2 = (RASPACF - RASPACF1) * BOOL_0AM ;

RASRPACM1 = arr(max(0 , RASPACF1 - COD8AW) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRPACM2 = arr(max(0 , RASPACF2 - COD8AX) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRPACT1 = arr(max(0 , RASPACF1 - COD8AW) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRPACT2 = arr(max(0 , RASPACF2 - COD8AX) / (null(COD8MT) + min(4 , COD8MT))) ;


RASACTOTIR = RASBAF1 + RASBAF2 + RASBICF1 + RASBICF2 + RASBNCF1 + RASBNCF2 + RASTSPEF1 + RASTSPEF2 + RASRVTOF + RASAUSAF1 + RASAUSAF2 + RASASSOF1 + RASASSOF2 + RASRFF + RASPACF ;

RASEXCEDIR = max(0 , COD8AA + COD8AB + COD8AC + COD8AD + COD8AE + COD8AF + COD8AG + COD8AH 
                     + COD8AI + COD8AJ + COD8AU + COD8AV + COD8AK + COD8AL + COD8AW + COD8AX - RASACTOTIR) ;

regle taux 201780:
application : iliad ;

RASPSPACM1 = arr(RASPSPACA / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPSPACM2 = (RASPSPACA - RASPSPACM1) * BOOL_0AM * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPSPACT1 = RASPSPACM1 * 3 ;

RASPSPACT2 = RASPSPACM2 * 3 ;

RASPSRFM1 = arr(RASPSRFA / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) ;

RASPSRFM2 = (RASPSRFA - RASPSRFM1) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASPSRFT1 = RASPSRFM1 * 3 ;

RASPSRFT2 = RASPSRFM2 * 3 ;

RASPSRVTOM1 = arr(RASPSRVTOA / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPSRVTOM2 = (RASPSRVTOA - RASPSRVTOM1) * BOOL_0AM * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPSRVTOT1 = RASPSRVTOM1 * 3 ;

RASPSRVTOT2 = RASPSRVTOM2 * 3 ;

RASPSBICM1 = arr(RASPSBIC1 * TXPASPS / 100 / 12) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPSBICM2 = arr(RASPSBIC2 * TXPASPS / 100 / 12) * BOOL_0AM * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPSBICT1 = RASPSBICM1 * 3 ;

RASPSBICT2 = RASPSBICM2 * 3 ;

RASPS5HYM = arr(RASPS5HY * TXPASPS / 100 / 12) * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPS5HYT = RASPS5HYM * 3 ;

RASPS5IYM = arr(RASPS5IY * TXPASPS / 100 / 12) * BOOL_0AM * (1 - null(2 - INDPAS)) * (1 - null(V_CNR2) * positif(DATDEPETR + 0)) ;

RASPS5IYT = RASPS5IYM * 3 ;


RASPSMEUB1 = arr(RASPSBIC1 * TXPASPS / 100) * (1 - null(2 - INDPAS)) ;

RASPSMEUB2 = arr(RASPSBIC2 * TXPASPS / 100) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASPSMEUBM1 = arr(max(0 , RASPSMEUB1 - COD8AM) / (null(COD8MM) + min(12 , COD8MM))) ;

RASPSMEUBM2 = arr(max(0 , RASPSMEUB2 - COD8AN) / (null(COD8MM) + min(12 , COD8MM))) ;

RASPSMEUBT1 = arr(max(0 , RASPSMEUB1 - COD8AM) / (null(COD8MT) + min(4 , COD8MT))) ;

RASPSMEUBT2 = arr(max(0 , RASPSMEUB2 - COD8AN) / (null(COD8MT) + min(4 , COD8MT))) ;

RASPSNSAL1 = arr(RASPS5HY * TXPASPS / 100) * (1 - null(2 - INDPAS)) ;

RASPSNSAL2 = arr(RASPS5IY * TXPASPS / 100) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASPSNSALM1 = arr(max(0 , RASPSNSAL1 - COD8AO) / (null(COD8MM) + min(12 , COD8MM))) ;

RASPSNSALM2 = arr(max(0 , RASPSNSAL2 - COD8AP) / (null(COD8MM) + min(12 , COD8MM))) ;

RASPSNSALT1 = arr(max(0 , RASPSNSAL1 - COD8AO) / (null(COD8MT) + min(4 , COD8MT))) ;

RASPSNSALT2 = arr(max(0 , RASPSNSAL2 - COD8AP) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRVTOPS = arr(RASPSRVTO * TXPASPS / 100) * (1 - null(2 - INDPAS)) ;

RASRVTOPS1 = arr(RASRVTOPS / (2 - null(BOOL_0AM))) ;

RASRVTOPS2 = (RASRVTOPS - RASRVTOPS1) * BOOL_0AM ;

RASRVTOPSM1 = arr(max(0 , RASRVTOPS1 - COD8AQ) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRVTOPSM2 = arr(max(0 , RASRVTOPS2 - COD8AR) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRVTOPST1 = arr(max(0 , RASRVTOPS1 - COD8AQ) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRVTOPST2 = arr(max(0 , RASRVTOPS2 - COD8AR) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRFPS = arr(RASPSRF * TXPASPS / 100) * (1 - null(2 - INDPAS)) ;

RASRFPS1 = arr(RASRFPS / (2 - null(BOOL_0AM))) ;

RASRFPS2 = (RASRFPS - RASRFPS1) * BOOL_0AM ;

RASRFPSM1 = arr(max(0 , RASRFPS1 - COD8AS) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRFPSM2 = arr(max(0 , RASRFPS2 - COD8AT) / (null(COD8MM) + min(12 , COD8MM))) ;

RASRFPST1 = arr(max(0 , RASRFPS1 - COD8AS) / (null(COD8MT) + min(4 , COD8MT))) ;

RASRFPST2 = arr(max(0 , RASRFPS2 - COD8AT) / (null(COD8MT) + min(4 , COD8MT))) ;

RASPACPS = arr(RASPSPAC * TXPASPS / 100) * (1 - null(2 - INDPAS)) ;

RASPACPS1 = arr(RASPACPS / (2 - null(BOOL_0AM))) * (1 - null(2 - INDPAS)) ;

RASPACPS2 = (RASPACPS - RASPACPS1) * BOOL_0AM * (1 - null(2 - INDPAS)) ;

RASPACPSM1 = arr(max(0 , RASPACPS1 - COD8AY) / (null(COD8MM) + min(12 , COD8MM))) ;

RASPACPSM2 = arr(max(0 , RASPACPS2 - COD8AZ) / (null(COD8MM) + min(12 , COD8MM))) ;

RASPACPST1 = arr(max(0 , RASPACPS1 - COD8AY) / (null(COD8MT) + min(4 , COD8MT))) ;

RASPACPST2 = arr(max(0 , RASPACPS2 - COD8AZ) / (null(COD8MT) + min(4 , COD8MT))) ;


RASACTOTPS = RASPSMEUB1 + RASPSMEUB2 + RASPSNSAL1 + RASPSNSAL2 + RASRVTOPS + RASRFPS + RASPACPS ;

RASEXCEDPS = max(0 , COD8AM + COD8AN + COD8AO + COD8AP + COD8AQ + COD8AR + COD8AS + COD8AT + COD8AY + COD8AZ - RASACTOTPS) ;

regle taux 201800:
application : iliad ;

RASBICM1 = RASBICFM1 + RASPSBICM1 ;

RASBICM2 = RASBICFM2 + RASPSBICM2 ;

RASPACM = RASPACFM1 + RASPACFM2 + RASPSPACM1 + RASPSPACM2 ;

RASRVTOM = RASRVTOFM1 + RASRVTOFM2 + RASPSRVTOM1 + RASPSRVTOM2 ;

RASRFM = RASRFFM1 + RASRFFM2 + RASPSRFM1 + RASPSRFM2 ;

RASTOTF = RASRFA + RASRVTOA + RASPACA + RASPSPACA + RASPSRFA + RASPSRVTOA ;

RASTOT1 = RASTSPEFM1 + RASAUSAFM1 + RASASSOFM1 + RASLEMPFM1 + RASBAFM1 + RASBICFM1 + RASBNCFM1 + RASPSBICM1 + RASPS5HYM ;

RASTOT2 = RASTSPEFM2 + RASAUSAFM2 + RASASSOFM2 + RASLEMPFM2 + RASBAFM2 + RASBICFM2 + RASBNCFM2 + RASPSBICM2 + RASPS5IYM ;

RASTOT = RASTOTF + RASTOT1 + RASTOT2 ;

RASTOTFM = RASTSPEFM1 + RASAUSAFM1 + RASASSOFM1 + RASLEMPFM1 + RASBAFM1 + RASBICFM1 + RASBNCFM1 + RASPSBICM1 + RASPS5HYM
           + RASTSPEFM2 + RASAUSAFM2 + RASASSOFM2 + RASLEMPFM2 + RASBAFM2 + RASBICFM2 + RASBNCFM2 + RASPSBICM2 + RASPS5IYM
	   + RASRFFM1 + RASRFFM2 + RASRVTOFM1 + RASRVTOFM2 + RASPSRFM1 + RASPSRFM2 + RASPSRVTOM1 + RASPSRVTOM2 
	   + RASPACFM1 + RASPACFM2 + RASPSPACM1 + RASPSPACM2 ;

RASTOTFT = RASTSPEFT1 + RASAUSAFT1 + RASASSOFT1 + RASBAFT1 + RASBICFT1 + RASBNCFT1 + RASPSBICT1 + RASPS5HYT
           + RASTSPEFT2 + RASAUSAFT2 + RASASSOFT2 + RASBAFT2 + RASBICFT2 + RASBNCFT2 + RASPSBICT2 + RASPS5IYT 
	   + RASRFFT1 + RASRFFT2 + RASRVTOFT1 + RASRVTOFT2 + RASPSRFT1 + RASPSRFT2 + RASPSRVTOT1 + RASPSRVTOT2 
	   + RASPACFT1 + RASPACFT2 + RASPSPACT1 + RASPSPACT2 ;

RASTOTIM = RASTSPEIM1 + RASAUSAIM1 + RASASSOIM1 + RASLEMPIM1 + RASBAIM1 + RASBICIM1 + RASBNCIM1 + RASPSBICM1 + RASPS5HYM
           + RASTSPEIM2 + RASAUSAIM2 + RASASSOIM2 + RASLEMPIM2 + RASBAIM2 + RASBICIM2 + RASBNCIM2 + RASPSBICM2 + RASPS5IYM 
	   + RASRFFM1 + RASRFFM2 + RASRVTOFM1 + RASRVTOFM2 + RASPSRFM1 + RASPSRFM2 + RASPSRVTOM1 + RASPSRVTOM2 
	   + RASPACFM1 + RASPACFM2 + RASPSPACM1 + RASPSPACM2 ;

RASTOTIT = RASTSPEIT1 + RASAUSAIT1 + RASASSOIT1 + RASBAIT1 + RASBICIT1 + RASBNCIT1 + RASPSBICT1 + RASPS5HYT
           + RASTSPEIT2 + RASAUSAIT2 + RASASSOIT2 + RASBAIT2 + RASBICIT2 + RASBNCIT2 + RASPSBICT2 + RASPS5IYT 
	   + RASRFFT1 + RASRFFT2 + RASRVTOFT1 + RASRVTOFT2 + RASPSRFT1 + RASPSRFT2 + RASPSRVTOT1 + RASPSRVTOT2 
	   + RASPACFT1 + RASPACFT2 + RASPSPACT1 + RASPSPACT2 ;

RASTOTM = RASTOTFM * (1 - positif(V_RASOI1 + V_RASOI2)) + RASTOTIM * positif(V_RASOI1 + V_RASOI2) ;

RASTOTT = RASTOTFT * (1 - positif(V_RASOI1 + V_RASOI2)) + RASTOTIT * positif(V_RASOI1 + V_RASOI2) ;

INDACPAS = positif_ou_nul(RASBAFM1 + RASBICFM1 + RASBNCFM1 + RASTSPEFM1 + RASAUSAFM1 + RASASSOFM1 + RASLEMPFM1 + RASBAFM2 + RASBICFM2 
                          + RASBNCFM2 + RASTSPEFM2 + RASAUSAFM2 + RASASSOFM2 + RASLEMPFM2 + RASRFA + RASRVTOA + RASPACA + RASPSPACA
                          + RASPSBICM1 + RASPS5HYM + RASPSBICM2 + RASPS5IYM + RASPSRFA + RASPSRVTOA - 5) ;

INDSALEMP = positif(RASLEMPFM1 + RASLEMPFM2) * INDACPAS ;

INDNOVACPT = positif(RASTOTFM - 4) * (1 - positif(RASACOIR + RASACOPS + 0)) ;

PASTOTAL = arr(TSTAUXRASV * RASTXFOYER / 100) + arr(TSTAUXRASC * RASTXFOYER / 100) + RASACTOTIR ;

PASTOTALPS = RASACTOTPS ;

regle taux 201820:
application : iliad ;


LIGPASEND = (1 - positif(V_NOPAS)) * positif(14 - V_NOTRAIT) * null(V_ZDC - 1) * (1 - V_ACTPAS) * (1 - V_RASDL) ;

LIGPAS = (1 - positif(V_NOPAS)) * positif(14 - V_NOTRAIT) * (1 - LIGPASEND) * (1 - positif(V_0AZ * BOOL_0AM)) * (1 - V_ACTPAS) * (1 - V_RASDL) ;

LIGPASIND = LIGPAS * BOOL_0AM ;

LIGPASZ = (1 - positif(V_NOPAS)) * positif(14 - V_NOTRAIT) * BOOL_0AM * positif(positif(V_0AZ + 0) + null(V_ZDC - 4)) * (1 - V_ACTPAS) * (1 - V_RASDL) ;

LIGPASPART = (1 - null(INDPAS - 1)) * LIGPAS ;

LIGRASTSPE = positif(RASTSPE1 + RASTSPE2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS ;

LIGRASAUSA = positif(RASAUSA1 + RASAUSA2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS ;

LIGRASASSO = positif(RASASSO1 + RASASSO2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS ;

LIGRASLEMP = positif(RASLEMP1 + RASLEMP2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS ;

LIGRASPAC = positif(RASPAC) * (1 - null(RASTXPAC)) * positif(RASTOT) * LIGPAS ;

LIGRASRVTO = positif(RASRVTO) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS ;

LIGRASRVTOS = positif(RASPSRVTO) * positif(RASTOT) * LIGPAS ;

LIGRASRF = positif(RASRF) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS ;

LIGRASRFS = positif(RASPSRF) * positif(RASTOT) * LIGPAS ;

LIGRASBA = positif(RASBA1 + RASBA2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS ;

LIGRASBIC = positif(RASBIC1 + RASBIC2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS ;

LIGRASBNC = positif(RASBNC1 + RASBNC2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS ;

LIGPSBIC = positif(RASPSBIC1 + RASPSBIC2) * positif(RASTOT) * LIGPAS ;

LIGPSPAC = positif(RASPSPAC) * positif(RASTOT) * LIGPAS ;

LIG5HY = positif(RASPS5HY) * positif(RASTOT) * LIGPAS ;

LIG5IY = positif(RASPS5IY) * positif(RASTOT) * LIGPAS ;

LIGIRRAS = positif(LIGRASTSPE + LIGRASAUSA + LIGRASASSO + LIGRASLEMP + LIGRASPAC + LIGRASRVTO + LIGRASRF + LIGRASBA + LIGRASBIC + LIGRASBNC) * LIGPAS ;

LIGPSRAS = positif(LIGRASRFS + LIGRASRVTOS + LIGPSBIC + LIGPSPAC + LIG5HY + LIG5IY) * LIGPAS ; 

LIGRASTOTF = positif(LIGRASRVTO + LIGRASRF + LIGRASPAC + LIGPSPAC + LIGRASRFS + LIGRASRVTOS) ;

LIGRASTOT1 = positif(RASTSPE1 + RASAUSA1 + RASASSO1 + RASBA1 + RASBIC1 + RASBNC1 + RASPSBIC1 + RASPS5HY) ;

LIGRASTOT2 = positif(RASTSPE2 + RASAUSA2 + RASASSO2 + RASBA2 + RASBIC2 + RASBNC2 + RASPSBIC2 + RASPS5IY) ;

LIGRAS = positif(LIGIRRAS + LIGPSRAS) * positif(RASTOT) * LIGPAS ;

LIGRASCDV = LIGRAS * (1 - BOOL_0AM) ;
LIGRASOM = LIGRAS * BOOL_0AM ;

LIGRASSUP = positif_ou_nul(RASTOT - 5) * LIGRAS ;

LIGRASINF = positif(5 - RASTOT) * LIGRAS ;

INDNCTAZ = INDPAS + (5 * INDTAZ) ;

regle taux 201822:
application : iliad ;


LIGPASENDL = (1 - positif(V_NOPAS)) * positif(14 - V_NOTRAIT) * null(V_ZDC - 1) * positif(V_ACTPAS + V_RASDL) ;

LIGPASDL = (1 - positif(V_NOPAS)) * (1 - LIGPASENDL) * positif(V_ACTPAS + V_RASDL + V_RASOM1 + V_RASOM2) * positif(positif(14 - V_NOTRAIT) + V_RASOM1 + V_RASOM2) ;

LIGPASZDL = (1 - positif(V_NOPAS)) * positif(14 - V_NOTRAIT) * BOOL_0AM * positif(positif(V_0AZ + 0) + null(V_ZDC - 4)) * positif(V_ACTPAS + V_RASDL) ;

regle taux 201824:
application : iliad ;


VARZDC = positif(null(V_ZDC - 1) + null(V_RASDC - 1) + null(VARZDC_A - 1)) ;

PASOPTION8 = positif((1 - (null(V_RASOI1 + 0) * null(V_RASOI2 + 0))) + positif(V_RASOC1 + V_RASOC2) + (present(V_RASOP1) * positif(V_RAS_ANO1))  + (present(V_RASOP2) * positif(V_RAS_ANO2))) ;

LIGPASE1 = (1 - positif(V_NOPAS)) * positif_ou_nul(V_NOTRAIT - 14) * VARZDC * (1 - PASOPTION8) * (1 - positif(V_RASOM1 + V_RASOM2)) ;

LIGPASZ1 = (1 - positif(V_NOPAS)) * BOOL_0AM * positif(positif(V_0AZ + 0) + null(V_ZDC - 4)) * positif_ou_nul(V_NOTRAIT - 14) * (1 - PASOPTION8) * (1 - positif(V_RASOM1 + V_RASOM2)) ;

LIGPAS1 = (1 - positif(V_NOPAS)) * positif_ou_nul(V_NOTRAIT - 14) * (1 - LIGPASE1) * (1 - LIGPASZ1) * (1 - PASOPTION8) * (1 - positif(V_RASOM1 + V_RASOM2)) ;

LIGRASIND21 = BOOL_0AM * (1 - positif(RMOND + DMOND)) * (1 - positif(V_NOPAS)) * positif(null(V_NOTRAIT - 14) + null(V_NOTRAIT - 16)) * LIGPAS1 ;

LIGRASIND31 = BOOL_0AM * (1 - positif(RMOND + DMOND)) * (1 - positif(V_NOPAS)) * positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)) * LIGPAS1 ;

LIGRASIND61 = BOOL_0AM * (1 - positif(RMOND + DMOND)) * (1 - positif(V_NOPAS)) * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) * LIGPAS1 ;

LIGRASIND1 = LIGRASIND21 + LIGRASIND31 + LIGRASIND61 ;

LIGPASPART1 = null(INDPAS - 2) * LIGPAS1 ;

LIGPASZEP1 = positif(LIGPASE1 + LIGPASZ1 + LIGPASPART1) ;

LIGRASTSPE1 = positif(RASTSPE1 + RASTSPE2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS1 ;

LIGRASAUSA1 = positif(RASAUSA1 + RASAUSA2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS1 ;

LIGRASASSO1 = positif(RASASSO1 + RASASSO2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS1 ;

LIGRASLEMP1 = positif(RASLEMP1 + RASLEMP2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS1 ;

LIGRASPAC1 = positif(RASPAC) * (1 - null(RASTXPAC)) * positif(RASTOT) * LIGPAS1 ;

LIGRASRVTO1 = positif(RASRVTO) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS1 ;

LIGRASRVTOS1 = positif(RASPSRVTO) * positif(RASTOT) * LIGPAS1 ;

LIGRASRF1 = positif(RASRF) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS1 ;

LIGRASRFS1 = positif(RASPSRF) * positif(RASTOT) * LIGPAS1 ;

LIGRASBA1 = positif(RASBA1 + RASBA2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS1 ;

LIGRASBIC1 = positif(RASBIC1 + RASBIC2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS1 ;

LIGRASBNC1 = positif(RASBNC1 + RASBNC2) * (1 - null(RASTXFOYER)) * positif(RASTOT) * LIGPAS1 ;

LIGPSBIC1 = positif(RASPSBIC1 + RASPSBIC2) * positif(RASTOT) * LIGPAS1 ;

LIGPSPAC1 = positif(RASPSPAC) * positif(RASTOT) * LIGPAS1 ;

LIG5HY1 = positif(RASPS5HY) * positif(RASTOT) * LIGPAS1 ;

LIG5IY1 = positif(RASPS5IY) * positif(RASTOT) * LIGPAS1 ;

LIGIRRAS1 = positif(LIGRASTSPE1 + LIGRASAUSA1 + LIGRASASSO1 + LIGRASLEMP1 + LIGRASPAC1 + LIGRASRVTO1 + LIGRASRF1 + LIGRASBA1 + LIGRASBIC1 + LIGRASBNC1) * LIGPAS1 ;

LIGPSRAS1 = positif(LIGRASRFS1 + LIGRASRVTOS1 + LIGPSBIC1 + LIGPSPAC1 + LIG5HY1 + LIG5IY1) * LIGPAS1 ;

LIGRAS1 = positif(LIGIRRAS1 + LIGPSRAS1) * LIGPAS1 ;

LIGRASCDV1 = LIGRAS1 * (1 - BOOL_0AM) ;
LIGRASOM1 = LIGRAS1 * BOOL_0AM ;

LIGPAS21 = positif_ou_nul(RASTOT - 5) * positif(null(V_NOTRAIT - 14) + null(V_NOTRAIT - 16)) * LIGRAS1 ;

LIGPAS31 = positif_ou_nul(RASTOT - 5) * positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)) * LIGRAS1 ;

LIGPAS61 = positif_ou_nul(RASTOT - 5) * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) * LIGRAS1 ;

LIGPASI1 = positif(5 - RASTOT) * LIGRAS1 ;

regle taux 201826:
application : iliad ;


LIGPAS8D = (1 - positif(V_NOPAS)) * positif_ou_nul(V_NOTRAIT - 14) * VARZDC * PASOPTION8 * (1 - positif(V_RASOM1 + V_RASOM2)) ;

LIGPAS8Z = (1 - positif(V_NOPAS)) * positif_ou_nul(V_NOTRAIT - 14) * BOOL_0AM * positif(positif(V_0AZ + 0) + null(V_ZDC - 4)) * PASOPTION8 * (1 - positif(V_RASOM1 + V_RASOM2)) ;

LIGPAS8 = (1 - positif(V_NOPAS)) * positif_ou_nul(V_NOTRAIT - 14) * (1 - LIGPAS8D) * (1 - LIGPAS8Z) * PASOPTION8 * (1 - positif(V_RASOM1 + V_RASOM2)) ;

LIGPAS28 = (1 - positif(V_NOPAS)) * positif(null(V_NOTRAIT - 14) + null(V_NOTRAIT - 16)) * LIGPAS8 ;

LIGPAS38 = (1 - positif(V_NOPAS)) * positif(null(V_NOTRAIT - 23) + null(V_NOTRAIT - 33) + null(V_NOTRAIT - 43) + null(V_NOTRAIT - 53) + null(V_NOTRAIT - 63)) * LIGPAS8 ;

LIGPAS68 = (1 - positif(V_NOPAS)) * positif(null(V_NOTRAIT - 26) + null(V_NOTRAIT - 36) + null(V_NOTRAIT - 46) + null(V_NOTRAIT - 56) + null(V_NOTRAIT - 66)) * LIGPAS8 ;

LIGRASIND28 = BOOL_0AM * (1 - positif(RMOND + DMOND)) * LIGPAS28 ;

LIGRASIND38 = BOOL_0AM * (1 - positif(RMOND + DMOND)) * LIGPAS38 ;

LIGRASIND68 = BOOL_0AM * (1 - positif(RMOND + DMOND)) * LIGPAS68 ;

LIGRASIND8 = BOOL_0AM * (1 - positif(RMOND + DMOND)) * LIGPAS8 ;

LIGPAS8F = (1 - positif(V_RASOI1 + V_RASOI2)) * BOOL_0AM * LIGPAS8 ;

LIGPAS8I = positif(V_RASOI1 + V_RASOI2) * LIGPAS8 ;

LIGPAS8C1 = positif(V_RASOC1) * LIGPAS8 ;

LIGPAS8C2 = positif(V_RASOC2) * LIGPAS8 ;

LIGPAS8C = positif(LIGPAS8C1 + LIGPAS8C2) ;

LIGPAS8T = positif(null(1 - V_RASOP1) + null(1 - V_RASOP2)) * positif(RASTOTT) * LIGPAS8 ;

LIGPAS8M = positif((null(V_RASOP1) * positif(V_RAS_ANO1))  + (null(V_RASOP2) * positif(V_RAS_ANO2))) * positif(RASTOTM) * LIGPAS8 ;

LIGPAS8P = null(INDPAS - 2) * LIGPAS8 ;

LIGPAS8ZEP = positif(LIGPAS8Z + LIGPAS8D + LIGPAS8P) ;

LIGPAS8OT = positif(null(1 - V_RASOP1) + null(1 - V_RASOP2)) * positif(RASTOTT) * LIGPAS8 ;

LIGPAS8OM = (1 - LIGPAS8OT) * positif(RASTOTM) * LIGPAS8 ;

LIGPAS38M = (1 - positif(V_RASOP1 + V_RASOP2)) * (1 - null(ANNEECOR - V_RAS_ANO1)) * (1 - null(ANNEECOR - V_RAS_ANO2)) * positif(RASTOTM - 5) * LIGPAS38 ;

LIGPAS38M1 = LIGPAS38M ;

LIGPAS38T = positif(V_RASOP1 + V_RASOP2) * (1 - null(ANNEECOR - V_RAS_ANO1)) * (1 - null(ANNEECOR - V_RAS_ANO2)) * positif(RASTOTT - 15) * LIGPAS38 ;

LIGPAS38T1 = LIGPAS38T ;

LIGPAS38T2 = positif(present(V_RASOP1) + present(V_RASOP2)) * positif(null(ANNEECOR - V_RAS_ANO1) + null(ANNEECOR - V_RAS_ANO2)) * positif(RASTOTT - 15) * LIGPAS38 ;

LIGPAS28M = (1 - positif(V_RASOP1 + V_RASOP2)) * (1 - null(ANNEECOR - V_RAS_ANO1)) * (1 - null(ANNEECOR - V_RAS_ANO2)) * positif(RASTOTM - 5) * LIGPAS28 ;

LIGPAS28M1 = LIGPAS28M ;

LIGPAS28T = positif(V_RASOP1 + V_RASOP2) * (1 - null(ANNEECOR - V_RAS_ANO1)) * (1 - null(ANNEECOR - V_RAS_ANO2)) * positif(RASTOTT - 15) * LIGPAS28 ;

LIGPAS28T1 = LIGPAS28T ;

LIGPAS28T2 = positif(present(V_RASOP1) + present(V_RASOP2)) * positif(null(ANNEECOR - V_RAS_ANO1) + null(ANNEECOR - V_RAS_ANO2)) * positif(RASTOTT - 15) * LIGPAS28 ;

LIGPAS68M = (1 - positif(V_RASOP1 + V_RASOP2)) * (1 - null(ANNEECOR - V_RAS_ANO1)) * (1 - null(ANNEECOR - V_RAS_ANO2)) * positif(RASTOTM - 5) * LIGPAS68 ;

LIGPAS68M1 = LIGPAS68M ;

LIGPAS68T = positif(V_RASOP1 + V_RASOP2) * (1 - null(ANNEECOR - V_RAS_ANO1)) * (1 - null(ANNEECOR - V_RAS_ANO2)) * positif(RASTOTT - 15) * LIGPAS68 ;

LIGPAS68T1 = LIGPAS68T ;

LIGPAS68T2 = positif(present(V_RASOP1) + present(V_RASOP2)) * positif(null(ANNEECOR - V_RAS_ANO1) + null(ANNEECOR - V_RAS_ANO2)) * positif(RASTOTT - 15) * LIGPAS68 ;

LIGPAS81 = positif(5 - RASTOTM) * LIGPAS8OM ;

LIGPAS82 = positif(15 - RASTOTT) * LIGPAS8OT ;

regle taux 201830:
application : iliad ;



BASACOMPENIR = CODSAA + CODSAB ;


SANACOMIR = arr(BASACOMPENIR * TXSANCTION/100) ;


BASACOMPENPS = CODSAC + CODSAD ;


SANACOMPS = arr(BASACOMPENPS * TXSANCTION/100) ;



SACPTHD = positif(SANACOMIR + SANACOMPS) * 1
        + (1 - positif(SANACOMIR + SANACOMPS)) * 0 ;

regle taux 201840:
application : iliad ;



NBPART = positif(null(NBPT - (V_BTNBP1/100)) * null(NBPT - (V_BTNBP2/100)) * (1 - positif(CODSDP * CODSDQ)) * (NBPT * V_BTNBP1 * V_BTNBP2)
               + null(NBPT - CODSDP/100) * null(NBPT - CODSDQ/100) * (NBPT * CODSDP * CODSDQ));


SPIE1IR = positif(CODSCB + CODSCC + CODSCD + CODSCE + CODSCF + CODSCG + CODSCL + CODSCM + CODSCO);
SPIE2IR = positif(CODSDB + CODSDC + CODSDD + CODSDE + CODSDF + CODSDG + CODSDL + CODSDM + CODSDO);

SPIE12IR = SPIE1IR + SPIE2IR ;


SPIEIRPS = positif(CODSCA + CODSDA) ;


DECSANIR = NBPART * positif(SPIE12IR + SPIEIRPS) ;



PASFARFAITIR = DECSANIR * PASTOTAL ;


RASATDEC1IR = DECSANIR * arr((TSTAUXRASV * (CODSAG/TX10))/100) ;
RASATDEC2IR = DECSANIR * arr((TSTAUXRASC * (CODSAH/TX10))/100) ;
ACATTDEC1IR = DECSANIR * CODSAI * NOMBRE12 ;
ACATTDEC2IR = DECSANIR * CODSAJ * NOMBRE12 ;

PASATDEC1IR = DECSANIR * RASATDEC1IR + ACATTDEC1IR ;
PASATDEC2IR = DECSANIR * RASATDEC2IR + ACATTDEC2IR ;

PASATFOYIR = DECSANIR * (PASATDEC1IR + PASATDEC2IR) ;


PASEFDEC1IR = DECSANIR * max(0 , (COD8HV + CODSAW + CODSAS + CODSAU - COD8HY)) ;
PASEFDEC2IR = DECSANIR * max(0 , (COD8IV + CODSAX + CODSAT + CODSAV - COD8IY)) ;

PASEFFOYIR = DECSANIR * (PASEFDEC1IR + PASEFDEC2IR) ;



SCBASIR = DECSANIR * (positif_ou_nul((PASEFFOYIR - arr((TX90/100) * PASFARFAITIR) ) + max(0,(PASEFFOYIR - PASATFOYIR))) * 0
                   + (1 - positif_ou_nul((PASEFFOYIR - arr((TX90/100) * PASFARFAITIR)) + max(0,(PASEFFOYIR - PASATFOYIR)))) * max(0 , (min(PASFARFAITIR , PASATFOYIR) - PASEFFOYIR))) ; 


SANCTXIR = positif(null(PASFARFAITIR) + positif(PASEFFOYIR - PASFARFAITIR) + positif_ou_nul(PASEFFOYIR - arr((TX70/100) * PASFARFAITIR))) ;

SCTXIR = DECSANIR * positif(SCBASIR) * (positif(null(PASFARFAITIR) + positif(PASEFFOYIR - PASFARFAITIR)) * 0
                                     +  positif_ou_nul(PASEFFOYIR - arr((TX70/100) * PASFARFAITIR)) * TX10
                                     + (1 - positif(SANCTXIR)) * arr(TX050 * ((PASFARFAITIR - PASEFFOYIR)/PASFARFAITIR)*100 * 10)/10) ;


SCMTIR = DECSANIR * arr(SCBASIR * SCTXIR/100) ;	     

regle taux 201850:
application : iliad ;


SPIE1PS = positif(CODSCH + CODSCI + CODSCJ + CODSCK + CODSCN) ;
SPIE2PS = positif(CODSDH + CODSDI + CODSDJ + CODSDK + CODSDN) ;

SPIE12PS = SPIE1PS + SPIE2PS ;


DECSANPS = NBPART * positif(SPIE12PS + SPIEIRPS) ;



BASACDEC1PS = DECSANPS * (positif(V_0AM + V_0AO) * arr(RASPSBIC1 + RASPS5HY + (RASPSRVTO/2) + (RASPSRF/2) + (RASPSBICP/2) + (RASPS5JY/2))
                       + (1 - positif(V_0AM + V_0AO)) * arr(RASPSBIC1 + RASPS5HY + RASPSRVTO + RASPSRF + RASPSBICP + RASPS5JY)) ;

BASACDEC2PS = DECSANPS * positif(V_0AM + V_0AO) * arr(RASPSBIC2 + RASPS5IY + (RASPSRVTO/2) + (RASPSRF/2) + (RASPSBICP/2) + (RASPS5JY/2)) ;

ACOMDEC1PS = DECSANPS * arr(BASACDEC1PS * TX172/100) ;

ACOMDEC2PS = DECSANPS * arr(BASACDEC2PS * TX172/100) ;

PASPARFAITPS = DECSANPS * (ACOMDEC1PS + ACOMDEC2PS) ;


PASATDEC1PS = DECSANPS * (CODSAK * NOMBRE12) ;
PASATDEC2PS = DECSANPS * (CODSAL * NOMBRE12) ;

PASATFOYPS = DECSANPS * (PASATDEC1PS + PASATDEC2PS) ;



PASEFDEC1PS = DECSANPS * max(0 , CODSAQ - COD8HZ) ;
PASEFDEC2PS = DECSANPS * max(0 , CODSAR - COD8IZ) ;

PASEFFOYPS = DECSANPS * (PASEFDEC1PS + PASEFDEC2PS) ;



SCBASPS = DECSANPS * (positif_ou_nul((PASEFFOYPS - arr((TX90/100) * PASPARFAITPS)) + max(0,(PASEFFOYPS - PASATFOYPS))) * 0
                   + (1 - positif_ou_nul((PASEFFOYPS - arr((TX90/100)) * PASPARFAITPS) + max(0,(PASEFFOYPS - PASATFOYPS)))) * max(0 , (min(PASPARFAITPS , PASATFOYPS) - PASEFFOYPS))) ;


SANCTXPS = positif(null(PASPARFAITPS) + positif(PASEFFOYPS - PASPARFAITPS) +  positif_ou_nul(PASEFFOYPS - arr((TX70/100) * PASPARFAITPS))) ;

SCTXPS = DECSANPS * positif(SCBASPS) * (positif(null(PASPARFAITPS) + positif(PASEFFOYPS - PASPARFAITPS)) * 0
                                     +  positif_ou_nul(PASEFFOYPS - arr((TX70/100) * PASPARFAITPS)) * TX10
                                     + (1 - positif(SANCTXPS)) * arr(TX050 * ((PASPARFAITPS - PASEFFOYPS)/PASPARFAITPS)*100 *10)/10) ;


SCMTPS = DECSANPS * arr(SCBASPS * SCTXPS/100);
regle taux 201860:
application : iliad ;


PRESAN = positif(CODSCA + CODSDA) * (1 - positif(SPIE1IR + SPIE2IR)) * positif(SCMTIR + SCMTPS)
       + (1 - positif(CODSCA + CODSDA)) * positif(SPIE1IR + SPIE2IR) * positif(SCMTIR + SCMTPS)
       + positif(CODSCA + CODSDA) * positif(SPIE1IR + SPIE2IR) * positif(SCMTIR + SCMTPS);

SACIND = positif(CODSCA + CODSDA) * (1 - positif(SPIE1IR + SPIE2IR)) * positif(SCMTIR + SCMTPS) * 1
       + (1 - positif(CODSCA + CODSDA)) * positif(SPIE1IR + SPIE2IR) * positif(SCMTIR + SCMTPS) * 2
       + positif(CODSCA + CODSDA) * positif(SPIE1IR + SPIE2IR) * positif(SCMTIR + SCMTPS) * 3 
       + (1 - positif(PRESAN)) * 0;


RDSANC = positif(
              positif(CODSCD) * (1 - positif(present(COD5BF) + present(COD5AN) + present(COD5CF)))
            + positif(CODSDD) * (1 - positif(present(COD5BI) + present(COD5BN) + present(COD5CI)))
            + positif(CODSCE) * (1 - positif(present(COD5AO) + present(COD5AP)))
            + positif(CODSDE) * (1 - positif(present(COD5BO) + present(COD5BP)))
            + positif(CODSCF) * (1 - positif(present(COD5AF)))
            + positif(CODSDF) * (1 - positif(present(COD5AI)))
            + positif(CODSCG + CODSCL + CODSCO) * (1 - positif(present(COD1GK) + present(COD1HK)))
            + positif(CODSDG + CODSDL + CODSDO) * (1 - positif(present(COD1GL) + present(COD1HL)))
                );
regle taux 201870:
application : iliad ;

PENAIRCS = PIR + PTAXA + PHAUTREV + PPCAP + PPSOL + PCSG + PRDS + PCDIS + PCVN
           + PGLOA + PRSE1 + PRSE2 + PRSE3 + PRSE4 + PRSE5 + PRSE6 + PRSE8 + PCSG820 ;
PENAPAS =  SANACOMIR + SANACOMPS + SCMTIR + SCMTPS ;

PTOTIRCS = PIR + PTAXA + PHAUTREV + PPCAP + PPSOL + PCSG + PRDS + PCDIS + PCVN
           + PGLOA + PRSE1 + PRSE2 + PRSE3 + PRSE4 + PRSE5 + PRSE6 + PRSE8 + PCSG820
           + SANACOMIR + SANACOMPS + SCMTIR + SCMTPS ;

AGPENA = PTOTIRCS ;

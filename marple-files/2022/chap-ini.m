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
regle irisf 1:
application : bareme , iliad ;


BIDON = 1 ;

regle 951050:
application : iliad ;

SOMMEA719 = (

   present( BAEXV ) + present ( BACREV ) + present( 4BACREV ) + present ( BA1AV ) + present ( BACDEV ) 
 + present( BAEXC ) + present ( BACREC ) + present( 4BACREC ) + present ( BA1AC ) + present ( BACDEC ) 
 + present( BAEXP ) + present ( BACREP ) + present( 4BACREP ) + present ( BA1AP ) + present ( BACDEP ) 
 + present( BAHEXV ) + present ( BAHREV ) + present( 4BAHREV ) + present ( BAHDEV ) 
 + present( BAHEXC ) + present ( BAHREC ) + present( 4BAHREC ) + present ( BAHDEC ) 
 + present( BAHEXP ) + present ( BAHREP ) + present( 4BAHREP ) + present ( BAHDEP ) 
 + present(COD5XT) + present(COD5XV) + present(COD5XU) + present(COD5XW)
 + present(COD5HA) + present(COD5IA) + present(COD5JA)

 + present( BICEXV ) + present ( BICNOV ) 
 + present ( BI1AV ) + present ( BICDNV ) 
 + present( BICEXC ) + present ( BICNOC ) 
 + present ( BI1AC ) + present ( BICDNC ) 
 + present( BICEXP ) + present ( BICNOP ) 
 + present ( BI1AP ) + present ( BICDNP ) 
 + present( BIHEXV ) + present ( BIHNOV ) 
 + present ( BIHDNV ) 
 + present( BIHEXC ) + present ( BIHNOC ) 
 + present ( BIHDNC ) 
 + present( BIHEXP ) + present ( BIHNOP ) 
 + present ( BIHDNP )+ present ( COD5UI )
 + present (COD5VI)  + present(COD5WI)
 + present (CODCKC) + present(CODCKI)
 + present (CODCLC) + present(CODCLI)
 + present (CODCMC) + present(CODCMI)

 + present( BICNPEXV ) + present ( BICREV ) + present( LOCNPCGAV )
 + present ( BI2AV ) + present ( BICDEV ) + present( LOCDEFNPCGAV )
 + present( BICNPEXC ) + present ( BICREC ) + present( LOCNPCGAC )
 + present ( BI2AC ) + present ( BICDEC ) + present( LOCDEFNPCGAC )
 + present( BICNPEXP ) + present ( BICREP ) + present( LOCNPCGAPAC )
 + present ( BI2AP ) + present ( BICDEP ) + present( LOCDEFNPCGAPAC )
 + present( BICNPHEXV ) + present ( BICHREV ) + present ( LOCNPV )
 + present ( BICHDEV ) + present ( LOCDEFNPV )
 + present( BICNPHEXC ) + present ( BICHREC ) + present ( LOCNPC )
 + present ( BICHDEC ) + present ( LOCDEFNPC )
 + present( BICNPHEXP ) + present ( BICHREP ) + present ( LOCNPPAC )
 + present ( BICHDEP ) + present ( LOCDEFNPPAC )
 + present (COD5TF) + present(COD5UF) + present(COD5VF) + present(LOCGITCV)
 + present(LOCGITHCV) + present(LOCGITCC) + present(LOCGITHCC) + present(LOCGITCP)
 + present(LOCGITHCP) + present(COD5WE) + present(COD5WF) + present(COD5XE)
 + present(COD5XF) + present(COD5YE) + present(COD5YF) + present(CODCNC) + present(CODCNI) + present(CODCOC) + present(CODCOI) + present(CODCPC)
 + present(CODCPI)

 + present( BNCEXV ) + present ( BNCREV ) + present ( BN1AV ) + present ( BNCDEV ) 
 + present( BNCEXC ) + present ( BNCREC ) + present ( BN1AC ) + present ( BNCDEC ) 
 + present( BNCEXP ) + present ( BNCREP ) + present ( BN1AP ) + present ( BNCDEP ) 
 + present( BNHEXV ) + present ( BNHREV ) + present ( BNHDEV ) 
 + present( BNHEXC ) + present ( BNHREC ) + present ( BNHDEC ) 
 + present( BNHEXP ) + present ( BNHREP ) + present ( BNHDEP )
 + present(COD5QA) + present(COD5RA) + present(COD5SA) + present(CODCQC)
 + present(CODCQI) + present(CODCRC) + present(CODCRI) + present(CODCSC)
 + present(CODCSI)

 + present ( BNCNPREXAAV ) + present ( BNCAABV )   + present ( BNCAADV )  + present ( BNCNPREXV ) 
 + present( ANOCEP ) + present( DNOCEP ) + present( PVINVE ) + present( INVENTV )
 + present ( BNCNPREXAAC ) + present ( BNCAABC ) + present ( BNCAADC ) + present ( BNCNPREXC )
 + present( ANOVEP ) + present( DNOCEPC ) + present( PVINCE ) + present( INVENTC )
 + present ( BNCNPREXAAP ) + present ( BNCAABP ) + present ( BNCAADP ) + present ( BNCNPREXP )
 + present( ANOPEP ) + present( DNOCEPP ) + present( PVINPE ) + present( INVENTP ) 
 + present(COD5QJ) + present(COD5RJ) +present(COD5SJ)+ present(CODCJG) + present(CODCNS) + present(CODCOS)+ present(CODCRF)+ present(CODCSF) + present(CODCSN)
 + 0
        ) ;

regle 951060:
application : iliad ;

SOMMEA030 =     
                somme(i=1..4: positif(TSHALLOi) + positif(ALLOi)
		+ positif(CARTSPi) + positif(REMPLAPi)
		+ positif(CARTSNBAPi) + positif(REMPLANBPi)
                + positif(PRBRi)
		+ positif(CARPEPi) + positif(CARPENBAPi)
                + positif(PALIi) + positif(FRNi) 
		+ positif(PENSALPi) + positif(PENSALNBPi)
		)
 + positif(BAFORESTP) + positif(BAFPVP)  + positif(BAF1AP)
 + positif(BAEXP) + positif(BACREP) + positif(4BACREP)  
 + positif(BA1AP) + positif(BACDEP)
 + positif(BAHEXP) + positif(BAHREP) + positif(4BAHREP) 
 + positif(BAHDEP) 
 + positif(MIBEXP) + positif(MIBVENP) + positif(MIBPRESP)  + positif(MIBPVP)  + positif(MIB1AP)  + positif(MIBDEP)
 + positif(BICPMVCTP) + positif(BICEXP) + positif(BICNOP) + positif(BI1AP)  
 + positif(BICDNP) 
 + positif(BIHEXP) + positif(BIHNOP) + positif(BIHDNP)  
 + positif(MIBNPEXP)  + positif(MIBNPVENP)  + positif(MIBNPPRESP)  + positif(MIBNPPVP)  + positif(MIBNP1AP)  + positif(MIBNPDEP)
 + positif(BICNPEXP)  + positif(BICREP) + positif(BI2AP)  + positif(BICDEP)  
 + positif(BICNPHEXP) + positif(BICHREP) + positif(BICHDEP) 
 + positif(BNCPROEXP)  + positif(BNCPROP)  + positif(BNCPROPVP)  + positif(BNCPRO1AP)  + positif(BNCPRODEP) + positif(BNCPMVCTP)
 + positif(BNCEXP)  + positif(BNCREP) + positif(BN1AP) 
 + positif(BNCDEP)
 + positif(BNHEXP)  + positif(BNHREP)  + positif(BNHDEP) + positif(BNCCRP)
 + positif(BNCNPP)  + positif(BNCNPPVP)  + positif(BNCNP1AP)  + positif(BNCNPDEP)
 + positif(ANOPEP) + positif(PVINPE) + positif(INVENTP) + positif(DNOCEPP) + positif(BNCCRFP)
 + positif(BNCAABP) + positif(BNCAADP)
 + positif(RCSP) 
 + positif(BAPERPP) 
 + positif(PERPP) + positif(PERP_COTP) + positif(PLAF_PERPP) + positif(COD6NU) + positif(COD6OU)
 + somme(i=1..4: positif(PEBFi))
 + positif( COTF1 ) + positif( COTF2 ) + positif( COTF3 ) + positif( COTF4 )
 + positif (BNCNPREXAAP) + positif (BNCNPREXP)
 + positif(AUTOBICVP) + positif(AUTOBICPP) 
 + positif(AUTOBNCP)
 + positif(COD5VJ) + positif(COD5VK) + positif(COD5VL) 
 + positif(LOCNPCGAPAC) + positif(LOCGITCP) + positif(LOCGITHCP) 
 + positif(LOCDEFNPCGAPAC)
 + positif(LOCNPPAC) + positif(LOCDEFNPPAC) 
 + positif(XSPENPP)
 + positif(BANOCGAP) + positif(MIBMEUP) + positif(MIBGITEP) + positif(LOCGITP) 
 + positif(SALEXT1) + positif(COD1CE) + positif(COD1CH)
 + positif(SALEXT2) + positif(COD1DE) + positif(COD1DH)
 + positif(SALEXT3) + positif(COD1EE) + positif(COD1EH)
 + positif(SALEXT4) + positif(COD1FE) + positif(COD1FH)
 + positif(RDSYPP) + positif(CARTSP1)
 + positif(PENIN1) + positif(PENIN2) + positif(PENIN3) + positif(PENIN4)
 + positif(CODRCZ) + positif(CODRDZ) + positif(CODREZ) + positif(CODRFZ)
 + positif(COD1CF) + positif(COD1CG) + positif(COD1CL) + positif(COD1CM)
 + positif(COD1DF) + positif(COD1DG) + positif(COD1DL) + positif(COD1DM)
 + positif(COD1EF) + positif(COD1EG) + positif(COD1EL) + positif(COD1EM)
 + positif(COD1FF) + positif(COD1FG) + positif(COD1FL) + positif(COD1FM) + positif(CODRCK)
 + positif(CODRCF) + positif(CODRDF) + positif(CODREF) + positif(CODRFF) + positif(CODRCG) 
 + positif(CODRDG) + positif(CODRGG) + positif(CODRFG) + positif(CODRCL) + positif(CODRDL) 
 + positif(CODREL) + positif(CODRFL) + positif(CODRCM) + positif(REMPLAP1) + positif(CARPEP1) 
 + positif(CODRDM) + positif(PENSALP2) + positif(CODREM) 
 + positif(CODRFM) + positif(COD1IB) + positif(COD1JB) 
 + positif(COD1CA) + positif(COD1DA) + positif(COD1EA) + positif(COD1FA) 
 + positif(COD1IE) + positif(COD1JE)
 + positif(COD1KE) + positif(COD1LE) + positif(COD1IF) + positif(COD1JF) + positif(COD1KF)
 + positif(COD1LF) 
 + positif(COD1GP) + positif(COD1GQ) + positif(COD1GR) + positif(COD1GS)
 + positif(COD1IG) + positif(COD1IH) + positif(COD1JG) + positif(COD1JH) + positif(COD1KG) 
 + positif(COD1KH) + positif(COD1LG) + positif(COD1LH)+ positif(COD1HP) + positif(COD1HQ) 
 + positif(COD1HR) + positif(COD1HS)
 + positif(COD1CI) + positif(COD1DI) + positif(COD1EI) + positif(COD1FI) + positif(COD1CT)
 + positif(COD1DT) + positif(COD1ET) + positif(COD1FT) 
 + positif(COD5CK) + positif(COD5CL) + positif(COD5FF) + positif(COD5FG) + positif(COD5GY) + positif(COD5GZ)
 + positif(COD5MD) + positif(COD5SZ) + positif(COD5WR) + positif(COD5WS) + positif(COD5ZA) + positif(COD5ZB) 
 + positif(COD5ZJ) + positif(COD5ZK) + positif(COD5ZN) + positif(COD5ZO) + positif(COD5ZS) + positif(COD5ZX) 
 + positif(INVENTP) + positif(XSPENPP) + positif(BICPMVCTP) 
 + positif(BNCNPREXP) + positif(COD5AH) + positif(COD5BH) + positif(COD5CM) + positif(COD5CN) 
 + positif(COD5CQ)
 + positif(COD5CR) + positif(COD5CU) + positif(COD5CV) + positif(COD5CY) + positif(COD5CZ) 
 + positif(COD5ED) + positif(COD5FB) + positif(COD5FD) + positif(COD5FK) + positif(COD5FL) 
 + positif(COD5FM) + positif(COD5FN) + positif(CODCMC) + positif(CODCMI) + positif(CODCOS) 
 + positif(CODCPC) + positif(CODCPI) + positif(CODCSC) + positif(CODCSF) + positif(CODCSI) 
 + positif(COD5PW) + positif(COD5TP) + positif(COD5VQ) + positif(COD5VV) + positif(COD5VW) 
 + positif(COD5VX) + positif(COD5ZH) + positif(COD5ZI) + positif(COD5ZL) + positif(COD5ZM) 
 + positif(COD5ZP) + positif(COD5ZQ) + positif(COD5ZR) + positif(COD5ZW) + positif(COD5ZY) 
 + positif(COD5ZZ) + positif(COD5ZT) + positif(COD5ZU) + positif(COD5EU)
 + positif(COD5EV) + positif(COD5JA) + positif(COD5SA) + positif(COD5SJ) + positif(COD5VF) 
 + positif(COD5WI) + positif(COD5YE) + positif(COD5YF)
 + positif(COD8UM) + positif(CODZRF) + positif(COD8JV) + positif(COD8JY) 
 + positif(COD8KV) + positif(COD8KY) + positif(COD8LI) + positif(COD8LL)
 + positif(COD8JW) + positif(COD8JX) + positif(COD8JZ) + positif(COD8KW)
 + positif(COD8KX) + positif(COD8LV) + positif(COD8LW) + positif(COD8LX)
 + positif(COD8LY) + positif(COD8LZ) + positif(COD8MV) + positif(COD8MW)
 + positif(COD8MX) + positif(COD8MY) + positif(COD8MZ) + positif(REVMAR3) 
 + positif(COD1IA) + positif(COD1JA) + positif(COD1KA) + positif(COD1LA)
 + positif(REMPLAP2) + positif(CARPEP2) + positif(CARTSP3) + positif(PENSALP3)
 + positif(REMPLAP3) + positif(CARPEP3) + positif(CARTSP4) + positif(PENSALP4)
 + positif(REMPLAP4) + positif(CARPEP4)
 + positif(COD1CD) + positif(COD1CV) + positif(COD1DD) + positif(COD1DV) + positif(COD1ED)
 + positif(COD1EV) + positif(COD1FD) + positif(COD1FV) + positif(COD1PD) + positif(COD1PE)
 + positif(COD1PF) + positif(COD1PG)
 + positif(COD5CC) + positif(COD5CE) + positif(COD5CG) + positif(COD5CT) + positif(COD5CX) 
 + 0 ;

regle 951070:
application : iliad ;

SOMMEA031 = positif(TSHALLOC + ALLOC + PRBRC + PALIC + GSALC + TSASSUC + XETRANC  
                    + EXOCETC + FRNC + PCAPTAXC + CARTSC + PENSALC + REMPLAC + CARPEC  
                    + GLDGRATC + BPCOSAC + BAFORESTC + BAFPVC + BAF1AC + BAEXC 
		    + BACREC + 4BACREC + BA1AC + BACDEC + BAHEXC + BAHREC + 4BAHREC 
                    + BAHDEC + BAPERPC + BANOCGAC + COD5XU + COD5XW + AUTOBICVC 
		    + AUTOBICPC + AUTOBNCC + COD5UJ + COD5UK + COD5UL 
		    + MIBEXC + MIBVENC + MIBPRESC + MIBPVC  
                    + MIB1AC + MIBDEC + BICPMVCTC + BICEXC + BICNOC + BI1AC + BICDNC  
                    + BIHEXC + BIHNOC + BIHDNC + MIBNPEXC + MIBNPVENC + MIBNPPRESC  
		    + MIBNPPVC + MIBNP1AC + MIBNPDEC + BICNPEXC + BICREC + LOCNPCGAC 
		    + BI2AC + BICDEC + LOCDEFNPCGAC + MIBMEUC + MIBGITEC + LOCGITC 
		    + LOCGITCC + LOCGITHCC + BICNPHEXC + BICHREC + LOCNPC + BICHDEC 
                    + LOCDEFNPC + BNCPROEXC + BNCPROC + BNCPROPVC + BNCPRO1AC + BNCPRODEC 
		    + BNCPMVCTC + BNCEXC + BNCREC + BN1AC + BNCDEC + BNHEXC + BNHREC 
		    + BNHDEC + BNCCRC + CESSASSC + XSPENPC + BNCNPC + BNCNPPVC + BNCNP1AC 
		    + BNCNPDEC + BNCNPREXAAC + BNCAABC + BNCAADC + BNCNPREXC + ANOVEP
                    + PVINCE + INVENTC + DNOCEPC+ BNCCRFC 
                    + RCSC + PVSOCC   + PEBFC  + PERPC + PERP_COTC + PLAF_PERPC 
                    + PERPPLAFCC + PERPPLAFNUC1 + PERPPLAFNUC2 + PERPPLAFNUC3
                    + CODDBJ + CODEBJ + SALEXTC + COD1BE + COD1BH + RDSYCJ + PENINC + CODRBZ
                    + COD1BF + COD1BL + COD1BM + COD1OX + COD1UP + COD5BD + COD5BI + COD5BK 
                    + COD5BL + COD5BN + COD5EB + COD5EF + COD5EG + COD5EK + COD5EL + COD5EM 
                    + COD5EN + COD5FY + COD5FZ + COD5LD + COD5RZ + COD5VP + COD5VR + COD5VS 
                    + COD5VT + COD5VU + COD5VY + COD5VZ + COD5WM + COD5WN + COD5YA + COD5YB  
                    + COD5YI + COD5YJ + COD5YN + COD5YO + COD5YP + COD5YQ + COD5YR + COD5YS 
                    + COD5YX + COD5YY + COD5YZ + COD5OW + CODCLC + CODCLI + CODCOC + CODCOI 
                    + CODCNS + CODCRC + CODCRF + CODCRI + COD1QM + COD5AI + COD5BO 
                    + COD5BP + COD5BQ + COD5BR + COD5BY + COD5YH + COD5YK + COD5YL + COD8WM 
                    + CODZRA + CODZRB + CODZRE + COD1HA + COD1HB + CODRBF + CODRBG + CODRBL 
                    + CODRBM + CODRBI + COD1BG + COD5DD + COD1BA + COD1HE  
                    + GLDGRATC + COD1HG + COD1HH + COD1HL + COD1GL + COD1BI + COD1HF + CODRBF
		    + COD5YT + COD5YU + COD8SI
		    + COD5EI + COD5EQ + COD5IA + COD5RA + COD5RJ + COD5UF + COD5VI
		    + COD5XE + COD5XF
		    + COD6NT + COD6OT + COD8EB + IPSUISC + COD8IV + COD8IW + COD8IX + COD8IY 
		    + COD8IZ + COD8LH + COD8LK + COD8AB + COD8AD + COD8AF + COD8AH + COD8AJ
		    + COD8AL + COD8AN + COD8AP + COD8AR + COD8AT + COD8AV + COD8AX + COD8AZ
		    + COD8BB + COD8BD + COD8RQ + REVMAR2 + INDJNONIMPC + COD8PC
		    + CODDBJ + CODEBJ + COD8ZQ
		    + CODSAB + CODSAD + CODSAF + CODSAH + CODSAJ + CODSAL + CODSAN + CODSAP
		    + CODSAR + CODSAT + CODSAV + CODSAX + CODSAZ + CODSDB + CODSDC + CODSDD
		    + CODSDE + CODSDF + CODSDG + CODSDH + CODSDI + CODSDJ + CODSDK + CODSDL
		    + CODSDM + CODSDN + CODSDO + COD7ZR + COD7ZT + COD7ZV
		    + COD1BD + COD1BV + COD1PC
		    + COD5BC + COD5BE + COD5BG + COD5BT + COD5BX + COD5CI
		    + COD8RC + COD8RF + COD8RM + COD8RV 
                    + 0 ) ;

regle 951080:
application : iliad ;  

SOMMEA804 = SOMMEANOEXP ;

SOMMEA805 = SOMMEANOEXP + positif(CODDAJ) + positif(CODEAJ) + positif(CODDBJ) + positif(CODEBJ) 
            + positif(CARTSV) + positif(CARTSNBAV) + positif(CARTSC) + positif(CARTSNBAC) ;

regle 951090:
application : iliad ;  

SOMMEA859 = present( BICEXV ) + present( BICNOV ) + present( BI1AV ) + present( BICDNV )
            + present( BICEXC ) + present( BICNOC ) + present( BI1AC ) + present( BICDNC )
	    + present( BICEXP ) + present( BICNOP ) + present( BI1AP ) + present( BICDNP )
	    + present( BIHEXV ) + present( BIHNOV ) + present( BIHDNV )
	    + present( BIHEXC ) + present( BIHNOC ) + present( BIHDNC )
	    + present( BIHEXP ) + present( BIHNOP ) + present( BIHDNP )
	    + present( COD5UI ) + present( COD5VI ) + present( COD5WI )
	    + present( CODCKC ) + present( CODCKI ) + present( CODCLC ) 
	    + present( CODCLI ) + present( CODCMC ) + present( CODCMI )
	    ;

SOMMEA895 = present(BAEXV) + present(BACREV) + present(4BACREV) + present(BA1AV) + present(BACDEV)
            + present(BAEXC) + present(BACREC) + present(4BACREC) + present(BA1AC) + present(BACDEC)
	    + present(BAEXP) + present(BACREP) + present(4BACREP) + present(BA1AP) + present(BACDEP)
	    + present(BAHEXV) + present(BAHREV) + present(4BAHREV) + present(BAHDEV)
	    + present(BAHEXC) + present(BAHREC) + present(4BAHREC) + present(BAHDEC)
	    + present(BAHEXP) + present(BAHREP) + present(4BAHREP) + present(BAHDEP)
	    + (1 - null(V_FORVA+0)) + present(BAFPVV) + present(BAF1AV)
            + (1 - null(V_FORCA+0)) + present(BAFPVC) + present(BAF1AC)
            + (1 - null(V_FORPA+0)) + present(BAFPVP) + present(BAF1AP) 
            + present(COD5XT) + present(COD5XV) + present(COD5XU) + present(COD5XW)
	    + present(COD5XB) + present(COD5YB) + present(COD5ZB)
	    + present(COD5XN) + present(COD5YN) + present(COD5ZN)
	    + present(COD5XA) + present(COD5YA) + present(COD5ZA)
	    + present(BAFORESTV) + present(BAFORESTC) + present(BAFORESTP)
	    + present(COD5XO) + present(COD5YO) + present(COD5ZO)
	    + present(COD5XN) + present(COD5YN) + present(COD5ZN)
	    + present(COD5HA) + present(COD5IA) + present(COD5JA)
	    ;

SOMMEA891 = SOMMEA895 ; 

SOMMEA892 = SOMMEA895 ;

SOMMEA898 = SOMMEA895 ;

SOMMEA881 =  
	     present(BAEXV) + present(BACREV) + present(4BACREV) + present(BA1AV) + present(BACDEV)
           + present(BAEXC) + present(BACREC) + present(4BACREC) + present(BA1AC) + present(BACDEC)
	   + present(BAEXP) + present(BACREP) + present(4BACREP) + present(BA1AP) + present(BACDEP)
	   + present(BAHEXV) + present(BAHREV) + present(4BAHREV) + present(BAHDEV)
	   + present(BAHEXC) + present(BAHREC) + present(4BAHREC) + present(BAHDEC)
	   + present(BAHEXP) + present(BAHREP) + present(4BAHREP) + present(BAHDEP)
           + present(COD5XT) + present(COD5XV) + present(COD5XU) + present(COD5XW)
	   + present(COD5HA) + present(COD5IA) + present(COD5JA)
	   + present( BICEXV ) + present( BICNOV ) + present( BI1AV ) + present( BICDNV )
	   + present( BICEXC ) + present( BICNOC ) + present( BI1AC )
	   + present( BICDNC ) + present( BICEXP ) + present( BICNOP ) 
	   + present( BI1AP ) + present( BICDNP ) + present( BIHEXV ) + present( BIHNOV )
	   + present( BIHDNV ) + present( BIHEXC )
	   + present( BIHNOC ) + present( BIHDNC ) 
	   + present( BIHEXP ) + present( BIHNOP ) + present( BIHDNP)
	   + present(COD5UI ) + present(COD5VI) + present(COD5WI)
           + present(CODCKC) + present(CODCKI) + present(CODCLC)
	   + present(CODCLI) + present(CODCMC) + present(CODCMI)
	   + present( BICNPEXV ) + present( BICREV ) + present( BI2AV )
	   + present( BICDEV ) + present( BICNPEXC ) + present( BICREC ) 
	   + present( BI2AC ) + present( BICDEC ) + present( BICNPEXP ) + present( BICREP )
	   + present( BI2AP ) + present( BICDEP ) + present( BICNPHEXV )
	   + present( BICHREV ) + present( BICHDEV ) 
	   + present( BICNPHEXC ) + present( BICHREC ) + present( BICHDEC )
	   + present( BICNPHEXP ) + present( BICHREP ) 
	   + present( BICHDEP ) 
	   + present( LOCNPCGAV ) + present( LOCGITCV ) + present( LOCDEFNPCGAV ) 
	   + present( LOCNPCGAC ) + present( LOCGITCC ) + present( LOCDEFNPCGAC ) 
	   + present( LOCNPCGAPAC ) + present( LOCGITCP ) + present( LOCDEFNPCGAPAC )
	   + present( LOCNPV ) + present( LOCGITHCV ) + present( LOCDEFNPV ) 
	   + present( LOCNPC ) + present( LOCGITHCC ) + present( LOCDEFNPC ) 
	   + present( LOCNPPAC ) + present( LOCGITHCP ) + present( LOCDEFNPPAC )
           + present( BAPERPV ) + present( BAPERPC ) + present( BAPERPP)
           + present( BANOCGAV ) + present( BANOCGAC ) + present( BANOCGAP)
	   + present(COD5TF) + present(COD5UF) + present(COD5VF) + present(COD5WE) 
	   + present(COD5WF) + present(COD5XE) + present(COD5XF) + present(COD5YE)
	   + present(COD5YF) + present(CODCNC) + present(CODCNI) + present(CODCOC)
	   + present(CODCOI) + present(CODCPC) + present(CODCPI)

	   + present(BNCEXV) + present(BNCREV) + present(BN1AV) + present(BNCDEV) 
	   + present(BNCEXC) + present(BNCREC) + present(BN1AC) + present(BNCDEC)
	   + present(BNCEXP) + present(BNCREP) + present(BN1AP) + present(BNCDEP) 
	   + present(BNHEXV) + present(BNHREV) + present(BNHDEV) 
	   + present(BNHEXC) + present(BNHREC) + present(BNHDEC) 
	   + present(BNHEXP) + present(BNHREP) + present(BNHDEP) 
           + present(COD5QA) + present(COD5RA) + present(COD5SA)
	   + present(CODCQC) + present(CODCQI) + present(CODCRC)
	   + present(CODCRI) + present(CODCSC) + present(CODCSI)

	   + present(BNCAABV) + present(ANOCEP) + present(INVENTV) 
	   + present(PVINVE) + present(BNCAADV) + present(DNOCEP) 
	   + present(BNCAABC) + present(ANOVEP) + present(INVENTC) 
	   + present(PVINCE) + present(BNCAADC) + present(DNOCEPC)
	   + present(BNCAABP) + present(ANOPEP) + present(INVENTP)
	   + present(PVINPE) + present(BNCAADP) + present(DNOCEPP)
           + present(BNCNPREXAAV) + present(BNCNPREXAAC) + present(BNCNPREXAAP)
           + present(BNCNPREXV) + present(BNCNPREXC) + present(BNCNPREXP)
	   + present(COD5QJ) + present(COD5RJ) + present(COD5SJ) 
	   + present(CODCJG) + present(CODCRF) + present(CODCSF)
	   + present(CODCSN) + present(CODCNS) + present(CODCOS)
	   ;

SOMMEA858 = SOMMEA881 + present(TSHALLOV) + present(TSHALLOC) + present(TSASSUV) + present(TSASSUC)
                      + present(RFMIC) + present(RFORDI) + present(RFDORD) + present(RFDHIS) 
		      + present(COD1GG) + present(COD1HG) + present(COD1IG) + present(COD1JG)
		      + present(COD1KG) + present(COD1LG) ;

SOMMEA893 = SOMMEA881 + present(COD1GB) + present(COD1HB) +  present(COD1IB) +  present(COD1JB) ;

SOMMEA890 = SOMMEA893 ;

SOMMEA896 = SOMMEA881 ;
		
SOMMEA874 =     present( COD1AF ) +  present( COD1BF ) +  present( COD1CF ) +
		present( COD1DF ) +  present( COD1EF ) +  present( COD1FF ) + 
                present( COD1TZ ) + present ( COD1TP ) + present ( COD1NX ) +  
	        present( COD1PM ) + present ( COD1UP ) + present ( COD1OX ) + 
	        present(COD1QM) + present (GLDGRATV) + present (GLDGRATC) + 
		present(CODRAF) + present(CODRBF) + present(CODRCF) +
                present(CODRDF) + present(CODREF) + present(CODRFF) +		
		
		present( COD1AL ) +  present( COD1BL ) +  present( COD1CL ) +  present( COD1DL ) +
		present( COD1EL ) +  present( COD1FL ) +  present (CODRAL) + present(CODRBL) + 
		present(CODRCL) + present(CODRDL) + present(CODREL) + present(CODRFL) +

		present( COD1AR ) +  present( COD1BR ) +  present( COD1CR ) +  present( COD1DR ) +
		present (CODRAR) + present (CODRBR) + present (CODRCR) + present (CODRDR) + 
		
                present(BPCOSAV) + present(BPCOSAC) +  present(BPVSJ) + present(BPVSK) +
                present(BPV18V) + present(BPV40V) + present(BPCOPTV) +  present(COD3TJ) +

                present( COD5XJ ) +  present( COD5XK ) +  present( COD5YJ ) + present( COD5YK ) + 
		present( COD5ZJ ) +  present( COD5ZK ) + present(BNCPROV) + present(BNCPROC) + 
		present(BNCPROP) +
		
		present( COD5XS ) +  present( COD5XX ) + present( COD5YS ) +
		present( COD5YX ) +  present( COD5ZS ) +  present( COD5ZX ) + present(BNCNPV) + 
		present(BNCNPC) + present(BNCNPP)
	
	;

SOMMEA877 =  present(BAEXV) + present(BACREV) + present(4BACREV) 
	   + present(BA1AV) + present(BACDEV) + present(BAEXC) 
	   + present(BACREC) + present(4BACREC)
	   + present(BA1AC) + present(BACDEC) + present(BAEXP) + present(BACREP) 
	   + present(4BACREP) + present(BA1AP) 
	   + present(BACDEP) + present(BAHEXV) + present(BAHREV)  
	   + present(4BAHREV) + present(BAHDEV) + present(BAHEXC) 
	   + present(BAHREC) + present(4BAHREC)
	   + present(BAHDEC) + present(BAHEXP) + present(BAHREP)  
	   + present(4BAHREP) + present(BAHDEP) + present(BICEXV) 
           + present(COD5XT) + present(COD5XV) + present(COD5XU) + present(COD5XW)
           + present(BAPERPV) + present(BANOCGAV) + present(BAPERPC) + present(BANOCGAC)
	   + present(BAPERPP) + present(BANOCGAP) + present(COD5HA) + present(COD5IA) + present(COD5JA)

	   + present(BICNOV) + present(BI1AV) + present(BICDNV) 
           + present(BICEXC) + present(BICNOC)  
	   + present(BI1AC) + present(BICDNC) + present(BICEXP) 
           + present(BICNOP) + present(BI1AP) + present(BICDNP) 
           + present(BIHEXV) + present(BIHNOV) + present(BIHDNV) 
           + present(BIHEXC) + present(BIHNOC) + present(BIHDNC) 
           + present(BIHEXP) + present(BIHNOP) + present(BIHDNP) 
	   + present(COD5UI) + present(COD5VI) + present(COD5WI)
	   + present(CODCKC) + present(CODCKI) + present(CODCLC)
	   + present(CODCLI) + present(CODCMC) + present(CODCMI) 
	   
	   + present(BICNPEXV) + present(BICREV) + present(LOCNPCGAV)
	   + present(LOCGITCV) + present(BI2AV) + present(BICDEV)
	   + present(LOCDEFNPCGAV) + present(BICNPEXC) + present(BICREC)
	   + present(LOCNPCGAC) + present(LOCGITCC) + present(BI2AC)
	   + present(BICDEC) + present(LOCDEFNPCGAC) + present(BICNPEXP)
	   + present(BICREP) + present(LOCNPCGAPAC) + present(LOCGITCP)
	   + present(BI2AP) + present(BICDEP) + present(LOCDEFNPCGAPAC)
	   + present(BICNPHEXV) + present(BICHREV) + present(LOCNPV)
	   + present(LOCGITHCV) + present(BICHDEV) + present(LOCDEFNPV)
	   + present(BICNPHEXC) + present(BICHREC) + present(LOCNPC)
	   + present(LOCGITHCC) + present(BICHDEC) + present(LOCDEFNPC)
	   + present(BICNPHEXP) + present(BICHREP) + present(LOCNPPAC)
	   + present(LOCGITHCP) + present(BICHDEP) + present(LOCDEFNPPAC)
	   + present(COD5TF) + present(COD5UF) + present(COD5VF) + present(COD5WE)
	   + present(COD5WF) + present(COD5XE) + present(COD5XF) + present(COD5YE)
	   + present(COD5YF) + present(CODCNC) + present(CODCNI) + present(CODCOC)
	   + present(CODCOI) + present(CODCPC) + present(CODCPI)
	   ;

SOMMEA879 =  
	     present(BACREV) + present(4BACREV) + present(BA1AV) + present(BACDEV) 
	   + present(BACREC) + present(4BACREC) + present(BA1AC) + present(BACDEC) 
           + present(BACREP) + present(4BACREP) + present(BA1AP) + present(BACDEP) 
	   + present(BAHREV) + present(4BAHREV) + present(BAHDEV) 
	   + present(BAHREC) + present(4BAHREC) + present(BAHDEC) 
           + present(BAHREP) + present(4BAHREP) + present(BAHDEP) 
           + present(COD5XT) + present(COD5XV) + present(COD5XU) + present(COD5XW)
           + present(BAPERPV) + present(BANOCGAV) + present(BAPERPC)
	   + present(BANOCGAC) + present(BAPERPP) + present(BANOCGAP)
	   + present(COD5HA) + present(COD5IA) + present(COD5JA)
	   
	   + present( BICNOV ) + present( BI1AV ) 
	   + present( BICDNV ) + present( BICNOC )  
	   + present( BI1AC ) + present( BICDNC ) + present( BICNOP ) 
	   + present( BI1AP ) + present( BICDNP )  
	   + present( BIHNOV ) + present( BIHDNV )  
	   + present( BIHNOC ) + present( BIHDNC )  
	   + present( BIHNOP ) + present( BIHDNP )
           + present( COD5UI ) + present( COD5VI )
           + present( COD5WI ) + present( CODCKC )
	   + present( CODCKI ) + present( CODCLC )
	   + present( CODCLI ) + present( CODCMC )
	   + present( CODCMI ) 

	   + present( BICREV ) + present( BI2AV ) + present( BICDEV ) 
	   + present( BICREC ) + present( BI2AC ) 
	   + present( BICDEC ) + present( BICREP )  
	   + present( BI2AP ) + present( BICDEP ) + present( BICHREV ) 
	   + present( BICHDEV ) + present( BICHREC ) 
	   + present( BICHDEC ) + present( BICHREP ) 
	   + present( BICHDEP ) 
	   + present( LOCNPCGAV ) + present( LOCGITCV ) + present( LOCDEFNPCGAV ) 
	   + present( LOCNPCGAC ) + present( LOCGITCC ) + present( LOCDEFNPCGAC ) 
	   + present( LOCNPCGAPAC ) + present( LOCGITCP ) + present( LOCDEFNPCGAPAC )
	   + present( LOCNPV ) + present( LOCGITHCV ) + present( LOCDEFNPV ) 
	   + present( LOCNPC ) + present( LOCGITHCC ) + present( LOCDEFNPC ) 
	   + present( LOCNPPAC ) + present( LOCGITHCP ) + present( LOCDEFNPPAC )
	   + present (COD5TF) + present (COD5UF) + present (COD5VF)
	   + present (COD5WE) + present (COD5WF) + present (COD5XE)
	   + present (COD5XF) + present (COD5YE) + present (COD5YF)
	   + present (CODCNC) + present (CODCNI) + present (CODCOC)
	   + present (CODCOI) + present (CODCPC) + present (CODCPI)
	   + present(BNCREV) + present(BN1AV) + present(BNCDEV) 
	   + present(BNCREC) + present(BN1AC) + present(BNCDEC) 
	   + present(BNCREP) + present(BN1AP) + present(BNCDEP) 
	   + present(BNHREV) + present(BNHDEV) 
	   + present(BNHREC) + present(BNHDEC) 
	   + present(BNHREP) + present(BNHDEP)
           + present(COD5QA) + present(COD5RA) + present(COD5SA)
           + present(CODCQC) + present(CODCQI) + present(CODCRC)
	   + present(CODCRI) + present(CODCSC) + present(CODCSI)

	   + present(BNCAABV) + present(ANOCEP) + present(INVENTV) 
	   + present(PVINVE) + present(BNCAADV) + present(DNOCEP) 
	   + present(BNCAABC) + present(ANOVEP) + present(INVENTC) 
	   + present(PVINCE) + present(BNCAADC) + present(DNOCEPC)
	   + present(BNCAABP) + present(ANOPEP) + present(INVENTP)
	   + present(PVINPE) + present(BNCAADP) + present(DNOCEPP)
	   + present(COD5QJ) + present(COD5RJ) + present(COD5SJ)
	   + present(CODCJG) + present(CODCRF) + present(CODCSF)
	   + present(CODCSN) + present(CODCNS) + present(CODCOS)
	   ; 

SOMMEA880 = 
           present(BAEXV) + present(BACREV) + present(4BACREV) + present(BA1AV)
         + present(BACDEV) + present(BAEXC) + present(BACREC) + present(4BACREC)
         + present(BA1AC) + present(BACDEC) + present(BAEXP) + present(BACREP)
	 + present(4BACREP) + present(BA1AP) + present(BACDEP) + present(BAHEXV)
	 + present(BAHREV) + present(4BAHREV) + present(BAHDEV) + present(BAHEXC)
	 + present(BAHREC) + present(4BAHREC) + present(BAHDEC) + present(BAHEXP)
	 + present(BAHREP) + present(4BAHREP) + present(BAHDEP) + present(BAPERPV)
	 + present(BANOCGAV) + present(BAPERPC) + present(BANOCGAC) + present(BAPERPP)
	 + present(BANOCGAP) + present(COD5XT) + present(COD5XV) + present(COD5XU)
	 + present(COD5XW) + present(COD5HA) + present(COD5IA) + present(COD5JA)
         + present(BICEXV) + present(BICNOV) + present(BI1AV) + present(BICDNV)
         + present(BICEXC) + present(BICNOC) + present(BI1AC) + present(BICDNC)
	 + present(BICEXP) + present(BICNOP) + present(BI1AP) + present(BICDNP)
	 + present(BIHEXV) + present(BIHNOV) + present(BIHDNV) + present(BIHEXC)
	 + present(BIHNOC) + present(BIHDNC) + present(BIHEXP) + present(BIHNOP)
	 + present(BIHDNP) + present(COD5UI) + present(COD5VI) + present(COD5WI)
	 + present(CODCKC) + present(CODCKI) + present(CODCLC) + present(CODCLI)
	 + present(CODCMC) + present(CODCMI) 
        + present(BICNPEXV) + present(BICREV) + present(LOCNPCGAV) + present(LOCGITCV)
	+ present(BI2AV) + present(BICDEV) + present(LOCDEFNPCGAV) + present(BICNPEXC)
	+ present(BICREC) + present(LOCNPCGAC) + present(LOCGITCC) + present(BI2AC)
	+ present(BICDEC) + present(LOCDEFNPCGAC) + present(BICNPEXP) + present(BICREP)
	+ present(LOCNPCGAPAC) + present(LOCGITCP) + present(BI2AP) + present(BICDEP)
	+ present(LOCDEFNPCGAPAC) + present(BICNPHEXV) + present(BICHREV) + present(LOCNPV)
	+ present(LOCGITHCV) + present(BICHDEV) + present(LOCDEFNPV) + present(BICNPHEXC)
	+ present(BICHREC) + present(LOCNPC) + present(LOCGITHCC) + present(BICHDEC)
	+ present(LOCDEFNPC) + present(BICNPHEXP) + present(BICHREP) + present(LOCNPPAC)
	+ present(LOCGITHCP) + present(BICHDEP) + present(LOCDEFNPPAC) + present(COD5TF)
	+ present(COD5UF) + present(COD5VF) + present(COD5WE) + present(COD5WF)
	+ present(COD5XE) + present(COD5XF) + present(COD5YE) + present(COD5YF)
	+ present(CODCNC) + present(CODCNI) + present(CODCOC) + present(CODCOI)
        + present(CODCPC) + present(CODCPI)
        + present(BNCEXV) + present(BNCREV) + present(BN1AV) + present(BNCDEV)	 
        + present(BNCEXC) + present(BNCREC) + present(BN1AC) + present(BNCDEC)
	+ present(BNCEXP) + present(BNCREP) + present(BN1AP) + present(BNCDEP)
	+ present(BNHEXV) + present(BNHREV) + present(BNHDEV) + present(BNHEXC)
	+ present(BNHREC) + present(BNHDEC) + present(BNHEXP) + present(BNHREP)
	+ present(BNHDEP) + present(COD5QA) + present(COD5RA) + present(COD5SA)
	+ present(CODCQC) + present(CODCQI) + present(CODCRC) + present(CODCRI)
	+ present(CODCSC) + present(CODCSI) 
        + present(BNCNPREXAAV) + present(BNCNPREXV) + present(BNCNPREXAAC) + present(BNCNPREXC)
        + present(BNCNPREXAAP) + present(BNCNPREXP) + present(BNCAABV) + present(ANOCEP)
	+ present(INVENTV) + present(PVINVE) + present(BNCAADV) + present(DNOCEP)
	+ present(BNCAABC) + present(ANOVEP) + present(INVENTC) + present(PVINCE)
	+ present(BNCAADC) + present(DNOCEPC) + present(BNCAABP) + present(ANOPEP)
	+ present(INVENTP) + present(PVINPE) + present(BNCAADP) + present(DNOCEPP)
	+ present(COD5QJ) + present(COD5RJ) + present(COD5SJ) + present(CODCJG)
	+ present(CODCRF) + present(CODCSF) + present(CODCSN) + present(CODCNS)
	+ present(CODCOS) ;



SOMMEA884 = present(PCAPTAXV) + present(PCAPTAXC) + present(COD1CT) + present(COD1DT) + present(COD1ET) + present(COD1FT)
           + present(CARTSV) + present(CARTSC) + present(CARTSP1)
           + present(CARTSP2) + present(CARTSP3) + present(CARTSP4)
           + present(CARTSNBAV) + present(CARTSNBAC) + present(CARTSNBAP1)
           + present(CARTSNBAP2) + present(CARTSNBAP3) + present(CARTSNBAP4)
           + present(REMPLAV) + present(REMPLAC) + present(REMPLAP1)
           + present(REMPLAP2) + present(REMPLAP3) + present(REMPLAP4)
           + present(REMPLANBV) + present(REMPLANBC) + present(REMPLANBP1)
           + present(REMPLANBP2) + present(REMPLANBP3) + present(REMPLANBP4)
           + present(CARPEV) + present(CARPEC) + present(CARPEP1)
           + present(CARPEP2) + present(CARPEP3) + present(CARPEP4)
           + present(CARPENBAV) + present(CARPENBAC) + present(CARPENBAP1)
           + present(CARPENBAP2) + present(CARPENBAP3) + present(CARPENBAP4)
           + present(PENSALV) + present(PENSALC) + present(PENSALP1)
           + present(PENSALP2) + present(PENSALP3) + present(PENSALP4)
           + present(PENSALNBV) + present(PENSALNBC) + present(PENSALNBP1)
           + present(PENSALNBP2) + present(PENSALNBP3) + present(PENSALNBP4)
	   + present(REVACT) + present(DISQUO) + present(REVACTNB) + present(DISQUONB) 
	   + present(RCMHAD) + present(RCMABD) + present(RCMHAB)  
           + present(CODRAZ) + present(CODRBZ) + present(CODRCZ) + present(CODRDZ) + present(CODREZ) + present(CODRFZ)
	   + present(COD1AF) + present(COD1BF) + present(COD1CF)
      	   + present(COD1DF) + present(COD1EF) + present(COD1FF)
      	   + present(COD1AL) + present(COD1BL) + present(COD1CL)
      	   + present(COD1DL) + present(COD1EL) + present(COD1FL)
	   + present(CODRAL) + present(CODRCL) 
	   + present(CODRDL) + present(CODREL) + present(CODRFL)
	   + present(CODRAF) + present(CODRBF) + present(CODRCF) + present (CODRDF)
	   + present(CODREF) + present(CODRFF) + present(GLDGRATV) + present(COD1TZ)
	   + present(COD1AR) + present(COD1BR) + present(COD1CR)
   	   + present(COD1DR) + present(CODRAR) + present(CODRBR)
           + present(CODRCR) + present(CODRDR)
	   + present(REAMOR) + present (CODRBT) + present(CODRBK)   
	   + present(COD4BK) + present(COD4BL)	
           + present(BPVRCM) + present(PVTAXSB)
           + present(CODRVG)
	   + present(4BACREV) + present(4BAHREV)  
	   + present(4BACREC) + present(4BAHREC)  
	   + present(4BACREP) + present(4BAHREP)  
           + present(COD5XT)  + present(COD5XV) + present(COD5XU) + present(COD5XW)
	   + present( COD5AK ) + present( COD5AL ) + present( COD5BK )
	   + present( COD5BL ) + present( COD5CK ) + present( COD5CL )

	   + present( COD5DF )
	   + present( COD5DG ) + present( COD5EF ) + present( COD5EG ) + present( COD5FF ) + present( COD5FG )
 	   + present( COD5UR )+ present( COD5US ) + present( COD5VR )
	   + present( COD5VS )+ present (COD5WR) + present (COD5WS) 
	   + present( COD5EY ) + present (COD5EZ ) 
	   + present( COD5FY ) + present( COD5FZ )
	   + present( COD5GY ) + present( COD5GZ )
	   + present( COD5XJ ) + present( COD5XK ) + present( COD5YJ ) + present( COD5YK ) + present( COD5ZJ ) + present( COD5ZK )
   	   + present( COD5XS ) + present( COD5XX ) + present( COD5YS ) + present( COD5YX ) + present( COD5ZS ) + present( COD5ZX )
	   ;

SOMMEA538VB =  present( BAFORESTV ) + present( BAFPVV ) + present( BACREV ) + present( 4BACREV ) 
	     + present( BAHREV ) + present( 4BAHREV ) + present( MIBVENV ) + present( MIBPRESV ) 
	     + present( MIBPVV ) + present( BICNOV ) + present( BIHNOV ) 
	     + present( MIBNPVENV ) + present( MIBNPPRESV ) + present( MIBNPPVV ) 
	     + present( BICREV ) + present( BICHREV ) 
             + present( BNCPROV ) + present( BNCPROPVV ) + present( BNCREV ) + present( BNHREV ) 
	     + present( BNCNPV ) + present( BNCNPPVV ) + present( ANOCEP ) + present( BNCAABV ) 
	     + present ( COD5XB) + present(COD5HA) + present(COD5UI) + present(COD5TF) 
	     + present(COD5QA) + present(CODCJG) + present(CODCKC) + present(CODCKI) 
	     + present(CODCNC) + present(CODCNI) + present(CODCQC) + present(CODCQI)
	     + present(COD5XT) + present(COD5XV) + present(CODCSN) ;
	     

SOMMEA538CB =  present( BAFORESTC ) + present( BAFPVC ) + present( BACREC ) + present( 4BACREC ) 
	     + present( BAHREC ) + present( 4BAHREC ) + present( MIBVENC ) + present( MIBPRESC )
             + present( MIBPVC ) + present( BICNOC ) + present( BIHNOC )
             + present( MIBNPVENC ) + present( MIBNPPRESC ) + present( MIBNPPVC )
             + present( BICREC ) + present( BICHREC ) 
             + present( BNCPROC ) + present( BNCPROPVC ) + present( BNCREC ) + present( BNHREC )
	     + present( BNCNPC ) + present( BNCNPPVC ) + present( ANOVEP ) + present( BNCAABC ) 
	     + present ( COD5YB) + present(COD5IA) + present(COD5VI) + present(COD5UF)
	     + present(COD5RA) + present(CODCLC) + present(CODCLI) + present(CODCNS)
	     + present(CODCOC) + present(CODCOI) + present(CODCRC) + present(CODCRF) + present(CODCRI)
	     + present(COD5XU) + present(COD5XW);
				                    
SOMMEA538PB =  present( BAFORESTP ) + present( BAFPVP ) + present( BACREP ) + present( 4BACREP ) 
	     + present( BAHREP ) + present( 4BAHREP ) + present( MIBVENP ) + present( MIBPRESP )
             + present( MIBPVP ) + present( BICNOP ) + present( BIHNOP )
	     + present( MIBNPVENP ) + present( MIBNPPRESP ) + present( MIBNPPVP )
             + present( BICREP ) + present( BICHREP ) 
	     + present( BNCPROP ) + present( BNCPROPVP ) + present( BNCREP ) + present( BNHREP )
	     + present( BNCNPP ) + present( BNCNPPVP ) + present( ANOPEP ) + present( BNCAABP ) 
	     + present ( COD5ZB ) + present(COD5JA) + present(COD5WI) + present(COD5VF)
                  + present(COD5SA) + present(CODCMC) + present(CODCMI) + present(CODCOS)
                  + present(CODCPC) + present(CODCPI) + present(CODCSC) + present(CODCSF) + present(CODCSI);

SOMMEA538VP =  present ( BAF1AV ) + present ( BA1AV ) + present ( MIB1AV ) + present ( BI1AV )
             + present ( MIBNP1AV ) + present ( BI2AV ) + present ( BNCPRO1AV ) + present ( BN1AV )
	     + present ( BNCNP1AV ) + present ( PVINVE ) ;


SOMMEA538CP =  present ( BAF1AC ) + present ( BA1AC ) + present ( MIB1AC ) + present ( BI1AC )
             + present ( MIBNP1AC ) + present ( BI2AC ) + present ( BNCPRO1AC ) + present ( BN1AC )
	     + present ( BNCNP1AC ) + present ( PVINCE ) ;

SOMMEA538PP =  present ( BAF1AP ) + present ( BA1AP ) + present ( MIB1AP ) + present ( BI1AP )
             + present ( MIBNP1AP ) + present ( BI2AP ) + present ( BNCPRO1AP ) + present ( BN1AP )
	     + present ( BNCNP1AP ) + present ( PVINPE ) ;

SOMMEA538 = SOMMEA538VB + SOMMEA538CB + SOMMEA538PB + SOMMEA538VP + SOMMEA538CP + SOMMEA538PP ;

SOMMEA862 =  

      present( MIBEXV ) + present( MIBVENV ) + present( MIBPRESV ) 
    + present( MIBPVV ) + present( MIB1AV ) + present( MIBDEV ) + present( BICPMVCTV )
    + present( MIBEXC ) + present( MIBVENC ) + present( MIBPRESC ) 
    + present( MIBPVC ) + present( MIB1AC ) + present( MIBDEC ) + present( BICPMVCTC ) 
    + present( MIBEXP ) + present( MIBVENP ) + present( MIBPRESP ) 
    + present( MIBPVP ) + present( MIB1AP ) + present( MIBDEP ) + present( BICPMVCTP ) 
    + present( BICEXV ) + present( BICNOV ) 
    + present( BI1AV ) + present( BICDNV ) 
    + present( BICEXC ) + present( BICNOC ) 
    + present( BI1AC ) + present( BICDNC ) 
    + present( BICEXP ) + present( BICNOP ) 
    + present( BI1AP ) + present( BICDNP ) 
    + present( BIHEXV ) + present( BIHNOV ) + present( BIHDNV )
    + present( BIHEXC ) + present( BIHNOC ) + present( BIHDNC )
    + present( BIHEXP ) + present( BIHNOP ) + present( BIHDNP )

    + present( MIBMEUV ) + present( MIBGITEV ) + present( LOCGITV ) + present( MIBNPEXV ) + present( MIBNPVENV ) 
    + present( MIBNPPRESV ) + present( MIBNPPVV ) + present( MIBNP1AV ) + present( MIBNPDEV ) 
    + present( MIBMEUC ) + present( MIBGITEC ) + present( LOCGITC ) + present( MIBNPEXC ) + present( MIBNPVENC ) 
    + present( MIBNPPRESC ) + present( MIBNPPVC ) + present( MIBNP1AC ) + present( MIBNPDEC ) 
    + present( MIBMEUP ) + present( MIBGITEP ) + present( LOCGITP ) + present( MIBNPEXP ) + present( MIBNPVENP ) 
    + present( MIBNPPRESP ) + present( MIBNPPVP ) + present( MIBNP1AP ) + present( MIBNPDEP ) 
    + present( MIBNPDCT )
    + present( BICNPEXV ) + present( BICREV ) + present( LOCNPCGAV ) + present( LOCGITCV )
    + present( BI2AV ) + present( BICDEV ) + present( LOCDEFNPCGAV )
    + present( BICNPEXC ) + present( BICREC ) + present( LOCNPCGAC ) + present( LOCGITCC )
    + present( BI2AC ) + present( BICDEC ) + present( LOCDEFNPCGAC )
    + present( BICNPEXP ) + present( BICREP ) + present( LOCNPCGAPAC ) + present( LOCGITCP )
    + present( BI2AP ) + present( BICDEP ) + present( LOCDEFNPCGAPAC )
    + present( BICNPHEXV ) + present( BICHREV ) + present( LOCNPV )
    + present( LOCGITHCV ) + present( BICHDEV ) + present( LOCDEFNPV ) 
    + present( BICNPHEXC ) + present( BICHREC ) + present( LOCNPC ) 
    + present( LOCGITHCC ) + present( BICHDEC ) + present( LOCDEFNPC ) 
    + present( BICNPHEXP ) + present( BICHREP ) + present( LOCNPPAC ) 
    + present( LOCGITHCP ) + present( BICHDEP ) + present( LOCDEFNPPAC )
    + present( COD5RZ ) + present( COD5SZ ) 

    + present( BNCPROEXV ) + present( BNCPROV ) + present( BNCPROPVV ) 
    + present( BNCPRO1AV ) + present( BNCPRODEV ) + present( BNCPMVCTV )
    + present( BNCPROEXC ) + present( BNCPROC ) + present( BNCPROPVC ) 
    + present( BNCPRO1AC ) + present( BNCPRODEC ) + present( BNCPMVCTC )
    + present( BNCPROEXP ) + present( BNCPROP ) + present( BNCPROPVP ) 
    + present( BNCPRO1AP ) + present( BNCPRODEP ) + present( BNCPMVCTP )
    + present( BNCPMVCTV ) 
    + present( BNCEXV ) + present( BNCREV ) + present( BN1AV ) + present( BNCDEV ) 
    + present( BNCEXC ) + present( BNCREC ) + present( BN1AC ) + present( BNCDEC )
    + present( BNCEXP ) + present( BNCREP ) + present( BN1AP ) + present( BNCDEP ) 
    + present( BNHEXV ) + present( BNHREV ) + present( BNHDEV ) 
    + present( BNHEXC ) + present( BNHREC ) + present( BNHDEC ) 
    + present( BNHEXP ) + present( BNHREP ) + present( BNHDEP ) 

    + present( XSPENPV ) + present( BNCNPV ) + present( BNCNPPVV ) + present( BNCNP1AV ) + present( BNCNPDEV )
    + present( XSPENPC ) + present( BNCNPC ) + present( BNCNPPVC ) + present( BNCNP1AC ) + present( BNCNPDEC ) 
    + present( XSPENPP ) + present( BNCNPP ) + present( BNCNPPVP ) + present( BNCNP1AP ) + present( BNCNPDEP ) 
    + present( BNCNPDCT ) 
    + present( BNCNPREXAAV ) + present( BNCAABV ) + present( BNCAADV ) + present( BNCNPREXV )
    + present( ANOCEP ) + present( DNOCEP ) + present( PVINVE ) + present( INVENTV )
    + present( BNCNPREXAAC ) + present( BNCAABC ) + present( BNCAADC ) + present( BNCNPREXC ) 
    + present( ANOVEP ) + present( DNOCEPC ) + present( PVINCE ) + present( INVENTC )
    + present( BNCNPREXAAP ) + present( BNCAABP ) + present( BNCAADP ) + present( BNCNPREXP ) 
    + present( ANOPEP ) + present( DNOCEPP ) + present( PVINPE ) + present( INVENTP )
    + present( COD5LD) + present( COD5MD)   
    +present(COD5NW) + present(COD5OW) + present(COD5PW)
    ;

SOMMEA802 = present(AUTOBICVV) + present(AUTOBICPV) + present(AUTOBICVC) 
            + present(AUTOBICPC) + present(AUTOBICVP) + present(AUTOBICPP) 
	    + present(AUTOBNCV) + present(AUTOBNCC) + present(AUTOBNCP) 
            + present(SALEXTV) + present(SALEXTC) + present(SALEXT1) + present(SALEXT2) + present(SALEXT3) + present(SALEXT4)
            + present(COD1AE) + present(COD1BE) + present(COD1CE) + present(COD1DE) + present(COD1EE) + present(COD1FE)
            + present(COD1AH) + present(COD1BH) + present(COD1CH) + present(COD1DH) + present(COD1EH) + present(COD1FH)
            ;

SOMMEA878 = present(COD4BK) + present(COD4BL) + present(CODRBT) + present(CODRBK) + present(CODRBL) ;  

regle 951100:
application : iliad ;  


SOMMEANOEXP = positif(PEBFV + COTFV + PEBFC + COTFC + PEBF1 + COTF1 + PEBF2 + COTF2 
                      + PEBF3 + COTF3 + PEBF4 + COTF4 + PENSALV + PENSALNBV + PENSALC + PENSALNBC 
                      + PENSALP1 + PENSALNBP1 + PENSALP2 + PENSALNBP2 + PENSALP3 + PENSALNBP3 + PENSALP4 + PENSALNBP4
                      + CARPEV + CARPENBAV + CARPEC + CARPENBAC + CARPEP1 + CARPENBAP1 + CARPEP2 + CARPENBAP2 
                      + CARPEP3 + CARPENBAP3 + CARPEP4 + CARPENBAP4 + CARTSP1 + CARTSNBAP1 + CARTSP2 + CARTSNBAP2 
		      + CARTSP3 + CARTSNBAP3 + CARTSP4 + CARTSNBAP4 + REMPLAV + REMPLANBV + REMPLAC + REMPLANBC 
                      + REMPLAP1 + REMPLANBP1 + REMPLAP2 + REMPLANBP2 + REMPLAP3 + REMPLANBP3 + REMPLAP4 + REMPLANBP4 
                      + RENTAX + RENTAX5 + RENTAX6 + RENTAX7 + RENTAXNB + RENTAXNB5 + RENTAXNB6 + RENTAXNB7
                      + FONCI + FONCINB + REAMOR + REAMORNB + CODRBT + CODRBE + CODNBE + CODRBK + REVACT 
		      + REVACTNB + REVPEA + REVPEANB + PROVIE + PROVIENB + DISQUO + DISQUONB  + RESTUC
                      + RESTUCNB + INTERE + INTERENB + 4BACREV + 4BAHREV + 4BACREC + 4BAHREC 
                      + 4BACREP + 4BAHREP 
                      + CODRAZ + CODRBZ + CODRCZ + CODRDZ + CODREZ + CODRFZ + CODNAZ + CODNBZ + CODNCZ 
	              + CODNDZ + CODNEZ + CODNFZ + CODRVG + CODNVG + CODRUA + CODNUA + CODRSG + CODRSL 
	              + CODRVA + CODRAF + CODNAF + CODRBF + CODNBF + CODRCF + CODNCF + CODRDF + CODNDF 
	              + CODREF + CODNEF + CODRFF + CODNFF + CODRAG + CODNAG + CODRBG + CODNBG + CODRCG 
	              + CODNCG + CODRDG + CODNDG + CODRGG + CODNGG + CODRFG + CODNFG + CODRAL + CODNAL 
	              + CODRBL + CODNBL + CODRCL + CODNCL + CODRDL + CODNDL + CODREL + CODNEL + CODRFL 
	              + CODNFL + CODRAM + CODNAM + CODRBM + CODNBM + CODRCM + CODNCM + CODRDM + CODNDM 
	              + CODREM + CODNEM + CODRFM + CODNFM + CODRAR + CODNAR + CODRBR + CODNBR + CODRCR 
	              + CODNCR + CODRDR + CODNDR + CODCKC + CODCKI + CODCLC + CODCLI + CODCMC + CODCMI 
	              + CODCNC + CODCNI + CODCOC + CODCOI + CODCPC + CODCPI + CODCNS + CODCOS + CODCQC 
	              + CODCQI + CODCRC + CODCRI + CODCSC + CODCSF + CODCSI + CODCSN + 4BACREP+ CODCRF
		      + CODRAI + CODNFM + CODRBI + CODNBI + CODRCK + CODNCK + CODCJG
                      + 0) ;

regle 951130:
application : iliad ;  

SOMMEA700 = 
          (
   present(BAEXV) + present(BACREV) + present(4BACREV) + present(BA1AV) + present(BACDEV) 
 + present(BAEXC) + present(BACREC) + present(4BACREC) + present(BA1AC) + present(BACDEC) 
 + present(BAEXP) + present(BACREP) + present(4BACREP) + present(BA1AP) + present(BACDEP) 
 + present(BAPERPV) + present(BAPERPC) + present(BAPERPP)
 + present(COD5XT) + present(COD5XU) 

 + present(BICEXV) + present(BICNOV) 
 + present(BI1AV) + present(BICDNV) 
 + present(BICEXC) + present(BICNOC) 
 + present(BI1AC) + present(BICDNC) 
 + present(BICEXP) + present(BICNOP) 
 + present(BI1AP) + present(BICDNP)
 + present(CODCKC) + present(CODCLC) +  present(CODCMC)

 + present(BICNPEXV) + present(BICREV) + present(LOCNPCGAV) + present(LOCGITCV)
 + present(BI2AV) + present(BICDEV) + present(LOCDEFNPCGAV) + present(COD5WE)
 + present(BICNPEXC) + present(BICREC) + present(LOCNPCGAC) + present(LOCGITCC)
 + present(BI2AC) + present(BICDEC) + present(LOCDEFNPCGAC) + present(COD5XE)
 + present(BICNPEXP) + present(BICREP) + present(LOCNPCGAPAC) + present(LOCGITCP)
 + present(BI2AP) + present(BICDEP) + present(LOCDEFNPCGAPAC) + present(COD5YE)
 + present(CODCNC) + present(CODCOC) + present(CODCPC)

 + present(BNCEXV) + present(BNCREV) + present(BN1AV) + present(BNCDEV)
 + present(BNCEXC) + present(BNCREC) + present(BN1AC) + present(BNCDEC)
 + present(BNCEXP) + present(BNCREP) + present(BN1AP) + present(BNCDEP)
 + present(CODCQC) + present(CODCRC) + present(CODCSC)

 + present(BNCNPREXAAV) + present(BNCNPREXAAC)
 + present(BNCNPREXAAP) 
 + present(BNCAABV) + present(BNCAADV)  
 + present(PVINVE) + present(INVENTV)
 + present(BNCAABC) + present(BNCAADC)  
 + present(PVINCE) + present(INVENTC)
 + present(BNCAABP) + present(BNCAADP) 
 + present(PVINPE) + present(INVENTP)
 + present(CODCJG) + present(CODCRF)
 + present(CODCSF)
          ) ;

regle 951135:
application : iliad ;

SOMMEA9201 = present(CODSAA) + present(CODSAC) + present(CODSAE) ;

SOMMEA9202 = present(CODSAB) + present(CODSAD) + present(CODSAF) ;

regle 951140:
application : iliad ;  

V_CNR  =   (V_REGCO+0) dans (2);
V_CNR2 =   (V_REGCO+0) dans (2,3);
V_EAD  =   (V_REGCO+0) dans (5);
V_EAG  =   (V_REGCO+0) dans (6);
regle 951145:
application : iliad ;  

ANNEEREV = V_ANREV + V_MODUL;
regle 951150:
application : iliad ;  

VARIPTEFN =  (IPTEFN*(1-FLAG_PVRO)+(COD3WG-IPTEFN)*positif(COD3WG-IPTEFN)*positif(IPTEFN)*FLAG_PVRO) * (1-positif(SOMMEMOND_2+PREM8_11));
VARIPTEFP = (IPTEFP + (COD3WG*FLAG_PVRO*positif(IPTEFP))+ max(0,DEFZU*positif(SOMMEMOND_2)*(1-PREM8_11)+DEFZU*PREM8_11 - IPTEFN )) * positif(present(IPTEFP)+present(IPTEFN));

VARDMOND = DMOND* (1-positif(SOMMEMOND_2+PREM8_11));

VARRMOND = (RMOND + max(0,DEFZU*positif(SOMMEMOND_2)*(1-PREM8_11)+DEFZU*PREM8_11 - DMOND)) * positif(present(RMOND)+present(DMOND));

regle 951160:
application : iliad ;  

FLAGRETARD = FLAG_RETARD + 0 ;
FLAGRETARD08 = FLAG_RETARD08 + 0 ;
FLAGDEFAUT = FLAG_DEFAUT + 0 ;
FLAGDEFAUT10 = FLAG_DEFAUT10 + 0 ;
FLAGDEFAUT11 = FLAG_DEFAUT11 + 0 ;

regle 951170:
application : iliad ;  


INDCODDAJ = positif(present(CODDAJ) + present(CODDBJ) + present(CODEAJ) + present(CODEBJ)) ;

regle 951180:
application : iliad ;  


DEFRI = positif(RIDEFRI + DEFRITS + DEFRIBA + DEFRIBIC + DEFRILOC + 
                DEFRIBNC + DEFRIRCM + DEFRIRF + DEFRIGLOB + DEFRIMOND + 0) ;

DEFRIMAJ = positif(DEFRIMAJ_DEF + DEFRI) ;
TOTSTR = positif(STR_TR03 + STR_TR04 + STR_TR05 + STR_TR06 + STR_TR07 + STR_TR08 + STR_TR09 + STR_TR10 + STR_TR11 + STR_TR12 + STR_TR13 + STR_TR14+0);
regle 951190:
application : iliad ;  

SOMMEBAINF = positif(null(SOMMEBA_2+0) + (1-positif(SHBA - SEUIL_IMPDEFBA))) ;
SOMMEBASUP = positif(SOMMEBA_2 * positif(SHBA - SEUIL_IMPDEFBA)) ;
SOMMEBA = SOMMEBA_2 ;
SOMMEBIC = SOMMEBIC_2 ;
SOMMELOC = SOMMELOC_2 ;
SOMMEBNC = SOMMEBNC_2 ;
SOMMERF = SOMMERF_2 ;
SOMMERCM = SOMMERCM_2 ;

regle 951195:
application : iliad ;
SOMBICDF = BICDNV +BICDNC +BICDNP +BIHDNV +BIHDNC +BIHDNP +BICDEV +BICDEC +BICDEP +BICHDEV +BICHDEC +BICHDEP +BICPMVCTV
            +BICPMVCTC +BICPMVCTP +MIBNPDCT +COD5RZ +COD5SZ +MIBDEV +MIBDEC +MIBDEP +MIBNPDEV +MIBNPDEC +MIBNPDEP
            +COD5WE +COD5WF +COD5XE +COD5XF +COD5YE +COD5YF +MIBNPDCT +COD5RZ +COD5SZ;
SOMBADF = BAHDEV +BAHDEC +BAHDEP +BACDEV +BACDEC +BACDEP +COD5XO +COD5YO +COD5ZO +COD5XN +COD5YN +COD5ZN;
SOMLOCDF = LOCDEFNPCGAV+ LOCDEFNPV+ LOCDEFNPCGAC+ LOCDEFNPC+ LOCDEFNPCGAPAC+ LOCDEFNPPAC+ LOCDEFNPV + LOCDEFNPC + LOCDEFNPPAC;
SOMBNCDF = BNHDEV + BNCDEV +BNHDEC + BNCDEC +BNHDEP + BNCDEP +BNCAADV +BNCAADC +BNCAADP +DNOCEP +DNOCEPC +DNOCEPP +BNCPMVCTV
        +BNCPMVCTC +BNCPMVCTP +BNCNPDCT +COD5LD +COD5MD +BNCPRODEV +BNCPRODEC +BNCPRODEP +BNCNPDEV +BNCNPDEC +BNCNPDEP;
regle 951200:
application : iliad ;

SOMDEFTS =
   FRNV * positif (FRNV - 10MINSV)
 + FRNC * positif (FRNC - 10MINSC)
 + FRN1 * positif (FRN1 - 10MINS1)
 + FRN2 * positif (FRN2 - 10MINS2)
 + FRN3 * positif (FRN3 - 10MINS3)
 + FRN4 * positif (FRN4 - 10MINS4);
SOMDEFBIC =
     BICNOV
   + arr(BIHNOV * MAJREV)
   + BICNOC
   + arr(BIHNOC * MAJREV)
   + BICNOP
   + arr(BIHNOP * MAJREV )
   - BIPN
   +BICPMVCTV +BICPMVCTC +BICPMVCTP;
SOMDEFBNC =
     BNCREV
    + arr(BNHREV * MAJREV)
    + BNCREC
    + arr(BNHREC * MAJREV)
    + BNCREP
    + arr(BNHREP * MAJREV)
    - BNRTOT
+BNCPMVCTV +BNCPMVCTC +BNCPMVCTP;
SOMDEFANT =
   DEFAA5
 + DEFAA4
 + DEFAA3
 + DEFAA2
 + DEFAA1
 + DEFAA0;
SOMDEFICIT =SOMDEFANT+SOMDEFBNC
                          +SOMDEFBANI * (1-positif(SHBA-SEUIL_IMPDEFBA))
                          +SOMDEFTS+SOMDEFBIC+RFDHIS;
SOMDEFICITHTS =SOMDEFANT+SOMDEFBNC
                          +SOMDEFBANI * (1-positif(SHBA-SEUIL_IMPDEFBA))
                          +SOMDEFBIC+RFDHIS;

regle 951210:
application : iliad ;  

DEFRITS = positif(
                positif(max(FRNV,10MINSV)-max(FRDV,10MINSV)) 
              + positif(max(FRNC,10MINSC)-max(FRDC,10MINSC)) 
              + positif(max(FRN1,10MINS1)-max(FRD1,10MINS1)) 
              + positif(max(FRN2,10MINS2)-max(FRD2,10MINS2)) 
              + positif(max(FRN3,10MINS3)-max(FRD3,10MINS3)) 
              + positif(max(FRN4,10MINS4)-max(FRD4,10MINS4))) ;
DEFRIBA =  positif(DEFBANIF)+0;
DEFRIBIC = positif(DEFBICNPF)+0;
DEFRILOC = positif(DEFLOCNPF)+0;
DEFRIBNC = positif(DEFBNCNPF)+0;
DEFRIRCM = positif(DEFRCMIMPU)+0;
DEFRIRF =  positif(DEFRFNONI)+0;
DEFRIGLOB = positif(DFANTIMPU)+0;
DEFRIMOND = positif(positif(TEFFP_2-TEFFP_1)+positif(TEFFN_2*(-1)-TEFFN_1*(-1)) +positif(RMOND_2-RMOND_1)+positif(DMOND_2*(-1)-DMOND_1*(-1)))+0;

regle 951230:
application : iliad ;  

REGCIAUTO = null(IPBOCH - (COD1AL + COD1BL +COD1CL +COD1DL +COD1EL +COD1FL +COD1AF +COD1BF +COD1CF +COD1DF +COD1EF +COD1FF + COD4BK+COD4BL ))
		      * positif_ou_nul(TSN1AF+TSN1BF+TSN1CF+TSN1DF+TSN1EF+TSN1FF)
                      * max(0,PRN1AL+PRN1BL+PRN1CL+PRN1DL+PRN1EL+PRN1FL
		      +TSN1AF*present(COD1AF)+TSN1BF*present(COD1BF)+TSN1CF*present(COD1CF)+TSN1DF*present(COD1DF)+TSN1EF*present(COD1EF)+TSN1FF*present(COD1FF)+COD4BL+(COD4BK*TX70/100))
	   + (1-null(IPBOCH - (COD1AL + COD1BL +COD1CL +COD1DL +COD1EL +COD1FL +COD1AF +COD1BF +COD1CF +COD1DF +COD1EF +COD1FF+ COD4BK+COD4BL) )
                      * positif_ou_nul(TSN1AF*present(COD1AF)+TSN1BF*present(COD1BF)+TSN1CF*present(COD1CF)+TSN1DF*present(COD1DF)+TSN1EF*present(COD1EF)+TSN1FF*present(COD1FF)))
	                * REGCI + 0	      ;
8SGAUTO = null(IPBOCH - (COD1AL + COD1BL +COD1CL +COD1DL +COD1EL +COD1FL +COD1AF +COD1BF +COD1CF +COD1DF +COD1EF +COD1FF + COD4BK+COD4BL )) * positif(COD4BL + COD4BK) * (COD4BL + COD4BK * TX70/100)
          + (1- null(IPBOCH - (COD1AL + COD1BL +COD1CL +COD1DL +COD1EL +COD1FL +COD1AF +COD1BF +COD1CF +COD1DF +COD1EF +COD1FF + COD4BK+COD4BL )))
	      * COD8SG + 0;

IND8XRAUTO = null(IPBOCH - (COD1AL + COD1BL +COD1CL +COD1DL +COD1EL +COD1FL +COD1AF +COD1BF +COD1CF +COD1DF +COD1EF +COD1FF + COD4BK+COD4BL))
                      * positif_ou_nul(TSN1AF*present(COD1AF)+TSN1BF*present(COD1BF)+TSN1CF*present(COD1CF)+TSN1DF*present(COD1DF)+TSN1EF*present(COD1EF)+TSN1FF*present(COD1FF));
regle 951240:
application : iliad ;


SOMMEM210 = COD7AB + RDSYVO + COD7AD + RDSYCJ + COD7AF + RDSYPP + COD7AH + COD7AI + COD7AP + COD7AR + COD7AS + COD7AT + COD7AU + COD7AU + PINELBI + COD7BK + COD7BL
          + COD7BM + COD7BN + COD7BO + COD7BS + COD7BT + COD7CA + RVCURE + RCCURE + COD7CH + COD7CI + COD7CR + COD7CS + COD7CT + COD7CV + COD7CX + COD7CY + PINELCZ
	  + CREAIDE + COD7DC + PINELDI + COD7DY + COD7EK + COD7EN + COD7EY + PINELEZ + RDCOM + NBACT + RSOCREPRISE + DUFLOFK + FIPDOMCOM + FIPCORSE + CINE2 + FFIP
	  + DUFLOFR + COD7FT + DUFLOFV + COD7FW + COD7FX + COD7FY + RDGARD1 + RDGARD2 + RDGARD3 + RDGARD4 + CODFGD + RDGARD1QAR + RDGARD2QAR + RDGARD3QAR + RDGARD4QAR + CODFHD
	  + CINE1 + FCPI + COD7GR + COD7GS + COD7GU + COD7GW + COD7GX + COD7GY + RDRESU + COD7HA + COD7HD + COD7HE + COD7HF + COD7HG + COD7HH + COD7HJ
	  + COD7HK + COD7HN + COD7HO + COD7HP + COD7HQ + COD7HR + COD7HS + COD7HT + COD7HU + COD7HV + COD7HW + COD7HX + COD7HY + COD7HZ + COD7JA + COD7JB
	  + COD7JC + COD7JD + COD7JM + COD7JN + COD7JO + COD7JP + COD7JQ + COD7JR + COD7JS + COD7JT + COD7JU + COD7JV + COD7JW + COD7JX + COD7JY + COD7KC + COD7KD
	  + COD7KE + COD7KF + COD7KG + COD7KH + COD7KI + COD7KM + COD7KT + COD7KU + COD7KV + COD7KW + COD7KX + COD7KZ + COD7LA + COD7LB + COD7LC + COD7LG
	  + COD7LH + COD7LI + COD7LJ + COD7LM + CELRREDLQ + CELRREDLR + CELRREDLU + CELRREDLV + COD7LY + COD7MA + COD7MB + COD7MC + COD7MD + COD7MI+ COD7MJ + COD7MK
	  + COD7ML + COD7MM + COD7MO + COD7MP + COD7MQ + COD7MR + COD7MS + COD7MT + COD7MU + COD7MV + COD7MX + COD7MY + RIRENOV + COD7OF + COD7OG
	  + COD7OH + COD7OI + COD7OJ + COD7OK + COD7OL + COD7OM + COD7ON + COD7OO + COD7OP+ COD7OQ + COD7OR + COD7OS+ COD7OT + COD7PA + COD7PB + COD7PC
	  + COD7PD + COD7PE + COD7PI + COD7PJ + COD7PP + COD7PQ + COD7PR + COD7PS + COD7PT + COD7PU + COD7PV + COD7PW + COD7PX + COD7PY + COD7QE + COD7RA
	  + COD7RB + COD7RC + COD7RD + COD7RE + COD7RF + COD7RG + COD7RH + COD7RI + COD7RJ + COD7RK + COD7RL + COD7RM + COD7RN + COD7RT + COD7RU + PINELRZ + COD7SA
	  + COD7SB + COD7SC + COD7SM + COD7SN + COD7SO + COD7SP + COD7SS + COD7TA + COD7TB + COD7TE + COD7TF + REPSINFOR5 + COD7TK + COD7TM + COD7TO + COD7TP
	  + COD7TQ + COD7TR + COD7TS + COD7TT + COD7TU + COD7TV + COD7TW + COD7TX + COD7TY + PINELTZ + COD7UA + COD7UB + FORET + RDREP + RDDOUP + COD7UG + COD7UH
	  + COD7UI + COD7UJ + COTFORET + INTDIFAGRI + RDFOREST + RDFORESTRA + RDFORESTGES + RDMECENAT + SINISFORET + COD7UU + COD7UV + COD7UW + COD7UX + COD7UY + COD7UZ + DONETRAN
	  + DONAUTRE + COD7VH + COD7VI + COD7VJ + COD7VK + COD7VM + COD7VN + COD7VQ + COD7VR + COD7WI + RDEQPAHA + RDTECH + SUBSTITRENTE + PRESCOMP2000 + PRESCOMPJUGE
	  + RDPRESREPORT + RISKTEC + CELREPWT + CELREPWU + CELREPWV + CELREPWW + COD7WX + COD7WY + COD7WZ + COD7XA + COD7XB + COD7XC + COD7XL + COD7XM + COD7XN
	  + COD7XO + COD7XP + COD7XQ + REPDON03 + REPDON04 + REPDON05 + REPDON06 + REPDON07 + COD7YA + COD7YC + COD7YG + COD7YI + COD7YJ + COD7YK + COD7YL + CELREPYM
	  + CELREPYN + CELREPYO + CELREPYP + COD7YR + COD7YS + CELREPYT + CELREPYU + CELREPYV + CELREPYW + COD7ZI + COD7ZJ + COD7ZK + COD7ZL + COD7ZM + COD7ZO + COD7ZP
	  + COD7ZQ + COD7ZR + COD7ZS + COD7ZT
	  ;


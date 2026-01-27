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
regle 881000:
application : iliad  ;

GLDOMAVDAJV = max (CODDAJ - ABDOMDAJ,0) * INDEFTSV;
GLDOMAVEAJV = max (CODEAJ - ABDOMEAJ,0) * INDEFTSV;
GLDOMAVDBJC = max (CODDBJ - ABDOMDBJ,0) * INDEFTSC;
GLDOMAVEBJC = max (CODEBJ - ABDOMEBJ,0) * INDEFTSC;
GLN4DAJV = (max(CODDAJ - ABDOMDAJ,0) * INDEFTSV);
GLN4V = (max(CODDAJ - ABDOMDAJ,0)+max(CODEAJ - ABDOMEAJ,0)) * INDEFTSV;
GLN4DBJC = (max(CODDBJ - ABDOMDBJ,0) * INDEFTSC);
GLN4C = (max(CODDBJ - ABDOMDBJ,0)+max(CODEBJ - ABDOMEBJ,0)) * INDEFTSC;

regle 881010:
application : iliad  ;

TSV = TSNV -max(CODDAJ - ABDOMDAJ,0)-max(CODEAJ - ABDOMEAJ,0);
TSC = TSNC -max(CODDBJ - ABDOMDBJ,0)-max(CODEBJ - ABDOMEBJ,0);
TS1 = TSN1;
TS2 = TSN2;
TS3 = TSN3;
TS4 = TSN4;
TPRV = TSNV + PRNV - GLN3V;
TPRC = TSNC + PRNC - GLN3C;
TPR1 = TSN1 + PRN1;
TPR2 = TSN2 + PRN2;
TPR3 = TSN3 + PRN3;
TPR4 = TSN4 + PRN4;
TSNNV =  positif(TSV) *arr(TSV *(TSBNV + BPCOSAV + GLDGRATV)/EXTSV )
          + (1 -positif(TSV)) * TSV ;
TSNNC =  positif(TSC) *arr(TSC *(TSBNC + BPCOSAC + GLDGRATC)/EXTSC )
          + (1 -positif(TSC)) * TSC ;
TSNN1 = (positif(TS1) * arr(TS1 * TSBN1 /EXTS1 )
            + (1 -positif(TS1)) * TS1)  ;
TSNN2 = (positif(TS2) * arr(TS2 * TSBN2 /EXTS2 )
            + (1 -positif(TS2)) * TS2)  ;
TSNN3 = (positif(TS3) * arr(TS3 * TSBN3 /EXTS3 )
            + (1 -positif(TS3)) * TS3)  ;
TSNN4 = (positif(TS4) * arr(TS4 * TSBN4 /EXTS4 )
            + (1 -positif(TS4)) * TS4)  ;
TSNN2V =  positif(TSV) * (
                  positif(CARTSV+REMPLAV+CODRAF+CODRAG)
                          * arr(TSV * 2TSNV / EXTSV )
                    + (1 -positif(CARTSV+REMPLAV+CODRAF+CODRAG))
                          * (TSV - TSNNV));
TSNN2C =  positif(TSC)
                * ( positif(CARTSC+REMPLAC+CODRBF+CODRBG)
                          * arr(TSC * 2TSNC / EXTSC )
                    + (1 -positif(CARTSC+REMPLAC+CODRBF+CODRBG))
                          * (TSC - TSNNC));
TSNN2VAFF =  positif(TSV)
                *  (positif(CARTSV+REMPLAV+CODRAF+CODRAG)
                          * arr(TSV * 2TSNV / EXTSV )
                    + (1 -positif(CARTSV+REMPLAV+CODRAF+CODRAG))
                          * (TSV - TSNNV))
         + positif(CODDAJ + CODEAJ) * (max(CODDAJ - ABDOMDAJ,0)+max(CODEAJ - ABDOMEAJ,0));
TSNN2CAFF =  positif(TSC)
                *  (positif(CARTSC+REMPLAC+CODRBF+CODRBG)
                          * arr(TSC * 2TSNC / EXTSC )
                    + (1 -positif(CARTSC+REMPLAC+CODRBF+CODRBG))
                          * (TSC - TSNNC))
         + positif(CODDBJ + CODEBJ) * (max(CODDBJ - ABDOMDBJ,0)+max(CODEBJ - ABDOMEBJ,0));
TSNN21 =  positif(TS1)
               * ( positif(CARTSP1+REMPLAP1+CODRCF+CODRCG)
                          * arr(TS1 * 2TSN1 /EXTS1 )
                    + (1 -positif(CARTSP1+REMPLAP1+CODRCF+CODRCG))
                          * (TS1 - TSNN1));
TSNN22 =  positif(TS2)
               * ( positif(CARTSP2+REMPLAP2+CODRDF+CODRDG)
                          * arr(TS2 * 2TSN2 /EXTS2 )
                    + (1 -positif(CARTSP2+REMPLAP2+CODRDF+CODRDG))
                          * (TS2 - TSNN2));
TSNN23 =  positif(TS3)
               * ( positif(CARTSP3+REMPLAP3+CODREF+CODRGG)
                          * arr(TS3 * 2TSN3 /EXTS3 )
                    + (1 -positif(CARTSP3+REMPLAP3+CODREF+CODRGG))
                          * (TS3 - TSNN3));
TSNN24 =  positif(TS4)
               * ( positif(CARTSP4+REMPLAP4+CODRFF+CODRFG)
                          * arr(TS4 * 2TSN4 /EXTS4 )
                    + (1 -positif(CARTSP4+REMPLAP4+CODRFF+CODRFG))
                          * (TS4 - TSNN4));
TSNN21AFF =  positif(TS1)
               *  (positif(CARTSP1+REMPLAP1+CODRCF+CODRCG)
                          * arr(TS1 * 2TSN1 /EXTS1 )
                    + (1 -positif(CARTSP1+REMPLAP1+CODRCF+CODRCG))
                          * (TS1 - TSNN1));
TSNN22AFF =  positif(TS2)
               *  (positif(CARTSP2+REMPLAP2+CODRDF+CODRDG)
                          * arr(TS2 * 2TSN2 /EXTS2 )
                    + (1 -positif(CARTSP2+REMPLAP2+CODRDF+CODRDG))
                          * (TS2 - TSNN2));
TSNN23AFF =  positif(TS3)
               *  (positif(CARTSP3+REMPLAP3+CODREF+CODRGG) * arr(TS3 * 2TSN3 /EXTS3 )
                    + (1 -positif(CARTSP3+REMPLAP3+CODREF+CODRGG)) * (TS3 - TSNN3));
TSNN24AFF =  positif(TS4)
               *  (positif(CARTSP4+REMPLAP4+CODRFF+CODRFG) * arr(TS4 * 2TSN4 /EXTS4 )
                    + (1 -positif(CARTSP4+REMPLAP4+CODRFF+CODRFG)) * (TS4 - TSNN4));

TSNN2PAFF = somme(i=1..4:TSNN2iAFF) ;
TSNN2TSV =  positif(TSV) * ( positif(REMPLAV+CODRAF+CODRAG) * arr(TSV * CARTSV / EXTSV )
                    + (1 -positif(REMPLAV+CODRAF+CODRAG)) * (TSV - TSNNV)) ;
TSNN2TSC =  positif(TSC) * ( positif(REMPLAC+CODRBF+CODRBG) * arr(TSC * CARTSC / EXTSC )
                    + (1 -positif(REMPLAC+CODRBF+CODRBG)) * (TSC - TSNNC)) ;
TSNN2TS1 =  positif(TS1) * ( positif(REMPLAP1+CODRCF+CODRCG) * arr(TS1 * CARTSP1 /EXTS1 )
                    + (1 -positif(REMPLAP1+CODRCF+CODRCG)) * (TS1 - TSNN1)) ;
TSNN2TS2 =  positif(TS2) * ( positif(REMPLAP2+CODRDF+CODRDG) * arr(TS2 * CARTSP2 /EXTS2 )
                    + (1 -positif(REMPLAP2+CODRDF+CODRDG)) * (TS2 - TSNN2)) ;
TSNN2TS3 =  positif(TS3) * ( positif(REMPLAP3+CODREF+CODRGG) * arr(TS3 * CARTSP3 /EXTS3 )
                    + (1 -positif(REMPLAP3+CODREF+CODRGG)) * (TS3 - TSNN3)) ;
TSNN2TS4 =  positif(TS4) * ( positif(REMPLAP4+CODRFF+CODRFG) * arr(TS4 * CARTSP4 /EXTS4 )
                    + (1 -positif(REMPLAP4+CODRFF+CODRFG)) * (TS4 - TSNN4)) ;

TSNN2RAF =  positif(TSV) * ( positif(REMPLAV+CODRAG) * arr(TSV * CODRAF / EXTSV )
                    + (1 -positif(REMPLAV+CODRAG)) * (TSV - TSNNV-TSNN2TSV)) ;
TSNN2RBF =  positif(TSC) * ( positif(REMPLAC+CODRBG) * arr(TSC * CODRBF / EXTSC )
                    + (1 -positif(REMPLAC+CODRBG)) * (TSC - TSNNC-TSNN2TSC)) ;
TSNN2RCF =  positif(TS1) * ( positif(REMPLAP1+CODRCG) * arr(TS1 * CODRCF /EXTS1 )
                    + (1 -positif(REMPLAP1+CODRCG)) * (TS1 - TSNN1-TSNN2TS1)) ;
TSNN2RDF =  positif(TS2) * ( positif(REMPLAP2+CODRDG) * arr(TS2 * CODRDF /EXTS2 )
                    + (1 -positif(REMPLAP2+CODRDG)) * (TS2 - TSNN2-TSNN2TS2)) ;
TSNN2REF =  positif(TS3) * ( positif(REMPLAP3+CODRGG) * arr(TS3 * CODREF /EXTS3 )
                    + (1 -positif(REMPLAP3+CODRGG)) * (TS3 - TSNN3-TSNN2TS3)) ;
TSNN2RFF =  positif(TS4) * ( positif(REMPLAP4+CODRFG) * arr(TS4 * CODRFF /EXTS4 )
                    + (1 -positif(REMPLAP4+CODRFG)) * (TS4 - TSNN4-TSNN2TS4)) ;

TSNN2RAG =  positif(TSV) * ( positif(REMPLAV) * arr(TSV * CODRAG / EXTSV )
                    + (1 -positif(REMPLAV)) * (TSV - TSNNV-TSNN2TSV-TSNN2RAF)) ;
TSNN2RBG =  positif(TSC) * ( positif(REMPLAC) * arr(TSC * CODRBG / EXTSC )
                    + (1 -positif(REMPLAC)) * (TSC - TSNNC-TSNN2TSC-TSNN2RBF)) ;
TSNN2RCG =  positif(TS1) * ( positif(REMPLAP1) * arr(TS1 * CODRCG /EXTS1 )
                    + (1 -positif(REMPLAP1)) * (TS1 - TSNN1-TSNN2TS1-TSNN2RCF)) ;
TSNN2RDG =  positif(TS2) * ( positif(REMPLAP2) * arr(TS2 * CODRDG /EXTS2 )
                    + (1 -positif(REMPLAP2)) * (TS2 - TSNN2-TSNN2TS2-TSNN2RDF)) ;
TSNN2RGG =  positif(TS3) * ( positif(REMPLAP3) * arr(TS3 * CODRGG /EXTS3 )
                    + (1 -positif(REMPLAP3)) * (TS3 - TSNN3-TSNN2TS3-TSNN2REF)) ;
TSNN2RFG =  positif(TS4) * ( positif(REMPLAP4) * arr(TS4 * CODRFG /EXTS4 )
                    + (1 -positif(REMPLAP4)) * (TS4 - TSNN4-TSNN2TS4-TSNN2RFF)) ;
TSNN2REMPV = (positif(TSV) * (TSV - TSNNV-TSNN2TSV-TSNN2RAF-TSNN2RAG)) ;
TSNN2REMPC = (positif(TSC) * (TSC - TSNNC-TSNN2TSC-TSNN2RBF-TSNN2RBG)) ;
TSNN2REMP1 = (positif(TS1) * (TS1 - TSNN1-TSNN2TS1-TSNN2RCF-TSNN2RCG)) ;
TSNN2REMP2 = (positif(TS2) * (TS2 - TSNN2-TSNN2TS2-TSNN2RDF-TSNN2RDG)) ;
TSNN2REMP3 = (positif(TS3) * (TS3 - TSNN3-TSNN2TS3-TSNN2REF-TSNN2RGG)) ;
TSNN2REMP4 = (positif(TS4) * (TS4 - TSNN4-TSNN2TS4-TSNN2RFF-TSNN2RFG)) ;


regle 881020:
application : iliad  ;

PRRV = arr(PRNV * (PRBV) / (EXPRV))+ PRN1AI ;
PRRC = arr(PRNC * (PRBC) / (EXPRC))+ PRN1BI ;
PRR1 = arr(PRN1 * (PRB1) / (EXPR1))+ PRN1CI;
PRR2 = arr(PRN2 * (PRB2) / (EXPR2))+ PRN1DI;
PRR3 = arr(PRN3 * (PRB3) / (EXPR3))+ PRN1EI;
PRR4 = arr(PRN4 * (PRB4) / (EXPR4))+ PRN1FI;
PRR2V = positif(PEBFV+PENSALV+CODRAZ+CODRAL+CODRAM+CODRAI) * arr(PRNV * CARPEV / EXPRV)
           +  (1 -positif(PEBFV+PENSALV+CODRAZ+CODRAL+CODRAM+CODRAI)) * (PRNV+PRN1AI -PRRV);
PRR2C = positif(PEBFC+PENSALC+CODRBZ+CODRBL+CODRBM+CODRBI) * arr(PRNC * CARPEC / EXPRC)
           +  (1 -positif(PEBFC+PENSALC+CODRBZ+CODRBL+CODRBM+CODRBI)) * (PRNC+PRN1BI -PRRC)  ;
PRR21 = positif(PEBF1+PENSALP1+CODRCZ+CODRCL+CODRCM+CODRCK) * arr(PRN1 * CARPEP1 / EXPR1 )
           +  (1 -positif(PEBF1+PENSALP1+CODRCZ+CODRCL+CODRCM+CODRCK)) * (PRN1+PRN1CI -PRR1);
PRR22 = positif(PEBF2+PENSALP2+CODRDZ+CODRDL+CODRDM) * arr(PRN2 * CARPEP2 / EXPR2 )
           +  (1 -positif(PEBF2+PENSALP2+CODRDZ+CODRDL+CODRDM)) * (PRN2+PRN1DI -PRR2);
PRR23 = positif(PEBF3+PENSALP3+CODREZ+CODREL+CODREM) * arr(PRN3 * CARPEP3 / EXPR3 )
           +  (1 -positif(PEBF3+PENSALP3+CODREZ+CODREL+CODREM)) * (PRN3 +PRN1EI-PRR3);
PRR24 = positif(PEBF4+PENSALP4+CODRFZ+CODRFL+CODRFM) * arr(PRN4 * CARPEP4 / EXPR4 )
           +  (1 -positif(PEBF4+PENSALP4+CODRFZ+CODRFL+CODRFM)) * (PRN4 +PRN1FI-PRR4);
PRR2ZV = positif(PEBFV+PENSALV+CODRAL+CODRAM+CODRAI) * arr(PRNV * CODRAZ / EXPRV)
           +  (1 -positif(PEBFV+PENSALV+CODRAL+CODRAM+CODRAI)) * (PRNV+PRN1AI -PRRV-PRR2V);
PRR2ZC = positif(PEBFC+PENSALC+CODRBL+CODRBM+CODRBI) * arr(PRNC * CODRBZ / EXPRC)
           +  (1 -positif(PEBFC+PENSALC+CODRBL+CODRBM+CODRBI)) * (PRNC+PRN1BI -PRRC-PRR2C);
PRR2Z1 = positif(PEBF1+PENSALP1+CODRCL+CODRCM+CODRCK) * arr(PRN1 * CODRCZ / EXPR1 )
           +  (1 -positif(PEBF1+PENSALP1+CODRCL+CODRCM+CODRCK)) * (PRN1 +PRN1CI-PRR1-PRR21);
PRR2Z2 = positif(PEBF2+PENSALP2+CODRDL+CODRDM) * arr(PRN2 * CODRDZ / EXPR2 )
           +  (1 -positif(PEBF2+PENSALP2+CODRDL+CODRDM)) * (PRN2+PRN1DI -PRR2-PRR22);
PRR2Z3 = positif(PEBF3+PENSALP3+CODREL+CODREM) * arr(PRN3 * CODREZ / EXPR3 )
           +  (1 -positif(PEBF3+PENSALP3+CODREL+CODREM)) * (PRN3+PRN1EI -PRR3-PRR23);
PRR2Z4 = positif(PEBF4+PENSALP4+CODRFL+CODRFM) * arr(PRN4 * CODRFZ / EXPR4 )
           +  (1 -positif(PEBF4+PENSALP4+CODRFL+CODRFM)) * (PRN4+PRN1FI -PRR4-PRR24);
PRR2ZP = PRR2Z1 + PRR2Z2 + PRR2Z3 + PRR2Z4;
PENFV =  (positif(PENSALV+CODRAL+CODRAM+CODRAI) * arr(PRNV * PEBFV / EXPRV)
       + (1 - positif(PENSALV+CODRAL+CODRAM+CODRAI)) * max(0,(PRNV+PRN1AI -PRRV -PRR2V-PRR2ZV)));
PENFC =  (positif(PENSALC+CODRBL+CODRBM+CODRBI) * arr(PRNC * PEBFC / EXPRC)
       + (1 - positif(PENSALC+CODRBL+CODRBM+CODRBI)) * max(0,(PRNC+PRN1BI -PRRC -PRR2C-PRR2ZC)));
PENF1 =  (positif(PENSALP1+CODRCL+CODRCM+CODRCK) * arr(PRN1 * PEBF1 / EXPR1)
       + (1 - positif(PENSALP1+CODRCL+CODRCM+CODRCK)) * max(0,(PRN1 +PRN1CI-PRR1 -PRR21-PRR2Z1)));
PENF2 =  (positif(PENSALP2+CODRDL+CODRDM) * arr(PRN2 * PEBF2 / EXPR2)
       + (1 - positif(PENSALP2+CODRDL+CODRDM)) * max(0,(PRN2 +PRN1DI-PRR2 -PRR22-PRR2Z2)));
PENF3 =  (positif(PENSALP3+CODREL+CODREM) * arr(PRN3 * PEBF3 / EXPR3)
       + (1 - positif(PENSALP3+CODREL+CODREM)) * max(0,(PRN3 +PRN1EI-PRR3 -PRR23-PRR2Z3)));
PENF4 =  (positif(PENSALP4+CODRFL+CODRFM) * arr(PRN4 * PEBF4 / EXPR4)
       + (1 - positif(PENSALP4+CODRFL+CODRFM)) * max(0,(PRN4 +PRN1FI-PRR4 -PRR24-PRR2Z4)));
PRR2RAL = positif(PENSALV+CODRAM+CODRAI) * arr(PRNV * CODRAL / EXPRV)
           +  (1 -positif(PENSALV+CODRAM+CODRAI)) * (PRNV +PRN1AI-PRRV-PRR2V-PRR2ZV-PENFV);
PRR2RBL = positif(PENSALC+CODRBM+CODRBI) * arr(PRNC * CODRBL / EXPRC)
           +  (1 -positif(PENSALC+CODRBM+CODRBI)) * (PRNC+PRN1BI -PRRC-PRR2C-PRR2ZC-PENFC);
PRR2RCL = positif(PENSALP1+CODRCM+CODRCK) * arr(PRN1 * CODRCL / EXPR1 )
           +  (1 -positif(PENSALP1+CODRCM+CODRCK)) * (PRN1+PRN1CI -PRR1-PRR21-PRR2Z1-PENF1);
PRR2RDL = positif(PENSALP2+CODRDM) * arr(PRN2 * CODRDL / EXPR2 )
           +  (1 -positif(PENSALP2+CODRDM)) * (PRN2+PRN1DI -PRR2-PRR22-PRR2Z2-PENF2);
PRR2REL = positif(PENSALP3+CODREM) * arr(PRN3 * CODREL / EXPR3 )
           +  (1 -positif(PENSALP3+CODREM)) * (PRN3+PRN1EI -PRR3-PRR23-PRR2Z3-PENF3);
PRR2RFL = positif(PENSALP4+CODRFM) * arr(PRN4 * CODRFL / EXPR4 )
           +  (1 -positif(PENSALP4+CODRFM)) * (PRN4+PRN1FI -PRR4-PRR24-PRR2Z4-PENF4);
PRR2RAM = positif(PENSALV+CODRAI) * arr(PRNV * CODRAM / EXPRV)
           +  (1 -positif(PENSALV+CODRAI)) * (PRNV +PRN1AI-PRRV-PRR2V-PENFV-PRR2ZV-PRR2RAL);
PRR2RBM = positif(PENSALC+CODRBI) * arr(PRNC * CODRBM / EXPRC)
           +  (1 -positif(PENSALC+CODRBI)) * (PRNC +PRN1BI-PRRC-PRR2C-PENFC-PRR2ZC-PRR2RBL);
PRR2RCM = positif(PENSALP1+CODRCK) * arr(PRN1 * CODRCM / EXPR1 )
           +  (1 -positif(PENSALP1+CODRCK)) * (PRN1+PRN1CI -PRR1-PRR21-PENF1-PRR2Z1-PRR2RCL);
PRR2RDM = positif(PENSALP2) * arr(PRN2 * CODRDM / EXPR2 )
           +  (1 -positif(PENSALP2)) * (PRN2 +PRN1DI-PRR2-PRR22-PENF2-PRR2Z2-PRR2RDL);
PRR2REM = positif(PENSALP3) * arr(PRN3 * CODREM / EXPR3 )
           +  (1 -positif(PENSALP3)) * (PRN3+PRN1EI -PRR3-PRR23-PENF3-PRR2Z3-PRR2REL);
PRR2RFM = positif(PENSALP4) * arr(PRN4 * CODRFM / EXPR4 )
           +  (1 -positif(PENSALP4)) * (PRN4+PRN1FI -PRR4-PRR24-PENF4-PRR2Z4-PRR2RFL);
PRR2RAI = positif(CODRAI) * (PRNRAI  * positif (-TSNTV)
                            + CODRAI * positif_ou_nul (TSNTV));
PRR2RBI = positif(CODRBI) * (PRNRBI  * positif (-TSNTC)
                            + CODRBI * positif_ou_nul (TSNTC));
PRR2RCK = positif(CODRCK) * (PRNRCK  * positif (-TSNT1)
                            + CODRCK * positif_ou_nul (TSNT1));
PENALIMV = positif(EXPRV) * (PRNV+PRN1AI -PRRV -PRR2V -PRR2ZV- PENFV-PRR2RAL-PRR2RAM) ;
PENALIMC = positif(EXPRC) * (PRNC+PRN1BI -PRRC -PRR2C -PRR2ZC- PENFC-PRR2RBL-PRR2RBM) ;
PENALIM1 = positif(EXPR1) * (PRN1+PRN1CI -PRR1 -PRR21 -PRR2Z1- PENF1-PRR2RCL-PRR2RCM) ;
PENALIM2 = positif(EXPR2) * (PRN2+PRN1DI -PRR2 -PRR22 -PRR2Z2- PENF2-PRR2RDL-PRR2RDM) ;
PENALIM3 = positif(EXPR3) * (PRN3+PRN1EI -PRR3 -PRR23 -PRR2Z3- PENF3-PRR2REL-PRR2REM) ;
PENALIM4 = positif(EXPR4) * (PRN4+PRN1FI -PRR4 -PRR24 -PRR2Z4- PENF4-PRR2RFL-PRR2RFM) ;

regle 881030:
application : iliad  ;

RV1 = arr(RVB1 * TXRVT1 / 100);
RV2 = arr(RVB2 * TXRVT2 / 100);
RV3 = arr(RVB3 * TXRVT3 / 100);
RV4 = arr(RVB4 * TXRVT4 / 100);
RVTOT = RV1 + RV2 + RV3 + RV4 
	+ arr(COD1AR * TXRVT1 / 100)
	+ arr(COD1BR * TXRVT2 / 100)
	+ arr(COD1CR * TXRVT3 / 100)
	+ arr(COD1DR * TXRVT4 / 100) ;


regle 881040:
application : iliad  ;

2RV1 = arr(RENTAX * TXRVT1 / 100);
2RV2 = arr(RENTAX5 * TXRVT2 / 100);
2RV3 = arr(RENTAX6 * TXRVT3 / 100);
2RV4 = arr(RENTAX7 * TXRVT4 / 100);
2RV1R = arr(CODRAR * TXRVT1 / 100) ;
2RV2R = arr(CODRBR * TXRVT2 / 100) ;
2RV3R = arr(CODRCR * TXRVT3 / 100) ;
2RV4R = arr(CODRDR * TXRVT4 / 100) ;

T2RV = 2RV1 + 2RV2 + 2RV3 + 2RV4+2RV1R + 2RV2R + 2RV3R + 2RV4R; 

regle 881060:
application : iliad  ;

TSPR = TSPRT + RVTOT ;

regle 881070:
application : iliad  ;


TSPRV = (TSNNV + PRRV);
TSPRC = (TSNNC + PRRC);
TSPR1 = (TSNN1 + PRR1);
TSPR2 = (TSNN2 + PRR2);
TSPR3 = (TSNN3 + PRR3);
TSPR4 = (TSNN4 + PRR4);

TSPRP = somme(i=1..4:TSPRi);
TSPRTOT = somme(i=V,C,1..4:TSPRi);
TSPRDP = somme(i=1..4:TSPRDi) ;
TSNNT = abs(TSNNV + PRRV
          + TSNNC + PRRC
          + TSNN1 + PRR1
          + TSNN2 + PRR2
          + TSNN3 + PRR3
          + TSNN4 + PRR4) 
         * (1-positif(TSNNV + PRRV + TSNNC + PRRC+ TSNN1 + PRR1 + TSNN2 + PRR2 + TSNN3 + PRR3 + TSNN4 + PRR4 )) ;

regle 881080:
application : iliad  ;

TSNN2P = somme(i=1..4: TSNN2i);
PRR2P =somme(i=1..4: PRR2i);
PENFP = PENF1 + PENF2 + PENF3 + PENF4 ;
PENALIMP = PENALIM1 + PENALIM2 + PENALIM3 + PENALIM4;

regle 881090:
application : iliad  ;


TSQVO = 2TSNV+CODDAJ+CODEAJ;
TSQCJ = 2TSNC+CODDBJ+CODEBJ;
TSQPC = somme(i=1..4: 2TSNi ) ;
PRQVO = CARPEV + PEBFV + CODRAL + CODRAM ;
PRQCJ = CARPEC + PEBFC + CODRBL + CODRBM ; 
PRQPC = CARPEP1 + PEBF1 + CARPEP2 + PEBF2 + CARPEP3 + PEBF3 + CARPEP4 + PEBF4 
        + CODRCL + CODRCM + CODRDL + CODRDM + CODREL + CODREM + CODRFL + CODRFM ;
PRQZV = CODRAZ ;
PRQZC = CODRBZ ; 
PRQZP = CODRCZ + CODRDZ + CODREZ + CODRFZ ;

PENSALP = PENSALP1 + PENSALP2 + PENSALP3 + PENSALP4 ;

regle 881100:
application : iliad  ;

PRQNV = 2PRBV;
PRQNC = 2PRBC; 
PRQNP = somme(i=1..4: 2PRBi) ;
PENSTOTV = (PRR2V + PRR2ZV + PRR2RAL + PRR2RAM + PENALIMV);
PENSTOTC = (PRR2C + PRR2ZC + PRR2RBL + PRR2RBM + PENALIMC);
PENSTOT1 = (PRR21 + PRR2Z1 + PRR2RCL + PRR2RCM + PENALIM1);
PENSTOT2 = (PRR22 + PRR2Z2 + PRR2RDL + PRR2RDM + PENALIM2);
PENSTOT3 = (PRR23 + PRR2Z3 + PRR2REL + PRR2REM + PENALIM3);
PENSTOT4 = (PRR24 + PRR2Z4 + PRR2RFL + PRR2RFM + PENALIM4);
PENSTOTP = PENSTOT1+PENSTOT2+PENSTOT3+PENSTOT4;

regle 881110:
application : iliad  ;

BPCAPTAXV = PCAPTAXV - arr(PCAPTAXV * TX_DEDPER/100);
BPCAPTAXC = PCAPTAXC - arr(PCAPTAXC * TX_DEDPER/100);
BPCAPTAXP1 = COD1CT - arr(COD1CT * TX_DEDPER/100); 
BPCAPTAXP2 = COD1DT - arr(COD1DT * TX_DEDPER/100);
BPCAPTAXP3 = COD1ET - arr(COD1ET * TX_DEDPER/100);
BPCAPTAXP4 = COD1FT - arr(COD1FT * TX_DEDPER/100);

BPCAPTAXP = BPCAPTAXP1 + BPCAPTAXP2 + BPCAPTAXP3 + BPCAPTAXP4 ;
IPCAPTAXV = arr(BPCAPTAXV * T_PCAPTAX/100) * (1 - positif(RE168 + TAX1649));
IPCAPTAXC = arr(BPCAPTAXC * T_PCAPTAX/100) * (1 - positif(RE168 + TAX1649));
IPCAPTAXP = arr(BPCAPTAXP * T_PCAPTAX/100) * (1 - positif(RE168 + TAX1649));
IPCAPTAXTOT = somme(i=V,C,P:IPCAPTAXi);

regle 881120:
application : iliad  ;

IPCAPTAXT = (IPCAPTAXTOT - CICAP) * (1 - positif(RE168 + TAX1649));


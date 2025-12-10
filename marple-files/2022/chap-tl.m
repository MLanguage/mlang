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
regle 21700:
application : iliad ;


RAP_RNI    = RNI_TL - RNI_INIT ;
RAP_EFF    = EFF_TL - EFF_INIT ;
RAP_PVQ    = PVQ_TL - PVQ_INIT ;
RAP_PV     = PV_TL - PV_INIT ;
RAP_RI     = - RI_TL + RI_INIT ;
RAP_CI     = CI_TL ;
RAP_CRDS   = RDS_TL - CRDS_INIT ;
RAP_RDS    = BRDS_TL - BRDS_INIT ;
RAP_PSOL    = BPSOL_TL - BPSOL_INIT ;
RAP_TAXAGA = TAXAGA_TL - TAXAGA_INIT ;
RAP_CAP    = PCAP_TL - PCAP_INIT ;
RAP_CHR    = CHR_TL - CHR_INIT ;
RAP_CVN    = CVNA_TL - CVN_INIT ;
RAP_CDIS   = CDISA_TL - CDIS_INIT ;
RAP_GLO    = GLOA_TL - GLO_INIT ;
RAP_RSE1   = RSE1A_TL - RSE1_INIT ;
RAP_RSE2   = RSE2A_TL - RSE2_INIT ;
RAP_RSE3   = RSE3A_TL - RSE3_INIT ;
RAP_RSE4   = RSE4A_TL - RSE4_INIT ;
RAP_RSE5   = RSE5A_TL - RSE5_INIT ;
RAP_RSE6   = RSE6A_TL - RSE6_INIT ;
RAP_RSE8   = RSE8A_TL - RSE8_INIT ;
RAP_MCSG820  = MCSG820_TL - MCSG820_INIT ;
RAP_PATNET = PATNET_TL - PATNET_INIT ; 


NUM_IR_TL = max(0 , RAP_RNI
                   + RAP_EFF
                   + RAP_PVQ
                   + RAP_PV
                   + RAP_RI 
                   + RAP_CI) ;

NUM_CS_TL     = max(0 , RAP_CRDS) ;
NUM_RD_TL     = max(0 , RAP_RDS) ;
NUM_PSOL_TL     = max(0 , RAP_PSOL) ;

NUM_TAXAGA_TL = max(0 , RAP_TAXAGA) ; 
NUM_CAP_TL    = max(0 , RAP_CAP) ;
NUM_CHR_TL    = max(0 , RAP_CHR) ;

NUM_CVN_TL    = max(0 , RAP_CVN) ;
NUM_CDIS_TL   = max(0 , RAP_CDIS) ;
NUM_GLO_TL    = max(0 , RAP_GLO) ;

NUM_RSE1_TL   = max(0 , RAP_RSE1) ;
NUM_RSE2_TL   = max(0 , RAP_RSE2) ;
NUM_RSE3_TL   = max(0 , RAP_RSE3) ;
NUM_RSE4_TL   = max(0 , RAP_RSE4) ;
NUM_RSE5_TL   = max(0 , RAP_RSE5) ;
NUM_RSE6_TL   = max(0 , RAP_RSE6) ;
NUM_RSE8_TL   = max(0 , RAP_RSE8) ;
NUM_MCSG820_TL  = max(0 , RAP_MCSG820) ;

NUM_PATNET_TL   = max(0 , RAP_PATNET) ;
regle 21710 :
application : iliad ;


DEN_IR_TL = max(0 , RNI_RECT 
                   + EFF_RECT
                   + PVQ_RECT
                   + PV_RECT
                   + RI_RECT 
                   + CI_RECT) ;

DEN_CS_TL     = max(0 , CRDS_RECT) ;
DEN_RD_TL     = max(0 , BRDS_RECT) ;
DEN_PSOL_TL     = max(0 , BPSOL_RECT) ;

DEN_TAXAGA_TL = max(0 , TAXAGA_RECT) ;
DEN_CAP_TL    = max(0 , PCAP_RECT) ;
DEN_CHR_TL    = max(0 , CHR_RECT) ;

DEN_CVN_TL    = max(0 , CVN_RECT) ;
DEN_CDIS_TL   = max(0 , CDIS_RECT) ;
DEN_GLO_TL    = max(0 , GLO_RECT) ;

DEN_RSE1_TL = max(0 , RSE1_RECT) ;
DEN_RSE2_TL = max(0 , RSE2_RECT) ;
DEN_RSE3_TL = max(0 , RSE3_RECT) ;
DEN_RSE4_TL = max(0 , RSE4_RECT) ;
DEN_RSE5_TL = max(0 , RSE5_RECT) ;
DEN_RSE6_TL = max(0 , RSE6_RECT) ;
DEN_RSE8_TL = max(0 , RSE8_RECT) ;
DEN_MCSG820_TL    = max(0 , MCSG820_RECT) ;

DEN_PATNET_TL = max(0 , PATNET_RECT) ;
regle 21720 :
application : iliad ;
enchaineur : ENCH_TL ;

RETARPRIM = null(V_IND_TRAIT - 4) * null(CMAJ - 7) ;

RETARPRIMIFI = null(V_IND_TRAIT - 4) * null(CMAJ_ISF - 7) ;

TL_IR = (1 - positif(TL_MF * positif(MFIR) + FLAG_RETARD + FLAG_DEFAUT + PASS_TLIR)) * positif(NUM_IR_TL / DEN_IR_TL  - 0.05)
        + positif(TL_MF * positif(MFIR) + FLAG_RETARD + FLAG_DEFAUT + PASS_TLIR) ;

TL_CS = (1 - positif(TL_MF * positif(MFCS) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_CS_TL / DEN_CS_TL  - 0.05) 
        + positif(TL_MF * positif(MFCS) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_RD = (1 - positif(TL_MF * positif(MFRD) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_RD_TL / DEN_RD_TL  - 0.05)
        + positif(TL_MF * positif(MFRD) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_PS = (1 - positif(TL_MF * positif(MFPS) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_PS_TL / DEN_PS_TL  - 0.05)
        + positif(TL_MF * positif(MFPS) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_PSOL = (1 - positif(TL_MF * positif(MFPSOL) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_PSOL_TL / DEN_PSOL_TL  - 0.05)
          + positif(TL_MF * positif(MFPSOL) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_TAXAGA = (1 - positif(TL_MF * positif(MFTAXAGA) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_TAXAGA_TL / DEN_TAXAGA_TL - 0.05)
            + positif(TL_MF * positif(MFTAXAGA) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_CAP = (1 - positif(TL_MF * positif(MFPCAP) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_CAP_TL / DEN_CAP_TL - 0.05)
         + positif(TL_MF * positif(MFPCAP) + FLAG_RETARD + FLAG_DEFAUT) ;


TL_CHR = (1 - positif(TL_MF * positif(MFIR + MFPCAP + MFTAXAGA ) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_CHR_TL / DEN_CHR_TL - 0.05)
         + positif(TL_MF * positif(MFIR + MFPCAP + MFTAXAGA ) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_CVN = (1 - positif(TL_MF * positif(MFCVN) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_CVN_TL / DEN_CVN_TL - 0.05)
	 + positif(TL_MF * positif(MFCVN) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_CDIS = (1 - positif(TL_MF * positif(MFCDIS) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_CDIS_TL / DEN_CDIS_TL - 0.05)
          + positif(TL_MF * positif(MFCDIS) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_GLO = (1 - positif(TL_MF * positif(MFGLO) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_GLO_TL / DEN_GLO_TL - 0.05)
         + positif(TL_MF * positif(MFGLO) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_RSE1 = (1 - positif(TL_MF * positif(MFRSE1) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_RSE1_TL / DEN_RSE1_TL - 0.05)
          + positif(TL_MF * positif(MFRSE1) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_RSE2 = (1 - positif(TL_MF * positif(MFRSE2) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_RSE2_TL / DEN_RSE2_TL - 0.05)
          + positif(TL_MF * positif(MFRSE2) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_RSE3 = (1 - positif(TL_MF * positif(MFRSE3) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_RSE3_TL / DEN_RSE3_TL - 0.05)
          + positif(TL_MF * positif(MFRSE3) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_RSE4 = (1 - positif(TL_MF * positif(MFRSE4) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_RSE4_TL / DEN_RSE4_TL - 0.05)
          + positif(TL_MF * positif(MFRSE4) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_RSE5 = (1 - positif(TL_MF * positif(MFRSE5) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_RSE5_TL / DEN_RSE5_TL - 0.05)
          + positif(TL_MF * positif(MFRSE5) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_RSE6 = (1 - positif(TL_MF * positif(MFRSE6) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_RSE6_TL / DEN_RSE6_TL - 0.05)
          + positif(TL_MF * positif(MFRSE6) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_RSE8 = (1 - positif(TL_MF * positif(MFRSE8) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_RSE8_TL / DEN_RSE8_TL - 0.05)
          + positif(TL_MF * positif(MFRSE8) + FLAG_RETARD + FLAG_DEFAUT) ;

TL_MCSG820 = (1 - positif(TL_MF * positif(MFMCSG820) + FLAG_RETARD + FLAG_DEFAUT)) * positif(NUM_MCSG820_TL / DEN_MCSG820_TL - 0.05)
             + positif(TL_MF * positif(MFMCSG820) + FLAG_RETARD + FLAG_DEFAUT) ;
	  
TL_C820 = TL_MCSG820 ;


TL_IFI = (1 - positif(TL_MF * positif(MFIFI) + FLAG_DEFAUT + FLAG_RETARD+ PASS_TLIFI)) * positif(NUM_PATNET_TL / DEN_PATNET_TL - 0.10)
         + positif(TL_MF * positif(MFIFI) + FLAG_DEFAUT + FLAG_RETARD+ PASS_TLIFI) ;


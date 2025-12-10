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
regle corrective base_tl_init 1202:
application :  iliad ;


TL_MF = IND_TL_MF;
RNI_INIT = RNI ;
EFF_INIT = (TEFFP - RNI) * positif(TEFFP - RNI) + max(0 , RMOND + DEFZU - DMOND) ;
PVQ_INIT = TTPVQ ;
PV_INIT  = BPTP3 + BPTP4 + BPTP40 + BPTP18 + BPTPD + BPTPG + BPTP19 + BPTP24 + VERSLIB;
RI_INIT  = (REDTL + CIMPTL) * (1-V_CNR) ;
CRDS_INIT = BCSG ;
BRDS_INIT = BRDS ;
BPSOL_INIT = BPSOL;
TAXAGA_INIT = BASSURV + BASSURC ;
PCAP_INIT = BPCAPTAXV + BPCAPTAXC + BPCAPTAXP ;
CHR_INIT  = REVKIREHR + (RFRH2 + RFRH1) * positif(HRCONDTHEO) ;
CVN_INIT = CVNSALAV + GLDGRATV + GLDGRATC ;
CDIS_INIT = GSALV + GSALC ;
GLO_INIT = GLDGRATV + GLDGRATC ;
RSE1_INIT = BRSE1 ;
RSE2_INIT = BRSE2 ;
RSE3_INIT = BRSE3 ;
RSE4_INIT = BRSE4 ;
RSE5_INIT = BRSE5 ;
RSE6_INIT = BRSE6 ;
RSE8_INIT = BRSE8 ;
MCSG820_INIT = BCSG820;
PATNET_INIT = IFIPAT;
regle corrective  base_tl 1204:
application :  iliad ;


RNI_TL = RNI ;
EFF_TL = (TEFFP - RNI) * positif(TEFFP - RNI) + max(0 , RMOND + DEFZU - DMOND) ;
PVQ_TL = TTPVQ ;
PV_TL  = BPTP3 + BPTP4 + BPTP40 + BPTP18 + BPTPD + BPTPG + BPTP19 + BPTP24 + VERSLIB;
RI_TL  = (REDTL + CIMPTL) * (1 - V_CNR) ;
RDS_TL  = BCSG ;
BRDS_TL = BRDS ;
BPSOL_TL = BPSOL ;
TAXAGA_TL = BASSURV + BASSURC ;
PCAP_TL = BPCAPTAXV + BPCAPTAXC + BPCAPTAXP ;
CHR_TL  = REVKIREHR + (RFRH2 + RFRH1) * positif(HRCONDTHEO) ;
CVNA_TL = CVNSALAV + GLDGRATV + GLDGRATC ;
CDISA_TL = GSALV + GSALC;
GLOA_TL = GLDGRATV + GLDGRATC ;
RSE1A_TL = BRSE1 ;
RSE2A_TL = BRSE2 ;
RSE3A_TL = BRSE3 ;
RSE4A_TL = BRSE4 ;
RSE5A_TL = BRSE5 ;
RSE6A_TL = BRSE6 ;
RSE8A_TL = BRSE8 ;
MCSG820_TL = BCSG820 ;
PATNET_TL = IFIPAT ;
regle corrective base_tl_rect 1206:
application :  iliad ;


RNI_RECT = RNI ;
EFF_RECT = (TEFFP - RNI) * positif(TEFFP - RNI) + max(0 , RMOND + DEFZU - DMOND) ;
PVQ_RECT = TTPVQ ;
PV_RECT  = BPTP3 + BPTP4 + BPTP40 + BPTP18 + BPTPD + BPTPG + BPTP19 + BPTP24 + VERSLIB;
RI_RECT  = RI_INIT - (REDTL + CIMPTL) * (1-V_CNR) ;
CRDS_RECT = BCSG ;
BRDS_RECT = BRDS ;
BPSOL_RECT = BPSOL ;
TAXAGA_RECT = BASSURV + BASSURC ;
PCAP_RECT = BPCAPTAXV + BPCAPTAXC + BPCAPTAXP ;
CHR_RECT  = REVKIREHR + (RFRH2 + RFRH1) * positif(HRCONDTHEO) ;
CVN_RECT = CVNSALAV + GLDGRATV + GLDGRATC ;
CDIS_RECT = GSALV + GSALC ;
GLO_RECT = GLDGRATV + GLDGRATC ;
RSE1_RECT = BRSE1 ;
RSE2_RECT = BRSE2 ;
RSE3_RECT = BRSE3 ;
RSE4_RECT = BRSE4 ;
RSE5_RECT = BRSE5 ;
RSE6_RECT = BRSE6 ;
RSE8_RECT = BRSE8 ;
MCSG820_RECT = BCSG820;
PATNET_RECT = IFIPAT ;
CSG_RECT = CSG ;


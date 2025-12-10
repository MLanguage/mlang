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

  ####   #    #    ##    #####      #     #####  #####   ######      #    
 #    #  #    #   #  #   #    #     #       #    #    #  #           #    #
 #       ######  #    #  #    #     #       #    #    #  #####       #    #
 #       #    #  ######  #####      #       #    #####   #           ######
 #    #  #    #  #    #  #          #       #    #   #   #                #
  ####   #    #  #    #  #          #       #    #    #  ######           #
regle 401000:
application : bareme , iliad ;


IRB = IAMD2 ; 
IRBINR = IAMD2INR ; 
IRB2 = IAMD2 + TAXASSUR + IPCAPTAXTOT + CHRAPRES ;

regle 401020:
application : bareme , iliad ;


IAMD1 = IAD11 + IBATMARG + ITP + PVMTS + REI + AVFISCOPTER + VERSLIB + AUTOVERSSUP + IMPETAL19 + IMPETAL20 + IMPETAL21
        + COD8UA + COD8UB + TAXASSUR + IPCAPTAXTOT + CHRAPRES + BRAS + NRINET + IMPRET + CODZRA ;

regle 401023:
application : bareme , iliad ;

IRBAF = IAD11  + VERSLIB + ITP + PVMTS + REI + AUTOVERSSUP;

regle 401025:
application : bareme , iliad ;

IAMD2 = IAD11 + IBATMARG + ITP + PVMTS + REI + AVFISCOPTER + VERSLIB + AUTOVERSSUP + IMPETAL19 + IMPETAL20 + IMPETAL21 + COD8UA + COD8UB ;
IAMD2INR = IAD11INR + IBATMARG + ITP + PVMTS + REI + AVFISCOPTER + VERSLIB + AUTOVERSSUP + IMPETAL19 + IMPETAL20 + IMPETAL21 + COD8UA + COD8UB ;
IAMD2TH = positif_ou_nul(IAMD2 - SEUIL_61) * IAMD2 ;


regle 401030:
application : bareme , iliad ;
IAVIM2 = IAMD1 + PTOT + PTAXA + PPCAP + PHAUTREV ;

regle 401060:
application : iliad ;

DOMITPD = arr(BN1 + SPEPV + BI12F + BA1) * (TX896/100) * positif(V_EAD);
DOMITPG = arr(BN1 + SPEPV + BI12F + BA1) * (TX768/100) * positif(V_EAG);
DOMAVTD = arr((BN1 + SPEPV + BI12F + BA1) * (TX128 - TX896)/100) * positif(V_EAD);
DOMAVTG = arr((BN1 + SPEPV + BI12F + BA1) * (TX128 - TX768)/100) * positif(V_EAG);
DOMAVTO = DOMAVTD + DOMAVTG;
DOMABDB = max(PLAF_RABDOM - ABADO , 0) * positif(V_EAD)
          + max(PLAF_RABGUY - ABAGU , 0) * positif(V_EAG);
DOMDOM = max(DOMAVTO - DOMABDB , 0) * positif(V_EAD + V_EAG);
ITP = 
        arr((BPTP4 * TX30/100) 
       +  DOMITPD + DOMITPG
       +(BPTP3 * TX128/100)
       + (BPTP10 * TX10/100)
       + (BPTP40 * TX41/100)
       + DOMDOM * positif(V_EAD + V_EAG)
       + (BPTP18 * TX18/100)
       + (BPTP5 * TX128/100) * positif(FLAG_EXIT)
       + (BPTPSJ * TX19/100)
       + (BPTPWI * TX24/100)
       + (BPTPWJ * TX19/100)
       + (BPTPPI * TX50/100)
       + IMPOT75
       + (BPTP24 * TX24/100)
	  )
       * (1 - positif(present(TAX1649)+present(RE168))) ; 

regle 401070:
application : iliad ;


REVTP = BPTP4 + BPTP3 + BPTP10 + BPTP40 + BPTP18 + (BPTP5 * positif(FLAG_EXIT)) + BPTPSJ + BPTPWI + BPTPWJ + BPTPPI + RCMIMPTR + BPTP24 + BPTPD + BPTPG ;

regle 401080:
application : iliad ;


BPTP3 =(BTP3A*(1 - positif(V_EAD + V_EAG)) + (1-positif(COD2OP))*(BTPM3VG+BTPM3UA+BPTPSB+BTPM3TJ+COD3SZ+RCMIMPTN+BPTPVT)+COD3AN)*(1-positif(present(TAX1649)+present(RE168)));


BPTP10 = PVINDUSPBIC + PVINDUSNPBIC  + PVINDUSPBNC + PVINDUSBA ;
 
regle 401085:
application : iliad ;

BTP3A =(BN1 + SPEPV + BI12F + BA1 );
BPTPD = BTP3A * positif(V_EAD)*(1-positif(present(TAX1649)+present(RE168)));
BPTPG = BTP3A * positif(V_EAG)*(1-positif(present(TAX1649)+present(RE168)));
BTP3G = BPVRCM;


BTPM3VG =(1-positif(COD2OP))*BPVRCM * (1-positif(present(TAX1649)+present(RE168)))  
                      + positif (COD2OP)* (max(0,(BPVRCM-COD3SG))) * (1-positif(present(TAX1649)+present(RE168))); 

BTPM3UA =(1-positif(COD2OP))*(max(0,(COD3UA-ABDETPLUS)))*(1-positif(present(TAX1649)+present(RE168)))
         +(positif(COD2OP)) * ((max(0,(COD3UA-ABDETPLUS-COD3SL)))* (1-positif(present(TAX1649)+present(RE168))));

BTPM3TJ =(1-positif(COD2OP))*(max(0,(COD3TJ-COD3TK)))*(1-positif(present(TAX1649)+present(RE168)))
         +(positif(COD2OP)) * ((max(0,(COD3TJ-COD3TK)))* (1-positif(present(TAX1649)+present(RE168))));

BPTPWI = COD3WI * (1-positif(present(TAX1649)+present(RE168))) ;

BPTPWJ = COD3WJ * (1-positif(present(TAX1649)+present(RE168))) ;

BPTPVT = GAINPEA * (1-positif(COD2OP)) *(1-positif(present(TAX1649)+present(RE168)));

BPTP18 = BPV18V * (1-positif(present(TAX1649)+present(RE168))) ;

BPTP4 = (BPCOPTV + BPVSK) * (1 - positif(present(TAX1649) + present(RE168))) ;
BPTP4I = BPCOPTV * (1 - positif(present(TAX1649) + present(RE168))) ;

BPTP40 = BPV40V * (1-positif(present(TAX1649)+present(RE168))) ;

BPTP5 = (PVIMPOS * (1-positif(present(TAX1649)+present(RE168))) + PVSURSI) * (1-present(COD2OP));

BPTPSJ = BPVSJ * (1-positif(present(TAX1649)+present(RE168))) ;
BPTPSK = BPVSK * (1-positif(present(TAX1649)+present(RE168)));



BPTPSB = PVTAXSB * (1-positif(present(TAX1649)+present(RE168))) ;

BTPM3SB  = BPTPSB *(1-positif(present(TAX1649)+present(RE168))) ;

BTPM3SZ = COD3SZ * (1-positif(present(TAX1649)+present(RE168)));

BPTPPI = COD3PI  * (1-positif(present(TAX1649)+present(RE168))) ;
BPTP19 = BPVSJ * (1 - positif(present(TAX1649) + present(RE168))) ;

BPTP24 = RCM2FA * (1 - positif(present(TAX1649) + present(RE168))) * (1 - V_CNR) ;
ITPRCM =( arr(BPTP24 * TX24/100));

BPT19 = BPTP19 + BPTPWJ ;

BPT24 = BPTP24 + BPTPWI ;

regle 401090:
application : iliad ;


REI = IPREP + IPPRICORSE ;

regle 401100:
application : bareme , iliad ;

IAD11 = ( max(0,IDOM11-DEC11-RED) *(1-positif(V_CNR))
        + positif(V_CNR) *max(0 , IDOM11 - RED) )
                                * (1-positif(RE168+TAX1649))
        + positif(RE168+TAX1649) * (IDOM16 - DEC6); 
IAD11INR = ( max(0,IDOM11-DEC11-RED_1) *(1-positif(V_CNR))
        + positif(V_CNR) *max(0 , IDOM11 - RED_1) )
                                * (1-positif(RE168+TAX1649))
        + positif(RE168+TAX1649) * (IDOM16 - DEC6); 
IAD13 = ( max(0,IDOM13-DEC13) *(1-positif(V_CNR))
        + positif(V_CNR) *max(0 , IDOM13 - RED3WG) )
                                * (1-positif(RE168+TAX1649))
        + positif(RE168+TAX1649) * IDOM16 ;

regle 401105:
application : bareme , iliad ;

3WBHORBAR = arr(PVIMPOS * positif(1-COD2OP) * TX128/100) * (1 - V_CNR);
3WAHORBAR = arr(PVSURSI * positif(1-COD2OP) * TX128/100) * (1 - V_CNR);
regle 401112:
application : bareme , iliad ;

IREXITI = present(FLAG_EXIT) * abs(IAD11 - V_ID113WB) * positif(positif(PVIMPOS)+positif(CODRWB)) * (1 - V_CNR) * positif(COD2OP) + 3WBHORBAR;

IREXITS = (
           abs(V_ID113WA-V_ID113WB) * positif(positif(PVIMPOS)+positif(CODRWB))
         + abs(V_ID113WA-IAD11) * (1-positif(positif(PVIMPOS)+positif(CODRWB)))
          ) 
          * present(FLAG_EXIT) * positif(positif(PVSURSI)+positif(CODRWA))
          * (1 - V_CNR) * positif(COD2OP) + 3WAHORBAR;


regle 401113:
application : bareme , iliad ;
EXITTAX3 = (V_ID113WB * positif(positif(PVIMPOS)+positif(CODRWB)) + NAPTIR * positif(positif(PVSURSI)+positif(CODRWA)) * (1-positif(positif(PVIMPOS)+positif(CODRWB)))) * (1 - V_CNR) ;


PVCREA = PVSURSI + CODRWA ;

PVCREB = PVIMPOS + CODRWB ;
regle 401115:
application : bareme , iliad ;



PVMTS =( COD3WR) ;

regle 401120:
application : bareme , iliad ;

IREXIT = IREXITI + IREXITS;
regle 401140:
application : bareme , iliad ;


DEC11 = min(max(arr((SEUIL_DECOTE1 * (1 - BOOL_0AM)) + (SEUIL_DECOTE2 * BOOL_0AM) - (IDOM11 * 45.25/100)) , 0) , IDOM11) * (1 - V_CNR) ;

DEC12 = min(max(arr((SEUIL_DECOTE1 * (1 - BOOL_0AM)) + (SEUIL_DECOTE2 * BOOL_0AM) - (IDOM12 * 45.25/100)) , 0) , IDOM12) * (1 - V_CNR) ;

DEC13 = min(max(arr((SEUIL_DECOTE1 * (1 - BOOL_0AM)) + (SEUIL_DECOTE2 * BOOL_0AM) - (IDOM13 * 45.25/100)) , 0) , IDOM13) * (1 - V_CNR) ;

DEC6 = min(max(arr((SEUIL_DECOTE1 * (1 - BOOL_0AM)) + (SEUIL_DECOTE2 * BOOL_0AM) - (IDOM16 * 45.25/100)) , 0) , IDOM16) * (1 - V_CNR) ;

regle 401150:
application : iliad ;

ART1731BIS = positif(positif(SOMMERI_2+SOMMEBIC_2+SOMMEBA_2+SOMMEBNC_2+SOMMELOC_2+SOMMERF_2+SOMMERCM_2+SOMMEMOND_2+SOMMEGLOBAL_2) + PREM8_11) ;

regle 401160:
application : iliad ;

      
RED = RCOTFOR + RREPA + RLOCANAH + RDONDJ + RDIFAGRI + RPRESSE + RFORET + RFIPDOM 
      + RFIPC + RCINE + RRESTIMO 
      + RSOCREPR + RRPRESCOMP + RHEBE + RSURV + RINNO + RSOUFIP
      + RRIRENOV + RLOGDOM + RCOMP + RRETU + RDONS + CRDIE
      + RDUFREP + RPINELTOT + RNORMTOT + RNOUV + RPENTOT  
      + RFOR + RREHAB + RRESTREP + RRESTIMO1
      + RCELTOT + RLOCNPRO 
      + RDOMSOC1 + RLOGSOC + RCOLENT + RLOCENT;
RED_1 = RCOTFOR_1 + RREPA_1  + RLOCANAH_1 + RDONDJ_1 + RDIFAGRI_1 + RPRESSE_1 + RFORET_1 + RFIPDOM_1 
      + RFIPC_1 + RCINE_1 + RRESTIMO_1 
      + RSOCREPR_1 + RRPRESCOMP_1 + RHEBE_1 + RSURV_1 + RINNO_1 + RSOUFIP_1
      + RRIRENOV_1 + RLOGDOM_1 + RCOMP_1 + RRETU_1 + RDONS_1 + CRDIE
      + ADUFREPFI_1 + ADUFREPFK_1 + ADUFREPFR_1 + ADUFREPFV_1 + ADUFREPFW_1 + ADUFREPFX_1 + ADUFREPFU_1
      + RPINELTOT_1 + RNORMTOT_1 + RNOUV_1 + RPENTOT_1 
      + RFOR_1 + RREHAB_1 + RRESTREP_1 + RRESTIMO1_1
      + RCELTOT_1 + RLOCNPRO_1 
      + RDOMSOC1_1 + RLOGSOC_1 + RCOLENT_1 + RLOCENT_1
       ;

REDTL = ASURV + ACOMP ;

CIMPTL = ATEC + ADEVDUR + TOTBGE ;


regle 401170:
application : bareme ;

RED = V_9UY ;

regle 401180:
application : iliad ;

DPRESSE = COD7MY + COD7MX ;

APRESSE_1 = (min(COD7MY , LIM10000 * (1 + BOOL_0AM)) + min(COD7MX , max(0 , LIM10000 * (1 + BOOL_0AM) - COD7MY))) * (1 - V_CNR) ;
APRESSE = positif(null(V_IND_TRAIT-4)+COD9ZA) * (APRESSE_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(APRESSE_1,max(APRESSE_P,APRESSE1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RAPRESSE = arr(min(COD7MY , LIM10000 * (1 + BOOL_0AM)) * TX50/100 + min(COD7MX , max(0 , LIM10000 * (1 + BOOL_0AM) - COD7MY)) * TX30/100) * (1 - V_CNR) ;

RPRESSE_1 = max(min(RAPRESSE , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH -RDONDJ-RDIFAGRI) , 0) ;
RPRESSE =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPRESSE_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RPRESSE_1,max(RPRESSE_P,RPRESSE1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401185:
application : iliad ;

DFORET = FORET ;

AFORET_1 = min(DFORET , LIM_FORET) * (1 - V_CNR) ;

AFORET = positif(null(V_IND_TRAIT-4)+COD9ZA) * (AFORET_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(AFORET_1,max(AFORET_P,AFORET1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RAFORET = arr(AFORET * TX_FORET/100) * (1 - V_CNR) ;

RFORET_1 = max(min(RAFORET , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE) , 0) ;

RFORET =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RFORET_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RFORET_1,max(RFORET_P,RFORET1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401190:
application : iliad ;

DFIPDOM = FIPDOMCOM ;

AFIPDOM_1 = min(FIPDOMCOM , LIMFIPDOM * (1 + BOOL_0AM)) * (1 - V_CNR) ;
AFIPDOM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AFIPDOM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(AFIPDOM_1 , max(AFIPDOM_P,AFIPDOM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

RFIPDOMCOM = arr(min(FIPDOMCOM , LIMFIPDOM * (1 + BOOL_0AM)) * TX30/100) * (1 - V_CNR) ;

RFIPDOM_1 = max(min(RFIPDOMCOM , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RCINE) , 0) ;
RFIPDOM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RFIPDOM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RFIPDOM_1 , max(RFIPDOM_P,RFIPDOM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

regle 401200:
application : iliad ;

DFIPC = FIPCORSE ;

AFIPC_1 = min(FIPCORSE , LIM_FIPCORSE * (1 + BOOL_0AM)) * (1 - V_CNR) ;
AFIPC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AFIPC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(AFIPC_1 , max(AFIPC_P,AFIPC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

RFIPCORSE = arr(min(FIPCORSE , LIM_FIPCORSE * (1 + BOOL_0AM)) * TX30/100) * (1 - V_CNR) ;

RFIPC_1 = max(min(RFIPCORSE , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RCINE-RFIPDOM) , 0) ;
RFIPC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RFIPC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RFIPC_1 , max(RFIPC_P,RFIPC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

regle 401210:
application : iliad ;

BSURV = min(RDRESU , PLAF_RSURV + PLAF_COMPSURV * (EAC + V_0DN) + PLAF_COMPSURVQAR * (V_0CH + V_0DP)) * (1 - V_CNR) ;

RRS = arr(BSURV * TX_REDSURV / 100) * (1 - V_CNR) ;

DSURV = RDRESU ;

ASURV = positif(null(V_IND_TRAIT-4)+COD9ZA) * (BSURV) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
        + (max(0,min(BSURV,max(BSURV_P,BSURV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RSURV_1 = max(min(RRS , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RFIPDOM-RFIPC
			            -RCINE-RRESTIMO-RSOCREPR-RRPRESCOMP-RHEBE ) , 0) ;
RSURV =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSURV_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSURV_1,max(RSURV_P,RSURV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


regle 401220:
application : iliad ;


DCINE = COD7EN + CINE1 + CINE2 ;

ACINE_1 = max(0 , min(DCINE , min(arr(max(SOFIRNG,RNG) * TX_CINE3/100) , PLAF_CINE))) * (1 - V_CNR) ;
ACINE = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACINE_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
        + (max(0,min(ACINE_1,max(ACINE_P,ACINE1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RRCN1 = min(COD7EN , min(arr(max(SOFIRNG , RNG) * TX_CINE3/100) , PLAF_CINE)) ;
RRCN2 = min(CINE1 , max(min(arr(max(SOFIRNG , RNG) * TX_CINE3/100) , PLAF_CINE) - RRCN1 , 0)) ;
RRCN3 = min(CINE2 , max(min(arr(max(SOFIRNG , RNG) * TX_CINE3/100) , PLAF_CINE) - RRCN1 - RRCN2 , 0)) ;

RRCN = arr((RRCN1 * TX48/100) + (RRCN2 * TX_CINE1/100) + (RRCN3 * TX_CINE2/100)) * (1 - V_CNR) ;

RCINE_1 = max(min(RRCN , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH -RDONDJ-RDIFAGRI- RPRESSE - RFORET) , 0) ;
RCINE =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCINE_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCINE_1,max(RCINE_P,RCINE1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401230:
application : iliad ;


BSOUFIPFT = min(COD7FT , LIM_SOUFIP * (1 + BOOL_0AM)) ;
BSOUFIP = min(FFIP , max(0,LIM_SOUFIP * (1 + BOOL_0AM)-COD7FT)) ;

RFIP = arr(BSOUFIP * TX_REDFIP / 100+BSOUFIPFT * TX25 / 100) * (1 - V_CNR) ;

DSOUFIP = FFIP + COD7FT;

ASOUFIP_1 = (BSOUFIP + BSOUFIPFT) * (1 - V_CNR) ;
ASOUFIP = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ASOUFIP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ASOUFIP_1,max(ASOUFIP_P,ASOUFIP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RSOUFIP_1 = max(min(RFIP , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RFIPDOM-RFIPC
			   -RCINE-RRESTIMO-RSOCREPR-RRPRESCOMP-RHEBE-RSURV-RINNO) , 0 ) ;
RSOUFIP =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOUFIP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSOUFIP_1,max(RSOUFIP_P,RSOUFIP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


regle 401240:
application : iliad ;


BRENOV = min(RIRENOV , PLAF_RENOV) * (1 - V_CNR) ;

RENOV = arr(BRENOV * TX_RENOV / 100) * (1 - V_CNR) ;

DRIRENOV = RIRENOV ;

ARIRENOV = positif(null(V_IND_TRAIT-4)+COD9ZA) * (BRENOV) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(BRENOV,max(BRENOV_P,BRENOV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RRIRENOV_1 = max(min(RENOV , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RFIPDOM-RFIPC-RCINE
			     -RRESTIMO-RSOCREPR-RRPRESCOMP-RHEBE-RSURV-RINNO-RSOUFIP) , 0 ) ;
RRIRENOV =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RRIRENOV_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RRIRENOV_1,max(RRIRENOV_P,RRIRENOV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


regle 401250:
application : iliad ;


NCOMP = max(1 , NBACT) * present(RDCOM) ;

DCOMP = RDCOM ;
ACOMP_1 = min(RDCOM , PLAF_FRCOMPTA * max(1 , NBACT)) * present(RDCOM) ;
ACOMP = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACOMP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
        + (max(0,min(ACOMP_1,max(ACOMP_P,ACOMP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


regle 401260:
application : iliad ;

RCOMP_1 = max(min(ACOMP , RRI1 - RLOGDOM) , 0) ;
RCOMP =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCOMP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCOMP_1,max(RCOMP_P,RCOMP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401265:
application : iliad ;

CONDPINEL = 1 - (null(2 - V_REGCO) + null(3 - V_REGCO) * V_INDVB31) * (1 - positif(COD7QH)) ;
CONDNORMD = 1 - (null(2 - V_REGCO) + null(3 - V_REGCO) * V_INDVB31) * (1 - positif(COD7QF)) ;

regle 401270:
application : iliad ;



ADUFREPFK_1 = (min(DUFLOFK , LIMREPDUF) * (1 - COD7QV) + DUFLOFK * COD7QV) * (1 - V_CNR) ;
ADUFREPFK = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ADUFREPFK_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
            + (max(0,min(ADUFREPFK_1,max(ADUFREPFK_P,ADUFREPFK1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

ADUFREPFR_1 = (min(DUFLOFR , LIMREPDUF) * (1 - COD7QV) + DUFLOFR * COD7QV) * (1 - V_CNR) ;
ADUFREPFR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ADUFREPFR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
            + (max(0,min(ADUFREPFR_1,max(ADUFREPFR_P,ADUFREPFR1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

ADUFREPFV_1 = (min(DUFLOFV , LIMREPDUF) * (1 - COD7QV) + DUFLOFV * COD7QV) * (1 - V_CNR) ;
ADUFREPFV = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ADUFREPFV_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
            + (max(0,min(ADUFREPFV_1,max(ADUFREPFV_P,ADUFREPFV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

ADUFREPFW_1 = (min(COD7FW , LIMREPDUF) * (1 - COD7QV) + COD7FW * COD7QV) * (1 - V_CNR) ;
ADUFREPFW = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ADUFREPFW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
            + (max(0,min(ADUFREPFW_1,max(ADUFREPFW_P,ADUFREPFW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

ADUFREPFX_1 = (min(COD7FX , LIMREPDUF) * (1 - COD7QV) + COD7FX * COD7QV) * (1 - V_CNR) ;
ADUFREPFX = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ADUFREPFX_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
            + (max(0,min(ADUFREPFX_1,max(ADUFREPFX_P,ADUFREPFX1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

ADUFREPFU_1 = (min(COD7FU , LIMREPDUF) * (1 - COD7QV) + COD7FU * COD7QV) * (1 - V_CNR) ;
ADUFREPFU = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ADUFREPFU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
            + (max(0,min(ADUFREPFU_1,max(ADUFREPFU_P,ADUFREPFU1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DDUFREP = DUFLOFK + DUFLOFR + DUFLOFV + COD7FW + COD7FX + COD7FU ;
ADUFREP = ADUFREPFK + ADUFREPFR + ADUFREPFV + ADUFREPFW + ADUFREPFX + ADUFREPFU ;

regle 401272:
application : iliad ;


APIREPBI_1 = (min(PINELBI , LIMREPPIN1) * (1 - COD7QV) + PINELBI * COD7QV) * (1 - V_CNR) ;
APIREPBI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPBI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPBI_1 , max(APIREPBI_P,APIREPBI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPDI_1 = (min(PINELDI , LIMREPPIN2) * (1 - COD7QV) + PINELDI * COD7QV) * (1 - V_CNR) ;
APIREPDI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPDI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPDI_1 , max(APIREPDI_P,APIREPDI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPCZ_1 = (min(PINELCZ , LIMREPPIN4) * (1 - COD7QV) + PINELCZ * COD7QV) * (1 - V_CNR) ;
APIREPCZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPCZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPCZ_1 , max(APIREPCZ_P,APIREPCZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPEZ_1 = (min(PINELEZ , LIMREPPIN5) * (1 - COD7QV) + PINELEZ * COD7QV) * (1 - V_CNR) ;
APIREPEZ = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPEZ_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPEZ_1,max(APIREPEZ_P,APIREPEZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPRZ_1 = (min(PINELRZ , LIMREPPIN4) * (1 - COD7QV) + PINELRZ * COD7QV) * (1 - V_CNR) ;
APIREPRZ = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPRZ_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPRZ_1,max(APIREPRZ_P,APIREPRZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPTZ_1 = (min(PINELTZ , LIMREPPIN5) * (1 - COD7QV) + PINELTZ * COD7QV) * (1 - V_CNR) ;
APIREPTZ = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPTZ_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPTZ_1,max(APIREPTZ_P,APIREPTZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPRA_1 = (min(COD7RA , LIMREPPIN4) * (1 - COD7QV) + COD7RA * COD7QV) * (1 - V_CNR) ;
APIREPRA = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPRA_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPRA_1,max(APIREPRA_P,APIREPRA1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPRB_1 = (min(COD7RB , LIMREPPIN4) * (1 - COD7QV) + COD7RB * COD7QV) * (1 - V_CNR) ;
APIREPRB = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPRB_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPRB_1,max(APIREPRB_P,APIREPRB1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPRC_1 = (min(COD7RC , LIMREPPIN5) * (1 - COD7QV) + COD7RC * COD7QV) * (1 - V_CNR) ;
APIREPRC = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPRC_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPRC_1,max(APIREPRC_P,APIREPRC1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPRD_1 = (min(COD7RD , LIMREPPIN5) * (1 - COD7QV) + COD7RD * COD7QV) * (1 - V_CNR) ;
APIREPRD = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPRD_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPRD_1,max(APIREPRD_P,APIREPRD1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPRE_1 = (min(COD7RE , LIMREPPIN4) * (1 - COD7QV) + COD7RE * COD7QV) * (1 - V_CNR) ;
APIREPRE = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPRE_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPRE_1,max(APIREPRE_P,APIREPRE1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPRF_1 = (min(COD7RF , LIMREPPIN4) * (1 - COD7QV) + COD7RF * COD7QV) * (1 - V_CNR) ;
APIREPRF = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPRF_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPRF_1,max(APIREPRF_P,APIREPRF1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPRG_1 = (min(COD7RG , LIMREPPIN5) * (1 - COD7QV) + COD7RG * COD7QV) * (1 - V_CNR) ;
APIREPRG = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPRG_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPRG_1,max(APIREPRG_P,APIREPRG1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPRH_1 = (min(COD7RH , LIMREPPIN5) * (1 - COD7QV) + COD7RH * COD7QV) * (1 - V_CNR) ;
APIREPRH = positif(null(V_IND_TRAIT-4)+COD9ZA) * APIREPRH_1 * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(APIREPRH_1,max(APIREPRH_P,APIREPRH1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

APIREPJM_1 = (min(COD7JM , LIMREPPIN4) * (1 - COD7QV) + COD7JM * COD7QV) * CONDPINEL ;
APIREPJM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPJM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPJM_1 , max(APIREPJM_P,APIREPJM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPKM_1 = (min(COD7KM , LIMREPPIN4) * (1 - COD7QV) + COD7KM * COD7QV) * CONDPINEL ;
APIREPKM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPKM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPKM_1 , max(APIREPKM_P,APIREPKM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPLM_1 = (min(COD7LM , LIMREPPIN5) * (1 - COD7QV) + COD7LM * COD7QV) * CONDPINEL ;
APIREPLM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPLM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPLM_1 , max(APIREPLM_P,APIREPLM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPMM_1 = (min(COD7MM , LIMREPPIN5) * (1 - COD7QV) + COD7MM * COD7QV) * CONDPINEL ;
APIREPMM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPMM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPMM_1 , max(APIREPMM_P,APIREPMM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPJN_1 = (min(COD7JN , LIMREPPIN4) * (1 - COD7QV) + COD7JN * COD7QV) * CONDPINEL ;
APIREPJN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPJN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPJN_1 , max(APIREPJN_P,APIREPJN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPJO_1 = (min(COD7JO , LIMREPPIN4) * (1 - COD7QV) + COD7JO * COD7QV) * CONDPINEL ;
APIREPJO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPJO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPJO_1 , max(APIREPJO_P,APIREPJO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPJP_1 = (min(COD7JP , LIMREPPIN5) * (1 - COD7QV) + COD7JP * COD7QV) * CONDPINEL ;
APIREPJP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPJP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPJP_1 , max(APIREPJP_P,APIREPJP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIREPJQ_1 = (min(COD7JQ , LIMREPPIN5) * (1 - COD7QV) + COD7JQ * COD7QV) * CONDPINEL ;
APIREPJQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIREPJQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIREPJQ_1 , max(APIREPJQ_P,APIREPJQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIJV_1 = (min(COD7JV , LIMREPPIN4) * (1 - COD7QV) + COD7JV * COD7QV) * CONDPINEL ;
APIJV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIJV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIJV_1 , max(APIJV_P,APIJV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIJW_1 = (min(COD7JW , LIMREPPIN4) * (1 - COD7QV) + COD7JW * COD7QV) * CONDPINEL ;
APIJW = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIJW_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIJW_1 , max(APIJW_P,APIJW1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIJX_1 = (min(COD7JX , LIMREPPIN5) * (1 - COD7QV) + COD7JX * COD7QV) * CONDPINEL ;
APIJX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIJX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIJX_1 , max(APIJX_P,APIJX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIJY_1 = (min(COD7JY , LIMREPPIN5) * (1 - COD7QV) + COD7JY * COD7QV) * CONDPINEL ;
APIJY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIJY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(APIJY_1 , max(APIJY_P,APIJY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

DPIREP = PINELBI + PINELDI + PINELCZ + PINELEZ + PINELRZ + PINELTZ + COD7RA + COD7RB + COD7RC + COD7RD + COD7RE + COD7RF + COD7RG 
         + COD7RH + COD7JM + COD7KM + COD7LM + COD7MM + COD7JN + COD7JO + COD7JP + COD7JQ + COD7JV + COD7JW + COD7JX + COD7JY ;

APIREP = APIREPBI + APIREPDI + APIREPCZ + APIREPEZ + APIREPRZ + APIREPTZ + APIREPRA + APIREPRB + APIREPRC + APIREPRD + APIREPRE + APIREPRF + APIREPRG 
         + APIREPRH + APIREPJM + APIREPKM + APIREPLM + APIREPMM + APIREPJN + APIREPJO + APIREPJP + APIREPJQ + APIJV + APIJW + APIJX + APIJY ;

regle 401273:
application : iliad ;


APISX_1 = (min(COD7SX , LIMREPPIN4) * (1 - COD7QV) + COD7SX * COD7QV) * (1 - V_CNR) ;
APISX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APISX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(APISX_1 , max(APISX_P,APISX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APISY_1 = (min(COD7SY , LIMREPPIN5) * (1 - COD7QV) + COD7SY * COD7QV) * (1 - V_CNR) ;
APISY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APISY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(APISY_1 , max(APISY_P,APISY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

DPROPIREP1 = COD7SX + COD7SY ;
APROPIREP1 = APISX + APISY ;

APIRI_1 = (min(COD7RI , LIMREPPIN4) * (1 - COD7QV) + COD7RI * COD7QV) * (1 - V_CNR) ;
APIRI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIRI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(APIRI_1 , max(APIRI_P,APIRI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIRJ_1 = (min(COD7RJ , LIMREPPIN5) * (1 - COD7QV) + COD7RJ * COD7QV) * (1 - V_CNR) ;
APIRJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIRJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(APIRJ_1 , max(APIRJ_P,APIRJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIUY_1 = (min(COD7UY , LIMREPPIN4) * (1 - COD7QV) + COD7UY * COD7QV) * (1 - V_CNR) ;
APIUY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIUY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(APIUY_1 , max(APIUY_P,APIUY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

APIUZ_1 = (min(COD7UZ , LIMREPPIN5) * (1 - COD7QV) + COD7UZ * COD7QV) * (1 - V_CNR) ;
APIUZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * APIUZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(APIUZ_1 , max(APIUZ_P,APIUZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

DPROPIREP = COD7RI + COD7RJ + COD7UY + COD7UZ ;
APROPIREP = APIRI + APIRJ + APIUY + APIUZ ;

regle 401274:
application : iliad ;


ANORMJA_1 = min(COD7JA , LIMREPPIN4) * CONDNORMD ;
ANORMJA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMJA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMJA_1 , max(ANORMJA_P,ANORMJA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMJB_1 = min(COD7JB , LIMREPPIN4) * CONDNORMD ;
ANORMJB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMJB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMJB_1 , max(ANORMJB_P,ANORMJB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMJC_1 = min(COD7JC , LIMREPPIN5) * CONDNORMD ;
ANORMJC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMJC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMJC_1 , max(ANORMJC_P,ANORMJC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMJD_1 = min(COD7JD , LIMREPPIN5) * CONDNORMD ;
ANORMJD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMJD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMJD_1 , max(ANORMJD_P,ANORMJD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMJR_1 = min(COD7JR , LIMREPPIN4) * CONDNORMD ;
ANORMJR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMJR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMJR_1 , max(ANORMJR_P,ANORMJR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMJS_1 = min(COD7JS , LIMREPPIN4) * CONDNORMD ;
ANORMJS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMJS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMJS_1 , max(ANORMJS_P,ANORMJS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMJT_1 = min(COD7JT , LIMREPPIN5) * CONDNORMD ;
ANORMJT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMJT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMJT_1 , max(ANORMJT_P,ANORMJT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMJU_1 = min(COD7JU , LIMREPPIN5) * CONDNORMD ;
ANORMJU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMJU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMJU_1 , max(ANORMJU_P,ANORMJU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMLG_1 = min(COD7LG , LIMREPPIN4) * CONDNORMD ;
ANORMLG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMLG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMLG_1 , max(ANORMLG_P,ANORMLG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMLH_1 = min(COD7LH , LIMREPPIN4) * CONDNORMD ;
ANORMLH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMLH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMLH_1 , max(ANORMLH_P,ANORMLH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMLI_1 = min(COD7LI , LIMREPPIN5) * CONDNORMD ;
ANORMLI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMLI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMLI_1 , max(ANORMLI_P,ANORMLI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ANORMLJ_1 = min(COD7LJ , LIMREPPIN5) * CONDNORMD ;
ANORMLJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ANORMLJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ANORMLJ_1 , max(ANORMLJ_P,ANORMLJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

DNORMREP = COD7JA + COD7JB + COD7JC + COD7JD + COD7JR + COD7JS + COD7JT + COD7JU + COD7LG + COD7LH + COD7LI + COD7LJ ;
ANORMREP = ANORMJA + ANORMJB + ANORMJC + ANORMJD + ANORMJR + ANORMJS + ANORMJT + ANORMJU + ANORMLG + ANORMLH + ANORMLI + ANORMLJ ;

regle 401276:
application : iliad ;


BAS7QQ = min(COD7QQ , LIMDUFLO) * CONDPINEL ;
VARTMP1 = BAS7QQ ;

BAS7ND = min(COD7ND , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7ND ;

BAS7QD = min(COD7QD , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QD ;

BAS7NH = min(COD7NH , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NH ;

BAS7QL = min(COD7QL , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QL ;

BAS7NL = min(COD7NL , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NL ;

BAS7QP = min(COD7QP , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QP ;

BAS7PG = min(COD7PG , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7PG ;

BAS7QY = min(COD7QY , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QY ;

BAS7NC = min(COD7NC , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NC ;

BAS7QC = min(COD7QC , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QC ;

BAS7NG = min(COD7NG , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NG ;

BAS7QK = min(COD7QK , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QK ;

BAS7NK = min(COD7NK , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NK ;

BAS7QO = min(COD7QO , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QO ;

BAS7PF = min(COD7PF , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7PF ;

BAS7QX = min(COD7QX , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QX ;

BAS7NB = min(COD7NB , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NB ;

BAS7QB = min(COD7QB , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QB ;

BAS7NF = min(COD7NF , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NF ;

BAS7QJ = min(COD7QJ , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QJ ;

BAS7NJ = min(COD7NJ , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NJ ;

BAS7QN = min(COD7QN , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QN ;

BAS7NN = min(COD7NN , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NN ;

BAS7QW = min(COD7QW , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QW ;

BAS7NA = min(COD7NA , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NA ;

BAS7QA = min(COD7QA , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QA ;

BAS7NE = min(COD7NE , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NE ;

BAS7QI = min(COD7QI , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QI ;

BAS7NI = min(COD7NI , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = VARTMP1 + BAS7NI ;

BAS7QM = min(COD7QM , LIMDUFLO - VARTMP1) * CONDPINEL ;
VARTMP1 = VARTMP1 + BAS7QM ;

BAS7NM = min(COD7NM , LIMDUFLO - VARTMP1) * CONDNORMD ;
VARTMP1 = 0 ;

BAS7RX = min(COD7RX , LIMDUFLO) * (1 - V_CNR) ;
BAS7RY = min(COD7RY , LIMDUFLO - BAS7RX) * (1 - V_CNR) ;

BAS7RP = min(COD7RP , LIMDUFLO) * (1 - V_CNR) ;
BAS7RQ = min(COD7RQ , LIMDUFLO - BAS7RP) * (1 - V_CNR) ;

regle 401278:
application : iliad ;

DNORMAN = COD7NA + COD7NB + COD7NC + COD7ND + COD7NE + COD7NF + COD7NG + COD7NH + COD7NI + COD7NJ + COD7NK + COD7NL + COD7NM + COD7NN + COD7PF + COD7PG ;

ANORMAN = arr(BAS7NA/6) + arr(BAS7NB/9) + arr(BAS7NC/6) + arr(BAS7ND/9) + arr(BAS7NE/6) + arr(BAS7NF/9) + arr(BAS7NG/6) + arr(BAS7NH/9) 
          + arr(BAS7NI/6) + arr(BAS7NJ/9) + arr(BAS7NK/6) + arr(BAS7NL/9) + arr(BAS7NM/6) + arr(BAS7NN/9) + arr(BAS7PF/6) + arr(BAS7PG/9) ;

RNORABCD = arr(arr(BAS7NA/6) * TX12/100) + arr(arr(BAS7NB/9) * TX18/100) + arr(arr(BAS7NC/6) * TX23/100) + arr(arr(BAS7ND/9) * TX29/100)
           + arr(arr(BAS7NE/6) * TX12/100) + arr(arr(BAS7NF/9) * TX18/100) + arr(arr(BAS7NG/6) * TX23/100) + arr(arr(BAS7NH/9) * TX29/100) 
	   + arr(arr(BAS7NI/6) * TX12/100) + arr(arr(BAS7NJ/9) * TX18/100) + arr(arr(BAS7NK/6) * TX23/100) + arr(arr(BAS7NL/9) * TX29/100) 
	   + arr(arr(BAS7NM/6) * TX12/100) + arr(arr(BAS7NN/9) * TX18/100) + arr(arr(BAS7PF/6) * TX23/100) + arr(arr(BAS7PG/9) * TX29/100) ;

DPINEL = COD7QA + COD7QB + COD7QC + COD7QD + COD7QQ + COD7QW + COD7QX + COD7QY + COD7QI + COD7QJ + COD7QK + COD7QL + COD7QM + COD7QN + COD7QO + COD7QP ;

APINEL = arr(BAS7QB/9) + arr(BAS7QA/6) + arr(BAS7QD/9) + arr(BAS7QC/6) + arr(BAS7QQ/9) + arr(BAS7QY/6) + arr(BAS7QX/9) + arr(BAS7QW/6)
         + arr(BAS7QJ/9) + arr(BAS7QI/6) + arr(BAS7QL/9) + arr(BAS7QK/6) + arr(BAS7QN/9) + arr(BAS7QM/6) + arr(BAS7QP/9) + arr(BAS7QO/6) ;

RPINABCD = arr(arr(BAS7QD/9) * TX29/100) + arr(arr(BAS7QC/6) * TX23/100) + arr(arr(BAS7QB/9) * TX18/100) + arr(arr(BAS7QA/6) * TX12/100) 
           + arr(arr(BAS7QQ/9) * TX29/100) + arr(arr(BAS7QY/6) * TX23/100) + arr(arr(BAS7QX/9) * TX18/100) + arr(arr(BAS7QW/6) * TX12/100) 
	   + arr(arr(BAS7QL/9) * TX29/100) + arr(arr(BAS7QK/6) * TX23/100) + arr(arr(BAS7QJ/9) * TX18/100) + arr(arr(BAS7QI/6) * TX12/100) 
	   + arr(arr(BAS7QP/9) * TX29/100) + arr(arr(BAS7QO/6) * TX23/100) + arr(arr(BAS7QN/9) * TX18/100) + arr(arr(BAS7QM/6) * TX12/100) ;

DPIRRS = COD7RX + COD7RY + COD7RP + COD7RQ ;

APIRRS = arr(BAS7RX/3) + arr(BAS7RY/3) + arr(BAS7RP/3) + arr(BAS7RQ/3) ;

RPINRRS = arr(arr(BAS7RX/3) * TX06/100) + arr(arr(BAS7RY/3) * TX06/100) + arr(arr(BAS7RP/3) * TX06/100) + arr(arr(BAS7RQ/3) * TX06/100) ;

regle 401280:
application : iliad ;


RRI1DUPI = RRI1 - RLOGDOM - RCOMP - RRETU - RDONS - CRDIE ;
VARTMP1 = 0 ;

regle 401282:
application : iliad ;

RDUFREP_1 = max(0 , min(ADUFREP , RRI1DUPI)) ;
RDUFREP = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RDUFREP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RDUFREP_1,max(RDUFREP_P,RDUFREP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = RDUFREP ;

RPIREPBI_1 = max(0 , min(APIREPBI , RRI1DUPI - VARTMP1)) ;
RPIREPBI = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPBI_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPBI_1,max(RPIREPBI_P,RPIREPBI1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPBI ;

RPIREPDI_1 = max(0 , min(APIREPDI , RRI1DUPI - VARTMP1)) ;
RPIREPDI = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPDI_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPDI_1,max(RPIREPDI_P,RPIREPDI1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPDI ;

RPIREPCZ_1 = max(0 , min(APIREPCZ , RRI1DUPI - VARTMP1)) ;
RPIREPCZ = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPCZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPCZ_1,max(RPIREPCZ_P,RPIREPCZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPCZ ;

RPIREPEZ_1 = max(0 , min(APIREPEZ , RRI1DUPI - VARTMP1)) ;
RPIREPEZ = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPEZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPEZ_1,max(RPIREPEZ_P,RPIREPEZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPEZ ;

RPIREPRZ_1 = max(0 , min(APIREPRZ , RRI1DUPI - VARTMP1)) ;
RPIREPRZ = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPRZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPRZ_1,max(RPIREPRZ_P,RPIREPRZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPRZ ;

RPIREPTZ_1 = max(0 , min(APIREPTZ , RRI1DUPI - VARTMP1)) ;
RPIREPTZ = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPTZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPTZ_1,max(RPIREPTZ_P,RPIREPTZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPTZ ;

RPIREPRA_1 = max(0 , min(APIREPRA , RRI1DUPI - VARTMP1)) ;
RPIREPRA = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPRA_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPRA_1,max(RPIREPRA_P,RPIREPRA1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPRA ;

RPIREPRB_1 = max(0 , min(APIREPRB , RRI1DUPI - VARTMP1)) ;
RPIREPRB = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPRB_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPRB_1,max(RPIREPRB_P,RPIREPRB1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPRB ;

RPIREPRC_1 = max(0 , min(APIREPRC , RRI1DUPI - VARTMP1)) ;
RPIREPRC = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPRC_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPRC_1,max(RPIREPRC_P,RPIREPRC1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPRC ;

RPIREPRD_1 = max(0 , min(APIREPRD , RRI1DUPI - VARTMP1)) ;
RPIREPRD = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPRD_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPRD_1,max(RPIREPRD_P,RPIREPRD1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPRD ;

RPIREPRE_1 = max(0 , min(APIREPRE , RRI1DUPI - VARTMP1)) ;
RPIREPRE = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPRE_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPRE_1,max(RPIREPRE_P,RPIREPRE1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPRE ;

RPIREPRF_1 = max(0 , min(APIREPRF , RRI1DUPI - VARTMP1)) ;
RPIREPRF = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPRF_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPRF_1,max(RPIREPRF_P,RPIREPRF1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPRF ;

RPIREPRG_1 = max(0 , min(APIREPRG , RRI1DUPI - VARTMP1)) ;
RPIREPRG = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPRG_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPRG_1,max(RPIREPRG_P,RPIREPRG1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPRG ;

RPIREPRH_1 = max(0 , min(APIREPRH , RRI1DUPI - VARTMP1)) ;
RPIREPRH = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPRH_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPRH_1,max(RPIREPRH_P,RPIREPRH1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPRH ;

RPIREPJM_1 = max(0 , min(APIREPJM , RRI1DUPI - VARTMP1)) ;
RPIREPJM = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPJM_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPJM_1,max(RPIREPJM_P,RPIREPJM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPJM ;

RNORMJA_1 = max(0 , min(ANORMJA , RRI1DUPI - VARTMP1)) ;
RNORMJA = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RNORMJA_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RNORMJA_1,max(RNORMJA_P,RNORMJA1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RNORMJA ;

RPIREPKM_1 = max(0 , min(APIREPKM , RRI1DUPI - VARTMP1)) ;
RPIREPKM = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPKM_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPKM_1,max(RPIREPKM_P,RPIREPKM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPKM ;

RNORMJB_1 = max(0 , min(ANORMJB , RRI1DUPI - VARTMP1)) ;
RNORMJB = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RNORMJB_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RNORMJB_1,max(RNORMJB_P,RNORMJB1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RNORMJB ;

RPIREPLM_1 = max(0 , min(APIREPLM , RRI1DUPI - VARTMP1)) ;
RPIREPLM = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPIREPLM_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RPIREPLM_1,max(RPIREPLM_P,RPIREPLM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RPIREPLM ;

RNORMJC_1 = max(0 , min(ANORMJC , RRI1DUPI - VARTMP1)) ;
RNORMJC = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RNORMJC_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RNORMJC_1,max(RNORMJC_P,RNORMJC1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RNORMJC ;

RPIREPMM_1 = max(0 , min(APIREPMM , RRI1DUPI - VARTMP1)) ;
RPIREPMM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIREPMM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(RPIREPMM_1 , max(RPIREPMM_P,RPIREPMM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIREPMM ;

RNORMJD_1 = max(0 , min(ANORMJD , RRI1DUPI - VARTMP1)) ;
RNORMJD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNORMJD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RNORMJD_1 , max(RNORMJD_P,RNORMJD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNORMJD ;

RPIJN_1 = max(0 , min(APIREPJN , RRI1DUPI - VARTMP1)) ;
RPIJN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIJN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIJN_1 , max(RPIJN_P,RPIJN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIJN ;

RNORMJR_1 = max(0 , min(ANORMJR , RRI1DUPI - VARTMP1)) ;
RNORMJR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNORMJR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RNORMJR_1 , max(RNORMJR_P,RNORMJR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNORMJR ;

RPIJO_1 = max(0 , min(APIREPJO , RRI1DUPI - VARTMP1)) ;
RPIJO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIJO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIJO_1 , max(RPIJO_P,RPIJO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIJO ;

RNORMJS_1 = max(0 , min(ANORMJS , RRI1DUPI - VARTMP1)) ;
RNORMJS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNORMJS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RNORMJS_1 , max(RNORMJS_P,RNORMJS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNORMJS ;

RPIJP_1 = max(0 , min(APIREPJP , RRI1DUPI - VARTMP1)) ;
RPIJP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIJP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIJP_1 , max(RPIJP_P,RPIJP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIJP ;

RNORMJT_1 = max(0 , min(ANORMJT , RRI1DUPI - VARTMP1)) ;
RNORMJT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNORMJT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RNORMJT_1 , max(RNORMJT_P,RNORMJT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNORMJT ;

RPIJQ_1 = max(0 , min(APIREPJQ , RRI1DUPI - VARTMP1)) ;
RPIJQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIJQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIJQ_1 , max(RPIJQ_P,RPIJQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIJQ ;

RNORMJU_1 = max(0 , min(ANORMJU , RRI1DUPI - VARTMP1)) ;
RNORMJU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNORMJU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RNORMJU_1 , max(RNORMJU_P,RNORMJU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNORMJU ;

RPIJV_1 = max(0 , min(APIJV , RRI1DUPI - VARTMP1)) ;
RPIJV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIJV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIJV_1 , max(RPIJV_P,RPIJV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIJV ;

RNORMLG_1 = max(0 , min(ANORMLG , RRI1DUPI - VARTMP1)) ;
RNORMLG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNORMLG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RNORMLG_1 , max(RNORMLG_P,RNORMLG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNORMLG ;

RPIJW_1 = max(0 , min(APIJW , RRI1DUPI - VARTMP1)) ;
RPIJW = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIJW_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIJW_1 , max(RPIJW_P,RPIJW1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIJW ;

RNORMLH_1 = max(0 , min(ANORMLH , RRI1DUPI - VARTMP1)) ;
RNORMLH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNORMLH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RNORMLH_1 , max(RNORMLH_P,RNORMLH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNORMLH ;

RPIJX_1 = max(0 , min(APIJX , RRI1DUPI - VARTMP1)) ;
RPIJX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIJX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIJX_1 , max(RPIJX_P,RPIJX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIJX ;

RNORMLI_1 = max(0 , min(ANORMLI , RRI1DUPI - VARTMP1)) ;
RNORMLI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNORMLI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RNORMLI_1 , max(RNORMLI_P,RNORMLI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNORMLI ;

RPIJY_1 = max(0 , min(APIJY , RRI1DUPI - VARTMP1)) ;
RPIJY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIJY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIJY_1 , max(RPIJY_P,RPIJY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIJY ;

RNORMLJ_1 = max(0 , min(ANORMLJ , RRI1DUPI - VARTMP1)) ;
RNORMLJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNORMLJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RNORMLJ_1 , max(RNORMLJ_P,RNORMLJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNORMLJ ;

RPISX_1 = max(0 , min(APISX , RRI1DUPI - VARTMP1)) ;
RPISX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPISX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPISX_1 , max(RPISX_P,RPISX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPISX ;

RPISY_1 = max(0 , min(APISY , RRI1DUPI - VARTMP1)) ;
RPISY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPISY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPISY_1 , max(RPISY_P,RPISY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPISY ;

RPIRI_1 = max(0 , min(APIRI , RRI1DUPI - VARTMP1)) ;
RPIRI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIRI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIRI_1 , max(RPIRI_P,RPIRI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIRI ;

RPIRJ_1 = max(0 , min(APIRJ , RRI1DUPI - VARTMP1)) ;
RPIRJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIRJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIRJ_1 , max(RPIRJ_P,RPIRJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIRJ ;

RPIUY_1 = max(0 , min(APIUY , RRI1DUPI - VARTMP1)) ;
RPIUY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIUY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIUY_1 , max(RPIUY_P,RPIUY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIUY ;

RPIUZ_1 = max(0 , min(APIUZ , RRI1DUPI - VARTMP1)) ;
RPIUZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIUZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIUZ_1 , max(RPIUZ_P,RPIUZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIUZ ;

RPIQQ_1 = max(0 , min(arr(arr(BAS7QQ/9) * TX29/100) , RRI1DUPI - VARTMP1)) ;
RPIQQ = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQQ_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQQ_1 , max(RPIQQ_P,RPIQQ1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQQ ;

RNOND_1 = max(0 , min(arr(arr(BAS7ND/9) * TX29/100) , RRI1DUPI - VARTMP1)) ;
RNOND = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNOND_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNOND_1 , max(RNOND_P,RNOND1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNOND ;

RPIQD_1 = max(0 , min(arr(arr(BAS7QD/9) * TX29/100) , RRI1DUPI - VARTMP1)) ;
RPIQD = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQD_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQD_1 , max(RPIQD_P,RPIQD1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQD ;

RNONH_1 = max(0 , min(arr(arr(BAS7NH/9) * TX29/100) , RRI1DUPI - VARTMP1)) ;
RNONH = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONH_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONH_1 , max(RNONH_P,RNONH1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONH ;

RPIQL_1 = max(0 , min(arr(arr(BAS7QL/9) * TX29/100) , RRI1DUPI - VARTMP1)) ;
RPIQL = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQL_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQL_1 , max(RPIQL_P,RPIQL1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQL ;

RNONL_1 = max(0 , min(arr(arr(BAS7NL/9) * TX29/100) , RRI1DUPI - VARTMP1)) ;
RNONL = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONL_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONL_1 , max(RNONL_P,RNONL1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONL ;

RPIQP_1 = max(0 , min(arr(arr(BAS7QP/9) * TX29/100) , RRI1DUPI - VARTMP1)) ;
RPIQP = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQP_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQP_1 , max(RPIQP_P,RPIQP1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQP ;

RNOPG_1 = max(0 , min(arr(arr(BAS7PG/9) * TX29/100) , RRI1DUPI - VARTMP1)) ;
RNOPG = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNOPG_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNOPG_1 , max(RNOPG_P,RNOPG1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNOPG ;

RPIQY_1 = max(0 , min(arr(arr(BAS7QY/6) * TX23/100) , RRI1DUPI - VARTMP1)) ;
RPIQY = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQY_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQY_1 , max(RPIQY_P,RPIQY1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQY ;

RNONC_1 = max(0 , min(arr(arr(BAS7NC/6) * TX23/100) , RRI1DUPI - VARTMP1)) ;
RNONC = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONC_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONC_1 , max(RNONC_P,RNONC1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONC ;

RPIQC_1 = max(0 , min(arr(arr(BAS7QC/6) * TX23/100) , RRI1DUPI - VARTMP1)) ;
RPIQC = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQC_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQC_1 , max(RPIQC_P,RPIQC1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQC ;

RNONG_1 = max(0 , min(arr(arr(BAS7NG/6) * TX23/100) , RRI1DUPI - VARTMP1)) ;
RNONG = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONG_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONG_1 , max(RNONG_P,RNONG1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONG ;

RPIQK_1 = max(0 , min(arr(arr(BAS7QK/6) * TX23/100) , RRI1DUPI - VARTMP1)) ;
RPIQK = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQK_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQK_1 , max(RPIQK_P,RPIQK1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQK ;

RNONK_1 = max(0 , min(arr(arr(BAS7NK/6) * TX23/100) , RRI1DUPI - VARTMP1)) ;
RNONK = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONK_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONK_1 , max(RNONK_P,RNONK1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONK ;

RPIQO_1 = max(0 , min(arr(arr(BAS7QO/6) * TX23/100) , RRI1DUPI - VARTMP1)) ;
RPIQO = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQO_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQO_1 , max(RPIQO_P,RPIQO1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQO ;

RNOPF_1 = max(0 , min(arr(arr(BAS7PF/6) * TX23/100) , RRI1DUPI - VARTMP1)) ;
RNOPF = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNOPF_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNOPF_1 , max(RNOPF_P,RNOPF1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNOPF ;

RPIQX_1 = max(0 , min(arr(arr(BAS7QX/9) * TX18/100) , RRI1DUPI - VARTMP1)) ;
RPIQX = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQX_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQX_1 , max(RPIQX_P,RPIQX1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQX ;

RNONB_1 = max(0 , min(arr(arr(BAS7NB/9) * TX18/100) , RRI1DUPI - VARTMP1)) ;
RNONB = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONB_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONB_1 , max(RNONB_P,RNONB1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONB ;

RPIQB_1 = max(0 , min(arr(arr(BAS7QB/9) * TX18/100) , RRI1DUPI - VARTMP1)) ;
RPIQB = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQB_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQB_1 , max(RPIQB_P,RPIQB1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQB ;

RNONF_1 = max(0 , min(arr(arr(BAS7NF/9) * TX18/100) , RRI1DUPI - VARTMP1)) ;
RNONF = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONF_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONF_1 , max(RNONF_P,RNONF1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONF ;

RPIQJ_1 = max(0 , min(arr(arr(BAS7QJ/9) * TX18/100) , RRI1DUPI - VARTMP1)) ;
RPIQJ = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQJ_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQJ_1 , max(RPIQJ_P,RPIQJ1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQJ ;

RNONJ_1 = max(0 , min(arr(arr(BAS7NJ/9) * TX18/100) , RRI1DUPI - VARTMP1)) ;
RNONJ = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONJ_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONJ_1 , max(RNONJ_P,RNONJ1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONJ ;

RPIQN_1 = max(0 , min(arr(arr(BAS7QN/9) * TX18/100) , RRI1DUPI - VARTMP1)) ;
RPIQN = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQN_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQN_1 , max(RPIQN_P,RPIQN1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQN ;

RNONN_1 = max(0 , min(arr(arr(BAS7NN/9) * TX18/100) , RRI1DUPI - VARTMP1)) ;
RNONN = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONN_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONN_1 , max(RNONN_P,RNONN1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONN ;

RPIQW_1 = max(0 , min(arr(arr(BAS7QW/6) * TX12/100) , RRI1DUPI - VARTMP1)) ;
RPIQW = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQW_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQW_1 , max(RPIQW_P,RPIQW1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQW ;

RNONA_1 = max(0 , min(arr(arr(BAS7NA/6) * TX12/100) , RRI1DUPI - VARTMP1)) ;
RNONA = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONA_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONA_1 , max(RNONA_P,RNONA1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONA ;

RPIQA_1 = max(0 , min(arr(arr(BAS7QA/6) * TX12/100) , RRI1DUPI - VARTMP1)) ;
RPIQA = positif(null(V_IND_TRAIT-4) + COD9ZA) * RPIQA_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RPIQA_1 , max(RPIQA_P,RPIQA1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RPIQA ;

RNONE_1 = max(0 , min(arr(arr(BAS7NE/6) * TX12/100) , RRI1DUPI - VARTMP1)) ;
RNONE = positif(null(V_IND_TRAIT-4) + COD9ZA) * RNONE_1 * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
        + (max(0,min(RNONE_1 , max(RNONE_P,RNONE1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;
VARTMP1 = VARTMP1 + RNONE ;

RPIQI_1 = max(0 , min(arr(arr(BAS7QI/6) * TX12/100) , RRI1DUPI - VARTMP1)) ;
RPIQI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIQI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIQI_1 , max(RPIQI_P,RPIQI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIQI ;

RNONI_1 = max(0 , min(arr(arr(BAS7NI/6) * TX12/100) , RRI1DUPI - VARTMP1)) ;
RNONI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNONI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RNONI_1 , max(RNONI_P,RNONI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNONI ;

RPIQM_1 = max(0 , min(arr(arr(BAS7QM/6) * TX12/100) , RRI1DUPI - VARTMP1)) ;
RPIQM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIQM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIQM_1 , max(RPIQM_P,RPIQM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIQM ;

RNONM_1 = max(0 , min(arr(arr(BAS7NM/6) * TX12/100) , RRI1DUPI - VARTMP1)) ;
RNONM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RNONM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RNONM_1 , max(RNONM_P,RNONM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RNONM ;


RPIRX_1 = max(0 , min(arr(arr(BAS7RX/3) * TX06/100) , RRI1DUPI - VARTMP1)) ;
RPIRX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIRX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIRX_1 , max(RPIRX_P,RPIRX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIRX ;

RPIRY_1 = max(0 , min(arr(arr(BAS7RY/3) * TX06/100) , RRI1DUPI - VARTMP1)) ;
RPIRY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIRY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIRY_1 , max(RPIRY_P,RPIRY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIRY ;

RPIRP_1 = max(0 , min(arr(arr(BAS7RP/3) * TX06/100) , RRI1DUPI - VARTMP1)) ;
RPIRP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIRP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIRP_1 , max(RPIRP_P,RPIRP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RPIRP ;

RPIRQ_1 = max(0 , min(arr(arr(BAS7RQ/3) * TX06/100) , RRI1DUPI - VARTMP1)) ;
RPIRQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RPIRQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
        + (max(0 , min(RPIRQ_1 , max(RPIRQ_P,RPIRQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

regle 401284:
application : iliad ;

RNORMREP = RNORMJA + RNORMJB + RNORMJC + RNORMJD + RNORMJR + RNORMJS + RNORMJT + RNORMJU + RNORMLG + RNORMLH + RNORMLI + RNORMLJ ;

RNORMAN = RNONC + RNOND + RNONA + RNONB + RNONG + RNONH + RNONE + RNONF + RNONI + RNONJ + RNONK + RNONL + RNOPG + RNOPF + RNONN + RNONM ;

RNORMTOT = RNORMREP + RNORMAN ;

RNORMTOT_1 = RNORMJA_1 + RNORMJB_1 + RNORMJC_1 + RNORMJD_1 + RNORMJR_1 + RNORMJS_1 + RNORMJT_1 + RNORMJU_1 + RNORMLG_1 + RNORMLH_1 
             + RNORMLI_1 + RNORMLJ_1 + RNONC_1 + RNOND_1 + RNONA_1 + RNONB_1 + RNONG_1 + RNONH_1 + RNONE_1 + RNONF_1 + RNONI_1 
	     + RNONJ_1 + RNONK_1 + RNONL_1 + RNOPG_1 + RNOPF_1 + RNONN_1 + RNONM_1 ;

RPIREP = RPIREPBI + RPIREPDI + RPIREPCZ + RPIREPEZ + RPIREPRZ + RPIREPTZ + RPIREPRA + RPIREPRB + RPIREPRC + RPIREPRD + RPIREPRE + RPIREPRF
         + RPIREPRG + RPIREPRH + RPIREPJM + RPIREPKM + RPIREPLM + RPIREPMM + RPIJN + RPIJO + RPIJP + RPIJQ + RPIJV + RPIJW + RPIJX + RPIJY ;

RPINEL = RPIQA + RPIQB + RPIQC + RPIQD + RPIQQ + RPIQY + RPIQW + RPIQX + RPIQO + RPIQP + RPIQM + RPIQN + RPIQI + RPIQJ + RPIQK + RPIQL ;

RPROPIREP1 = RPISX + RPISY ;

RPROPIREP = RPIRI + RPIRJ + RPIUY + RPIUZ ;

RPROPINEL = RPIRX + RPIRY + RPIRP + RPIRQ ;

RPINELTOT = RPIREP + RPINEL + RPROPIREP1 + RPROPIREP + RPROPINEL ;

RPINELTOT_1 = max(0,min(APIREPBI_1 + APIREPDI_1 + APIREPCZ_1 + APIREPEZ_1 + APIREPRZ_1 + APIREPTZ_1 + APIREPRA_1
                        + APIREPRB_1 + APIREPRC_1 + APIREPRD_1 + APIREPRE_1 + APIREPRF_1 + APIREPRG_1 + APIREPRH_1
                        + APIREPJM_1 + APIREPKM_1 + APIREPLM_1 + APIREPMM_1 + RPIJN_1 + RPIJO_1 + RPIJP_1 + RPIJQ_1 + RPIJV_1 + RPIJW_1 + RPIJX_1 + RPIJY_1 , RRI1DUPI))
              + RPIQA_1 + RPIQB_1 + RPIQC_1 + RPIQD_1 + RPIQQ_1 + RPIQY_1 + RPIQW_1 + RPIQX_1 + RPIQO_1 + RPIQP_1 + RPIQM_1 + RPIQN_1 + RPIQI_1 + RPIQJ_1 + RPIQK_1 + RPIQL_1
              + RPIRX_1 + RPIRY_1 + RPIRP_1 + RPIRQ_1 + RPISX_1 + RPISY_1 + RPIRI_1 + RPIRJ_1 + RPIUY_1 + RPIUZ_1 ;

regle 401290:
application : iliad ;


RIVPIQP = arr(arr(BAS7QP/9) * (TX29/100)) * (1 - V_CNR) ;
RIVPIQP8 = max(0 , arr(BAS7QP * (TX29/100)) - 8 * RIVPIQP) * (1 - V_CNR) ; 

RIVPIQN = arr(arr(BAS7QN/9) * (TX18/100)) * (1 - V_CNR) ;
RIVPIQN8 = max(0 , arr(BAS7QN * (TX18/100)) - 8 * RIVPIQN) * (1 - V_CNR) ;

RIVPIQO = arr(arr(BAS7QO/6) * (TX23/100)) * (1 - V_CNR) ;
RIVPIQO5 = max(0 , arr(BAS7QO * (TX23/100)) - 5 * RIVPIQO) * (1 - V_CNR) ;

RIVPIQM = arr(arr(BAS7QM/6) * (TX12/100)) * (1 - V_CNR) ;
RIVPIQM5 = max(0 , arr(BAS7QM * (TX12/100)) - 5 * RIVPIQM) * (1 - V_CNR) ;


RIVPIQL = arr(arr(BAS7QL/9) * (TX29/100)) * (1 - V_CNR) ;
RIVPIQL8 = max(0 , arr(BAS7QL * (TX29/100)) - 8 * RIVPIQL) * (1 - V_CNR) ; 

RIVPIQJ = arr(arr(BAS7QJ/9) * (TX18/100)) * (1 - V_CNR) ;
RIVPIQJ8 = max(0 , arr(BAS7QJ * (TX18/100)) - 8 * RIVPIQJ) * (1 - V_CNR) ;

RIVPIQK = arr(arr(BAS7QK/6) * (TX23/100)) * (1 - V_CNR) ;
RIVPIQK5 = max(0 , arr(BAS7QK * (TX23/100)) - 5 * RIVPIQK) * (1 - V_CNR) ;

RIVPIQI = arr(arr(BAS7QI/6) * (TX12/100)) * (1 - V_CNR) ;
RIVPIQI5 = max(0 , arr(BAS7QI * (TX12/100)) - 5 * RIVPIQI) * (1 - V_CNR) ;


RIVPIQD = arr(arr(BAS7QD/9) * TX29/100) ;
RIVPIQD8 = max(0 , arr(BAS7QD * TX29/100) - 8 * RIVPIQD) ;

RIVPIQB = arr(arr(BAS7QB/9) * TX18/100) ;
RIVPIQB8 = max(0 , arr(BAS7QB * TX18/100) - 8 * RIVPIQB) ;

RIVPIQC = arr(arr(BAS7QC/6) * TX23/100) ;
RIVPIQC5 = max(0 , arr(BAS7QC * TX23/100) - 5 * RIVPIQC) ;

RIVPIQA = arr(arr(BAS7QA/6) * TX12/100) ;
RIVPIQA5 = max(0 , arr(BAS7QA * TX12/100) - 5 * RIVPIQA) ;


RIVPIQQ = arr(arr(BAS7QQ/9) * TX29/100) ;
RIVPIQQ8 = max(0 , arr(BAS7QQ * TX29/100) - 8 * RIVPIQQ) ;

RIVPIQX = arr(arr(BAS7QX/9) * TX18/100) ;
RIVPIQX8 = max(0 , arr(BAS7QX * TX18/100) - 8 * RIVPIQX) ;

RIVPIQY = arr(arr(BAS7QY/6) * TX23/100) ;
RIVPIQY5 = max(0 , arr(BAS7QY * TX23/100) - 5 * RIVPIQY) ;

RIVPIQW = arr(arr(BAS7QW/6) * TX12/100) ;
RIVPIQW5 = max(0 , arr(BAS7QW * TX12/100) - 5 * RIVPIQW) ;


RIVNOPG = arr(arr(BAS7PG/9) * TX29/100) ;
RIVNOPG8 = max(0 , arr(BAS7PG * TX29/100) - 8 * RIVNOPG) ;

RIVNONN = arr(arr(BAS7NN/9) * TX18/100) ;
RIVNONN8 = max(0 , arr(BAS7NN * TX18/100) - 8 * RIVNONN) ;

RIVNOPF = arr(arr(BAS7PF/6) * TX23/100) ;
RIVNOPF5 = max(0 , arr(BAS7PF * TX23/100) - 5 * RIVNOPF) ;

RIVNONM = arr(arr(BAS7NM/6) * TX12/100) ;
RIVNONM5 = max(0 , arr(BAS7NM * TX12/100) - 5 * RIVNONM) ;


RIVNONL = arr(arr(BAS7NL/9) * TX29/100) ;
RIVNONL8 = max(0 , arr(BAS7NL * TX29/100) - 8 * RIVNONL) ;

RIVNONJ = arr(arr(BAS7NJ/9) * TX18/100) ;
RIVNONJ8 = max(0 , arr(BAS7NJ * TX18/100) - 8 * RIVNONJ) ;

RIVNONK = arr(arr(BAS7NK/6) * TX23/100) ;
RIVNONK5 = max(0 , arr(BAS7NK * TX23/100) - 5 * RIVNONK) ;

RIVNONI = arr(arr(BAS7NI/6) * TX12/100) ;
RIVNONI5 = max(0 , arr(BAS7NI * TX12/100) - 5 * RIVNONI) ;


RIVNONH = arr(arr(BAS7NH/9) * TX29/100) ;
RIVNONH8 = max(0 , arr(BAS7NH * TX29/100) - 8 * RIVNONH) ;

RIVNONF = arr(arr(BAS7NF/9) * TX18/100) ;
RIVNONF8 = max(0 , arr(BAS7NF * TX18/100) - 8 * RIVNONF) ;

RIVNONG = arr(arr(BAS7NG/6) * TX23/100) ;
RIVNONG5 = max(0 , arr(BAS7NG * TX23/100) - 5 * RIVNONG) ;

RIVNONE = arr(arr(BAS7NE/6) * TX12/100) ;
RIVNONE5 = max(0 , arr(BAS7NE * TX12/100) - 5 * RIVNONE) ;


RIVNOND = arr(arr(BAS7ND/9) * TX29/100) * (1 - V_CNR) ;
RIVNOND8 = max(0 , arr(BAS7ND * TX29/100) - 8 * RIVNOND) * (1 - V_CNR) ;

RIVNONB = arr(arr(BAS7NB/9) * TX18/100) * (1 - V_CNR) ;
RIVNONB8 = max(0 , arr(BAS7NB * TX18/100) - 8 * RIVNONB) * (1 - V_CNR) ;

RIVNONC = arr(arr(BAS7NC/6) * TX23/100) * (1 - V_CNR) ;
RIVNONC5 = max(0 , arr(BAS7NC * TX23/100) - 5 * RIVNONC) * (1 - V_CNR) ;

RIVNONA = arr(arr(BAS7NA/6) * TX12/100) * (1 - V_CNR) ;
RIVNONA5 = max(0 , arr(BAS7NA * TX12/100) - 5 * RIVNONA) * (1 - V_CNR) ;


RIVPIRX = arr(arr(BAS7RX/3) * (TX06/100)) * (1 - V_CNR) ;
RIVPIRX2 = max(0 , arr(BAS7RX * (TX06/100)) - 2 * RIVPIRX) * (1 - V_CNR) ;

RIVPIRY = arr(arr(BAS7RY/3) * (TX06/100)) * (1 - V_CNR) ;
RIVPIRY2 = max(0 , arr(BAS7RY * (TX06/100)) - 2 * RIVPIRY) * (1 - V_CNR) ;

RIVPIRP = arr(arr(BAS7RP/3) * (TX06/100)) * (1 - V_CNR) ;
RIVPIRP2 = max(0 , arr(BAS7RP * (TX06/100)) - 2 * RIVPIRP) * (1 - V_CNR) ;

RIVPIRQ = arr(arr(BAS7RQ/3) * (TX06/100)) * (1 - V_CNR) ;
RIVPIRQ2 = max(0 , arr(BAS7RQ * (TX06/100)) - 2 * RIVPIRQ) * (1 - V_CNR) ;

REPIQP = RIVPIQP * 7 + RIVPIQP8 ;
REPIQN = RIVPIQN * 7 + RIVPIQN8 ;
REPIQO = RIVPIQO * 4 + RIVPIQO5 ;
REPIQM = RIVPIQM * 4 + RIVPIQM5 ;
REPIQL = RIVPIQL * 7 + RIVPIQL8 ;
REPIQJ = RIVPIQJ * 7 + RIVPIQJ8 ;
REPIQK = RIVPIQK * 4 + RIVPIQK5 ;
REPIQI = RIVPIQI * 4 + RIVPIQI5 ;
REPIQD = RIVPIQD * 7 + RIVPIQD8 ;
REPIQB = RIVPIQB * 7 + RIVPIQB8 ;
REPIQC = RIVPIQC * 4 + RIVPIQC5 ;
REPIQA = RIVPIQA * 4 + RIVPIQA5 ;
REPIQQ = RIVPIQQ * 7 + RIVPIQQ8 ;
REPIQX = RIVPIQX * 7 + RIVPIQX8 ;
REPIQY = RIVPIQY * 4 + RIVPIQY5 ;
REPIQW = RIVPIQW * 4 + RIVPIQW5 ;
RENOPG = RIVNOPG * 7 + RIVNOPG8 ;
RENONN = RIVNONN * 7 + RIVNONN8 ;
RENOPF = RIVNOPF * 4 + RIVNOPF5 ;
RENONM = RIVNONM * 4 + RIVNONM5 ;
RENONL = RIVNONL * 7 + RIVNONL8 ;
RENONJ = RIVNONJ * 7 + RIVNONJ8 ;
RENONK = RIVNONK * 4 + RIVNONK5 ;
RENONI = RIVNONI * 4 + RIVNONI5 ;
RENONH = RIVNONH * 7 + RIVNONH8 ;
RENONF = RIVNONF * 7 + RIVNONF8 ;
RENONG = RIVNONG * 4 + RIVNONG5 ;
RENONE = RIVNONE * 4 + RIVNONE5 ;
RENOND = RIVNOND * 7 + RIVNOND8 ;
RENONB = RIVNONB * 7 + RIVNONB8 ;
RENONC = RIVNONC * 4 + RIVNONC5 ;
RENONA = RIVNONA * 4 + RIVNONA5 ;
REPIRX = RIVPIRX + RIVPIRX2 ;
REPIRY = RIVPIRY + RIVPIRY2 ;
REPIRP = RIVPIRP + RIVPIRP2 ;
REPIRQ = RIVPIRQ + RIVPIRQ2 ;

regle 401300:
application : iliad ;


REDUCAVTCEL = RCOTFOR + RREPA + RLOCANAH + RDONDJ + RDIFAGRI + RPRESSE + RFORET + RFIPDOM + RFIPC + RCINE + RRESTIMO + RSOCREPR + RRPRESCOMP 
              + RHEBE + RSURV + RINNO + RSOUFIP + RRIRENOV + RLOGDOM + RCOMP + RRETU + RDONS + CRDIE + RDUFREP + RPINELTOT 
	      + RNORMTOT + RNOUV + RPENTOT + RFOR + RREHAB + RRESTREP + RRESTIMO1 ;

VARTMP1 = DEC11 + REDUCAVTCEL ;

RCELRREDLQ_1 = max(min(CELRREDLQ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRREDLQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELRREDLQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
             + (max(0 , min(RCELRREDLQ_1 , max(RCELRREDLQ_P,RCELRREDLQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELRREDLQ ;

RCELRREDLR_1 = max(min(CELRREDLR , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRREDLR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELRREDLR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
             + (max(0 , min(RCELRREDLR_1 , max(RCELRREDLR_P,RCELRREDLR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELRREDLR ;

RCELRREDLU_1 = max(min(CELRREDLU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRREDLU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELRREDLU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
             + (max(0 , min(RCELRREDLU_1 , max(RCELRREDLU_P,RCELRREDLU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELRREDLU ;

RCELRREDLV_1 = max(min(CELRREDLV , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRREDLV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELRREDLV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
             + (max(0 , min(RCELRREDLV_1 , max(RCELRREDLV_P,RCELRREDLV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELRREDLV ;

RCELLA_1 = max(min(COD7LA , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELLA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLA_1 , max(RCELLA_P,RCELLA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLA ;

RCELLB_1 = max(min(COD7LB , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELLB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLB_1 , max(RCELLB_P,RCELLB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLB ;

RCELLC_1 = max(min(COD7LC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELLC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLC_1 , max(RCELLC_P,RCELLC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLC ;

RCELLY_1 = max(min(COD7LY , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELLY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLY_1 , max(RCELLY_P,RCELLY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLY ;

RCELMS_1 = max(min(COD7MS , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMS_1 , max(RCELMS_P,RCELMS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMS ;

RCELMT_1 = max(min(COD7MT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMT_1 , max(RCELMT_P,RCELMT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMT ;

RCELMU_1 = max(min(COD7MU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMU_1 , max(RCELMU_P,RCELMU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMU ;

RCELMV_1 = max(min(COD7MV , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMV_1 , max(RCELMV_P,RCELMV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMV ;

RCELMO_1 = max(min(COD7MO , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMO_1 , max(RCELMO_P,RCELMO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMO ;

RCELMP_1 = max(min(COD7MP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMP_1 , max(RCELMP_P,RCELMP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMP ;

RCELMQ_1 = max(min(COD7MQ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMQ_1 , max(RCELMQ_P,RCELMQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMQ ;

RCELMR_1 = max(min(COD7MR , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMR_1 , max(RCELMR_P,RCELMR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMR ;

RCELMA_1 = max(min(COD7MA , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMA_1 , max(RCELMA_P,RCELMA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMA ;

RCELMB_1 = max(min(COD7MB , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMB_1 , max(RCELMB_P,RCELMB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMB ;

RCELMC_1 = max(min(COD7MC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMC_1 , max(RCELMC_P,RCELMC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMC ;

RCELMD_1 = max(min(COD7MD , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMD_1 , max(RCELMD_P,RCELMD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMD ;

RCELMI_1 = max(min(COD7MI , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMI_1 , max(RCELMI_P,RCELMI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMI ;

RCELMJ_1 = max(min(COD7MJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMJ_1 , max(RCELMJ_P,RCELMJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMJ ;

RCELMK_1 = max(min(COD7MK , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELMK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMK_1 , max(RCELMK_P,RCELMK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMK ;

RCELML_1 = max(min(COD7ML , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELML = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELML_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELML_1 , max(RCELML_P,RCELML1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

regle 401301:
application : iliad ;

DCELSOM1 = somme (i=Q,R,U,V : CELRREDLi) + COD7LA + COD7LB + COD7LC + COD7LY + COD7MS + COD7MT + COD7MU + COD7MV + COD7MO + COD7MA 
                                         + COD7MP + COD7MB + COD7MQ + COD7MC + COD7MR + COD7MD + COD7MI + COD7MJ + COD7MK + COD7ML ;

ACELSOM1 = DCELSOM1 * ( 1 - V_CNR ) ;

RCELSOM1 = somme (i=Q,R,U,V : RCELRREDLi) + RCELLA + RCELLB + RCELLC + RCELLY + RCELMS + RCELMT + RCELMU + RCELMV + RCELMO + RCELMA 
                                          + RCELMP + RCELMB + RCELMQ + RCELMC + RCELMR + RCELMD + RCELMI + RCELMJ + RCELMK + RCELML ;

RCELSOM1_1 = somme (i=Q,R,U,V : RCELRREDLi_1) + RCELLA_1 + RCELLB_1 + RCELLC_1 + RCELLY_1 + RCELMS_1 + RCELMT_1 + RCELMU_1 + RCELMV_1 + RCELMO_1 + RCELMA_1 
                                              + RCELMP_1 + RCELMB_1 + RCELMQ_1 + RCELMC_1 + RCELMR_1 + RCELMD_1 + RCELMI_1 + RCELMJ_1 + RCELMK_1 + RCELML_1 ;

regle 401302:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELSOM1 ;

RCELZP_1 = max(min(COD7ZP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELZP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELZP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELZP_1 , max(RCELZP_P,RCELZP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELZP ;

RCELZO_1 = max(min(COD7ZO , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELZO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELZO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELZO_1 , max(RCELZO_P,RCELZO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELZO ;

RCELXP_1 = max(min(COD7XP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXP_1 , max(RCELXP_P,RCELXP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXP ;

RCELXO_1 = max(min(COD7XO , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXO_1 , max(RCELXO_P,RCELXO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXO ;

RCELXQ_1 = max(min(COD7XQ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXQ_1 , max(RCELXQ_P,RCELXQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXQ ;

RCELYI_1 = max(min(COD7YI , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYI_1 , max(RCELYI_P,RCELYI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYI ;

RCELYJ_1 = max(min(COD7YJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYJ_1 , max(RCELYJ_P,RCELYJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYJ ;

RCELYK_1 = max(min(COD7YK , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYK_1 , max(RCELYK_P,RCELYK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYK ;

RCELYL_1 = max(min(COD7YL , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYL_1 , max(RCELYL_P,RCELYL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYL ;

RCELZI_1 = max(min(COD7ZI , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELZI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELZI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELZI_1 , max(RCELZI_P,RCELZI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELZI ;

RCELZJ_1 = max(min(COD7ZJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELZJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELZJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELZJ_1 , max(RCELZJ_P,RCELZJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELZJ ;

RCELZK_1 = max(min(COD7ZK , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELZK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELZK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELZK_1 , max(RCELZK_P,RCELZK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELZK ;

RCELZL_1 = max(min(COD7ZL , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELZL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELZL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCELZL_1 , max(RCELZL_P,RCELZL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELZL ;

RCELUU_1 = max(min(COD7UU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELUU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELUU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCELUU_1 , max(RCELUU_P,RCELUU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELUU ;

RCELUV_1 = max(min(COD7UV , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELUV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELUV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCELUV_1 , max(RCELUV_P,RCELUV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELUV ;

RCELUW_1 = max(min(COD7UW , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELUW = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELUW_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCELUW_1 , max(RCELUW_P,RCELUW1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELUW ;

RCELUX_1 = max(min(COD7UX , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELUX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELUX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCELUX_1 , max(RCELUX_P,RCELUX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELUX ;

RCELRK_1 = max(min(COD7RK , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELRK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCELRK_1 , max(RCELRK_P,RCELRK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELRK ;

RCELRL_1 = max(min(COD7RL , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELRL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCELRL_1 , max(RCELRL_P,RCELRL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELRL ;

RCELRM_1 = max(min(COD7RM , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELRM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCELRM_1 , max(RCELRM_P,RCELRM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELRM ;

RCELRN_1 = max(min(COD7RN , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELRN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCELRN_1 , max(RCELRN_P,RCELRN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

regle 401303:
application : iliad ;

DCELSOM2 = COD7YI + COD7ZI + COD7ZP + COD7XP + COD7YJ + COD7ZJ + COD7ZO + COD7XO + COD7YK + COD7ZK + COD7XQ 
           + COD7YL + COD7ZL + COD7UU + COD7UV + COD7UW + COD7UX + COD7RK + COD7RL + COD7RM + COD7RN ;

ACELSOM2 = DCELSOM2 * (1 - V_CNR) ;

RCELSOM2 = RCELYI + RCELZI + RCELZP + RCELXP + RCELYJ + RCELZJ + RCELZO + RCELXO + RCELYK + RCELZK + RCELXQ 
           + RCELYL + RCELZL + RCELUU + RCELUV + RCELUW + RCELUX + RCELRK + RCELRL + RCELRM + RCELRN ;

RCELSOM2_1 = RCELYI_1 + RCELZI_1 + RCELZP_1 + RCELXP_1 + RCELYJ_1 + RCELZJ_1 + RCELZO_1 + RCELXO_1 + RCELYK_1 + RCELZK_1 + RCELXQ_1 
             + RCELYL_1 + RCELZL_1 + RCELUU_1 + RCELUV_1 + RCELUW_1 + RCELUX_1 + RCELRK_1 + RCELRL_1 + RCELRM_1 + RCELRN_1 ;

regle 401304:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELSOM1 + RCELSOM2 ;

RCELHZ_1 = max(min(COD7HZ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELHZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELHZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELHZ_1 , max(RCELHZ_P,RCELHZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELHZ ;

RCELKC_1 = max(min(COD7KC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKC_1 , max(RCELKC_P,RCELKC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKC ;

RCELPC_1 = max(min(COD7PC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELPC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELPC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELPC_1 , max(RCELPC_P,RCELPC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELPC ;

RCELKT_1 = max(min(COD7KT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKT_1 , max(RCELKT_P,RCELKT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKT ;

RCELKD_1 = max(min(COD7KD , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKD_1 , max(RCELKD_P,RCELKD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKD ;

RCELPD_1 = max(min(COD7PD , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELPD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELPD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELPD_1 , max(RCELPD_P,RCELPD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELPD ;

RCELKU_1 = max(min(COD7KU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKU_1 , max(RCELKU_P,RCELKU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKU ;

RCELPE_1 = max(min(COD7PE , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELPE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELPE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELPE_1 , max(RCELPE_P,RCELPE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELPE ;

RCELKV_1 = max(min(COD7KV , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKV_1 , max(RCELKV_P,RCELKV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

regle 401305:
application : iliad ;

DCELSOM5 = COD7HZ + COD7KC + COD7PC + COD7KT + COD7KD + COD7PD + COD7KU + COD7PE + COD7KV ;

ACELSOM5 = DCELSOM5 * (1 - V_CNR) ;

RCELSOM5 = RCELHZ + RCELKC + RCELPC + RCELKT + RCELKD + RCELPD + RCELKU + RCELPE + RCELKV ;

RCELSOM5_1 = RCELHZ_1 + RCELKC_1 + RCELPC_1 + RCELKT_1 + RCELKD_1 + RCELPD_1 + RCELKU_1 + RCELPE_1 + RCELKV_1 ;

regle 401306:
application : iliad ;

ACELREPYP_1 = (min(LIMREPSC7 , CELREPYP) * (1 - COD7YE) + CELREPYP * COD7YE) * (1 - V_CNR) ;
ACELREPYP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPYP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPYP_1 , max(ACELREPYP_P,ACELREPYP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPYO_1 = (min(LIMREPSC7 , CELREPYO) * (1 - COD7YE) + CELREPYO * COD7YE) * (1 - V_CNR) ;
ACELREPYO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPYO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPYO_1 , max(ACELREPYO_P,ACELREPYO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPYN_1 = (min(LIMREPSC6 , CELREPYN) * (1 - COD7YE) + CELREPYN * COD7YE) * (1 - V_CNR) ;
ACELREPYN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPYN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPYN_1 , max(ACELREPYN_P,ACELREPYN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPYM_1 = (min(LIMREPSC4 , CELREPYM) * (1 - COD7YE) + CELREPYM * COD7YE) * (1 - V_CNR) ;
ACELREPYM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPYM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPYM_1 , max(ACELREPYM_P,ACELREPYM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPYW_1 = (min(LIMREPSC7 , CELREPYW) * (1 - COD7YE) + CELREPYW * COD7YE) * (1 - V_CNR) ;
ACELREPYW = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPYW_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPYW_1 , max(ACELREPYW_P,ACELREPYW1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPYV_1 = (min(LIMREPSC7 , CELREPYV) * (1 - COD7YE) + CELREPYV * COD7YE) * (1 - V_CNR) ;
ACELREPYV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPYV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPYV_1 , max(ACELREPYV_P,ACELREPYV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPYU_1 = (min(LIMREPSC6 , CELREPYU) * (1 - COD7YE) + CELREPYU * COD7YE) * (1 - V_CNR) ;
ACELREPYU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPYU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPYU_1 , max(ACELREPYU_P,ACELREPYU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPYT_1 = (min(LIMREPSC4 , CELREPYT) * (1 - COD7YE) + CELREPYT * COD7YE) * (1 - V_CNR) ;
ACELREPYT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPYT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPYT_1 , max(ACELREPYT_P,ACELREPYT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPWW_1 = (min(LIMREPSC7 , CELREPWW) * (1 - COD7YE) + CELREPWW * COD7YE) * (1 - V_CNR) ;
ACELREPWW = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPWW_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPWW_1 , max(ACELREPWW_P,ACELREPWW1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPWV_1 = (min(LIMREPSC7 , CELREPWV) * (1 - COD7YE) + CELREPWV * COD7YE) * (1 - V_CNR) ;
ACELREPWV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPWV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPWV_1 , max(ACELREPWV_P,ACELREPWV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ; 

ACELREPWU_1 = (min(LIMREPSC6 , CELREPWU) * (1 - COD7YE) + CELREPWU * COD7YE) * (1 - V_CNR) ;
ACELREPWU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPWU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPWU_1 , max(ACELREPWU_P,ACELREPWU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELREPWT_1 = (min(LIMREPSC4 , CELREPWT) * (1 - COD7YE) + CELREPWT * COD7YE) * (1 - V_CNR) ;
ACELREPWT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELREPWT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
            + (max(0 , min(ACELREPWT_1 , max(ACELREPWT_P,ACELREPWT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELRU_1 = (min(LIMREPSC3 , COD7RU) * (1 - COD7YE) + COD7RU * COD7YE) * (1 - V_CNR) ;
ACELRU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELRU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELRU_1 , max(ACELRU_P,ACELRU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELRT_1 = (min(LIMREPSC4 , COD7RT) * (1 - COD7YE) + COD7RT * COD7YE) * (1 - V_CNR) ;
ACELRT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELRT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELRT_1 , max(ACELRT_P,ACELRT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

DCELSOM4 = CELREPYP + CELREPYO + CELREPYN + CELREPYM + CELREPYW + CELREPYV + CELREPYU 
           + CELREPYT + CELREPWW + CELREPWV + CELREPWU + CELREPWT + COD7RU + COD7RT ;

ACELSOM4 = ACELREPYP + ACELREPYO + ACELREPYN + ACELREPYM + ACELREPYW + ACELREPYV + ACELREPYU 
           + ACELREPYT + ACELREPWW + ACELREPWV + ACELREPWU + ACELREPWT + ACELRU + ACELRT ;

regle 401308:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELSOM1 + RCELSOM2 + RCELSOM5 ;

RCELREPYP_1 = max(min(ACELREPYP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPYP =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPYP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPYP_1,max(RCELREPYP_P,RCELREPYP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPYP ;

RCELREPYO_1 = max(min(ACELREPYO , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPYO =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPYO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPYO_1,max(RCELREPYO_P,RCELREPYO1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPYO ;

RCELREPYN_1 = max(min(ACELREPYN , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPYN =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPYN_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPYN_1,max(RCELREPYN_P,RCELREPYN1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPYN ;

RCELREPYM_1 = max(min(ACELREPYM , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPYM =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPYM_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPYM_1,max(RCELREPYM_P,RCELREPYM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPYM ;

RCELREPYW_1 = max(min(ACELREPYW , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPYW =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPYW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPYW_1,max(RCELREPYW_P,RCELREPYW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPYW ;

RCELREPYV_1 = max(min(ACELREPYV , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPYV =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPYV_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPYV_1,max(RCELREPYV_P,RCELREPYV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPYV ;

RCELREPYU_1 = max(min(ACELREPYU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPYU =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPYU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPYU_1,max(RCELREPYU_P,RCELREPYU1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPYU ;

RCELREPYT_1 = max(min(ACELREPYT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPYT =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPYT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPYT_1,max(RCELREPYT_P,RCELREPYT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPYT ;

RCELREPWW_1 = max(min(ACELREPWW , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPWW =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPWW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPWW_1,max(RCELREPWW_P,RCELREPWW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPWW ;

RCELREPWV_1 = max(min(ACELREPWV , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPWV =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPWV_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPWV_1,max(RCELREPWV_P,RCELREPWV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPWV ;

RCELREPWU_1 = max(min(ACELREPWU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPWU =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPWU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPWU_1,max(RCELREPWU_P,RCELREPWU1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPWU ;

RCELREPWT_1 = max(min(ACELREPWT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELREPWT =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELREPWT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELREPWT_1,max(RCELREPWT_P,RCELREPWT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELREPWT ;

RCELRU_1 = max(min(ACELRU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRU =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELRU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELRU_1,max(RCELRU_P,RCELRU1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCELRU ;

RCELRT_1 = max(min(ACELRT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELRT =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCELRT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCELRT_1,max(RCELRT_P,RCELRT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = 0 ;

RCELSOM4 = RCELREPYP + RCELREPYO + RCELREPYN + RCELREPYM + RCELREPYW + RCELREPYV + RCELREPYU 
           + RCELREPYT + RCELREPWW + RCELREPWV + RCELREPWU + RCELREPWT + RCELRU + RCELRT ;

RCELSOM4_1 = min(ACELREPYP_1 + ACELREPYO_1 + ACELREPYN_1 + ACELREPYM_1 + ACELREPYW_1 + ACELREPYV_1 + ACELREPYU_1 
                 + ACELREPYT_1 + ACELREPWW_1 + ACELREPWV_1 + ACELREPWU_1 + ACELREPWT_1 + ACELRU_1 + ACELRT_1
	         , IDOM11-(DEC11 + REDUCAVTCEL + RCELSOM1_1 + RCELSOM2_1 + RCELSOM5_1)) ;

regle 401310:
application : iliad ;

ACELXA_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7XA) + (COD7YE * COD7XA)) * (1 - V_CNR) ;
ACELXA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXA_1 , max(ACELXA_P,ACELXA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELXB_1 = (min(LIMREPSC3 * (1 - COD7YE) , COD7XB) + (COD7YE * COD7XB)) * (1 - V_CNR) ;
ACELXB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXB_1 , max(ACELXB_P,ACELXB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELYS_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7YS) + (COD7YE * COD7YS)) * (1 - V_CNR) ;
ACELYS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELYS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELYS_1 , max(ACELYS_P,ACELYS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELXC_1 = (min(LIMREPSC3 * (1 - COD7YE) , COD7XC) + (COD7YE * COD7XC)) * (1 - V_CNR) ;
ACELXC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXC_1 , max(ACELXC_P,ACELXC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELXL_1 = (min(LIMREPSC3 * (1 - COD7YE) , COD7XL) + (COD7YE * COD7XL)) * (1 - V_CNR) ;
ACELXL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXL_1 , max(ACELXL_P,ACELXL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELQE_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7QE) + (COD7YE * COD7QE)) * (1 - V_CNR) ;
ACELQE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELQE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELQE_1 , max(ACELQE_P,ACELQE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


ACELPI_1 = (min(LIMREPSC3 * (1 - COD7YE) , COD7PI) + (COD7YE * COD7PI)) * (1 - V_CNR) ;
ACELPI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELPI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELPI_1 , max(ACELPI_P,ACELPI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELPJ_1 = (min(LIMREPSC3 * (1 - COD7YE) , COD7PJ) + (COD7YE * COD7PJ)) * (1 - V_CNR) ;
ACELPJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELPJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELPJ_1 , max(ACELPJ_P,ACELPJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELXM_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7XM) + (COD7YE * COD7XM)) * (1 - V_CNR) ;
ACELXM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXM_1 , max(ACELXM_P,ACELXM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELXN_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7XN) + (COD7YE * COD7XN)) * (1 - V_CNR) ;
ACELXN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXN_1 , max(ACELXN_P,ACELXN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELYA_1 = (min(LIMREPSC2 * (1 - COD7YE) , COD7YA) + (COD7YE * COD7YA)) * (1 - V_CNR) ;
ACELYA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELYA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELYA_1 , max(ACELYA_P,ACELYA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELYC_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7YC) + (COD7YE * COD7YC)) * (1 - V_CNR) ;
ACELYC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELYC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELYC_1 , max(ACELYC_P,ACELYC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELYG_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7YG) + (COD7YE * COD7YG)) * (1 - V_CNR) ;
ACELYG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELYG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELYG_1 , max(ACELYG_P,ACELYG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELYR_1 = (min(LIMREPSC2 * (1 - COD7YE) , COD7YR) + (COD7YE * COD7YR)) * (1 - V_CNR) ;
ACELYR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELYR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELYR_1 , max(ACELYR_P,ACELYR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAU_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7AU) + (COD7YE * COD7AU)) * (1 - V_CNR) ;
ACELAU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAU_1 , max(ACELAU_P,ACELAU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAB_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7AB) + (COD7YE * COD7AB)) * (1 - V_CNR) ;
ACELAB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAB_1 , max(ACELAB_P,ACELAB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAD_1 = (min(LIMREPSC3 * (1 - COD7YE) , COD7AD) + (COD7YE * COD7AD)) * (1 - V_CNR) ;
ACELAD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAD_1 , max(ACELAD_P,ACELAD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAF_1 = (min(LIMREPSC3 * (1 - COD7YE) , COD7AF) + (COD7YE * COD7AF)) * (1 - V_CNR) ;
ACELAF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAF_1 , max(ACELAF_P,ACELAF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAH_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7AH) + (COD7YE * COD7AH)) * (1 - V_CNR) ;
ACELAH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAH_1 , max(ACELAH_P,ACELAH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAI_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7AI) + (COD7YE * COD7AI)) * (1 - V_CNR) ;
ACELAI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAI_1 , max(ACELAI_1,ACELAI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAP_1 = (min(LIMREPSC2 * (1 - COD7YE) , COD7AP) + (COD7YE * COD7AP)) * (1 - V_CNR) ;
ACELAP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAP_1 , max(ACELAP_P,ACELAP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAR_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7AR) + (COD7YE * COD7AR)) * (1 - V_CNR) ;
ACELAR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAR_1 , max(ACELAR_P,ACELAR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAS_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7AS) + (COD7YE * COD7AS)) * (1 - V_CNR) ;
ACELAS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAS_1 , max(ACELAS_P,ACELAS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELAT_1 = (min(LIMREPSC2 * (1 - COD7YE) , COD7AT) + (COD7YE * COD7AT)) * (1 - V_CNR) ;
ACELAT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELAT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELAT_1 , max(ACELAT_P,ACELAT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

DCELSOM6 = COD7XA + COD7XB + COD7YS + COD7XC + COD7XL + COD7QE + COD7PI + COD7PJ
           + COD7XM + COD7XN + COD7YA + COD7YC + COD7YG + COD7YR 
	   + COD7AU + COD7AB + COD7AD + COD7AF + COD7AH + COD7AI + COD7AP + COD7AR + COD7AS + COD7AT ;

ACELSOM6 = ACELXA + ACELXB + ACELYS + ACELXC + ACELXL + ACELQE + ACELPI + ACELPJ
           + ACELXM + ACELXN + ACELYA + ACELYC + ACELYG + ACELYR 
	   + ACELAU + ACELAB + ACELAD + ACELAF + ACELAH + ACELAI + ACELAP + ACELAR + ACELAS + ACELAT ;

regle 401312:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELSOM1 + RCELSOM2 + RCELSOM5 + RCELSOM4 ;

RCELXB_1 = max(min(ACELXB , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXB_1 , max(RCELXB_P,RCELXB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXB ;

RCELXA_1 = max(min(ACELXA , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXA_1 , max(RCELXA_P,RCELXA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXA ;

RCELXL_1 = max(min(ACELXL , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXL_1 , max(RCELXL_P,RCELXL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXL ;

RCELXC_1 = max(min(ACELXC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXC_1 , max(RCELXC_P,RCELXC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXC ;

RCELYS_1 = max(min(ACELYS , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYS_1 , max(RCELYS_P,RCELYS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYS ;

RCELPJ_1 = max(min(ACELPJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELPJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELPJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELPJ_1 , max(RCELPJ_P,RCELPJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELPJ ;

RCELPI_1 = max(min(ACELPI , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELPI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELPI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELPI_1 , max(RCELPI_P,RCELPI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELPI ;


RCELQE_1 = max(min(ACELQE , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELQE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELQE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELQE_1 , max(RCELQE_P,RCELQE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELQE ;

RCELXM_1 = max(min(ACELXM , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXM_1 , max(RCELXM_P,RCELXM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXM ;

RCELXN_1 = max(min(ACELXN , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXN_1 , max(RCELXN_P,RCELXN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXN ;

RCELYA_1 = max(min(ACELYA , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYA_1 , max(RCELYA_P,RCELYA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYA ;

RCELYC_1 = max(min(ACELYC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYC_1 , max(RCELYC_P,RCELYC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYC ;

RCELYG_1 = max(min(ACELYG , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYG_1 , max(RCELYG_P,RCELYG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYG ;

RCELYR_1 = max(min(ACELYR , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYR_1 , max(RCELYR_P,RCELYR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYR ;

RCELAF_1 = max(min(ACELAF , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAF_1 , max(RCELAF_P,RCELAF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELAF ;

RCELAD_1 = max(min(ACELAD , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAD_1 , max(RCELAD_P,RCELAD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELAD ;

RCELAB_1 = max(min(ACELAB , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAB_1 , max(RCELAB_1,RCELAB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELAB ;

RCELAU_1 = max(min(ACELAU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAU_1 , max(RCELAU_P,RCELAU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELAU ;

RCELAH_1 = max(min(ACELAH , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAH_1 , max(RCELAH_P,RCELAH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELAH ;

RCELAI_1 = max(min(ACELAI , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAI_1 , max(RCELAI_P,RCELAI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELAI ;

RCELAP_1 = max(min(ACELAP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAP_1 , max(RCELAP_P,RCELAP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELAP ;

RCELAR_1 = max(min(ACELAR , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAR_1 , max(RCELAR_P,RCELAR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELAR ;

RCELAS_1 = max(min(ACELAS , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAS_1 , max(RCELAS_P,RCELAS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELAS ;

RCELAT_1 = max(min(ACELAT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELAT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELAT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELAT_1 , max(RCELAT_P,RCELAT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

RCELSOM6 = RCELXA + RCELXB + RCELYS + RCELXC + RCELXL + RCELQE + RCELPI + RCELPJ
           + RCELXM + RCELXN + RCELYA + RCELYC + RCELYG + RCELYR 
	   + RCELAU + RCELAB + RCELAD + RCELAF + RCELAH + RCELAI + RCELAP + RCELAR + RCELAS + RCELAT ;

RCELSOM6_1 = min(ACELXA_1 + ACELXB_1 + ACELYS_1 + ACELXC_1 + ACELXL_1 + ACELQE_1 + ACELPI_1 + ACELPJ_1
                 + ACELXM_1 + ACELXN_1 + ACELYA_1 + ACELYC_1 + ACELYG_1 + ACELYR_1 
	         + ACELAU_1 + ACELAB_1 + ACELAD_1 + ACELAF_1 + ACELAH_1 + ACELAI_1 + ACELAP_1 + ACELAR_1 + ACELAS_1 + ACELAT_1 
                 , IDOM11-(DEC11 + REDUCAVTCEL + RCELSOM1 + RCELSOM2 + RCELSOM5 + RCELSOM4)) ;

regle 401314:
application : iliad ;

ACELZM_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7ZM) + (COD7YE * COD7ZM)) * (1 - V_CNR) ;
ACELZM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELZM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELZM_1 , max(ACELZM_P,ACELZM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELGS_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7GS) + (COD7YE * COD7GS)) * (1 - V_CNR) ;
ACELGS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELGS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELGS_1 , max(ACELGS_P,ACELGS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELGU_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7GU) + (COD7YE * COD7GU)) * (1 - V_CNR) ;
ACELGU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELGU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELGU_1 , max(ACELGU_P,ACELGU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELGX_1 = (min(LIMREPSC2 * (1 - COD7YE) , COD7GX) + (COD7YE * COD7GX)) * (1 - V_CNR) ;
ACELGX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELGX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELGX_1 , max(ACELGX_P,ACELGX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELWX_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7WX) + (COD7YE * COD7WX)) * (1 - V_CNR) ;
ACELWX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELWX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELWX_1 , max(ACELWX_P,ACELWX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELWY_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7WY) + (COD7YE * COD7WY)) * (1 - V_CNR) ;
ACELWY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELWY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELWY_1 , max(ACELWY_P,ACELWY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELWZ_1 = (min(LIMREPSC2 * (1 - COD7YE) , COD7WZ) + (COD7YE * COD7WZ)) * (1 - V_CNR) ;
ACELWZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELWZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELWZ_1 , max(ACELWZ_P,ACELWZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

DCELSOM8 = COD7ZM + COD7GS + COD7GU + COD7GX + COD7WX + COD7WY + COD7WZ ;
ACELSOM8 = ACELZM + ACELGS + ACELGU + ACELGX + ACELWX + ACELWY + ACELWZ ;

regle 402314:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELSOM1 + RCELSOM2 + RCELSOM5 + RCELSOM4 + RCELSOM6 + RCELSOM9 ;

RCELZM_1 = max(min(ACELZM , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELZM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELZM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELZM_1 , max(RCELZM_P,RCELZM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELZM ;

RCELGS_1 = max(min(ACELGS , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELGS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELGS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELGS_1 , max(RCELGS_P,RCELGS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELGS ;

RCELGU_1 = max(min(ACELGU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELGU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELGU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELGU_1 , max(RCELGU_P,RCELGU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELGU ;

RCELGX_1 = max(min(ACELGX , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELGX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELGX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELGX_1 , max(RCELGX_P,RCELGX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELGX ;

RCELWX_1 = max(min(ACELWX , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELWX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELWX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELWX_1 , max(RCELWX_P,RCELWX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELWX ;

RCELWY_1 = max(min(ACELWY , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELWY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELWY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELWY_1 , max(RCELWY_P,RCELWY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELWY ;

RCELWZ_1 = max(min(ACELWZ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELWZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELWZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELWZ_1 , max(RCELWZ_P,RCELWZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

RCELSOM8 = RCELZM + RCELGS + RCELGU + RCELGX + RCELWX + RCELWY + RCELWZ ;
RCELSOM8_1 = ACELZM_1 + ACELGS_1 + ACELGU_1 + ACELGX_1 + ACELWX_1 + ACELWY_1 + ACELWZ_1 ;

regle 401315:
application : iliad ;

ACELHA_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7HA) + (COD7YE * COD7HA)) * (1 - V_CNR) ;
ACELHA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELHA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELHA_1 , max(ACELHA_P,ACELHA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELHJ_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7HJ) + (COD7YE * COD7HJ)) * (1 - V_CNR) ;
ACELHJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELHJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELHJ_1 , max(ACELHJ_P,ACELHJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELHK_1 = (min(LIMREPSC11 * (1 - COD7YE) , COD7HK) + (COD7YE * COD7HK)) * (1 - V_CNR) ;
ACELHK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELHK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELHK_1 , max(ACELHK_P,ACELHK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELHN_1 = (min(LIMREPSC12 * (1 - COD7YE) , COD7HN) + (COD7YE * COD7HN)) * (1 - V_CNR) ;
ACELHN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELHN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELHN_1 , max(ACELHN_P,ACELHN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELHY_1 = (min(LIMREPSC2 * (1 - COD7YE) , COD7HY) + (COD7YE * COD7HY)) * (1 - V_CNR) ;
ACELHY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELHY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELHY_1 , max(ACELHY_P,ACELHY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

DCELSOM9 = COD7HA + COD7HJ + COD7HK + COD7HN + COD7HY ;
ACELSOM9 = ACELHA + ACELHJ + ACELHK + ACELHN + ACELHY ;

regle 402315:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELSOM1 + RCELSOM2 + RCELSOM5 + RCELSOM4 + RCELSOM6 ;

RCELHA_1 = max(min(ACELHA , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELHA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELHA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELHA_1 , max(RCELHA_P,RCELHA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELHA ;

RCELHJ_1 = max(min(ACELHJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELHJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELHJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELHJ_1 , max(RCELHJ_P,RCELHJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELHJ ;

RCELHK_1 = max(min(ACELHK , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELHK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELHK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELHK_1 , max(RCELHK_P,RCELHK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELHK ;

RCELHN_1 = max(min(ACELHN , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELHN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELHN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELHN_1 , max(RCELHN_P,RCELHN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELHN ;

RCELHY_1 = max(min(ACELHY , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELHY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELHY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELHY_1 , max(RCELHY_P,RCELHY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

RCELSOM9 = RCELHA + RCELHJ + RCELHK + RCELHN + RCELHY ;
RCELSOM9_1 = ACELHA_1 + ACELHJ_1 + ACELHK_1 + ACELHN_1 + ACELHY_1 ;

regle 401316:
application : iliad ;

ACELSR_1 = arr((min(COD7SR , LIMCELLIER) * (1 - COD7YE) + COD7SR * COD7YE) /3) * (1 - V_CNR) ;
ACELSR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSR_1 , max(ACELSR_P,ACELSR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELYZ_1 = arr((min(COD7YZ , LIMCELLIER) * (1 - COD7YE) + COD7YZ * COD7YE) /3) * (1 - V_CNR) ;
ACELYZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELYZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELYZ_1 , max(ACELYZ_P,ACELYZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSL_1 = arr((min(COD7SL , LIMCELLIER) * (1 - COD7YE) + COD7SL * COD7YE) /3) * (1 - V_CNR) ;
ACELSL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSL_1 , max(ACELSL_P,ACELSL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSQ_1 = arr((min(COD7SQ , LIMCELLIER) * (1 - COD7YE) + COD7SQ * COD7YE) /3) * (1 - V_CNR) ;
ACELSQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSQ_1 , max(ACELSQ_P,ACELSQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELYX_1 = arr((min(COD7YX , LIMCELLIER) * (1 - COD7YE) + COD7YX * COD7YE) /3) * (1 - V_CNR) ;
ACELYX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELYX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELYX_1 , max(ACELYX_P,ACELYX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELYY_1 = arr((min(COD7YY , LIMCELLIER) * (1 - COD7YE) + COD7YY * COD7YE) /3) * (1 - V_CNR) ;
ACELYY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELYY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELYY_1 , max(ACELYY_P,ACELYY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSH_1 = arr((min(COD7SH , LIMCELLIER) * (1 - COD7YE) + COD7SH * COD7YE) /3) * (1 - V_CNR) ;
ACELSH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSH_1 , max(ACELSH_P,ACELSH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSI_1 = arr((min(COD7SI , LIMCELLIER) * (1 - COD7YE) + COD7SI * COD7YE) /3) * (1 - V_CNR) ;
ACELSI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSI_1 , max(ACELSI_P,ACELSI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSJ_1 = arr((min(COD7SJ , LIMCELLIER) * (1 - COD7YE) + COD7SJ * COD7YE) /3) * (1 - V_CNR) ;
ACELSJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSJ_1 , max(ACELSJ_P,ACELSJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSK_1 = arr((min(COD7SK , LIMCELLIER) * (1 - COD7YE) + COD7SK * COD7YE) /3) * (1 - V_CNR) ;
ACELSK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSK_1 , max(ACELSK_P,ACELSK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELXH_1 = arr((min(COD7XH , LIMCELLIER) * (1 - COD7YE) + COD7XH * COD7YE) /3) * (1 - V_CNR) ;
ACELXH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXH_1 , max(ACELXH_P,ACELXH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELXI_1 = arr((min(COD7XI , LIMCELLIER) * (1 - COD7YE) + COD7XI * COD7YE) /3) * (1 - V_CNR) ;
ACELXI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXI_1 , max(ACELXI_P,ACELXI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELXJ_1 = arr((min(COD7XJ , LIMCELLIER) * (1 - COD7YE) + COD7XJ * COD7YE) /3) * (1 - V_CNR) ;
ACELXJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXJ_1 , max(ACELXJ_P,ACELXJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELXK_1 = arr((min(COD7XK , LIMCELLIER) * (1 - COD7YE) + COD7XK * COD7YE) /3) * (1 - V_CNR) ;
ACELXK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELXK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELXK_1 , max(ACELXK_P,ACELXK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELIA_1 = arr((min(COD7IA , LIMCELLIER) * (1 - COD7YE) + COD7IA * COD7YE) /3) * (1 - V_CNR) ;
ACELIA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIA_1 , max(ACELIA_P,ACELIA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELIB_1 = arr((min(COD7IB , LIMCELLIER) * (1 - COD7YE) + COD7IB * COD7YE) /3) * (1 - V_CNR) ;
ACELIB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIB_1 , max(ACELIB_P,ACELIB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELIC_1 = arr((min(COD7IC , LIMCELLIER) * (1 - COD7YE) + COD7IC * COD7YE) /3) * (1 - V_CNR) ;
ACELIC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIC_1 , max(ACELIC_P,ACELIC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELIE_1 = arr((min(COD7IE , LIMCELLIER) * (1 - COD7YE) + COD7IE * COD7YE) /3) * (1 - V_CNR) ;
ACELIE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIE_1 , max(ACELIE_P,ACELIE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELKJ_1 = arr((min(COD7KJ , LIMCELLIER) * (1 - COD7YE) + COD7KJ * COD7YE) /3) * (1 - V_CNR) ;
ACELKJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELKJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELKJ_1 , max(ACELKJ_P,ACELKJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


ACELKL_1 = arr((min(COD7KL , LIMCELLIER) * (1 - COD7YE) + COD7KL * COD7YE) /3) * (1 - V_CNR) ;
ACELKL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELKL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELKL_1 , max(ACELKL_P,ACELKL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELKN_1 = arr((min(COD7KN , LIMCELLIER) * (1 - COD7YE) + COD7KN * COD7YE) /3) * (1 - V_CNR) ;
ACELKN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELKN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELKN_1 , max(ACELKN_P,ACELKN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSD_1 = arr((min(COD7SD , LIMCELLIER) * (1 - COD7YE) + COD7SD * COD7YE) /3) * (1 - V_CNR) ;
ACELSD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSD_1 , max(ACELSD_P,ACELSD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSE_1 = arr((min(COD7SE , LIMCELLIER) * (1 - COD7YE) + COD7SE * COD7YE) /3) * (1 - V_CNR) ;
ACELSE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSE_1 , max(ACELSE_P,ACELSE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSF_1 = arr((min(COD7SF , LIMCELLIER) * (1 - COD7YE) + COD7SF * COD7YE) /3) * (1 - V_CNR) ;
ACELSF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSF_1 , max(ACELSF_P,ACELSF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELSG_1 = arr((min(COD7SG , LIMCELLIER) * (1 - COD7YE) + COD7SG * COD7YE) /3) * (1 - V_CNR) ;
ACELSG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELSG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELSG_1 , max(ACELSG_P,ACELSG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELWD_1 = arr((min(COD7WD , LIMCELLIER) * (1 - COD7YE) + COD7WD * COD7YE) /3) * (1 - V_CNR) ;
ACELWD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELWD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELWD_1 , max(ACELWD_P,ACELWD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELWE_1 = arr((min(COD7WE , LIMCELLIER) * (1 - COD7YE) + COD7WE * COD7YE) /3) * (1 - V_CNR) ;
ACELWE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELWE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELWE_1 , max(ACELWE_P,ACELWE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELWF_1 = arr((min(COD7WF , LIMCELLIER) * (1 - COD7YE) + COD7WF * COD7YE) /3) * (1 - V_CNR) ;
ACELWF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELWF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELWF_1 , max(ACELWF_P,ACELWF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELWG_1 = arr((min(COD7WG , LIMCELLIER) * (1 - COD7YE) + COD7WG * COD7YE) /3) * (1 - V_CNR) ;
ACELWG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELWG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELWG_1 , max(ACELWG_P,ACELWG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELIF_1 = arr((min(COD7IF , LIMCELLIER) * (1 - COD7YE) + COD7IF * COD7YE) /3) * (1 - V_CNR) ;
ACELIF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIF_1 , max(ACELIF_P,ACELIF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELIG_1 = arr((min(COD7IG , LIMCELLIER) * (1 - COD7YE) + COD7IG * COD7YE) /3) * (1 - V_CNR) ;
ACELIG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIG_1 , max(ACELIG_P,ACELIG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELIH_1 = arr((min(COD7IH , LIMCELLIER) * (1 - COD7YE) + COD7IH * COD7YE) /3) * (1 - V_CNR) ;
ACELIH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIH_1 , max(ACELIH_P,ACELIH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELIO_1 = arr((min(COD7IO , LIMCELLIER) * (1 - COD7YE) + COD7IO * COD7YE) /3) * (1 - V_CNR) ;
ACELIO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIO_1 , max(ACELIO_P,ACELIO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELKO_1 = arr((min(COD7KO , LIMCELLIER) * (1 - COD7YE) + COD7KO * COD7YE) /3) * (1 - V_CNR) ;
ACELKO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELKO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELKO_1 , max(ACELKO_P,ACELKO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


ACELKQ_1 = arr((min(COD7KQ , LIMCELLIER) * (1 - COD7YE) + COD7KQ * COD7YE) /3) * (1 - V_CNR) ;
ACELKQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELKQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELKQ_1 , max(ACELKQ_P,ACELKQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELKR_1 = arr((min(COD7KR , LIMCELLIER) * (1 - COD7YE) + COD7KR * COD7YE) /3) * (1 - V_CNR) ;
ACELKR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELKR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELKR_1 , max(ACELKR_P,ACELKR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELIP_1 = arr((min(COD7IP , LIMCELLIER) * (1 - COD7YE) + COD7IP * COD7YE) /3) * (1 - V_CNR) ;
ACELIP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIP_1 , max(ACELIP_P,ACELIP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELWC_1 = arr((min(COD7WC , LIMCELLIER) * (1 - COD7YE) + COD7WC * COD7YE) /3) * (1 - V_CNR) ;
ACELWC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELWC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELWC_1 , max(ACELWC_P,ACELWC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELKS_1 = arr((min(COD7KS , LIMCELLIER) * (1 - COD7YE) + COD7KS * COD7YE) /3) * (1 - V_CNR) ;
ACELKS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELKS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELKS_1 , max(ACELKS_P,ACELKS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


RCEL7SR = arr(ACELSR * (TX06/100)) ;
RCEL7SR_1 = arr(ACELSR_1 * (TX06/100)) ;

RCEL7YZ = arr(ACELYZ * (TX06/100)) ;
RCEL7YZ_1 = arr(ACELYZ_1 * (TX06/100)) ;

RCEL7SL = arr(ACELSL * (TX06/100)) ;
RCEL7SL_1 = arr(ACELSL_1 * (TX06/100)) ;

RCEL7SQ = arr(ACELSQ * (TX06/100)) ;
RCEL7SQ_1 = arr(ACELSQ_1 * (TX06/100)) ;

RCEL7YX = arr(ACELYX * (TX06/100)) ;
RCEL7YX_1 = arr(ACELYX_1 * (TX06/100)) ;

RCEL7YY = arr(ACELYY * (TX06/100)) ;
RCEL7YY_1 = arr(ACELYY_1 * (TX06/100)) ;

RCEL7SH = arr(ACELSH * (TX05/100)) ;
RCEL7SH_1 = arr(ACELSH_1 * (TX05/100)) ;

RCEL7SI = arr(ACELSI * (TX06/100)) ;
RCEL7SI_1 = arr(ACELSI_1 * (TX06/100)) ;

RCEL7SJ = arr(ACELSJ * (TX06/100)) ;
RCEL7SJ_1 = arr(ACELSJ_1 * (TX06/100)) ;

RCEL7SK = arr(ACELSK * (TX05/100)) ;
RCEL7SK_1 = arr(ACELSK_1 * (TX05/100)) ;

RCEL7XH = arr(ACELXH * (TX05/100)) ;
RCEL7XH_1 = arr(ACELXH_1 * (TX05/100)) ;

RCEL7XI = arr(ACELXI * (TX06/100)) ;
RCEL7XI_1 = arr(ACELXI_1 * (TX06/100)) ;

RCEL7XJ = arr(ACELXJ * (TX06/100)) ;
RCEL7XJ_1 = arr(ACELXJ_1 * (TX06/100)) ;

RCEL7XK = arr(ACELXK * (TX05/100)) ;
RCEL7XK_1 = arr(ACELXK_1 * (TX05/100)) ;

RCEL7IA = arr(ACELIA * (TX05/100)) ;
RCEL7IA_1 = arr(ACELIA_1 * (TX05/100)) ;

RCEL7IB = arr(ACELIB * (TX06/100)) ;
RCEL7IB_1 = arr(ACELIB_1 * (TX06/100)) ;

RCEL7IC = arr(ACELIC * (TX06/100)) ;
RCEL7IC_1 = arr(ACELIC_1 * (TX06/100)) ;

RCEL7IE = arr(ACELIE * (TX05/100)) ;
RCEL7IE_1 = arr(ACELIE_1 * (TX05/100)) ;

RCEL7KJ = arr(ACELKJ * (TX05/100)) ;
RCEL7KJ_1 = arr(ACELKJ_1 * (TX05/100)) ;


RCEL7KL = arr(ACELKL * (TX06/100)) ;
RCEL7KL_1 = arr(ACELKL_1 * (TX06/100)) ;

RCEL7KN = arr(ACELKN * (TX05/100)) ;
RCEL7KN_1 = arr(ACELKN_1 * (TX05/100)) ;


RCEL7SD = arr(ACELSD * (TX04/100)) ;
RCEL7SD_1 = arr(ACELSD_1 * (TX04/100)) ;

RCEL7SE = arr(ACELSE * (TX05/100)) ;
RCEL7SE_1 = arr(ACELSE_1 * (TX05/100)) ;

RCEL7SF = arr(ACELSF * (TX05/100)) ;
RCEL7SF_1 = arr(ACELSF_1 * (TX05/100)) ;

RCEL7SG = arr(ACELSG * (TX04/100)) ;
RCEL7SG_1 = arr(ACELSG_1 * (TX04/100)) ;

RCEL7WD = arr(ACELWD * (TX04/100)) ;
RCEL7WD_1 = arr(ACELWD_1 * (TX04/100)) ;

RCEL7WE = arr(ACELWE * (TX05/100)) ;
RCEL7WE_1 = arr(ACELWE_1 * (TX05/100)) ;

RCEL7WF = arr(ACELWF * (TX05/100)) ;
RCEL7WF_1 = arr(ACELWF_1 * (TX05/100)) ;

RCEL7WG = arr(ACELWG * (TX04/100)) ;
RCEL7WG_1 = arr(ACELWG_1 * (TX04/100)) ;

RCEL7IF = arr(ACELIF * (TX04/100)) ;
RCEL7IF_1 = arr(ACELIF_1 * (TX04/100)) ;

RCEL7IG = arr(ACELIG * (TX05/100)) ;
RCEL7IG_1 = arr(ACELIG_1 * (TX05/100)) ;

RCEL7IH = arr(ACELIH * (TX05/100)) ;
RCEL7IH_1 = arr(ACELIH_1 * (TX05/100)) ;

RCEL7IO = arr(ACELIO * (TX04/100)) ;
RCEL7IO_1 = arr(ACELIO_1 * (TX04/100)) ;

RCEL7KO = arr(ACELKO * (TX04/100)) ;
RCEL7KO_1 = arr(ACELKO_1 * (TX04/100)) ;


RCEL7KQ = arr(ACELKQ * (TX05/100)) ;
RCEL7KQ_1 = arr(ACELKQ_1 * (TX05/100)) ;

RCEL7KR = arr(ACELKR * (TX04/100)) ;
RCEL7KR_1 = arr(ACELKR_1 * (TX04/100)) ;


RCEL7IP = arr(ACELIP * (TX04/100)) ;
RCEL7IP_1 = arr(ACELIP_1 * (TX04/100)) ;

RCEL7WC = arr(ACELWC * (TX04/100)) ;
RCEL7WC_1 = arr(ACELWC_1 * (TX04/100)) ;

RCEL7KS = arr(ACELKS * (TX04/100)) ;
RCEL7KS_1 = arr(ACELKS_1 * (TX04/100)) ;

regle 401318:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELSOM1 + RCELSOM2 + RCELSOM5 + RCELSOM4 + RCELSOM6 + RCELSOM8 + RCELSOM9 ;

RCELSR_1 = max(min(RCEL7SR , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSR_1 , max(RCELSR_P,RCELSR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSR ;

RCELYZ_1 = max(min(RCEL7YZ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYZ_1 , max(RCELYZ_P,RCELYZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYZ ;

RCELSL_1 = max(min(RCEL7SL , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSL_1 , max(RCELSL_P,RCELSL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSL ;

RCELSQ_1 = max(min(RCEL7SQ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSQ_1 , max(RCELSQ_P,RCELSQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSQ ;

RCELYX_1 = max(min(RCEL7YX , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYX_1 , max(RCELYX_P,RCELYX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYX ;

RCELYY_1 = max(min(RCEL7YY , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELYY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELYY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELYY_1 , max(RCELYY_P,RCELYY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELYY ;

RCELSH_1 = max(min(RCEL7SH , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSH_1 , max(RCELSH_P,RCELSH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSH ;

RCELSI_1 = max(min(RCEL7SI , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSI_1 , max(RCELSI_P,RCELSI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSI ;

RCELSJ_1 = max(min(RCEL7SJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSJ_1 , max(RCELSJ_P,RCELSJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSJ ;

RCELSK_1 = max(min(RCEL7SK , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSK_1 , max(RCELSK_P,RCELSK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSK ;

RCELXH_1 = max(min(RCEL7XH , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXH_1 , max(RCELXH_P,RCELXH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXH ;

RCELXI_1 = max(min(RCEL7XI , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXI_1 , max(RCELXI_P,RCELXI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXI ;

RCELXJ_1 = max(min(RCEL7XJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXJ_1 , max(RCELXJ_P,RCELXJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXJ ;

RCELXK_1 = max(min(RCEL7XK , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELXK = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELXK_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELXK_1 , max(RCELXK_P,RCELXK1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELXK ;

RCELIA_1 = max(min(RCEL7IA , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELIA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIA_1 , max(RCELIA_P,RCELIA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIA ;

RCELIB_1 = max(min(RCEL7IB , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELIB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIB_1 , max(RCELIB_P,RCELIB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIB ;

RCELIC_1 = max(min(RCEL7IC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELIC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIC_1 , max(RCELIC_P,RCELIC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIC ;

RCELIE_1 = max(min(RCEL7IE , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELIE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIE_1 , max(RCELIE_P,RCELIE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIE ;

RCELKJ_1 = max(min(RCEL7KJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKJ_1 , max(RCELKJ_P,RCELKJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKJ ;


RCELKL_1 = max(min(RCEL7KL , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKL_1 , max(RCELKL_P,RCELKL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKL ;

RCELKN_1 = max(min(RCEL7KN , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKN_1 , max(RCELKN_P,RCELKN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKN ;

RCELSD_1 = max(min(RCEL7SD , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSD_1 , max(RCELSD_P,RCELSD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSD ;

RCELSE_1 = max(min(RCEL7SE , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSE_1 , max(RCELSE_P,RCELSE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSE ;

RCELSF_1 = max(min(RCEL7SF , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSF_1 , max(RCELSF_P,RCELSF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSF ;

RCELSG_1 = max(min(RCEL7SG , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELSG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELSG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELSG_1 , max(RCELSG_P,RCELSG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELSG ;

RCELWD_1 = max(min(RCEL7WD , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELWD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELWD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELWD_1 , max(RCELWD_P,RCELWD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELWD ;

RCELWE_1 = max(min(RCEL7WE , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELWE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELWE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELWE_1 , max(RCELWE_P,RCELWE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELWE ;

RCELWF_1 = max(min(RCEL7WF , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELWF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELWF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELWF_1 , max(RCELWF_P,RCELWF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELWF ;

RCELWG_1 = max(min(RCEL7WG , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELWG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELWG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELWG_1 , max(RCELWG_P,RCELWG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELWG ;

RCELIF_1 = max(min(RCEL7IF , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELIF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIF_1 , max(RCELIF_P,RCELIF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIF ;

RCELIG_1 = max(min(RCEL7IG , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELIG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIG_1 , max(RCELIG_P,RCELIG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIG ;

RCELIH_1 = max(min(RCEL7IH , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELIH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIH_1 , max(RCELIH_P,RCELIH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIH ;

RCELIO_1 = max(min(RCEL7IO , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELIO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIO_1 , max(RCELIO_P,RCELIO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIO ;

RCELKO_1 = max(min(RCEL7KO , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKO_1 , max(RCELKO_P,RCELKO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKO ;


RCELKQ_1 = max(min(RCEL7KQ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKQ_1 , max(RCELKQ_P,RCELKQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKQ ;

RCELKR_1 = max(min(RCEL7KR , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKR_1 , max(RCELKR_P,RCELKR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELKR ;

RCELIP_1 = max(min(RCEL7IP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELIP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIP_1 , max(RCELIP_P,RCELIP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIP ;

RCELWC_1 = max(min(RCEL7WC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELWC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELWC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELWC_1 , max(RCELWC_P,RCELWC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELWC ;

RCELKS_1 = max(min(RCEL7KS , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RCELKS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELKS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELKS_1 , max(RCELKS_P,RCELKS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

DCELSOM7 = COD7SR + COD7YZ + COD7SL + COD7SQ + COD7YX + COD7YY + COD7SH + COD7SI + COD7SJ + COD7SK + COD7XH + COD7XI + COD7XJ + COD7XK 
           + COD7IA + COD7IB + COD7IC + COD7IE + COD7KJ + COD7KL + COD7KN + COD7SD + COD7SE + COD7SF + COD7SG + COD7WD + COD7WE 
	   + COD7WF + COD7WG + COD7IF + COD7IG + COD7IH + COD7IO + COD7KO + COD7KQ + COD7KR + COD7IP + COD7WC + COD7KS ;

ACELSOM7 = ACELSR + ACELYZ + ACELSL + ACELSQ + ACELYX + ACELYY + ACELSH + ACELSI + ACELSJ + ACELSK + ACELXH + ACELXI + ACELXJ + ACELXK 
           + ACELIA + ACELIB + ACELIC + ACELIE + ACELKJ + ACELKL + ACELKN + ACELSD + ACELSE + ACELSF + ACELSG + ACELWD + ACELWE 
	   + ACELWF + ACELWG + ACELIF + ACELIG + ACELIH + ACELIO + ACELKO + ACELKQ + ACELKR + ACELIP + ACELWC + ACELKS ;

RCELSOM7 = RCELSR + RCELYZ + RCELSL + RCELSQ + RCELYX + RCELYY + RCELSH + RCELSI + RCELSJ + RCELSK + RCELXH + RCELXI + RCELXJ + RCELXK 
           + RCELIA + RCELIB + RCELIC + RCELIE + RCELKJ + RCELKL + RCELKN + RCELSD + RCELSE + RCELSF + RCELSG + RCELWD + RCELWE 
	   + RCELWF + RCELWG + RCELIF + RCELIG + RCELIH + RCELIO + RCELKO + RCELKQ + RCELKR + RCELIP + RCELWC + RCELKS ;

RCELSOM7_1 = min(RCEL7SR_1 + RCEL7YZ_1 + RCEL7SL_1 + RCEL7SQ_1 + RCEL7YX_1 + RCEL7YY_1 + RCEL7SH_1 + RCEL7SI_1 + RCEL7SJ_1 + RCEL7SK_1 + RCEL7XH_1 + RCEL7XI_1 + RCEL7XJ_1 + RCEL7XK_1
                 + RCEL7IA_1 + RCEL7IB_1 + RCEL7IC_1 + RCEL7IE_1 + RCEL7KJ_1 + RCEL7KL_1 + RCEL7KN_1 + RCEL7SD_1 + RCEL7SE_1 + RCEL7SF_1 + RCEL7SG_1 + RCEL7WD_1 + RCEL7WE_1
	         + RCEL7WF_1 + RCEL7WG_1 + RCEL7IF_1 + RCEL7IG_1 + RCEL7IH_1 + RCEL7IO_1 + RCEL7KO_1 + RCEL7KQ_1 + RCEL7KR_1 + RCEL7IP_1 + RCEL7WC_1 + RCEL7KS_1
                 , IDOM11-(DEC11 + REDUCAVTCEL + RCELSOM1_1 + RCELSOM2_1 + RCELSOM5_1 + RCELSOM4_1 + RCELSOM6_1 + RCELSOM8_1 + RCELSOM9_1)) ;

regle 401320:
application : iliad ;

ACELIQ_1 = arr((min(COD7IQ , LIMCELLIER) * (1 - COD7YE) + COD7IQ * COD7YE) /3) * (1 - V_CNR) ;
ACELIQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELIQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELIQ_1 , max(ACELIQ_P,ACELIQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELHL_1 = arr((min(COD7HL , LIMCELLIER) * (1 - COD7YE) + COD7HL * COD7YE) /3) * (1 - V_CNR) ;
ACELHL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELHL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELHL_1 , max(ACELHL_P,ACELHL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELHM_1 = arr((min(COD7HM , LIMCELLIER) * (1 - COD7YE) + COD7HM * COD7YE) /3) * (1 - V_CNR) ;
ACELHM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELHM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELHM_1 , max(ACELHM_P,ACELHM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELLD_1 = arr((min(COD7LD , LIMCELLIER) * (1 - COD7YE) + COD7LD * COD7YE) /3) * (1 - V_CNR) ;
ACELLD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELLD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELLD_1 , max(ACELLD_P,ACELLD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELLE_1 = arr((min(COD7LE , LIMCELLIER) * (1 - COD7YE) + COD7LE * COD7YE) /3) * (1 - V_CNR) ;
ACELLE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELLE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELLE_1 , max(ACELLE_P,ACELLE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELLF_1 = arr((min(COD7LF , LIMCELLIER) * (1 - COD7YE) + COD7LF * COD7YE) /3) * (1 - V_CNR) ;
ACELLF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELLF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELLF_1 , max(ACELLF_P,ACELLF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELLN_1 = arr((min(COD7LN , LIMCELLIER) * (1 - COD7YE) + COD7LN * COD7YE) /3) * (1 - V_CNR) ;
ACELLN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELLN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELLN_1 , max(ACELLN_P,ACELLN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELBA_1 = arr((min(COD7BA , LIMCELLIER) * (1 - COD7YE) + COD7BA * COD7YE) /3) * (1 - V_CNR) ;
ACELBA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELBA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELBA_1 , max(ACELBA_P,ACELBA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELBB_1 = arr((min(COD7BB , LIMCELLIER) * (1 - COD7YE) + COD7BB * COD7YE) /3) * (1 - V_CNR) ;
ACELBB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELBB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELBB_1 , max(ACELBB_P,ACELBB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELBC_1 = arr((min(COD7BC , LIMCELLIER) * (1 - COD7YE) + COD7BC * COD7YE) /3) * (1 - V_CNR) ;
ACELBC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELBC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELBC_1 , max(ACELBC_P,ACELBC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELBD_1 = arr((min(COD7BD , LIMCELLIER) * (1 - COD7YE) + COD7BD * COD7YE) /3) * (1 - V_CNR) ;
ACELBD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELBD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELBD_1 , max(ACELBD_P,ACELBD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELLT_1 = arr((min(COD7LT , LIMCELLIER) * (1 - COD7YE) + COD7LT * COD7YE) /3) * (1 - V_CNR) ;
ACELLT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELLT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELLT_1 , max(ACELLT_P,ACELLT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELLX_1 = arr((min(COD7LX , LIMCELLIER) * (1 - COD7YE) + COD7LX * COD7YE) /3) * (1 - V_CNR) ;
ACELLX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELLX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELLX_1 , max(ACELLX_P,ACELLX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELLZ_1 = arr((min(COD7LZ , LIMCELLIER) * (1 - COD7YE) + COD7LZ * COD7YE) /3) * (1 - V_CNR) ;
ACELLZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELLZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELLZ_1 , max(ACELLZ_P,ACELLZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELMG_1 = arr((min(COD7MG , LIMCELLIER) * (1 - COD7YE) + COD7MG * COD7YE) /3) * (1 - V_CNR) ;
ACELMG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELMG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELMG_1 , max(ACELMG_P,ACELMG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELBE_1 = arr((min(COD7BE , LIMCELLIER) * (1 - COD7YE) + COD7BE * COD7YE) /3) * (1 - V_CNR) ;
ACELBE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELBE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELBE_1 , max(ACELBE_P,ACELBE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELBF_1 = arr((min(COD7BF , LIMCELLIER) * (1 - COD7YE) + COD7BF * COD7YE) /3) * (1 - V_CNR) ;
ACELBF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELBF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELBF_1 , max(ACELBF_P,ACELBF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELBG_1 = arr((min(COD7BG , LIMCELLIER) * (1 - COD7YE) + COD7BG * COD7YE) /3) * (1 - V_CNR) ;
ACELBG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELBG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELBG_1 , max(ACELBG_P,ACELBG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELBH_1 = arr((min(COD7BH , LIMCELLIER) * (1 - COD7YE) + COD7BH * COD7YE) /3) * (1 - V_CNR) ;
ACELBH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELBH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELBH_1 , max(ACELBH_P,ACELBH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELMH_1 = arr((min(COD7MH , LIMCELLIER) * (1 - COD7YE) + COD7MH * COD7YE) /3) * (1 - V_CNR) ;
ACELMH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELMH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELMH_1 , max(ACELMH_P,ACELMH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ACELBJ_1 = arr((min(COD7BJ , LIMCELLIER) * (1 - COD7YE) + COD7BJ * COD7YE) /3) * (1 - V_CNR) ;
ACELBJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ACELBJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(ACELBJ_1 , max(ACELBJ_P,ACELBJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


RCEL7IQ = arr(ACELIQ * (TX06/100)) ;
RCEL7IQ_1 = arr(ACELIQ_1 * (TX06/100)) ;

RCEL7HL = arr(ACELHL * (TX06/100)) ;
RCEL7HL_1 = arr(ACELHL_1 * (TX06/100)) ;

RCEL7HM = arr(ACELHM * (TX06/100)) ;
RCEL7HM_1 = arr(ACELHM_1 * (TX06/100)) ;

RCEL7LD = arr(ACELLD * (TX05/100)) ;
RCEL7LD_1 = arr(ACELLD_1 * (TX05/100)) ;

RCEL7LE = arr(ACELLE * (TX06/100)) ;
RCEL7LE_1 = arr(ACELLE_1 * (TX06/100)) ;

RCEL7LF = arr(ACELLF * (TX06/100)) ;
RCEL7LF_1 = arr(ACELLF_1 * (TX06/100)) ;

RCEL7LN = arr(ACELLN * (TX05/100)) ;
RCEL7LN_1 = arr(ACELLN_1 * (TX05/100)) ;

RCEL7BA = arr(ACELBA * (TX05/100)) ;
RCEL7BA_1 = arr(ACELBA_1 * (TX05/100)) ;

RCEL7BB = arr(ACELBB * (TX06/100)) ;
RCEL7BB_1 = arr(ACELBB_1 * (TX06/100)) ;

RCEL7BC = arr(ACELBC * (TX06/100)) ;
RCEL7BC_1 = arr(ACELBC_1 * (TX06/100)) ;

RCEL7BD = arr(ACELBD * (TX05/100)) ;
RCEL7BD_1 = arr(ACELBD_1 * (TX05/100)) ;

RCEL7LT = arr(ACELLT * (TX04/100)) ;
RCEL7LT_1 = arr(ACELLT_1 * (TX04/100)) ;

RCEL7LX = arr(ACELLX * (TX05/100)) ;
RCEL7LX_1 = arr(ACELLX_1 * (TX05/100)) ;

RCEL7LZ = arr(ACELLZ * (TX05/100)) ;
RCEL7LZ_1 = arr(ACELLZ_1 * (TX05/100)) ;

RCEL7MG = arr(ACELMG * (TX04/100)) ;
RCEL7MG_1 = arr(ACELMG_1 * (TX04/100)) ;

RCEL7BE = arr(ACELBE * (TX04/100)) ;
RCEL7BE_1 = arr(ACELBE_1 * (TX04/100)) ;

RCEL7BF = arr(ACELBF * (TX05/100)) ;
RCEL7BF_1 = arr(ACELBF_1 * (TX05/100)) ;

RCEL7BG = arr(ACELBG * (TX05/100)) ;
RCEL7BG_1 = arr(ACELBG_1 * (TX05/100)) ;

RCEL7BH = arr(ACELBH * (TX04/100)) ;
RCEL7BH_1 = arr(ACELBH_1 * (TX04/100)) ;

RCEL7MH = arr(ACELMH * (TX04/100)) ;
RCEL7MH_1 = arr(ACELMH_1 * (TX04/100)) ;

RCEL7BJ = arr(ACELBJ * (TX04/100)) ;
RCEL7BJ_1 = arr(ACELBJ_1 * (TX04/100)) ;

regle 401322:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELSOM1 + RCELSOM2 + RCELSOM5 + RCELSOM4 + RCELSOM6 + RCELSOM8 + RCELSOM9 + RCELSOM7 ;

RCELIQ_1 = max(min(RCEL7IQ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELIQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELIQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELIQ_1 , max(RCELIQ_P,RCELIQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELIQ ;

RCELHL_1 = max(min(RCEL7HL , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELHL = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELHL_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELHL_1 , max(RCELHL_P,RCELHL1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELHL ;

RCELHM_1 = max(min(RCEL7HM , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELHM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELHM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELHM_1 , max(RCELHM_P,RCELHM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELHM ;

RCELLD_1 = max(min(RCEL7LD , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELLD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLD_1 , max(RCELLD_P,RCELLD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLD ;

RCELLE_1 = max(min(RCEL7LE , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELLE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLE_1 , max(RCELLE_P,RCELLE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLE ;

RCELLF_1 = max(min(RCEL7LF , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELLF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLF_1 , max(RCELLF_P,RCELLF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLF ;

RCELLN_1 = max(min(RCEL7LN , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELLN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLN_1 , max(RCELLN_P,RCELLN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLN ;

RCELBA_1 = max(min(RCEL7BA , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELBA = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELBA_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELBA_1 , max(RCELBA_P,RCELBA1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELBA ;

RCELBB_1 = max(min(RCEL7BB , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELBB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELBB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELBB_1 , max(RCELBB_P,RCELBB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELBB ;

RCELBC_1 = max(min(RCEL7BC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELBC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELBC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELBC_1 , max(RCELBC_P,RCELBC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELBC ;

RCELBD_1 = max(min(RCEL7BD , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELBD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELBD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELBD_1 , max(RCELBD_P,RCELBD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELBD ;

RCELLT_1 = max(min(RCEL7LT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELLT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLT_1 , max(RCELLT_P,RCELLT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLT ;

RCELLX_1 = max(min(RCEL7LX , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELLX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLX_1 , max(RCELLX_P,RCELLX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLX ;

RCELLZ_1 = max(min(RCEL7LZ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELLZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELLZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELLZ_1 , max(RCELLZ_P,RCELLZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELLZ ;

RCELMG_1 = max(min(RCEL7MG , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELMG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMG_1 , max(RCELMG_P,RCELMG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMG ;

RCELBE_1 = max(min(RCEL7BE , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELBE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELBE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELBE_1 , max(RCELBE_P,RCELBE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELBE ;

RCELBF_1 = max(min(RCEL7BF , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELBF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELBF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELBF_1 , max(RCELBF_P,RCELBF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELBF ;

RCELBG_1 = max(min(RCEL7BG , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELBG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELBG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELBG_1 , max(RCELBG_P,RCELBG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELBG ;

RCELBH_1 = max(min(RCEL7BH , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELBH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELBH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELBH_1 , max(RCELBH_P,RCELBH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELBH ;

RCELMH_1 = max(min(RCEL7MH , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELMH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELMH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELMH_1 , max(RCELMH_P,RCELMH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCELMH ;

RCELBJ_1 = max(min(RCEL7BJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ; 
RCELBJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCELBJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCELBJ_1 , max(RCELBJ_P,RCELBJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

DCELSOM3 = COD7IQ + COD7HL + COD7HM + COD7LD + COD7LE + COD7LF + COD7LN + COD7BA + COD7BB + COD7BC + COD7BD 
           + COD7LT + COD7LX + COD7LZ + COD7MG + COD7BE + COD7BF + COD7BG + COD7BH + COD7MH + COD7BJ ;

ACELSOM3 = ACELIQ + ACELHL + ACELHM + ACELLD + ACELLE + ACELLF + ACELLN + ACELBA + ACELBB + ACELBC + ACELBD 
           + ACELLT + ACELLX + ACELLZ + ACELMG + ACELBE + ACELBF + ACELBG + ACELBH + ACELMH + ACELBJ ;

RCELSOM3 = RCELIQ + RCELHL + RCELHM + RCELLD + RCELLE + RCELLF + RCELLN + RCELBA + RCELBB + RCELBC + RCELBD
           + RCELLT + RCELLX + RCELLZ + RCELMG + RCELBE + RCELBF + RCELBG + RCELBH + RCELMH + RCELBJ ;

RCELSOM3_1 = min(RCEL7IQ_1 + RCEL7HL_1 + RCEL7HM_1 + RCEL7LD_1 + RCEL7LE_1 + RCEL7LF_1 + RCEL7LN_1 + RCEL7BA_1 + RCEL7BB_1 + RCEL7BC_1 + RCEL7BD_1
                 + RCEL7LT_1 + RCEL7LX_1 + RCEL7LZ_1 + RCEL7MG_1 + RCEL7BE_1 + RCEL7BF_1 + RCEL7BG_1 + RCEL7BH_1 + RCEL7MH_1 + RCEL7BJ_1
                 , IDOM11 - ( DEC11 + REDUCAVTCEL + RCELSOM1_1 + RCELSOM2_1 + RCELSOM5_1 + RCELSOM4_1 + RCELSOM6_1 + RCELSOM8_1 + RCELSOM9_1 + RCELSOM7_1)) ;

RCELTOT = RCELSOM1 + RCELSOM2 + RCELSOM5 + RCELSOM4 + RCELSOM6 + RCELSOM8 + RCELSOM9 + RCELSOM7 + RCELSOM3 ;

RCELTOT_1 = RCELSOM1_1 + RCELSOM2_1 + RCELSOM5_1 + RCELSOM4_1 + RCELSOM6_1 + RCELSOM8_1 + RCELSOM9_1 + RCELSOM7_1 + RCELSOM3_1 ;

regle 401324:
application : iliad ;


RIVCELZMN1 = (RCEL7SR + RCEL7YZ) * (1 - V_CNR) ;

RIVCELZMN3 = (arr(min(COD7SR + COD7YZ , LIMCELLIER) * TX06/100) - (2 * RIVCELZMN1)) * (1 - V_CNR) ;

REPCELZMN = RIVCELZMN1 + RIVCELZMN3 ;

RIVCELZAB1 = (RCEL7SL + RCEL7SQ + RCEL7YX + RCEL7YY) * (1 - V_CNR) ;

RIVCELZAB3 = (arr(min(COD7SL + COD7SQ + COD7YX + COD7YY , LIMCELLIER) * TX06/100) - (2 * RIVCELZAB1)) * (1 - V_CNR) ;

REPCELZAB = RIVCELZAB1 + RIVCELZAB3 ;

RIVCELSIJ1 = (RCEL7SH + RCEL7SI + RCEL7SJ + RCEL7SK + RCEL7XH + RCEL7XI + RCEL7XJ + RCEL7XK
              + RCEL7IA + RCEL7IB + RCEL7IC + RCEL7IE + RCEL7KJ + RCEL7KL + RCEL7KN) * (1 - V_CNR) ;

RIVCELSIJ3 = (arr(min(COD7SH + COD7SK + COD7XH + COD7XK + COD7IA + COD7IE + COD7KJ + COD7KN , LIMCELLIER) * TX05/100
                   + min(COD7SI + COD7SJ + COD7XI + COD7XJ + COD7IB + COD7IC + COD7KL , LIMCELLIER) * TX06/100) - (2 * RIVCELSIJ1)) * (1 - V_CNR) ;

REPCELSIJKL = RIVCELSIJ1 + RIVCELSIJ3 ;

RIVCELRMN1 = (RCEL7SD + RCEL7SE + RCEL7SF + RCEL7SG + RCEL7WD + RCEL7WE + RCEL7WF + RCEL7WG
              + RCEL7IF + RCEL7IG + RCEL7IH + RCEL7IO + RCEL7KO + RCEL7KQ + RCEL7KR) * (1 - V_CNR) ;

RIVCELRMN3 = (arr(min(COD7SD + COD7SG + COD7WD + COD7WG + COD7IF + COD7IO + COD7KO + COD7KR , LIMCELLIER) * TX04/100
                   + min(COD7SE + COD7SF + COD7WE + COD7WF + COD7IG + COD7IH + COD7KQ , LIMCELLIER) * TX05/100) - (2 * RIVCELRMN1)) * (1 - V_CNR) ;

REPCELRMNOP = RIVCELRMN1 + RIVCELRMN3 ;

RIVCELRQ1 = (RCEL7IP + RCEL7WC + RCEL7KS) * (1 - V_CNR) ;

RIVCELRQ3 = (arr(min(COD7IP + COD7WC + COD7KS , LIMCELLIER) * TX04/100) - (2 * RIVCELRQ1)) * (1 - V_CNR) ;

REPCELRQ = RIVCELRQ1 + RIVCELRQ3 ;

RIVCELIQ1 = RCEL7IQ * (1 - V_CNR) ;

RIVCELIQ3 = (arr(min(COD7IQ , LIMCELLIER) * TX06/100) - (2 * RIVCELIQ1)) * (1 - V_CNR) ;

REPCELIQ = RIVCELIQ1 + RIVCELIQ3 ;

RIVCELHL1 = (RCEL7HL + RCEL7HM) * (1 - V_CNR) ;

RIVCELHL3 = (arr(min(COD7HL + COD7HM , LIMCELLIER) * TX06/100) - (2 * RIVCELHL1)) * (1 - V_CNR) ;

REPCELHL = RIVCELHL1 + RIVCELHL3 ;

RIVCELXHI1 = (RCEL7LD + RCEL7LE + RCEL7LF + RCEL7LN + RCEL7BA + RCEL7BB + RCEL7BC + RCEL7BD) * (1 - V_CNR) ;

RIVCELXHI3 = (arr(min(COD7LD + COD7LN + COD7BA + COD7BD , LIMCELLIER) * TX05/100
                   + min(COD7LE + COD7LF + COD7BB + COD7BC , LIMCELLIER) * TX06/100) - (2 * RIVCELXHI1)) * (1 - V_CNR) ;

REPCELXHIJK = RIVCELXHI1 + RIVCELXHI3 ;

RIVCELJIJ1 = (RCEL7LT + RCEL7LX + RCEL7LZ + RCEL7MG + RCEL7BE + RCEL7BF + RCEL7BG + RCEL7BH) * (1 - V_CNR) ;

RIVCELJIJ3 = (arr(min(COD7LT + COD7MG + COD7BE + COD7BH , LIMCELLIER) * TX04/100 
                  + min(COD7LX + COD7LZ + COD7BF + COD7BG , LIMCELLIER) * TX05/100) - (2 * RIVCELJIJ1)) * (1 - V_CNR) ;

REPCELJIJ = RIVCELJIJ1 + RIVCELJIJ3 ;

RIVCELMH1 = (RCEL7MH + RCEL7BJ) * (1 - V_CNR) ;

RIVCELMH3 = (arr(min(COD7MH + COD7BJ , LIMCELLIER) * TX04/100) - (2 * RIVCELMH1)) * (1 - V_CNR) ;

REPCELMH = RIVCELMH1 + RIVCELMH3 ;

regle 401350:
application : iliad ;


RRCELLV = max(0 , CELRREDLV - RCELRREDLV) * (1 - V_CNR) ; 

RRCELLY = max(0 , COD7LY - RCELLY) * (1 - V_CNR) ; 

RRCELMV = max(0 , COD7MV - RCELMV) * (1 - V_CNR) ; 

RRCELMR = max(0 , COD7MR - RCELMR) * (1 - V_CNR) ;

RRCELMD = max(0 , COD7MD - RCELMD) * (1 - V_CNR) ; 

RRCELML = max(0 , COD7ML - RCELML) * (1 - V_CNR) ; 

RRCELA = (max(0 , ACELREPYM + ACELREPYT + ACELREPWT + ACELRT 
                  - RCELREPYM - RCELREPYT - RCELREPWT - RCELRT) * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , min(ACELREPYM_1 , max(ACELREPYM_P,ACELREPYM1731)) 
	  + min(ACELREPYT_1 , max(ACELREPYT_P,ACELREPYT1731)) 
	  + min(ACELREPWT_1 , max(ACELREPWT_P,ACELREPWT1731)) 
	  + min(ACELRT_1 , max(ACELRT_P,ACELRT1731)) 
	    - min(RCELREPYM_1 , max(RCELREPYM_P,RCELREPYM1731) )
	    - min(RCELREPYT_1 , max(RCELREPYT_P,RCELREPYT1731) )
	    - min(RCELREPWT_1 , max(RCELREPWT_P,RCELREPWT1731) )
	    - min(RCELRT_1 , max(RCELRT_P,RCELRT1731))) 
	     * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;


RRCELLU = max(0, CELRREDLU - RCELRREDLU) * (1 - V_CNR) ;

RRCELLC = max(0, COD7LC - RCELLC) * (1 - V_CNR) ;

RRCELMU = max(0 , COD7MU - RCELMU) * (1 - V_CNR) ; 

RRCELMQ = max(0 , COD7MQ - RCELMQ) * (1 - V_CNR) ; 

RRCELMC = max(0 , COD7MC - RCELMC) * (1 - V_CNR) ; 

RRCELMK = max(0 , COD7MK - RCELMK) * (1 - V_CNR) ; 

RRCELB = (max(0 , ACELREPYN + ACELREPYU + ACELREPWU + ACELRU 
                  - RCELREPYN - RCELREPYU - RCELREPWU - RCELRU) * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , min(ACELREPYN_1 , max(ACELREPYN_P,ACELREPYN1731) )
	  + min(ACELREPYU_1 , max(ACELREPYU_P,ACELREPYU1731) )
	  + min(ACELREPWU_1 , max(ACELREPWU_P,ACELREPWU1731) )
	  + min(ACELRU_1 , max(ACELRU_P,ACELRU1731) )
	   - min(RCELREPYN_1 , max(RCELREPYN_P,RCELREPYN1731) )
	   - min(RCELREPYU_1 , max(RCELREPYU_P,RCELREPYU1731) )
	   - min(RCELREPWU_1 , max(RCELREPWU_P,RCELREPWU1731) )
	   - min(RCELRU_1 , max(RCELRU_P,RCELRU1731))) 
	     * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;


RRCELLR = max(0 , CELRREDLR - RCELRREDLR) * (1 - V_CNR) ;

RRCELLB = max(0 , COD7LB - RCELLB) * (1 - V_CNR) ;

RRCELMT = max(0 , COD7MT - RCELMT) * (1 - V_CNR) ; 

RRCELMP = max(0 , COD7MP - RCELMP) * (1 - V_CNR) ; 

RRCELMB = max(0 , COD7MB - RCELMB) * (1 - V_CNR) ; 

RRCELMJ = max(0 , COD7MJ - RCELMJ) * (1 - V_CNR) ; 

RRCELC = (max(0 , ACELREPYO + ACELREPYV + ACELREPWV - RCELREPYO - RCELREPYV - RCELREPWV) 
           * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , min(ACELREPYO_1 , max(ACELREPYO_P,ACELREPYO1731) )
	  + min(ACELREPYV_1 , max(ACELREPYV_P,ACELREPYV1731) )
	  + min(ACELREPWV_1 , max(ACELREPWV_P,ACELREPWV1731) )
	    - min(RCELREPYO_1 , max(RCELREPYO_P,RCELREPYO1731) )
	    - min(RCELREPYV_1 , max(RCELREPYV_P,RCELREPYV1731) )
	    - min(RCELREPWV_1 , max(RCELREPWV_P,RCELREPWV1731))) 
	     * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;


RRCELLQ = max(0 , CELRREDLQ - RCELRREDLQ) * (1 - V_CNR) ; 

RRCELLA = max(0 , COD7LA - RCELLA) * (1 - V_CNR) ; 

RRCELMS = max(0 , COD7MS - RCELMS) * (1 - V_CNR) ; 

RRCELMO = max(0 , COD7MO - RCELMO) * (1 - V_CNR) ; 

RRCELMA = max(0 , COD7MA - RCELMA) * (1 - V_CNR) ; 

RRCELMI = max(0 , COD7MI - RCELMI) * (1 - V_CNR) ; 

RRCELD = (max(0 , ACELREPYP + ACELREPYW + ACELREPWW - RCELREPYP - RCELREPYW - RCELREPWW) 
           * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , min(ACELREPYP_1 , max(ACELREPYP_P,ACELREPYP1731) )
	  + min(ACELREPYW_1 , max(ACELREPYW_P,ACELREPYW1731) )
	  + min(ACELREPWW_1 , max(ACELREPWW_P,ACELREPWW1731) )
		    - min(RCELREPYP_1 , max(RCELREPYP_P,RCELREPYP1731) )
		    - min(RCELREPYW_1 , max(RCELREPYW_P,RCELREPYW1731) )
		    - min(RCELREPWW_1 , max(RCELREPWW_P,RCELREPWW1731))
               ) * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;

RRCELYI = max(0 , COD7YI - RCELYI) * (1 - V_CNR) ;

RRCELZI = max(0 , COD7ZI - RCELZI) * (1 - V_CNR) ;

RRCELUU = max(0 , COD7UU - RCELUU) * (1 - V_CNR) ;

RRCELRK = max(0 , COD7RK - RCELRK) * (1 - V_CNR) ;

RRCELE = (max(0 , RCEL7SQ + RCEL7SR + RCEL7YY + RCEL7YZ + ACELXB + ACELXL + ACELPJ + ACELAF
                  - RCELSQ - RCELSR - RCELYY - RCELYZ - RCELXB - RCELXL - RCELPJ - RCELAF) 
	   * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , RCEL7SQ_1 + RCEL7SR_1 + RCEL7YY_1
		    + RCEL7YZ_1
	            + min(ACELXB_1 , max(ACELXB_P,ACELXB1731) )
		    + min(ACELXL_1 , max(ACELXL_P,ACELXL1731) )
		    + min(ACELPJ_1 , max(ACELPJ_P,ACELPJ1731) )
		    + min(ACELAF_1 , max(ACELAF_P,ACELAF1731))
		    - min(RCELSQ_1 , max(RCELSQ_P,RCELSQ1731) )
		    - min(RCELSR_1 , max(RCELSR_P,RCELSR1731) )
		    - min(RCELYY_1 , max(RCELYY_P,RCELYY1731) )
		    - min(RCELYZ_1 , max(RCELYZ_P,RCELYZ1731))
		    - min(RCELXB_1 , max(RCELXB_P,RCELXB1731) )
		    - min(RCELXL_1 , max(RCELXL_P,RCELXL1731) )
		    - min(RCELPJ_1 , max(RCELPJ_P,RCELPJ1731) )
		    - min(RCELAF_1 , max(RCELAF_P,RCELAF1731))
	       ) * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;

RRCELZP = max(0 , COD7ZP - RCELZP) * (1 - V_CNR) ;

RRCELXP = max(0 , COD7XP - RCELXP) * (1 - V_CNR) ;

RRCELYJ = max(0 , COD7YJ - RCELYJ) * (1 - V_CNR) ;

RRCELZJ = max(0 , COD7ZJ - RCELZJ) * (1 - V_CNR) ;

RRCELUV = max(0 , COD7UV - RCELUV) * (1 - V_CNR) ;

RRCELRL = max(0 , COD7RL - RCELRL) * (1 - V_CNR) ;

RRCELF = (max(0 , RCEL7SI + RCEL7SL + RCEL7XI + RCEL7YX + RCEL7IB + ACELXA + ACELXC + ACELPI + ACELXM + ACELYC + ACELAD + ACELAH + ACELAR
                  - RCELSI - RCELSL - RCELXI - RCELYX - RCELIB - RCELXA - RCELXC - RCELPI - RCELXM - RCELYC - RCELAD - RCELAH - RCELAR)  
	   * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , RCEL7SI_1 + RCEL7SL_1 + RCEL7XI_1 + RCEL7YX_1 + RCEL7IB_1 
	  + min(ACELXA_1 ,max(ACELXA_P, ACELXA1731) )
	  + min(ACELXC_1 ,max(ACELXC_P, ACELXC1731) )
	            + min(ACELPI_1 , max(ACELPI_P,ACELPI1731) )
		    + min(ACELXM_1 , max(ACELXM_P,ACELXM1731) )
		    + min(ACELYC_1 , max(ACELYC_P,ACELYC1731) )
		    + min(ACELAD_1 , max(ACELAD_P,ACELAD1731) )
		    + min(ACELAH_1 , max(ACELAH_P,ACELAH1731) )
		    + min(ACELAR_1 , max(ACELAR_P,ACELAR1731))
	            - min(RCELSI_1 , max(RCELSI_P,RCELSI1731) )
		    - min(RCELSL_1 , max(RCELSL_P,RCELSL1731) )
		    - min(RCELXI_1 , max(RCELXI_P,RCELXI1731) )
		    - min(RCELYX_1 , max(RCELYX_P,RCELYX1731) )
		    - min(RCELIB_1 , max(RCELIB_P,RCELIB1731) )
		    - min(RCELXA_1 , max(RCELXA_P,RCELXA1731) )
		    - min(RCELXC_1 , max(RCELXC_P,RCELXC1731) )
		    - min(RCELPI_1 , max(RCELPI_P,RCELPI1731) )
		    - min(RCELXM_1 , max(RCELXM_P,RCELXM1731) )
	            - min(RCELYC_1 , max(RCELYC_P,RCELYC1731) )
		    - min(RCELAD_1 , max(RCELAD_P,RCELAD1731) )
		    - min(RCELAH_1 , max(RCELAH_P,RCELAH1731) )
		    - min(RCELAR_1 , max(RCELAR_P,RCELAR1731))
	       ) * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;

RRCELZO = max(0 , COD7ZO - RCELZO) * (1 - V_CNR) ;

RRCELXO = max(0 , COD7XO - RCELXO) * (1 - V_CNR) ;

RRCELYK = max(0 , COD7YK - RCELYK) * (1 - V_CNR) ;

RRCELZK = max(0 , COD7ZK - RCELZK) * (1 - V_CNR) ;

RRCELUW = max(0 , COD7UW - RCELUW) * (1 - V_CNR) ;

RRCELRM = max(0 , COD7RM - RCELRM) * (1 - V_CNR) ;

RRCELG = (max(0 , RCEL7SE + RCEL7SH + RCEL7SJ + RCEL7SK + RCEL7WE + RCEL7XH + RCEL7XJ + RCEL7XK + RCEL7IA + RCEL7IC 
                  + RCEL7IG + RCEL7KJ + RCEL7KL + ACELYS + ACELXN + ACELYG + ACELAB + ACELAI + ACELAS
                  - RCELSE - RCELSH - RCELSJ - RCELSK - RCELWE - RCELXH - RCELXJ - RCELXK - RCELIA - RCELIC - RCELIG 
		  - RCELKJ - RCELKL - RCELYS - RCELXN - RCELYG - RCELAB - RCELAI - RCELAS)  
	   * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , RCEL7SE_1 + RCEL7SH_1 + RCEL7SJ_1 + RCEL7SK_1 + RCEL7WE_1 + RCEL7XH_1
		    + RCEL7XJ_1 + RCEL7XK_1 + RCEL7IA_1 + RCEL7IC_1 + RCEL7IG_1 + RCEL7KJ_1 + RCEL7KL_1
		    + min(ACELYS_1 , max(ACELYS_P,ACELYS1731) )
		    + min(ACELXN_1 , max(ACELXN_P,ACELXN1731) )
		    + min(ACELYG_1 , max(ACELYG_P,ACELYG1731) )
		    + min(ACELAB_1 , max(ACELAB_P,ACELAB1731) )
		    + min(ACELAI_1 , max(ACELAI_P,ACELAI1731) )
		    + min(ACELAS_1 , max(ACELAS_P,ACELAS1731))
	            - min(RCELSE_1 , max(RCELSE_P,RCELSE1731) )
		    - min(RCELSH_1 , max(RCELSH_P,RCELSH1731) )
		    - min(RCELSJ_1 , max(RCELSJ_P,RCELSJ1731) )
		    - min(RCELSK_1 , max(RCELSK_P,RCELSK1731))
		    - min(RCELWE_1 , max(RCELWE_P,RCELWE1731) )
		    - min(RCELXH_1 , max(RCELXH_P,RCELXH1731) )
		    - min(RCELXJ_1 , max(RCELXJ_P,RCELXJ1731) )
		    - min(RCELXK_1 , max(RCELXK_P,RCELXK1731))
		    - min(RCELIA_1 , max(RCELIA_P,RCELIA1731) )
		    - min(RCELIC_1 , max(RCELIC_P,RCELIC1731) )
		    - min(RCELIG_1 , max(RCELIG_P,RCELIG1731) )
		    - min(RCELKJ_1 , max(RCELKJ_P,RCELKJ1731))
		    - min(RCELKL_1 , max(RCELKL_P,RCELKL1731) )
		    - min(RCELYS_1 , max(RCELYS_P,RCELYS1731) )
		    - min(RCELXN_1 , max(RCELXN_P,RCELXN1731) )
		    - min(RCELYG_1 , max(RCELYG_P,RCELYG1731) )
		    - min(RCELAB_1 , max(RCELAB_P,RCELAB1731) )
		    - min(RCELAI_1 , max(RCELAI_P,RCELAI1731) )
		    - min(RCELAS_1 , max(RCELAS_P,RCELAS1731) )
	       ) * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;

RRCELXQ = max(0 , COD7XQ - RCELXQ) * (1 - V_CNR) ;

RRCELYL = max(0 , COD7YL - RCELYL) * (1 - V_CNR) ;

RRCELZL = max(0 , COD7ZL - RCELZL) * (1 - V_CNR) ;

RRCELUX = max(0 , COD7UX - RCELUX) * (1 - V_CNR) ;

RRCELRN = max(0 , COD7RN - RCELRN) * (1 - V_CNR) ;

RRCELH = (max(0 , RCEL7SD + RCEL7SF + RCEL7SG + RCEL7WC + RCEL7WD + RCEL7WF + RCEL7WG + RCEL7IE + RCEL7IF + RCEL7IH + RCEL7IO + RCEL7IP 
                  + RCEL7KN + RCEL7KO + RCEL7KQ + RCEL7KR + RCEL7KS + ACELQE + ACELYA + ACELYR + ACELAU + ACELAP + ACELAT
                  - RCELSD - RCELSF - RCELSG - RCELWC - RCELWD - RCELWF - RCELWG - RCELIE - RCELIF - RCELIH - RCELIO - RCELIP - RCELKN 
		  - RCELKO - RCELKQ - RCELKR - RCELKS - RCELQE - RCELYA - RCELYR - RCELAU - RCELAP - RCELAT)  
	   * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , RCEL7SD_1 + RCEL7SF_1 + RCEL7SG_1 + RCEL7WC_1 + RCEL7WD_1 + RCEL7WF_1 + RCEL7WG_1 + RCEL7IE_1
		    + RCEL7IF_1 + RCEL7IH_1 + RCEL7IO_1 + RCEL7IP_1 + RCEL7KN_1 + RCEL7KO_1 + RCEL7KQ_1 + RCEL7KR_1 + RCEL7KS_1
	            + min(ACELQE_1 , max(ACELQE_P,ACELQE1731) )
		    + min(ACELYA_1 , max(ACELYA_P,ACELYA1731) )
		    + min(ACELYR_1 , max(ACELYR_P,ACELYR1731) )
	            + min(ACELAU_1 , max(ACELAU_P,ACELAU1731) )
		    + min(ACELAP_1 , max(ACELAP_P,ACELAP1731) )
		    + min(ACELAT_1 , max(ACELAT_P,ACELAT1731))
	            - min(RCELSD_1 , max(RCELSD_P,RCELSD1731) )
		    - min(RCELSF_1 , max(RCELSF_P,RCELSF1731) )
		    - min(RCELSG_1 , max(RCELSG_P,RCELSG1731) )
		    - min(RCELWC_1 , max(RCELWC_P,RCELWC1731))
		    - min(RCELWD_1 , max(RCELWD_P,RCELWD1731) )
		    - min(RCELWF_1 , max(RCELWF_P,RCELWF1731) )
		    - min(RCELWG_1 , max(RCELWG_P,RCELWG1731) )
		    - min(RCELIE_1 , max(RCELIE_P,RCELIE1731))
		    - min(RCELIF_1 , max(RCELIF_P,RCELIF1731) )
		    - min(RCELIH_1 , max(RCELIH_P,RCELIH1731) )
		    - min(RCELIO_1 , max(RCELIO_P,RCELIO1731) )
		    - min(RCELIP_1 , max(RCELIP_P,RCELIP1731) )
		    - min(RCELKN_1 , max(RCELKN_P,RCELKN1731) )
		    - min(RCELKO_1 , max(RCELKO_P,RCELKO1731) )
		    - min(RCELKQ_1 , max(RCELKQ_P,RCELKQ1731) )
		    - min(RCELKR_1 , max(RCELKR_P,RCELKR1731))
		    - min(RCELKS_1 , max(RCELKS_P,RCELKS1731) )
		    - min(RCELQE_1 , max(RCELQE_P,RCELQE1731) )
		    - min(RCELYA_1 , max(RCELYA_P,RCELYA1731) )
		    - min(RCELYR_1 , max(RCELYR_P,RCELYR1731))
		    - min(RCELAU_1 , max(RCELAU_P,RCELAU1731) )
		    - min(RCELAP_1 , max(RCELAP_P,RCELAP1731) )
		    - min(RCELAT_1 , max(RCELAT_P,RCELAT1731))
	       ) * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;

RRCELKD = max(0 , COD7KD - RCELKD) * (1 - V_CNR) ;

RRCELPD = max(0 , COD7PD - RCELPD) * (1 - V_CNR) ;

RRCELKU = max(0 , COD7KU - RCELKU) * (1 - V_CNR) ;

RRCELI = (max(0 , RCEL7HL + RCEL7LE + RCEL7BB + ACELHA + ACELHK + ACELGS + ACELWX
                  - RCELHL - RCELLE - RCELBB - RCELHA - RCELHK - RCELGS - RCELWX) 
	   * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*null(V_IND_TRAIT - 5))
	  + max(0 , RCEL7HL_1 + RCEL7LE_1 + RCEL7BB_1
	  + min(ACELHA_1 , max(ACELHA_P,ACELHA1731) )
	  + min(ACELHK_1 , max(ACELHK_P,ACELHK1731) )
	            + min(ACELGS_1 , max(ACELGS_P,ACELGS1731) )
		    + min(ACELWX_1 , max(ACELWX_P,ACELWX1731))
	            - min(RCELHL_1 , max(RCELHL_P,RCELHL1731) )
		    - min(RCELLE_1 , max(RCELLE_P,RCELLE1731) )
		    - min(RCELBB_1 , max(RCELBB_P,RCELBB1731) )
		    - min(RCELHA_1 , max(RCELHA_P,RCELHA1731) )
		    - min(RCELHK_1 , max(RCELHK_P,RCELHK1731) )
		    - min(RCELGS_1 , max(RCELGS_P,RCELGS1731) )
		    - min(RCELWX_1 , max(RCELWX_P,RCELWX1731))
	       ) * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;

RRCELKC = max(0 , COD7KC - RCELKC) * (1 - V_CNR) ;

RRCELPC = max(0 , COD7PC - RCELPC) * (1 - V_CNR) ;

RRCELKT = max(0 , COD7KT - RCELKT) * (1 - V_CNR) ;

RRCELJ = (max(0 , RCEL7LD + RCEL7LF + RCEL7LN + RCEL7LX + RCEL7BA + RCEL7BC + RCEL7BD + RCEL7BF + ACELHJ + ACELHN + ACELGU + ACELWY
                  - RCELLD - RCELLF - RCELLN - RCELLX - RCELBA - RCELBC - RCELBD - RCELBF - RCELHJ - RCELHN - RCELGU - RCELWY) 
	   * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))* null(V_IND_TRAIT - 5))
	  + max(0 , RCEL7LD_1 + RCEL7LF_1 + RCEL7LN_1 + RCEL7LX_1 + RCEL7BA_1
		    + RCEL7BC_1 + RCEL7BD_1 + RCEL7BF_1
	            + min(ACELHJ_1 , max(ACELHJ_P,ACELHJ1731) )
		    + min(ACELHN_1 , max(ACELHN_P,ACELHN1731) )
		    + min(ACELGU_1 , max(ACELGU_P,ACELGU1731) )
		    + min(ACELWY_1 , max(ACELWY_P,ACELWY1731))
	            - min(RCELLD_1 , max(RCELLD_P,RCELLD1731) )
		    - min(RCELLF_1 , max(RCELLF_P,RCELLF1731) )
		    - min(RCELLN_1 , max(RCELLN_P,RCELLN1731) )
		    - min(RCELLX_1 , max(RCELLX_P,RCELLX1731) )
		    - min(RCELBA_1 , max(RCELLX_P,RCELBA1731) )
		    - min(RCELBC_1 , max(RCELBC_P,RCELBC1731) )
		    - min(RCELBD_1 , max(RCELBD_P,RCELBD1731) )
		    - min(RCELBF_1 , max(RCELBF_P,RCELBF1731))
	            - min(RCELHJ_1 , max(RCELHJ_P,RCELHJ1731) )
		    - min(RCELHN_1 , max(RCELHN_P,RCELHN1731) )
		    - min(RCELGU_1 , max(RCELGU_P,RCELGU1731) )
		    - min(RCELWY_1 , max(RCELWY_P,RCELWY1731))
	       ) * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;

RRCELPE = max(0 , COD7PE - RCELPE) * (1 - V_CNR) ;

RRCELKV = max(0 , COD7KV - RCELKV) * (1 - V_CNR) ;

RRCELK = (max(0 , RCEL7LT + RCEL7LZ + RCEL7MG + RCEL7MH + RCEL7BE + RCEL7BG + RCEL7BH + RCEL7BJ + ACELHY + ACELGX + ACELWZ
                  - RCELLT - RCELLZ - RCELMG - RCELMH - RCELBE - RCELBG - RCELBH - RCELBJ - RCELHY - RCELGX - RCELWZ)  
	   * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , RCEL7LT_1 + RCEL7LZ_1 + RCEL7MG_1 + RCEL7MH_1 + RCEL7BE_1
		    + RCEL7BG_1 + RCEL7BH_1 + RCEL7BJ_1
		    + min(ACELHY_1 , max(ACELHY_P,ACELHY1731) )
		    + min(ACELGX_1 , max(ACELGX_P,ACELGX1731) )
		    + min(ACELWZ_1 , max(ACELWZ_P,ACELWZ1731))
	            - min(RCELLT_1 , max(RCELLT_P,RCELLT1731) )
		    - min(RCELLZ_1 , max(RCELLZ_P,RCELLZ1731) )
		    - min(RCELMG_1 , max(RCELMG_P,RCELMG1731) )
		    - min(RCELMH_1 , max(RCELMH_P,RCELMH1731) )
		    - min(RCELBE_1 , max(RCELBE_P,RCELBE1731) )
		    - min(RCELBG_1 , max(RCELBG_P,RCELBG1731) )
		    - min(RCELBH_1 , max(RCELBH_P,RCELBH1731) )
		    - min(RCELBJ_1 , max(RCELBJ_P,RCELBJ1731))
		    - min(RCELHY_1 , max(RCELHY_P,RCELHY1731) )
		    - min(RCELGX_1 , max(RCELGX_P,RCELGX1731) )
		    - min(RCELWZ_1 , max(RCELWZ_P,RCELWZ1731))
	       ) * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;

RRCELHZ = max(0 , COD7HZ - RCELHZ) * (1 - V_CNR) ;

RRCELL = (max(0 , RCEL7IQ + RCEL7HM + ACELZM - RCELIQ - RCELHM - RCELZM) * positif(null(V_IND_TRAIT - 4)+positif(1 - COD9ZA) * (1-positif(PREM8_11))*  null(V_IND_TRAIT - 5))
	  + max(0 , RCEL7IQ_1 + RCEL7HM_1
	  + min(ACELZM_1 , max(ACELZM_P,ACELZM1731) )
	  - min(RCELIQ_1 , max(RCELIQ_P,RCELIQ1731) )
	  - min(RCELHM_1 , max(RCELHM_P,RCELHM1731) )
	  - min(RCELZM_1 , max(RCELZM_P,RCELZM1731))) 
	     * positif(1 - COD9ZA) * positif(PREM8_11)*  null(V_IND_TRAIT - 5)) * (1 - V_CNR) ;

regle 401390 :
application : iliad ;

RRI1 = IDOM11 - DEC11 - RCOTFOR - RREPA - RLOCANAH -  RDONDJ - RDIFAGRI - RPRESSE - RFORET - RFIPDOM - RFIPC 
              - RCINE - RRESTIMO - RSOCREPR - RRPRESCOMP - RHEBE - RSURV - RINNO - RSOUFIP - RRIRENOV ;

regle 401400 :
application : iliad ;


BAH = (min (RVCURE,LIM_CURE) + min(RCCURE,LIM_CURE)) * (1 - V_CNR) ;

RAH = arr (BAH * TX_CURE /100) ;

DHEBE = RVCURE + RCCURE ;

AHEBE = positif(null(V_IND_TRAIT-4)+COD9ZA) * BAH * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
        + (max(0,min(BAH,max(BAH_P,BAH1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RHEBE_1 = max( min( RAH , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RFIPDOM-RFIPC
			-RCINE-RRESTIMO-RSOCREPR-RRPRESCOMP) , 0 );
RHEBE =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RHEBE_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RHEBE_1,max(RHEBE_P,RHEBE1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401410:
application : iliad ;


DREPA = RDREP + DONETRAN ;

EXCEDANTA = max(0 , RDREP + DONETRAN - PLAF_REDREPAS) ;

BAALIM = min(DREPA , PLAF_REDREPAS) * (1 - V_CNR) ;

RAALIM = arr(BAALIM * TX_REDREPAS/100) * (1 - V_CNR) ;

AREPA = positif(null(V_IND_TRAIT-4)+COD9ZA) * BAALIM * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
        + (max(0,min(BAALIM,max(BAALIM_P,BAALIM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RREPA_1 = max(min(RAALIM , IDOM11-DEC11-RCOTFOR) , 0) ;

RREPA =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RREPA_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RREPA_1,max(RREPA_P,RREPA1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401411:
application : iliad ;


DLOCANAH = COD7BK + COD7BL + COD7BM + COD7BN + COD7BO ;

ALOCANAH = (COD7BK + COD7BL + COD7BM + COD7BN + COD7BO) * positif( (null(V_REGCO - 3) * positif(V_INDVB31) * positif(COD7AA)) + (null(V_REGCO - 3) * (1-positif(V_INDVB31)))  + null(V_REGCO - 1) + null(V_REGCO - 4) + null(V_REGCO - 5) + null(V_REGCO - 6) ) ;

RLOCANA = (COD7BK * TX15/100 + COD7BL * TX20/100 + COD7BM * TX35/100 + COD7BN * TX40/100 + COD7BO * TX65/100) * positif( (null(V_REGCO - 3) * positif(V_INDVB31) * positif(COD7AA)) + (null(V_REGCO - 3) * (1-positif(V_INDVB31))) + null(V_REGCO - 1) + null(V_REGCO - 4) + null(V_REGCO - 5) + null(V_REGCO - 6)) ;



RLOCANAH_1 = arr(max(min(RLOCANA, IDOM11-DEC11 - RCOTFOR-RREPA) , 0)) ;

RLOCANAH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RLOCANAH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RLOCANAH_1,max(RLOCANAH_P,RLOCANAH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
regle 401415:
application : iliad ;


DDONDJ = COD7UJ + COD7UG ;

BADONJ = min(DDONDJ , PLAF_ASSCULT) ;

EXCEDANTD = max(0 , DDONDJ - PLAF_ASSCULT) ;

RANDJ = arr(BADONJ * TX_REDREPAS/100) * (1 - V_CNR) ;

ADONDJ = (positif(null(V_IND_TRAIT - 4) + COD9ZA) * BADONJ * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(BADONJ , max(BADONJ_P,BADONJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0) * (1 - V_CNR) ;

RDONDJ_1 = max(min(RANDJ , IDOM11-DEC11-RCOTFOR-RREPA - RLOCANAH) , 0) ;

RDONDJ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RDONDJ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RDONDJ_1 , max(RDONDJ_P,RDONDJ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

regle 401420:
application : iliad ;
 
DNOUV = COD7CR + COD7CV + COD7CX + COD7CS + COD7CT + COD7CA + COD7DC + COD7CI + COD7CH + COD7BS + COD7BT + COD7GW ;

BSN1 = min (DNOUV , LIM_TITPRISE * (1 + BOOL_0AM)) ;

BSNCR = max(0, min(COD7CR , LIM_TITPRISE * (1 + BOOL_0AM)) ) ;
RSNNCR = BSNCR * TX18/100 ;

BSNCV = max(0, min(COD7CV , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR)) ;
RSNNCV = BSNCV * TX18/100 ;

BSNCS = max(0, min(COD7CS , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR -BSNCV )) ;
RSNNCS = BSNCS * TX25/100 ;

BSNCX = max(0, min(COD7CX , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR - BSNCV - BSNCS)) ;
RSNNCX = BSNCX * TX18/100 ;

BSNCA = max(0, min(COD7CA , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR - BSNCV - BSNCS - BSNCX)) ;
RSNNCA = BSNCA * TX25/100 ;

BSNDC = max(0, min(COD7DC , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR - BSNCV - BSNCS - BSNCX - BSNCA)) ;
RSNNDC = BSNDC * TX25/100 ;

BSNCT = max(0, min(COD7CT , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR - BSNCV - BSNCS - BSNCX - BSNCA - BSNDC)) ;
RSNNCT = BSNCT * TX18/100 ;

BSNCH = max(0, min(COD7CH , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR - BSNCV - BSNCS - BSNCX - BSNCA - BSNDC - BSNCT)) ;
RSNNCH = BSNCH * TX25/100 ;

BSNCI = max(0, min(COD7CI , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR - BSNCV - BSNCS - BSNCX - BSNCH - BSNCX - BSNCA - BSNDC - BSNCT)) ;
RSNNCI = BSNCI * TX18/100 ;

BSNBS = max(0, min(COD7BS , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR - BSNCV - BSNCS - BSNCX - BSNCH  - BSNCA - BSNDC - BSNCT - BSNCI )) ;
RSNNBS = BSNBS * TX25/100 ;

BSNBT = max(0, min(COD7BT , LIM_TITPRISE * (1 + BOOL_0AM) - BSNCR - BSNCV - BSNCS - BSNCX - BSNCH  - BSNCA - BSNDC - BSNCT - BSNCI - BSNBS )) ;
RSNNBT = BSNBT * TX25/100 ;

BSNGW = max(0, min(COD7GW , LIM_TITPRISE * (1 + BOOL_0AM)  - BSNCR - BSNCV - BSNCS - BSNCX - BSNCA - BSNDC - BSNCT - BSNCH - BSNCI - BSNBS - BSNBT )) ;
RSNNGW = BSNGW * TX25/100 ;

RSN = arr(RSNNCR + RSNNCV + RSNNCX + RSNNCS + RSNNCA + RSNNDC + RSNNCT + RSNNCH + RSNNCI + RSNNBS + RSNNBT + RSNNGW) * (1 - V_CNR) ;

ANOUV = (positif(null(V_IND_TRAIT-4) + COD9ZA) * (BSN1 + BSN2) * (1 - positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
         + (max(0,min(BSN1 + BSN2 , max(BSN1_P+BSN2_P,BSN11731 + BSN21731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5) + 0) * (1 - V_CNR) ;

regle 401430:
application : iliad ;


VARTMP1 = RLOGDOM + RCOMP + RRETU + RDONS + CRDIE + RDUFREP + RPINELTOT + RNORMTOT ;

RSNCR_1 = max(0, min(RSNNCR , RRI1 - VARTMP1)) ;
RSNCR =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNCR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNCR_1,max(RSNCR_P,RSNCR1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNCR ;

RSNCV_1 = max(0, min(RSNNCV , RRI1 - VARTMP1)) ;
RSNCV =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNCV_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNCV_1,max(RSNCV_P,RSNCV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNCV ;

RSNCS_1 = max(0, min(RSNNCS , RRI1 - VARTMP1)) ;
RSNCS =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNCS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSNCS_1,max(RSNCS_P,RSNCS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNCS ;

RSNCX_1 = max(0, min(RSNNCX , RRI1 - VARTMP1)) ;
RSNCX =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNCX_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNCX_1,max(RSNCX_P,RSNCX1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNCX ;

RSNCA_1 = max(0, min(RSNNCA , RRI1 - VARTMP1)) ;
RSNCA =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNCA_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNCA_1,max(RSNCA_P,RSNCA1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNCA ;

RSNDC_1 = max(0, min(RSNNDC , RRI1 - VARTMP1)) ;
RSNDC =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNDC_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNDC_1,max(RSNDC_P,RSNDC1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNDC ;

RSNCT_1 = max(0, min(RSNNCT , RRI1 - VARTMP1)) ;
RSNCT =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNCT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNCT_1,max(RSNCT_P,RSNCT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNCT ;

RSNCH_1 = max(0, min(RSNNCH , RRI1 - VARTMP1)) ;
RSNCH =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNCH_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNCH_1,max(RSNCH_P,RSNCH1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNCH ;

RSNCI_1 = max(0, min(RSNNCI , RRI1 - VARTMP1)) ;
RSNCI =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNCI_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNCI_1,max(RSNCI_P,RSNCI1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNCI ;

RSNBS_1 = max(0, min(RSNNBS , RRI1 - VARTMP1)) ;
RSNBS =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNBS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNBS_1,max(RSNBS_P,RSNBS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNBS ;

RSNBT_1 = max(0, min(RSNNBT , RRI1 - VARTMP1)) ;
RSNBT =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNBT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNBT_1,max(RSNBT_P,RSNBT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSNBT ;

RSNGW_1 = max(0, min(RSNNGW , RRI1 - VARTMP1)) ;
RSNGW =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSNGW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSNGW_1,max(RSNGW_P,RSNGW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = 0 ;

RNOUV_1 = arr(RSNCR_1 + RSNCV_1 + RSNCS_1 + RSNCX_1 + RSNCA_1 + RSNDC_1 + RSNCT_1 + RSNCH_1 + RSNCI_1 + RSNBS_1 + RSNBT_1 + RSNGW_1) ; 

RNOUV = arr(RSNCR + RSNCV + RSNCS + RSNCX + RSNCA + RSNDC + RSNCT + RSNCH + RSNCI + RSNBS + RSNBT + RSNGW) * (1 - V_CNR) ;

regle 401435:
application : iliad ;

DROUVB = min(COD7CV + COD7CX + COD7CH + COD7CS + COD7CT + COD7CA + COD7DC + COD7CI + COD7BT + COD7BS + COD7GW , LIM_TITPRISE * (1 + BOOL_0AM) - COD7CR)  ;


RINVPECV = max(0 , COD7CV - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - COD7CR))* (1 - V_CNR) ;

RINVPECS = max(0 , COD7CS - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV)))* (1 - V_CNR) ;

RINVPECX = max(0 , COD7CX - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV + COD7CS)))* (1 - V_CNR) ;

RINVPECA = max(0 , COD7CA - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV + COD7CS + COD7CX)))* (1 - V_CNR) ;

RINVPEDC = max(0 , COD7DC - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV + COD7CS + COD7CX + COD7CA)))* (1 - V_CNR) ;

RINVPECT = max(0 , COD7CT - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV + COD7CS + COD7CX + COD7CA + COD7DC)))* (1 - V_CNR) ;


RINVPECH = max(0 , COD7CH - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV + COD7CS + COD7CX + COD7CA + COD7DC + COD7CT)))* (1 - V_CNR) ;

RINVPECI = max(0 , COD7CI - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV + COD7CS + COD7CX + COD7CA + COD7DC + COD7CT  + COD7CH)))* (1 - V_CNR) ;

RINVPEBS = max(0 , COD7BS - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV + COD7CS + COD7CX + COD7CA + COD7DC + COD7CT + COD7CH + COD7CI)))* (1 - V_CNR);

RINVPEBT = max(0 , COD7BT - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV + COD7CS + COD7CX + COD7CA + COD7DC + COD7CT + COD7CH + COD7CI + COD7BS)))* (1 - V_CNR);

RINVPEGW = max(0 , COD7GW - max(0 , LIM_TITPRISE * (1 + BOOL_0AM) - (COD7CR + COD7CV + COD7CS + COD7CX + COD7CA + COD7DC + COD7CT + COD7CH + COD7CI + COD7BS + COD7BT)))* (1 - V_CNR) ;
regle 401440:
application : iliad ;


DPENTCY = COD7CY ;
APENTCY_1 = COD7CY * positif(COD7CY) * (1 - V_CNR) ;
APENTCY = positif(null(V_IND_TRAIT-4)+COD9ZA) * (APENTCY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(APENTCY_1,max(APENTCY_P,APENTCY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RPENTCY_1 = max(min(APENTCY , RRI1-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV) , 0) ;
RPENTCY = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPENTCY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RPENTCY_1,max(RPENTCY_P,RPENTCY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DPENTDY = COD7DY ;
APENTDY_1 = COD7DY * positif(COD7DY) * (1 - V_CNR) ;
APENTDY = positif(null(V_IND_TRAIT-4)+COD9ZA) * (APENTDY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(APENTDY_1,max(APENTDY_P,APENTDY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RPENTDY_1 = max(min(APENTDY , RRI1-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTCY) , 0) ;
RPENTDY =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPENTDY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RPENTDY_1,max(RPENTDY_P,RPENTDY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DPENTEY = COD7EY ;
APENTEY_1 = COD7EY * positif(COD7EY) * (1 - V_CNR) ;
APENTEY = positif(null(V_IND_TRAIT-4)+COD9ZA) * (APENTEY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(APENTEY_1,max(APENTEY_P,APENTEY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RPENTEY_1 = max(min(APENTEY , RRI1-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTCY- RPENTDY) , 0) ;
RPENTEY =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPENTEY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RPENTEY_1,max(RPENTEY_P,RPENTEY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DPENTFY = COD7FY ;
APENTFY_1 = COD7FY * positif(COD7FY) * (1 - V_CNR) ;
APENTFY = positif(null(V_IND_TRAIT-4)+COD9ZA) * (APENTFY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(APENTFY_1,max(APENTFY_P,APENTFY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RPENTFY_1 = max(min(APENTFY , RRI1-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTCY-RPENTDY-RPENTEY) , 0);
RPENTFY =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPENTFY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RPENTFY_1,max(RPENTFY_P,RPENTFY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DPENTGY = COD7GY ;
APENTGY_1 = COD7GY * positif(COD7GY) * (1 - V_CNR) ;
APENTGY = positif(null(V_IND_TRAIT-4)+COD9ZA) * (APENTGY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(APENTGY_1,max(APENTGY_P,APENTGY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RPENTGY_1 =  max(min(APENTGY , RRI1-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTCY-RPENTDY-RPENTEY-RPENTFY) , 0) ;
RPENTGY =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPENTGY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RPENTGY_1,max(RPENTGY_P,RPENTGY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DPENTEK = COD7EK ;
APENTEK_1 = COD7EK * positif(COD7EK) * (1 - V_CNR) ;
APENTEK = positif(null(V_IND_TRAIT-4)+COD9ZA) * (APENTEK_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(APENTEK_1,max(APENTEK_P,APENTEK1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RPENTEK_1 =  max(min(APENTEK , RRI1-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTCY-RPENTDY-RPENTEY-RPENTFY - RPENTGY) , 0) ;
RPENTEK =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RPENTEK_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RPENTEK_1,max(RPENTEK_P,RPENTEK1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


DPENTY = DPENTCY + DPENTDY + DPENTEY + DPENTFY + DPENTGY + DPENTEK ;  

APENTY = APENTCY + APENTDY + APENTEY + APENTFY + APENTGY + APENTEK ; 

RPENTOT = RPENTCY + RPENTDY + RPENTEY + RPENTFY + RPENTGY + RPENTEK ;
RPENTOT_1 = RPENTCY_1 + RPENTDY_1 + RPENTEY_1 + RPENTFY_1 + RPENTGY_1 + RPENTEK_1 ;

regle 401460:
application : iliad ;




RPLAFPME22 = arr(max(0 , RSNCH + RSNGW + RSNCI - 10000)) * (1 - V_CNR) * positif(AVFISCOPTER) ;

RPLAFPME18 = arr(max(0 ,(RSNCH + RSNGW + RSNCI + RSNCR + RPENTDY) - (10000 + RPLAFPME22))) * positif(AVFISCOPTER) ;

RPLAFPME19 = arr(max(0 ,(RSNCH + RSNGW + RSNCI + RSNCR + RPENTDY + RSNCV + RPENTEY) - (10000 +RPLAFPME22 + RPLAFPME18))) * positif(AVFISCOPTER) ;

RPLAFPME20 = arr(max(0 , (RSNCH + RSNGW + RSNCI + RSNCR + RPENTDY + RSNCV + RPENTEY + RSNCX + RSNCS + RSNBS + RPENTFY) - (10000 + RPLAFPME22 + RPLAFPME18 + RPLAFPME19))) * positif(AVFISCOPTER) ;

RPLAFPME21 = arr(max(0 , (RSNCH + RSNGW + RSNCI + RSNCR + RPENTDY + RSNCV + RPENTEY + RSNCX + RSNCS + RSNBS + RPENTFY + RSNCT + RSNCA + RPENTGY) - (10000  + RPLAFPME22 + RPLAFPME18 + RPLAFPME19 + RPLAFPME20 ))) * positif(AVFISCOPTER) ;

R2PLAFPME21 = arr(max(0 , (RSNCH + RSNGW + RSNCI + RSNCR + RPENTDY + RSNCV + RPENTEY + RSNCX + RSNCS + RSNBS + RPENTFY + RSNCT + RSNCA + RPENTGY + RSNDC + RSNBT + RPENTEK) - (13000  + RPLAFPME22 + RPLAFPME18 + RPLAFPME19 + RPLAFPME20 + RPLAFPME21))) * positif(AVFISCOPTER) ;


regle 401470:
application : iliad ;



AILMPP_1 = COD7PP * (1 - V_CNR) ;
AILMPP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMPP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMPP_1 , max(AILMPP_P,AILMPP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ; 

AILMPU_1 = COD7PU * (1 - V_CNR) ;
AILMPU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMPU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(AILMPU_1 , max(AILMPU_P,AILMPU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

AILMHO_1 = COD7HO * (1 - V_CNR) ;
AILMHO =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMHO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMHO_1,max(AILMHO_P,AILMHO1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHT_1 = COD7HT * (1 - V_CNR) ;
AILMHT =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMHT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMHT_1,max(AILMHT_P,AILMHT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHD_1 = COD7HD * (1 - V_CNR) ;
AILMHD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMHD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMHD_1 , max(AILMHD_P,AILMHD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


AILMPQ_1 = COD7PQ * (1 - V_CNR) ;
AILMPQ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMPQ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMPQ_1,max(AILMPQ_P,AILMPQ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMPV_1 = COD7PV * (1 - V_CNR) ;
AILMPV =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMPV_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMPV_1,max(AILMPV_P,AILMPV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHP_1 = COD7HP * (1 - V_CNR) ;
AILMHP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMHP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMHP_1 , max(AILMHP_P,AILMHP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

AILMHU_1 = COD7HU * (1 - V_CNR) ;
AILMHU =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMHU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMHU_1,max(AILMHU_P,AILMHU1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHE_1 = COD7HE * (1 - V_CNR) ;
AILMHE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMHE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMHE_1 , max(AILMHE_P,AILMHE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


AILMPR_1 = COD7PR * (1 - V_CNR) ;
AILMPR =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMPR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMPR_1,max(AILMPR_P,AILMPR1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMPW_1 = COD7PW * (1 - V_CNR) ;
AILMPW =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMPW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMPW_1,max(AILMPW_P,AILMPW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHQ_1 = COD7HQ * (1 - V_CNR) ;
AILMHQ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMHQ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMHQ_1,max(AILMHQ_P,AILMHQ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHV_1 = COD7HV * (1 - V_CNR) ;
AILMHV =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMHV_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMHV_1,max(AILMHV_P,AILMHV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHF_1 = COD7HF * (1 - V_CNR) ;
AILMHF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMHF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMHF_1 , max(AILMHF_P,AILMHF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


AILMPS_1 = COD7PS * (1 - V_CNR) ;
AILMPS =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMPS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMPS_1,max(AILMPS_P,AILMPS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMPX_1 = COD7PX * (1 - V_CNR) ;
AILMPX =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMPX_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMPX_1,max(AILMPX_P,AILMPX1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHR_1 = COD7HR * (1 - V_CNR) ;
AILMHR =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMHR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMHR_1,max(AILMHR_P,AILMHR1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHW_1 = COD7HW * (1 - V_CNR) ;
AILMHW =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMHW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMHW_1,max(AILMHW_P,AILMHW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHG_1 = COD7HG * (1 - V_CNR) ;
AILMHG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMHG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMHG_1 , max(AILMHG_P,AILMHG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


AILMPT_1 = COD7PT * (1 - V_CNR) ;
AILMPT =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMPT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMPT_1,max(AILMPT_P,AILMPT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMPY_1 = COD7PY * (1 - V_CNR) ;
AILMPY =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMPY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMPY_1,max(AILMPY_P,AILMPY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHS_1 = COD7HS * (1 - V_CNR) ;
AILMHS =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMHS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMHS_1,max(AILMHS_P,AILMHS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHX_1 = COD7HX * (1 - V_CNR) ;
AILMHX =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMHX_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMHX_1,max(AILMHX_P,AILMHX1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

AILMHH_1 = COD7HH * (1 - V_CNR) ;
AILMHH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMHH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMHH_1 , max(AILMHH_P,AILMHH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

AILMKE_1 = COD7KE * (1 - V_CNR) ;
AILMKE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMKE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMKE_1 , max(AILMKE_P,AILMKE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

AILMKF_1 = COD7KF * (1 - V_CNR) ;
AILMKF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMKF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMKF_1 , max(AILMKF_P,AILMKF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

AILMKG_1 = COD7KG * (1 - V_CNR) ;
AILMKG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMKG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMKG_1 , max(AILMKG_P,AILMKG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

AILMKH_1 = COD7KH * (1 - V_CNR) ;
AILMKH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMKH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMKH_1 , max(AILMKH_P,AILMKH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

AILMKI_1 = COD7KI * (1 - V_CNR) ;
AILMKI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * AILMKI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(AILMKI_1 , max(AILMKI_P,AILMKI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;


DILMNP1 = COD7HD + COD7PP + COD7PU + COD7HO + COD7HT + COD7HE + COD7PQ + COD7PV 
          + COD7HP + COD7HU + COD7HF + COD7PR + COD7PW + COD7HQ + COD7HV + COD7HG 
	  + COD7PS + COD7PX + COD7HR + COD7HW + COD7HH + COD7PT + COD7PY + COD7HS + COD7HX 
	  + COD7KE + COD7KF + COD7KG + COD7KH + COD7KI ;

AILMNP1 = AILMHD + AILMPP + AILMPU + AILMHO + AILMHT + AILMHE + AILMPQ + AILMPV 
          + AILMHP + AILMHU + AILMHF + AILMPR + AILMPW + AILMHQ + AILMHV + AILMHG 
	  + AILMPS + AILMPX + AILMHR + AILMHW + AILMHH + AILMPT + AILMPY + AILMHS + AILMHX 
	  + AILMKE + AILMKF + AILMKG + AILMKH + AILMKI ;







BILMOJ = min(LIMREPLOC8 , COD7OJ) * (1 - COD7OZ) + COD7OJ * COD7OZ ;
AILMOJ_1 = BILMOJ * (1 - V_CNR) ;
AILMOJ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOJ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOJ_1,max(AILMOJ_P,AILMOJ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOI = min(LIMREPLOC8 , COD7OI) * (1 - COD7OZ) + COD7OI * COD7OZ ;
AILMOI_1 = BILMOI * (1 - V_CNR) ;
AILMOI =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOI_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOI_1,max(AILMOI_P,AILMOI1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOH = min(LIMREPLOC6 , COD7OH) * (1 - COD7OZ) + COD7OH * COD7OZ ;
AILMOH_1 = BILMOH * (1 - V_CNR) ;
AILMOH =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOH_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOH_1,max(AILMOH_P,AILMOH1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOG = min(LIMREPLOC5 , COD7OG) * (1 - COD7OZ) + COD7OG * COD7OZ ;
AILMOG_1 = BILMOG * (1 - V_CNR) ;
AILMOG =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOG_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOG_1,max(AILMOG_P,AILMOG1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOF = min(LIMREPLOC3 , COD7OF) * (1 - COD7OZ) + COD7OF * COD7OZ ;
AILMOF_1 = BILMOF * (1 - V_CNR) ;
AILMOF =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOF_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOF_1,max(AILMOF_P,AILMOF1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOO = min(LIMREPLOC8 , COD7OO) * (1 - COD7OZ) + COD7OO * COD7OZ ;
AILMOO_1 = BILMOO * (1 - V_CNR) ;
AILMOO =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOO_1,max(AILMOO_P,AILMOO1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMON = min(LIMREPLOC8 , COD7ON) * (1 - COD7OZ) + COD7ON * COD7OZ ;
AILMON_1 = BILMON * (1 - V_CNR) ;
AILMON =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMON_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMON_1,max(AILMON_P,AILMON1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOM = min(LIMREPLOC6 , COD7OM) * (1 - COD7OZ) + COD7OM * COD7OZ ;
AILMOM_1 = BILMOM * (1 - V_CNR) ;
AILMOM =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOM_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOM_1,max(AILMOM_P,AILMOM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOL = min(LIMREPLOC5 , COD7OL) * (1 - COD7OZ) + COD7OL * COD7OZ ;
AILMOL_1 = (BILMOL) * (1 - V_CNR) ;
AILMOL =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOL_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOL_1,max(AILMOL_P,AILMOL1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOK = min(LIMREPLOC11 , COD7OK) * (1 - COD7OZ) + COD7OK * COD7OZ ;
AILMOK_1 = (BILMOK) * (1 - V_CNR) ;
AILMOK =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOK_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOK_1,max(AILMOK_P,AILMOK1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOT = min(LIMREPLOC8 , COD7OT) * (1 - COD7OZ) + COD7OT * COD7OZ ;
AILMOT_1 = (BILMOT) * (1 - V_CNR) ;
AILMOT =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOT_1,max(AILMOT_P,AILMOT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOS = min(LIMREPLOC8 , COD7OS) * (1 - COD7OZ) + COD7OS * COD7OZ ;
AILMOS_1 = (BILMOS) * (1 - V_CNR) ;
AILMOS =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOS_1,max(AILMOS_P,AILMOS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOR = min(LIMREPLOC6 , COD7OR) * (1 - COD7OZ) + COD7OR * COD7OZ ;
AILMOR_1 = (BILMOR) * (1 - V_CNR) ;
AILMOR =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOR_1,max(AILMOR_P,AILMOR1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOQ = min(LIMREPLOC5 , COD7OQ) * (1 - COD7OZ) + COD7OQ * COD7OZ ;
AILMOQ_1 = (BILMOQ) * (1 - V_CNR) ;
AILMOQ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOQ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOQ_1,max(AILMOQ_P,AILMOQ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMOP = min(LIMREPLOC11 , COD7OP) * (1 - COD7OZ) + COD7OP * COD7OZ ;
AILMOP_1 = (BILMOP) * (1 - V_CNR) ;
AILMOP =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMOP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMOP_1,max(AILMOP_P,AILMOP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMSA = min(LIMREPLOC11 , COD7SA) * (1 - COD7OZ) + COD7SA * COD7OZ ;
AILMSA_1 = (BILMSA) * (1 - V_CNR) ;
AILMSA =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMSA_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMSA_1,max(AILMSA_P,AILMSA1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMSB = min(LIMREPLOC2 , COD7SB) * (1 - COD7OZ) + COD7SB * COD7OZ ;
AILMSB_1 = (BILMSB) * (1 - V_CNR) ;
AILMSB =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMSB_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMSB_1,max(AILMSB_P,AILMSB1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMSC = min(LIMREPLOC9 , COD7SC) * (1 - COD7OZ) + COD7SC * COD7OZ ;
AILMSC_1 = (BILMSC) * (1 - V_CNR) ;
AILMSC =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMSC_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMSC_1,max(AILMSC_P,AILMSC1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMSN = min(LIMREPLOC11 , COD7SN) * (1 - COD7OZ) + COD7SN * COD7OZ ;
AILMSN_1 = (BILMSN) * (1 - V_CNR) ;
AILMSN =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMSN_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMSN_1,max(AILMSN_P,AILMSN1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMSO = min(LIMREPLOC2 , COD7SO) * (1 - COD7OZ) + COD7SO * COD7OZ ;
AILMSO_1 = (BILMSO) * (1 - V_CNR) ;
AILMSO =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMSO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMSO_1,max(AILMSO_P,AILMSO1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMSP = min(LIM10000 , COD7SP) * (1 - COD7OZ) + COD7SP * COD7OZ ;
AILMSP_1 = (BILMSP) * (1 - V_CNR) ;
AILMSP =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMSP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMSP_1,max(AILMSP_P,AILMSP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
BILMSM = min(LIM10000 , COD7SM) * (1 - COD7OZ) + COD7SM * COD7OZ ;
AILMSM_1 = (BILMSM) * (1 - V_CNR) ;
AILMSM =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMSM_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMSM_1,max(AILMSM_P,AILMSM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

BILMSS = min(LIM10000 , COD7SS) * (1 - COD7OZ) + COD7SS * COD7OZ ;
AILMSS_1 = (BILMSS) * (1 - V_CNR) ;
AILMSS =positif(null(V_IND_TRAIT-4)+COD9ZA) * (AILMSS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AILMSS_1,max(AILMSS_P,AILMSS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


DILMNP3 = COD7OF 
          + COD7OG + COD7OH + COD7OI + COD7OJ + COD7OK + COD7OL + COD7OM + COD7ON + COD7OO + COD7OP + COD7OQ 
	  + COD7OR + COD7OS + COD7OT + COD7SA + COD7SB + COD7SC + COD7SN + COD7SO + COD7SP + COD7SM + COD7SS ;

AILMNP3 = AILMOF 
          + AILMOG + AILMOH + AILMOI + AILMOJ + AILMOK + AILMOL + AILMOM + AILMON + AILMOO + AILMOP 
	  + AILMOQ + AILMOR + AILMOS + AILMOT + AILMSA + AILMSB + AILMSC + AILMSN + AILMSO + AILMSP + AILMSM + AILMSS ;

regle 401500:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELTOT ;






RILMPP_1 = max(min(COD7PP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPP_1 , max(RILMPP_P,RILMPP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPP ;

REPMEUPP = (COD7PP - RILMPP) * (1 - V_CNR) ;

RILMPQ_1 = max(min(COD7PQ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPQ_1 , max(RILMPQ_P,RILMPQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPQ ;

REPMEUPQ = (COD7PQ - RILMPQ) * (1 - V_CNR) ;

RILMPR_1 = max(min(COD7PR , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPR_1 , max(RILMPR_P,RILMPR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPR ;

REPMEUPR = (COD7PR - RILMPR) * (1 - V_CNR) ;

RILMPS_1 = max(min(COD7PS , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPS_1 , max(RILMPS_P,RILMPS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPS ;

REPMEUPS = (COD7PS - RILMPS) * (1 - V_CNR) ;

RILMPT_1 = max(min(COD7PT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPT_1 , max(RILMPT_P,RILMPT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPT  ;

REPMEUPT = (COD7PT - RILMPT) * (1 - V_CNR) ;

RILMPU_1 = max(min(COD7PU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPU_1 , max(RILMPU_P,RILMPU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPU ;

REPMEUPU = (COD7PU - RILMPU) * (1 - V_CNR) ;

RILMPV_1 = max(min(COD7PV , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPV_1 , max(RILMPV_P,RILMPV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPV ;

REPMEUPV = (COD7PV - RILMPV) * (1 - V_CNR) ;

RILMPW_1 = max(min(COD7PW , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPW = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPW_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPW_1 , max(RILMPW_P,RILMPW1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPW ;

REPMEUPW = (COD7PW - RILMPW) * (1 - V_CNR) ;

RILMPX_1 = max(min(COD7PX , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPX_1 , max(RILMPX_P,RILMPX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPX ;

REPMEUPX = (COD7PX - RILMPX) * (1 - V_CNR) ;

RILMPY_1 = max(min(COD7PY , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMPY = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMPY_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMPY_1 , max(RILMPY_P,RILMPY1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMPY ;

REPMEUPY = (COD7PY - RILMPY) * (1 - V_CNR) ;

RILMHO_1 = max(min(COD7HO , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHO = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHO_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHO_1 , max(RILMHO_P,RILMHO1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHO ;

REPMEUHO = (COD7HO - RILMHO) * (1 - V_CNR) ;

RILMHP_1 = max(min(COD7HP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHP_1 , max(RILMHP_P,RILMHP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHP ;

REPMEUHP = (COD7HP - RILMHP) * (1 - V_CNR) ;

RILMHQ_1 = max(min(COD7HQ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHQ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHQ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHQ_1 , max(RILMHQ_P,RILMHQ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHQ ;

REPMEUHQ = (COD7HQ - RILMHQ) * (1 - V_CNR) ;

RILMHR_1 = max(min(COD7HR , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHR_1 , max(RILMHR_P,RILMHR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHR ;

REPMEUHR = (COD7HR - RILMHR) * (1 - V_CNR) ;

RILMHS_1 = max(min(COD7HS , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHS = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHS_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHS_1 , max(RILMHS_P,RILMHS1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHS ;

REPMEUHS = (COD7HS - RILMHS) * (1 - V_CNR) ;

RILMHT_1 = max(min(COD7HT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHT = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHT_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHT_1 , max(RILMHT_P,RILMHT1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHT ;

REPMEUHT = (COD7HT - RILMHT) * (1 - V_CNR) ;

RILMHU_1 = max(min(COD7HU , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHU = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHU_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHU_1 , max(RILMHU_P,RILMHU1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHU ;

REPMEUHU = (COD7HU - RILMHU) * (1 - V_CNR) ;

RILMHV_1 = max(min(COD7HV , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHV = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHV_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHV_1 , max(RILMHV_P,RILMHV1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHV ;

REPMEUHV = (COD7HV - RILMHV) * (1 - V_CNR) ;

RILMHW_1 = max(min(COD7HW , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHW = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHW_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHW_1 , max(RILMHW_P,RILMHW1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHW ;

REPMEUHW = (COD7HW - RILMHW) * (1 - V_CNR) ;

RILMHX_1 = max(min(COD7HX , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHX = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHX_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHX_1 , max(RILMHX_P,RILMHX1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHX ;

REPMEUHX = (COD7HX - RILMHX) * (1 - V_CNR) ;

RILMHD_1 = max(min(COD7HD , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHD = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHD_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHD_1 , max(RILMHD_P,RILMHD1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHD ;

REPMEUHD = (COD7HD - RILMHD) * (1 - V_CNR) ;

RILMHE_1 = max(min(COD7HE , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHE_1 , max(RILMHE_P,RILMHE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHE ;

REPMEUHE = (COD7HE - RILMHE) * (1 - V_CNR) ;

RILMHF_1 = max(min(COD7HF , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHF_1 , max(RILMHF_P,RILMHF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHF ;

REPMEUHF = (COD7HF - RILMHF) * (1 - V_CNR) ;

RILMHG_1 = max(min(COD7HG , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHG_1 , max(RILMHG_P,RILMHG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHG ;

REPMEUHG = (COD7HG - RILMHG) * (1 - V_CNR) ;

RILMHH_1 = max(min(COD7HH , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMHH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMHH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMHH_1 , max(RILMHH_P,RILMHH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMHH ;

REPMEUHH = (COD7HH - RILMHH) * (1 - V_CNR) ;

RILMKE_1 = max(min(COD7KE , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMKE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMKE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMKE_1 , max(RILMKE_P,RILMKE1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMKE ;

REPMEUKE = (COD7KE - RILMKE) * (1 - V_CNR) ;

RILMKF_1 = max(min(COD7KF , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMKF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMKF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMKF_1 , max(RILMKF_P,RILMKF1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMKF ;

REPMEUKF = (COD7KF - RILMKF) * (1 - V_CNR) ;

RILMKG_1 = max(min(COD7KG , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMKG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMKG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMKG_1 , max(RILMKG_P,RILMKG1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
	 VARTMP1 = VARTMP1 + RILMKG ;

REPMEUKG = (COD7KG - RILMKG) * (1 - V_CNR) ;

RILMKH_1 = max(min(COD7KH , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMKH = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMKH_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMKH_1 , max(RILMKH_P,RILMKH1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RILMKH ;

REPMEUKH = (COD7KH - RILMKH) * (1 - V_CNR) ;

RILMKI_1 = max(min(COD7KI , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMKI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RILMKI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RILMKI_1 , max(RILMKI_P,RILMKI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

REPMEUKI = (COD7KI - RILMKI) * (1 - V_CNR) ;

RILMNP1 = RILMHD + RILMPK + RILMPP + RILMPU + RILMHO + RILMHT + RILMHE + RILMPL + RILMPQ + RILMPV + RILMHP + RILMHU + RILMHF
          + RILMPM + RILMPR + RILMPW + RILMHQ + RILMHV + RILMHG + RILMPN + RILMPS + RILMPX + RILMHR + RILMHW 
	  + RILMPO + RILMPT + RILMPY + RILMHS + RILMHX + RILMHH + RILMKE + RILMKF + RILMKG + RILMKH + RILMKI ;
RILMNP1_1 = RILMHD_1 + RILMPK_1 + RILMPP_1 + RILMPU_1 + RILMHO_1 + RILMHT_1 + RILMHE_1 + RILMPL_1 + RILMPQ_1 + RILMPV_1 + RILMHP_1 + RILMHU_1 + RILMHF_1 
          + RILMPM_1 + RILMPR_1 + RILMPW_1 + RILMHQ_1 + RILMHV_1 + RILMHG_1 + RILMPN_1 + RILMPS_1 + RILMPX_1 + RILMHR_1 + RILMHW_1 + RILMHH_1 
	  + RILMPO_1 + RILMPT_1 + RILMPY_1 + RILMHS_1 + RILMHX_1  + RILMKE_1 + RILMKF_1 + RILMKG_1 + RILMKH_1 + RILMKI_1;

regle 401680:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELTOT + RILMNP1 ;






RILMOJ_1 = max(min(BILMOJ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOJ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOJ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOJ_1,max(RILMOJ_P,RILMOJ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOJ ;

RILMOI_1 = max(min(BILMOI , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOI =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOI_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOI_1,max(RILMOI_P,RILMOI1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOI ;

RILMOH_1 = max(min(BILMOH , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOH =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOH_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOH_1,max(RILMOH_P,RILMOH1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOH ;

RILMOG_1 = max(min(BILMOG , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOG =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOG_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOG_1,max(RILMOG_P,RILMOG1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOG ;

RILMOF_1 = max(min(BILMOF , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOF =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOF_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOF_1,max(RILMOF_P,RILMOF1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOF ;

RILMOO_1 = max(min(BILMOO , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOO =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOO_1,max(RILMOO_P,RILMOO1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOO ;

RILMON_1 = max(min(BILMON , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMON =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMON_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMON_1,max(RILMON_P,RILMON1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMON ;

RILMOM_1 = max(min(BILMOM , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOM =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOM_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOM_1,max(RILMOM_P,RILMOM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOM ;

RILMOL_1 = max(min(BILMOL , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOL =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOL_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOL_1,max(RILMOL_P,RILMOL1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOL ;

RILMOK_1 = max(min(BILMOK , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOK =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOK_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOK_1,max(RILMOK_P,RILMOK1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOK ;

RILMOT_1 = max(min(BILMOT , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOT =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOT_1,max(RILMOT_P,RILMOT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOT ;

RILMOS_1 = max(min(BILMOS , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOS =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOS_1,max(RILMOS_P,RILMOS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOS ;

RILMOR_1 = max(min(BILMOR , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOR =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOR_1,max(RILMOR_P,RILMOR1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOR ;

RILMOQ_1 = max(min(BILMOQ , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOQ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOQ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOQ_1,max(RILMOQ_P,RILMOQ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOQ ;

RILMOP_1 = max(min(BILMOP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMOP =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMOP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMOP_1,max(RILMOP_P,RILMOP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMOP ;

RILMSC_1 = max(min(BILMSC , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMSC =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMSC_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMSC_1,max(RILMSC_P,RILMSC1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMSC ;
RILMSB_1 = max(min(BILMSB , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMSB =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMSB_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMSB_1,max(RILMSB_P,RILMSB1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMSB ;
RILMSA_1 = max(min(BILMSA , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMSA =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMSA_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMSA_1,max(RILMSA_P,RILMSA1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMSA ;

RILMSO_1 = max(min(BILMSO , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMSO =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMSO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMSO_1,max(RILMSO_P,RILMSO1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMSO ;

RILMSN_1 = max(min(BILMSN , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMSN =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMSN_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMSN_1,max(RILMSN_P,RILMSN1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMSN ;

RILMSP_1 = max(min(BILMSP , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMSP =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMSP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMSP_1,max(RILMSP_P,RILMSP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMSP ;

RILMSM_1 = max(min(BILMSM , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMSM =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMSM_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMSM_1,max(RILMSM_P,RILMSM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RILMSM ;

RILMSS_1 = max(min(BILMSS , IDOM11 - VARTMP1) , 0) * (1 - V_CNR) ;
RILMSS =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RILMSS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RILMSS_1,max(RILMSS_P,RILMSS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = 0 ;


RILMNP3 =  RILMOF + RILMOG + RILMOH 
          + RILMOI + RILMOJ + RILMOK + RILMOL + RILMOM + RILMON + RILMOO + RILMOP + RILMOQ + RILMOR + RILMOS + RILMOT 
	  + RILMSA + RILMSB + RILMSC + RILMSN + RILMSO + RILMSP + RILMSM + RILMSS ;
RILMNP3_1 = RILMOF_1 + RILMOG_1 + RILMOH_1 
          + RILMOI_1 + RILMOJ_1 + RILMOK_1 + RILMOL_1 + RILMOM_1 + RILMON_1 + RILMOO_1 + RILMOP_1 + RILMOQ_1 + RILMOR_1 + RILMOS_1 + RILMOT_1 
	  + RILMSA_1 + RILMSB_1 + RILMSC_1 + RILMSN_1 + RILMSO_1 + RILMSP_1 + RILMSM_1 + RILMSS_1 ;

REPMEUOJ = (BILMOJ - RILMOJ) * (1 - V_CNR) ;
REPMEUOI = (BILMOI - RILMOI) * (1 - V_CNR) ;
REPMEUOH = (BILMOH - RILMOH) * (1 - V_CNR) ;
REPMEUOG = (BILMOG - RILMOG) * (1 - V_CNR) ;
REPMEUOF = (BILMOF - RILMOF) * (1 - V_CNR) ;
REPMEUOO = (BILMOO - RILMOO) * (1 - V_CNR) ;
REPMEUON = (BILMON - RILMON) * (1 - V_CNR) ;
REPMEUOM = (BILMOM - RILMOM) * (1 - V_CNR) ;
REPMEUOL = (BILMOL - RILMOL) * (1 - V_CNR) ;
REPMEUOK = (BILMOK - RILMOK) * (1 - V_CNR) ;
REPMEUOT = (BILMOT - RILMOT) * (1 - V_CNR) ;
REPMEUOS = (BILMOS - RILMOS) * (1 - V_CNR) ;
REPMEUOR = (BILMOR - RILMOR) * (1 - V_CNR) ;
REPMEUOQ = (BILMOQ - RILMOQ) * (1 - V_CNR) ;
REPMEUOP = (BILMOP - RILMOP) * (1 - V_CNR) ;
REPMEUSA = (BILMSA - RILMSA) * (1 - V_CNR) ;
REPMEUSB = (BILMSB - RILMSB) * (1 - V_CNR) ;
REPMEUSC = (BILMSC - RILMSC) * (1 - V_CNR) ;
REPMEUSN = (BILMSN - RILMSN) * (1 - V_CNR) ;
REPMEUSO = (BILMSO - RILMSO) * (1 - V_CNR) ;
REPMEUSP = (BILMSP - RILMSP) * (1 - V_CNR) ;
REPMEUSM = (BILMSM - RILMSM) * (1 - V_CNR) ;
REPMEUSS = (BILMSS - RILMSS) * (1 - V_CNR) ;

regle 401750:
application : iliad ;


DCODMN = COD7MN ;
ACODMN_1 = min(DCODMN , PLAF_RESINEUV) * (1 - V_CNR) ;
ACODMN =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACODMN_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACODMN_1,max(ACODMN_P,ACODMN1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


DCODMW = COD7MW ;
ACODMW_1 = min(DCODMW , PLAF_RESINEUV) * (1 - V_CNR) ;
ACODMW =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACODMW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACODMW_1,max(ACODMW_P,ACODMW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
DCODMZ = COD7MZ ;
ACODMZ_1 = min(DCODMZ , PLAF_RESINEUV) * (1 - V_CNR) ;
ACODMZ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACODMZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACODMZ_1,max(ACODMZ_P,ACODMZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
DCODPZ = COD7PZ ;
ACODPZ_1 = min(DCODPZ , PLAF_RESINEUV) * (1 - V_CNR) ;
ACODPZ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACODPZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACODPZ_1,max(ACODPZ_P,ACODPZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DCODOY = COD7OY ;
ACODOY_1 = min(DCODOY , PLAF_RESINEUV) * (1 - V_CNR) ;
ACODOY =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACODOY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACODOY_1,max(ACODOY_P,ACODOY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DCODOX = COD7OX ;
ACODOX_1 =  min(DCODOX , PLAF_RESINEUV) * (1 - V_CNR) ;
ACODOX =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACODOX_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACODOX_1,max(ACODOX_P,ACODOX1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DCODOW = COD7OW ;
ACODOW_1 = min(DCODOW , PLAF_RESINEUV) * (1 - V_CNR) ;
ACODOW =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACODOW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACODOW_1,max(ACODOW_P,ACODOW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


DCODJZ = COD7JZ ;
ACODJZ_1 = min(COD7JZ , PLAF_RESINEUV) * (1 - V_CNR) ;
ACODJZ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACODJZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACODJZ_1,max(ACODJZ_P,ACODJZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DLOCIDEFG = LOCMEUBID ;

ACODID = min(LOCMEUBID , PLAF_RESINEUV)* (1 - V_CNR) ;
ALOCIDEFG_1 = (ACODID) * (1 - V_CNR) ;
ALOCIDEFG =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ALOCIDEFG_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ALOCIDEFG_1,max(ALOCIDEFG_P,ALOCIDEFG1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


DRESINEUV = LOCRESINEUV + INVNPROF1 ;

ACODIN = min(INVNPROF1 , PLAF_RESINEUV) * (1 - V_CNR) ;
ACODIJ = min(LOCRESINEUV ,( PLAF_RESINEUV - ACODIN)) * (1 - V_CNR) ;

ARESINEUV_1 = ACODIN  + ACODIJ  ; 
ARESINEUV =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ARESINEUV_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ARESINEUV_1,max(ARESINEUV_P,ARESINEUV1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

DRESIVIEU = RESIVIEU ;
ACODIM = min(RESIVIEU , PLAF_RESINEUV) * (1 - V_CNR) ;

ARESIVIEU_1 = min(RESIVIEU , PLAF_RESINEUV) * (1 - V_CNR) ;
ARESIVIEU =positif(null(V_IND_TRAIT-4)+COD9ZA) * (ARESIVIEU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ARESIVIEU_1,max(ARESIVIEU_P,ARESIVIEU1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;



RETCODMN = arr((ACODMN * TX11/100) /9) ;
RETCODMN_1 = arr((ACODMN_1 * TX11/100) /9) ;

RETCODMW = arr((ACODMW * TX11/100) /9) ;
RETCODMW_1 = arr((ACODMW_1 * TX11/100) /9) ;

RETCODMZ = arr((ACODMZ * TX11/100) /9) ;
RETCODMZ_1 = arr((ACODMZ_1 * TX11/100) /9) ;

RETCODPZ = arr((ACODPZ * TX11/100) /9) ;
RETCODPZ_1 = arr((ACODPZ_1 * TX11/100) /9) ;

RETCODOY = arr((ACODOY * TX11/100) /9) ;
RETCODOY_1 = arr((ACODOY_1 * TX11/100) /9) ;

RETCODOX = arr((ACODOX * TX11/100) /9) ;
RETCODOX_1 = arr((ACODOX_1 * TX11/100) /9) ;

RETCODOW = arr((ACODOW* TX11/100)  /9) ;
RETCODOW_1 = arr((ACODOW_1* TX11/100)  /9) ;


RETCODJZ = arr((ACODJZ* TX11/100) / 9)  ;
RETCODJZ_1 = arr((ACODJZ_1* TX11/100) / 9)  ;

RETCODID = arr((ACODID* TX11/100) /9)  ;


RETRESINEUV = arr((ACODIN * TX20 / 100) / 9) + arr((ACODIJ  * TX18 / 100) / 9) ;

RETCODIN = arr((ACODIN  * TX20 / 100) / 9) ;

RETCODIJ = arr((ACODIJ  * TX18 / 100) / 9) ;


RETRESIVIEU = arr((ACODIM * TX25 / 100) / 9) ;

RETCODIM = arr((ACODIM * TX25 / 100) / 9)  ;

regle 401770:
application : iliad ;

VARTMP1 = DEC11 + REDUCAVTCEL + RCELTOT + RILMNP1 + RILMNP3 ;

RCODIM_1 = max(min(RETCODIM , IDOM11 - VARTMP1) , 0) ;
RCODIM = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCODIM_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCODIM_1 , max(RCODIM_P,RCODIM1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCODIM ;

REPLOCIM = (RETCODIM - RCODIM) * positif(RESIVIEU + 0) * (1 - V_CNR) ;

RRESIVIEU = RCODIM ;

RCODIN_1 = max(min(RETCODIN , IDOM11 - VARTMP1) , 0) ;
RCODIN =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCODIN_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCODIN_1,max(RCODIN_P,RCODIN1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCODIN ;

REPLOCIN = (RETCODIN - RCODIN) * positif(INVNPROF1 + 0) * (1 - V_CNR) ;

RCODIJ_1 = max(min(RETCODIJ , IDOM11 - VARTMP1) , 0) ;
RCODIJ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCODIJ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCODIJ_1,max(RCODIJ_P,RCODIJ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCODIJ ;

REPLOCIJ = (RETCODIJ - RCODIJ) * positif(LOCRESINEUV + 0) * (1 - V_CNR) ;

RRESINEUV = RCODIN + RCODIJ ;

RCODID_1 = max(min(RETCODID , IDOM11 - VARTMP1) , 0) ;
RCODID =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCODID_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCODID_1,max(RCODID_P,RCODID1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCODID ;

REPLOCID = (RETCODID - RCODID) * positif(LOCMEUBID + 0) * (1 - V_CNR) ;

RLOCIDEFG = RCODID ;

RCODJZ_1 = max(min(RETCODJZ , IDOM11 - VARTMP1) , 0) ;
RCODJZ = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCODJZ_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RCODJZ_1 , max(RCODJZ_P,RCODJZ1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCODJZ ;

RCODJZ = RCODJZ ;

REPLOCJZ = (RETCODJZ - RCODJZ) * positif(COD7JZ + 0) * (1 - V_CNR) ;


RCODOW_1 = max(min(RETCODOW , IDOM11 - VARTMP1) , 0) ;
RCODOW =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCODOW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCODOW_1,max(RCODOW_P,RCODOW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCODOW ;

REPMEUOW = (RETCODOW - RCODOW) * positif(COD7OW + 0) * (1 - V_CNR) ;

RCODOX_1 = max(min(RETCODOX , IDOM11 - VARTMP1) , 0) ;
RCODOX =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCODOX_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCODOX_1,max(RCODOX_P,RCODOX1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCODOX ;

REPMEUOX = (RETCODOX - RCODOX) * positif(COD7OX + 0) * (1 - V_CNR) ;

RCODOY_1 = max(min(RETCODOY , IDOM11 - VARTMP1) , 0) ;
RCODOY =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCODOY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCODOY_1,max(RCODOY_P,RCODOY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCODOY ;

REPMEUOY = (RETCODOY - RCODOY) * positif(COD7OY + 0) * (1 - V_CNR) ;

RCODPZ_1 = max(min(RETCODPZ , IDOM11 - VARTMP1) , 0) ;
RCODPZ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCODPZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCODPZ_1,max(RCODPZ_P,RCODPZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCODPZ ;

REPMEUPZ = (RETCODPZ - RCODPZ) * positif(COD7PZ + 0) * (1 - V_CNR) ;

RCODMZ_1 = max(min(RETCODMZ , IDOM11 - VARTMP1) , 0) ;
RCODMZ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCODMZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCODMZ_1,max(RCODMZ_P,RCODMZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RCODMZ ;

REPMEUMZ = (RETCODMZ - RCODMZ) * positif(COD7MZ + 0) * (1 - V_CNR) ;

RCODMW_1 = max(min(RETCODMW , IDOM11 - VARTMP1) , 0) ;
RCODMW = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCODMW_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCODMW_1 , max(RCODMW_P,RCODMW1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RCODMW ;

REPMEUMW = (RETCODMW - RCODMW) * positif(COD7MW + 0) * (1 - V_CNR) ;

RCODMN_1 = max(min(RETCODMN , IDOM11 - VARTMP1) , 0) ;
RCODMN = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RCODMN_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34  -CMAJ)))
         + (max(0 , min(RCODMN_1 , max(RCODMN_P,RCODMN1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

REPMEUMN = (RETCODMN - RCODMN) * positif(COD7MN + 0) * (1 - V_CNR) ;


DILMNP4 = DRESIVIEU + DRESINEUV + DLOCIDEFG + DCODJZ + DCODOW + DCODOX + DCODOY + DCODPZ + DCODMZ + DCODMW + DCODMN ;

AILMNP4 = ARESIVIEU + ARESINEUV + ALOCIDEFG + ACODJZ + ACODOW + ACODOX + ACODOY + ACODPZ + ACODMZ + ACODMW + ACODMN ;

RILMNP4 = RRESIVIEU + RRESINEUV + RLOCIDEFG + RCODJZ + RCODOW + RCODOX + RCODOY + RCODPZ + RCODMZ + RCODMW + RCODMN ;
RILMNP4_1 = RETRESIVIEU + RETRESINEUV + RETCODIJ + RETCODID + RETCODJZ_1 + RETCODOV_1 + RETCODOW_1 + RETCODOX_1 + RETCODOY_1 + RETCODPZ_1 + RETCODMZ_1 + RETCODMW_1 + RETCODMN_1 ;

RLOCNPRO = RILMNP1 + RILMNP3 + RILMNP4 ;

RLOCNPRO_1 = RILMNP1_1 + RILMNP3_1 + RILMNP4_1 ;

regle 401810:
application : iliad ;


REP13MEU = REPLOCJZ + REPMEUOF + REPMEUOW + REPMEUOK + REPMEUOX + REPMEUOY + REPMEUOP + REPMEUSA + REPMEUPZ + REPMEUMZ  + REPMEUMW + REPMEUSN + REPMEUSP + REPMEUSM + REPMEUSS + REPMEUMN ;

REP12MEU = REPLOCID + REPMEUOG + REPMEUOL + REPMEUOQ + REPMEUSB + REPMEUSO ; 

REP11MEU = REPLOCIN + REPLOCIJ + REPMEUOH + REPMEUOM + REPMEUOR + REPMEUSC ;

REP10MEU = REPLOCIM + REPMEUOI + REPMEUON + REPMEUOS ;

REP9MEU = REPMEUOJ + REPMEUOO + REPMEUOT ; 
           
regle 401820:
application : iliad ;

RCODMN1 = arr((ACODMN * TX11/100)/9) ;
RCODMN8 = (arr(min(PLAF_RESINEUV , COD7MN) * TX11/100) - 8 * RCODMN1) * (1 - V_CNR) ;

REPLOCMN = (RCODMN8 + RCODMN1 * 7) ;

RCODMW1 = arr((ACODMW * TX11/100)/9) ;
RCODMW8 = (arr(min(PLAF_RESINEUV , COD7MW) * TX11/100) - 8 * RCODMW1) * (1 - V_CNR) ;

REPLOCMW = (RCODMW8 + RCODMW1 * 7) ;

RCODMZ1 = arr((ACODMZ * TX11/100)/9) ;
RCODMZ8 = (arr(min(PLAF_RESINEUV , COD7MZ) * TX11/100) - 8 * RCODMZ1) * (1 - V_CNR) ;

REPLOCMZ = (RCODMZ8 + RCODMZ1 * 7) ;

RCODPZ1 = arr((ACODPZ * TX11/100)/9) ;
RCODPZ8 = (arr(min(PLAF_RESINEUV , COD7PZ) * TX11/100) - 8 * RCODPZ1) * (1 - V_CNR) ;

REPLOCPZ = (RCODPZ8 + RCODPZ1 * 7) ;

RCODOY1 = arr((ACODOY * TX11/100)/9) ;
RCODOY8 = (arr(min(PLAF_RESINEUV , COD7OY) * TX11/100) - 8 * RCODOY1) * (1 - V_CNR) ;

REPLOCOY = (RCODOY8 + RCODOY1 * 7) ;

RCODOX1 = arr((ACODOX * TX11/100)/9) ;
RCODOX8 = (arr(min(PLAF_RESINEUV , COD7OX) * TX11/100) - 8 * RCODOX1) * (1 -V_CNR) ;

REPLOCOX = (RCODOX8 + RCODOX1 * 7) ;

RCODOW1 = arr((ACODOW * TX11/100)/9) ;
RCODOW8 = (arr(min(PLAF_RESINEUV , COD7OW) * TX11/100) - 8 * RCODOW1) * (1 -V_CNR) ;

REPLOCOW = (RCODOW8 + RCODOW1 * 7) ;



RCODJZ1 = arr(arr(ACODJZ * TX11/100) / 9) ;
RCODJZ8 = (arr(min(PLAF_RESINEUV ,COD7JZ) * TX11/100) - 8 * RCODJZ1) * (1 -V_CNR) ; 

REPLNPT = (RCODJZ1 * 7) + RCODJZ8 ;

RLOCIDFG1 = arr(arr(ACODID * TX11/100) / 9) ;
RLOCIDFG8 = arr(ACODID * TX11/100) - (8 * RLOCIDFG1) ;

REPLOCIDFG = (RLOCIDFG1 * 7) + RLOCIDFG8 ;

RESINEUV1 = arr((ACODIN* TX20/100) / 9) + arr((ACODIJ * TX18/100) / 9) ;
RESINEUV8 = arr(ACODIN * TX20/100) + arr(ACODIJ * TX18/100) - (8 * RESINEUV1) ;

REPINVLOCNP = (RESINEUV1 * 7) + RESINEUV8 ;

RESIVIEU1 = arr((ACODIM * TX25/100) / 9) ;
RESIVIEU8 = arr(ACODIM * TX25/100) - (8 * RESIVIEU1) ;

REPINVIEU = (RESIVIEU1 * 7) + RESIVIEU8 ;

regle 401830:
application : iliad ;


BSOCREP = min(RSOCREPRISE , LIM_SOCREPR * ( 1 + BOOL_0AM)) ;

RSOCREP = arr ( TX_SOCREPR/100 * BSOCREP ) * (1 - V_CNR);

DSOCREPR = RSOCREPRISE;

ASOCREPR = (positif(null(V_IND_TRAIT-4) + COD9ZA) * BSOCREP * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
            + (max(0 , min(BSOCREP,max(BSOCREP_P,BSOCREP1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5) + 0) * (1 - V_CNR) ;

RSOCREPR_1 = max( min( RSOCREP , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE
                                             -RFORET-RFIPDOM-RFIPC-RCINE-RRESTIMO) , 0 );
RSOCREPR =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOCREPR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSOCREPR_1,max(RSOCREPR_P,RSOCREPR1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401835:
application : iliad ;



RCOD7KX_1 = max(0 , min(COD7KX , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RFIPDOM-RFIPC-RCINE-RRESTIMO-RSOCREPR
                                     -RRPRESCOMP-RHEBE-RSURV-RINNO-RSOUFIP-RRIRENOV-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE
				     -RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTOT-RFOR-RREHAB))  * (1 - null(V_REGCO - 2));
RCOD7KX =(positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCOD7KX_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCOD7KX_1,max(RCOD7KX_P,RCOD7KX1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0)  * (1 - null(V_REGCO - 2));

REPRESTKX = max(0 , COD7KX - RCOD7KX) * (1 - null(V_REGCO - 2)) ;

RCOD7KW_1 = max(0 , min(COD7KW , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RFIPDOM-RFIPC-RCINE-RRESTIMO-RSOCREPR
                                     -RRPRESCOMP-RHEBE-RSURV-RINNO-RSOUFIP-RRIRENOV-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE
				     -RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTOT-RFOR-RREHAB-RCOD7KX))  * (1 - null(V_REGCO - 2));
RCOD7KW =(positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCOD7KW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCOD7KW_1,max(RCOD7KW_P,RCOD7KW1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0)  * (1 - null(V_REGCO - 2));

REPRESTKW = max(0 , COD7KW - RCOD7KW) * (1 - null(V_REGCO - 2)) ;

RCOD7KZ_1 = max(0 , min(COD7KZ , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RFIPDOM-RFIPC-RCINE-RRESTIMO-RSOCREPR
                                     -RRPRESCOMP-RHEBE-RSURV-RINNO-RSOUFIP-RRIRENOV-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE
				     -RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTOT-RFOR-RREHAB-RCOD7KX-RCOD7KW))  * (1 - null(V_REGCO - 2));
RCOD7KZ =(positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCOD7KZ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCOD7KZ_1,max(RCOD7KZ_P,RCOD7KZ1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0)  * (1 - null(V_REGCO - 2));

REPRESTKZ = max(0 , COD7KZ - RCOD7KZ) * (1 - null(V_REGCO - 2)) ;

DRESTREP = COD7KX + COD7KW+COD7KZ;
ARESTREP = DRESTREP ;
RRESTREP = RCOD7KX + RCOD7KW+RCOD7KZ; 
RRESTREP_1 = RCOD7KX_1 + RCOD7KW_1+RCOD7KZ_1; 

regle 401840:
application : iliad ;


DRESTIMO = COD7NX + COD7NY ;


DRESTIMO1 = COD7TX + COD7TY ;


RESTIMONX = min(COD7NX , LIM_RESTIMO) ;

RRESTIMONX_1 = max(min(RESTIMONX * TX30/100 , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RFIPDOM
                                                  -RDIFAGRI-RPRESSE-RFORET-RFIPC-RCINE) , 0) ;
RRESTIMONX =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RRESTIMONX_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RRESTIMONX_1,max(RRESTIMONX_P,RRESTIMONX1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RESTIMONY = min(COD7NY , max(0,LIM_RESTIMO - RESTIMONX)) ;

RRESTIMONY_1 = max(min(RESTIMONY * TX22/100 , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RFIPDOM
                                                  -RDIFAGRI-RPRESSE-RFORET-RFIPC-RCINE-RRESTIMONX ) , 0) ;
RRESTIMONY =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RRESTIMONY_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RRESTIMONY_1,max(RRESTIMONY_P,RRESTIMONY1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401844:
application : iliad ;


RESTIMOTX = min(COD7TX , max(0,LIM_RESTIMO1 - V_BTDRIMM3*(1-present(COD7SU)) - V_BTDRIMM2 * (1-present(COD7SV))-V_BTDRIMM1*(1-present(COD7SW))- COD7SU-COD7SV-COD7SW)) ;
											                                            

RESTIMOTY = min(COD7TY , max(0,(LIM_RESTIMO1-RESTIMOTX - V_BTDRIMM3*(1-present(COD7SU))- V_BTDRIMM2 * (1-present(COD7SV)) -V_BTDRIMM1*(1-present(COD7SW))- COD7SU-COD7SV-COD7SW))) ;


regle 401845:
application : iliad ;

ARESTIMO_1 = (RESTIMONX + RESTIMONY) * (1 - V_CNR) ;
ARESTIMO = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ARESTIMO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(ARESTIMO_1,max(ARESTIMO_P,ARESTIMO1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RETRESTIMO = arr((RESTIMONX * TX30/100) + (RESTIMONY * TX22/100)) * (1 - V_CNR) ;

RRESTIMO_1 = max (min (RETRESTIMO , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RFIPDOM
                               -RDIFAGRI-RPRESSE-RFORET-RFIPC-RCINE) , 0) ;
RRESTIMO =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RRESTIMO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RRESTIMO_1,max(RRESTIMO_P,RRESTIMO1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401847:
application : iliad ;

ARESTIMO1_1 = (RESTIMOTX + RESTIMOTY) * (1 - V_CNR) ;
ARESTIMO1 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ARESTIMO1_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
            + (max(0,min(ARESTIMO1_1,max(ARESTIMO1_P,ARESTIMO11731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RETRESTIMO_2 = arr((RESTIMOTX * TX30/100) + (RESTIMOTY * TX22/100) ) * (1 - V_CNR) ;

RRESTIMO1_1 = max(min(RETRESTIMO_2 , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RFIPDOM-RFIPC
                                           -RCINE-RRESTIMO-RSOCREPR-RRPRESCOMP-RHEBE-RSURV- RINNO-RSOUFIP-RRIRENOV-RLOGDOM-RCOMP- RRETU-RDONS
					   -CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTOT-RFOR-RREHAB-RRESTREP),0);
RRESTIMO1 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RRESTIMO1_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RRESTIMO1_1,max(RRESTIMO1_P,RRESTIMO11731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;


A12RRESTIMO = RRESTIMO * (1 - V_CNR) ;

REPRESTXY = max(0 , RETRESTIMO_2 - RRESTIMO1) * (1 - V_CNR) ;

regle 401850:
application : iliad ;

REVDON = max(0 , RBL1 + TOTALQUOHT - SDDD - SDC1) ;


BDON7UH = min(LIM15000 , COD7UH) ;

BASEDONB = REPDON03 + REPDON04 + REPDON05 + REPDON06 + REPDON07 + RDDOUP + COD7UH + DONAUTRE ;
BASEDONP = RDDOUP + BDON7UH + DONAUTRE + EXCEDANTA + EXCEDANTD ;

BONS = null(4 - V_IND_TRAIT) * arr(min(REPDON03 + REPDON04 + REPDON05 + REPDON06 + REPDON07 + BASEDONP , REVDON * TX_BASEDUP/100))
      + null(5 - V_IND_TRAIT) * arr(min(REPDON03 + REPDON04 + REPDON05 + REPDON06 + REPDON07 + BASEDONP , min(REVDON,REVDON1731) * TX_BASEDUP/100)) ;

regle 401860:
application : iliad ;


BASEDONF = null(4 - V_IND_TRAIT) * min(REPDON03 , arr(REVDON * TX_BASEDUP/100))
           + null(5 - V_IND_TRAIT) * min(REPDON03 , arr(min(REVDON,REVDON1731) * TX_BASEDUP/100)) ;
REPDON = null(4 - V_IND_TRAIT) * max(BASEDONF + REPDON04 + REPDON05 + REPDON06 + REPDON07 + BASEDONP - arr(REVDON * TX_BASEDUP/100) , 0) * (1 - V_CNR)
      +  null(5 - V_IND_TRAIT) * max(BASEDONF + REPDON04 + REPDON05 + REPDON06 + REPDON07 + BASEDONP - arr(min(REVDON,REVDON1731) * TX_BASEDUP/100) , 0) * (1 - V_CNR) ;

REPDONR4 = (null(4 - V_IND_TRAIT) * (positif_ou_nul(BASEDONF - arr(REVDON * (TX_BASEDUP)/100)) * REPDON04
                  + (1 - positif_ou_nul(BASEDONF - arr(REVDON * (TX_BASEDUP)/100)))
                * max(REPDON04 + (BASEDONF - arr(REVDON * (TX_BASEDUP)/100)),0))
           + null(5 - V_IND_TRAIT) * (positif_ou_nul(BASEDONF - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)) * REPDON04
               + (1 - positif_ou_nul(BASEDONF - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)))
                     * max(REPDON04 + (BASEDONF - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)),0))
                )
           * (1 - V_CNR);

REPDONR3 = (null(4 - V_IND_TRAIT) * (positif_ou_nul(BASEDONF + REPDON04 - arr(REVDON * (TX_BASEDUP)/100)) * REPDON05
               + (1 - positif_ou_nul(BASEDONF + REPDON04 - arr(REVDON * (TX_BASEDUP)/100)))
                     * max(REPDON05 + (BASEDONF + REPDON04 - arr(REVDON * (TX_BASEDUP)/100)),0))
                + null(5 - V_IND_TRAIT) * (positif_ou_nul(BASEDONF + REPDON04 - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)) * REPDON05
            + (1 - positif_ou_nul(BASEDONF + REPDON04 - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)))
                  * max(REPDON05 + (BASEDONF + REPDON04 - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)),0))
             )
               * (1 - V_CNR);

REPDONR2 = (null(4 - V_IND_TRAIT) * (positif_ou_nul(BASEDONF + REPDON04 + REPDON05 - arr(REVDON * (TX_BASEDUP)/100)) * REPDON06
            + (1 - positif_ou_nul(BASEDONF + REPDON04 + REPDON05 - arr(REVDON * (TX_BASEDUP)/100)))
                  * max(REPDON06 + (BASEDONF + REPDON04 + REPDON05 - arr(REVDON * (TX_BASEDUP)/100)),0))
            +  null(5 - V_IND_TRAIT) * (positif_ou_nul(BASEDONF + REPDON04 + REPDON05 - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)) * REPDON06
                 + (1 - positif_ou_nul(BASEDONF + REPDON04 + REPDON05 - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)))
             * max(REPDON06 + (BASEDONF + REPDON04 + REPDON05 - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)),0))
                  )
             * (1 - V_CNR);
REPDONR1 = (null(4 - V_IND_TRAIT) * (positif_ou_nul(BASEDONF + REPDON04 + REPDON05 + REPDON06 - arr(REVDON * (TX_BASEDUP)/100)) * REPDON07
            + (1 - positif_ou_nul(BASEDONF + REPDON04 + REPDON05 + REPDON06 - arr(REVDON * (TX_BASEDUP)/100)))
                  * max(REPDON07 + (BASEDONF + REPDON04 + REPDON05 + REPDON06 - arr(REVDON * (TX_BASEDUP)/100)),0))
             +  null(5 - V_IND_TRAIT) * (positif_ou_nul(BASEDONF + REPDON04 + REPDON05 + REPDON06 - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)) * REPDON07
                 + (1 - positif_ou_nul(BASEDONF + REPDON04 + REPDON05 + REPDON06 - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)))
               * max(REPDON07 + (BASEDONF + REPDON04 + REPDON05 + REPDON06 - arr(min(REVDON,REVDON1731) * (TX_BASEDUP)/100)),0))
                  )
             * (1 - V_CNR) ;

REPDONR = max(REPDON - REPDONR1 - REPDONR2 - REPDONR3 - REPDONR4 , 0) * (1 - V_CNR) ;
regle 401870:
application : iliad ;


RONS = arr(BONS * TX_REDDON /100) * (1 - V_CNR) ;

DDONS = REPDON03 + REPDON04 + REPDON05 + REPDON06 + REPDON07 + RDDOUP + COD7UH + DONAUTRE ;

ADONS_1 = BONS * (1 - V_CNR) ;
ADONS = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ADONS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
        + (max(0,min(ADONS_1,max(ADONS_P,ADONS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401880:
application : iliad ;

RDONS_1 = max(min(RONS , RRI1-RLOGDOM-RCOMP-RRETU) , 0) ;
RDONS =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RDONS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RDONS_1,max(RDONS_P,RDONS1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401885:
application : iliad ;

CRCFA = arr(IPQ1 * REGCIAUTO / (RB01 + TONEQUO)) * (1 - positif(RE168 + TAX1649)) ;

regle 401887:
application : iliad ;

CRDIE = max( min( CRCFA , RRI1-RLOGDOM-RCOMP-RRETU-RDONS) , 0 ) ;

regle 401890:
application : iliad ;


SEUILRED1 = arr((arr(RI1)+REVQUO) / NBPT) ;

regle 401900:
application : iliad ;


RETUD = arr((RDENS * MTRC) + (RDENL * MTRL) + (RDENU * MTRS) + (RDENSQAR * MTRC /2) + (RDENLQAR * MTRL /2) + (RDENUQAR * MTRS /2)) 
        * (1 - V_CNR) ;

DNBE = RDENS + RDENL + RDENU + RDENSQAR + RDENLQAR + RDENUQAR ;

RNBE = DNBE ;

regle 401910:
application : iliad ;

RRETU_1 = max(min(RETUD , RRI1-RLOGDOM-RCOMP) , 0) ;
RRETU =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RRETU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RRETU_1,max(RRETU_P,RRETU1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401920:
application : iliad ;


BFCPIGR = min(COD7GR , PLAF_FCPI * (1 + BOOL_0AM)) * (1 - V_CNR) ;
BFCPIGQ = min(FCPI , max(0 , PLAF_FCPI * (1 + BOOL_0AM) - COD7GR)) * (1 - V_CNR) ;

DFCPI = FCPI + COD7GR ;
BFCPI = (BFCPIGR + BFCPIGQ) * (1 - V_CNR) ;

RFCPI = arr(BFCPIGQ * TX_FCPI/100 + BFCPIGR * TX25/100) * (1 - V_CNR) ; 

RINNO_1 = max(0 , min(RFCPI , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RFIPDOM-RDIFAGRI-RPRESSE-RFORET
                                  -RFIPC-RCINE-RRESTIMO-RSOCREPR-RRPRESCOMP-RHEBE-RSURV)) ;
RINNO =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RINNO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RINNO_1,max(RINNO_P,RINNO1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401930:
application : iliad ;


BPRESCOMP =(RDPRESREPORT 
	   + (1-positif(RDPRESREPORT+0)) * 
	   arr(
	         ((1 - present(SUBSTITRENTE)) * 
                  arr (
                 null(PRESCOMP2000 - PRESCOMPJUGE)
                   * min(PLAFPRESCOMP,PRESCOMP2000)
	         + positif(PRESCOMPJUGE - PRESCOMP2000)
                   * (positif_ou_nul(PLAFPRESCOMP -PRESCOMPJUGE))
                   * PRESCOMP2000
	         + positif(PRESCOMPJUGE - PRESCOMP2000)
                    * (1 - positif_ou_nul(PLAFPRESCOMP -PRESCOMPJUGE))
                    * PLAFPRESCOMP * PRESCOMP2000/PRESCOMPJUGE
                       )
	       +
             present(SUBSTITRENTE) *
                  arr (
                   null(PRESCOMP2000 - SUBSTITRENTE)
		   * 
		   (positif_ou_nul(PLAFPRESCOMP - PRESCOMPJUGE)*SUBSTITRENTE
		   + 
		   positif(PRESCOMPJUGE-PLAFPRESCOMP)*arr(PLAFPRESCOMP*SUBSTITRENTE/PRESCOMPJUGE))
                 + 
		   positif(SUBSTITRENTE - PRESCOMP2000)
		   * (positif_ou_nul(PLAFPRESCOMP - PRESCOMPJUGE)*PRESCOMP2000
		   + 
		   positif(PRESCOMPJUGE-PLAFPRESCOMP)*arr(PLAFPRESCOMP*(SUBSTITRENTE/PRESCOMPJUGE)*(PRESCOMP2000/SUBSTITRENTE)))
                       )
	           )
                )
              )
               *(1 - V_CNR);


RPRESCOMP = arr (BPRESCOMP * TX_PRESCOMP / 100) * (1 -V_CNR);
BPRESCOMP01 = max(0,(1 - present(SUBSTITRENTE)) * 
                   (  positif_ou_nul(PLAFPRESCOMP -PRESCOMPJUGE)
                       * (PRESCOMPJUGE - BPRESCOMP)
                     + positif(PRESCOMPJUGE - PLAFPRESCOMP)
                       * (PLAFPRESCOMP - BPRESCOMP)
                   )
	       +
             present(SUBSTITRENTE) *
                   (  positif_ou_nul(PLAFPRESCOMP -PRESCOMPJUGE)
                       * (SUBSTITRENTE - PRESCOMP2000)
                     + positif(PRESCOMPJUGE - PLAFPRESCOMP)
		     *arr(PLAFPRESCOMP*(SUBSTITRENTE/PRESCOMPJUGE)*((SUBSTITRENTE-PRESCOMP2000)/SUBSTITRENTE))
                   )
		* (1 - V_CNR)
		);
DPRESCOMP = PRESCOMP2000 + RDPRESREPORT ;

APRESCOMP = (positif(null(V_IND_TRAIT-4) + COD9ZA) * BPRESCOMP * (1-positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
             + (max(0 , min(BPRESCOMP,max(BPRESCOMP_P,BPRESCOMP1731))) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0) * (1 - V_CNR) ;

RRPRESCOMP_1 = max( min( RPRESCOMP , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RFIPDOM-RDIFAGRI-RPRESSE-RFORET
                                                 -RFIPC-RCINE-RRESTIMO-RSOCREPR) , 0) ;
RRPRESCOMP =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RRPRESCOMP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RRPRESCOMP_1,max(RRPRESCOMP_P,RRPRESCOMP1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RPRESCOMPREP = max( min( RPRESCOMP , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RFIPDOM-RDIFAGRI-RPRESSE-RFORET
				      -RFIPC-RCINE-RRESTIMO-RSOCREPR) , 0) * positif(RDPRESREPORT) ;

RPRESCOMPAN = RRPRESCOMP * (1-positif(RDPRESREPORT)) ;

regle 401940:
application : iliad ;
                                     

DCOTFOR = COTFORET ;

ACOTFOR_R = min(DCOTFOR , PLAF_FOREST1 * (1 + BOOL_0AM)) * (1 - V_CNR) ;

ACOTFOR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACOTFOR_R) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACOTFOR_R,max(ACOTFOR_P,ACOTFOR1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RCOTFOR_1 = max( min( arr(ACOTFOR_R * TX76/100) , IDOM11-DEC11) , 0) ;

RCOTFOR =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RCOTFOR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RCOTFOR_1,max(RCOTFOR_P,RCOTFOR1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 401950:
application : iliad ;


DFOREST = RDFOREST;


AFOREST_1 = min(RDFOREST, PLAF_FOREST * (1 + BOOL_0AM)) * (1 - V_CNR) ; 
AFOREST = positif(null(V_IND_TRAIT-4)+COD9ZA) * (AFOREST_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(AFOREST_1,max(AFOREST_P,AFOREST1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;



RFOREST = arr( max(0 , AFOREST * TX18/100)) * (1 - V_CNR) ;

regle 401960:
application : iliad ;

RFOR_1 = max(min(RFOREST , IDOM11-DEC11-RCOTFOR-RREPA-RLOCANAH-RDONDJ-RDIFAGRI-RPRESSE-RFORET-RFIPDOM-RFIPC-RCINE-RRESTIMO-RSOCREPR-RRPRESCOMP-RHEBE
                                 -RSURV-RINNO-RSOUFIP-RRIRENOV-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTOT) , 0) ;

RFOR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RFOR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
       + (max(0 , min(RFOR_1 , max(RFOR_P,RFOR1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

regle 401980:
application : iliad ;


BDIFAGRI = min(INTDIFAGRI , LIM_DIFAGRI * (1 + BOOL_0AM)) * (1 - V_CNR) ;

DDIFAGRI = INTDIFAGRI ;

ADIFAGRI = positif(null(V_IND_TRAIT-4)+COD9ZA) * (BDIFAGRI) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(BDIFAGRI,max(BDIFAGRI_P,BDIFAGRI1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RAGRI = arr(BDIFAGRI * TX_DIFAGRI / 100) ;

RDIFAGRI_1 = max(min(RAGRI , IDOM11-DEC11-RCOTFOR-RREPA - RLOCANAH - RDONDJ) , 0) ;
RDIFAGRI = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RDIFAGRI_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(RDIFAGRI_1 , max(RDIFAGRI_P,RDIFAGRI1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

regle 401990:
application : iliad ;


ITRED = min( RED , IDOM11-DEC11) ;

regle 402000:
application : iliad ;


NNRI2 = max(0 , RRI1 - (DLOGDOM + ACOMP + RETUD + RONS + CRCFA + ADUFREP + APIREP + APROPIREP1 + APROPIREP + ANORMREP + RPINABCD + RPINRRS + RNORABCD
			+ RSN + COD7CY + COD7DY + COD7EY + COD7FY + COD7GY + COD7EK + RFOREST 
                        + CELRREDLQ + CELRREDLR + CELRREDLU + CELRREDLV 
                        + COD7LA + COD7LB + COD7LC + COD7LY + COD7MS + COD7MT + COD7MU + COD7MQ + COD7MC + COD7MV + COD7MO + COD7MA + COD7MP + COD7MB + COD7MR + COD7MD
			+ COD7MI + COD7MJ + COD7MK + COD7ML
			+ COD7YI + COD7ZI + COD7ZP + COD7XP + COD7YJ + COD7ZJ + COD7ZO + COD7XO + COD7YK + COD7ZK + COD7XQ + COD7YL + COD7ZL + COD7UU + COD7UV + COD7UW + COD7UX
			+ COD7RK + COD7RL + COD7RM + COD7RN
			+ COD7HZ + COD7KC + COD7KD + COD7PC + COD7PD + COD7PE + COD7KT + COD7KU + COD7KV + ACELHA + ACELHJ + ACELHK + ACELHN + ACELHY 
			+ ACELGS + ACELGU + ACELGX + ACELZM + ACELWX + ACELWY + ACELWZ + ACELSOM4 + ACELSOM6 
			+ RCEL7SR + RCEL7YZ + RCEL7SL + RCEL7SQ + RCEL7YX + RCEL7YY + RCEL7SH + RCEL7SI + RCEL7SJ + RCEL7SK + RCEL7XH + RCEL7XI + RCEL7XJ + RCEL7XK
			+ RCEL7IA + RCEL7IB + RCEL7IC + RCEL7IE + RCEL7KJ + RCEL7KL + RCEL7KN + RCEL7SD + RCEL7SE + RCEL7SF + RCEL7SG + RCEL7WD + RCEL7WE
			+ RCEL7WF + RCEL7WG + RCEL7IF + RCEL7IG + RCEL7IH + RCEL7IO + RCEL7KO + RCEL7KQ + RCEL7KR + RCEL7IP + RCEL7WC + RCEL7KS 
			+ RCEL7IQ + RCEL7HL + RCEL7HM + RCEL7LD + RCEL7LE + RCEL7LF + RCEL7LN + RCEL7BA + RCEL7BB + RCEL7BC + RCEL7BD + RCEL7LT + RCEL7LX + RCEL7LZ 
			+ RCEL7MG + RCEL7BE + RCEL7BF + RCEL7BG + RCEL7BH + RCEL7MH + RCEL7BJ
			+ RETCODID + RETCODIJ + RETCODIM + RETCODIN + RETCODJZ + RETCODOU + RETCODOV + RETCODOW + RETCODOX + RETCODOY + RETCODPZ + RETCODMZ + RETCODMW + RETCODMN
			+ COD7KX + COD7KW + COD7KZ + AILMNP1 + AILMNP3 + RRREHAP + arr(RESTIMOTX*TX30/100) + arr(RESTIMOTY*TX22/100))) ; 

regle 402010:
application : iliad ;




DLOGDOM = INVLOG2008 + INVLGDEB2009 + INVLGDEB + INVLGAUTRE + INVLGDEB2010 + INVLOG2009 + INVOMLOGOA + INVOMLOGOB 
          + INVOMLOGOC + INVOMLOGOH + INVOMLOGOI + INVOMLOGOJ + INVOMLOGOK + INVOMLOGOL + INVOMLOGOM + INVOMLOGON 
          + INVOMLOGOO + INVOMLOGOP + INVOMLOGOQ + INVOMLOGOR + INVOMLOGOS + INVOMLOGOT + INVOMLOGOU + INVOMLOGOV 
          + INVOMLOGOW 
          + CODHOD + CODHOE + CODHOF + CODHOG + CODHOX + CODHOY + CODHOZ + CODHUA + CODHUB + CODHUC + CODHUD + CODHUE 
          + CODHUF + CODHUG + CODHUH + CODHUI + CODHUJ + CODHUK + CODHUL + CODHUM + CODHUN + CODHUO + CODHUP + CODHUQ
	  + CODHUR + CODHUS + CODHUT + CODHUU + CODHVA + CODHVB + CODHVC + CODHVD + CODHVE + CODHVF + CODHVG + CODHVH 
	  + CODHVI + CODHVJ + CODHVK + CODHVL ;


DDOMSOC1 = CODHXU + CODHXQ + CODHXR + CODHXS + CODHXT 
           + CODHYA + CODHYB + CODHYC + CODHYD + CODHYE + CODHYF ;

DLOGSOC = CODHYG ;


DCOLENT = CODHDI + CODHDJ + CODHDK + CODHDM + CODHDN + CODHDO + CODHDP + CODHDR + CODHDS + CODHDT + CODHDU + CODHDW + CODHEN + CODHEO 
	  + CODHEP + CODHER + CODHES + CODHET + CODHEU + CODHEW + CODHFN + CODHFO + CODHFP + CODHFR + CODHFS + CODHFT + CODHFU + CODHFW 
	  + CODHGS + CODHGT + CODHGU + CODHGW + CODHHS + CODHHT + CODHHU + CODHHW ;

DLOCENT = CODHIS + CODHIT + CODHIU + CODHIW ;

regle 402020:
application : iliad ;



TOTALPLAF1 = INVRETXU + INVRETXQ 
	     + INVRETXR + INVRETXS + INVRETXT + INVRETYB + INVRETYA + INVRETYD + INVRETYC + INVRETYE + INVRETYF + INVRETYG + INVRETQL + INVRETQM 
	     + INVRETQD + INVRETOB + INVRETOC + INVRETOM + INVRETON + INVRETOD + INVRETUA + INVRETUH + INVRETUO + INVRETVA
	     + INVRETXUR 
	     + INVRETXQR + INVRETXRR + INVRETXSR + INVRETXTR + INVRETYBR + INVRETYAR + INVRETYDR + INVRETYCR + INVRETYER + INVRETYFR + INVRETYGR
	     ;

TOTALPLAF2 = INVRETOI + INVRETOJ + INVRETOK + INVRETOP + INVRETOQ + INVRETOR + INVRETOE + INVRETOF + INVRETUB + INVRETUC 
             + INVRETUI + INVRETUJ + INVRETUP + INVRETUQ + INVRETVB + INVRETVC ;

TOTALPLAF3 = INVRETDT + INVRETDJ + INVRETDO + INVRETDS + INVRETDI + INVRETDN + INVRETET + INVRETEO + INVRETES + INVRETEN 
	     + INVRETEP + INVRETEU + INVRETER + INVRETEW + INVRETFT + INVRETFO + INVRETFS + INVRETFN + INVRETFP + INVRETFU + INVRETFR 
	     + INVRETFW + INVRETGT + INVRETGS + INVRETGU + INVRETGW + INVRETHT + INVRETHS + INVRETHU + INVRETHW + INVRETIT + INVRETIS + INVRETIU + INVRETIW
	     + INVRETDK + INVRETDP 
	     + INVRETDU + INVRETDM + INVRETDR + INVRETDW + INVRETOT + INVRETOU + INVRETOV + INVRETOW + INVRETOG + INVRETOX + INVRETOY 
	     + INVRETOZ + INVRETUD + INVRETUE + INVRETUF + INVRETUG + INVRETUK + INVRETUL + INVRETUM + INVRETUN + INVRETUR + INVRETUS 
	     + INVRETUT + INVRETUU + INVRETVD + INVRETVE + INVRETVF + INVRETVG + INVRETVH + INVRETVI + INVRETVJ + INVRETVK + INVRETVL
	     + INVRETDTR + INVRETDJR + INVRETDOR + INVRETDSR 
	     + INVRETDIR + INVRETDNR + INVRETETR + INVRETEOR + INVRETESR + INVRETENR + INVRETFTR + INVRETFOR + INVRETFSR + INVRETFNR 
	     + INVRETGTR + INVRETGSR + INVRETHTR + INVRETHSR + INVRETITR + INVRETISR ;

RNIDOM1 = arr((RNG + TTPVQ) * TX15/100) * (1 - V_CNR) ;

RNIDOM2 = arr((RNG + TTPVQ) * TX13/100) * (1 - V_CNR) ;

RNIDOM3 = arr((RNG + TTPVQ) * TX11/100) * (1 - V_CNR) ;

INDPLAF1 = positif(RNIDOM1 - TOTALPLAF1) * (1 - V_CNR) ;

regle 402022:
application : iliad ;


VARTMP1 = 0 ;

INVRETXUA = min(arr(NINVRETXU * TX30 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXUA ;

INVRETXQA = min(arr(NINVRETXQ * TX35 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXQA ;

INVRETXRA = min(arr(NINVRETXR * TX35 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXRA ;

INVRETXSA = min(arr(NINVRETXS * TX35 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXSA ;

INVRETXTA = min(arr(NINVRETXT * TX35 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXTA ;

INVRETYBA = min(arr(NINVRETYB * TX30 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYBA ;

INVRETYAA = min(arr(NINVRETYA * TX35 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYAA ;

INVRETYDA = min(arr(NINVRETYD * TX30 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYDA ;

INVRETYCA = min(arr(NINVRETYC * TX35 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYCA ;

INVRETYEA = min(arr(NINVRETYE * TX30 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYEA ;

INVRETYFA = min(arr(NINVRETYF * TX30 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYFA ;

INVRETYGA = min(arr(NINVRETYG * TX30 / 100) , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYGA ;

INVRETQLA = min(NINVRETQL , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETQLA ;

INVRETQMA = min(NINVRETQM , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETQMA ;

INVRETQDA = min(NINVRETQD , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETQDA ;

INVRETOBA = min(NINVRETOB , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOBA ;

INVRETOCA = min(NINVRETOC , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOCA ;

INVRETOMA = min(NINVRETOM , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOMA ;

INVRETONA = min(NINVRETON , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETONA ;

INVRETODA = min(NINVRETOD , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETODA ;

INVRETUAA = min(NINVRETUA , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUAA ;

INVRETUHA = min(NINVRETUH , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUHA ;

INVRETUOA = min(NINVRETUO , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUOA ;

INVRETVAA = min(NINVRETVA , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVAA ;

INVRETXURA = min(NINVRETXUR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXURA ;

INVRETXQRA = min(NINVRETXQR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXQRA ;

INVRETXRRA = min(NINVRETXRR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXRRA ;

INVRETXSRA = min(NINVRETXSR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXSRA ;

INVRETXTRA = min(NINVRETXTR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETXTRA ;

INVRETYBRA = min(NINVRETYBR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYBRA ;

INVRETYARA = min(NINVRETYAR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYARA ;

INVRETYDRA = min(NINVRETYDR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYDRA ;

INVRETYCRA = min(NINVRETYCR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYCRA ;

INVRETYERA = min(NINVRETYER , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYERA ;

INVRETYFRA = min(NINVRETYFR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETYFRA ;

INVRETYGRA = min(NINVRETYGR , max(0 , RNIDOM1 - VARTMP1)) ;
VARTMP1 = 0 ;

TOTALPLAFA = INVRETXUA + INVRETXQA 
	     + INVRETXRA + INVRETXSA + INVRETXTA + INVRETYBA + INVRETYAA + INVRETYDA + INVRETYCA + INVRETYEA + INVRETYFA + INVRETYGA + INVRETQLA + INVRETQMA 
	     + INVRETQDA + INVRETOBA + INVRETOCA + INVRETOMA + INVRETONA + INVRETODA + INVRETUAA + INVRETUHA + INVRETUOA + INVRETVAA 
	     + INVRETXURA + INVRETXQRA + INVRETXRRA + INVRETXSRA + INVRETXTRA + INVRETYBRA + INVRETYARA + INVRETYDRA + INVRETYCRA 
	     + INVRETYERA + INVRETYFRA + INVRETYGRA ; 

INDPLAF2 = positif(RNIDOM2 - TOTALPLAF2 - TOTALPLAFA) ;

regle 402024:
application : iliad ;


VARTMP1 = 0 ;
MAXDOM2 = max(0,RNIDOM2 - TOTALPLAFA) ;

INVRETOIA = min(NINVRETOI , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOIA ;

INVRETOJA = min(NINVRETOJ , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOJA ;

INVRETOKA = min(NINVRETOK , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOKA ;

INVRETOPA = min(NINVRETOP , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOPA ;

INVRETOQA = min(NINVRETOQ , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOQA ;

INVRETORA = min(NINVRETOR , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETORA ;

INVRETOEA = min(NINVRETOE , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOEA ;

INVRETOFA = min(NINVRETOF , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOFA ;

INVRETUBA = min(NINVRETUB , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUBA ;

INVRETUCA = min(NINVRETUC , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUCA ;

INVRETUIA = min(NINVRETUI , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUIA ;

INVRETUJA = min(NINVRETUJ , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUJA ;

INVRETUPA = min(NINVRETUP , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUPA ;

INVRETUQA = min(NINVRETUQ , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUQA ;

INVRETVBA = min(NINVRETVB , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVBA ;

INVRETVCA = min(NINVRETVC , max(0 , MAXDOM2 - VARTMP1)) ;
VARTMP1 = 0 ;

TOTALPLAFB = INVRETOIA + INVRETOJA 
             + INVRETOKA + INVRETOPA + INVRETOQA + INVRETORA + INVRETOEA + INVRETOFA + INVRETUBA + INVRETUCA + INVRETUIA + INVRETUJA 
	     + INVRETUPA + INVRETUQA + INVRETVBA + INVRETVCA ;
 
INDPLAF3 = positif(RNIDOM3 - TOTALPLAF3 - TOTALPLAFA - TOTALPLAFB) ;

regle 402026:
application : iliad ;


VARTMP1 = 0 ;
MAXDOM3 = max(0,RNIDOM3 -TOTALPLAFA-TOTALPLAFB) ;

INVRETDTA = min(arr(NINVRETDT*TX34/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDTA ;

INVRETDJA = min(arr(NINVRETDJ*TX375/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDJA ;

INVRETDOA = min(arr(NINVRETDO*TX375/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDOA ;

INVRETDSA = min(arr(NINVRETDS*TX44/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDSA ;

INVRETDIA = min(arr(NINVRETDI*TX4737/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDIA ;

INVRETDNA = min(arr(NINVRETDN*TX4737/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDNA ;

INVRETDKA = min(NINVRETDK , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDKA ;

INVRETDPA = min(NINVRETDP , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDPA ;

INVRETDUA = min(NINVRETDU , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDUA ;

INVRETDMA = min(NINVRETDM , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDMA ;

INVRETDRA = min(NINVRETDR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDRA ;

INVRETDWA = min(NINVRETDW , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDWA ;

INVRETETA = min(arr(NINVRETET*TX34/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETETA ;

INVRETEOA = min(arr(NINVRETEO*TX375/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETEOA ;

INVRETESA = min(arr(NINVRETES*TX44/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETESA ;

INVRETENA = min(arr(NINVRETEN*TX4737/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETENA ;

INVRETEPA = min(NINVRETEP , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETEPA ;

INVRETEUA = min(NINVRETEU , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETEUA ;

INVRETERA = min(NINVRETER , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETERA ;

INVRETEWA = min(NINVRETEW , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETEWA ;

INVRETFTA = min(arr(NINVRETFT*TX34/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFTA ;

INVRETFOA = min(arr(NINVRETFO*TX375/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFOA ;

INVRETFSA = min(arr(NINVRETFS*TX44/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFSA ;

INVRETFNA = min(arr(NINVRETFN*TX4737/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFNA ;

INVRETGTA = min(arr(NINVRETGT*TX34/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETGTA ;

INVRETGSA = min(arr(NINVRETGS*TX44/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETGSA ;

INVRETHTA = min(arr(NINVRETHT*TX34/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETHTA ;

INVRETHSA = min(arr(NINVRETHS*TX44/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETHSA ;

INVRETITA = min(arr(NINVRETIT*TX34/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETITA ;

INVRETISA = min(arr(NINVRETIS*TX44/100) , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETISA ;

INVRETFPA = min(NINVRETFP , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFPA ;

INVRETFUA = min(NINVRETFU , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFUA ;

INVRETFRA = min(NINVRETFR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFRA ;

INVRETFWA = min(NINVRETFW , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFWA ;

INVRETGUA = min(NINVRETGU , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETGUA ;

INVRETGWA = min(NINVRETGW , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETGWA ;

INVRETHUA = min(NINVRETHU , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETHUA ;

INVRETHWA = min(NINVRETHW , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETHWA ;

INVRETIUA = min(NINVRETIU , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETIUA ;

INVRETIWA = min(NINVRETIW , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETIWA ;

INVRETOTA = min(NINVRETOT , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOTA ;

INVRETOUA = min(NINVRETOU , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOUA ;

INVRETOVA = min(NINVRETOV , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOVA ;

INVRETOWA = min(NINVRETOW , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOWA ;

INVRETOGA = min(NINVRETOG , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOGA ;

INVRETOXA = min(NINVRETOX , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOXA ;

INVRETOYA = min(NINVRETOY , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOYA ;

INVRETOZA = min(NINVRETOZ , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETOZA ;

INVRETUDA = min(NINVRETUD , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUDA ;

INVRETUEA = min(NINVRETUE , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUEA ;

INVRETUFA = min(NINVRETUF , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUFA ;

INVRETUGA = min(NINVRETUG , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUGA ;

INVRETUKA = min(NINVRETUK , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUKA ;

INVRETULA = min(NINVRETUL , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETULA ;

INVRETUMA = min(NINVRETUM , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUMA ;

INVRETUNA = min(NINVRETUN , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUNA ;

INVRETURA = min(NINVRETUR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETURA ;

INVRETUSA = min(NINVRETUS , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUSA ;

INVRETUTA = min(NINVRETUT , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUTA ;

INVRETUUA = min(NINVRETUU , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETUUA ;

INVRETVDA = min(NINVRETVD , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVDA ;

INVRETVEA = min(NINVRETVE , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVEA ;

INVRETVFA = min(NINVRETVF , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVFA ;

INVRETVGA = min(NINVRETVG , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVGA ;

INVRETVHA = min(NINVRETVH , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVHA ;

INVRETVIA = min(NINVRETVI , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVIA ;

INVRETVJA = min(NINVRETVJ , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVJA ;

INVRETVKA = min(NINVRETVK , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVKA ;

INVRETVLA = min(NINVRETVL , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETVLA ;

INVRETDTRA = min(NINVRETDTR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDTRA ;

INVRETDJRA = min(NINVRETDJR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDJRA ;

INVRETDORA = min(NINVRETDOR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDORA ;

INVRETDSRA = min(NINVRETDSR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDSRA ;

INVRETDIRA = min(NINVRETDIR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDIRA ;

INVRETDNRA = min(NINVRETDNR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETDNRA ;

INVRETETRA = min(NINVRETETR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETETRA ;

INVRETEORA = min(NINVRETEOR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETEORA ;

INVRETESRA = min(NINVRETESR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETESRA ;

INVRETENRA = min(NINVRETENR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETENRA ;

INVRETFTRA = min(NINVRETFTR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFTRA ;

INVRETFORA = min(NINVRETFOR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFORA ;

INVRETFSRA = min(NINVRETFSR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFSRA ;

INVRETFNRA = min(NINVRETFNR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETFNRA ;

INVRETGTRA = min(NINVRETGTR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETGTRA ;

INVRETGSRA = min(NINVRETGSR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETGSRA ;

INVRETHTRA = min(NINVRETHTR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETHTRA ;

INVRETHSRA = min(NINVRETHSR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETHSRA ;

INVRETITRA = min(NINVRETITR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = VARTMP1 + INVRETITRA ;

INVRETISRA = min(NINVRETISR , max(0 , MAXDOM3 - VARTMP1)) ;
VARTMP1 = 0 ;

TOTALPLAFC = INVRETDTA 
	     + INVRETDJA + INVRETDOA + INVRETDSA + INVRETDIA + INVRETDNA + INVRETETA + INVRETEOA + INVRETESA + INVRETENA + INVRETFTA + INVRETFOA + INVRETFSA + INVRETFNA
	     + INVRETGTA + INVRETGSA + INVRETHTA + INVRETHSA + INVRETITA + INVRETISA + INVRETDKA + INVRETDPA + INVRETDUA + INVRETDMA + INVRETDRA + INVRETDWA + INVRETEPA 
	     + INVRETEUA + INVRETERA 
	     + INVRETEWA + INVRETFPA + INVRETFUA + INVRETFRA + INVRETFWA + INVRETGUA + INVRETGWA + INVRETHUA + INVRETHWA + INVRETIUA + INVRETIWA + INVRETOTA + INVRETOUA 
	     + INVRETOVA + INVRETOWA 
	     + INVRETOGA + INVRETOXA + INVRETOYA + INVRETOZA + INVRETUDA + INVRETUEA + INVRETUFA + INVRETUGA + INVRETUKA + INVRETULA + INVRETUMA + INVRETUNA + INVRETURA 
	     + INVRETUSA + INVRETUTA + INVRETUUA + INVRETVDA + INVRETVEA + INVRETVFA + INVRETVGA + INVRETVHA + INVRETVIA + INVRETVJA + INVRETVKA + INVRETVLA
	     + INVRETDTRA + INVRETDJRA + INVRETDORA + INVRETDSRA + INVRETDIRA + INVRETDNRA 
	     + INVRETETRA + INVRETEORA + INVRETESRA + INVRETENRA + INVRETFTRA + INVRETFORA + INVRETFSRA + INVRETFNRA + INVRETGTRA + INVRETGSRA + INVRETHTRA + INVRETHSRA 
	     + INVRETITRA + INVRETISRA ;

INDPLAF = positif(TOTALPLAFA + TOTALPLAFB + TOTALPLAFC - TOTALPLAF1 - TOTALPLAF2 - TOTALPLAF3) * positif(INDPLAF1 + INDPLAF2 + INDPLAF3) * positif(OPTPLAF15) ;


ALOGDOM_1 = (INVLOG2008 + INVLGDEB2009 + INVLGDEB + INVOMLOGOA + INVOMLOGOH + INVOMLOGOL + INVOMLOGOO + INVOMLOGOS
                      + (INVRETQL + INVRETQM + INVRETQD + INVRETOB + INVRETOC + INVRETOM + INVRETON + INVRETOI + INVRETOJ + INVRETOK + INVRETOP 
			 + INVRETOQ + INVRETOR + INVRETOT + INVRETOU + INVRETOV + INVRETOW + INVRETOD + INVRETOE + INVRETOF + INVRETOG
                         + INVRETOX + INVRETOY + INVRETOZ + INVRETUA + INVRETUB + INVRETUC + INVRETUD + INVRETUE + INVRETUF + INVRETUG
                         + INVRETUH + INVRETUI + INVRETUJ + INVRETUK + INVRETUL + INVRETUM + INVRETUN + INVRETUO + INVRETUP + INVRETUQ 
			 + INVRETUR + INVRETUS + INVRETUT + INVRETUU + INVRETVA + INVRETVB + INVRETVC + INVRETVD + INVRETVE + INVRETVF
			 + INVRETVG + INVRETVH + INVRETVI + INVRETVJ + INVRETVK + INVRETVL) * (1 - INDPLAF)
		      + (INVRETQLA + INVRETQMA + INVRETQDA + INVRETOBA + INVRETOCA + INVRETOMA + INVRETONA + INVRETOIA + INVRETOJA + INVRETOKA 
			 + INVRETOPA + INVRETOQA + INVRETORA + INVRETOTA + INVRETOUA + INVRETOVA + INVRETOWA + INVRETODA + INVRETOEA + INVRETOFA 
			 + INVRETOGA + INVRETOXA + INVRETOYA + INVRETOZA + INVRETUAA + INVRETUBA + INVRETUCA + INVRETUDA + INVRETUEA + INVRETUFA 
			 + INVRETUGA + INVRETUHA + INVRETUIA + INVRETUJA + INVRETUKA + INVRETULA + INVRETUMA + INVRETUNA + INVRETUOA + INVRETUPA 
			 + INVRETUQA + INVRETURA + INVRETUSA + INVRETUTA + INVRETUUA + INVRETVAA + INVRETVBA + INVRETVCA + INVRETVDA + INVRETVEA 
			 + INVRETVFA + INVRETVGA + INVRETVHA + INVRETVIA + INVRETVJA + INVRETVKA + INVRETVLA) * INDPLAF)
	     * (1 - V_CNR) ;
ALOGDOM = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ALOGDOM_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ALOGDOM_1,max(ALOGDOM_P,ALOGDOM1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

ALOGSOC_1 = ((INVRETYG + INVRETYGR) * (1 - INDPLAF) + (INVRETYGA + INVRETYGRA) * INDPLAF) * (1 - V_CNR) ;
ALOGSOC = positif(null(V_IND_TRAIT - 4) + COD9ZA) * ALOGSOC_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(ALOGSOC_1 , max(ALOGSOC_P,ALOGSOC1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

ADOMSOC1_1 = ((INVRETXU + INVRETXQ + INVRETXR + INVRETXS + INVRETXT + INVRETYB 
               + INVRETYA + INVRETYC + INVRETYD + INVRETYE + INVRETYF
	       + INVRETXUR + INVRETXQR + INVRETXRR + INVRETXSR + INVRETXTR 
	       + INVRETYBR + INVRETYAR + INVRETYCR + INVRETYDR + INVRETYER + INVRETYFR) * (1 - INDPLAF) 
	     + (INVRETXUA + INVRETXQA + INVRETXRA + INVRETXSA + INVRETXTA 
	        + INVRETYBA + INVRETYAA + INVRETYCA + INVRETYDA + INVRETYEA + INVRETYFA
		+ INVRETXURA + INVRETXQRA + INVRETXRRA + INVRETXSRA 
		+ INVRETXTRA + INVRETYBRA + INVRETYARA + INVRETYCRA + INVRETYDRA + INVRETYERA + INVRETYFRA) * INDPLAF) 
              * (1 - V_CNR) ;
ADOMSOC1 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ADOMSOC1_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(ADOMSOC1_1,max(ADOMSOC1_P,ADOMSOC11731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

ALOCENT_1 = ((INVRETIT + INVRETIS + INVRETIU + INVRETIW
              + INVRETITR + INVRETISR) * (1 - INDPLAF)
            + (INVRETITA + INVRETISA + INVRETIUA + INVRETIWA
               + INVRETITRA + INVRETISRA) * INDPLAF)
            * (1 - V_CNR) ;
ALOCENT = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ALOCENT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ALOCENT_1,max(ALOCENT_P,ALOCENT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

ACOLENT_1 = ((INVRETDT + INVRETDJ + INVRETDO + INVRETDS + INVRETDI + INVRETDN + INVRETDK + INVRETDP + INVRETDU + INVRETDM + INVRETDR 
	      + INVRETDW + INVRETET + INVRETEO + INVRETES + INVRETEN + INVRETEP + INVRETEU + INVRETER + INVRETEW + INVRETFT + INVRETFO + INVRETFS + INVRETFN 
	      + INVRETFP + INVRETFU + INVRETFR + INVRETFW + INVRETGT + INVRETGS + INVRETGU + INVRETGW + INVRETHT + INVRETHS + INVRETHU + INVRETHW
	      + INVRETDTR + INVRETDJR + INVRETDOR + INVRETDSR + INVRETDIR + INVRETDNR 
	      + INVRETETR + INVRETEOR + INVRETESR + INVRETENR + INVRETFTR + INVRETFOR + INVRETFSR + INVRETFNR + INVRETGTR + INVRETGSR + INVRETHTR + INVRETHSR) * (1 - INDPLAF) 
	   + (INVRETDTA + INVRETDJA + INVRETDOA + INVRETDSA + INVRETDIA + INVRETDNA + INVRETDKA + INVRETDPA + INVRETDUA 
	      + INVRETDMA + INVRETDRA + INVRETDWA + INVRETETA + INVRETEOA + INVRETESA + INVRETENA + INVRETEPA + INVRETEUA + INVRETERA + INVRETEWA + INVRETFTA 
	      + INVRETFOA + INVRETFSA + INVRETFNA + INVRETFPA + INVRETFUA + INVRETFRA + INVRETFWA + INVRETGTA + INVRETGSA + INVRETGUA + INVRETGWA + INVRETHTA 
	      + INVRETHSA + INVRETHUA + INVRETHWA
	      + INVRETDTRA + INVRETDJRA + INVRETDORA + INVRETDSRA + INVRETDIRA 
	      + INVRETDNRA + INVRETETRA + INVRETEORA + INVRETESRA + INVRETENRA + INVRETFTRA + INVRETFORA + INVRETFSRA + INVRETFNRA + INVRETGTRA + INVRETGSRA
	      + INVRETHTRA + INVRETHSRA) * INDPLAF) 
	     * (1 - V_CNR) ;
ACOLENT = positif(null(V_IND_TRAIT-4)+COD9ZA) * (ACOLENT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(ACOLENT_1,max(ACOLENT_P,ACOLENT1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

regle 402030:
application : iliad ;


VARTMP1 = 0 ;

NINVRETQB = max(min(INVLOG2008 , RRI1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETQB ;

NINVRETQC = max(min(INVLGDEB2009 , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETQC ;

NINVRETQT = max(min(INVLGDEB , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETQT ;

NINVRETOA = max(min(INVOMLOGOA , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOA ;

NINVRETOH = max(min(INVOMLOGOH , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOH ;

NINVRETOL = max(min(INVOMLOGOL , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOL ;

NINVRETOO = max(min(INVOMLOGOO , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOO ;

NINVRETOS = max(min(INVOMLOGOS , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOS ;

NINVRETQL = max(min(INVLGAUTRE , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETQL ;

NINVRETQM = max(min(INVLGDEB2010 , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETQM ;

NINVRETQD = max(min(INVLOG2009 , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETQD ;

NINVRETOB = max(min(INVOMLOGOB , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOB ;

NINVRETOC = max(min(INVOMLOGOC , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOC ;

NINVRETOI = max(min(INVOMLOGOI , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOI ;

NINVRETOJ = max(min(INVOMLOGOJ , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOJ ;

NINVRETOK = max(min(INVOMLOGOK , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOK ;

NINVRETOM = max(min(INVOMLOGOM , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOM ;

NINVRETON = max(min(INVOMLOGON , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETON ;

NINVRETOP = max(min(INVOMLOGOP , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOP ;

NINVRETOQ = max(min(INVOMLOGOQ , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOQ ; 

NINVRETOR = max(min(INVOMLOGOR , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOR ;

NINVRETOT = max(min(INVOMLOGOT , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOT ;

NINVRETOU = max(min(INVOMLOGOU , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOU ;

NINVRETOV = max(min(INVOMLOGOV , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOV ;

NINVRETOW = max(min(INVOMLOGOW , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOW ;

NINVRETOD = max(min(CODHOD , RRI1-VARTMP1) , 0) * (1 - V_CNR) ; 
VARTMP1 = VARTMP1 + NINVRETOD ;

NINVRETOE = max(min(CODHOE , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOE ;

NINVRETOF = max(min(CODHOF , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOF ;

NINVRETOG = max(min(CODHOG , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOG ;

NINVRETOX = max(min(CODHOX , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOX ;

NINVRETOY = max(min(CODHOY , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOY ;

NINVRETOZ = max(min(CODHOZ , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETOZ ;

NINVRETUA = max(min(CODHUA , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUA ;

NINVRETUB = max(min(CODHUB , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUB ;

NINVRETUC = max(min(CODHUC , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUC ;

NINVRETUD = max(min(CODHUD , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUD ; 

NINVRETUE = max(min(CODHUE , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUE ;

NINVRETUF = max(min(CODHUF , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUF ;

NINVRETUG = max(min(CODHUG , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUG ;

NINVRETUH = max(min(CODHUH , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUH ;

NINVRETUI = max(min(CODHUI , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUI ;

NINVRETUJ = max(min(CODHUJ , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUJ ;

NINVRETUK = max(min(CODHUK , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUK ;

NINVRETUL = max(min(CODHUL , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUL ;

NINVRETUM = max(min(CODHUM , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUM ;

NINVRETUN = max(min(CODHUN , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUN ;

NINVRETUO = max(min(CODHUO , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUO ;

NINVRETUP = max(min(CODHUP , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUP ;

NINVRETUQ = max(min(CODHUQ , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUQ ;

NINVRETUR = max(min(CODHUR , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUR ;

NINVRETUS = max(min(CODHUS , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUS ;

NINVRETUT = max(min(CODHUT , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUT ;

NINVRETUU = max(min(CODHUU , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETUU ; 

NINVRETVA = max(min(CODHVA , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVA ;

NINVRETVB = max(min(CODHVB , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVB ;

NINVRETVC = max(min(CODHVC , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVC ;

NINVRETVD = max(min(CODHVD , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVD ;

NINVRETVE = max(min(CODHVE , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVE ;

NINVRETVF = max(min(CODHVF , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVF ;

NINVRETVG = max(min(CODHVG , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVG ;

NINVRETVH = max(min(CODHVH , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVH ;

NINVRETVI = max(min(CODHVI , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVI ;

NINVRETVJ = max(min(CODHVJ , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVJ ;

NINVRETVK = max(min(CODHVK , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETVK ;

NINVRETVL = max(min(CODHVL , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = 0 ; 

regle 402040:
application : iliad ;


VARTMP1 = 0 ;

NINVRETXU = max(min(CODHXU , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETXU ;

NINVRETXQ = max(min(CODHXQ , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETXQ ;

NINVRETXR = max(min(CODHXR , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETXR ;

NINVRETXS = max(min(CODHXS , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETXS ;

NINVRETXT = max(min(CODHXT , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETXT ;

NINVRETYB = max(min(CODHYB , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETYB ;

NINVRETYA = max(min(CODHYA , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETYA ;

NINVRETYD = max(min(CODHYD , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETYD ;

NINVRETYC = max(min(CODHYC , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETYC ;

NINVRETYE = max(min(CODHYE , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETYE ;

NINVRETYF = max(min(CODHYF , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETYF ;

NINVRETYG = max(min(CODHYG , NNRI2-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = 0 ;

NRDOMSOC1 = NINVRETXU + NINVRETXQ + NINVRETXR + NINVRETXS + NINVRETXT 
            + NINVRETYB + NINVRETYA + NINVRETYD + NINVRETYC + NINVRETYE + NINVRETYF ;

NRLOGSOC = NINVRETYG ;


NINVRETXUR = NINVRETXU - arr(NINVRETXU * TX30 / 100) ;

NINVRETXQR = NINVRETXQ - arr(NINVRETXQ * TX35 / 100) ;

NINVRETXRR = NINVRETXR - arr(NINVRETXR * TX35 / 100) ;

NINVRETXSR = NINVRETXS - arr(NINVRETXS * TX35 / 100) ;

NINVRETXTR = NINVRETXT - arr(NINVRETXT * TX35 / 100) ;

NINVRETYBR = NINVRETYB - arr(NINVRETYB * TX30 / 100) ;

NINVRETYAR = NINVRETYA - arr(NINVRETYA * TX35 / 100) ;

NINVRETYDR = NINVRETYD - arr(NINVRETYD * TX30 / 100) ;

NINVRETYCR = NINVRETYC - arr(NINVRETYC * TX35 / 100) ;

NINVRETYER = NINVRETYE - arr(NINVRETYE * TX30 / 100) ;

NINVRETYFR = NINVRETYF - arr(NINVRETYF * TX30 / 100) ;

NINVRETYGR = NINVRETYG - arr(NINVRETYG * TX30 / 100) ;

regle 402050:
application : iliad ;


VARTMP1 = 0 ;

INVRETXU = min(arr(NINVRETXU * TX30 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETXU ;

INVRETXQ = min(arr(NINVRETXQ * TX35 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETXQ ;

INVRETXR = min(arr(NINVRETXR * TX35 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETXR ;

INVRETXS = min(arr(NINVRETXS * TX35 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETXS ;

INVRETXT = min(arr(NINVRETXT * TX35 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETXT ;

INVRETYB = min(arr(NINVRETYB * TX30 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETYB ;

INVRETYA = min(arr(NINVRETYA * TX35 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETYA ;

INVRETYD = min(arr(NINVRETYD * TX30 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETYD ;

INVRETYC = min(arr(NINVRETYC * TX35 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETYC ;

INVRETYE = min(arr(NINVRETYE * TX30 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETYE ;

INVRETYF = min(arr(NINVRETYF * TX30 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETYF ;

INVRETYG = min(arr(NINVRETYG * TX30 / 100) , max(0 , PLAF_INVDOM - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = 0 ;

INVRETSOC = INVRETXU + INVRETXQ + INVRETXR + INVRETXS + INVRETXT + INVRETYB 
	    + INVRETYA + INVRETYD + INVRETYC + INVRETYE + INVRETYF + INVRETYG ;


INVRETXUR = min(arr(INVRETXU * 7 / 3) , NINVRETXU - INVRETXU) * (1 - null(1 - abs(arr(INVRETXU * 7 / 3) - (NINVRETXU - INVRETXU))))
                + (NINVRETXU - INVRETXU) * null(1 - abs(arr(INVRETXU * 7 / 3) - (NINVRETXU - INVRETXU))) ;

INVRETXQR = min(arr(INVRETXQ * 13 / 7) , NINVRETXQ - INVRETXQ) * (1 - null(1 - abs(arr(INVRETXQ * 13 / 7) - (NINVRETXQ - INVRETXQ))))
                + (NINVRETXQ - INVRETXQ) * null(1 - abs(arr(INVRETXQ * 13 / 7) - (NINVRETXQ - INVRETXQ))) ;

INVRETXRR = min(arr(INVRETXR * 13 / 7) , NINVRETXR - INVRETXR) * (1 - null(1 - abs(arr(INVRETXR * 13 / 7) - (NINVRETXR - INVRETXR))))
                + (NINVRETXR - INVRETXR) * null(1 - abs(arr(INVRETXR * 13 / 7) - (NINVRETXR - INVRETXR))) ;

INVRETXSR = min(arr(INVRETXS * 13 / 7) , NINVRETXS - INVRETXS) * (1 - null(1 - abs(arr(INVRETXS * 13 / 7) - (NINVRETXS - INVRETXS))))
                + (NINVRETXS - INVRETXS) * null(1 - abs(arr(INVRETXS * 13 / 7) - (NINVRETXS - INVRETXS))) ;

INVRETXTR = min(arr(INVRETXT * 13 / 7) , NINVRETXT - INVRETXT) * (1 - null(1 - abs(arr(INVRETXT * 13 / 7) - (NINVRETXT - INVRETXT))))
                + (NINVRETXT - INVRETXT) * null(1 - abs(arr(INVRETXT * 13 / 7) - (NINVRETXT - INVRETXT))) ;

INVRETYBR = min(arr(INVRETYB * 7 / 3) , NINVRETYB - INVRETYB) * (1 - null(1 - abs(arr(INVRETYB * 7 / 3) - (NINVRETYB - INVRETYB))))
                + (NINVRETYB - INVRETYB) * null(1 - abs(arr(INVRETYB * 7 / 3) - (NINVRETYB - INVRETYB))) ;

INVRETYAR = min(arr(INVRETYA * 13 / 7) , NINVRETYA - INVRETYA) * (1 - null(1 - abs(arr(INVRETYA * 13 / 7) - (NINVRETYA - INVRETYA))))
                + (NINVRETYA - INVRETYA) * null(1 - abs(arr(INVRETYA * 13 / 7) - (NINVRETYA - INVRETYA))) ;

INVRETYDR = min(arr(INVRETYD * 7 / 3) , NINVRETYD - INVRETYD) * (1 - null(1 - abs(arr(INVRETYD * 7 / 3) - (NINVRETYD - INVRETYD))))
                + (NINVRETYD - INVRETYD) * null(1 - abs(arr(INVRETYD * 7 / 3) - (NINVRETYD - INVRETYD))) ;

INVRETYCR = min(arr(INVRETYC * 13 / 7) , NINVRETYC - INVRETYC) * (1 - null(1 - abs(arr(INVRETYC * 13 / 7) - (NINVRETYC - INVRETYC))))
                + (NINVRETYC - INVRETYC) * null(1 - abs(arr(INVRETYC * 13 / 7) - (NINVRETYC - INVRETYC))) ;

INVRETYER = min(arr(INVRETYE * 7 / 3) , NINVRETYE - INVRETYE) * (1 - null(1 - abs(arr(INVRETYE * 7 / 3) - (NINVRETYE - INVRETYE))))
                + (NINVRETYE - INVRETYE) * null(1 - abs(arr(INVRETYE * 7 / 3) - (NINVRETYE - INVRETYE))) ;

INVRETYFR = min(arr(INVRETYF * 7 / 3) , NINVRETYF - INVRETYF) * (1 - null(1 - abs(arr(INVRETYF * 7 / 3) - (NINVRETYF - INVRETYF))))
                + (NINVRETYF - INVRETYF) * null(1 - abs(arr(INVRETYF * 7 / 3) - (NINVRETYF - INVRETYF))) ;

INVRETYGR = min(arr(INVRETYG * 7 / 3) , NINVRETYG - INVRETYG) * (1 - null(1 - abs(arr(INVRETYG * 7 / 3) - (NINVRETYG - INVRETYG))))
                + (NINVRETYG - INVRETYG) * null(1 - abs(arr(INVRETYG * 7 / 3) - (NINVRETYG - INVRETYG))) ;

regle 402070:
application : iliad ;


NINVENT12 = NRLOGSOC + NRDOMSOC1 ;


VARTMP1 = 0 ;

NINVRETDT = max(min(CODHDT , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDT ;

NINVRETDJ = max(min(CODHDJ , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDJ ;

NINVRETDO = max(min(CODHDO , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDO ;

NINVRETDS = max(min(CODHDS , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDS ;

NINVRETDI = max(min(CODHDI , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDI ;

NINVRETDN = max(min(CODHDN , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDN ;

NINVRETDK = max(min(CODHDK , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDK ;

NINVRETDP = max(min(CODHDP , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDP ;

NINVRETDU = max(min(CODHDU , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDU ;

NINVRETDM = max(min(CODHDM , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDM ;

NINVRETDR = max(min(CODHDR , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDR ;

NINVRETDW = max(min(CODHDW , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETDW ;

NINVRETET = max(min(CODHET , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETET ;

NINVRETEO = max(min(CODHEO , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETEO ;

NINVRETES = max(min(CODHES , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETES ;

NINVRETEN = max(min(CODHEN , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETEN ;

NINVRETEP = max(min(CODHEP , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETEP ;

NINVRETEU = max(min(CODHEU , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETEU ;

NINVRETER = max(min(CODHER , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETER ;

NINVRETEW = max(min(CODHEW , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETEW ;

NINVRETFT = max(min(CODHFT , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETFT ;

NINVRETFO = max(min(CODHFO , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETFO ;

NINVRETFS = max(min(CODHFS , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETFS ;

NINVRETFN = max(min(CODHFN , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETFN ;

NINVRETFP = max(min(CODHFP , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETFP ;

NINVRETFU = max(min(CODHFU , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETFU ;

NINVRETFR = max(min(CODHFR , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETFR ;

NINVRETFW = max(min(CODHFW , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETFW ;

NINVRETGT = max(min(CODHGT , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETGT ;

NINVRETGU = max(min(CODHGU , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETGU ;

NINVRETGW = max(min(CODHGW , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETGW ;

NINVRETGS = max(min(CODHGS , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETGS ;

NINVRETHT = max(min(CODHHT , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETHT ;

NINVRETHU = max(min(CODHHU , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETHU ;

NINVRETHW = max(min(CODHHW , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETHW ;

NINVRETHS = max(min(CODHHS , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETHS ;

NINVRETIT = max(min(CODHIT , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETIT ;

NINVRETIU = max(min(CODHIU , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETIU ;

NINVRETIW = max(min(CODHIW , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + NINVRETIW ;

NINVRETIS = max(min(CODHIS , NNRI2-NINVENT12-VARTMP1) , 0) * (1 - V_CNR) ;
VARTMP1 = 0 ;


NINVRETDTR = NINVRETDT - arr(NINVRETDT * TX34/100) ;

NINVRETDJR = NINVRETDJ - arr(NINVRETDJ * TX375/100) ;

NINVRETDOR = NINVRETDO - arr(NINVRETDO * TX375/100) ;

NINVRETDSR = NINVRETDS - arr(NINVRETDS * TX44/100) ;

NINVRETDIR = NINVRETDI - arr(NINVRETDI * TX4737/100) ;

NINVRETDNR = NINVRETDN - arr(NINVRETDN * TX4737/100) ;

NINVRETETR = NINVRETET - arr(NINVRETET * TX34/100) ;

NINVRETEOR = NINVRETEO - arr(NINVRETEO * TX375/100) ;

NINVRETESR = NINVRETES - arr(NINVRETES * TX44/100) ;

NINVRETENR = NINVRETEN - arr(NINVRETEN * TX4737/100) ;

NINVRETFTR = NINVRETFT - arr(NINVRETFT * TX34/100) ;

NINVRETFOR = NINVRETFO - arr(NINVRETFO * TX375/100) ;

NINVRETFSR = NINVRETFS - arr(NINVRETFS * TX44/100) ;

NINVRETFNR = NINVRETFN - arr(NINVRETFN * TX4737/100) ;

NINVRETGTR = NINVRETGT - arr(NINVRETGT * TX34/100) ;

NINVRETGSR = NINVRETGS - arr(NINVRETGS * TX44/100) ;

NINVRETHTR = NINVRETHT - arr(NINVRETHT * TX34/100) ;

NINVRETHSR = NINVRETHS - arr(NINVRETHS * TX44/100) ;

NINVRETITR = NINVRETIT - arr(NINVRETIT * TX34/100) ;

NINVRETISR = NINVRETIS - arr(NINVRETIS * TX44/100) ;

regle 402080:
application : iliad ;


VARTMP1 = 0 ;

INVRETDT = min(arr(NINVRETDT * TX34/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETDT ;

INVRETDJ = min(arr(NINVRETDJ * TX375/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETDJ ;

INVRETDO = min(arr(NINVRETDO * TX375/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETDO ;

INVRETDS = min(arr(NINVRETDS * TX44/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETDS ;

INVRETDI = min(arr(NINVRETDI * TX4737/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETDI ;

INVRETDN = min(arr(NINVRETDN * TX4737/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETDN ;

INVRETET = min(arr(NINVRETET * TX34/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETET ;

INVRETEO = min(arr(NINVRETEO * TX375/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETEO ;

INVRETES = min(arr(NINVRETES * TX44/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETES ;

INVRETEN = min(arr(NINVRETEN * TX4737/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETEN ;

INVRETFT = min(arr(NINVRETFT * TX34/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETFT ;

INVRETFO = min(arr(NINVRETFO * TX375/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETFO ;

INVRETFS = min(arr(NINVRETFS * TX44/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETFS ;

INVRETFN = min(arr(NINVRETFN * TX4737/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETFN ;

INVRETGT = min(arr(NINVRETGT * TX34/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETGT ;

INVRETGS = min(arr(NINVRETGS * TX44/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETGS ;

INVRETHT = min(arr(NINVRETHT * TX34/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETHT ;

INVRETHS = min(arr(NINVRETHS * TX44/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETHS ;

INVRETIT = min(arr(NINVRETIT * TX34/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETIT ;

INVRETIS = min(arr(NINVRETIS * TX44/100) , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETIS ;

INVRETDK = min(NINVRETDK , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETDK ;

INVRETDP = min(NINVRETDP , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETDP ;

INVRETDU = min(NINVRETDU , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETDU ;

INVRETEP = min(NINVRETEP , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETEP ;

INVRETEU = min(NINVRETEU , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETEU ;

INVRETFP = min(NINVRETFP , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETFP ;

INVRETFU = min(NINVRETFU , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETFU ;

INVRETGU = min(NINVRETGU , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETGU ;

INVRETHU = min(NINVRETHU , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETHU ;

INVRETIU = min(NINVRETIU , max(0 , PLAF_INVDOM4 -INVRETSOC-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = 0 ;

INVRETENT = INVRETDT + INVRETDJ + INVRETDO + INVRETDS + INVRETDI + INVRETDN + INVRETET + INVRETEO + INVRETES + INVRETEN + INVRETFT + INVRETFO 
	    + INVRETFS + INVRETFN + INVRETGT + INVRETGS + INVRETHT + INVRETHS + INVRETIT + INVRETIS + INVRETDK 
	    + INVRETDP + INVRETDU + INVRETEP + INVRETEU + INVRETFP + INVRETFU + INVRETGU + INVRETHU + INVRETIU ;

INVRETDM = NINVRETDM ;

INVRETDR = NINVRETDR ;

INVRETDW = NINVRETDW ;

INVRETER = NINVRETER ;

INVRETEW = NINVRETEW ;

INVRETFR = NINVRETFR ;

INVRETFW = NINVRETFW ;

INVRETGW = NINVRETGW ;

INVRETHW = NINVRETHW ;

INVRETIW = NINVRETIW ;


INVRETDTR = min(arr(INVRETDT * 33/17) , NINVRETDT - INVRETDT) * (1 - null(1 - abs(arr(INVRETDT * 33/17) - (NINVRETDT - INVRETDT))))
            + (NINVRETDT - INVRETDT) * null(1 - abs(arr(INVRETDT * 33/17) - (NINVRETDT - INVRETDT))) ;

INVRETDJR = min(arr(INVRETDJ * 5/3) , NINVRETDJ - INVRETDJ) * (1 - null(1 - abs(arr(INVRETDJ * 5/3) - (NINVRETDJ - INVRETDJ))))
            + (NINVRETDJ - INVRETDJ) * null(1 - abs(arr(INVRETDJ * 5/3) - (NINVRETDJ - INVRETDJ))) ;

INVRETDOR = min(arr(INVRETDO * 5/3) , NINVRETDO - INVRETDO) * (1 - null(1 - abs(arr(INVRETDO * 5/3) - (NINVRETDO - INVRETDO))))
            + (NINVRETDO - INVRETDO) * null(1 - abs(arr(INVRETDO * 5/3) - (NINVRETDO - INVRETDO))) ;

INVRETDSR = min(arr(INVRETDS * 14/11) , NINVRETDS - INVRETDS) * (1 - null(1 - abs(arr(INVRETDS * 14/11) - (NINVRETDS - INVRETDS))))
            + (NINVRETDS - INVRETDS) * null(1 - abs(arr(INVRETDS * 14/11) - (NINVRETDS - INVRETDS))) ;

INVRETDIR = min(arr(INVRETDI * 10/9) , NINVRETDI - INVRETDI) * (1 - null(1 - abs(arr(INVRETDI * 10/9) - (NINVRETDI - INVRETDI))))
            + (NINVRETDI - INVRETDI) * null(1 - abs(arr(INVRETDI * 10/9) - (NINVRETDI - INVRETDI))) ;

INVRETDNR = min(arr(INVRETDN * 10/9) , NINVRETDN - INVRETDN) * (1 - null(1 - abs(arr(INVRETDN * 10/9) - (NINVRETDN - INVRETDN))))
            + (NINVRETDN - INVRETDN) * null(1 - abs(arr(INVRETDN * 10/9) - (NINVRETDN - INVRETDN))) ;

INVRETETR = min(arr(INVRETET * 33/17) , NINVRETET - INVRETET) * (1 - null(1 - abs(arr(INVRETET * 33/17) - (NINVRETET - INVRETET))))
            + (NINVRETET - INVRETET) * null(1 - abs(arr(INVRETET * 33/17) - (NINVRETET - INVRETET))) ;

INVRETEOR = min(arr(INVRETEO * 5/3) , NINVRETEO - INVRETEO) * (1 - null(1 - abs(arr(INVRETEO * 5/3) - (NINVRETEO - INVRETEO))))
            + (NINVRETEO - INVRETEO) * null(1 - abs(arr(INVRETEO * 5/3) - (NINVRETEO - INVRETEO))) ;

INVRETESR = min(arr(INVRETES * 14/11) , NINVRETES - INVRETES) * (1 - null(1 - abs(arr(INVRETES * 14/11) - (NINVRETES - INVRETES))))
            + (NINVRETES - INVRETES) * null(1 - abs(arr(INVRETES * 14/11) - (NINVRETES - INVRETES))) ;

INVRETENR = min(arr(INVRETEN * 10/9) , NINVRETEN - INVRETEN) * (1 - null(1 - abs(arr(INVRETEN * 10/9) - (NINVRETEN - INVRETEN))))
            + (NINVRETEN - INVRETEN) * null(1 - abs(arr(INVRETEN * 10/9) - (NINVRETEN - INVRETEN))) ;

INVRETFTR = min(arr(INVRETFT * 33/17) , NINVRETFT - INVRETFT) * (1 - null(1 - abs(arr(INVRETFT * 33/17) - (NINVRETFT - INVRETFT))))
            + (NINVRETFT - INVRETFT) * null(1 - abs(arr(INVRETFT * 33/17) - (NINVRETFT - INVRETFT))) ;

INVRETFOR = min(arr(INVRETFO * 5/3) , NINVRETFO - INVRETFO) * (1 - null(1 - abs(arr(INVRETFO * 5/3) - (NINVRETFO - INVRETFO))))
            + (NINVRETFO - INVRETFO) * null(1 - abs(arr(INVRETFO * 5/3) - (NINVRETFO - INVRETFO))) ;

INVRETFSR = min(arr(INVRETFS * 14/11) , NINVRETFS - INVRETFS) * (1 - null(1 - abs(arr(INVRETFS * 14/11) - (NINVRETFS - INVRETFS))))
            + (NINVRETFS - INVRETFS) * null(1 - abs(arr(INVRETFS * 14/11) - (NINVRETFS - INVRETFS))) ;

INVRETFNR = min(arr(INVRETFN * 10/9) , NINVRETFN - INVRETFN) * (1 - null(1 - abs(arr(INVRETFN * 10/9) - (NINVRETFN - INVRETFN))))
            + (NINVRETFN - INVRETFN) * null(1 - abs(arr(INVRETFN * 10/9) - (NINVRETFN - INVRETFN))) ;

INVRETGTR = min(arr(INVRETGT * 33/17) , NINVRETGT - INVRETGT) * (1 - null(1 - abs(arr(INVRETGT * 33/17) - (NINVRETGT - INVRETGT))))
            + (NINVRETGT - INVRETGT) * null(1 - abs(arr(INVRETGT * 33/17) - (NINVRETGT - INVRETGT))) ;

INVRETGSR = min(arr(INVRETGS * 14/11) , NINVRETGS - INVRETGS) * (1 - null(1 - abs(arr(INVRETGS * 14/11) - (NINVRETGS - INVRETGS))))
            + (NINVRETGS - INVRETGS) * null(1 - abs(arr(INVRETGS * 14/11) - (NINVRETGS - INVRETGS))) ;

INVRETHTR = min(arr(INVRETHT * 33/17) , NINVRETHT - INVRETHT) * (1 - null(1 - abs(arr(INVRETHT * 33/17) - (NINVRETHT - INVRETHT))))
            + (NINVRETHT - INVRETHT) * null(1 - abs(arr(INVRETHT * 33/17) - (NINVRETHT - INVRETHT))) ;

INVRETHSR = min(arr(INVRETHS * 14/11) , NINVRETHS - INVRETHS) * (1 - null(1 - abs(arr(INVRETHS * 14/11) - (NINVRETHS - INVRETHS))))
            + (NINVRETHS - INVRETHS) * null(1 - abs(arr(INVRETHS * 14/11) - (NINVRETHS - INVRETHS))) ;

INVRETITR = min(arr(INVRETIT * 33/17) , NINVRETIT - INVRETIT) * (1 - null(1 - abs(arr(INVRETIT * 33/17) - (NINVRETIT - INVRETIT))))
            + (NINVRETIT - INVRETIT) * null(1 - abs(arr(INVRETIT * 33/17) - (NINVRETIT - INVRETIT))) ;

INVRETISR = min(arr(INVRETIS * 14/11) , NINVRETIS - INVRETIS) * (1 - null(1 - abs(arr(INVRETIS * 14/11) - (NINVRETIS - INVRETIS))))
            + (NINVRETIS - INVRETIS) * null(1 - abs(arr(INVRETIS * 14/11) - (NINVRETIS - INVRETIS))) ;

regle 402100:
application : iliad ;


VARTMP1 = 0 ;

INVRETQB = NINVRETQB ; 

INVRETQC = NINVRETQC ; 

INVRETQT = NINVRETQT ; 

INVRETQL = min(NINVRETQL , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETQL ;

INVRETQM = min(NINVRETQM , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETQM ;

INVRETQD = min(NINVRETQD , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETQD ;

INVRETOB = min(NINVRETOB , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOB ;

INVRETOC = min(NINVRETOC , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOC ;

INVRETOI = min(NINVRETOI , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOI ;

INVRETOJ = min(NINVRETOJ , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOJ ;

INVRETOK = min(NINVRETOK , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOK ;

INVRETOM = min(NINVRETOM , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOM ;

INVRETON = min(NINVRETON , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETON ;

INVRETOP = min(NINVRETOP , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOP ;

INVRETOQ = min(NINVRETOQ , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOQ ;

INVRETOR = min(NINVRETOR , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOR ;

INVRETOT = min(NINVRETOT , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOT ;

INVRETOU = min(NINVRETOU , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOU ;

INVRETOV = min(NINVRETOV , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOV ;

INVRETOW = min(NINVRETOW , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOW ;

INVRETOD = min(NINVRETOD , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOD ;

INVRETOE = min(NINVRETOE , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOE ;

INVRETOF = min(NINVRETOF , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOF ;

INVRETOG = min(NINVRETOG , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOG ;

INVRETOX = min(NINVRETOX , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOX ;

INVRETOY = min(NINVRETOY , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOY ;

INVRETOZ = min(NINVRETOZ , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETOZ ;

INVRETUA = min(NINVRETUA , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUA ;

INVRETUB = min(NINVRETUB , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUB ;

INVRETUC = min(NINVRETUC , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUC ;

INVRETUD = min(NINVRETUD , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUD ;

INVRETUE = min(NINVRETUE , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUE ;

INVRETUF = min(NINVRETUF , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUF ;

INVRETUG = min(NINVRETUG , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUG ;

INVRETUH = min(NINVRETUH , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUH ;

INVRETUI = min(NINVRETUI , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUI ;

INVRETUJ = min(NINVRETUJ , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUJ ;

INVRETUK = min(NINVRETUK , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUK ;

INVRETUL = min(NINVRETUL , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUL ;

INVRETUM = min(NINVRETUM , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUM ;

INVRETUN = min(NINVRETUN , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUN ;

INVRETUO = min(NINVRETUO , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUO ;

INVRETUP = min(NINVRETUP , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUP ;

INVRETUQ = min(NINVRETUQ , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUQ ;

INVRETUR = min(NINVRETUR , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUR ;

INVRETUS = min(NINVRETUS , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUS ;

INVRETUT = min(NINVRETUT , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUT ;

INVRETUU = min(NINVRETUU , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETUU ;

INVRETVA = min(NINVRETVA , max(0 , PLAF_INVDOM -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVA ;

INVRETVB = min(NINVRETVB , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVB ;

INVRETVC = min(NINVRETVC , max(0 , PLAF_INVDOM3 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVC ;

INVRETVD = min(NINVRETVD , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVD ;

INVRETVE = min(NINVRETVE , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVE ;

INVRETVF = min(NINVRETVF , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVF ;

INVRETVG = min(NINVRETVG , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVG ;

INVRETVH = min(NINVRETVH , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVH ;

INVRETVI = min(NINVRETVI , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVI ;

INVRETVJ = min(NINVRETVJ , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVJ ;

INVRETVK = min(NINVRETVK , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + INVRETVK ;

INVRETVL = min(NINVRETVL , max(0 , PLAF_INVDOM4 -INVRETSOC-INVRETENT-VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = 0 ;

INVRETLOG = INVRETQL + INVRETQM + INVRETQD + INVRETOB + INVRETOC + INVRETOI + INVRETOJ + INVRETOK + INVRETOM + INVRETON + INVRETOP + INVRETOQ
            + INVRETOR + INVRETOT + INVRETOU + INVRETOV + INVRETOW + INVRETOD + INVRETOE + INVRETOF + INVRETOG + INVRETOX + INVRETOY + INVRETOZ 
            + INVRETUA + INVRETUB + INVRETUC + INVRETUD + INVRETUE + INVRETUF + INVRETUG + INVRETUH + INVRETUI + INVRETUJ + INVRETUK + INVRETUL
            + INVRETUM + INVRETUN + INVRETUO + INVRETUP + INVRETUQ + INVRETUR + INVRETUS + INVRETUT + INVRETUU + INVRETVA + INVRETVB + INVRETVC
	    + INVRETVD + INVRETVE + INVRETVF + INVRETVG + INVRETVH + INVRETVI + INVRETVJ + INVRETVK + INVRETVL ;

regle 402060:
application : iliad ;


RLOGDOM = (1 - V_CNR) * ( null(4-V_IND_TRAIT) * min(ALOGDOM , RRI1)
                        + null(5-V_IND_TRAIT) * min(min(RLOGDOM1731,ALOGDOM) , RRI1));
RLOGDOM_1 = min(ALOGDOM_1 , RRI1) * (1 - V_CNR);

RINVDOMTOMLG = RLOGDOM ;

regle 402110:
application : iliad ;


RRISUP = RRI1 - RLOGDOM - RCOMP - RRETU - RDONS - CRDIE - RLOCNPRO - RDUFREP - RPINELTOT - RNORMTOT - RNOUV 
              - RPENTOT - RFOR - RREHAB - RRESTREP - RRESTIMO1 - RCELTOT ; 


RDOMSOC1 = min(ADOMSOC1 , RRISUP) * (1 - V_CNR) ;
RDOMSOC1_1 = min(ADOMSOC1_1 , RRISUP) * (1 - V_CNR) ;

RLOGSOC = min(ALOGSOC , max(0 , RRISUP - ADOMSOC1)) * (1 - V_CNR) ;
RLOGSOC_1 = min(ALOGSOC_1 , max(0 , RRISUP - ADOMSOC1)) * (1 - V_CNR) ;

RLOGSOCTEO = (arr((((INVRETXQ + INVRETXQR) * (1 - INDPLAF) + (INVRETXQA + INVRETXQRA) * INDPLAF)) * TX65/100)
              + arr((((INVRETXR + INVRETXRR) * (1 - INDPLAF) + (INVRETXRA + INVRETXRRA) * INDPLAF)) * TX65/100)
              + arr((((INVRETXS + INVRETXSR) * (1 - INDPLAF) + (INVRETXSA + INVRETXSRA) * INDPLAF)) * TX65/100)
              + arr((((INVRETXT + INVRETXTR) * (1 - INDPLAF) + (INVRETXTA + INVRETXTRA) * INDPLAF)) * TX65/100)
              + arr((((INVRETXU + INVRETXUR) * (1 - INDPLAF) + (INVRETXUA + INVRETXURA) * INDPLAF)) * TX70/100)
              + arr((((INVRETYB + INVRETYBR) * (1 - INDPLAF) + (INVRETYBA + INVRETYBRA) * INDPLAF)) * TX70/100)
              + arr((((INVRETYA + INVRETYAR) * (1 - INDPLAF) + (INVRETYAA + INVRETYARA) * INDPLAF)) * TX65/100)
              + arr((((INVRETYD + INVRETYDR) * (1 - INDPLAF) + (INVRETYDA + INVRETYDRA) * INDPLAF)) * TX70/100)
              + arr((((INVRETYC + INVRETYCR) * (1 - INDPLAF) + (INVRETYCA + INVRETYCRA) * INDPLAF)) * TX65/100)
              + arr((((INVRETYE + INVRETYER) * (1 - INDPLAF) + (INVRETYEA + INVRETYERA) * INDPLAF)) * TX70/100)
              + arr((((INVRETYF + INVRETYFR) * (1 - INDPLAF) + (INVRETYFA + INVRETYFRA) * INDPLAF)) * TX70/100)
              + arr((((INVRETYG + INVRETYGR) * (1 - INDPLAF) + (INVRETYGA + INVRETYGRA) * INDPLAF)) * TX70/100)
             ) * (1 - V_CNR) ; 

regle 402120:
application : iliad ;


RCOLENT = min(ACOLENT , max(0 , RRISUP - ALOGSOC - ADOMSOC1)) * (1 - V_CNR) ;
RCOLENT_1 = min(ACOLENT_1 , max(0 , RRISUP - ALOGSOC_1 - ADOMSOC1_1)) * (1 - V_CNR) ;

RLOCENT = min(ALOCENT , max(0 , RRISUP - ALOGSOC - ADOMSOC1 - ACOLENT)) * (1 - V_CNR) ;
RLOCENT_1 = min(ALOCENT_1 , max(0 , RRISUP - ALOGSOC_1 - ADOMSOC1_1 - ACOLENT_1)) * (1 - V_CNR) ;

RIDOMENT = RLOCENT ;

RCOLENTTEO = (
              arr(((INVRETDI + INVRETDIR) * (1 - INDPLAF) + (INVRETDIA + INVRETDIRA) * INDPLAF) * TX5263/100)
              + arr(((INVRETDJ + INVRETDJR) * (1 - INDPLAF) + (INVRETDJA + INVRETDJRA) * INDPLAF) * TX625/100)

              + arr(((INVRETDN + INVRETDNR) * (1 - INDPLAF) + (INVRETDNA + INVRETDNRA) * INDPLAF) * TX5263/100)
              + arr(((INVRETDO + INVRETDOR) * (1 - INDPLAF) + (INVRETDOA + INVRETDORA) * INDPLAF) * TX625/100)
              + arr(((INVRETDS + INVRETDSR) * (1 - INDPLAF) + (INVRETDSA + INVRETDSRA) * INDPLAF) * TX56/100)
              + arr(((INVRETDT + INVRETDTR) * (1 - INDPLAF) + (INVRETDTA + INVRETDTRA) * INDPLAF) * TX66/100)
              + arr(((INVRETEN + INVRETENR) * (1 - INDPLAF) + (INVRETENA + INVRETENRA) * INDPLAF) * TX5263/100)
              + arr(((INVRETEO + INVRETEOR) * (1 - INDPLAF) + (INVRETEOA + INVRETEORA) * INDPLAF) * TX625/100)
              + arr(((INVRETES + INVRETESR) * (1 - INDPLAF) + (INVRETESA + INVRETESRA) * INDPLAF) * TX56/100)
              + arr(((INVRETET + INVRETETR) * (1 - INDPLAF) + (INVRETETA + INVRETETRA) * INDPLAF) * TX66/100)

              + arr(((INVRETFN + INVRETFNR) * (1 - INDPLAF) + (INVRETFNA + INVRETFNRA) * INDPLAF) * TX5263/100)
              + arr(((INVRETFO + INVRETFOR) * (1 - INDPLAF) + (INVRETFOA + INVRETFORA) * INDPLAF) * TX625/100)
              + arr(((INVRETFS + INVRETFSR) * (1 - INDPLAF) + (INVRETFSA + INVRETFSRA) * INDPLAF) * TX56/100)
              + arr(((INVRETFT + INVRETFTR) * (1 - INDPLAF) + (INVRETFTA + INVRETFTRA) * INDPLAF) * TX66/100)

              + arr(((INVRETGS + INVRETGSR) * (1 - INDPLAF) + (INVRETGSA + INVRETGSRA) * INDPLAF) * TX56/100)
              + arr(((INVRETGT + INVRETGTR) * (1 - INDPLAF) + (INVRETGTA + INVRETGTRA) * INDPLAF) * TX66/100)
              + arr(((INVRETHS + INVRETHSR) * (1 - INDPLAF) + (INVRETHSA + INVRETHSRA) * INDPLAF) * TX56/100)
              + arr(((INVRETHT + INVRETHTR) * (1 - INDPLAF) + (INVRETHTA + INVRETHTRA) * INDPLAF) * TX66/100)
              + arr(((INVRETIS + INVRETISR) * (1 - INDPLAF) + (INVRETISA + INVRETISRA) * INDPLAF) * TX56/100)
              + arr(((INVRETIT + INVRETITR) * (1 - INDPLAF) + (INVRETITA + INVRETITRA) * INDPLAF) * TX66/100)
              ) * (1 - V_CNR) ;

regle 402130:
application : iliad ;


RRIREP_1 = RRI1 - DLOGDOM - RCOMP_1 - RRETU_1 - RDONS_1 - CRDIE - RLOCNPRO_1 - RDUFREP_1 - RPINELTOT_1 - RNORMTOT_1 - RNOUV_1 
                - RPENTOT_1 - RFOR_1 - RREHAB_1 - RRESTREP_1 - RRESTIMO1_1 - RCELTOT_1 ;

RRIREP = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RRIREP_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RRIREP_1 , max(RRIREP_P,RRIREP1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

VARTMP1 = 0 ;

REPYB = max(0 , CODHYB - max(0 , RRIREP - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHYB ;

REPYA = max(0 , CODHYA - max(0 , RRIREP - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHYA ;

REPDOMSOC4 = REPYB + REPYA ;


REPYD = max(0 , CODHYD - max(0 , RRIREP - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHYD ;

REPYC = max(0 , CODHYC - max(0 , RRIREP - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHYC ;

REPDOMSOC3 = REPYD + REPYC ;


REPYE = max(0 , CODHYE - max(0 , RRIREP - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHYE ;

REPDOMSOC2 = REPYE ;


REPYF = max(0 , CODHYF - max(0 , RRIREP - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHYF ;

REPDOMSOC1 = REPYF ;


REPYG = max(0 , CODHYG - max(0 , RRIREP - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHYG ;

REPDOMSOC = REPYG ;

REPSOC = CODHXU + CODHXQ + CODHXR + CODHXS + CODHXT + CODHYB 
         + CODHYA + CODHYD + CODHYC + CODHYE + CODHYF + CODHYG ;


REPENT5 = CODHDT + CODHDJ + CODHDO + CODHDS + CODHDI + CODHDN + CODHDK + CODHDP + CODHDU + CODHDM + CODHDR + CODHDW ;


REPET = max(0 , CODHET - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHET ; 

REPEO = max(0 , CODHEO - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHEO ;

REPES = max(0 , CODHES - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHES ;

REPEN = max(0 , CODHEN - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHEN ;

REPEP = max(0 , CODHEP - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHEP ;

REPEU = max(0 , CODHEU - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHEU ;

REPER = max(0 , CODHER - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHER ;

REPEW = max(0 , CODHEW - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHEW ;

REPDOMENTR4 = REPET + REPEO + REPES + REPEN + REPEP + REPEU + REPER + REPEW ;


REPFT = max(0 , CODHFT - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHFT ; 

REPFO = max(0 , CODHFO - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHFO ;

REPFS = max(0 , CODHFS - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHFS ;

REPFN = max(0 , CODHFN - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHFN ;

REPFP = max(0 , CODHFP - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHFP ;

REPFU = max(0 , CODHFU - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHFU ;

REPFR = max(0 , CODHFR - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHFR ;

REPFW = max(0 , CODHFW - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHFW ;

REPDOMENTR3 = REPFT + REPFO + REPFS + REPFN + REPFP + REPFU + REPFR + REPFW ;


REPGT = max(0 , CODHGT - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHGT ; 

REPGS = max(0 , CODHGS - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHGS ; 

REPGU = max(0 , CODHGU - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHGU ; 

REPGW = max(0 , CODHGW - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHGW ;

REPDOMENTR2 = REPGT + REPGS + REPGU + REPGW ;


REPHT = max(0 , CODHHT - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHHT ; 

REPHS = max(0 , CODHHS - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHHS ; 

REPHU = max(0 , CODHHU - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHHU ; 

REPHW = max(0 , CODHHW - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHHW ;

REPDOMENTR1 = REPHT + REPHS + REPHU + REPHW ;


REPIT = max(0 , CODHIT - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHIT ; 

REPIS = max(0 , CODHIS - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHIS ; 

REPIU = max(0 , CODHIU - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = VARTMP1 + CODHIU ; 

REPIW = max(0 , CODHIW - max(0 , RRIREP - REPENT5 - VARTMP1)) * (1 - V_CNR) ;
VARTMP1 = 0 ;

REPDOMENTR = REPIT + REPIS + REPIU + REPIW ;




RIDEFRI = (1-COD9ZA+0) * (positif(positif(RED_1 - RED1731+0+SOMMERI1731*(1-V_INDTEO)*positif(RED_1))
                    + positif(RDONIFI_1+RDONIFI2_1 - (RDONIFI11731 + RDONIFI21731)*(1-V_INDTEO)*positif(RDONIFI_1+RDONIFI2_1) * ART1731BISIFI)) * positif(1-PREM8_11)
                    + positif(positif(RED1731+0 + SOMMERI1731*(1-V_INDTEO))+ positif(RDONIFI_1+RDONIFI2_1 - (RDONIFI11731 + RDONIFI21731)*(1-V_INDTEO))) * positif(PREM8_11)) * null(V_IND_TRAIT - 5);
       
regle 4666:
application : iliad ;

TOTRI3WG = RAPRESSE + RAFORET + RFIPDOMCOM + RFIPCORSE + RRS + RRCN + RFIP + RENOV + ACOMP
           + ADUFREP + APIREP + ANORMREP + RPINABCD + RPINRRS + RNORABCD
	   + CELRREDLQ + CELRREDLR + CELRREDLU 
	   + CELRREDLV + COD7LA + COD7LB + COD7LC + COD7LY
	   + COD7MS + COD7MT + COD7MU + COD7MV
	   + CELREPYP + CELREPYO + CELREPYN + CELREPYM 
	   + CELREPYW + CELREPYV + CELREPYU + CELREPYT 
	   + CELREPWT + CELREPWU + CELREPWV + CELREPWW 
           + COD7RU + COD7RT
	   + RCEL7IQ + RCEL7LD + RCEL7LE + RCEL7LF + RCEL7LN 
	   + RCEL7LT + RCEL7LX + RCEL7LZ + RCEL7MG + RCEL7MH
	   + RAH + RAALIM + RSNNCL + RSNNCC + RSNNCR
	   + RSNNCV + RSNNCX + RSN + APLAFREPME4 + APENTCY + APENTDY + APENTEY + APENTFY + APENTGY
	   + DILMNP1 + DILMNP3
	   + RETCODIM + RETCODIN + RETCODIJ + RETCODID 
	   + RETCODJZ + RETCODOU + RETCODOV + RETCODOW + RETCODOX + RETCODOY + RETCODPZ + RETCODMZ + RETCODMW + RETCODMN + RSOCREP + RETRESTIMO + RONS 
	   + CRCFA + RETUD + RFCPI + RPRESCOMP + arr(ACOTFOR_R * TX76/100) + RFOREST
	   + RAGRI + TOTINVDOM ;

regle 4700:
application : iliad ;
RED3WG =  max( min(TOTRI3WG , IDOM13-DEC13) , 0 ) ;
regle 4800:
application : iliad ;

TOTINVDOM = INVLOG2008 +INVLGDEB2009 +INVLGDEB +INVOMLOGOA
           +INVOMLOGOH +INVOMLOGOL +INVOMLOGOO +INVOMLOGOS
           +(INVRETQL * (1 - INDPLAF) + INVRETQLA * INDPLAF)
           +(INVRETQM * (1 - INDPLAF) + INVRETQMA * INDPLAF)
           +(INVRETQD * (1 - INDPLAF) + INVRETQDA * INDPLAF)
           +(INVRETOB * (1 - INDPLAF) + INVRETOBA * INDPLAF)
           +(INVRETOC * (1 - INDPLAF) + INVRETOCA * INDPLAF)
           +(INVRETOI * (1 - INDPLAF) + INVRETOIA * INDPLAF)
           +(INVRETOJ * (1 - INDPLAF) + INVRETOJA * INDPLAF)
           +(INVRETOK * (1 - INDPLAF) + INVRETOKA * INDPLAF)
           +(INVRETOM * (1 - INDPLAF) + INVRETOMA * INDPLAF)
           +(INVRETON * (1 - INDPLAF) + INVRETONA * INDPLAF)
           +(INVRETOP * (1 - INDPLAF) + INVRETOPA * INDPLAF)
           +(INVRETOQ * (1 - INDPLAF) + INVRETOQA * INDPLAF)
           +(INVRETOR * (1 - INDPLAF) + INVRETORA * INDPLAF)
           +(INVRETOT * (1 - INDPLAF) + INVRETOTA * INDPLAF)
           +(INVRETOU * (1 - INDPLAF) + INVRETOUA * INDPLAF)
           +(INVRETOV * (1 - INDPLAF) + INVRETOVA * INDPLAF)
           +(INVRETOW * (1 - INDPLAF) + INVRETOWA * INDPLAF)
           +(INVRETOD * (1 - INDPLAF) + INVRETODA * INDPLAF)
           +(INVRETOE * (1 - INDPLAF) + INVRETOEA * INDPLAF)
           +(INVRETOF * (1 - INDPLAF) + INVRETOFA * INDPLAF)
           +(INVRETOG * (1 - INDPLAF) + INVRETOGA * INDPLAF)
           +(INVRETOX * (1 - INDPLAF) + INVRETOXA * INDPLAF)
           +(INVRETOY * (1 - INDPLAF) + INVRETOYA * INDPLAF)
           +(INVRETOZ * (1 - INDPLAF) + INVRETOZA * INDPLAF)
           +(INVRETUA * (1 - INDPLAF) + INVRETUAA * INDPLAF)
           +(INVRETUB * (1 - INDPLAF) + INVRETUBA * INDPLAF)
           +(INVRETUC * (1 - INDPLAF) + INVRETUCA * INDPLAF)
           +(INVRETUD * (1 - INDPLAF) + INVRETUDA * INDPLAF)
           +(INVRETUE * (1 - INDPLAF) + INVRETUEA * INDPLAF)
           +(INVRETUF * (1 - INDPLAF) + INVRETUFA * INDPLAF)
           +(INVRETUG * (1 - INDPLAF) + INVRETUGA * INDPLAF)
           +(INVRETUH * (1 - INDPLAF) + INVRETUHA * INDPLAF)
           +(INVRETUI * (1 - INDPLAF) + INVRETUIA * INDPLAF)
           +(INVRETUJ * (1 - INDPLAF) + INVRETUJA * INDPLAF)
           +(INVRETUK * (1 - INDPLAF) + INVRETUKA * INDPLAF)
           +(INVRETUL * (1 - INDPLAF) + INVRETULA * INDPLAF)
           +(INVRETUM * (1 - INDPLAF) + INVRETUMA * INDPLAF)
           +(INVRETUN * (1 - INDPLAF) + INVRETUNA * INDPLAF)
           +(INVRETUO * (1 - INDPLAF) + INVRETUOA * INDPLAF)
           +(INVRETUP * (1 - INDPLAF) + INVRETUPA * INDPLAF)
           +(INVRETUQ * (1 - INDPLAF) + INVRETUQA * INDPLAF)
           +(INVRETUR * (1 - INDPLAF) + INVRETURA * INDPLAF)
           +(INVRETUS * (1 - INDPLAF) + INVRETUSA * INDPLAF)
           +(INVRETUT * (1 - INDPLAF) + INVRETUTA * INDPLAF)
           +(INVRETUU * (1 - INDPLAF) + INVRETUUA * INDPLAF)
           +(INVRETVA * (1 - INDPLAF) + INVRETVAA * INDPLAF)
           +(INVRETVB * (1 - INDPLAF) + INVRETVBA * INDPLAF)
           +(INVRETVC * (1 - INDPLAF) + INVRETVCA * INDPLAF)
           +(INVRETVD * (1 - INDPLAF) + INVRETVDA * INDPLAF)
           +(INVRETVE * (1 - INDPLAF) + INVRETVEA * INDPLAF)
           +(INVRETVF * (1 - INDPLAF) + INVRETVFA * INDPLAF)
           +(INVRETVG * (1 - INDPLAF) + INVRETVGA * INDPLAF)
           +(INVRETVH * (1 - INDPLAF) + INVRETVHA * INDPLAF)
           +(INVRETVI * (1 - INDPLAF) + INVRETVIA * INDPLAF)
           +(INVRETVJ * (1 - INDPLAF) + INVRETVJA * INDPLAF)
           +(INVRETVK * (1 - INDPLAF) + INVRETVKA * INDPLAF) 
           +(INVRETVL * (1 - INDPLAF) + INVRETVLA * INDPLAF) ;

regle 402160 :
application : iliad ;

DREHAB = COD7XX ;

AREHAB_1 = DREHAB * (1 - V_CNR) ;
AREHAB = positif(null(V_IND_TRAIT-4)+COD9ZA) * (AREHAB_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(AREHAB_1,max(AREHAB_P,AREHAB1731)))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;

RRREHAP = arr(AREHAB_1 * TX20/100) ;

regle 402161 :
application : iliad ;
RREHAB_1 = max(min(RRREHAP , RRI1-RLOGDOM-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPENTOT-RFOR) , 0) ;
RREHAB = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RREHAB_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
         + (max(0 , min(RREHAB_1 , max(RREHAB_P,RREHAB1731))) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;



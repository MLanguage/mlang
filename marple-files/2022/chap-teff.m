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
regle 99991000:
application : iliad  ;

TMIB_TVENV = MIBVENV + AUTOBICVV + MIBNPVENV + MIBGITEV+LOCGITV;
TMIB_TVENC = MIBVENC + AUTOBICVC + MIBNPVENC + MIBGITEC+LOCGITC;
TMIB_TVENP = MIBVENP + AUTOBICVP + MIBNPVENP + MIBGITEP+LOCGITP;

TMIB_TPRESV = MIBPRESV + AUTOBICPV + MIBNPPRESV + MIBMEUV+COD5NW;
TMIB_TPRESC = MIBPRESC + AUTOBICPC + MIBNPPRESC + MIBMEUC+COD5OW;
TMIB_TPRESP = MIBPRESP + AUTOBICPP + MIBNPPRESP + MIBMEUP+COD5PW;

TMIB_TTV = TMIB_TVENV + TMIB_TPRESV;
TMIB_TTC = TMIB_TVENC + TMIB_TPRESC;
TMIB_TTP = TMIB_TVENP + TMIB_TPRESP;
regle 99991004:
application : iliad  ;


TMIB_AVV = min ( TMIB_TVENV,
                         (max(MIN_MBIC,
                              arr( (TMIB_TVENV)*TX_MIBVEN/100))
                         )
              );
TMIB_AVC = min ( TMIB_TVENC,
                         (max(MIN_MBIC,
                              arr( (TMIB_TVENC)*TX_MIBVEN/100))
                         )
              );
TMIB_AVP = min ( TMIB_TVENP,
                         (max(MIN_MBIC,
                              arr( (TMIB_TVENP)*TX_MIBVEN/100))
                         )
              );
TMIB_VENTAV = min ( (MIBVENV + MIBNPVENV),
                         (max(MIN_MBIC,
                              arr( (MIBVENV + MIBNPVENV)*TX_MIBVEN/100))
                         )
              );
TMIB_VENTAC = min ( (MIBVENC + MIBNPVENC),
                         (max(MIN_MBIC,
                              arr( (MIBVENC + MIBNPVENC)*TX_MIBVEN/100))
                         )
              );
TMIB_VENTAP = min ( (MIBVENP + MIBNPVENP),
                         (max(MIN_MBIC,
                              arr( (MIBVENP + MIBNPVENP)*TX_MIBVEN/100))
                         )
              );
TMIB_AUTOAVV = TMIB_AVV - TMIB_VENTAV; 
TMIB_AUTOAVC = TMIB_AVC - TMIB_VENTAC; 
TMIB_AUTOAVP = TMIB_AVP - TMIB_VENTAP; 


TMIB_APV = min ( TMIB_TPRESV,
                         (max(MIN_MBIC,
                              arr( (TMIB_TPRESV)*TX_MIBPRES/100))
                         )
               );
TMIB_APC = min ( TMIB_TPRESC,
                         (max(MIN_MBIC,
                              arr( (TMIB_TPRESC)*TX_MIBPRES/100))
                         )
               );
TMIB_APP = min ( TMIB_TPRESP,
                         (max(MIN_MBIC,
                              arr( (TMIB_TPRESP)*TX_MIBPRES/100))
                         )
               );
TMIB_PRESAV = min ( (MIBPRESV + MIBNPPRESV),
                         (max(MIN_MBIC,
                              arr( (MIBPRESV + MIBNPPRESV)*TX_MIBPRES/100))
                         )
               );
TMIB_PRESAC = min ( (MIBPRESC + MIBNPPRESC),
                         (max(MIN_MBIC,
                              arr( (MIBPRESC + MIBNPPRESC)*TX_MIBPRES/100))
                         )
               );
TMIB_PRESAP = min ( (MIBPRESP + MIBNPPRESP),
                         (max(MIN_MBIC,
                              arr( (MIBPRESP + MIBNPPRESP)*TX_MIBPRES/100))
                         )
               );
TMIB_AUTOAPV = TMIB_APV - TMIB_PRESAV; 
TMIB_AUTOAPC = TMIB_APC - TMIB_PRESAC; 
TMIB_AUTOAPP = TMIB_APP - TMIB_PRESAP; 
TPMIB_AVV = min ( (MIBVENV + AUTOBICVV),
                         (max(MIN_MBIC,
                              arr( (MIBVENV+ AUTOBICVV)*TX_MIBVEN/100))
                         )
              );
TPMIB_AVC = min ( (MIBVENC + AUTOBICVC),
                         (max(MIN_MBIC,
                              arr( (MIBVENC+ AUTOBICVC)*TX_MIBVEN/100))
                         )
              );
TPMIB_AVP = min ( (MIBVENP + AUTOBICVP),
                         (max(MIN_MBIC,
                              arr( (MIBVENP+ AUTOBICVP)*TX_MIBVEN/100))
                         )
              );
TPMIB_APV = min ( (MIBPRESV+ AUTOBICPV),
                         (max(MIN_MBIC,
                              arr( (MIBPRESV+ AUTOBICPV)*TX_MIBPRES/100))
                         )
               );
TPMIB_APC = min ( (MIBPRESC+ AUTOBICPC),
                         (max(MIN_MBIC,
                              arr( (MIBPRESC+ AUTOBICPC)*TX_MIBPRES/100))
                         )
               );
TPMIB_APP = min ( (MIBPRESP+ AUTOBICPP),
                         (max(MIN_MBIC,
                              arr( (MIBPRESP+ AUTOBICPP)*TX_MIBPRES/100))
                         )
               );


regle 99991005:
application : iliad  ;

TMIB_ABVV = max(0,arr(TMIB_AVV * (MIBVENV + AUTOBICVV)/ (TMIB_TVENV)));
TMIB_ABVC = max(0,arr(TMIB_AVC * (MIBVENC + AUTOBICVC)/ (TMIB_TVENC)));
TMIB_ABVP = max(0,arr(TMIB_AVP * (MIBVENP + AUTOBICVP)/ (TMIB_TVENP)));
TMIB_ABNPVV = max(0,arr(TMIB_AVV * MIBNPVENV / TMIB_TVENV))* positif(present(MIBGITEV)+present(LOCGITV))
	      + (TMIB_AVV - TMIB_ABVV) * (1 - positif(present(MIBGITEV)+present(LOCGITV)));
TMIB_ABNPVC = max(0,arr(TMIB_AVC * MIBNPVENC / TMIB_TVENC))* positif(present(MIBGITEC)+present(LOCGITC))
	      + (TMIB_AVC - TMIB_ABVC) * (1 - positif(present(MIBGITEC)+present(LOCGITC)));
TMIB_ABNPVP = max(0,arr(TMIB_AVP * MIBNPVENP / TMIB_TVENP))* positif(present(MIBGITEP)+present(LOCGITP))
	      + (TMIB_AVP - TMIB_ABVP) * (1 - positif(present(MIBGITEP)+present(LOCGITP)));
TMIB_ABNPVLV = (TMIB_AVV - TMIB_ABVV - TMIB_ABNPVV) *  positif(present(MIBGITEV)+present(LOCGITV));
TMIB_ABNPVLC = (TMIB_AVC - TMIB_ABVC - TMIB_ABNPVC) *  positif(present(MIBGITEC)+present(LOCGITC));
TMIB_ABNPVLP = (TMIB_AVP - TMIB_ABVP - TMIB_ABNPVP) *  positif(present(MIBGITEP)+present(LOCGITP));

TMIB_ABPV = max(0,arr(TMIB_APV * (MIBPRESV + AUTOBICPV)/ (TMIB_TPRESV)));
TMIB_ABPC = max(0,arr(TMIB_APC * (MIBPRESC + AUTOBICPC)/ (TMIB_TPRESC)));
TMIB_ABPP = max(0,arr(TMIB_APP * (MIBPRESP + AUTOBICPP)/ (TMIB_TPRESP)));
TMIB_ABNPPV = max(0,arr(TMIB_APV * MIBNPPRESV / (TMIB_TPRESV))) * positif(present(MIBMEUV)+present(COD5NW))
	      + (TMIB_APV - TMIB_ABPV) * (1 - positif(present(MIBMEUV)+present(COD5NW)));
TMIB_ABNPPC = max(0,arr(TMIB_APC * MIBNPPRESC / (TMIB_TPRESC))) * positif(present(MIBMEUC)+present(COD5OW))
	      + (TMIB_APC - TMIB_ABPC) * (1 - positif(present(MIBMEUC)+present(COD5OW)));
TMIB_ABNPPP = max(0,arr(TMIB_APP * MIBNPPRESP / (TMIB_TPRESP))) * positif(present(MIBMEUP)+present(COD5PW))
	      + (TMIB_APP - TMIB_ABPP) * (1 - positif(present(MIBMEUP)+present(COD5PW)));
TMIB_ABNPPLV = (TMIB_APV - TMIB_ABPV - TMIB_ABNPPV) *  positif(present(MIBMEUV)+present(COD5NW));
TMIB_ABNPPLC = (TMIB_APC - TMIB_ABPC - TMIB_ABNPPC) *  positif(present(MIBMEUC)+present(COD5OW));
TMIB_ABNPPLP = (TMIB_APP - TMIB_ABPP - TMIB_ABNPPP) *  positif(present(MIBMEUP)+present(COD5PW));


regle 99991006:
application : iliad  ;
TPMIB_NETVV = MIBVENV + AUTOBICVV - TPMIB_AVV;
TPMIB_NETVC = MIBVENC + AUTOBICVC - TPMIB_AVC;
TPMIB_NETVP = MIBVENP + AUTOBICVP - TPMIB_AVP;
TPMIB_NETPV = MIBPRESV + AUTOBICPV - TPMIB_APV;
TPMIB_NETPC = MIBPRESC + AUTOBICPC - TPMIB_APC;
TPMIB_NETPP = MIBPRESP + AUTOBICPP - TPMIB_APP;

TMIB_NETVV = MIBVENV + AUTOBICVV - TMIB_ABVV;
TMIB_NETVC = MIBVENC + AUTOBICVC - TMIB_ABVC;
TMIB_NETVP = MIBVENP + AUTOBICVP - TMIB_ABVP;
TMIBNETVF = somme(i=V,C,P:TMIB_NETVi) ;
TMIB_NETNPVV = MIBNPVENV - TMIB_ABNPVV;
TMIB_NETNPVC = MIBNPVENC - TMIB_ABNPVC;
TMIB_NETNPVP = MIBNPVENP - TMIB_ABNPVP;
TMIBNETNPVF = somme(i=V,C,P:TMIB_NETNPVi);

TMIB_NETPV = MIBPRESV + AUTOBICPV - TMIB_ABPV;
TMIB_NETPC = MIBPRESC + AUTOBICPC - TMIB_ABPC;
TMIB_NETPP = MIBPRESP + AUTOBICPP - TMIB_ABPP;
TMIBNETPF = somme(i=V,C,P:TMIB_NETPi) ;
TMIB_NETNPPV = MIBNPPRESV - TMIB_ABNPPV;
TMIB_NETNPPC = MIBNPPRESC - TMIB_ABNPPC;
TMIB_NETNPPP = MIBNPPRESP - TMIB_ABNPPP;
TMIBNETNPPF = somme(i=V,C,P:TMIB_NETNPPi);
TMIBNETNPTOT = TMIBNETNPVF + TMIBNETNPPF + MIB_NETNPCT;

TBICPABV =   arr((TMIB_ABVV * AUTOBICVV/(MIBVENV+AUTOBICVV))
          + (TMIB_ABPV * AUTOBICPV/(MIBPRESV+AUTOBICPV)));

TBICPROVC = max(0,arr((TMIB_ABVC * AUTOBICVC/(MIBVENC+AUTOBICVC)) + (TMIB_ABPC * AUTOBICPC/(MIBPRESC+AUTOBICPC))));

TBICPABC =  min(TBICPROVC,arr((TMIB_ABVC * AUTOBICVC/(MIBVENC+AUTOBICVC))
          + (TMIB_ABPC * AUTOBICPC/(MIBPRESC+AUTOBICPC))));

TBICPROVP = max(0,arr((TMIB_ABVP * AUTOBICVP/(MIBVENP+AUTOBICVP)) + (TMIB_ABPP * AUTOBICPP/(MIBPRESP+AUTOBICPP))));

TBICPABP =  min(TBICPROVP,arr((TMIB_ABVP * AUTOBICVP/(MIBVENP+AUTOBICVP))
          + (TMIB_ABPP* AUTOBICPP/(MIBPRESP+AUTOBICPP))));

TBICNPABV = arr((TMIB_ABNPVV /(MIBNPVENV))
          + (TMIB_ABNPPV /(MIBNPPRESV)));
TBICNPPROVC = max(0,arr((TMIB_ABNPVC /(MIBNPVENC)) + (TMIB_ABNPPC /(MIBNPPRESC))));
TBICNPABC = min(TBICNPPROVC,arr((TMIB_ABNPVC /(MIBNPVENC))
          + (TMIB_ABNPPC /(MIBNPPRESC))));
TBICNPPROVP = max(0,arr((TMIB_ABNPVP /(MIBNPVENP)) + (TMIB_ABNPPP /(MIBNPPRESP))));
TBICNPABP = min(TBICNPPROVP,arr((TMIB_ABNPVP /(MIBNPVENP))
          + (TMIB_ABNPPP /(MIBNPPRESP))));
ABICPDECV = AUTOBICVV + AUTOBICPV;
ABICPDECC = AUTOBICVC + AUTOBICPC;
ABICPDECP = AUTOBICVP + AUTOBICPP;
ABICPNETV =  AUTOBICVV + AUTOBICPV - max(0,TMIB_AUTOAVV-TMIB_ABNPVLV) -max(0,TMIB_AUTOAPV-TMIB_ABNPPLV);
ABICPNETC =  AUTOBICVC + AUTOBICPC - max(0,TMIB_AUTOAVC-TMIB_ABNPVLC) -max(0,TMIB_AUTOAPC-TMIB_ABNPPLC);
ABICPNETP =  AUTOBICVP + AUTOBICPP - max(0,TMIB_AUTOAVP-TMIB_ABNPVLP) -max(0,TMIB_AUTOAPP-TMIB_ABNPPLP);

AUTOBICPNET = ABICPNETV + ABICPNETC + ABICPNETP;





regle 91040010:
application:iliad;

TBICPROONCV=BIPTAV+BIHTAV+(TMIB_NETVV+TMIB_NETPV)+MIB_NETCV;
TBICPROONCC=BIPTAC+BIHTAC+(TMIB_NETVC+TMIB_NETPC)+MIB_NETCC;
TBICPROONCP=BIPTAP+BIHTAP+(TMIB_NETVP+TMIB_NETPP)+MIB_NETCP;


regle 91040020:
application:iliad;

TBICPROOCV = min(0 , TBICPROONCV + BICPROQNCV) * positif(BICPROQNCV) * (1 - positif_ou_nul(TBICPROONCV))
             + TBICPROONCV * (1 - positif(BICPROQNCV) * (1 - positif_ou_nul(TBICPROONCV))) ;

TBICPROOCC = min(0 , TBICPROONCC + BICPROQNCC) * positif(BICPROQNCC) * (1 - positif_ou_nul(TBICPROONCC))
             + TBICPROONCC * (1 - positif(BICPROQNCC) * (1 - positif_ou_nul(TBICPROONCC))) ;

TBICPROOCP = min(0 , TBICPROONCP + BICPROQNCP) * positif(BICPROQNCP) * (1 - positif_ou_nul(TBICPROONCP))
             + TBICPROONCP * (1 - positif(BICPROQNCP) * (1 - positif_ou_nul(TBICPROONCP))) ;

TBICPROOCF = TBICPROOCV + TBICPROOCC + TBICPROOCP ;

TBICPROQCV = max(0 , TBICPROONCV + BICPROQNCV) * positif(BICPROQNCV) * (1 - positif_ou_nul(TBICPROONCV))
             + BICPROQNCV * (1 - positif(BICPROQNCV) * (1 - positif_ou_nul(TBICPROONCV))) ;

TBICPROQCC = max(0 , TBICPROONCC + BICPROQNCC) * positif(BICPROQNCC) * (1 - positif_ou_nul(TBICPROONCC))
             + BICPROQNCC * (1 - positif(BICPROQNCC) * (1 - positif_ou_nul(TBICPROONCC))) ;

TBICPROQCP = max(0 , TBICPROONCP + BICPROQNCP) * positif(BICPROQNCP) * (1 - positif_ou_nul(TBICPROONCP))
             + BICPROQNCP * (1 - positif(BICPROQNCP) * (1 - positif_ou_nul(TBICPROONCP))) ;

TBICPROQCF = TBICPROQCV + TBICPROQCC + TBICPROQCP ;


regle 91040030:
application:iliad;

TBICPROOF = min(0 , TBICPROOCF + TBICPROQCF) * positif(TBICPROQCF) * (1 - positif_ou_nul(TBICPROOCF))
            + TBICPROOCF * (1 - positif(TBICPROQCF) * (1 - positif_ou_nul(TBICPROOCF))) ;

TBICPROQF = max(0 , TBICPROOCF + TBICPROQCF) * positif(TBICPROQCF) * (1 - positif_ou_nul(TBICPROOCF))
            + TBICPROQCF * (1 - positif(TBICPROQCF) * (1 - positif_ou_nul(TBICPROOCF))) ;



regle 91040040:
application:iliad;

TBICNPONCV=BINTAV+BINHTAV+(TMIB_NETNPVV+TMIB_NETNPPV)+MIBNPPVV-MIBNPDCT;
TBICNPONCC=BINTAC+BINHTAC+(TMIB_NETNPVC+TMIB_NETNPPC)+MIBNPPVC-COD5RZ;
TBICNPONCP=BINTAP+BINHTAP+(TMIB_NETNPVP+TMIB_NETNPPP)+MIBNPPVP-COD5SZ;


regle 91040050:
application:iliad;

TBICNPOCV = min(0 , TBICNPONCV + BICNPQNCV) * positif(BICNPQNCV) * (1 - positif_ou_nul(TBICNPONCV))
            + TBICNPONCV * (1 - positif(BICNPQNCV) * (1 - positif_ou_nul(TBICNPONCV))) ;

TBICNPOCC = min(0 , TBICNPONCC + BICNPQNCC) * positif(BICNPQNCC) * (1 - positif_ou_nul(TBICNPONCC))
            + TBICNPONCC * (1 - positif(BICNPQNCC) * (1 - positif_ou_nul(TBICNPONCC))) ;

TBICNPOCP = min(0 , TBICNPONCP + BICNPQNCP) * positif(BICNPQNCP) * (1 - positif_ou_nul(TBICNPONCP))
            + TBICNPONCP * (1 - positif(BICNPQNCP) * (1 - positif_ou_nul(TBICNPONCP))) ;

TBICNPOCF = TBICNPOCV + TBICNPOCC + TBICNPOCP ;

TBICNPQCV = max(0 , TBICNPONCV + BICNPQNCV) * positif(BICNPQNCV) * (1 - positif_ou_nul(TBICNPONCV))
            + BICNPQNCV * (1 - positif(BICNPQNCV) * (1 - positif_ou_nul(TBICNPONCV))) ;

TBICNPQCC = max(0 , TBICNPONCC + BICNPQNCC) * positif(BICNPQNCC) * (1 - positif_ou_nul(TBICNPONCC))
            + BICNPQNCC * (1 - positif(BICNPQNCC) * (1 - positif_ou_nul(TBICNPONCC))) ;

TBICNPQCP = max(0 , TBICNPONCP + BICNPQNCP) * positif(BICNPQNCP) * (1 - positif_ou_nul(TBICNPONCP))
            + BICNPQNCP * (1 - positif(BICNPQNCP) * (1 - positif_ou_nul(TBICNPONCP))) ;

TBICNPQCF = TBICNPQCV + TBICNPQCC + TBICNPQCP ;



regle 91040060:
application:iliad;

TBICNPOF = min(0 , TBICNPOCF + TBICNPQCF) * positif(TBICNPQCF) * (1 - positif_ou_nul(TBICNPOCF))
           + TBICNPOCF * (1 - positif(TBICNPQCF) * (1 - positif_ou_nul(TBICNPOCF))) ;

TBICNPQF = max(0 , TBICNPOCF + TBICNPQCF) * positif(TBICNPQCF) * (1 - positif_ou_nul(TBICNPOCF))
           + TBICNPQCF * (1 - positif(TBICNPQCF) * (1 - positif_ou_nul(TBICNPOCF))) ;


regle 91040070:
application:iliad;

TBICNPOTF=positif(TBICNPOF)*max(0,(TBICNPOF-TDEFNPI))+(1-positif(TBICNPOF))*TBICNPOF;

TBICNPQTF=max(0,TBICNPQF-max(0,TDEFNPI-TBICNPOF));




regle 99991009:                                                                    
application : iliad   ;                          
TSPETOTV = BNCPROV + AUTOBNCV + BNCNPV ;
TSPETOTC = BNCPROC + AUTOBNCC + BNCNPC ;
TSPETOTP = BNCPROP + AUTOBNCP + BNCNPP ;
regle 99991010:
application : iliad   ;                          
TSPEBASABV =TSPETOTV;
TSPEBASABC =TSPETOTC;
TSPEBASABP =TSPETOTP;
TSPEABV = arr((max(MIN_SPEBNC,(TSPEBASABV * SPETXAB/100))) * 
                       positif_ou_nul(TSPETOTV - MIN_SPEBNC)) +
          arr((min(MIN_SPEBNC,TSPEBASABV )) * 
                       positif(MIN_SPEBNC - TSPETOTV)); 
TSPEABC = arr((max(MIN_SPEBNC,(TSPEBASABC * SPETXAB/100))) * 
                       positif_ou_nul(TSPETOTC - MIN_SPEBNC)) +
          arr((min(MIN_SPEBNC,TSPEBASABC )) * 
                       positif(MIN_SPEBNC - TSPETOTC)); 
TSPEABP = arr((max(MIN_SPEBNC,(TSPEBASABP * SPETXAB/100))) * 
                       positif_ou_nul(TSPETOTP - MIN_SPEBNC)) +
          arr((min(MIN_SPEBNC,TSPEBASABP )) * 
                       positif(MIN_SPEBNC - TSPETOTP)); 
regle 99991011:
application : iliad   ;                          
TSPEABPV = arr((TSPEABV * (BNCPROV + AUTOBNCV))/TSPETOTV);                                  
TSPEABPC = arr((TSPEABC * (BNCPROC + AUTOBNCC))/TSPETOTC);                                  
TSPEABPP = arr((TSPEABP * (BNCPROP + AUTOBNCP))/TSPETOTP);                                  
TBNCPABV = arr(TSPEABPV * AUTOBNCV/(BNCPROV+AUTOBNCV)); 
TBNCPABC = arr(TSPEABPC * AUTOBNCC/(BNCPROC+AUTOBNCC)); 
TBNCPABP = arr(TSPEABPP * AUTOBNCP/(BNCPROP+AUTOBNCP)); 
TBNCTOTABV = arr(TSPEABV * (AUTOBNCV)/(TSPETOTV)); 
TBNCTOTABC = arr(TSPEABC * (AUTOBNCC)/(TSPETOTC)); 
TBNCTOTABP = arr(TSPEABP * (AUTOBNCP)/(TSPETOTP)); 

TSPEABNPV = TSPEABV - TSPEABPV;                                  
TSPEABNPC = TSPEABC - TSPEABPC;                                  
TSPEABNPP = TSPEABP - TSPEABPP;                                  
TBNCNPABV = (TBNCTOTABV - TBNCPABV) ;
TBNCNPABC = (TBNCTOTABC - TBNCPABC) ;
TBNCNPABP = (TBNCTOTABP - TBNCPABP) ;

ABNCPDECV =  AUTOBNCV; 
ABNCPDECC =  AUTOBNCC; 
ABNCPDECP =  AUTOBNCP; 
ABNCPNETV =  AUTOBNCV - TBNCPABV; 
ABNCPNETC =  AUTOBNCC - TBNCPABC; 
ABNCPNETP =  AUTOBNCP - TBNCPABP; 
AUTOBNCPNET = ABNCPNETV + ABNCPNETC + ABNCPNETP;
regle 99991012:
application : iliad   ;                          
TSPENETPV = max (0,(BNCPROV  + AUTOBNCV - TSPEABPV));
TSPENETPC = max (0,(BNCPROC  + AUTOBNCC - TSPEABPC));
TSPENETPP = max (0,(BNCPROP  + AUTOBNCP - TSPEABPP));
TSPENETNPV = max (0,(BNCNPV - TSPEABNPV));
TSPENETNPC = max (0,(BNCNPC - TSPEABNPC));
TSPENETNPP = max (0,(BNCNPP - TSPEABNPP));
TSPENETNPF = somme(i=V,C,P:(TSPENETNPi)) + SPENETNPCT ;
TSPENETV = TSPENETPV + TSPENETNPV;
TSPENETC = TSPENETPC + TSPENETNPC;
TSPENETP = TSPENETPP + TSPENETNPP;
TSPENET = somme(i=V,C,P:(TSPENETi));

regle 99991020:
application : iliad   ;                          

TXSPEAAV = (BNCREV + COD5XJ + CODCQC - BNCDEV);

TDEF5QC = positif_ou_nul(BNCREV + COD5XJ + CODCQC - BNCDEV) * (positif(COD5XJ+CODCQC) * arr(BNCDEV*BNCREV/(BNCREV + COD5XJ+CODCQC))
                                                             + (1-positif(COD5XJ+CODCQC)) * BNCDEV);
TDEF5XJ = positif_ou_nul(BNCREV + COD5XJ + CODCQC- BNCDEV) * (positif(CODCQC) * arr(BNCDEV*COD5XJ/(BNCREV + COD5XJ+CODCQC))
                                                             + (1-positif(CODCQC)) * (BNCDEV-TDEF5QC));
TDEF5TF = positif_ou_nul(BNCREV + COD5XJ + CODCQC - BNCDEV) * (positif(CODCQC) * arr(BNCDEV/(BNCREV + COD5XJ+CODCQC))
                                                             + (1-positif(CODCQC)) * (BNCDEV-TDEF5QC-TDEF5XJ));
TDEFCQC = positif_ou_nul(BNCREV + COD5XJ + CODCQC- BNCDEV) * max(0,BNCDEV-TDEF5QC-TDEF5XJ-TDEF5TF);
TR15QC = max(0,BNCREV - TDEF5QC);
TR15TF = max(0, TDEF5TF);
TR15XJ = max(0,COD5XJ - TDEF5XJ);
TBNNAMNV = positif_ou_nul(BNCREV + COD5XJ + CODCQC - BNCDEV) * arr(TR15QC+TR15XJ+TR15TF) + (1-positif_ou_nul(BNCREV + COD5XJ + CODCQC - BNCDEV)) * TXSPEAAV;
TR1CQC = max(0,CODCQC - TDEFCQC);


TXSPEAAC = (BNCREC + COD5YJ + CODCRC - BNCDEC);

TDEF5RC = positif_ou_nul(BNCREC + COD5YJ + CODCRC - BNCDEC) * (positif(COD5YJ+CODCRC) * arr(BNCDEC*BNCREC/(BNCREC + COD5YJ+CODCRC))
                                                             + (1-positif(COD5YJ+CODCRC)) * BNCDEC);
TDEF5YJ = positif_ou_nul(BNCREC + COD5YJ + CODCRC - BNCDEC) * (positif(CODCRC) * arr(BNCDEC*COD5YJ/(BNCREC + COD5YJ+CODCRC))
                                                             + (1-positif(CODCRC)) * (BNCDEC-TDEF5RC));
TDEF5UF = positif_ou_nul(BNCREC + COD5YJ + CODCRC - BNCDEC) * (positif(CODCRC) * arr(BNCDEC/(BNCREC + COD5YJ+CODCRC))
                                                             + (1-positif(CODCRC)) * (BNCDEC-TDEF5RC-TDEF5YJ));
TDEFCRC = positif_ou_nul(BNCREC + COD5YJ + CODCRC - BNCDEC) * max(0,BNCDEC-TDEF5RC-TDEF5YJ-TDEF5UF);
TR15RC = max(0,BNCREC - TDEF5RC);
TR15UF = max(0, TDEF5UF);
TR15YJ = max(0,COD5YJ - TDEF5YJ);
TBNNAMNC = positif_ou_nul(BNCREC + COD5YJ + CODCRC - BNCDEC) * arr(TR15RC+TR15YJ+TR15UF) + (1-positif_ou_nul(BNCREC + COD5YJ + CODCRC- BNCDEC)) * TXSPEAAC;
TR1CRC = max(0,CODCRC - TDEFCRC);


TXSPEAAP = (BNCREP + COD5ZJ + CODCSC - BNCDEP);
TDEF5SC = positif_ou_nul(BNCREP + COD5ZJ + CODCSC- BNCDEP) * (positif(COD5ZJ+CODCSC) * arr(BNCDEP*BNCREP/(BNCREP + COD5ZJ+CODCSC))
                                                             + (1-positif(COD5ZJ+CODCRC)) * BNCDEP);
TDEF5ZJ = positif_ou_nul(BNCREP + COD5ZJ + CODCSC- BNCDEP) * (positif(CODCSC) * arr(BNCDEP*COD5ZJ/(BNCREP + COD5ZJ+CODCSC))
                                                             + (1-positif(CODCSC)) * (BNCDEP-TDEF5SC));
TDEF5VF = positif_ou_nul(BNCREP + COD5ZJ + CODCSC- BNCDEP) * (positif(CODCSC) * arr(BNCDEP/(BNCREP + COD5ZJ+CODCSC))
                                                             + (1-positif(CODCSC)) * (BNCDEP-TDEF5SC-TDEF5ZJ));
TDEFCSC = positif_ou_nul(BNCREP + COD5ZJ + CODCSC - BNCDEP) * max(0,BNCDEP-TDEF5SC-TDEF5ZJ-TDEF5VF);
TR15SC = max(0,BNCREP - TDEF5SC);
TR15VF = max(0, TDEF5VF);
TR15ZJ = max(0,COD5ZJ - TDEF5ZJ);
TBNNAMNP = positif_ou_nul(BNCREP + COD5ZJ + CODCSC- BNCDEP) * (TR15SC+TR15ZJ+TR15VF) + (1-positif_ou_nul(BNCREP + COD5ZJ + CODCSC - BNCDEP)) * TXSPEAAP;
TR1CSC = max(0,CODCSC - TDEFCSC);



TXSPEHV = BNHREV + COD5XK + CODCQI - BNHDEV;
TDEF5QI = positif_ou_nul(BNHREV + COD5XK + CODCQI- BNHDEV) * (positif(COD5XK+CODCQI) * arr(BNHDEV*BNHREV/(BNHREV + COD5XK+CODCQI))
                                                             + (1-positif(COD5XK+CODCQI)) * BNHDEV);
TDEF5XK = positif_ou_nul(BNHREV + COD5XK + CODCQI- BNHDEV) * (positif(CODCQI) * arr(BNHDEV*COD5XK/(BNHREV + COD5XK+CODCQI))
                                                             + (1-positif(CODCQI)) * (BNHDEV-TDEF5QI));
TDEF5TI = positif_ou_nul(BNHREV + COD5XK + CODCQI- BNHDEV) * (positif(CODCQI) * arr(BNHDEV/(BNHREV + COD5XK+CODCQI))
                                                             + (1-positif(CODCQI)) * (BNHDEV-TDEF5QI-TDEF5XK));
TDEFCQI = positif_ou_nul(BNHREV + COD5XK + CODCQI- BNHDEV) * max(0,BNHDEV-TDEF5QI-TDEF5XK-TDEF5TI);
TBNNS5QI = max(0,BNHREV - TDEF5QI);
TR2MAJ5QI = TBNNS5QI * MAJREV20;
TBNNS5TI = max(0, TDEF5TI);
TBNNS5XK = max(0,COD5XK - TDEF5XK);
TR2MAJCQI = arr(max(0,CODCQI - TDEFCQI)* MAJREV20);
TBNNSMNV = positif_ou_nul(BNHREV + COD5XK + CODCQI- BNHDEV) * arr((TBNNS5QI+TBNNS5XK+TBNNS5TI) * MAJREV20) + (1-positif_ou_nul(BNHREV + COD5XK + CODCQI- BNHDEV)) * TXSPEHV;


TXSPEHC = BNHREC + COD5YK + CODCRI - BNHDEC;
TDEF5RI = positif_ou_nul(BNHREC + COD5YK + CODCRI - BNHDEC) * (positif(COD5YK+CODCRI) * arr(BNHDEC*BNHREC/(BNHREC + COD5YK+CODCRI))
                                                             + (1-positif(COD5YK+CODCRI)) * BNHDEC);
TDEF5YK = positif_ou_nul(BNHREC + COD5YK + CODCRI - BNHDEC) * (positif(CODCRI) * arr(BNHDEC*COD5YK/(BNHREC + COD5YK+CODCRI))
                                                             + (1-positif(CODCRI)) * (BNHDEC-TDEF5RI));
TDEF5UI = positif_ou_nul(BNHREC + COD5YK + CODCRI - BNHDEC) * (positif(CODCRI) * arr(BNHDEC/(BNHREC + COD5YK+CODCRI))
                                                             + (1-positif(CODCRI)) * (BNHDEC-TDEF5RI-TDEF5YK));
TDEFCRI = positif_ou_nul(BNHREC + COD5YK + CODCRI - BNHDEC) * max(0,BNHDEC-TDEF5RI-TDEF5YK-TDEF5UI);
TBNNS5RI = max(0,BNHREC - TDEF5RI);
TR2MAJ5RI = TBNNS5RI * MAJREV20;
TBNNS5YK = max(0,COD5YK - TDEF5YK);
TBNNS5UI = max(0, TDEF5UI);
TR2MAJCRI = arr(max(0,CODCRI - TDEFCRI)* MAJREV20);
TBNNSMNC = positif_ou_nul(BNHREC + COD5YK + CODCRI - BNHDEC) * arr((TBNNS5RI+TBNNS5YK+TBNNS5UI) * MAJREV20) + (1-positif_ou_nul(BNHREC + COD5YK + CODCRI - BNHDEC)) * TXSPEHC;


TXSPEHP = BNHREP + COD5ZK + CODCSI - BNHDEP;
TDEF5SI = positif_ou_nul(BNHREP + COD5ZK + CODCSI - BNHDEP) * (positif(COD5ZK+CODCSI) * arr(BNHDEP*BNHREP/(BNHREP + COD5ZK+CODCSI))
                                                             + (1-positif(COD5ZK+CODCSI)) * BNHDEP);
TDEF5ZK = positif_ou_nul(BNHREP + COD5ZK + CODCSI- BNHDEP) * (positif(CODCSI) * arr(BNHDEP*COD5ZK/(BNHREP + COD5ZK+CODCSI))
                                                             + (1-positif(CODCSI)) * (BNHDEP-TDEF5SI));
TDEF5VI = positif_ou_nul(BNHREP + COD5ZK + CODCSI- BNHDEP) * (positif(CODCSI) * arr(BNHDEP/(BNHREP + COD5ZK+CODCSI))
                                                             + (1-positif(CODCSI)) * (BNHDEP-TDEF5SI-TDEF5ZK));
TDEFCSI = positif_ou_nul(BNHREP + COD5ZK + CODCSI - BNHDEP) * max(0,BNHDEP-TDEF5SI-TDEF5ZK-TDEF5VI);
TBNNS5SI = max(0,BNHREP - TDEF5SI);
TR2MAJ5SI = TBNNS5SI * MAJREV20;
TBNNS5ZK = max(0,COD5ZK - TDEF5ZK);
TBNNS5VI = max(0, TDEF5VI);
TR2MAJCSI = arr(max(0,CODCSI - TDEFCSI)* MAJREV20);
TBNNSMNP = positif_ou_nul(BNHREP + COD5ZK + CODCSI- BNHDEP) * arr((TBNNS5SI+TBNNS5ZK+TBNNS5VI) * MAJREV20) + (1-positif_ou_nul(BNHREP + COD5ZK + CODCSI- BNHDEP)) * TXSPEHP;

regle 99991022:
application : iliad   ;                          

TBNCPHQNCV = TBNNAMNV + TBNNSMNV + TSPENETPV + SPENETCTV;
TBNCPHQNCC = TBNNAMNC + TBNNSMNC + TSPENETPC + SPENETCTC;
TBNCPHQNCP = TBNNAMNP + TBNNSMNP + TSPENETPP + SPENETCTP;
TBNCPHQNCF = TBNCPHQNCV + TBNCPHQNCC + TBNCPHQNCP;

regle 99991023:
application : iliad   ;                          
TBNCPQNCV = max(0,TR1CQC + TR2MAJCQI);
TBNCPQNCC = max(0,TR1CRC + TR2MAJCRI);
TBNCPQNCP = max(0,TR1CSC + TR2MAJCSI);
TBNCPQUOTF = TBNCPQNCV+TBNCPQNCC+TBNCPQNCP;

TBNCPHQCV = ((1-positif(TBNCPHQNCV))* positif_ou_nul(TBNCPQNCV)) * min(0,TBNCPHQNCV+TBNCPQNCV) + positif_ou_nul(TBNCPHQNCV)* TBNCPHQNCV;
TBNCPHQCC = ((1-positif(TBNCPHQNCC))* positif_ou_nul(TBNCPQNCC)) * min(0,TBNCPHQNCC+TBNCPQNCC) + positif_ou_nul(TBNCPHQNCC)* TBNCPHQNCC;
TBNCPHQCP = ((1-positif(TBNCPHQNCP))* positif_ou_nul(TBNCPQNCP)) * min(0,TBNCPHQNCP+TBNCPQNCP) + positif_ou_nul(TBNCPHQNCP)* TBNCPHQNCP;
TBNCPQCV =   positif(TBNCPHQNCV) * (TR1CQC+TR2MAJCQI)
           + (1-positif(TBNCPHQNCV)) * max(0,TR1CQC+TR2MAJCQI+TBNCPHQNCV);
TBNCPQCC =   positif(TBNCPHQNCC) * (TR1CRC+TR2MAJCRI)
	              + (1-positif(TBNCPHQNCC)) * max(0,TR1CRC+TR2MAJCRI+TBNCPHQNCC);
TBNCPQCP =   positif(TBNCPHQNCP) * (TR1CSC+TR2MAJCSI)
		                 + (1-positif(TBNCPHQNCP)) * max(0,TR1CSC+TR2MAJCSI+TBNCPHQNCP);
				 # Total foyer pro ordinaire et quotient
TBNCPQCF = TBNCPQCV +  TBNCPQCC + TBNCPQCP;
TBNCPHQCF = TBNCPHQCV+TBNCPHQCC+TBNCPHQCP;
TBNCPHQF = (1-positif(TBNCPHQCF)) * positif_ou_nul(TBNCPQCF) * min(0,TBNCPHQCF+TBNCPQCF)
          + positif(TBNCPHQCF) * TBNCPHQCF;
TBNCPQF = (1-positif(TBNCPHQCF)) * positif_ou_nul(TBNCPQCF) * max(0,TBNCPHQCF+TBNCPQCF)
	            + positif(TBNCPHQCF) * TBNCPQCF;
TBNCNPHQNCV = BNNAANV+ NOCEPIMPNV + TSPENETNPV + BNCNPPVV - BNCNPDCT;
TBNCNPHQNCC = BNNAANC+ NOCEPIMPNC + TSPENETNPC + BNCNPPVC - COD5LD;
TBNCNPHQNCP = BNNAANP+ NOCEPIMPNP + TSPENETNPP + BNCNPPVP - COD5MD;
TBNCNPHQCV = (1-positif(TBNCNPHQNCV)) * positif_ou_nul(BNCNPQNCV) * min(0,TBNCNPHQNCV+BNCNPQNCV)+ positif_ou_nul(TBNCNPHQNCV)*TBNCNPHQNCV;
TBNCNPHQCC = (1-positif(TBNCNPHQNCC)) * positif_ou_nul(BNCNPQNCC) * min(0,TBNCNPHQNCC+BNCNPQNCC)+ positif_ou_nul(TBNCNPHQNCC)*TBNCNPHQNCC;
TBNCNPHQCP = (1-positif(TBNCNPHQNCP)) * positif_ou_nul(BNCNPQNCP) * min(0,TBNCNPHQNCP+BNCNPQNCP)+ positif_ou_nul(TBNCNPHQNCP)*TBNCNPHQNCP;
TBNNNV = max(0,TBNCNPHQCV);
TBNNNC = max(0,TBNCNPHQCC);
TBNNNP = max(0,TBNCNPHQCP);
TBNCNPHQCF = TBNCNPHQCV+TBNCNPHQCC+TBNCNPHQCP;
TBNCNPQCV =   positif(TBNCNPHQNCV) * (R1CJG+R2MAJCSN)
           + (1-positif(TBNCNPHQNCV)) * max(0,R1CJG+R2MAJCSN+TBNCNPHQNCV);
TBNCNPQCC =   positif(TBNCNPHQNCC) * (R1CRF+R2MAJCNS)
           + (1-positif(TBNCNPHQNCC)) * max(0,R1CRF+R2MAJCNS+TBNCNPHQNCC);
TBNCNPQCP =   positif(TBNCNPHQNCP) * (R1CSF+R2MAJCOS)
           + (1-positif(TBNCNPHQNCP)) * max(0,R1CSF+R2MAJCOS+TBNCNPHQNCP);
TBNCNPQCF = TBNCNPQCV + TBNCNPQCC + TBNCNPQCP;
regle 999910251:
application : iliad   ;                          
TBNCNPHQF = (1-positif(TBNCNPHQCF)) * positif_ou_nul(TBNCNPQCF) * min(0,max(0,TBNCNPHQCF-TDIDABNCNPHQ)+TBNCNPQCF)
          + positif(TBNCNPHQCF) * max(0,TBNCNPHQCF-TDIDABNCNPHQ);
TBNCNPQF = (1-positif(TBNCNPHQCF)) * positif_ou_nul(TBNCNPQCF) * max(0,TBNCNPHQCF+TBNCNPQCF-TDIDABNCNPQ)
	            + positif(TBNCNPHQCF) * max(0,TBNCNPQCF-TDIDABNCNPQ);
regle 99991025:
application : iliad   ;                          

TREV4 = BAQTOTAVIS +GLN4V + GLN4C+TREV4_1731+TBNCPQF+TBNCNPQF+
       BICQV+BICQC+BICQP+BIPTAQV+BIPTAQC+BIPTAQP+BIHTAQV+BIHTAQC+BIHTAQP;
regle 99992000:
application : iliad   ;
COD1GHRET = max(0 , COD1GH - LIM7500) * positif_ou_nul(COD1GH) ;
COD1HHRET = max(0 , COD1HH - LIM7500) * positif_ou_nul(COD1HH) ;
COD1IHRET = max(0 , COD1IH - LIM7500) * positif_ou_nul(COD1IH) ;
COD1JHRET = max(0 , COD1JH - LIM7500) * positif_ou_nul(COD1JH) ;
COD1KHRET = max(0 , COD1KH - LIM7500) * positif_ou_nul(COD1KH) ;
COD1LHRET = max(0 , COD1LH - LIM7500) * positif_ou_nul(COD1LH) ;
regle 99992010:
application : iliad   ;
COD1ADRET = max(0 , COD1AD - LIM3000 * (1+positif(COD1AV))) * positif_ou_nul(COD1AD) ;
COD1BDRET = max(0 , COD1BD - LIM3000 * (1+positif(COD1BV))) * positif_ou_nul(COD1BD) ;
COD1CDRET = max(0 , COD1CD - LIM3000 * (1+positif(COD1CV))) * positif_ou_nul(COD1CD) ;
COD1DDRET = max(0 , COD1DD - LIM3000 * (1+positif(COD1DV))) * positif_ou_nul(COD1DD) ;
COD1EDRET = max(0 , COD1ED - LIM3000 * (1+positif(COD1EV))) * positif_ou_nul(COD1ED) ;
COD1FDRET = max(0 , COD1FD - LIM3000 * (1+positif(COD1FV))) * positif_ou_nul(COD1FD) ;
regle 99992050:
application : iliad   ;
TTSBNV = TSHALLOV + ALLOV + SALEXTV + COD1PM + COD1TP + COD1NX + COD1AF + COD1AG + COD1GB + COD1AA + COD1GF + COD1GG + COD1GHRET + COD1ADRET;
TTSBNC = TSHALLOC + ALLOC + SALEXTC + COD1QM + COD1UP + COD1OX + COD1BF + COD1BG + COD1HB + COD1BA + COD1HF + COD1HG + COD1HHRET + COD1BDRET;
TTSBN1 = TSHALLO1 + ALLO1 + SALEXT1 + COD1CF + COD1CG + COD1IB + COD1CA + COD1IF + COD1IG + COD1IHRET + COD1CDRET;
TTSBN2 = TSHALLO2 + ALLO2 + SALEXT2 + COD1DF + COD1DG + COD1JB + COD1DA + COD1JF + COD1JG + COD1JHRET + COD1DDRET;
TTSBN3 = TSHALLO3 + ALLO3 + SALEXT3 + COD1EF + COD1EG + COD1EA + COD1KF + COD1KG + COD1KHRET + COD1EDRET;
TTSBN4 = TSHALLO4 + ALLO4 + SALEXT4 + COD1FF + COD1FG + COD1FA + COD1LF + COD1LG + COD1LHRET + COD1FDRET;

T2TSNV = CARTSV + REMPLAV+CODRAF+CODRAG;
T2TSNC = CARTSC + REMPLAC+CODRBF+CODRBG;
T2TSN1 = CARTSP1 + REMPLAP1+CODRCF+CODRCG;
T2TSN2 = CARTSP2 + REMPLAP2+CODRDF+CODRDG;
T2TSN3 = CARTSP3 + REMPLAP3+CODREF+CODRGG;
T2TSN4 = CARTSP4 + REMPLAP4+CODRFF+CODRFG;
TEXTSV = TTSBNV + BPCOSAV + GLDGRATV + T2TSNV;
TEXTSC = TTSBNC + BPCOSAC + GLDGRATC + T2TSNC;
TGATASAV = BPCOSAV + GLDGRATV ;
TGATASAC = BPCOSAC + GLDGRATC ;

TEXTS1 = TTSBN1 + T2TSN1;
TEXTS2 = TTSBN2 + T2TSN2;
TEXTS3 = TTSBN3 + T2TSN3;
TEXTS4 = TTSBN4 + T2TSN4;
TTSBV = TEXTSV + CODDAJ + CODEAJ ;
TTSBC = TEXTSC + CODDBJ + CODEBJ ;
TTSB1 = TEXTS1;
TTSB2 = TEXTS2;
TTSB3 = TEXTS3;
TTSB4 = TEXTS4;
TTSBP = somme(i=1..4:TTSBi);
TPRBV = PRBRV + PALIV + PENINV+COD1AL+COD1AM;
TPRBC = PRBRC + PALIC + PENINC+COD1BL+COD1BM;
TPRB1 = PRBR1 + PALI1 + PENIN1+COD1CL+COD1CM;
TPRB2 = PRBR2 + PALI2 + PENIN2+COD1DL+COD1DM;
TPRB3 = PRBR3 + PALI3 + PENIN3+COD1EL+COD1EM;
TPRB4 = PRBR4 + PALI4 + PENIN4+COD1FL+COD1FM;

T2PRBV = CARPEV + PENSALV + CODRAZ+CODRAL+CODRAM;
T2PRBC = CARPEC + PENSALC + CODRBZ+CODRBL+CODRBM;
T2PRB1 = CARPEP1 + PENSALP1 + CODRCZ+CODRCL+CODRCM;
T2PRB2 = CARPEP2 + PENSALP2 + CODRDZ+CODRDL+CODRDM;
T2PRB3 = CARPEP3 + PENSALP3 + CODREZ+CODREL+CODREM;
T2PRB4 = CARPEP4 + PENSALP4 + CODRFZ+CODRFL+CODRFM;
TEXPRV = TPRBV + COD1AH + T2PRBV + PEBFV;
TEXPRC = TPRBC + COD1BH + T2PRBC + PEBFC;
TEXPR1 = TPRB1 + COD1CH + T2PRB1 + PEBF1;
TEXPR2 = TPRB2 + COD1DH + T2PRB2 + PEBF2;
TEXPR3 = TPRB3 + COD1EH + T2PRB3 + PEBF3;
TEXPR4 = TPRB4 + COD1FH + T2PRB4 + PEBF4;
TEXSPBV = TEXTSV + TEXPRV ;
TEXSPBC = TEXTSC + TEXPRC ;
TEXSPB1 = TEXTS1 + TEXPR1 ;
TEXSPB2 = TEXTS2 + TEXPR2 ;
TEXSPB3 = TEXTS3 + TEXPR3 ;
TEXSPB4 = TEXTS4 + TEXPR4 ;
regle 99992100:
application : iliad   ;
TTPS10V = arr (TTSBV * TX_DEDFORFTS /100);
TTPS10C = arr (TTSBC * TX_DEDFORFTS /100);
TTPS101 = arr (TTSB1 * TX_DEDFORFTS /100);
TTPS102 = arr (TTSB2 * TX_DEDFORFTS /100);
TTPS103 = arr (TTSB3 * TX_DEDFORFTS /100);
TTPS104 = arr (TTSB4 * TX_DEDFORFTS /100);
TDFNV =  min( PLAF_DEDFORFTS , TTPS10V );
TDFNC =  min( PLAF_DEDFORFTS , TTPS10C );
TDFN1 =  min( PLAF_DEDFORFTS , TTPS101 );
TDFN2 =  min( PLAF_DEDFORFTS , TTPS102 );
TDFN3 =  min( PLAF_DEDFORFTS , TTPS103 );
TDFN4 =  min( PLAF_DEDFORFTS , TTPS104 );
regle 99992200:
application : iliad   ;
TDEDMINV = MIN_DEDSFORFTS;
TDEDMINC = MIN_DEDSFORFTS;
TDEDMIN1 = MIN_DEDSFORFTS;
TDEDMIN2 = MIN_DEDSFORFTS;
TDEDMIN3 = MIN_DEDSFORFTS;
TDEDMIN4 = MIN_DEDSFORFTS;
T10MINSV = max( min(TTSBV,TDEDMINV) , TDFNV );
T10MINSC = max( min(TTSBC,TDEDMINC) , TDFNC );
T10MINS1 = max( min(TTSB1,TDEDMIN1) , TDFN1 );
T10MINS2 = max( min(TTSB2,TDEDMIN2) , TDFN2 );
T10MINS3 = max( min(TTSB3,TDEDMIN3) , TDFN3 );
T10MINS4 = max( min(TTSB4,TDEDMIN4) , TDFN4 );
TIND_10MIN_0V = positif(TDEDMINV - TDFNV ) * positif (TTSBV );
TIND_10MIN_0C = positif(TDEDMINC - TDFNC ) * positif (TTSBC );
TIND_10MIN_01 = positif(TDEDMIN1 - TDFN1 ) * positif (TTSB1 );
TIND_10MIN_02 = positif(TDEDMIN2 - TDFN2 ) * positif (TTSB2 );
TIND_10MIN_03 = positif(TDEDMIN3 - TDFN3 ) * positif (TTSB3 );
TIND_10MIN_04 = positif(TDEDMIN4 - TDFN4 ) * positif (TTSB4 );
TIND_MINV = 1 - positif( TIND_10MIN_0V );
TIND_MINC = 1 - positif( TIND_10MIN_0C );
TIND_MIN1 = 1 - positif( TIND_10MIN_01 );
TIND_MIN2 = 1 - positif( TIND_10MIN_02 );
TIND_MIN3 = 1 - positif( TIND_10MIN_03 );
TIND_MIN4 = 1 - positif( TIND_10MIN_04 );
regle 99992300:
application : iliad   ;
T10MINSP = T10MINS1 + T10MINS2 + T10MINS3 + T10MINS4;
TFRDPROVV = TTSBNV + TPRV + PALIV - TAPRV;
TFRDPROVC = TTSBNC + TPRC + PALIC - TAPRC;
TFRDPROV1 = TTSBN1 + PRBR1 + PALI1 - TAPR1;
TFRDPROV2 = TTSBN2 + PRBR2 + PALI2 - TAPR2;
TFRDPROV3 = TTSBN3 + PRBR3 + PALI3 - TAPR3;
TFRDPROV4 = TTSBN4 + PRBR4 + PALI4 - TAPR4;
TFRDPROVP = TFRDPROV1 +TFRDPROV2 +TFRDPROV3 +TFRDPROV4;
TFRDP = (FRNP+COD1CE+COD1DE+COD1EE+COD1FE) * positif(FRNP+COD1CE+COD1DE+COD1EE+COD1FE - T10MINSP);

TFRDV = (FRNV+COD1AE) * positif(FRNV+COD1AE - T10MINSV);

TFRDC = (FRNC+COD1BE) * positif(FRNC+COD1BE - T10MINSC);

TFRD1 = (FRN1+COD1CE) * positif(FRN1+COD1CE - T10MINS1);

TFRD2 = (FRN2+COD1DE) * positif(FRN2+COD1DE - T10MINS2);

TFRD3 = (FRN3+COD1EE) * positif(FRN3+COD1EE - T10MINS3);

TFRD4 = (FRN4+COD1FE) * positif(FRN4+COD1FE - T10MINS4);

TIND_10V = positif_ou_nul( T10MINSV - (TFRDV+COD1AE) ) ;
TIND_10C = positif_ou_nul( T10MINSC - (TFRDC+COD1BE) ) ;
TIND_101 = positif_ou_nul( T10MINS1 - (TFRD1+COD1CE) ) ;
TIND_102 = positif_ou_nul( T10MINS2 - (TFRD2+COD1DE) ) ;
TIND_103 = positif_ou_nul( T10MINS3 - (TFRD3+COD1EE) ) ;
TIND_104 = positif_ou_nul( T10MINS4 - (TFRD4+COD1FE) ) ;
TFPTV = max(TFRDV, T10MINSV);
TFPTC = max(TFRDC, T10MINSC);
TFPT1 = max(TFRD1, T10MINS1);
TFPT2 = max(TFRD2, T10MINS2);
TFPT3 = max(TFRD3, T10MINS3);
TFPT4 = max(TFRD4, T10MINS4);
TD10MV = TIND_MINV *TDFNV + (1 - TIND_MINV)* T10MINSV ; 
TD10MC = TIND_MINC *TDFNC + (1 - TIND_MINC)* T10MINSC ; 
TD10M1 = TIND_MIN1 *TDFN1 + (1 - TIND_MIN1)* T10MINS1 ; 
TD10M2 = TIND_MIN2 *TDFN2 + (1 - TIND_MIN2)* T10MINS2 ; 
TD10M3 = TIND_MIN3 *TDFN3 + (1 - TIND_MIN3)* T10MINS3 ; 
TD10M4 = TIND_MIN4 *TDFN4 + (1 - TIND_MIN4)* T10MINS4 ; 
TREP10V =  TIND_10V * TD10MV + (1-TIND_10V) * TFPTV ;
TREP10C =  TIND_10C * TD10MC + (1-TIND_10C) * TFPTC ;
TREP101 =  TIND_101 * TD10M1 + (1-TIND_101) * TFPT1 ;
TREP102 =  TIND_102 * TD10M2 + (1-TIND_102) * TFPT2 ;
TREP103 =  TIND_103 * TD10M3 + (1-TIND_103) * TFPT3 ;
TREP104 =  TIND_104 * TD10M4 + (1-TIND_104) * TFPT4 ;
VARTMP2=0;
VARTMP1=0;
VARTMP2=TSHALLOV+COD1PM+COD1TP+COD1NX+COD1AF+COD1AG+SALEXTV+ALLOV+BPCOSAV+GLDGRATV+COD1GB+COD1AA+COD1GF+COD1GG+COD1GHRET+COD1ADRET+CODRAF+CODRAG+CARTSV+REMPLAV+CODDAJ+CODEAJ+COD1ADRET;
TABTS1AJ=arr(TREP10V*(TSHALLOV)/TTSBV);
VARTMP2=VARTMP2 - TSHALLOV-COD1PM;
VARTMP1=TABTS1AJ;
TABTS1PM= positif(VARTMP2) * arr(TREP10V*(COD1PM)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1TP;
VARTMP1=VARTMP1+TABTS1PM;
TABTS1TP= positif(VARTMP2) * arr(TREP10V*(COD1TP)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1NX;
VARTMP1=VARTMP1+TABTS1TP;
TABTS1NX= positif(VARTMP2) * arr(TREP10V*(COD1NX)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1AF;
VARTMP1=VARTMP1+TABTS1NX;
TABTS1AF= positif(VARTMP2) * arr(TREP10V*(COD1AF)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1AG;
VARTMP1=VARTMP1+TABTS1AF;
TABTS1AG= positif(VARTMP2) * arr(TREP10V*(COD1AG)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - SALEXTV;
VARTMP1=VARTMP1+TABTS1AG;
TABTS1AC= positif(VARTMP2) * arr(TREP10V*(SALEXTV)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - ALLOV;
VARTMP1=VARTMP1+TABTS1AC;
TABTS1AP= positif(VARTMP2) * arr(TREP10V*(ALLOV)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - BPCOSAV;
VARTMP1=VARTMP1+TABTS1AP;
TABTS3VJ= positif(VARTMP2) * arr(TREP10V*(BPCOSAV)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - GLDGRATV;
VARTMP1=VARTMP1+TABTS3VJ;
TABTS1TT= positif(VARTMP2) *  arr(TREP10V*(GLDGRATV)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1GB;
VARTMP1=VARTMP1+TABTS1TT;
TABTS1GB= positif(VARTMP2) *  arr(TREP10V*(COD1GB)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1AA;
VARTMP1=VARTMP1+TABTS1GB;
TABTS1AA= positif(VARTMP2) *  arr(TREP10V*(COD1AA)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1GF;
VARTMP1=VARTMP1+TABTS1AA;
TABTS1GF= positif(VARTMP2) *  arr(TREP10V*(COD1GF)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1GG;
VARTMP1=VARTMP1+TABTS1GF;
TABTS1GG= positif(VARTMP2) *  arr(TREP10V*(COD1GG)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1GHRET;
VARTMP1=VARTMP1+TABTS1GG;
TABTS1GH= positif(VARTMP2) *  arr(TREP10V*(COD1GHRET)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                           ;
VARTMP2=VARTMP2 - COD1ADRET;
VARTMP1=VARTMP1+TABTS1GH;
TABTS1AD= positif(VARTMP2) *  arr(TREP10V*(COD1ADRET)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
		             ;
VARTMP2=VARTMP2 - CARTSV;
VARTMP1=VARTMP1+TABTS1AD;
TABTSRAJ= positif(VARTMP2) *  arr(TREP10V*(CARTSV)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - REMPLAV;
VARTMP1=VARTMP1+TABTSRAJ;
TABTSRAP= positif(VARTMP2) *  arr(TREP10V*(REMPLAV)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - CODRAF;
VARTMP1=VARTMP1+TABTSRAP;
TABTSRAF= positif(VARTMP2) *  arr(TREP10V*(CODRAF)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
VARTMP2=VARTMP2 - CODRAG;
VARTMP1=VARTMP1+TABTSRAF;
TABTSRAG= positif(VARTMP2) *  arr(TREP10V*(CODRAG)/TTSBV)
                 + (1-positif(VARTMP2)) * max(0,TREP10V-VARTMP1)
                          ;
TABTSV = VARTMP1+TABTSRAG;
TABDOMDAJ = positif(CODEAJ) * arr(TREP10V*CODDAJ/TTSBV)
	   + (1-positif(CODEAJ)) * max(0,TREP10V-TABTSV)+0;
TABDOMEAJ = max(0,TREP10V-TABTSV-TABDOMDAJ)+0;
VARTMP2=0;
VARTMP1=0;
VARTMP2=TSHALLOC+COD1QM+COD1UP+COD1OX+COD1BF+COD1BG+SALEXTC+ALLOC+BPCOSAC+GLDGRATC+CARTSC+REMPLAC+CODDBJ+CODEBJ+COD1HB+COD1BA+COD1HF+COD1HG+COD1HHRET+COD1BDRET+CODRBF+CODRBG+COD1BDRET;
TABTS1BJ=arr(TREP10C*(TSHALLOC)/TTSBC);
VARTMP2=VARTMP2 - TSHALLOC-COD1QM;
VARTMP1=TABTS1BJ;
TABTS1QM= positif(VARTMP2) * arr(TREP10C*(COD1QM)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1UP;
VARTMP1=VARTMP1+TABTS1QM;
TABTS1UP= positif(VARTMP2) * arr(TREP10C*(COD1UP)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1OX;
VARTMP1=VARTMP1+TABTS1UP;
TABTS1OX= positif(VARTMP2) * arr(TREP10C*(COD1OX)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1BF;
VARTMP1=VARTMP1+TABTS1OX;
TABTS1BF= positif(VARTMP2) * arr(TREP10C*(COD1BF)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1BG;
VARTMP1=VARTMP1+TABTS1BF;
TABTS1BG= positif(VARTMP2) * arr(TREP10C*(COD1BG)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - SALEXTC;
VARTMP1=VARTMP1+TABTS1BG;
TABTS1BC= positif(VARTMP2) * arr(TREP10C*(SALEXTC)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - ALLOC;
VARTMP1=VARTMP1+TABTS1BC;
TABTS1BP= positif(VARTMP2) * arr(TREP10C*(ALLOC)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - BPCOSAC;
VARTMP1=VARTMP1+TABTS1BP;
TABTS3VK= positif(VARTMP2) * arr(TREP10C*(BPCOSAC)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - GLDGRATC;
VARTMP1=VARTMP1+TABTS3VK;
TABTS1UT= positif(VARTMP2) * arr(TREP10C*(GLDGRATC)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1HB;
VARTMP1=VARTMP1+TABTS1UT;
TABTS1HB= positif(VARTMP2) * arr(TREP10C*(COD1HB)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1BA;
VARTMP1=VARTMP1+TABTS1HB;
TABTS1BA= positif(VARTMP2) * arr(TREP10C*(COD1BA)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1HF;
VARTMP1=VARTMP1+TABTS1BA;
TABTS1HF= positif(VARTMP2) * arr(TREP10C*(COD1HF)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1HG;
VARTMP1=VARTMP1+TABTS1HF;
TABTS1HG= positif(VARTMP2) * arr(TREP10C*(COD1HG)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - COD1HHRET;
VARTMP1=VARTMP1+TABTS1HG;
TABTS1HH= positif(VARTMP2) *  arr(TREP10C*(COD1HHRET)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                           ;
VARTMP2=VARTMP2 - COD1BDRET;
VARTMP1=VARTMP1+TABTS1HH;
TABTS1BD= positif(VARTMP2) *  arr(TREP10C*(COD1BDRET)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
		        ;
VARTMP2=VARTMP2 - CARTSC;
VARTMP1=VARTMP1+TABTS1BD;
TABTSRBJ= positif(VARTMP2) * arr(TREP10C*(CARTSC)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - REMPLAC;
VARTMP1=VARTMP1+TABTSRBJ;
TABTSRBP= positif(VARTMP2) * arr(TREP10C*(REMPLAC)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - CODRBF;
VARTMP1=VARTMP1+TABTSRBP;
TABTSRBF= positif(VARTMP2) * arr(TREP10C*(CODRBF)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
VARTMP2=VARTMP2 - CODRBG;
VARTMP1=VARTMP1+TABTSRBF;
TABTSRBG= positif(VARTMP2) * arr(TREP10C*(CODRBG)/TTSBC)
                 + (1-positif(VARTMP2)) * max(0,TREP10C-VARTMP1)
                          ;
TABTSC=VARTMP1+TABTSRBG;
TABDOMDBJ = positif(CODEBJ) * arr(TREP10C*CODDBJ/TTSBC)
	   + (1-positif(CODEBJ)) * max(0,TREP10C-TABTSC)+0;
TABDOMEBJ = max(0,TREP10C-TABTSC-TABDOMDBJ)+0;
VARTMP2=0;
VARTMP1=0;
VARTMP2=TSHALLO1+COD1CF+COD1CG+SALEXT1+ALLO1+CARTSP1+REMPLAP1+COD1IB+COD1CA+COD1IF+COD1IG+COD1IHRET+COD1CDRET+CODRCF+CODRCG+COD1CDRET;
TABTS1CJ=arr(TREP101*(TSHALLO1)/TTSB1);
VARTMP2=VARTMP2 - TSHALLO1-COD1CF;
VARTMP1=TABTS1CJ;
TABTS1CF= positif(VARTMP2) * arr(TREP101*(COD1CF)/TTSB1)
                + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1);
VARTMP2=VARTMP2 - COD1CG;
VARTMP1=VARTMP1+TABTS1CF;
TABTS1CG= positif(VARTMP2) * arr(TREP101*(COD1CG)/TTSB1)
                + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1);
VARTMP2=VARTMP2 - SALEXT1;
VARTMP1=VARTMP1+TABTS1CG;
TABTS1CC= positif(VARTMP2) * arr(TREP101*(SALEXT1)/TTSB1)
                 + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1)
                          ;
VARTMP2=VARTMP2 - ALLO1;
VARTMP1=VARTMP1+TABTS1CC;
TABTS1CP= positif(VARTMP2) * arr(TREP101*(ALLO1)/TTSB1)
                + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1);
VARTMP2=VARTMP2 - COD1IB;
VARTMP1=VARTMP1+TABTS1CP;
TABTS1IB= positif(VARTMP2) * arr(TREP101*(COD1IB)/TTSB1)
                + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1);
VARTMP2=VARTMP2 - COD1CA;
VARTMP1=VARTMP1+TABTS1IB;
TABTS1CA= positif(VARTMP2) * arr(TREP101*(COD1CA)/TTSB1)
                + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1);
VARTMP2=VARTMP2 - COD1IF;
VARTMP1=VARTMP1+TABTS1CA;
TABTS1IF= positif(VARTMP2) * arr(TREP101*(COD1IF)/TTSB1)
                + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1);
VARTMP2=VARTMP2 - COD1IG;
VARTMP1=VARTMP1+TABTS1IF;
TABTS1IG= positif(VARTMP2) * arr(TREP101*(COD1IG)/TTSB1)
                 + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1);
VARTMP2=VARTMP2 - COD1IHRET;
VARTMP1=VARTMP1+TABTS1IG;
TABTS1IH= positif(VARTMP2) *  arr(TREP101*(COD1IHRET)/TTSB1)
                 + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1)
                           ;
VARTMP2=VARTMP2 - COD1CDRET;
VARTMP1=VARTMP1+TABTS1IH;
TABTS1CD= positif(VARTMP2) *  arr(TREP101*(COD1CDRET)/TTSB1)
                 + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1)
		                           ;
VARTMP2=VARTMP2 - CARTSP1;
VARTMP1=VARTMP1+TABTS1CD;
TABTSRCJ= positif(VARTMP2) * arr(TREP101*(CARTSP1)/TTSB1)
                 + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1);
VARTMP2=VARTMP2 - REMPLAP1;
VARTMP1=VARTMP1+TABTSRCJ;
TABTSRCP= positif(VARTMP2) * arr(TREP101*(REMPLAP1)/TTSB1)
                 + (1-positif(VARTMP2)) * max(0,TREP101-VARTMP1);
VARTMP1=VARTMP1+TABTSRCP;
TABTSRCF= positif(CODRCG) * arr(TREP101*(CODRCF)/TTSB1)
                 + (1-positif(CODRCG)) * max(0,TREP101-VARTMP1);
VARTMP1=VARTMP1+TABTSRCF;
TABTSRCG=max(0,TREP101 - VARTMP1)+0;
VARTMP2=0;
VARTMP1=0;
VARTMP2=TSHALLO2+COD1DF+COD1DG+SALEXT2+ALLO2+CARTSP2+REMPLAP2+COD1JB+COD1DA+COD1JF+COD1JG+COD1JHRET+COD1DDRET+CODRDF+CODRDG+COD1DDRET;
TABTS1DJ=arr(TREP102*(TSHALLO2)/TTSB2);
VARTMP2=VARTMP2 - TSHALLO2-COD1DF;
VARTMP1=TABTS1DJ;
TABTS1DF= positif(VARTMP2) * arr(TREP102*(COD1DF)/TTSB2)
                + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1);
VARTMP2=VARTMP2 - COD1DG;
VARTMP1=VARTMP1+TABTS1DF;
TABTS1DG= positif(VARTMP2) * arr(TREP102*(COD1DG)/TTSB2)
                + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1);
VARTMP2=VARTMP2 - SALEXT2;
VARTMP1=VARTMP1+TABTS1DG;
TABTS1DC= positif(VARTMP2) * arr(TREP102*(SALEXT2)/TTSB2)
                 + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1)
                          ;
VARTMP2=VARTMP2 - ALLO2;
VARTMP1=VARTMP1+TABTS1DC;
TABTS1DP= positif(VARTMP2) * arr(TREP102*(ALLO2)/TTSB2)
                + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1);
VARTMP2=VARTMP2 - COD1JB;
VARTMP1=VARTMP1+TABTS1DP;
TABTS1JB= positif(VARTMP2) * arr(TREP102*(COD1JB)/TTSB2)
                 + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1);
VARTMP2=VARTMP2 - COD1DA;
VARTMP1=VARTMP1+TABTS1JB;
TABTS1DA= positif(VARTMP2) * arr(TREP102*(COD1DA)/TTSB2)
                 + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1);
VARTMP2=VARTMP2 - COD1JF;
VARTMP1=VARTMP1+TABTS1DA;
TABTS1JF= positif(VARTMP2) * arr(TREP102*(COD1JF)/TTSB2)
                 + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1);
VARTMP2=VARTMP2 - COD1JG;
VARTMP1=VARTMP1+TABTS1JF;
TABTS1JG= positif(VARTMP2) * arr(TREP102*(COD1JG)/TTSB2)
                 + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1);
VARTMP2=VARTMP2 - COD1JHRET;
VARTMP1=VARTMP1+TABTS1JG;
TABTS1JH= positif(VARTMP2) *  arr(TREP102*(COD1JHRET)/TTSB2)
                 + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1)
                           ;
VARTMP2=VARTMP2 - COD1DDRET;
VARTMP1=VARTMP1+TABTS1JH;
TABTS1DD= positif(VARTMP2) *  arr(TREP102*(COD1DDRET)/TTSB2)
                 + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1)
		                           ;
VARTMP2=VARTMP2 - CARTSP2;
VARTMP1=VARTMP1+TABTS1DD;
TABTSRDJ= positif(VARTMP2) * arr(TREP102*(CARTSP2)/TTSB2)
                 + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1);
VARTMP2=VARTMP2 - REMPLAP2;
VARTMP1=VARTMP1+TABTSRDJ;
TABTSRDP= positif(VARTMP2) * arr(TREP102*(REMPLAP2)/TTSB2)
                 + (1-positif(VARTMP2)) * max(0,TREP102-VARTMP1);

VARTMP2 = VARTMP2 - CODRDF ;
VARTMP1 = VARTMP1 + TABTSRDP ;
TABTSRDF = positif(VARTMP2) * arr(TREP102 * CODRDF / TTSB2)
                 + (1 - positif(VARTMP2)) * max(0 , TREP102 - VARTMP1) ;

VARTMP1=VARTMP1+TABTSRDF;
TABTSRDG=max(0,TREP102 - VARTMP1)+0;
VARTMP2=0;
VARTMP1=0;
VARTMP2=TSHALLO3+COD1EF+COD1EG+SALEXT3+ALLO3+CARTSP3+REMPLAP3+COD1EA+COD1KF+COD1KG+COD1KHRET+COD1EDRET+CODREF+CODRGG+COD1EDRET;
TABTS1EJ=arr(TREP103*(TSHALLO3)/TTSB3);
VARTMP2=VARTMP2 - TSHALLO3-COD1EF;
VARTMP1=TABTS1EJ;
TABTS1EF= positif(VARTMP2) * arr(TREP103*(COD1EF)/TTSB3)
                + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1);
VARTMP2=VARTMP2 - COD1EG;
VARTMP1=VARTMP1+TABTS1EF;
TABTS1EG= positif(VARTMP2) * arr(TREP103*(COD1EG)/TTSB3)
                + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1);
VARTMP2=VARTMP2 - SALEXTC;
VARTMP1=VARTMP1+TABTS1EG;
TABTS1EC= positif(VARTMP2) * arr(TREP103*(SALEXT3)/TTSB3)
                 + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1)
                          ;
VARTMP2=VARTMP2 - ALLO3;
VARTMP1=VARTMP1+TABTS1EC;
TABTS1EP= positif(VARTMP2) * arr(TREP103*(ALLO3)/TTSB3)
                + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1);
VARTMP1=VARTMP1+TABTS1EP;
VARTMP2=VARTMP2 - COD1EA;
TABTS1EA= positif(VARTMP2) * arr(TREP103*(COD1EA)/TTSB3)
                 + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1);
VARTMP2=VARTMP2 - COD1KF;
VARTMP1=VARTMP1+TABTS1EA;
TABTS1KF= positif(VARTMP2) * arr(TREP103*(COD1KF)/TTSB3)
                 + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1);
VARTMP2=VARTMP2 - COD1KG;
VARTMP1=VARTMP1+TABTS1KF;
TABTS1KG= positif(VARTMP2) * arr(TREP103*(COD1KG)/TTSB3)
                 + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1);
VARTMP2=VARTMP2 - COD1KHRET;
VARTMP1=VARTMP1+TABTS1KG;
TABTS1KH= positif(VARTMP2) *  arr(TREP103*(COD1KHRET)/TTSB3)
                 + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1)
                           ;
VARTMP2=VARTMP2 - COD1EDRET;
VARTMP1=VARTMP1+TABTS1KH;
TABTS1ED= positif(VARTMP2) *  arr(TREP103*(COD1EDRET)/TTSB3)
                 + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1)
		          ;
VARTMP2=VARTMP2 - CARTSP3;
VARTMP1=VARTMP1+TABTS1ED;
TABTSREJ= positif(VARTMP2) * arr(TREP103*(CARTSP3)/TTSB3)
                 + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1);
VARTMP2=VARTMP2 - REMPLAP3;
VARTMP1=VARTMP1+TABTSREJ;
TABTSREP= positif(VARTMP2) * arr(TREP103*(REMPLAP3)/TTSB3)
                 + (1-positif(VARTMP2)) * max(0,TREP103-VARTMP1);
VARTMP1=VARTMP1+TABTSREP;
TABTSREF= positif(CODRGG) * arr(TREP103*(CODREF)/TTSB3)
                 + (1-positif(CODRGG)) * max(0,TREP103-VARTMP1);
VARTMP1=VARTMP1+TABTSREF;
TABTSRGG=max(0,TREP103 - VARTMP1) +0;
VARTMP2=0;
VARTMP1=0;
VARTMP2=TSHALLO4+COD1FF+COD1FG+SALEXT4+ALLO4+CARTSP4+REMPLAP4+COD1FA+COD1LF+COD1LG+COD1LHRET+COD1FDRET+CODRFF+CODRFG+COD1FDRET;
TABTS1FJ=arr(TREP104*(TSHALLO4)/TTSB4);
VARTMP2=VARTMP2 - TSHALLO4-COD1FF;
VARTMP1=TABTS1FJ;
TABTS1FF= positif(VARTMP2) * arr(TREP104*(COD1FF)/TTSB4)
                + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1);
VARTMP2=VARTMP2 - COD1FG;
VARTMP1=VARTMP1+TABTS1FF;
TABTS1FG= positif(VARTMP2) * arr(TREP104*(COD1FG)/TTSB4)
                + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1);
VARTMP2=VARTMP2 - SALEXTC;
VARTMP1=VARTMP1+TABTS1FG;
TABTS1FC= positif(VARTMP2) * arr(TREP104*(SALEXT4)/TTSB4)
                 + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1)
                          ;
VARTMP2=VARTMP2 - ALLO4;
VARTMP1=VARTMP1+TABTS1FC;
TABTS1FP= positif(VARTMP2) * arr(TREP104*(ALLO4)/TTSB4)
                + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1);
VARTMP1=VARTMP1+TABTS1FP;
VARTMP2=VARTMP2 - COD1FA;
TABTS1FA= positif(VARTMP2) * arr(TREP104*(COD1FA)/TTSB4)
                 + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1);
VARTMP2=VARTMP2 - COD1LF;
VARTMP1=VARTMP1+TABTS1FA;
TABTS1LF= positif(VARTMP2) * arr(TREP104*(COD1LF)/TTSB4)
                 + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1);
VARTMP2=VARTMP2 - COD1LG;
VARTMP1=VARTMP1+TABTS1LF;
TABTS1LG= positif(VARTMP2) * arr(TREP104*(COD1LG)/TTSB4)
                 + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1);
VARTMP2=VARTMP2 - COD1LHRET;
VARTMP1=VARTMP1+TABTS1LG;
TABTS1LH= positif(VARTMP2) *  arr(TREP104*(COD1LHRET)/TTSB4)
                 + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1)
                           ;
VARTMP2=VARTMP2 - COD1FDRET;
VARTMP1=VARTMP1+TABTS1LH;
TABTS1FD= positif(VARTMP2) *  arr(TREP104*(COD1FDRET)/TTSB4)
                 + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1)
                          ;
VARTMP2=VARTMP2 - CARTSP4;
VARTMP1=VARTMP1+TABTS1FD;
TABTSRFJ= positif(VARTMP2) * arr(TREP104*(CARTSP4)/TTSB4)
                 + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1);
VARTMP2=VARTMP2 - REMPLAP4;
VARTMP1=VARTMP1+TABTSRFJ;
TABTSRFP= positif(VARTMP2) * arr(TREP104*(REMPLAP4)/TTSB4)
                 + (1-positif(VARTMP2)) * max(0,TREP104-VARTMP1);
VARTMP1=VARTMP1+TABTSRFP;
TABTSRFF= positif(CODRFG) * arr(TREP104*(CODRFF)/TTSB4)
                 + (1-positif(CODRFG)) * max(0,TREP104-VARTMP1);
VARTMP1=VARTMP1+TABTSRFF;
TABTSRFG=max(0,TREP104 - VARTMP1)+0;
VARTMP2=0;
VARTMP1=0;
regle 99992600:
application : iliad   ;
TABGLTV = somme (x=1..3: TABGLxV)+TABDOMDAJ + TABDOMEAJ;
TABGLTC = somme (x=1..3: TABGLxC)+TABDOMDBJ + TABDOMEBJ;
regle 899999999:
application : iliad   ;
TTSN1AJ = TSHALLOV - TABTS1AJ;
TTSN1PM = COD1PM - TABTS1PM ;
TTSN1TP = COD1TP - TABTS1TP ;
TTSN1NX = COD1NX - TABTS1NX ;
TTSN1AF = COD1AF - TABTS1AF ;
TTSN1AG = COD1AG - TABTS1AG ;
TTSN1AC = SALEXTV- TABTS1AC;
TTSN1AP = ALLOV - TABTS1AP;
TTSN3VJ = BPCOSAV - TABTS3VJ;
TTSN1TT = GLDGRATV - TABTS1TT;
TTSNRAJ = (CARTSV - TABTSRAJ) ;
TTSNRAP = (REMPLAV - TABTSRAP);
TTSNDAJ = (CODDAJ - TABDOMDAJ);
TTSNEAJ = (CODEAJ - TABDOMEAJ);
TTSN1GB = (COD1GB - TABTS1GB) ;
TTSN1AA = (COD1AA - TABTS1AA) ;
TTSN1GF = (COD1GF - TABTS1GF) ;
TTSN1GG = (COD1GG - TABTS1GG) ;
TTSN1GH = (COD1GHRET - TABTS1GH) ;
TTSN1AD = (COD1ADRET - TABTS1AD) ;
TTSNRAF = (CODRAF - TABTSRAF) ;
TTSNRAG = (CODRAG - TABTSRAG) ;
TTSN1BJ = TSHALLOC - TABTS1BJ;
TTSN1QM = COD1QM - TABTS1QM ;
TTSN1UP = COD1UP - TABTS1UP ;
TTSN1OX = COD1OX - TABTS1OX ;
TTSN1BF = COD1BF - TABTS1BF ;
TTSN1BG = COD1BG - TABTS1BG ;
TTSN1BC = SALEXTC- TABTS1BC;
TTSN1BP = ALLOC - TABTS1BP;
TTSN3VK = BPCOSAC - TABTS3VK;
TTSN1UT = GLDGRATC - TABTS1UT;
TTSNRBJ = (CARTSC - TABTSRBJ);
TTSNRBP = (REMPLAC - TABTSRBP);
TTSNDBJ = (CODDBJ - TABDOMDBJ);
TTSNEBJ = (CODEBJ - TABDOMEBJ);
TTSN1HB = (COD1HB - TABTS1HB) ;
TTSN1BA = (COD1BA - TABTS1BA) ;
TTSN1HF = (COD1HF - TABTS1HF) ;
TTSN1HG = (COD1HG - TABTS1HG) ;
TTSN1HH = (COD1HHRET - TABTS1HH) ;
TTSN1BD = (COD1BDRET - TABTS1BD) ;
TTSNRBF = (CODRBF - TABTSRBF) ;
TTSNRBG = (CODRBG - TABTSRBG) ;
TTSN1CJ = TSHALLO1 - TABTS1CJ;
TTSN1CF = COD1CF - TABTS1CF ;
TTSN1CG = COD1CG - TABTS1CG ;
TTSN1CC = SALEXT1- TABTS1CC;
TTSN1CP = ALLO1 - TABTS1CP;
TTSNRCJ = (CARTSP1 - TABTSRCJ);
TTSNRCP = (REMPLAP1 - TABTSRCP);
TTSN1IB = (COD1IB - TABTS1IB) ;
TTSN1CA = (COD1CA - TABTS1CA) ;
TTSN1IF = (COD1IF - TABTS1IF) ;
TTSN1IG = (COD1IG - TABTS1IG) ;
TTSN1IH = (COD1IHRET - TABTS1IH) ;
TTSN1CD = (COD1CDRET - TABTS1CD) ;
TTSNRCF = (CODRCF - TABTSRCF) ;
TTSNRCG = (CODRCG - TABTSRCG) ;
TTSN1DJ = TSHALLO2 - TABTS1DJ;
TTSN1DF = COD1DF - TABTS1DF ;
TTSN1DG = COD1DG - TABTS1DG ;
TTSN1DC = SALEXT2- TABTS1DC;
TTSN1DP = ALLO2 - TABTS1DP;
TTSNRDJ = (CARTSP2 - TABTSRDJ);
TTSNRDP = (REMPLAP2 - TABTSRDP);
TTSN1JB = (COD1JB - TABTS1JB) ;
TTSN1DA = (COD1DA - TABTS1DA) ;
TTSN1JF = (COD1JF - TABTS1JF) ;
TTSN1JG = (COD1JG - TABTS1JG) ;
TTSN1JH = (COD1JHRET - TABTS1JH) ;
TTSN1DD = (COD1DDRET - TABTS1DD) ;
TTSNRDF = (CODRDF - TABTSRDF) ;
TTSNRDG = (CODRDG - TABTSRDG) ;
TTSN1EJ = TSHALLO3 - TABTS1EJ;
TTSN1EF = COD1EF - TABTS1EF ;
TTSN1EG = COD1EG - TABTS1EG ;
TTSN1EC = SALEXT3- TABTS1EC;
TTSN1EP = ALLO3 - TABTS1EP;
TTSNREJ = (CARTSP3 - TABTSREJ);
TTSNREP = (REMPLAP3 - TABTSREP);
TTSN1EA = (COD1EA - TABTS1EA) ;
TTSN1KF = (COD1KF - TABTS1KF) ;
TTSN1KG = (COD1KG - TABTS1KG) ;
TTSN1KH = (COD1KHRET - TABTS1KH) ;
TTSN1ED = (COD1EDRET - TABTS1ED) ;
TTSNREF = (CODREF - TABTSREF) ;
TTSNRGG = (CODRGG - TABTSRGG) ;
TTSN1FJ = TSHALLO4 - TABTS1FJ;
TTSN1FF = COD1FF - TABTS1FF ;
TTSN1FG = COD1FG - TABTS1FG ;
TTSN1FC = SALEXT4- TABTS1FC;
TTSN1FP = ALLO4 - TABTS1FP;
TTSNRFJ = (CARTSP4 - TABTSRFJ);
TTSNRFP = (REMPLAP4 - TABTSRFP);
TTSN1FA = (COD1FA - TABTS1FA) ;
TTSN1LF = (COD1LF - TABTS1LF) ;
TTSN1LG = (COD1LG - TABTS1LG) ;
TTSN1LH = (COD1LHRET - TABTS1LH) ;
TTSN1FD = (COD1FDRET - TABTS1FD) ;
TTSNRFF = (CODRFF - TABTSRFF) ;
TTSNRFG = (CODRFG - TABTSRFG) ;

regle 99992700:
application : iliad   ;
TPLRV = min ( MIN_DEDPR , TEXPRV );
TPLRC = min ( MIN_DEDPR , TEXPRC );
TPLR1 = min ( MIN_DEDPR , TEXPR1 );
TPLR2 = min ( MIN_DEDPR , TEXPR2 );
TPLR3 = min ( MIN_DEDPR , TEXPR3 );
TPLR4 = min ( MIN_DEDPR , TEXPR4 );
TAPBV = max( TPLRV , arr(TEXPRV*TX_DEDPER/100));
TAPBC = max( TPLRC , arr(TEXPRC*TX_DEDPER/100));
TAPB1 = max( TPLR1 , arr(TEXPR1*TX_DEDPER/100));
TAPB2 = max( TPLR2 , arr(TEXPR2*TX_DEDPER/100));
TAPB3 = max( TPLR3 , arr(TEXPR3*TX_DEDPER/100));
TAPB4 = max( TPLR4 , arr(TEXPR4*TX_DEDPER/100));
TIND_APBV = positif_ou_nul(TPLRV- arr(TEXPRV * TX_DEDPER/100));
TIND_APBC = positif_ou_nul(TPLRC- arr(TEXPRC * TX_DEDPER/100));
TIND_APB1 = positif_ou_nul(TPLR1- arr(TEXPR1 * TX_DEDPER/100));
TIND_APB2 = positif_ou_nul(TPLR2- arr(TEXPR2 * TX_DEDPER/100));
TIND_APB3 = positif_ou_nul(TPLR3- arr(TEXPR3 * TX_DEDPER/100));
TIND_APB4 = positif_ou_nul(TPLR4- arr(TEXPR4 * TX_DEDPER/100));
TPL_PB = arr(PLAF_DEDPRFOYER -somme (i=V,C,1..4: TAPBi * TIND_APBi));
regle 99992800:
application : iliad   ;
TABPRV = arr ( (1 - TIND_APBV) * min(TAPBV,(TPL_PB * TAPBV / somme(x=V,C,1..4:TAPBx * (1 - TIND_APBx)))) + TIND_APBV * TAPBV );
TABPRC = positif(TEXPR1+TEXPR2+TEXPR3+TEXPR4) * (arr ( (1 - TIND_APBC) * min(TAPBC,(TPL_PB * TAPBC / somme(x=V,C,1..4:TAPBx * (1 - TIND_APBx)))) + TIND_APBC * TAPBC ))
        + (1-positif(TEXPR1+TEXPR2+TEXPR3+TEXPR4)) *  max(0,min(TAPBV+TAPBC+TAPB1+TAPB2+TAPB3+TAPB4,PLAF_DEDPRFOYER) - TABPRV);
TABPR1 = positif(TEXPR2+TEXPR3+TEXPR4) * (arr ( (1 - TIND_APB1) * min(TAPB1,(TPL_PB * TAPB1 / somme(x=V,C,1..4:TAPBx * (1 - TIND_APBx)))) + TIND_APB1 * TAPB1 ))
        + (1-positif(TEXPR2+TEXPR3+TEXPR4)) *  max(0,min(TAPBV+TAPBC+TAPB1+TAPB2+TAPB3+TAPB4,PLAF_DEDPRFOYER) - TABPRV-TABPRC);
TABPR2 = positif(TEXPR3+TEXPR4) * (arr ( (1 - TIND_APB2) * min(TAPB2,(TPL_PB * TAPB2 / somme(x=V,C,1..4:TAPBx * (1 - TIND_APBx)))) + TIND_APB2 * TAPB2 ))
        + (1-positif(TEXPR3+TEXPR4)) *  max(0,min(TAPBV+TAPBC+TAPB1+TAPB2+TAPB3+TAPB4,PLAF_DEDPRFOYER) - TABPRV-TABPRC-TABPR1);
TABPR3 = positif(TEXPR4) * (arr ( (1 - TIND_APB3) * min(TAPB3,(TPL_PB * TAPB3 / somme(x=V,C,1..4:TAPBx * (1 - TIND_APBx)))) + TIND_APB3 * TAPB3 ))
        + (1-positif(TEXPR4)) *  max(0,min(TAPBV+TAPBC+TAPB1+TAPB2+TAPB3+TAPB4,PLAF_DEDPRFOYER) - TABPRV-TABPRC-TABPR1-TABPR2);
TABPR4 = max(0,min(TAPBV+TAPBC+TAPB1+TAPB2+TAPB3+TAPB4,PLAF_DEDPRFOYER) - TABPRV-TABPRC-TABPR1-TABPR2-TABPR3);
regle 99992900:
application : iliad   ;

VARTMP2=0;
VARTMP1=0;
VARTMP2=PRBRV+COD1AL+COD1AM+PENINV+PALIV+CARPEV+CODRAZ+PENSALV+COD1AH+PEBFV+CODRAL+CODRAM;
TAPR1AS = arr(TABPRV*(PRBRV)/TEXPRV);
VARTMP2=VARTMP2 - PRBRV-COD1AL;
VARTMP1=TAPR1AS;
TAPR1AL = positif(VARTMP2) * arr(TABPRV*(COD1AL)/TEXPRV)
                 + (1-positif(VARTMP2)) * max(0,TABPRV-VARTMP1) ;
VARTMP2=VARTMP2 - COD1AM;
VARTMP1=VARTMP1+TAPR1AL;
TAPR1AM = positif(VARTMP2) * arr(TABPRV*(COD1AM)/TEXPRV)
                 + (1-positif(VARTMP2)) * max(0,TABPRV-VARTMP1) ;
VARTMP2=VARTMP2 - PENINV;
VARTMP1=VARTMP1+TAPR1AM;
TAPR1AZ = positif(VARTMP2) * arr(TABPRV*(PENINV)/TEXPRV)
                 + (1-positif(VARTMP2)) * max(0,TABPRV-VARTMP1) ;
VARTMP2=VARTMP2 - PALIV;
VARTMP1=VARTMP1+TAPR1AZ;
TAPR1AO = positif(VARTMP2) * arr(TABPRV*(PALIV)/TEXPRV)
                 + (1-positif(VARTMP2)) * max(0,TABPRV-VARTMP1) ;
VARTMP2=VARTMP2 - CARPEV;
VARTMP1=VARTMP1+TAPR1AO;
TAPRRAS = positif(VARTMP2) * arr(TABPRV*(CARPEV)/TEXPRV)
                 + (1-positif(VARTMP2)) * max(0,TABPRV-VARTMP1) ;
VARTMP2=VARTMP2 - CODRAL;
VARTMP1=VARTMP1+TAPRRAS;
TAPRRAL = positif(VARTMP2) * arr(TABPRV*(CODRAL)/TEXPRV)
                 + (1-positif(VARTMP2)) * max(0,TABPRV-VARTMP1) ;
VARTMP2=VARTMP2 - CODRAM;
VARTMP1=VARTMP1+TAPRRAL;
TAPRRAM = positif(VARTMP2) * arr(TABPRV*(CODRAM)/TEXPRV)
                 + (1-positif(VARTMP2)) * max(0,TABPRV-VARTMP1) ;
VARTMP2=VARTMP2 - CODRAZ;
VARTMP1=VARTMP1+TAPRRAM;
TAPRRAZ = positif(VARTMP2) * arr(TABPRV*(CODRAZ)/TEXPRV)
                 + (1-positif(VARTMP2)) * max(0,TABPRV-VARTMP1) ;
VARTMP2=VARTMP2 - PENSALV;
VARTMP1=VARTMP1+TAPRRAZ;
TAPRRAO = positif(VARTMP2) * arr(TABPRV*(PENSALV)/TEXPRV)
                 + (1-positif(VARTMP2)) * max(0,TABPRV-VARTMP1) ;
VARTMP1=VARTMP1+TAPRRAO;
TAPR1AH = positif(PEBFV) * arr(TABPRV*(COD1AH)/TEXPRV)
                 + (1-positif(PEBFV)) * max(0,TABPRV-VARTMP1) ;
VARTMP1=VARTMP1+TAPR1AH;
TAPRFAS = max(0,TABPRV-VARTMP1);
TAPRV  =  TAPR1AS+TAPR1AL+TAPR1AM+TAPR1AZ+TAPR1AO+TAPRRAS+TAPRRAZ+TAPRRAO +TAPR1AH+TAPRFAS+TAPRRAL+TAPRRAM;
VARTMP2=0;
VARTMP1=0;
VARTMP2=PRBRC+COD1BL+COD1BM+PENINC+PALIC+CARPEC+CODRBZ+PENSALC+COD1BH+PEBFC+CODRBL+CODRBM;
TAPR1BS = arr(TABPRC*(PRBRC)/EXPRC);
VARTMP2=VARTMP2 - PRBRC-COD1BL;
VARTMP1=TAPR1BS;
TAPR1BL = positif(VARTMP2) * arr(TABPRC*(COD1BL)/TEXPRC)
                 + (1-positif(VARTMP2)) * max(0,TABPRC-VARTMP1) ;
VARTMP2=VARTMP2 - COD1BM;
VARTMP1=VARTMP1+TAPR1BL;
TAPR1BM = positif(VARTMP2) * arr(TABPRC*(COD1BM)/TEXPRC)
                 + (1-positif(VARTMP2)) * max(0,TABPRC-VARTMP1) ;
VARTMP2=VARTMP2 -PENINC;
VARTMP1=VARTMP1+TAPR1BM;
TAPR1BZ = positif(VARTMP2) * arr(TABPRC*(PENINC)/TEXPRC)
                 + (1-positif(VARTMP2)) * max(0,TABPRC-VARTMP1) ;
VARTMP2=VARTMP2 - PALIC;
VARTMP1=VARTMP1+TAPR1BZ;
TAPR1BO = positif(VARTMP2) * arr(TABPRC*(PALIC)/TEXPRC)
                 + (1-positif(VARTMP2)) * max(0,TABPRC-VARTMP1) ;
VARTMP2=VARTMP2 - CARPEC;
VARTMP1=VARTMP1+TAPR1BO;
TAPRRBS = positif(VARTMP2) * arr(TABPRC*(CARPEC)/TEXPRC)
                 + (1-positif(VARTMP2)) * max(0,TABPRC-VARTMP1) ;
VARTMP2=VARTMP2 - CODRBL;
VARTMP1=VARTMP1+TAPRRBS;
TAPRRBL = positif(VARTMP2) * arr(TABPRC*(CODRBL)/TEXPRC)
                 + (1-positif(VARTMP2)) * max(0,TABPRC-VARTMP1) ;
VARTMP2=VARTMP2 - CODRBM;
VARTMP1=VARTMP1+TAPRRBL;
TAPRRBM = positif(VARTMP2) * arr(TABPRC*(CODRBM)/TEXPRC)
                 + (1-positif(VARTMP2)) * max(0,TABPRC-VARTMP1) ;
VARTMP2=VARTMP2 - CODRBZ;
VARTMP1=VARTMP1+TAPRRBM;
TAPRRBZ = positif(VARTMP2) * arr(TABPRC*(CODRBZ)/TEXPRC)
                 + (1-positif(VARTMP2)) * max(0,TABPRC-VARTMP1) ;
VARTMP2=VARTMP2 - PENSALC;
VARTMP1=VARTMP1+TAPRRBZ;
TAPRRBO = positif(VARTMP2) * arr(TABPRC*(PENSALC)/TEXPRC)
                 + (1-positif(VARTMP2)) * max(0,TABPRC-VARTMP1) ;
VARTMP1=VARTMP1+TAPRRBO;
TAPR1BH = positif(PEBFC) * arr(TABPRC*(COD1BH)/TEXPRC)
                 + (1-positif(PEBFC)) * max(0,TABPRC-VARTMP1) ;
VARTMP1=VARTMP1+TAPR1BH;
TAPRFBS = max(0,TABPRC-VARTMP1);
TAPRC  =  TAPR1BS+TAPR1BL+TAPR1BM+TAPR1BZ+TAPR1BO+TAPRRBS+TAPRRBZ+TAPRRBO+ TAPRFBS+TAPRRBL+TAPRRBM;
VARTMP2=0;
VARTMP1=0;
VARTMP2=PRBR1+COD1CL+COD1CM+PENIN1+PALI1+CARPEP1+CODRCZ+PENSALP1+COD1CH+PEBF1+CODRCL+CODRCM;
TAPR1CS = arr(TABPR1*(PRBR1)/TEXPR1);
VARTMP2=VARTMP2 - PRBR1-COD1CL;
VARTMP1=TAPR1CS;
TAPR1CL = positif(VARTMP2) * arr(TABPR1*(COD1CL)/TEXPR1)
                 + (1-positif(VARTMP2)) * max(0,TABPR1-VARTMP1) ;
VARTMP2=VARTMP2 - COD1CM;
VARTMP1=VARTMP1+TAPR1CL;
TAPR1CM = positif(VARTMP2) * arr(TABPR1*(COD1CM)/TEXPR1)
                 + (1-positif(VARTMP2)) * max(0,TABPR1-VARTMP1) ;
VARTMP2=VARTMP2 - PENIN1;
VARTMP1=VARTMP1+TAPR1CM;
TAPR1CZ = positif(VARTMP2) * arr(TABPR1*(PENIN1)/TEXPR1)
                 + (1-positif(VARTMP2)) * max(0,TABPR1-VARTMP1) ;
VARTMP2=VARTMP2 - PALI1;
VARTMP1=VARTMP1+TAPR1CZ;
TAPR1CO = positif(VARTMP2) * arr(TABPR1*(PALI1)/TEXPR1)
                 + (1-positif(VARTMP2)) * max(0,TABPR1-VARTMP1) ;
VARTMP2=VARTMP2 - CARPEP1;
VARTMP1=VARTMP1+TAPR1CO;
TAPRRCS = positif(VARTMP2) * arr(TABPR1*(CARPEP1)/TEXPR1)
                 + (1-positif(VARTMP2)) * max(0,TABPR1-VARTMP1) ;
VARTMP2=VARTMP2 - CODRCL;
VARTMP1=VARTMP1+TAPRRCS;
TAPRRCL = positif(VARTMP2) * arr(TABPR1*(CODRCL)/TEXPR1)
                 + (1-positif(VARTMP2)) * max(0,TABPR1-VARTMP1) ;
VARTMP2=VARTMP2 - CODRCM;
VARTMP1=VARTMP1+TAPRRCL;
TAPRRCM = positif(VARTMP2) * arr(TABPR1*(CODRCM)/TEXPR1)
                 + (1-positif(VARTMP2)) * max(0,TABPR1-VARTMP1) ;
VARTMP2=VARTMP2 - CODRCZ;
VARTMP1=VARTMP1+TAPRRCM;
TAPRRCZ = positif(VARTMP2) * arr(TABPR1*(CODRCZ)/TEXPR1)
                 + (1-positif(VARTMP2)) * max(0,TABPR1-VARTMP1) ;
VARTMP2=VARTMP2 - PENSALP1;
VARTMP1=VARTMP1+TAPRRCZ;
TAPRRCO = positif(VARTMP2) * arr(TABPR1*(PENSALP1)/TEXPR1)
                 + (1-positif(VARTMP2)) * max(0,TABPR1-VARTMP1) ;
VARTMP1=VARTMP1+TAPRRCO;
TAPR1CH = positif(PEBF1) * arr(TABPR1*(COD1CH)/TEXPR1)
                 + (1-positif(PEBF1)) * max(0,TABPR1-VARTMP1) ;
VARTMP1=VARTMP1+TAPR1CH;
TAPRFCS = max(0,TABPR1-VARTMP1);
TAPR1  =  TAPR1CS+TAPR1CL+TAPR1CM+TAPR1CZ+TAPR1CO+TAPRRCS+TAPRRCZ+TAPRRCO +TAPR1CH+TAPRFCS+TAPRRCL+TAPRRCM;
VARTMP2=0;
VARTMP1=0;
VARTMP2=PRBR2+COD1DL+COD1DM+PENIN2+PALI2+CARPEP2+CODRDZ+PENSALP2+COD1DH+PEBF2+CODRDL+CODRDM;
TAPR1DS = arr(TABPR2*(PRBR2)/TEXPR2);
VARTMP2=VARTMP2 - PRBR2-COD1DL;
VARTMP1=TAPR1DS;
TAPR1DL = positif(VARTMP2) * arr(TABPR2*(COD1DL)/TEXPR2)
                 + (1-positif(VARTMP2)) * max(0,TABPR2-VARTMP1) ;
VARTMP2=VARTMP2 - COD1DM;
VARTMP1=VARTMP1+TAPR1DL;
TAPR1DM = positif(VARTMP2) * arr(TABPR2*(COD1DM)/TEXPR2)
                 + (1-positif(VARTMP2)) * max(0,TABPR2-VARTMP1) ;
VARTMP2=VARTMP2 - PENIN2;
VARTMP1=VARTMP1+TAPR1DM;
TAPR1DZ = positif(VARTMP2) * arr(TABPR2*(PENIN2)/TEXPR2)
                 + (1-positif(VARTMP2)) * max(0,TABPR2-VARTMP1) ;
VARTMP2=VARTMP2 - PALI2;
VARTMP1=VARTMP1+TAPR1DZ;
TAPR1DO = positif(VARTMP2) * arr(TABPR2*(PALI2)/TEXPR2)
                 + (1-positif(VARTMP2)) * max(0,TABPR2-VARTMP1) ;
VARTMP2=VARTMP2 - CARPEP2;
VARTMP1=VARTMP1+TAPR1DO;
TAPRRDS = positif(VARTMP2) * arr(TABPR2*(CARPEP2)/TEXPR2)
                 + (1-positif(VARTMP2)) * max(0,TABPR2-VARTMP1) ;
VARTMP2=VARTMP2 - CODRDL;
VARTMP1=VARTMP1+TAPRRDS;
TAPRRDL = positif(VARTMP2) * arr(TABPR2*(CODRDL)/TEXPR2)
                 + (1-positif(VARTMP2)) * max(0,TABPR2-VARTMP1) ;
VARTMP2=VARTMP2 - CODRDM;
VARTMP1=VARTMP1+TAPRRDL;
TAPRRDM = positif(VARTMP2) * arr(TABPR2*(CODRDM)/TEXPR2)
                 + (1-positif(VARTMP2)) * max(0,TABPR2-VARTMP1) ;
VARTMP2=VARTMP2 - CODRDZ;
VARTMP1=VARTMP1+TAPRRDM;
TAPRRDZ = positif(VARTMP2) * arr(TABPR2*(CODRDZ)/TEXPR2)
                 + (1-positif(VARTMP2)) * max(0,TABPR2-VARTMP1) ;
VARTMP2=VARTMP2 - PENSALP2;
VARTMP1=VARTMP1+TAPRRDZ;
TAPRRDO = positif(VARTMP2) * arr(TABPR2*(PENSALP2)/TEXPR2)
                 + (1-positif(VARTMP2)) * max(0,TABPR2-VARTMP1) ;
VARTMP1=VARTMP1+TAPRRDO;
TAPR1DH = positif(PEBF2) * arr(TABPR2*(COD1DH)/TEXPR2)
                 + (1-positif(PEBF2)) * max(0,TABPR2-VARTMP1) ;
VARTMP1=VARTMP1+TAPR1DH;
TAPRFDS = max(0,TABPR2-VARTMP1);
TAPR2  =  TAPR1DS+TAPR1DL+TAPR1DM+TAPR1DZ+TAPR1DO+TAPRRDS+TAPRRDZ+TAPRRDO +TAPR1DH+TAPRFDS+TAPRRDL+TAPRRDM;
VARTMP2=0;
VARTMP1=0;
VARTMP2=PRBR3+COD1EL+COD1EM+PENIN3+PALI3+CARPEP3+CODREZ+PENSALP3+COD1AH+PEBF3+CODREL+CODREM;
TAPR1ES = arr(TABPR3*(PRBR3)/TEXPR3);
VARTMP2=VARTMP2 - PRBR3-COD1EL;
VARTMP1=TAPR1ES;
TAPR1EL = positif(VARTMP2) * arr(TABPR3*(COD1EL)/TEXPR3)
                 + (1-positif(VARTMP2)) * max(0,TABPR3-VARTMP1) ;
VARTMP2=VARTMP2 - COD1EM;
VARTMP1=VARTMP1+TAPR1EL;
TAPR1EM = positif(VARTMP2) * arr(TABPR3*(COD1EM)/TEXPR3)
                 + (1-positif(VARTMP2)) * max(0,TABPR3-VARTMP1) ;
VARTMP2=VARTMP2 - PENIN3;
VARTMP1=VARTMP1+TAPR1EM;
TAPR1EZ = positif(VARTMP2) * arr(TABPR3*(PENIN3)/TEXPR3)
                 + (1-positif(VARTMP2)) * max(0,TABPR3-VARTMP1) ;
VARTMP2=VARTMP2 - PALI3;
VARTMP1=VARTMP1+TAPR1EZ;
TAPR1EO = positif(VARTMP2) * arr(TABPR3*(PALI3)/TEXPR3)
                 + (1-positif(VARTMP2)) * max(0,TABPR3-VARTMP1) ;
VARTMP2=VARTMP2 - CARPEP3;
VARTMP1=VARTMP1+TAPR1EO;
TAPRRES = positif(VARTMP2) * arr(TABPR3*(CARPEP3)/TEXPR3)
                 + (1-positif(VARTMP2)) * max(0,TABPR3-VARTMP1) ;
VARTMP2=VARTMP2 - CODREL;
VARTMP1=VARTMP1+TAPRRES;
TAPRREL = positif(VARTMP2) * arr(TABPR3*(CODREL)/TEXPR3)
                 + (1-positif(VARTMP2)) * max(0,TABPR3-VARTMP1) ;
VARTMP2=VARTMP2 - CODREM;
VARTMP1=VARTMP1+TAPRREL;
TAPRREM = positif(VARTMP2) * arr(TABPR3*(CODREM)/TEXPR3)
                 + (1-positif(VARTMP2)) * max(0,TABPR3-VARTMP1) ;
VARTMP2=VARTMP2 - CODREZ;
VARTMP1=VARTMP1+TAPRREM;
TAPRREZ = positif(VARTMP2) * arr(TABPR3*(CODREZ)/TEXPR3)
                 + (1-positif(VARTMP2)) * max(0,TABPR3-VARTMP1) ;
VARTMP2=VARTMP2 - PENSALP3;
VARTMP1=VARTMP1+TAPRREZ;
TAPRREO = positif(VARTMP2) * arr(TABPR3*(PENSALP3)/TEXPR3)
                 + (1-positif(VARTMP2)) * max(0,TABPR3-VARTMP1) ;
VARTMP1=VARTMP1+TAPRREO;
TAPR1EH = positif(PEBF3) * arr(TABPR3*(COD1EH)/TEXPR3)
                 + (1-positif(PEBF3)) * max(0,TABPR3-VARTMP1) ;
VARTMP1=VARTMP1+TAPR1EH;
TAPRFES = max(0,TABPR3-VARTMP1);
TAPR3  =  TAPR1ES+TAPR1EL+TAPR1EM+TAPR1EZ+TAPR1EO+TAPRRES+TAPRREZ+TAPRREO +TAPR1EH+TAPRFES+TAPRREL+TAPRREM;
VARTMP2=0;
VARTMP1=0;
VARTMP2=PRBR4+COD1FL+COD1FM+PENIN4+PALI4+CARPEP4+CODRFZ+PENSALP4+COD1EH+PEBF4+CODRFL+CODRFM;
TAPR1FS = arr(TABPR4*(PRBR4)/TEXPR4);
VARTMP2=VARTMP2 - PRBR4-COD1FL;
VARTMP1=TAPR1FS;
TAPR1FL = positif(VARTMP2) * arr(TABPR4*(COD1FL)/TEXPR4)
                 + (1-positif(VARTMP2)) * max(0,TABPR4-VARTMP1) ;
VARTMP2=VARTMP2 - COD1FM;
VARTMP1=VARTMP1+TAPR1FL;
TAPR1FM = positif(VARTMP2) * arr(TABPR4*(COD1FM)/TEXPR4)
                 + (1-positif(VARTMP2)) * max(0,TABPR4-VARTMP1) ;
VARTMP2=VARTMP2 - PENIN4;
VARTMP1=VARTMP1+TAPR1FM;
TAPR1FZ = positif(VARTMP2) * arr(TABPR4*(PENIN4)/TEXPR4)
                 + (1-positif(VARTMP2)) * max(0,TABPR4-VARTMP1) ;
VARTMP2=VARTMP2 - PALI4;
VARTMP1=VARTMP1+TAPR1FZ;
TAPR1FO = positif(VARTMP2) * arr(TABPR4*(PALI4)/TEXPR4)
                 + (1-positif(VARTMP2)) * max(0,TABPR4-VARTMP1) ;
VARTMP2=VARTMP2 - CARPEP4;
VARTMP1=VARTMP1+TAPR1FO;
TAPRRFS = positif(VARTMP2) * arr(TABPR4*(CARPEP4)/TEXPR4)
                 + (1-positif(VARTMP2)) * max(0,TABPR4-VARTMP1) ;
VARTMP2=VARTMP2 - CODRFL;
VARTMP1=VARTMP1+TAPRRFS;
TAPRRFL = positif(VARTMP2) * arr(TABPR4*(CODRFL)/TEXPR4)
                 + (1-positif(VARTMP2)) * max(0,TABPR4-VARTMP1) ;
VARTMP2=VARTMP2 - CODRFM;
VARTMP1=VARTMP1+TAPRRFL;
TAPRRFM = positif(VARTMP2) * arr(TABPR4*(CODRFM)/TEXPR4)
                 + (1-positif(VARTMP2)) * max(0,TABPR4-VARTMP1) ;
VARTMP2=VARTMP2 - CODRFZ;
VARTMP1=VARTMP1+TAPRRFM;
TAPRRFZ = positif(VARTMP2) * arr(TABPR4*(CODRFZ)/TEXPR4)
                 + (1-positif(VARTMP2)) * max(0,TABPR4-VARTMP1) ;
VARTMP2=VARTMP2 - PENSALP4;
VARTMP1=VARTMP1+TAPRRFZ;
TAPRRFO = positif(VARTMP2) * arr(TABPR4*(PENSALP4)/TEXPR4)
                 + (1-positif(VARTMP2)) * max(0,TABPR4-VARTMP1) ;
VARTMP1=VARTMP1+TAPRRFO;
TAPR1FH = positif(PEBF4) * arr(TABPR4*(COD1FH)/TEXPR4)
                 + (1-positif(PEBF4)) * max(0,TABPR4-VARTMP1) ;
VARTMP1=VARTMP1+TAPR1FH;
TAPRFFS = max(0,TABPR4-VARTMP1);
TAPR4  =  TAPR1FS+TAPR1FL+TAPR1FM+TAPR1FZ+TAPR1FO+TAPRRFS+TAPRRFZ+TAPRRFO +TAPR1FH+TAPRFFS+TAPRRFL+TAPRRFM;
VARTMP2=0;
VARTMP1=0;
regle 99992110:
application : iliad   ;

TPRNN1AS = PRBRV - TAPR1AS;
TPRNN1AL = COD1AL - TAPR1AL;
TPRNN1AM = COD1AM - TAPR1AM;
TPRNN1AZ = PENINV - TAPR1AZ;
TPRNN1AO = PALIV - TAPR1AO;
TPRNNRAS = CARPEV - TAPRRAS;
TPRNNRAL = CODRAL - TAPRRAL;
TPRNNRAM = CODRAM - TAPRRAM;
TPRNNRAZ = CODRAZ - TAPRRAZ;
TPRNNRAO = PENSALV - TAPRRAO;
TPRNN1AH = COD1AH - TAPR1AH;
TPRNNFAS = PEBFV - TAPRFAS;
TPRNNV = TPRNN1AS+TPRNN1AL+TPRNN1AM+TPRNN1AZ+TPRNN1AO+TPRNNRAS+TPRNNRAZ+TPRNNRAO +TPRNN1AH+TPRNNFAS+TPRNNRAL+TPRNNRAM+COD1AI+CODRAI;

TPRNN1BS = PRBRC - TAPR1BS;
TPRNN1BL = COD1BL - TAPR1BL;
TPRNN1BM = COD1BM - TAPR1BM;
TPRNN1BZ = PENINC - TAPR1BZ;
TPRNN1BO = PALIC - TAPR1BO;
TPRNNRBS = CARPEC - TAPRRBS;
TPRNNRBL = CODRBL - TAPRRBL;
TPRNNRBM = CODRBM - TAPRRBM;
TPRNNRBZ = CODRBZ - TAPRRBZ;
TPRNNRBO = PENSALC - TAPRRBO;
TPRNN1BH = COD1BH - TAPR1BH;
TPRNNFBS = PEBFC - TAPRFBS;
TPRNNC = TPRNN1BS+TPRNN1BL+TPRNN1BM+TPRNN1BZ+TPRNN1BO+TPRNNRBS+TPRNNRBZ+TPRNNRBO+TPRNN1BH+TPRNNFBS+TPRNNRBL+TPRNNRBM+COD1BI+CODRBI;

TPRNN1CS = PRBR1 - TAPR1CS;
TPRNN1CL = COD1CL - TAPR1CL;
TPRNN1CM = COD1CM - TAPR1CM;
TPRNN1CZ = PENIN1 - TAPR1CZ;
TPRNN1CO = PALI1 - TAPR1CO;
TPRNNRCS = CARPEP1 - TAPRRCS;
TPRNNRCL = CODRCL - TAPRRCL;
TPRNNRCM = CODRCM - TAPRRCM;
TPRNNRCZ = CODRCZ - TAPRRCZ;
TPRNNRCO = PENSALP1 - TAPRRCO;
TPRNN1CH = COD1CH - TAPR1CH;
TPRNNFCS = PEBF1 - TAPRFCS;
TPRNN1 = TPRNN1CS + TPRNN1CL + TPRNN1CM + TPRNN1CZ + TPRNN1CO + COD1CI + TPRNNRCS + TPRNNRCZ + TPRNNRCO + TPRNN1CH + TPRNNFCS + TPRNNRCL + TPRNNRCM +CODRCK;

TPRNN1DS = PRBR2 - TAPR1DS;
TPRNN1DL = COD1DL - TAPR1DL;
TPRNN1DM = COD1DM - TAPR1DM;
TPRNN1DZ = PENIN2 - TAPR1DZ;
TPRNN1DO = PALI2 - TAPR1DO;
TPRNNRDS = CARPEP2 - TAPRRDS;
TPRNNRDL = CODRDL - TAPRRDL;
TPRNNRDM = CODRDM - TAPRRDM;
TPRNNRDZ = CODRDZ - TAPRRDZ;
TPRNNRDO = PENSALP2 - TAPRRDO;
TPRNN1DH = COD1DH - TAPR1DH;
TPRNNFDS = PEBF2 - TAPRFDS;
TPRNN2 = TPRNN1DS + TPRNN1DL + TPRNN1DM + TPRNN1DZ + TPRNN1DO + COD1DI +TPRNNRDS + TPRNNRDZ + TPRNNRDO + TPRNN1DH + TPRNNFDS + TPRNNRDL + TPRNNRDM ;

TPRNN1ES = PRBR3 - TAPR1ES;
TPRNN1EL = COD1EL - TAPR1EL;
TPRNN1EM = COD1EM - TAPR1EM;
TPRNN1EZ = PENIN3 - TAPR1EZ;
TPRNN1EO = PALI3 - TAPR1EO;
TPRNNRES = CARPEP3 - TAPRRES;
TPRNNREL = CODREL - TAPRREL;
TPRNNREM = CODREM - TAPRREM;
TPRNNREZ = CODREZ - TAPRREZ;
TPRNNREO = PENSALP3 - TAPRREO;
TPRNN1EH = COD1EH - TAPR1EH;
TPRNNFES = PEBF3 - TAPRFES;
TPRNN3 = TPRNN1ES + TPRNN1EL + TPRNN1EM + TPRNN1EZ + TPRNN1EO + COD1EI + TPRNNRES + TPRNNREZ + TPRNNREO + TPRNN1EH + TPRNNFES + TPRNNREL + TPRNNREM ;

TPRNN1FS = PRBR4 - TAPR1FS;
TPRNN1FL = COD1FL - TAPR1FL;
TPRNN1FM = COD1FM - TAPR1FM;
TPRNN1FZ = PENIN4 - TAPR1FZ;
TPRNN1FO = PALI4 - TAPR1FO;
TPRNNRFS = CARPEP4 - TAPRRFS;
TPRNNRFL = CODRFL - TAPRRFL;
TPRNNRFM = CODRFM - TAPRRFM;
TPRNNRFZ = CODRFZ - TAPRRFZ;
TPRNNRFO = PENSALP4 - TAPRRFO;
TPRNN1FH = COD1FH - TAPR1FH;
TPRNNFFS = PEBF4 - TAPRFFS;
TPRNN4 = TPRNN1FS + TPRNN1FL + TPRNN1FM + TPRNN1FZ + TPRNN1FO + COD1FI + TPRNNRFS + TPRNNRFZ + TPRNNRFO + TPRNN1FH + TPRNNFFS + TPRNNRFL + TPRNNRFM ;
TPRNNP = TPRNN1 + TPRNN2 + TPRNN3 + TPRNN4 ;
regle 99992120:
application : iliad   ;
TTSNTV =  TTSN1AJ+TTSN1PM+TTSN1TP+TTSN1NX+TTSN1AF+TTSN1AG+TTSN1AC+TTSN1AP+TTSN3VJ+TTSN1TT+TTSNRAJ+TTSNRAP
        +TTSNDAJ+TTSNEAJ+ TTSN1GB+ TTSN1AA+TTSN1GF+TTSN1GG+TTSN1GH+TTSN1AD+TTSNRAF+TTSNRAG ;
TTSNTC = TTSN1BJ+TTSN1QM+TTSN1UP+TTSN1OX+TTSN1BF+TTSN1BG+TTSN1BC+TTSN1BP+TTSN3VK+TTSN1UT+TTSNRBJ+TTSNRBP
       +TTSNDBJ+TTSNEBJ+ TTSN1HB+ TTSN1BA+TTSN1HF+TTSN1HG+TTSN1HH+TTSN1BD+TTSNRBF+TTSNRBG;
TTSNT1 = TTSN1CJ + TTSN1CF + TTSN1CG + TTSN1CC + TTSN1CP + TTSNRCJ + TTSNRCP + TTSN1IB + TTSN1CA + TTSN1IF +TTSN1IG+TTSN1IH+TTSN1CD+ TTSNRCF + TTSNRCG ;
TTSNT2 = TTSN1DJ + TTSN1DF + TTSN1DG + TTSN1DC + TTSN1DP + TTSNRDJ + TTSNRDP + TTSN1JB + TTSN1DA + TTSN1JF +TTSN1JG+TTSN1JH+TTSN1DD+ TTSNRDF + TTSNRDG ;
TTSNT3 = TTSN1EJ + TTSN1EF + TTSN1EG + TTSN1EC + TTSN1EP + TTSNREJ + TTSNREP + TTSN1EA + TTSN1KF + TTSNREF +TTSN1KG+TTSN1KH+TTSN1ED+ TTSNRGG ;
TTSNT4 = TTSN1FJ + TTSN1FF + TTSN1FG + TTSN1FC + TTSN1FP + TTSNRFJ + TTSNRFP + TTSN1FA + TTSN1LF + TTSNRFF +TTSN1LG+TTSN1LH+TTSN1FD+ TTSNRFG ;
regle 99992130:
application : iliad   ;
TTSNV = positif (-TTSNTV) * min (0 , TTSNTV + TPRNNV) + positif_ou_nul (TTSNTV) * TTSNTV;
TTSNC = positif (-TTSNTC) * min (0 , TTSNTC + TPRNNC) + positif_ou_nul (TTSNTC) * TTSNTC;
TTSN1 = positif (-TTSNT1) * min (0 , TTSNT1 + TPRNN1) + positif_ou_nul (TTSNT1) * TTSNT1;
TTSN2 = positif (-TTSNT2) * min (0 , TTSNT2 + TPRNN2) + positif_ou_nul (TTSNT2) * TTSNT2;
TTSN3 = positif (-TTSNT3) * min (0 , TTSNT3 + TPRNN3) + positif_ou_nul (TTSNT3) * TTSNT3;
TTSN4 = positif (-TTSNT4) * min (0 , TTSNT4 + TPRNN4) + positif_ou_nul (TTSNT4) * TTSNT4;

VARTMP2 = 0 ;
VARTMP1 = 0 ;
VARTMP2 = COD1AL + COD1AM + PENINV + PALIV + CARPEV + CODRAZ + PENSALV + PEBFV + CODRAL + CODRAM + COD1AH + COD1AI + CODRAI;

TPRNDEF1AS = positif (-TTSNTV) * (positif(VARTMP2)* arr(-TTSNTV*TPRNN1AS / TPRNNV)
                              + (1-positif(VARTMP2)) * (-TTSNTV))  ;
VARTMP2=VARTMP2-COD1AL;
TPRNDEF1AL = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNN1AL / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-TPRNDEF1AS))  ;
VARTMP2=VARTMP2-COD1AM;
VARTMP1=TPRNDEF1AS+TPRNDEF1AL;
TPRNDEF1AM = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNN1AM / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-PENINV;
VARTMP1=VARTMP1+TPRNDEF1AM;
TPRNDEF1AZ = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNN1AZ / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-PALIV;
VARTMP1=VARTMP1+TPRNDEF1AZ;
TPRNDEF1AO = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNN1AO / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-CARPEV;
VARTMP1=VARTMP1+TPRNDEF1AO;
TPRNDEFRAS = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNNRAS / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-CODRAL;
VARTMP1=VARTMP1+TPRNDEFRAS;
TPRNDEFRAL = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNNRAL / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-CODRAM;
VARTMP1=VARTMP1+TPRNDEFRAL;
TPRNDEFRAM = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNNRAM / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-CODRAZ;
VARTMP1=VARTMP1+TPRNDEFRAM;
TPRNDEFRAZ = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNNRAZ / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-COD1AH;
VARTMP1=VARTMP1+TPRNDEFRAZ;
TPRNDEF1AH = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNN1AH / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-PENSALV;
VARTMP1=VARTMP1+TPRNDEF1AH;
TPRNDEFRAO = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*TPRNNRAO / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-COD1AI;
VARTMP1=VARTMP1+TPRNDEFRAO;
TPRNDEF1AI = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*COD1AI / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;
VARTMP2=VARTMP2-CODRAI;
VARTMP1=VARTMP1+TPRNDEF1AI;
TPRNDEFRAI = positif (-TTSNTV) * (positif(VARTMP2) * arr(-TTSNTV*CODRAI / TPRNNV)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTV-VARTMP1))  ;

VARTMP2=VARTMP2-PEBFV;
VARTMP1=VARTMP1+TPRNDEFRAI;
TPRNDEFFAS = positif (-TTSNTV) * max(0,-TTSNTV-VARTMP1)  ;

VARTMP2 = 0 ;
VARTMP1 = 0 ;
VARTMP2 = COD1BL + COD1BM + PENINC + PALIC + CARPEC + CODRBZ + PENSALC + COD1BH + PEBFC + CODRBL + CODRBM + COD1BI + CODRBI;

TPRNDEF1BS = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNN1BS / TPRNNC)
                              + (1-positif(VARTMP2)) * (-TTSNTC))  ;
VARTMP2=VARTMP2-COD1BL;
TPRNDEF1BL = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNN1BL / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-TPRNDEF1BS))  ;
VARTMP2=VARTMP2-COD1BM;
VARTMP1=TPRNDEF1BS+TPRNDEF1BL;
TPRNDEF1BM = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNN1BM / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-PENINC;
VARTMP1=VARTMP1+TPRNDEF1BM;
TPRNDEF1BZ = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNN1BZ / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-PALIC;
VARTMP1=VARTMP1+TPRNDEF1BZ;
TPRNDEF1BO = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNN1BO / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-CARPEC;
VARTMP1=VARTMP1+TPRNDEF1BO;
TPRNDEFRBS = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNNRBS / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-CODRBL;
VARTMP1=VARTMP1+TPRNDEFRBS;
TPRNDEFRBL = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNNRBL / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-CODRBM;
VARTMP1=VARTMP1+TPRNDEFRBL;
TPRNDEFRBM = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNNRBM / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-CODRBZ;
VARTMP1=VARTMP1+TPRNDEFRBM;
TPRNDEFRBZ = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNNRBZ / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-COD1BH;
VARTMP1=VARTMP1+TPRNDEFRBZ;
TPRNDEF1BH = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNN1BH / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-PENSALC;
VARTMP1=VARTMP1+TPRNDEF1BH;
TPRNDEFRBO = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*TPRNNRBO / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-COD1BI;
VARTMP1=VARTMP1+TPRNDEFRBO;
TPRNDEF1BI = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*COD1BI / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;
VARTMP2=VARTMP2-CODRBI;
VARTMP1=VARTMP1+TPRNDEF1BI;
TPRNDEFRBI = positif (-TTSNTC) * (positif(VARTMP2) * arr(-TTSNTC*CODRBI / TPRNNC)
                              + (1-positif(VARTMP2)) * max(0,-TTSNTC-VARTMP1))  ;

VARTMP1=VARTMP1+TPRNDEFRBI;
TPRNDEFFBS = positif (-TTSNTC) * max(0,-TTSNTC-VARTMP1)  ;

VARTMP2 = 0 ;
VARTMP1 = 0 ;
VARTMP2 = COD1CL + COD1CM + PENIN1 + PALI1 + COD1CI + CARPEP1 + CODRCZ + PENSALP1 + COD1CH + PEBF1 + CODRCL + CODRCM + CODRCK;

TPRNDEF1CS = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNN1CS / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1))  ;
VARTMP2=VARTMP2-COD1CL;
TPRNDEF1CL = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNN1CL / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1-TPRNDEF1CS))  ;
VARTMP2=VARTMP2-COD1CM;
VARTMP1=TPRNDEF1CS+TPRNDEF1CL;
TPRNDEF1CM = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNN1CM / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1-VARTMP1))  ;
VARTMP2=VARTMP2-PENIN1;
VARTMP1=VARTMP1+TPRNDEF1CM;
TPRNDEF1CZ = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNN1CZ / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1-VARTMP1))  ;
VARTMP2=VARTMP2-PALI1;
VARTMP1=VARTMP1+TPRNDEF1CZ;
TPRNDEF1CO = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNN1CO / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1-VARTMP1))  ;
VARTMP2 = VARTMP2 - COD1CI ;
VARTMP1 = VARTMP1 + TPRNDEF1CO ;
TPRNDEF1CI = positif(-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1 * COD1CI / TPRNN1)
                                 + (1 - positif(VARTMP2)) * (-TTSNT1 - VARTMP1)) ;
VARTMP2=VARTMP2-CARPEP1;
VARTMP1=VARTMP1+TPRNDEF1CI;
TPRNDEFRCS = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNNRCS / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1-VARTMP1))  ;
VARTMP2=VARTMP2-CODRCL;
VARTMP1=VARTMP1+TPRNDEFRCS;
TPRNDEFRCL = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNNRCL / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1-VARTMP1))  ;
VARTMP2=VARTMP2-CODRCM;
VARTMP1=VARTMP1+TPRNDEFRCL;
TPRNDEFRCM = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNNRCM / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1-VARTMP1))  ;
VARTMP2=VARTMP2-CODRCZ;
VARTMP1=VARTMP1+TPRNDEFRCM;
TPRNDEFRCZ = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNNRCZ / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1-VARTMP1))  ;
VARTMP2=VARTMP2-COD1CH;
VARTMP1=VARTMP1+TPRNDEFRCZ;
TPRNDEF1CH = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNN1CH / TPRNN1)
                              + (1-positif(VARTMP2)) * max(0,-TTSNT1-VARTMP1))  ;
VARTMP2=VARTMP2-PENSALP1;
VARTMP1=VARTMP1+TPRNDEF1CH;
TPRNDEFRCO = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*TPRNNRCO / TPRNN1)
                              + (1-positif(VARTMP2)) * (-TTSNT1-VARTMP1))  ;
VARTMP2=VARTMP2-CODRCK;
VARTMP1=VARTMP1+TPRNDEFRCO;
TPRNDEFRCK = positif (-TTSNT1) * (positif(VARTMP2) * arr(-TTSNT1*CODRCK / TPRNN1)
                              + (1-positif(VARTMP2)) * max(0,-TTSNT1-VARTMP1))  ;

VARTMP1=VARTMP1+TPRNDEFRCK;
TPRNDEFFCS = positif (-TTSNT1) * max(0,-TTSNT1-VARTMP1)  ;

VARTMP2=0;
VARTMP1=0;
VARTMP2=COD1DL+COD1DM+PENIN2+PALI2+CARPEP2+CODRDZ+PENSALP2+COD1DH+PEBF2+CODRDL+CODRDM;
TPRNDEF1DS = positif (-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2*TPRNN1DS / TPRNN2)
                              + (1-positif(VARTMP2)) * (-TTSNT2))  ;
VARTMP2=VARTMP2-COD1DL;
TPRNDEF1DL = positif (-TTSNT2) * (positif(VARTMP2) * arr(TTSNT2*TPRNN1DL / TPRNN2)
                              + (1-positif(VARTMP2)) * (TTSNT2-TPRNDEF1DS))  ;
VARTMP2=VARTMP2-COD1DM;
VARTMP1=TPRNDEF1DS+TPRNDEF1DL;
TPRNDEF1DM = positif (-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2*TPRNN1DM / TPRNN2)
                              + (1-positif(VARTMP2)) * (-TTSNT2-VARTMP1))  ;
VARTMP2=VARTMP2-PENIN2;
VARTMP1=VARTMP1+TPRNDEF1DM;
TPRNDEF1DZ = positif (-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2*TPRNN1DZ / TPRNN2)
                              + (1-positif(VARTMP2)) * (-TTSNT2-VARTMP1))  ;
VARTMP2=VARTMP2-PALI2;
VARTMP1=VARTMP1+TPRNDEF1DZ;
TPRNDEF1DO = positif (-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2*TPRNN1DO / TPRNN2)
                              + (1-positif(VARTMP2)) * (-TTSNT2-VARTMP1))  ;
VARTMP2 = VARTMP2 - COD1DI ;
VARTMP1 = VARTMP1 + TPRNDEF1DO ;
TPRNDEF1DI = positif(-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2 * COD1DI / TPRNN2)
                                 + (1 - positif(VARTMP2)) * (-TTSNT2 - VARTMP1)) ;
VARTMP2=VARTMP2-CARPEP2;
VARTMP1=VARTMP1+TPRNDEF1DI;
TPRNDEFRDS = positif (-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2*TPRNNRDS / TPRNN2)
                              + (1-positif(VARTMP2)) * (-TTSNT2-VARTMP1))  ;
VARTMP2=VARTMP2-CODRDL;
VARTMP1=VARTMP1+TPRNDEFRDS;
TPRNDEFRDL = positif (-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2*TPRNNRDL / TPRNN2)
                              + (1-positif(VARTMP2)) * (-TTSNT2-VARTMP1))  ;
VARTMP2=VARTMP2-CODRDM;
VARTMP1=VARTMP1+TPRNDEFRDL;
TPRNDEFRDM = positif (-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2*TPRNNRDM / TPRNN2)
                              + (1-positif(VARTMP2)) * (-TTSNT2-VARTMP1))  ;
VARTMP2=VARTMP2-CODRDZ;
VARTMP1=VARTMP1+TPRNDEFRDM;
TPRNDEFRDZ = positif (-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2*TPRNNRDZ / TPRNN2)
                              + (1-positif(VARTMP2)) * (-TTSNT2-VARTMP1))  ;
VARTMP2=VARTMP2-COD1DH;
VARTMP1=VARTMP1+TPRNDEFRDZ;
TPRNDEF1DH = positif (-TTSNT2) * (positif(VARTMP2) * arr(-TTSNT2*TPRNN1DH / TPRNN2)
                              + (1-positif(VARTMP2)) * max(0,-TTSNT2-VARTMP1))  ;
VARTMP1=VARTMP1+TPRNDEF1DH;
TPRNDEFRDO = positif (-TTSNT2) * (positif(PEBF2) * arr(-TTSNT2*TPRNNRDO / TPRNN2)
                              + (1-positif(PEBF2)) * (-TTSNT2-VARTMP1))  ;
VARTMP1=VARTMP1+TPRNDEFRDO;
TPRNDEFFDS = positif (-TTSNT2) * max(0,-TTSNT2-VARTMP1)  ;

VARTMP2=0;
VARTMP1=0;
VARTMP2=COD1EL+COD1EM+PENIN3+PALI3+CARPEP3+CODREZ+PENSALP3+COD1EH+PEBF3+CODREL+CODREM;
TPRNDEF1ES = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNN1ES / TPRNN3)
                              + (1-positif(VARTMP2)) * (-TTSNT3))  ;
VARTMP2=VARTMP2-COD1EL;
TPRNDEF1EL = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNN1EL / TPRNN3)
                              + (1-positif(VARTMP2)) * (-TTSNT3-TPRNDEF1ES))  ;
VARTMP2=VARTMP2-COD1EM;
VARTMP1=TPRNDEF1ES+TPRNDEF1EL;
TPRNDEF1EM = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNN1EM / TPRNN3)
                              + (1-positif(VARTMP2)) * (-TTSNT3-VARTMP1))  ;
VARTMP2=VARTMP2-PENIN3;
VARTMP1=VARTMP1+TPRNDEF1EM;
TPRNDEF1EZ = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNN1EZ / TPRNN3)
                              + (1-positif(VARTMP2)) * (-TTSNT3-VARTMP1))  ;
VARTMP2=VARTMP2-PALI3;
VARTMP1=VARTMP1+TPRNDEF1EZ;
TPRNDEF1EO = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNN1EO / TPRNN3)
                              + (1-positif(VARTMP2)) * (-TTSNT3-VARTMP1))  ;
VARTMP2 = VARTMP2 - COD1EI ;
VARTMP1 = VARTMP1 + TPRNDEF1EO ;
TPRNDEF1EI = positif(-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3 * COD1EI / TPRNN3)
                                 + (1 - positif(VARTMP2)) * (-TTSNT3 - VARTMP1)) ;
VARTMP2=VARTMP2-CARPEP3;
VARTMP1=VARTMP1+TPRNDEF1EI;
TPRNDEFRES = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNNRES / TPRNN3)
                              + (1-positif(VARTMP2)) * (-TTSNT3-VARTMP1))  ;
VARTMP2=VARTMP2-CODREL;
VARTMP1=VARTMP1+TPRNDEFRES;
TPRNDEFREL = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNNREL / TPRNN3)
                              + (1-positif(VARTMP2)) * (-TTSNT3-VARTMP1))  ;
VARTMP2=VARTMP2-CODREM;
VARTMP1=VARTMP1+TPRNDEFREL;
TPRNDEFREM = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNNREM / TPRNN3)
                              + (1-positif(VARTMP2)) * (-TTSNT3-VARTMP1))  ;
VARTMP2=VARTMP2-CODREZ;
VARTMP1=VARTMP1+TPRNDEFREM;
TPRNDEFREZ = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNNREZ / TPRNN3)
                              + (1-positif(VARTMP2)) * (-TTSNT3-VARTMP1))  ;
VARTMP2=VARTMP2-COD1EH;
VARTMP1=VARTMP1+TPRNDEFREZ;
TPRNDEF1EH = positif (-TTSNT3) * (positif(VARTMP2) * arr(-TTSNT3*TPRNN1EH / TPRNN3)
                              + (1-positif(VARTMP2)) * max(0,-TTSNT3-VARTMP1))  ;
VARTMP1=VARTMP1+TPRNDEF1EH;
TPRNDEFREO = positif (-TTSNT3) * (positif(PEBF3) * arr(-TTSNT3*TPRNNREO / TPRNN3)
                              + (1-positif(PEBF3)) * (-TTSNT3-VARTMP1))  ;
VARTMP2=VARTMP2-PEBF3;
VARTMP1=VARTMP1+TPRNDEFREO;
TPRNDEFFES = positif (-TTSNT3) * max(0,-TTSNT3-VARTMP1)  ;

VARTMP2=0;
VARTMP1=0;
VARTMP2=COD1FL+COD1FM+PENIN4+PALI4+CARPEP4+CODRFZ+PENSALP4+COD1FH+PEBF4+CODRFL+CODRFM;
TPRNDEF1FS = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNN1FS / TPRNN4)
                              + (1-positif(VARTMP2)) * (-TTSNT4))  ;
VARTMP2=VARTMP2-COD1FL;
TPRNDEF1FL = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNN1FL / TPRNN4)
                              + (1-positif(VARTMP2)) * (-TTSNT4-TPRNDEF1FS))  ;
VARTMP2=VARTMP2-COD1FM;
VARTMP1=TPRNDEF1FS+TPRNDEF1FL;
TPRNDEF1FM = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNN1FM / TPRNN4)
                              + (1-positif(VARTMP2)) * (-TTSNT4-VARTMP1))  ;
VARTMP2=VARTMP2-PENIN4;
VARTMP1=VARTMP1+TPRNDEF1FM;
TPRNDEF1FZ = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNN1FZ / TPRNN4)
                              + (1-positif(VARTMP2)) * (-TTSNT4-VARTMP1))  ;
VARTMP2=VARTMP2-PALI4;
VARTMP1=VARTMP1+TPRNDEF1FZ;
TPRNDEF1FO = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNN1FO / TPRNN4)
                              + (1-positif(VARTMP2)) * (-TTSNT4-VARTMP1))  ;
VARTMP2 = VARTMP2 - COD1FI ;
VARTMP1 = VARTMP1 + TPRNDEF1FO ;
TPRNDEF1FI = positif(-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4 * COD1FI / TPRNN4)
                                 + (1 - positif(VARTMP2)) * (-TTSNT4 - VARTMP1)) ;
VARTMP2=VARTMP2-CARPEP4;
VARTMP1=VARTMP1+TPRNDEF1FI;
TPRNDEFRFS = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNNRFS / TPRNN4)
                              + (1-positif(VARTMP2)) * (-TTSNT4-VARTMP1))  ;
VARTMP2=VARTMP2-CODRFL;
VARTMP1=VARTMP1+TPRNDEFRFS;
TPRNDEFRFL = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNNRFL / TPRNN4)
                              + (1-positif(VARTMP2)) * (-TTSNT4-VARTMP1))  ;
VARTMP2=VARTMP2-CODRFM;
VARTMP1=VARTMP1+TPRNDEFRFL;
TPRNDEFRFM = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNNRFM / TPRNN4)
                              + (1-positif(VARTMP2)) * (-TTSNT4-VARTMP1))  ;
VARTMP2=VARTMP2-CODRFZ;
VARTMP1=VARTMP1+TPRNDEFRFM;
TPRNDEFRFZ = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNNRFZ / TPRNN4)
                              + (1-positif(VARTMP2)) * (-TTSNT4-VARTMP1))  ;
VARTMP2=VARTMP2-COD1FH;
VARTMP1=VARTMP1+TPRNDEFRFZ;
TPRNDEF1FH = positif (-TTSNT4) * (positif(VARTMP2) * arr(-TTSNT4*TPRNN1FH / TPRNN4)
                              + (1-positif(VARTMP2)) * max(0,-TTSNT4-VARTMP1))  ;
VARTMP2=VARTMP2-PENSALP4;
VARTMP1=VARTMP1+TPRNDEF1FH;
TPRNDEFRFO = positif (-TTSNT4) * (positif(PEBF4) * arr(-TTSNT4*TPRNNRFO / TPRNN4)
                              + (1-positif(PEBF4)) * (-TTSNT4-VARTMP1))  ;
VARTMP2=VARTMP2-PEBF4;
VARTMP1=VARTMP1+TPRNDEFRFO;
TPRNDEFFFS = positif (-TTSNT4) * max(0,-TTSNT4-VARTMP1)  ;


TPRN1AS = TPRNN1AS - TPRNDEF1AS;
TPRN1AL = TPRNN1AL - TPRNDEF1AL;
TPRN1AM = TPRNN1AM - TPRNDEF1AM;
TPRN1AZ = TPRNN1AZ - TPRNDEF1AZ;
TPRN1AO = TPRNN1AO - TPRNDEF1AO;
TPRNRAS = TPRNNRAS - TPRNDEFRAS;
TPRNRAL = TPRNNRAL - TPRNDEFRAL;
TPRNRAM = TPRNNRAM - TPRNDEFRAM;
TPRNRAZ = TPRNNRAZ - TPRNDEFRAZ;
TPRNRAO = TPRNNRAO - TPRNDEFRAO;
TPRN1AH = TPRNN1AH - TPRNDEF1AH;
TPRNFAS = TPRNNFAS - TPRNDEFFAS;
TPRN1AI = max(0,COD1AI - TPRNDEF1AI);
TPRNRAI = max(0,CODRAI - TPRNDEFRAI);
TPRNV = max(0,TPRN1AS+TPRN1AL+TPRN1AM+TPRN1AZ+TPRN1AO+TPRNRAS+TPRNRAZ+TPRNRAO+TPRN1AH+TPRNFAS+TPRNRAL+TPRNRAM+TPRN1AI);

TPRN1BS = TPRNN1BS - TPRNDEF1BS;
TPRN1BL = TPRNN1BL - TPRNDEF1BL;
TPRN1BM = TPRNN1BM - TPRNDEF1BM;
TPRN1BZ = TPRNN1BZ - TPRNDEF1BZ;
TPRN1BO = TPRNN1BO - TPRNDEF1BO;
TPRNRBS = TPRNNRBS - TPRNDEFRBS;
TPRNRBL = TPRNNRBL - TPRNDEFRBL;
TPRNRBM = TPRNNRBM - TPRNDEFRBM;
TPRNRBZ = TPRNNRBZ - TPRNDEFRBZ;
TPRNRBO = TPRNNRBO - TPRNDEFRBO;
TPRN1BH = TPRNN1BH - TPRNDEF1BH;
TPRNFBS = TPRNNFBS - TPRNDEFFBS;
TPRN1BI = max(0,COD1BI - TPRNDEF1BI);
TPRNRBI = max(0,CODRBI - TPRNDEFRBI);
TPRNC = max(0,TPRN1BS+TPRN1BL+TPRN1BM+TPRN1BZ+TPRN1BO+TPRNRBS+TPRNRBZ+TPRNRBO+TPRN1BH+TPRNFBS+TPRNRBL+TPRNRBM+TPRN1BI);

TPRN1CS = TPRNN1CS - TPRNDEF1CS;
TPRN1CL = TPRNN1CL - TPRNDEF1CL;
TPRN1CM = TPRNN1CM - TPRNDEF1CM;
TPRN1CZ = TPRNN1CZ - TPRNDEF1CZ;
TPRN1CO = TPRNN1CO - TPRNDEF1CO;
TPRN1CI = max(0,COD1CI - TPRNDEF1CI) ;
TPRNRCS = TPRNNRCS - TPRNDEFRCS;
TPRNRCL = TPRNNRCL - TPRNDEFRCL;
TPRNRCM = TPRNNRCM - TPRNDEFRCM;
TPRNRCZ = TPRNNRCZ - TPRNDEFRCZ;
TPRNRCO = TPRNNRCO - TPRNDEFRCO;
TPRN1CH = TPRNN1CH - TPRNDEF1CH;
TPRNFCS = TPRNNFCS - TPRNDEFFCS;
TPRNRCK = max(0,CODRCK - TPRNDEFRCK);
TPRN1 = max(0 , TPRN1CS + TPRN1CL + TPRN1CM + TPRN1CZ + TPRN1CO + TPRN1CI + TPRNRCS + TPRNRCZ + TPRNRCO + TPRN1CH + TPRNFCS + TPRNRCL + TPRNRCM) ;

TPRN1DS = TPRNN1DS - TPRNDEF1DS;
TPRN1DL = TPRNN1DL - TPRNDEF1DL;
TPRN1DM = TPRNN1DM - TPRNDEF1DM;
TPRN1DZ = TPRNN1DZ - TPRNDEF1DZ;
TPRN1DO = TPRNN1DO - TPRNDEF1DO;
TPRN1DI = COD1DI - TPRNDEF1DI ;
TPRNRDS = TPRNNRDS - TPRNDEFRDS;
TPRNRDL = TPRNNRDL - TPRNDEFRDL;
TPRNRDM = TPRNNRDM - TPRNDEFRDM;
TPRNRDZ = TPRNNRDZ - TPRNDEFRDZ;
TPRNRDO = TPRNNRDO - TPRNDEFRDO;
TPRN1DH = TPRNN1DH - TPRNDEF1DH;
TPRNFDS = TPRNNFDS - TPRNDEFFDS;
TPRN2 = max(0 , TPRN1DS + TPRN1DL + TPRN1DM + TPRN1DZ + TPRN1DO + TPRN1DI + TPRNRDS + TPRNRDZ + TPRNRDO + TPRN1DH + TPRNFDS + TPRNRDL + TPRNRDM) ;

TPRN1ES = TPRNN1ES - TPRNDEF1ES;
TPRN1EL = TPRNN1EL - TPRNDEF1EL;
TPRN1EM = TPRNN1EM - TPRNDEF1EM;
TPRN1EZ = TPRNN1EZ - TPRNDEF1EZ;
TPRN1EO = TPRNN1EO - TPRNDEF1EO;
TPRN1EI = COD1EI - TPRNDEF1EI ;
TPRNRES = TPRNNRES - TPRNDEFRES;
TPRNREL = TPRNNREL - TPRNDEFREL;
TPRNREM = TPRNNREM - TPRNDEFREM;
TPRNREZ = TPRNNREZ - TPRNDEFREZ;
TPRNREO = TPRNNREO - TPRNDEFREO;
TPRN1EH = TPRNN1EH - TPRNDEF1EH;
TPRNFES = TPRNNFES - TPRNDEFFES;
TPRN3 = max(0 , TPRN1ES + TPRN1EL + TPRN1EM + TPRN1EZ + TPRN1EO + TPRN1EI + TPRNRES + TPRNREZ + TPRNREO + TPRN1EH + TPRNFES + TPRNREL + TPRNREM) ;

TPRN1FS = TPRNN1FS - TPRNDEF1FS;
TPRN1FL = TPRNN1FL - TPRNDEF1FL;
TPRN1FM = TPRNN1FM - TPRNDEF1FM;
TPRN1FZ = TPRNN1FZ - TPRNDEF1FZ;
TPRN1FO = TPRNN1FO - TPRNDEF1FO;
TPRN1FI = COD1FI - TPRNDEF1FI ;
TPRNRFS = TPRNNRFS - TPRNDEFRFS;
TPRNRFL = TPRNNRFL - TPRNDEFRFL;
TPRNRFM = TPRNNRFM - TPRNDEFRFM;
TPRNRFZ = TPRNNRFZ - TPRNDEFRFZ;
TPRNRFO = TPRNNRFO - TPRNDEFRFO;
TPRN1FH = TPRNN1FH - TPRNDEF1FH;
TPRNFFS = TPRNNFFS - TPRNDEFFFS;
TPRN4 = max(0 , TPRN1FS + TPRN1FL + TPRN1FM + TPRN1FZ + TPRN1FO + TPRN1FI + TPRNRFS + TPRNRFZ + TPRNRFO + TPRN1FH + TPRNFFS + TPRNRFL + TPRNRFM) ;

VARTMP1=0;
VARTMP2=0;
regle 99992210:
application : iliad   ;
TGLDOMAVDAJV = max (CODDAJ - TABDOMDAJ,0);
TGLDOMAVEAJV = max (CODEAJ - TABDOMEAJ,0);
TGLDOMAVDBJC = max (CODDBJ - TABDOMDBJ,0);
TGLDOMAVEBJC = max (CODEBJ - TABDOMEBJ,0);
TGLN4V = max(CODDAJ - TABDOMDAJ,0)+max(CODEAJ - TABDOMEAJ,0);
TGLN4C = max(CODDBJ - TABDOMDBJ,0)+max(CODEBJ - TABDOMEBJ,0);
regle 99992230:
application : iliad   ;
TTSV = TTSNV - max(CODDAJ - TABDOMDAJ,0)-max(CODEAJ - TABDOMEAJ,0);
TTSC = TTSNC - max(CODDBJ - TABDOMDBJ,0)-max(CODEBJ - TABDOMEBJ,0);
TTS1 = TTSN1;
TTS2 = TTSN2;
TTS3 = TTSN3;
TTS4 = TTSN4;
TTPRV = TTSNV + TPRNV - TGLN3V;
TTPRC = TTSNC + TPRNC - TGLN3C;
TTPR1 = TTSN2 + TPRN1;
TTPR2 = TTSN2 + TPRN2;
TTPR3 = TTSN3 + TPRN3;
TTPR4 = TTSN4 + TPRN4;
TTSNNV =  positif(TTSV) *arr(TTSV *(TTSBNV + BPCOSAV + GLDGRATV)/TEXTSV )
          + (1 -positif(TTSV)) * TTSV ;
TTSNNC =  positif(TTSC) *arr(TTSC *(TTSBNC + BPCOSAC + GLDGRATC)/TEXTSC )
          + (1 -positif(TTSC)) * TTSC ;
TTSNN1 = (positif(TTS1) * arr(TTS1 * TTSBN1 /TEXTS1 ) + (1 -positif(TTS1)) * TTS1)  ;
TTSNN2 = (positif(TTS2) * arr(TTS2 * TTSBN2 /TEXTS2 ) + (1 -positif(TTS2)) * TTS2)  ;
TTSNN3 = (positif(TTS3) * arr(TTS3 * TTSBN3 /TEXTS3 ) + (1 -positif(TTS3)) * TTS3)  ;
TTSNN4 = (positif(TTS4) * arr(TTS4 * TTSBN4 /TEXTS4 ) + (1 -positif(TTS4)) * TTS4)  ;
TTSNN2V = ( positif(TTSV)
                * ( positif(CARTSV+REMPLAV+CODRAF+CODRAG) 
                          * arr(TTSV * T2TSNV / TEXTSV )
                    + (1 -positif(CARTSV+REMPLAV+CODRAF+CODRAG)) 
                          * (TTSV - TTSNNV))) ;
TTSNN2C = ( positif(TTSC)
                * ( positif(CARTSC+REMPLAC+CODRBF+CODRBG) 
                          * arr(TTSC * T2TSNC / TEXTSC )
                    + (1 -positif(CARTSC+REMPLAC+CODRBF+CODRBG)) 
                          * (TTSC - TTSNNC))) ;
TTSNN21 = ( positif(TTS1)
               * ( positif(CARTSP1+REMPLAP1+CODRCF+CODRCG) 
                          * arr(TTS1 * T2TSN1 /TEXTS1 )
                    + (1 -positif(CARTSP1+REMPLAP1+CODRCF+CODRCG)) 
                          * (TTS1 - TTSNN1))) ;
TTSNN22 = ( positif(TTS2)
               * ( positif(CARTSP2+REMPLAP2+CODRDF+CODRDG) 
                          * arr(TTS2 * T2TSN2 /TEXTS2 )
                    + (1 -positif(CARTSP2+REMPLAP2+CODRDF+CODRDG)) 
                          * (TTS2 - TTSNN2))) ;
TTSNN23 = ( positif(TTS3)
               * ( positif(CARTSP3+REMPLAP3+CODREF+CODRGG) 
                          * arr(TTS3 * T2TSN3 /TEXTS3 )
                    + (1 -positif(CARTSP3+REMPLAP3+CODREF+CODRGG)) 
                          * (TTS3 - TTSNN3))) ;
TTSNN24 = ( positif(TTS4)
               * ( positif(CARTSP4+REMPLAP4+CODRFF+CODRFG) 
                          * arr(TTS4 * T2TSN4 /TEXTS4 )
                    + (1 -positif(CARTSP4+REMPLAP4+CODRFF+CODRFG)) 
                          * (TTS4 - TTSNN4))) ;
TTSNN2TSV = ( positif(TTSV) * ( positif(REMPLAV+CODRAF+CODRAG) * arr(TTSV * CARTSV / TEXTSV )
                    + (1 -positif(REMPLAV+CODRAF+CODRAG)) * (TTSV - TTSNNV))) ;
TTSNN2TSC = ( positif(TTSC) * ( positif(REMPLAC+CODRBF+CODRBG) * arr(TTSC * CARTSC / TEXTSC )
                    + (1 -positif(REMPLAC+CODRBF+CODRBG)) * (TTSC - TTSNNC))) ;
TTSNN2TS1 = ( positif(TTS1) * ( positif(REMPLAP1+CODRCF+CODRCG) * arr(TTS1 * CARTSP1 /TEXTS1 )
                    + (1 -positif(REMPLAP1+CODRCF+CODRCG)) * (TTS1 - TTSNN1))) ;
TTSNN2TS2 = ( positif(TTS2) * ( positif(REMPLAP2+CODRDF+CODRDG) * arr(TTS2 * CARTSP2 /TEXTS2 )
                    + (1 -positif(REMPLAP2+CODRDF+CODRDG)) * (TTS2 - TTSNN2))) ;
TTSNN2TS3 = ( positif(TTS3) * ( positif(REMPLAP3+CODREF+CODRGG) * arr(TTS3 * CARTSP3 /TEXTS3 )
                    + (1 -positif(REMPLAP3+CODREF+CODRGG)) * (TTS3 - TTSNN3))) ;
TTSNN2TS4 = ( positif(TTS4) * ( positif(REMPLAP4+CODRFF+CODRFG) * arr(TTS4 * CARTSP4 /TEXTS4 )
                    + (1 -positif(REMPLAP4+CODRFF+CODRFG)) * (TTS4 - TTSNN4))) ;
TTSNN2RAF =  positif(TTSV) * ( positif(REMPLAV+CODRAG) * arr(TTSV * CODRAF / TEXTSV )
                    + (1 -positif(REMPLAV+CODRAG)) * (TTSV - TTSNNV-TTSNN2TSV)) ;
TTSNN2RBF =  positif(TTSC) * ( positif(REMPLAC+CODRBG) * arr(TTSC * CODRBF / TEXTSC )
                    + (1 -positif(REMPLAC+CODRBG)) * (TTSC - TTSNNC-TTSNN2TSC)) ;
TTSNN2RCF =  positif(TTS1) * ( positif(REMPLAP1+CODRCG) * arr(TTS1 * CODRCF /TEXTS1 )
                    + (1 -positif(REMPLAP1+CODRCG)) * (TTS1 - TTSNN1-TTSNN2TS1)) ;
TTSNN2RDF =  positif(TTS2) * ( positif(REMPLAP2+CODRDG) * arr(TTS2 * CODRDF /TEXTS2 )
                    + (1 -positif(REMPLAP2+CODRDG)) * (TTS2 - TTSNN2-TTSNN2TS2)) ;
TTSNN2REF =  positif(TTS3) * ( positif(REMPLAP3+CODRGG) * arr(TTS3 * CODREF /TEXTS3 )
                    + (1 -positif(REMPLAP3+CODRGG)) * (TTS3 - TTSNN3-TTSNN2TS3)) ;
TTSNN2RFF =  positif(TTS4) * ( positif(REMPLAP4+CODRFG) * arr(TTS4 * CODRFF /TEXTS4 )
                    + (1 -positif(REMPLAP4+CODRFG)) * (TTS4 - TTSNN4-TTSNN2TS4)) ;

TTSNN2RAG =  positif(TTSV) * ( positif(REMPLAV) * arr(TTSV * CODRAG / TEXTSV )
                    + (1 -positif(REMPLAV)) * (TTSV - TTSNNV-TTSNN2TSV-TTSNN2RAF)) ;
TTSNN2RBG =  positif(TTSC) * ( positif(REMPLAC) * arr(TTSC * CODRBG / TEXTSC )
                    + (1 -positif(REMPLAC)) * (TTSC - TTSNNC-TTSNN2TSC-TTSNN2RBF)) ;
TTSNN2RCG =  positif(TTS1) * ( positif(REMPLAP1) * arr(TTS1 * CODRCG /TEXTS1 )
                    + (1 -positif(REMPLAP1)) * (TTS1 - TTSNN1-TTSNN2TS1-TTSNN2RCF)) ;
TTSNN2RDG =  positif(TTS2) * ( positif(REMPLAP2) * arr(TTS2 * CODRDG /TEXTS2 )
                    + (1 -positif(REMPLAP2)) * (TTS2 - TTSNN2-TTSNN2TS2-TTSNN2RDF)) ;
TTSNN2RGG =  positif(TTS3) * ( positif(REMPLAP3) * arr(TTS3 * CODRGG /TEXTS3 )
                    + (1 -positif(REMPLAP3)) * (TTS3 - TTSNN3-TTSNN2TS3-TTSNN2REF)) ;
TTSNN2RFG =  positif(TTS4) * ( positif(REMPLAP4) * arr(TTS4 * CODRFG /TEXTS4 )
                    + (1 -positif(REMPLAP4)) * (TTS4 - TTSNN4-TTSNN2TS4-TTSNN2RFF)) ;
TTSNN2REMPV = (positif(TTSV) * (TTSV - TTSNNV-TTSNN2TSV-TTSNN2RAF-TTSNN2RAG)) ;
TTSNN2REMPC = (positif(TTSC) * (TTSC - TTSNNC-TTSNN2TSC-TTSNN2RBF-TTSNN2RBG)) ;
TTSNN2REMP1 = (positif(TTS1) * (TTS1 - TTSNN1-TTSNN2TS1-TTSNN2RCF-TTSNN2RCG)) ;
TTSNN2REMP2 = (positif(TTS2) * (TTS2 - TTSNN2-TTSNN2TS2-TTSNN2RDF-TTSNN2RDG)) ;
TTSNN2REMP3 = (positif(TTS3) * (TTS3 - TTSNN3-TTSNN2TS3-TTSNN2REF-TTSNN2RGG)) ;
TTSNN2REMP4 = (positif(TTS4) * (TTS4 - TTSNN4-TTSNN2TS4-TTSNN2RFF-TTSNN2RFG)) ;

regle 99992240:
application : iliad   ;

TPRRV = arr(TPRNV * (PRBV + COD1AI) / (TEXPRV + COD1AI)) +  arr(TPRNV * COD1AH / (TEXPRV + COD1AI)) ;
TPRRC = arr(TPRNC * (PRBC + COD1BI) / (TEXPRC + COD1BI)) +  arr(TPRNC * COD1BH / (TEXPRC + COD1BI)) ;
TPRR1 = arr(TPRN1 * (PRB1 + COD1CI) / (TEXPR1 + COD1CI)) +  arr(TPRN1 * COD1CH / (TEXPR1 + COD1CI)) ;
TPRR2 = arr(TPRN2 * (PRB2 + COD1DI) / (TEXPR2 + COD1DI)) +  arr(TPRN2 * COD1DH / (TEXPR2 + COD1DI)) ;
TPRR3 = arr(TPRN3 * (PRB3 + COD1EI) / (TEXPR3 + COD1EI)) +  arr(TPRN3 * COD1EH / (TEXPR3 + COD1EI)) ;
TPRR4 = arr(TPRN4 * (PRB4 + COD1FI) / (TEXPR4 + COD1FI)) +  arr(TPRN4 * COD1FH / (TEXPR4 + COD1FI)) ;

TPRR2V = positif(PEBFV+PENSALV+CODRAZ+CODRAL+CODRAM) * arr(TPRNV * CARPEV / TEXPRV)
           +  (1 -positif(PEBFV+PENSALV+CODRAZ+CODRAL+CODRAM)) * (TPRNV -TPRRV)   ;
TPRR2C = positif(PEBFC+PENSALC+CODRBZ+CODRBL+CODRBM) * arr(TPRNC * CARPEC / TEXPRC)
           +  (1 -positif(PEBFC+PENSALC+CODRBZ+CODRBL+CODRBM)) * (TPRNC -TPRRC)   ;
TPRR21 = positif(PEBF1+PENSALP1+CODRCZ+CODRCL+CODRCM) * arr(TPRN1 * CARPEP1 / TEXPR1 )
           +  (1 -positif(PEBF1+PENSALP1+CODRCZ+CODRCL+CODRCM)) * (TPRN1 -TPRR1);
TPRR22 = positif(PEBF2+PENSALP2+CODRDZ+CODRDL+CODRDM) * arr(TPRN2 * CARPEP2 / TEXPR2 )
           +  (1 -positif(PEBF2+PENSALP2+CODRDZ+CODRDL+CODRDM)) * (TPRN2 -TPRR2);
TPRR23 = positif(PEBF3+PENSALP3+CODREZ+CODREL+CODREM) * arr(TPRN3 * CARPEP3 / TEXPR3 )
           +  (1 -positif(PEBF3+PENSALP3+CODREZ+CODREL+CODREM)) * (TPRN3 -TPRR3);
TPRR24 = positif(PEBF4+PENSALP4+CODRFZ+CODRFL+CODRFM) * arr(TPRN4 * CARPEP4 / TEXPR4 )
           +  (1 -positif(PEBF4+PENSALP4+CODRFZ+CODRFL+CODRFM)) * (TPRN4 -TPRR4);
TPRR2ZV = positif(PEBFV+PENSALV+CODRAL+CODRAM) * arr(TPRNV * CODRAZ / TEXPRV)
           +  (1 -positif(PEBFV+PENSALV+CODRAL+CODRAM)) * (TPRNV -TPRRV-TPRR2V)   ;
TPRR2ZC = positif(PEBFC+PENSALC+CODRBL+CODRBM) * arr(TPRNC * CODRBZ / TEXPRC)
           +  (1 -positif(PEBFC+PENSALC+CODRBL+CODRBM)) * (TPRNC -TPRRC-TPRR2C)   ;
TPRR2Z1 = positif(PEBF1+PENSALP1+CODRCL+CODRCM) * arr(TPRN1 * CODRCZ / TEXPR1)
           +  (1 -positif(PEBF1+PENSALP1+CODRCL+CODRCM)) * (TPRN1 -TPRR1-TPRR21)   ;
TPRR2Z2 = positif(PEBF2+PENSALP2+CODRDL+CODRDM) * arr(TPRN2 * CODRDZ / TEXPR2)
           +  (1 -positif(PEBF2+PENSALP2+CODRDL+CODRDM)) * (TPRN2 -TPRR2-TPRR22)   ;
TPRR2Z3 = positif(PEBF3+PENSALP3+CODREL+CODREM) * arr(TPRN3 * CODREZ / TEXPR3)
           +  (1 -positif(PEBF3+PENSALP3+CODREL+CODREM)) * (TPRN3 -TPRR3-TPRR23)   ;
TPRR2Z4 = positif(PEBF4+PENSALP4+CODRFL+CODRFM) * arr(TPRN4 * CODRFZ / TEXPR4 )
           +  (1 -positif(PEBF4+PENSALP4+CODRFL+CODRFM)) * (TPRN4 -TPRR4-TPRR24);
TPENFV =  positif(PENSALV+CODRAL+CODRAM) * arr(TPRNV * PEBFV / TEXPRV)
       + (1 - positif(PENSALV+CODRAL+CODRAM)) * max(0,(TPRNV -TPRRV -TPRR2V-TPRR2ZV));
TPENFC =  positif(PENSALC+CODRBL+CODRBM) * arr(TPRNC * PEBFC / TEXPRC)
       + (1 - positif(PENSALC+CODRBL+CODRBM)) * max(0,(TPRNC -TPRRC -TPRR2C-TPRR2ZC));
TPENF1 =  positif(PENSALP1+CODRCL+CODRCM) * arr(TPRN1 * PEBF1 / TEXPR1)
        + (1- positif(PENSALP1+CODRCL+CODRCM)) * (TPRN1 -TPRR1 -TPRR21-TPRR2Z1);
TPENF2 =  positif(PENSALP2+CODRDL+CODRDM) * arr(TPRN2 * PEBF2 / TEXPR2)
        + (1- positif(PENSALP2+CODRDL+CODRDM)) * (TPRN2 -TPRR2 -TPRR22-TPRR2Z2);
TPENF3 =  positif(PENSALP3+CODREL+CODREM) * arr(TPRN3 * PEBF3 / TEXPR3)
        + (1- positif(PENSALP3+CODREL+CODREM)) * (TPRN3 -TPRR3 -TPRR23-TPRR2Z3);
TPENF4 =  positif(PENSALP4+CODRFL+CODRFM) * arr(TPRN4 * PEBF4 / TEXPR4)
        + (1- positif(PENSALP4+CODRFL+CODRFM)) * (TPRN4 -TPRR4 -TPRR24-TPRR2Z4);
TPRR2RAL = positif(PENSALV+CODRAM) * arr(TPRNV * CODRAL / TEXPRV)
           +  (1 -positif(PENSALV+CODRAM)) * (TPRNV -TPRRV-TPRR2V-TPRR2ZV-TPENFV);
TPRR2RBL = positif(PENSALC+CODRBM) * arr(TPRNC * CODRBL / TEXPRC)
           +  (1 -positif(PENSALC+CODRBM)) * (TPRNC -TPRRC-TPRR2C-TPRR2ZC-TPENFC);
TPRR2RCL = positif(PENSALP1+CODRCM) * arr(TPRN1 * CODRCL / TEXPR1 )
           +  (1 -positif(PENSALP1+CODRCM)) * (TPRN1 -TPRR1-TPRR21-TPRR2Z1-TPENF1);
TPRR2RDL = positif(PENSALP2+CODRDM) * arr(TPRN2 * CODRDL / TEXPR2 )
           +  (1 -positif(PENSALP2+CODRDM)) * (TPRN2 -TPRR2-TPRR22-TPRR2Z2-TPENF2);
TPRR2REL = positif(PENSALP3+CODREM) * arr(TPRN3 * CODREL / TEXPR3 )
           +  (1 -positif(PENSALP3+CODREM)) * (TPRN3 -TPRR3-TPRR23-TPRR2Z3-TPENF3);
TPRR2RFL = positif(PENSALP4+CODRFM) * arr(TPRN4 * CODRFL / TEXPR4 )
           +  (1 -positif(PENSALP4+CODRFM)) * (TPRN4 -TPRR4-TPRR24-TPRR2Z4-TPENF4);
TPRR2RAM = positif(PENSALV) * arr(TPRNV * CODRAM / TEXPRV)
           +  (1 -positif(PENSALV)) * (TPRNV -TPRRV-TPRR2V-TPENFV-TPRR2ZV-TPRR2RAL);
TPRR2RBM = positif(PENSALC) * arr(TPRNC * CODRBM / TEXPRC)
           +  (1 -positif(PENSALC)) * (TPRNC -TPRRC-TPRR2C-TPENFC-TPRR2ZC-TPRR2RBL);
TPRR2RCM = positif(PENSALP1) * arr(TPRN1 * CODRCM / TEXPR1 )
           +  (1 -positif(PENSALP1)) * (TPRN1 -TPRR1-TPRR21-TPENF1-TPRR2Z1-TPRR2RCL);
TPRR2RDM = positif(PENSALP2) * arr(TPRN2 * CODRDM / TEXPR2 )
           +  (1 -positif(PENSALP2)) * (TPRN2 -TPRR2-TPRR22-TPENF2-TPRR2Z2-TPRR2RDL);
TPRR2REM = positif(PENSALP3) * arr(TPRN3 * CODREM / TEXPR3 )
           +  (1 -positif(PENSALP3)) * (TPRN3 -TPRR3-TPRR23-TPENF3-TPRR2Z3-TPRR2REL);
TPRR2RFM = positif(PENSALP4) * arr(TPRN4 * CODRFM / TEXPR4 )
           +  (1 -positif(PENSALP4)) * (TPRN4 -TPRR4-TPRR24-TPENF4-TPRR2Z4-TPRR2RFL);
TPRR2RAI = positif(CODRAI) * (TPRNRAI * positif (-TTSNTV)
                            + CODRAI * positif_ou_nul (TTSNTV));
TPRR2RBI = positif(CODRBI) * (TPRNRBI * positif (-TTSNTC)
                                + CODRBI * positif_ou_nul (TTSNTC));
TPRR2RCK = positif(CODRCK) * (TPRNRCK * positif (-TTSNT1)
                            + CODRCK * positif_ou_nul (TTSNT1));
TPENALIMV = positif(TEXPRV) * (TPRNV -TPRRV -TPRR2V -TPRR2ZV- TPENFV-TPRR2RAL-TPRR2RAM) ;
TPENALIMC = positif(TEXPRC) * (TPRNC -TPRRC -TPRR2C -TPRR2ZC- TPENFC-TPRR2RBL-TPRR2RBM) ;
TPENALIM1 = positif(TEXPR1) * (TPRN1 -TPRR1 -TPRR21 -TPRR2Z1- TPENF1-TPRR2RCL-TPRR2RCM) ;
TPENALIM2 = positif(TEXPR2) * (TPRN2 -TPRR2 -TPRR22 -TPRR2Z2- TPENF2-TPRR2RDL-TPRR2RDM) ;
TPENALIM3 = positif(TEXPR3) * (TPRN3 -TPRR3 -TPRR23 -TPRR2Z3- TPENF3-TPRR2REL-TPRR2REM) ;
TPENALIM4 = positif(TEXPR4) * (TPRN4 -TPRR4 -TPRR24 -TPRR2Z4- TPENF4-TPRR2RFL-TPRR2RFM) ;




DTSTEFF = min(0 , TTSNV + TTSNC + TTSN1 + TTSN2 + TTSN3 + TTSN4 + TPRNV + TPRNC + TPRN1 + TPRN2 + TPRN3 + TPRN4 + TPRNRAI + TPRNRBI + TPRNRCK) ;

PRTEFF = max(0 , TPRNV + TPRNC + TPRN1 + TPRN2 + TPRN3 + TPRN4 + TPRNRAI + TPRNRBI + TPRNRCK + min(0 , TTSNV + TTSNC + TTSN1 + TTSN2 + TTSN3 + TTSN4)) ;

TSTEFF = (TTSNV + TTSNC + TTSN1 + TTSN2 + TTSN3 + TTSN4 + TPRNV + TPRNC + TPRN1 + TPRN2 + TPRN3 + TPRN4 + TPRNRAI + TPRNRBI + TPRNRCK) - PRTEFF + DTSTEFF ;

CUMSALEXTEF = (TSTEFF - DTSTEFF) - (TSN - DEFTS) ; 

CALPEN = PRTEFF - PRN ;

CUMPENEXTEF = CALPEN ;

regle 99992310:
application : iliad   ;
TTSPRT = (TTSNNV + TPRRV 
        + TTSNNC + TPRRC
        + TTSNN1 + TPRR1
        + TTSNN2 + TPRR2
        + TTSNN3 + TPRR3
        + TTSNN4 + TPRR4);
TTSPRT1731 = max(0,TTSPRV) + max(0,TTSPRC) + max(0,TTSPR1) + max(0,TTSPR2)+ max(0,TTSPR3)+ max(0,TTSPR4);
TTSPR = TTSPRT + RVTOT;
regle 99992320:
application : iliad   ;
TTSPRV = (TTSNNV + TPRRV);
TTSPRC = (TTSNNC + TPRRC);
TTSPR1 = (TTSNN1 + TPRR1);
TTSPR2 = (TTSNN2 + TPRR2);
TTSPR3 = (TTSNN3 + TPRR3);
TTSPR4 = (TTSNN4 + TPRR4);
TTSPRP = somme(i=1..4:TTSPRi) ;
TTSPRDP = somme(i=1..4:TTSPRDi) ;
regle 91030111:
application : iliad  ;

TBNCDF1 = ((1-positif_ou_nul(NOCEPIMP+TSPENETNPF)) * abs(NOCEPIMP+TSPENETNPF)
                + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                * positif_ou_nul(DABNCNP5+DABNCNP4+DABNCNP3+DABNCNP2+DABNCNP1-NOCEPIMP-TSPENETNPF)
                * (DABNCNP5+DABNCNP4+DABNCNP3+DABNCNP2+DABNCNP1-NOCEPIMP-TSPENETNPF)
                * null(TBNCDF6P+TBNCDF5P+TBNCDF4P+TBNCDF3P+TBNCDF2P)) * null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(TDEFBNCNPF) * max(0,TDEFBNCNPF-TDIDABNCNP)
              + (1-positif(TDEFBNCNPF)) *  max(0,-(NOCEPIMPV+NOCEPIMPC+NOCEPIMPP+TSPENETNPF)));

regle 91030112:
application : iliad  ;

TBNCDF2 = ((1-positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP1)
                + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3-DABNCNP2,0)-DABNCNP1,DABNCNP1)*(-1)
                * positif_ou_nul(DABNCNP1-max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3-DABNCNP2,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(TDEFBNCNPF) * min(DABNCNP1,TDEFBNCNPF+DABNCNP-TDIDABNCNP-TBNCDF1)
              + (1-positif(TDEFBNCNPF)) *  min(DABNCNP1,DABNCNP-TDIDABNCNP));

regle 91030113:
application : iliad  ;

TBNCDF3 = ((1 - positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP2)
                 + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                 * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3,0)-DABNCNP2,DABNCNP2)*(-1)
                 * positif_ou_nul(DABNCNP2-max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(TDEFBNCNPF) * min(DABNCNP2,TDEFBNCNPF+DABNCNP-TDIDABNCNP-TBNCDF1-TBNCDF2)
              + (1-positif(TDEFBNCNPF)) *  min(DABNCNP2,DABNCNP-TDIDABNCNP-TBNCDF2));
regle 91030114:
application : iliad  ;

TBNCDF4 = ((1 - positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP3)
                 + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                 * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4,0)-DABNCNP3,DABNCNP3)*(-1)
                 * positif_ou_nul(DABNCNP3-max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(TDEFBNCNPF) * min(DABNCNP3,TDEFBNCNPF+DABNCNP-TDIDABNCNP-TBNCDF1-TBNCDF2-TBNCDF3)
              + (1-positif(TDEFBNCNPF)) *  min(DABNCNP3,DABNCNP-TDIDABNCNP-TBNCDF2-TBNCDF3));
regle 91030115:
application : iliad  ;

TBNCDF5 = ((1 - positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP4)
                 + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                 * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5,0)-DABNCNP4,DABNCNP4)*(-1)
                 * positif_ou_nul(DABNCNP4-max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(TDEFBNCNPF) * min(DABNCNP4,TDEFBNCNPF+DABNCNP-TDIDABNCNP-TBNCDF1-TBNCDF2-TBNCDF3-TBNCDF4)
              + (1-positif(TDEFBNCNPF)) *  min(DABNCNP4,DABNCNP-TDIDABNCNP-TBNCDF2-TBNCDF3-TBNCDF4));
regle 91030116:
application : iliad  ;

TBNCDF6 = ((1 - positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP5)
                 + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                 * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6,0)-DABNCNP5,DABNCNP5)*(-1)
                 * positif_ou_nul(DABNCNP5-max(NOCEPIMP+TSPENETNPF-DABNCNP6,0)))* null(4-V_IND_TRAIT)
          + null(5-V_IND_TRAIT) * (
               positif(TDEFBNCNPF) * min(DABNCNP5,TDEFBNCNPF+DABNCNP-TDIDABNCNP-TBNCDF1-TBNCDF2-TBNCDF3-TBNCDF4-TBNCDF5)
              + (1-positif(TDEFBNCNPF)) *  min(DABNCNP5,DABNCNP-TDIDABNCNP-TBNCDF2-TBNCDF3-TBNCDF4-TBNCDF5));
regle 91030117:
application : iliad   ;
TBNCDF2P = ((1-positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP1)
                + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3-DABNCNP2,0)-DABNCNP1,DABNCNP1)*(-1)
                * positif_ou_nul(DABNCNP1-max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3-DABNCNP2,0)));
TBNCDF3P = ((1 - positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP2)
                 + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                 * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3,0)-DABNCNP2,DABNCNP2)*(-1)
                 * positif_ou_nul(DABNCNP2-max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4-DABNCNP3,0)));
TBNCDF4P = ((1 - positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP3)
                 + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                 * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4,0)-DABNCNP3,DABNCNP3)*(-1)
                 * positif_ou_nul(DABNCNP3-max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5-DABNCNP4,0)));
TBNCDF5P = ((1 - positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP4)
                 + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                 * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5,0)-DABNCNP4,DABNCNP4)*(-1)
                 * positif_ou_nul(DABNCNP4-max(NOCEPIMP+TSPENETNPF-DABNCNP6-DABNCNP5,0)));
TBNCDF6P = ((1 - positif_ou_nul(NOCEPIMP+TSPENETNPF)) * (DABNCNP5)
                 + positif_ou_nul(NOCEPIMP+TSPENETNPF)
                 * min(max(NOCEPIMP+TSPENETNPF-DABNCNP6,0)-DABNCNP5,DABNCNP5)*(-1)
                 * positif_ou_nul(DABNCNP5-max(NOCEPIMP+TSPENETNPF-DABNCNP6,0)));
regle 91030118:
application : iliad   ;                          
TBNCDF = TBNCDF1 + TBNCDF2 + TBNCDF3 + TBNCDF4 + TBNCDF5 + TBNCDF6;
regle 99991030:
application : iliad   ;                          
TDIDABNCNPHQ = max(0 , min(TBNCNPHQCF, DABNCNP1+DABNCNP2+DABNCNP3+DABNCNP4+DABNCNP5+DABNCNP6)) ;
TDIDABNCNPQ = max(0 , min(TBNCNPQCF, DABNCNP1+DABNCNP2+DABNCNP3+DABNCNP4+DABNCNP5+DABNCNP6-TDIDABNCNPHQ)) ;
TDIDABNCNP = max(0 , min(TBNCNPHQCF + TBNCNPQCF, DABNCNP1+DABNCNP2+DABNCNP3+DABNCNP4+DABNCNP5+DABNCNP6)) ;
TDABNCNPF =  max(0 , min(TBNCNPHQCF + TBNCNPQCF, DABNCNP1+DABNCNP2+DABNCNP3+DABNCNP4+DABNCNP5+DABNCNP6)) ;


TDABNCNPV = arr(TDABNCNPF * (TBNCNPHQCV + TBNCNPQCV)/(TBNCNPHQCF*positif(TBNCNPHQCF) + TBNCNPQCF*positif(TBNCNPQCF)))*positif(TBNCNPHQCV + TBNCNPQCV);
TDABNCNPC = arr(TDABNCNPF * (TBNCNPHQCC + TBNCNPQCC)/(TBNCNPHQCF*positif(TBNCNPHQCF) + TBNCNPQCF*positif(TBNCNPQCF)))*positif(TBNCNPHQCC + TBNCNPQCC);
TDABNCNPP = arr(TDABNCNPF * (TBNCNPHQCP + TBNCNPQCP)/(TBNCNPHQCF*positif(TBNCNPHQCF) + TBNCNPQCF*positif(TBNCNPQCF)))*positif(TBNCNPHQCP + TBNCNPQCP);
regle 99991016:
application : iliad   ;
TDEFBNCNP = arr(TSPENETNPV+TSPENETNPC+TSPENETNPP+BNCNPPVV+BNCNPPVC+BNCNPPVP+BNCAABV+ANOCEP*MAJREV20+BNCAABC+ANOVEP*MAJREV20+BNCAABP+ANOPEP*MAJREV20
                               +COD5XS + COD5XX*MAJREV20+COD5YS+COD5YX*MAJREV20+COD5ZS+COD5ZX*MAJREV20
                                      + CODCJG+ CODCNS*MAJREV20+ CODCOS+ CODCRF*MAJREV20+ CODCSF+ CODCSN*MAJREV20);

 regle 99991018:
 application : iliad   ;
TDNPLOCIMPU = max(0,min(TOTDEFLOCNP,NPLOCNETV + NPLOCNETC + NPLOCNETPAC+somme(i=V,C,P:max(0,MLOCDECi-TMIB_ABNPVLi-TMIB_ABNPPLi))));
regle 99993000:
application : iliad   ;

TDEFBNCNPF = (1-PREM8_11) * positif(positif(SOMMEBNCND_2) * positif(BNCDF_P +BNCDFP2 +BNCDF1731))
                     * (max(0,min(min(max(BNCDF_P +BNCDF7_P,BNCDFP2+BNCDF7P2),BNCDF1731+BNCDF71731),
                                        BNCNPDCT+COD5MD+COD5LD+AUTOBNCV+AUTOBNCC+AUTOBNCP+TDIDABNCNP+BNCAABV+ANOCEP*MAJREV20+BNCAABC+ANOVEP*MAJREV20+BNCAABP+ANOPEP*MAJREV20
                            +COD5XS+ COD5XX*MAJREV20+ COD5YS+ COD5YX*MAJREV20+ COD5ZS+ COD5ZX*MAJREV20+ CODCJG+ CODCNS*MAJREV20+ CODCOS+ CODCRF*MAJREV20+ CODCSF+ CODCSN*MAJREV20
                                -NOCEPIMPNV -NOCEPIMPNC -NOCEPIMPNP -BNNAAV -BNNAAC -BNNAAP
                                                              -(max(TDEFBNCNP1731,max(TDEFBNCNP_P,TDEFBNCNPP2)))
                                                               - max(0,arr(TSPENETNPV+TSPENETNPC+TSPENETNPP+AUTOBNCV+AUTOBNCC+AUTOBNCP+BNCNPPVV
                                                                     +BNCNPPVC+BNCNPPVP+BNCAABV+ANOCEP*MAJREV20
                                                                  +BNCAABC+ANOVEP*MAJREV20+BNCAABP+ANOPEP*MAJREV20
                                                                      + COD5XS + COD5XX*MAJREV20+COD5YS+COD5YX*MAJREV20+COD5ZS+COD5ZX*MAJREV20+ CODCJG+ CODCNS*MAJREV20+ CODCOS+ CODCRF*MAJREV20+ CODCSF+ CODCSN*MAJREV20
                                                                                      -TDEFBNCNPP3)))))
                        + PREM8_11 * positif(DEFBNCNPH470) *
                          (BNCNPDCT+COD5MD+COD5LD+TDIDABNCNP+AUTOBNCV+AUTOBNCC+AUTOBNCP+BNCAABV+ANOCEP*MAJREV20+BNCAABC+ANOVEP*MAJREV20+BNCAABP+ANOPEP*MAJREV20
                            + COD5XS+ COD5XX*MAJREV20+ COD5YS+ COD5YX*MAJREV20+ COD5ZS+ COD5ZX*MAJREV20+ CODCJG+ CODCNS*MAJREV20+ CODCOS+ CODCRF*MAJREV20+ CODCSF+ CODCSN*MAJREV20
                                -NOCEPIMPNV -NOCEPIMPNC -NOCEPIMPNP-BNNAAV -BNNAAC -BNNAAP
                           - min(BNCNPDCT+COD5MD+COD5LD,max(TDEFBNCNP1731,TDEFBNCNPP2)))+0;
regle 99993010:
application : iliad   ;
TBNCIF =  max (0,TBNCNPHQCF-TDIDABNCNP+TDEFBNCNPF);
regle 99993100:
application : iliad   ;
TDFBICNPF = max(0,MIBNPRNETV + MIBNPPVV+MIBNPRNETC + MIBNPPVC+MIBNPRNETP + MIBNPPVP+ BICREV + BICHREV * MAJREV20 + BICREC + BICHREC * MAJREV20 + BICREP + BICHREP * MAJREV20 - TDEFNPI - TBICNPF);
regle 99993110:
application : iliad   ;
TDEFBICNPF = (1-PREM8_11) * positif(positif(SOMMEBICND_2)* positif(DLMRN_P +DLMRNP2 +DLMRN1731))
                * (max(0,min(min(max(DLMRN_P+DLMRN7_P,DLMRNP2+DLMRN7P2),DLMRN1731+DLMRN71731),
                       (MIBNPDCT + COD5RZ + COD5SZ + TDEFNPI + BICREV + (BICHREV * MAJREV20) + BICREC + (BICHREC * MAJREV20) + BICREP + (BICHREP * MAJREV20)
		                                             + COD5UR + (COD5US * MAJREV20) + COD5VR + (COD5VS * MAJREV20) + COD5WR + (COD5WS * MAJREV20)
							     + CODCNC + (CODCNI * MAJREV20) + CODCOC + (CODCOI * MAJREV20) + CODCPC + (CODCPI * MAJREV20))
							     - (BINNV + BINNC + BINNP + BINTAQV + BINHTAQV + BINTAQC + BINHTAQC + BINTAQP + BINHTAQP)
                         -(max(TDFBICNPF1731 , max(TDFBICNPF_P , TDFBICNPFP2)))
                 - max(0 , MIBNPRNETV + MIBNPPVV + MIBNPRNETC + MIBNPPVC + MIBNPRNETP + MIBNPPVP + BICREV + (BICHREV * MAJREV20) + BICREC + (BICHREC * MAJREV20) + BICREP + (BICHREP * MAJREV20)
		                                                                                 + COD5UR + (COD5US * MAJREV20) + COD5VR + (COD5VS * MAJREV20) + COD5WR + (COD5WS * MAJREV20)
												 + CODCNC + (CODCNI * MAJREV20) + CODCOC + (CODCOI * MAJREV20) + CODCPC + (CODCPI * MAJREV20) -TDFBICNPFP3))))

            + PREM8_11 * positif(MIBNPRNETV + MIBNPRNETC + MIBNPRNETP + MIBNPPVV + MIBNPPVC + MIBNPPVP + TDEFNPI + BICREV + (BICHREV * MAJREV20) + BICREC + (BICHREC * MAJREV20) + BICREP + (BICHREP * MAJREV20)
	                                                                                                         + COD5UR + (COD5US * MAJREV20) + COD5VR + (COD5VS * MAJREV20) + COD5WR + (COD5WS * MAJREV20)
														 + CODCNC + (CODCNI * MAJREV20) + CODCOC + (CODCOI * MAJREV20) + CODCPC + (CODCPI * MAJREV20))
                       * ((MIBNPDCT + COD5RZ + COD5SZ + TDEFNPI + BICREV + (BICHREV * MAJREV20) + BICREC + (BICHREC * MAJREV20) + BICREP + (BICHREP * MAJREV20)
		                                                + COD5UR + (COD5US * MAJREV20) + COD5VR + (COD5VS * MAJREV20) + COD5WR + (COD5WS * MAJREV20)
								+ CODCNC + (CODCNI * MAJREV20) + CODCOC + (CODCOI * MAJREV20) + CODCPC + (CODCPI * MAJREV20)
								- (BINNV + BINNC + BINNP + BINTAQV + BINHTAQV + BINTAQC + BINHTAQC + BINTAQP + BINHTAQP)) - min(MIBNPDCT , max(TDFBICNPF1731 , TDFBICNPFP2))) ;
regle 99993330:
application : iliad   ;

TDEFNPI=max(0,min(TBICNPOCF+TBICNPQCF,DEFNP));

regle 99993335:
application : iliad   ;
TBNN =  TBNCPHQF + TBNCIF;
regle 99993340:
application : iliad   ;
TBICNPF = (1-PREM8_11) * max(0,BINNV + BINNC + BINNP + TMIBNETNPTOT- TDEFNPI + TDEFBICNPF)
          + PREM8_11 * (positif(BICREV + BICHREV * MAJREV20 + BICREC + BICHREC* MAJREV20 + BICREP + BICHREP * MAJREV20) *(BINNV + BINNC + BINNP + TMIBNETNPTOT)
                        +(1-positif(BICREV + BICHREV * MAJREV20 + BICREC + BICHREC* MAJREV20 + BICREP + BICHREP * MAJREV20)) * max(0,BINNV + BINNC + BINNP + TMIBNETNPTOT- TDEFNPI)) ;
TBICPF = TMIBNETVF + TMIBNETPF + MIB_NETCT  ;
regle 99993343:
application : iliad   ;
TRGPROV = ( TSHBA +  max( BANOR, 0 ) + REB +R1649+PREREV+
           min(BANOR,0) *
                 positif(SEUIL_IMPDEFBA + 1 - TSHBA - (REVTP-BA1)
                         - REVQTOTQHT)
                                ) * (1-positif(RE168+TAX1649))
                                     + (RE168+TAX1649) * positif(RE168+TAX1649) ;
regle 99993344:
application : iliad   ;
TDFANTPROV = min(0,(TRGPROV - DAR )) + SOMDEFICIT;
regle 8412601:
application : iliad  ;


TDEFBANI = max(0,BAFORESTV+BAFPVV+BACREV+arr(BAHREV*MAJREV20)+BAFORESTC+BAFPVC+BACREC+arr(BAHREC*MAJREV20)+BAFORESTP+BAFPVP+BACREP+arr(BAHREP*MAJREV20)
                 +4BACREV + arr(4BAHREV * MAJREV20) + 4BACREC + arr(4BAHREC * MAJREV20) + 4BACREP + arr(4BAHREP * MAJREV20)
                 + min(0,BAHQV+BAHQC+BAHQP+4BAQV+4BAQC+4BAQP) * (1-positif(TSHBA-SEUIL_IMPDEFBA))) ;
regle 8412701:
application : iliad  ;


TDEFBANIF = (1-PREM8_11) * positif(SOMMEBAND_2) * positif(DEFBA_P+DEFBAP2+DEFBA1731)
                      * max(0,DBAIP+SOMDEFBANI
                                        -(max(TDEFBANI1731,max(TDEFBANI_P,TDEFBANIP2)))
                                             - max(0,TDEFBANI-TDEFBANIP3))
         + PREM8_11 * positif(TDEFBANI) * (DBAIP + SOMDEFBANI * positif(TSHBA-SEUIL_IMPDEFBA));
regle 99993345:
application : iliad   ;
TDFANTIMPU =  max(0,SOMDEFICIT-max(TDFANTPROV1731-TDEFBANI1731*(1-positif(TSHBA1731-SEUIL_IMPDEFBA))* positif(TSHBA-SEUIL_IMPDEFBA)
                              ,max(TDFANTPROV_P-TDEFBANI_P*(1-positif(TSHBA_P-SEUIL_IMPDEFBA))* positif(TSHBA-SEUIL_IMPDEFBA)
                              ,TDFANTPROVP2-TDEFBANIP2*(1-positif(TSHBAP2-SEUIL_IMPDEFBA))* positif(TSHBA-SEUIL_IMPDEFBA)))
                             -max(0,TDFANTPROV - TDFANTPROVP3 - TDEFBANIP3 * positif(SEUIL_IMPDEFBA - TSHBAP3)*positif(TSHBA-SEUIL_IMPDEFBA)))
                                  * positif(positif(SOMMEGLOBAL_2)
                                          * positif(positif(SOMMEGLOBND_2)
                                          + (positif(SOMMEBAND_2)   * (1-positif(TSHBA-SEUIL_IMPDEFBA)))
                                          + (positif(SOMMEBA_2)   * positif(RBAT) * positif(TSHBA-SEUIL_IMPDEFBA))
                                          + (positif(SOMMEBIC_2)  * positif(TBICNPF))
                                          + (positif(SOMMELOC_2)  * positif(NPLOCNETF))
                                          + (positif(SOMMEBNC_2)  * positif(TDEFBNCNPF))
                                          + (positif(SOMMERCM_2)  * positif(RCM1))
                                          + (positif(SOMMERF_2)   * positif(RRFI+RFDHIS))))
                                  * null(PREM8_11)
                         +  PREM8_11 * ( max(0,min(FRNV,min(-1 * TTSPRVP2,-1 * TTSPRV1731)))
                                       + max(0,min(FRNC,min(-1 * TTSPRCP2,-1 * TTSPRC1731)))
                                       + max(0,min(FRNP,min(-1 * TTSPRPP2,-1 * TTSPRP1731)))
                                      + SOMDEFBANI * (1-positif(TSHBA-SEUIL_IMPDEFBA))
                                      + (BICPMVCTV+BICPMVCTC+BICPMVCTP - min(BICPMVCTV+BICPMVCTC+BICPMVCTP,max(MIBRNETVP2+MIBRNETCP2+MIBRNETPP2+MIBPVVP2+MIBPVCP2+MIBPVPP2,
                                                                                                                MIBRNETVP3+MIBRNETCP3+MIBRNETPP3+MIBPVVP3+MIBPVCP3+MIBPVPP3)))
                                      + (BICNOV + BICNOC + BICNOP
                                      + (BIHNOV + BIHNOC + BIHNOP) * MAJREV20 - (BIPNV+BIPNC+BIPNP))
                                      + (BNCREV + BNCREC + BNCREP
                                      + (BNHREV + BNHREC + BNHREP) * MAJREV20 - (BNRV+BNRC+BNRP))
                                      + (BNCPMVCTV+BNCPMVCTC+BNCPMVCTP-min(BNCPMVCTV+BNCPMVCTC+BNCPMVCTP,max(SPENETPVP2+SPENETPCP2+SPENETPPP2+BNCPROPVVP2+BNCPROPVCP2+BNCPROPVPP2,
                                                                                                           SPENETPVP3+SPENETPCP3+SPENETPPP3+BNCPROPVVP3+BNCPROPVCP3+BNCPROPVPP3)))
                                      + RFDHIS
                                      + DEFAA4 + DEFAA3 + DEFAA2 + DEFAA1 + DEFAA0
                                       )
                              ;
regle 99991055:
application :  iliad   ;                          
TEFFREV =   INDTEFF * 
                  (
                  (TBICPF + TBICNPF + TBNN
                  + BIHTAV + BIHTAC + BIHTAP
                  + BIPTAV + BIPTAC + BIPTAP
                  + ESFP + TTSPR + RCM1 
                  + max(0,max(0,MLOCDECV-TMIB_ABNPVLV-TMIB_ABNPPLV)+max(0,MLOCDECC-TMIB_ABNPVLC-TMIB_ABNPPLC)+max(0,MLOCDECP-TMIB_ABNPVLP-TMIB_ABNPPLP)
                  +NPLOCNETV + NPLOCNETC + NPLOCNETPAC - TDNPLOCIMPU +DEFLOCNPF)
                  + RFNTEO * V_INDTEO + RRFI * (1-V_INDTEO)
          	+ COD1TZ + positif(COD2OP) * (BTPM3VG + PVTAXSB + BTPM3TJ + BTPM3UA + COD3SZ + GAINPEA)
                + PVIMPOS * null(1-FLAG_EXIT)+(PVIMPOS + PVSURSI) * null(2 -FLAG_EXIT)		
                  + max(BANOR,0) + REB +
                  min(BANOR,0) *
                  positif(SEUIL_IMPDEFBA + 1
                  -TSHBA- (REVTP-BA1)
                  - REVQTOTQHT))
                  + R1649
                  +DFANTIMPU);
regle 99991060:
application :  iliad   ;                          
TEFFREVRFR =   INDTEFF *
                  (
                  (TBICPF + TBICNPF + TBNN
                  + BIHTAV + BIHTAC + BIHTAP
                  + BIPTAV + BIPTAC + BIPTAP
                  + ESFP + TTSPR + RCM1  
                  + max(0,max(0,MLOCDECV-TMIB_ABNPVLV-TMIB_ABNPPLV)+max(0,MLOCDECC-TMIB_ABNPVLC-TMIB_ABNPPLC)+max(0,MLOCDECP-TMIB_ABNPVLP-TMIB_ABNPPLP)
                           +NPLOCNETV + NPLOCNETC + NPLOCNETPAC - TDNPLOCIMPU +DEFLOCNPF)
                  + RFNTEO * V_INDTEO + RRFI * (1-V_INDTEO)
		  + COD1TZ+COD1UZ + COD1WZ + COD1VZ
		  + max(0,BPVRCM+COD3UA+COD3TJ-ABIMPMV+ABIMPPV)
                  + PVBARPA
                  + max(BANOR,0) + REB +
                  min(BANOR,0) *
                  positif(SEUIL_IMPDEFBA + 1
                  -TSHBA- (REVTP-BA1)
                  - REVQTOTQHT))
                  + R1649
                  +DFANTIMPU);
regle 99991065:
application :  iliad   ;                          
RBGTEF = (1 - positif(TEFFREV  +PREREV- DAR)) * min( 0 , TEFFREV  +PREREV- DAR + TOTALQUO )
                  + positif(TEFFREV+PREREV - DAR) * (TEFFREV +PREREV - DAR);
RBGTEFRFR = (1 - positif(TEFFREVRFR  +PREREV- DAR)) * min( 0 , TEFFREVRFR  +PREREV- DAR + TOTALQUO )
                  + positif(TEFFREVRFR+PREREV - DAR) * (TEFFREVRFR +PREREV - DAR);
SDDTEF =  max(0, DAR - TEFFREV) ;
SDDTEFRFR = max(0, DAR -TEFFREVRFR);
RPALETEF = max(0,min(somme(i=1..4:min(NCHENFi,LIM_PENSALENF)+min(arr(CHENFi*MAJREV),LIM_PENSALENF))+COD6GZ+COD6EZ,
                                    RBGTEF-DDCSG+TOTALQUO-SDDTEF)) *(1-V_CNR);
RPALETEFRFR = max(0,min(somme(i=1..4:min(NCHENFi,LIM_PENSALENF)+min(arr(CHENFi*MAJREV),LIM_PENSALENF))+COD6GZ+COD6EZ,
                                    RBGTEFRFR-DDCSG+TOTALQUO-SDDTEFRFR)) *(1-V_CNR);
RPALPTEF = max( min(TOTPA,RBGTEF - RPALETEF - DDCSG + TOTALQUO - SDDTEF) , 0 ) * (1 -V_CNR);
RPALPTEFRFR = max( min(TOTPA,RBGTEFRFR - RPALETEFRFR - DDCSG + TOTALQUO - SDDTEFRFR) , 0 ) * (1 -V_CNR);
RFACCTEF = max( min(DDFA,RBGTEF - RPALETEF - RPALPTEF  - DDCSG + TOTALQUO - SDDTEF) , 0);
RFACCTEFRFR = max( min(DDFA,RBGTEFRFR - RPALETEFRFR - RPALPTEFRFR  - DDCSG + TOTALQUO - SDDTEFRFR) , 0);
RDDIVTEF = max( min(DEDIV * (1 - V_CNR),RBGTEF - RPALETEF - RPALPTEF - RFACCTEF - DDCSG + TOTALQUO - SDDTEF ) , 0 );
RDDIVTEFRFR = max( min(DEDIV * (1 - V_CNR),RBGTEFRFR - RPALETEFRFR - RPALPTEFRFR - RFACCTEFRFR - DDCSG + TOTALQUO - SDDTEFRFR ) , 0 );
RD6DGTEF = max( min(DED6DG * (1 - V_CNR),RBGTEF - RPALETEF - RPALPTEF - RFACCTEF - RDDIVTEF - DDCSG + TOTALQUO - SDDTEF ) , 0 );
RD6DGTEFRFR = max( min(DED6DG * (1 - V_CNR),RBGTEFRFR - RPALETEFRFR - RPALPTEFRFR - RFACCTEFRFR - RDDIVTEFRFR - DDCSG + TOTALQUO - SDDTEFRFR ) , 0 );
APERPVTEF = (1 - V_CNR) * max(min(RPERPV,RBGTEF - RPALETEF - RPALPTEF - RFACCTEF
                                    - RDDIVTEF - RD6DGTEF - DDCSG + TOTALQUO -SDDTEF), 0);
APERPVTEFRFR = (1 - V_CNR) * max(min(RPERPV,RBGTEFRFR - RPALETEFRFR - RPALPTEFRFR - RFACCTEFRFR
                                    - RDDIVTEFRFR - RD6DGTEFRFR - DDCSG + TOTALQUO -SDDTEFRFR), 0);
APERPCTEF = (1 - V_CNR) * max(min(RPERPC,RBGTEF - RPALETEF - RPALPTEF  - RFACCTEF
                                    - RDDIVTEF - DDCSG + TOTALQUO -SDDTEF - APERPVTEF), 0);
APERPCTEFRFR = (1 - V_CNR) * max(min(RPERPC,RBGTEFRFR - RPALETEFRFR - RPALPTEFRFR  - RFACCTEFRFR
                                    - RDDIVTEFRFR - RD6DGTEFRFR - DDCSG + TOTALQUO -SDDTEFRFR - APERPVTEFRFR), 0);
APERPPTEF = (1 - V_CNR) * max(min(RPERPP,RBGTEF - RPALETEF - RPALPTEF  - RFACCTEF
                                    - RDDIVTEF - RD6DGTEF - DDCSG + TOTALQUO -SDDTEF - APERPVTEF - APERPCTEF), 0);
APERPPTEFRFR = (1 - V_CNR) * max(min(RPERPP,RBGTEFRFR - RPALETEFRFR - RPALPTEFRFR  - RFACCTEFRFR
                                    - RDDIVTEFRFR - RD6DGTEFRFR - DDCSG + TOTALQUO -SDDTEFRFR - APERPVTEFRFR - APERPCTEFRFR), 0);
RRBGTEF = (TEFFREV - DAR ) *(1-positif(RE168+TAX1649)) + positif(RE168+TAX1649) * (RE168+TAX1649);
RRBGTEFRFR = (TEFFREVRFR - DAR ) *(1-positif(RE168+TAX1649)) + positif(RE168+TAX1649) * (RE168+TAX1649);
NUREPARTEF = min(NUPROPT , max(0,min(PLAF_NUREPAR, RRBGTEF - RPALETEF - RPALPTEF - RFACCTEF
                                    - RDDIVTEF - RD6DGTEF - APERPVTEF - APERPCTEF - APERPPTEF - DDCSG + TOTALQUO - SDDTEF)))
                                    * (1 - V_CNR) ;
NUREPARTEFRFR = min(NUPROPT , max(0,min(PLAF_NUREPAR, RRBGTEFRFR - RPALETEFRFR - RPALPTEFRFR - RFACCTEFRFR
                                    - RDDIVTEFRFR - RD6DGTEFRFR - APERPVTEFRFR - APERPCTEFRFR - APERPPTEFRFR - DDCSG + TOTALQUO - SDDTEFRFR)))
                                    * (1 - V_CNR) ;
RBG2TEF = RBGTEF - max(0,min(RBGTEF , DDCSG)) *(1-positif(RE168+TAX1649));
RBG2TEFRFR = RBGTEFRFR - max(0,min(RBGTEFRFR , DDCSG)) *(1-positif(RE168+TAX1649));
RBLTEF =  RBG2TEF - max(0,min( RBG2TEF , ( DDPA + DDFA + RDDIVTEF + RD6DGTEF + APERPVTEF + APERPCTEF + APERPPTEF + NUREPARTEF )))  *(1-positif(RE168+TAX1649))* ( 1 - V_CNR )
                                    - min( RBG2TEF , V_8ZT+ CODZRE + CODZRF) * V_CNR;
RBLTEFRFR = RBG2TEFRFR - max(0,min( RBG2TEFRFR , ( DDPA + DDFA + RDDIVTEFRFR + RD6DGTEFRFR + APERPVTEFRFR + APERPCTEFRFR + APERPPTEFRFR + NUREPARTEFRFR ))) *(1-positif(RE168+TAX1649))* ( 1 - V_CNR )
                                    - min( RBG2TEFRFR , V_8ZT+ CODZRE + CODZRF) * V_CNR;
RNGTEF = RBLTEF ;
RNGTEFRFR = RBLTEFRFR ;
SDCTEF = max(0, DDCSG + DDPA + DDFA + RDDIVTEF + RD6DGTEF + APERPVTEF + APERPCTEF + APERPPTEF + NUREPARTEF - max(0,RBGTEF)) * (1 - V_CNR)
	          + max(0, V_8ZT+ CODZRE + CODZRF - max(0,RBGTEF)) * V_CNR ;
SDCTEFRFR = max(0, DDCSG + DDPA + DDFA + RDDIVTEFRFR + RD6DGTEFRFR + APERPVTEFRFR + APERPCTEFRFR + APERPPTEFRFR + NUREPARTEFRFR - max(0,RBGTEF)) * (1 - V_CNR)
	          + max(0, V_8ZT+ CODZRE + CODZRF - max(0,RBGTEF)) * V_CNR ;
NABTEF =   min( max( LIM_ABTRNGDBL + 1  - (RNGTEF+ TOTALQUO- SDDTEF- SDCTEF), 0 ), 1 )
                  + min( max( LIM_ABTRNGSIMP + 1 - (RNGTEF+ TOTALQUO- SDDTEF- SDCTEF), 0 ), 1 );
NABTEFRFR =   min( max( LIM_ABTRNGDBL + 1  - (RNGTEFRFR+ TOTALQUO- SDDTEFRFR- SDCTEFRFR), 0 ), 1 )
                  + min( max( LIM_ABTRNGSIMP + 1 - (RNGTEFRFR+ TOTALQUO- SDDTEFRFR- SDCTEFRFR), 0 ), 1 );
ABTPATEF = NDA * NABTEF * ABAT_UNVIEUX * (1-V_CNR);
ABTPATEFRFR = NDA * NABTEFRFR * ABAT_UNVIEUX * (1-V_CNR);
regle 99991070:
application :  iliad   ;                          
TEFFREVINTER =    INDTEFF * 
                  (
                  (TBICPF + TBICNPF + TBNN
                  + BIHTAV + BIHTAC + BIHTAP
                  + BIPTAV + BIPTAC + BIPTAP 
                  + ESFP + TTSPR + RCM1 
                  + max(0,max(0,MLOCDECV-TMIB_ABNPVLV-TMIB_ABNPPLV)+max(0,MLOCDECC-TMIB_ABNPVLC-TMIB_ABNPPLC)+max(0,MLOCDECP-TMIB_ABNPVLP-TMIB_ABNPPLP)
                           +NPLOCNETV + NPLOCNETC + NPLOCNETPAC - TDNPLOCIMPU +DEFLOCNPF)
                  + RFNTEO * V_INDTEO + RRFI * (1-V_INDTEO)
                  + COD1TZ+positif(COD2OP)* (BTPM3VG+PVTAXSB+BTPM3TJ+BTPM3UA+COD3SZ)
                  + PVIMPOS * null(1-FLAG_EXIT)+(PVIMPOS + PVSURSI) * null(2 -FLAG_EXIT)
                  + max(BANOR,0) + REB +
                  min(BANOR,0) *
                  positif(SEUIL_IMPDEFBA + 1
                  -SHBA- (REVTP-BA1)
                  - REVQTOTQHT))
                  + R1649 - DAR *(1-positif(RE168+TAX1649))
                  +DFANTIMPU);
regle 99991075:
application :  iliad   ;                          

TEFFREVINTERHR =    INDTEFF * 
                  (
                  (TBICPF + TBICNPF + TBNN
                  + BIHTAV + BIHTAC + BIHTAP
                  + BIPTAV + BIPTAC + BIPTAP 
                  + ESFP + TTSPR + RCM1 
                  + max(0,max(0,MLOCDECV-TMIB_ABNPVLV-TMIB_ABNPPLV)+max(0,MLOCDECC-TMIB_ABNPVLC-TMIB_ABNPPLC)+max(0,MLOCDECP-TMIB_ABNPVLP-TMIB_ABNPPLP)
                          +NPLOCNETV + NPLOCNETC + NPLOCNETPAC - TDNPLOCIMPU +DEFLOCNPF)
		  + RRFI 
		  + PVTXEFFHR
                  + COD1TZ+positif(COD2OP)* (BTPM3VG+PVTAXSB+BTPM3TJ+BTPM3UA+COD3SZ)
                  + PVIMPOS * null(1-FLAG_EXIT)+(PVIMPOS + PVSURSI) * null(2 -FLAG_EXIT)		
                  + max(BANOR,0) + REB +
                  min(BANOR,0) *
                  positif(SEUIL_IMPDEFBA + 1
                  -SHBA- (REVTP-BA1)
                  - REVQTOTQHT))
                  + R1649 - DAR *(1-positif(RE168+TAX1649))
                  +DFANTIMPU);
regle 99991080:
application :  iliad   ;                          

TEFFREVTOT =    INDTEFF * 
                  max(0,
                  (TBICPF + TBICNPF + TBNN
                  + BIHTAV + BIHTAC + BIHTAP
                  + BIPTAV + BIPTAC + BIPTAP 
                  + ESFP + TTSPR + RCM1  
                  + max(0,max(0,MLOCDECV-TMIB_ABNPVLV-TMIB_ABNPPLV)+max(0,MLOCDECC-TMIB_ABNPVLC-TMIB_ABNPPLC)+max(0,MLOCDECP-TMIB_ABNPVLP-TMIB_ABNPPLP)
                           +NPLOCNETV + NPLOCNETC + NPLOCNETPAC - TDNPLOCIMPU +DEFLOCNPF)
                  + RFNTEO * V_INDTEO + RRFI * (1-V_INDTEO)
         	   + COD1TZ+positif(COD2OP)* (BTPM3VG+ PVTAXSB + BTPM3TJ + BTPM3UA+COD3SZ+GAINPEA)
                  + PVIMPOS * null(1-FLAG_EXIT)+(PVIMPOS + PVSURSI) * null(2 -FLAG_EXIT)		
                  + max(BANOR,0) + REB +
                  min(BANOR,0) *
                  positif(SEUIL_IMPDEFBA + 1
                  -SHBA- (REVTP-BA1)
                  - REVQTOTQHT))
                  + R1649 - (DAR + max(0,min(TEFFREVINTER,DDPA + DDFA + RDDIVTEF + RD6DGTEF+ APERPVTEF + APERPCTEF + APERPPTEF + NUREPARTEF + ABTPATEF + ABTMA+DDCSG))) *(1-positif(RE168+TAX1649))
                  +DFANTIMPU)
                  ;
regle 99991085:
application :  iliad   ;                          
TEFFREVTOTRFR =    INDTEFF * 
                  max(0,
                  (TBICPF + TBICNPF + TBNN
                  + BIHTAV + BIHTAC + BIHTAP
                  + BIPTAV + BIPTAC + BIPTAP 
                  + ESFP + TTSPR + RCM1 
                  + max(0,max(0,MLOCDECV-TMIB_ABNPVLV-TMIB_ABNPPLV)+max(0,MLOCDECC-TMIB_ABNPVLC-TMIB_ABNPPLC)+max(0,MLOCDECP-TMIB_ABNPVLP-TMIB_ABNPPLP)
                           +NPLOCNETV + NPLOCNETC + NPLOCNETPAC - TDNPLOCIMPU +DEFLOCNPF)
                  + RFNTEO * V_INDTEO + RRFI * (1-V_INDTEO)
                  + COD1TZ+positif(COD2OP)*(BTPM3VG + BTPM3TJ+ BTPM3UA+COD3SZ)
                  + PVIMPOS * null(1-FLAG_EXIT)+(PVIMPOS + PVSURSI) * null(2 -FLAG_EXIT)
                  + max(BANOR,0) + REB +
                  min(BANOR,0) *
                  positif(SEUIL_IMPDEFBA + 1
                  -SHBA- (REVTP-BA1)
                  - REVQTOTQHT))
                  + R1649 - (DAR + max(0,min(TEFFREVINTER,DDPA + DDFA + RDDIVTEFRFR +RD6DGTEFRFR+ NUREPARTEFRFR + ABTPATEFRFR + ABTMA+DDCSG))) *(1-positif(RE168+TAX1649))
                  +DFANTIMPU)
                  ;
regle 99991090:
application :  iliad   ;                          
TEFFREVTOTRFRHR =    INDTEFF * 
                  max(0,
                  (TBICPF + TBICNPF + TBNN
                  + BIHTAV + BIHTAC + BIHTAP
                  + BIPTAV + BIPTAC + BIPTAP 
                  + ESFP + TTSPR + RCM1 
                  + max(0,max(0,MLOCDECV-TMIB_ABNPVLV-TMIB_ABNPPLV)+max(0,MLOCDECC-TMIB_ABNPVLC-TMIB_ABNPPLC)+max(0,MLOCDECP-TMIB_ABNPVLP-TMIB_ABNPPLP)
                           +NPLOCNETV + NPLOCNETC + NPLOCNETPAC - TDNPLOCIMPU +DEFLOCNPF)
                  + RRFI  
                  + COD1TZ+positif(COD2OP) *(BTPM3VG+COD3SZ) * (1-positif(ABIMPMV)) 
		  +(positif(COD2OP) * BTPM3TJ)+(positif(COD2OP)* BTPM3UA* (1-positif(ABIMPMV)))
                  + PVIMPOS * null(1-FLAG_EXIT)+(PVIMPOS + PVSURSI) * null(2 -FLAG_EXIT)
                  + max(BANOR,0) + REB +
                  min(BANOR,0) *
                  positif(SEUIL_IMPDEFBA + 1
                  -SHBA- (REVTP-BA1)
                  - REVQTOTQHT))
                  + R1649 - (DAR + max(0,min(TEFFREVINTERHR,DDPA + DDFA + RDDIVTEFRFR + NUREPARTEFRFR + ABTPATEFRFR + ABTMA+DDCSG))) *(1-positif(RE168+TAX1649))
                  +DFANTIMPU)
                  ;

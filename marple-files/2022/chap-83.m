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
regle 831000:
application : iliad ;


RRFI = (RFON + DRCF + RMFN+DEFRFNONIB);
RRFIPS = RRFI ; 

regle 831010:
application : iliad ;


MICFR=arr(RFMIC*TX_MICFON/100);
MICFRQ=arr(CODRBE*TX_MICFON/100);

regle 831020:
application : iliad ;


RMF = RFMIC - MICFR ;
RMFRBE = CODRBE - MICFRQ ;
RMF4BK = COD4BK - arr(COD4BK*TX_MICFON/100) ;
RMFRBK = CODRBK - arr(CODRBK*TX_MICFON/100);

D4BD4BE = min (RFDANT,RMF+RMFRBE) * RMF/(RMF+RMFRBE);
D4BDRBE = min (RFDANT,RMF+RMFRBE) * RMFRBE/(RMF+RMFRBE);
RMFN = max(0 , RMF - D4BD4BE) ;
RMFNQ = max(0 , RMFRBE - D4BDRBE) ;
regle 831025:
application : iliad ;

PASRFASS = positif(RMF + RMFRBE- RFDANT) * (max(0,RMFN - R4BK) + max(0,RMFNQ - RRBK)) ;

regle 831030:
application : iliad ;


RFCD = RFORDI + FONCI + REAMOR;

regle 831040:
application : iliad ;


RFCE = max(0,RFCD- RFDORD);
RFCEAPS = max(0,RFORDI- RFDORD);
RFCEPS = max(0,RFCD-RFDORD);

DFCE = min(0,RFCD- RFDORD);

RFCF = max(0,RFCE-RFDHIS);
RFCFPS = (RFCEPS-RFDHIS);
RFCFAPS = max(0,RFCEAPS-RFDHIS);

DRCF  = min(0,RFCE-RFDHIS);

RFCG = max(0,RFCF- RFDANT);
DFCG = min(0,RFCF- RFDANT);
regle 831050:
application : iliad ;

RFON = arr(RFCG*RFORDI/RFCD);
regle 831052:
application : iliad ;

2REVFBIS = null(4 - V_IND_TRAIT) * min( arr (RFCG*(FONCI)/(RFORDI + FONCI + REAMOR + CODRBE)) , RFCG-RFON)
        +  null(5 - V_IND_TRAIT) * min( arr (RFCG*(FONCI)/(RFORDI + FONCI + REAMOR + CODRBE)) , RFCG-RFON1731) ;
3REVFBIS = null(4 - V_IND_TRAIT) * min( arr (RFCG*(REAMOR)/(RFORDI + FONCI + REAMOR + CODRBE)) , RFCG-RFON-2REVFBIS)
         +  null(5 - V_IND_TRAIT) * min( arr (RFCG*(REAMOR)/(RFORDI + FONCI + REAMOR + CODRBE)) , RFCG-RFON1731-2REVFBIS) ;
4REVFBIS = max(0, RMFNQ * CODRBE/(RFORDI + FONCI + REAMOR + CODRBE)) ;

RFQ = FONCI + REAMOR + CODRBE ;
regle 831053:
application : iliad ;

DEFRFQ2 = arr ((DEFRFNONIQ)*(FONCI)/(FONCI + REAMOR + CODRBE));
DEFRFQ3 = arr (DEFRFNONIQ*REAMOR/(FONCI + REAMOR + CODRBE));
DEFRFQ4 = arr (DEFRFNONIQ*RMFNQ/(FONCI + REAMOR + CODRBE));
regle 831054:
application : iliad ;

2REVF = null(4 - V_IND_TRAIT) * min( arr (RFCG*(FONCI)/(RFORDI + FONCI + REAMOR + CODRBE)) , RFCG-RFON)
           + null(5 - V_IND_TRAIT) * min( arr (RFCG*(FONCI)/(RFORDI + FONCI + REAMOR + CODRBE)) , max(0,RFCG-RFON1731))+DEFRFQ2 ;
3REVF = null(4 - V_IND_TRAIT) * min( arr (RFCG*REAMOR/(RFORDI + FONCI + REAMOR + CODRBE)) , RFCG-RFON-2REVFBIS)
            + null(5 - V_IND_TRAIT) * min( arr (RFCG*REAMOR/(RFORDI + FONCI + REAMOR + CODRBE)) , max(0,RFCG-RFON1731-2REVFBIS))+DEFRFQ3 ;
4REVF = max(0, RMFNQ * CODRBE/(RFORDI + FONCI + REAMOR + CODRBE)+DEFRFQ4) ;
regle 831055:
application : iliad ;

RFDANT4BA = max(0,RFORDI - RFON);
RFDANTRBA = max(0,FONCI - 2REVF);
RFDANTSBA = max(0,REAMOR - 3REVF);
RFDANTRBE = max(0,CODRBE - 4REVF);

PASRF = (1 - positif(-DFCG)) * max(0 , present(RFORDI) * (RFON  - (COD4BL - arr(COD4BL * RFDANT4BA/RFORDI)))) ;

regle 831060:
application : iliad ;

 
DEF4BB = min(RFDORD,RFORDI + RFMIC * 0.70 + FONCI + REAMOR+CODRBE) ;
DEFRF4BB = min(RFDORD,max(DEF4BB1731,max(DEF4BB_P,DEF4BBP2))) * positif(SOMMERF_2) * (1-PREM8_11) ;

regle 831070:
application : iliad ;
 
 
DEF4BD = min(RFDANT,RFORDI + RFMIC * 0.70 + FONCI + REAMOR+CODRBE-RFDORD - RFDHIS) ;
DEFRF4BD = min(RFDANT,max(DEF4BD1731,max(DEF4BD_P,DEF4BDP2)))* positif(SOMMERF_2) * (1-PREM8_11) ;

regle 831080:
application : iliad ;

DEF4BC = max(0, RFORDI + RFMIC * 0.70 + FONCI + REAMOR +CODRBE- RFDORD) ;
DEFRF4BC = max(0,RFDHIS-max(DEF4BC1731,max(DEF4BC_P,DEF4BCP2))) * positif(DFANTIMPU)*(1-positif(PREM8_11))
          + RFDHIS *positif(PREM8_11);
regle 834210:
application : iliad ;

RFREVENU = (RFORDI + RFMIC * 0.70 + FONCI + REAMOR+CODRBE);
regle 834215:
application : iliad ;
DEFRFNONIBIS =  min(RFDORD,RFORDI + RFMIC * 0.70 + FONCI + REAMOR+CODRBE) + max(0,min(RFDANT,RFORDI + RFMIC * 0.70 + FONCI +CODRBE+ REAMOR-RFDORD - RFDHIS));
regle 831090:
application : iliad ;
DEFRFNONI = (min(RFDANT,max(0,min(RFDANT,RFCD) - RFCD1731)) * (1-PREM8_11) * (1-positif(DAR))
              + min(max(0,RMF - RMF1731+RMFRBE-RMFRBE1731),RFDANT) * (1-PREM8_11) 
	               + (min(RMF,RFDANT)*positif(RMF) + min(RFCF,RFDANT) *(1-positif(RMF))) * PREM8_11
                            ) * null(V_IND_TRAIT - 5) * (1-positif(COD9ZA))  * positif(ART1731BIS);
DEFRFNONIB = arr((RFON+RMF) * DEFRFNONI /(RFON+2REVFBIS+3REVFBIS+4REVFBIS+RMF+RMFRBE))*positif(2REVFBIS+3REVFBIS+4REVFBIS) + (1-positif(2REVFBIS+3REVFBIS+4REVFBIS)) * DEFRFNONI;
DEFRFNONIQ = max(0,DEFRFNONI - DEFRFNONIB);

regle 831095:
application : iliad ;




4BB4BA =arr(RFDORD*(RFORDI/RFCD));
R14BA = RFORDI-4BB4BA;
4BC4BA =arr( RFDHIS*(R14BA/RFCE));
R24BA = R14BA -4BC4BA;
R2BA = RFCE - RFDHIS ;
4BD4BA =arr( RFDANT * (R24BA/R2BA));
R3BA = max (0, R2BA-4BD4BA);
D3BA = min (0,R2BA-4BD4BA);

R34BA = R24BA -4BD4BA ; 


4BBRBA = max (0,arr(RFDORD*(FONCI/RFCD)));
R1RBA =max(0, FONCI- 4BBRBA);
4BCRBA =max(0,arr( RFDHIS*(R1RBA/RFCE)));
R2RBA =max(0, R1RBA - 4BCRBA);
4BDRBA =max (0,arr(RFDANT*( R2RBA/R2BA)));

R3RBA = max(0,arr(R2RBA - 4BDRBA)) ; 


4BBSBA =max (0,arr( RFDORD*(REAMOR/RFCD)));
R1SBA =max(0, REAMOR-4BBSBA);
4BCSBA = max(0,RFDHIS*(R1SBA/RFCE));
R2SBA =max(0,R1SBA-4BCSBA);
4BDSBA =max(0, RFDANT*( R2SBA/R2BA));

R3SBA =max (0,R2SBA - 4BDSBA);

4BD4BE = max(0 , min(RFDANT , RMF + RMFRBE) * RMF/(RMF + RMFRBE)) ;
R4BE = max(0 , RMF - 4BD4BE) ;
D4BE = min(0 , RMF - 4BD4BE) ;

4BDRBE = max(0 , min(RFDANT , RMF + RMFRBE) - 4BD4BE) ;
RRBE = max(0 , RMFRBE - 4BDRBE) ;
DRBE = min(0 , RMFRBE - 4BDRBE) ;

R4BL = (positif (D3BA))*0
      + (positif(R3BA))*( arr(COD4BL-(COD4BL*4BD4BA/RFORDI)));


RRBT =(positif(R3BA))*max(0,arr( CODRBT - arr((4BBRBA+4BCRBA+4BDRBA)*CODRBT/FONCI)))
       + positif(D3BA)*(0) ;
R4BK =(positif(R4BE))*max(0,arr( RMF4BK - 4BD4BE*COD4BK/RFMIC))
       + positif(D4BE)*(0) ;
RRBK =(positif(RRBE))*max(0,arr( RMFRBK - 4BDRBE*CODRBK/CODRBE))
       + positif(DRBE)*(0) ;
       
regle 831096:
application : iliad ;


REVORDIRF = RRFI - R4BL ;


REVQUOTRF = REVRF -RRBT;



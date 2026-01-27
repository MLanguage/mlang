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
 #
 #     CHAPITRE 2. CALCUL DU NET A PAYER
 #
 #
 #
regle corrective 10801:
application : iliad;
TXINR = (max(0,NBMOISI2+NBMOISI) * TXMOISRETARD2) *null(2017 - V_ANREV)
        +  max(0,NBMOISI2 * TXMOISRETARD2) * positif(V_ANREV-2017);

TXINRRED22 = (1-positif(V_FLAGR24))* ((max(0,NBMOISI2+NBMOISI) * TXMOISRETARD2 *TXMOISRED) * null(2017 - V_ANREV) + (NBMOISI2 * TXMOISRETARD2 *TXMOISRED) * positif(V_ANREV - 2017));
TXINRRED24 = positif(V_FLAGR24) * ((max(0,NBMOISI2+NBMOISI) * TXMOISRETARD2 *TXMOISRED2) * null(2017 - V_ANREV) + (NBMOISI2 * TXMOISRETARD2 *TXMOISRED2) * positif(V_ANREV - 2017));

INDESSOC = positif(V_FLAGR22+V_FLAGR24)+0;
regle corrective 1081:
application : iliad ;
IND_PASSAGE = positif(FLAG_DEFAUT + FLAG_RETARD) + IND_PASSAGE_A;
IND_PASSR9901 = 1 + IND_PASSR9901_A;
regle corrective 1081091:
application : iliad ;
NIRNIN_PA = abs(min(0,IAN + AVFISCOPTER -IR9YI- IRE-AUTOVERSLIB-CODCOA+CODZRA)*positif_ou_nul(IAN + AVFISCOPTER - IRE)*positif(CODCOA)*null(1 - IND_PASSAGE) + NIRNIN_PA_A);
NCSBASE_PA = abs(min(0,CSG - CSGIM  - CS9YP-CODCOB)*positif(CODCOB)*null(1 - IND_PASSAGE) + NCSBASE_PA_A);
NRDBASE_PA = abs(min(0,RDSN - CRDSIM  - RD9YP-CODCOR)*positif(CODCOR)*null(1 - IND_PASSAGE) + NRDBASE_PA_A);
NPSOLBASE_PA = abs(min(0,PSOL - PRSPROV  - PS9YP-CODCOD)*positif(CODCOD)*null(1 - IND_PASSAGE) + NPSOLBASE_PA_A);
NCVNBASE_PA = abs(min(0,CVNN - COD8YT  - CVN9YP-CODCOE)*positif(CODCOE)*null(1 - IND_PASSAGE) + NCVNBASE_PA_A);
NCDISBASE_PA = abs(min(0,CDIS - CDISPROV  - CDIS9YP-CODCOF)*positif(CODCOF)*null(1 - IND_PASSAGE) + NCDISBASE_PA_A);
NC820BASE_PA = abs(min(0,MCSG820 - COD8ZH -C8209YP-CODCOQ)*positif(CODCOQ)*null(1 - IND_PASSAGE) + NC820BASE_PA_A);
NGLOBASE_PA = abs(min(0,CGLOA - COD8YL  - GLO9YP-CODCOG)*positif(CODCOG)*null(1 - IND_PASSAGE) + NGLOBASE_PA_A);
NRSE1BASE_PA = abs(min(0,RSE1N - CSPROVYD - RSE19YP-CODCOT)*positif(CODCOT)*null(1 - IND_PASSAGE) + NRSE1BASE_PA_A);
NRSE2BASE_PA = abs(min(0, max(0, RSE8TV - CIRSE8TV - CSPROVYF) + max(0, RSE8SA -CIRSE8SA - CSPROVYN) - RSE29YP-CODCOL)*positif(CODCOL)*null(1 - IND_PASSAGE) + NRSE2BASE_PA_A);
NRSE3BASE_PA = abs(min(0,RSE3N - CSPROVYG - RSE39YP-CODCOM)*positif(CODCOM)*null(1 - IND_PASSAGE) + NRSE3BASE_PA_A);
NRSE4BASE_PA = abs(min(0, RSE4N  - CSPROVYH - CSPROVYP - RSE49YP-CODCOO)*positif(CODCOO)*null(1 - IND_PASSAGE) + NRSE4BASE_PA_A);
NRSE5BASE_PA = abs(min(0,RSE5N - CSPROVYE - RSE59YP-CODCOJ)*positif(CODCOJ)*null(1 - IND_PASSAGE) + NRSE5BASE_PA_A);
NRSE6BASE_PA = abs(min(0,RSE6N - COD8YQ - RSE69YP-CODCOP)*positif(CODCOP)*null(1 - IND_PASSAGE) + NRSE6BASE_PA_A);
NRSE8BASE_PA = abs(min(0,RSE8N - COD8YV - COD8YX - RSE89YP-CODCOH)*positif(CODCOH)*null(1 - IND_PASSAGE) + NRSE8BASE_PA_A);
NTAXABASE_PA = abs(min(0,TAXASSUR  -CODCOU-TAXA9YI)*positif(CODCOU)*null(1 - IND_PASSAGE) + NTAXABASE_PA_A);
NPCAPBASE_PA = abs(min(0,IPCAPTAXT -CODCOV-CAP9YI)*positif(CODCOV)*null(1 - IND_PASSAGE) + NPCAPBASE_PA_A);
NCHRBASE_PA = abs(min(0,IHAUTREVT +CHRPVIMP -CODCOX-CHR9YI)*positif(CODCOX)*null(1 - IND_PASSAGE) + NCHRBASE_PA_A);
IR9YI_PA = IR9YI *null(1 - IND_PASSAGE) + IR9YI_PA_A* positif(IND_PASSAGE-1);
TAXA9YI_PA = TAXA9YI *null(1 - IND_PASSAGE) + TAXA9YI_PA_A* positif(IND_PASSAGE-1);
CAP9YI_PA = CAP9YI *null(1 - IND_PASSAGE) + CAP9YI_PA_A* positif(IND_PASSAGE-1);
CHR9YI_PA = CHR9YI *null(1 - IND_PASSAGE) + CHR9YI_PA_A* positif(IND_PASSAGE-1);
CS9YP_PA = CS9YP *null(1 - IND_PASSAGE) + CS9YP_PA_A* positif(IND_PASSAGE-1);
RD9YP_PA = RD9YP *null(1 - IND_PASSAGE) + RD9YP_PA_A* positif(IND_PASSAGE-1);
PS9YP_PA = PS9YP *null(1 - IND_PASSAGE) + PS9YP_PA_A* positif(IND_PASSAGE-1);
CDIS9YP_PA = CDIS9YP *null(1 - IND_PASSAGE) + CDIS9YP_PA_A* positif(IND_PASSAGE-1);
CVN9YP_PA = CVN9YP *null(1 - IND_PASSAGE) + CVN9YP_PA_A* positif(IND_PASSAGE-1);
GLO9YP_PA = GLO9YP *null(1 - IND_PASSAGE) + GLO9YP_PA_A* positif(IND_PASSAGE-1);
C8209YP_PA = C8209YP *null(1 - IND_PASSAGE) + C8209YP_PA_A* positif(IND_PASSAGE-1);
RSE19YP_PA = RSE19YP *null(1 - IND_PASSAGE) + RSE19YP_PA_A* positif(IND_PASSAGE-1);
RSE29YP_PA = RSE29YP *null(1 - IND_PASSAGE) + RSE29YP_PA_A* positif(IND_PASSAGE-1);
RSE39YP_PA = RSE39YP *null(1 - IND_PASSAGE) + RSE39YP_PA_A* positif(IND_PASSAGE-1);
RSE49YP_PA = RSE49YP *null(1 - IND_PASSAGE) + RSE49YP_PA_A* positif(IND_PASSAGE-1);
RSE59YP_PA = RSE59YP *null(1 - IND_PASSAGE) + RSE59YP_PA_A* positif(IND_PASSAGE-1);
RSE69YP_PA = RSE69YP *null(1 - IND_PASSAGE) + RSE69YP_PA_A* positif(IND_PASSAGE-1);
RSE89YP_PA = RSE89YP *null(1 - IND_PASSAGE) + RSE89YP_PA_A* positif(IND_PASSAGE-1);
regle corrective 1081095:
application : iliad ;
IRNIN_PA = max(0,IRNIN_INR-IRNIN_R9901) * positif(FLAG_RETARD+FLAG_DEFAUT)* null(1 - IND_PASSAGE) + IRNIN_PA_A* positif(IND_PASSAGE-1);
IRNIN_PADO = max(0,IRNIN_INR-NIRNIN_PA) * positif(FLAG_RETARD+FLAG_DEFAUT)* null(1 - IND_PASSAGE) + IRNIN_PADO_A * positif(IND_PASSAGE-1);
TXINR_PA22 = TXINRRED22 * null(1 - IND_PASSAGE) + TXINR_PA22_A;
TXINR_PA = TXINR * null(1 - IND_PASSAGE) + TXINR_PA_A;
INRIR_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * ( 
             arr(max(0,IRNIN_INR-NIRNIN_PA) * TXINR / 100) * positif(max(0,IRNIN_INR-NIRNIN_PA)) * null(1 - IND_PASSAGE) 
             + INRIR_RETDEF_A* (1-positif(SOM9YI))
		  + arr((max(0,IRNIN_PA-CODCOA)) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                );
INR_IR_TARDIF = (((arr(max(0,IRNIN_INR+NIRNIN-IRNIN_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100) * positif(IRNIN_INR) * null(1-IND_PASSAGE)+ INR_IR_TARDIF_A*(1-positif(SOM9YI))))* (1-V_FLAGR22)
                  + ((arr(max(0,IRNIN_INR+NIRNIN-IRNIN_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100) * positif(IRNIN_INR) * null(1-IND_PASSAGE)+ INR_IR_TARD22_A*(1-positif(SOM9YI)))) * V_FLAGR22)
                      * (1-FLAG_RETARD99) * FLAG_RETARD * (1-IND_RJLJ);
CSG_PA = max(0,CSBASE_INR-CSG_R9901) * null(1 - IND_PASSAGE) + CSG_PA_A* positif(IND_PASSAGE-1);
CSG_PADO = max(0,CSBASE_INR-NCSBASE_PA) * null(1 - IND_PASSAGE) + CSG_PADO_A * positif(IND_PASSAGE-1);
INRCSG_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
                arr(max(0,CSBASE_INR-NCSBASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRCSG_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,CSG_PA-CODCOB) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                );
INR_CSG_TARDIF = ((arr(max(0,CSBASE_INR+NCSBASE-CSG_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100) * (1-V_FLAGR22)* (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CSG_TARDIF_A)
                + (arr(max(0,CSBASE_INR+NCSBASE-CSG_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100) * V_FLAGR22* (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CSG_TARD22_A))
                  * (1-IND_RJLJ);
PSOL_PA = max(0,PSOLBASE_INR-PSOL_R9901) * null(1 - IND_PASSAGE) + PSOL_PA_A* positif(IND_PASSAGE-1);
PSOL_PADO = max(0,PSOLBASE_INR-NPSOLBASE_PA) * null(1 - IND_PASSAGE) + PSOL_PADO_A * positif(IND_PASSAGE-1);
INRPSOL_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
             arr(max(0,PSOLBASE_INR-NPSOLBASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRPSOL_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,PSOL_PA-CODCOD) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_PSOL_TARDIF = ((arr(max(0,PSOLBASE_INR+NPSOLBASE-PSOL_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100) * (1-V_FLAGR22)   * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_PSOL_TARDIF_A)
                 + (arr(max(0,PSOLBASE_INR+NPSOLBASE-PSOL_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100) * V_FLAGR22  * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_PSOL_TARD22_A)) * (1-IND_RJLJ);
CRDS_PA = max(0,RDBASE_INR-RDS_R9901) * null(1 - IND_PASSAGE) + CRDS_PA_A* positif(IND_PASSAGE-1);
CRDS_PADO = max(0,RDBASE_INR-NRDBASE_PA) * null(1 - IND_PASSAGE) + CRDS_PADO_A * positif(IND_PASSAGE-1);
INRCRDS_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
             arr(max(0,RDBASE_INR-NRDBASE) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRCRDS_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,CRDS_PA-CODCOR) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_CRDS_TARDIF = ((arr(max(0,RDBASE_INR+NRDBASE-RDS_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CRDS_TARDIF_A)
                 + (arr(max(0,RDBASE_INR+NRDBASE-RDS_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CRDS_TARD22_A)) * (1-IND_RJLJ);
TAXA_PA = max(0,TAXABASE_INR-TAXA_R9901) * null(1 - IND_PASSAGE) + TAXA_PA_A* positif(IND_PASSAGE-1);
INRTAXA_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,TAXABASE_INR+NTAXABASE_PA) * TXINR/ 100) * null(IND_PASSAGE - 1)
             + INRTAXA_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,TAXA_PA-CODCOU) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_TAXAGA_TARDIF = ((arr(max(0,TAXABASE_INR+NTAXABASE-TAXA_R9901*(1-FLAG_PRIM*FLAG_9YT)-TAXA9YI_PA) * TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_TAXA_TARDIF_A)
                   + (arr(max(0,TAXABASE_INR+NTAXABASE-TAXA_R9901*(1-FLAG_PRIM*FLAG_9YT)-TAXA9YI_PA) * TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_TAXA_TARD22_A)) * (1-IND_RJLJ);
CHR_PA = max(0,CHRBASE_INR-CHR_R9901) * null(1 - IND_PASSAGE) + CHR_PA_A* positif(IND_PASSAGE-1);
INRCHR_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,CHRBASE_INR+NCHRBASE_PA)* TXINR / 100) * null(IND_PASSAGE - 1)
             + INRCHR_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,CHR_PA-CODCOX) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_CHR_TARDIF = ((arr(max(0,CHRBASE_INR+NCHRBASE-CHR_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CHR_TARDIF_A)
                   + (arr(max(0,CHRBASE_INR+NCHRBASE-CHR_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CHR_TARD22_A)) * (1-IND_RJLJ);
PCAP_PA = max(0,PCAPBASE_INR-PCAP_R9901) * null(1 - IND_PASSAGE) + PCAP_PA_A* positif(IND_PASSAGE-1);
INRPCAP_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,PCAPBASE_INR+NPCAPBASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRPCAP_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,PCAP_PA-CODCOV) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_PCAP_TARDIF = (arr(max(0,PCAPBASE_INR-PCAP_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR_PA/100) 
                                         * FLAG_RETARD  * (1-V_FLAGR22) * (1-FLAG_RETARD99)* null(1-IND_PASSAGE)) * (1-IND_RJLJ)
                + (arr(max(0,PCAPBASE_INR-PCAP_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR_PA22/100) 
		                         * FLAG_RETARD  * V_FLAGR22 * (1-FLAG_RETARD99)* null(1-IND_PASSAGE)) * (1-IND_RJLJ)
		          + INR_PCAP_TARDIF_A;
RSE1_PA = max(0,RSE1BASE_INR-RSE1_R9901) * null(1 - IND_PASSAGE) + RSE1_PA_A* positif(IND_PASSAGE-1);
INRRSE1_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,RSE1BASE_INR+NRSE1BASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRRSE1_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,RSE1_PA-CODCOT) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_RSE1_TARDIF = ((arr(max(0,RSE1BASE_INR +NRSE1BASE-RSE1_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE1_TARDIF_A)
                   + (arr(max(0,RSE1BASE_INR +NRSE1BASE-RSE1_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE1_TARD22_A)) * (1-IND_RJLJ);
RSE2_PA = max(0,RSE2BASE_INR-RSE2_R9901) * null(1 - IND_PASSAGE) + RSE2_PA_A* positif(IND_PASSAGE-1);
INRRSE2_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,RSE2BASE_INR+NRSE2BASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRRSE2_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,RSE2_PA-CODCOL) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_RSE2_TARDIF = ((arr(max(0,RSE2BASE_INR+NRSE2BASE-RSE2_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE2_TARDIF_A)
                   + (arr(max(0,RSE2BASE_INR+NRSE2BASE-RSE2_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE2_TARD22_A)) * (1-IND_RJLJ);
RSE3_PA = max(0,RSE3BASE_INR-RSE3_R9901) * null(1 - IND_PASSAGE) + RSE3_PA_A* positif(IND_PASSAGE-1);
INRRSE3_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,RSE3BASE_INR+NRSE3BASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRRSE3_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,RSE3_PA-CODCOM) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_RSE3_TARDIF = ((arr(max(0,RSE3BASE_INR+NRSE3BASE-RSE3_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE3_TARDIF_A)
                   + (arr(max(0,RSE3BASE_INR+NRSE3BASE-RSE3_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE3_TARD22_A)) * (1-IND_RJLJ);
RSE4_PA = max(0,RSE4BASE_INR-RSE4_R9901)* null(1 - IND_PASSAGE) + RSE4_PA_A* positif(IND_PASSAGE-1);
INRRSE4_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,RSE4BASE_INR+NRSE4BASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRRSE4_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,RSE4_PA-CODCOO) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_RSE4_TARDIF = ((arr(max(0,RSE4BASE_INR+NRSE4BASE-RSE4_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE4_TARDIF_A)
                   + (arr(max(0,RSE4BASE_INR+NRSE4BASE-RSE4_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE4_TARD22_A)) * (1-IND_RJLJ);
CDIS_PA = max(0,CDISBASE_INR-CDIS_R9901) * null(1 - IND_PASSAGE) + CDIS_PA_A* positif(IND_PASSAGE-1);
INRCDIS_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,CDISBASE_INR+NCDISBASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRCDIS_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,CDIS_PA-CODCOF) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_CDIS_TARDIF = ((arr(max(0,CDISBASE_INR+NCDISBASE-CDIS_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CDIS_TARDIF_A)
                   + (arr(max(0,CDISBASE_INR+NCDISBASE-CDIS_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CDIS_TARD22_A)) * (1-IND_RJLJ);
RSE5_PA = max(0,RSE5BASE_INR-RSE5_R9901) * null(1 - IND_PASSAGE) + RSE5_PA_A* positif(IND_PASSAGE-1);
INRRSE5_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,RSE5BASE_INR +NRSE5BASE_PA)* TXINR / 100) * null(IND_PASSAGE - 1)
                                )
             + INRRSE5_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,RSE5_PA-CODCOJ) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                        ;
INR_RSE5_TARDIF = ((arr(max(0,RSE5BASE_INR +NRSE5BASE-RSE5_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE5_TARDIF_A)
                   + (arr(max(0,RSE5BASE_INR +NRSE5BASE-RSE5_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE5_TARD22_A)) * (1-IND_RJLJ);
RSE6_PA = max(0,RSE6BASE_INR-RSE6_R9901) * null(1 - IND_PASSAGE) + RSE6_PA_A* positif(IND_PASSAGE-1);
INRRSE6_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,RSE6BASE_INR +NRSE6BASE_PA)* TXINR / 100) * null(IND_PASSAGE - 1)
             + INRRSE6_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,RSE6_PA-CODCOP) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_RSE6_TARDIF = ((arr(max(0,RSE6BASE_INR +NRSE6BASE-RSE6_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE6_TARDIF_A)
                   + (arr(max(0,RSE6BASE_INR +NRSE6BASE-RSE6_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE6_TARD22_A)) * (1-IND_RJLJ);
RSE8_PA = max(0,RSE8BASE_INR-RSE8_R9901) * null(1 - IND_PASSAGE) + RSE8_PA_A* positif(IND_PASSAGE-1);
INRRSE8_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,RSE8BASE_INR +NRSE8BASE_PA)* TXINR / 100) * null(IND_PASSAGE - 1)
             + INRRSE8_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,RSE8_PA-CODCOH) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_RSE8_TARDIF = ((arr(max(0,RSE8BASE_INR+NRSE8BASE-RSE8_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE8_TARDIF_A)
                   + (arr(max(0,RSE8BASE_INR+NRSE8BASE-RSE8_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100)  * V_FLAGR22* (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_RSE8_TARD22_A)) * (1-IND_RJLJ);

CVN_PA = max(0,CVNBASE_INR-CVN_R9901)* null(1 - IND_PASSAGE) + CVN_PA_A* positif(IND_PASSAGE-1);
INRCVN_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,CVNBASE_INR +NCVNBASE_PA)* TXINR / 100) * null(IND_PASSAGE - 1)
             + INRCVN_RETDEF_A* (1-positif(SOM9YI))
		  + arr(max(0,CVN_PA-CODCOE) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_CVN_TARDIF = ((arr(max(0,CVNBASE_INR+NCVNBASE-CVN_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CVN_TARDIF_A)
                   + (arr(max(0,CVNBASE_INR+NCVNBASE-CVN_R9901*(1-FLAG_PRIM*FLAG_9YT)) * TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_CVN_TARD22_A)) * (1-IND_RJLJ);
GLO_PA = max(0,GLOBASE_INR-GLO_R9901) * null(1 - IND_PASSAGE) + GLO_PA_A* positif(IND_PASSAGE-1);
INRGLO_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,GLOBASE_INR+NGLOBASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRGLO_RETDEF_A * (1-positif(SOM9YI))
		  + arr(max(0,GLO_PA-CODCOG) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_GLO_TARDIF = ((arr(max(0,GLOBASE_INR +NGLOBASE-GLO_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINR/100) * (1-FLAG_RETARD99)  * (1-V_FLAGR22) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_GLO_TARDIF_A)
                   + (arr(max(0,GLOBASE_INR +NGLOBASE-GLO_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINRRED22/100) * (1-FLAG_RETARD99)  * V_FLAGR22 * FLAG_RETARD * null(1-IND_PASSAGE)+INR_GLO_TARD22_A)) * (1-IND_RJLJ);
C820_PA = max(0,C820BASE_INR-C820_R9901) * null(1 - IND_PASSAGE) + C820_PA_A* positif(IND_PASSAGE-1);
INRC820_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(max(0,C820BASE_INR+NC820BASE_PA) * TXINR / 100) * null(IND_PASSAGE - 1)
             + INRC820_RETDEF_A * (1-positif(SOM9YI))
		  + arr(max(0,C820_PA-CODCOQ) * TXINR_PA/100) * positif(IND_PASSAGE -1)* positif(SOM9YI)
                                )
                        ;
INR_C820_TARDIF = ((arr(max(0,C820BASE_INR +NC820BASE-C820_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINR/100)  * (1-V_FLAGR22) * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_C820_TARDIF_A)
                   + (arr(max(0,C820BASE_INR +NC820BASE-C820_R9901*(1-FLAG_PRIM*FLAG_9YT))* TXINRRED22/100)  * V_FLAGR22 * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGE)+INR_C820_TARD22_A)) * (1-IND_RJLJ);
IFI_PA = IFIBASE_INR * null(1 - IND_PASSAGEISF) + IFI_PA_A;
TXINRISF_PA = TXINRISF * null(1 - IND_PASSAGEISF) + TXINRISF_PA_A;
INRIFI_RETDEF = (1 - IND_RJLJ) * FLAG_DEFAUT * (
               arr(IFIBASE_INR * TXINRISF / 100) * null(IND_PASSAGEISF - 1)
             + INRIFI_RETDEF_A * (1-positif(SOM9YI))
		  + arr(IFI_PA * TXINRISF_PA/100) * positif(IND_PASSAGEISF -1)* positif(SOM9YI)
                                )
                        ;
INR_IFI_TARDIF = (arr(IFIBASE_INR * TXINRISF/100)  * (1-FLAG_RETARD99) * FLAG_RETARD * null(1-IND_PASSAGEISF)+INR_IFI_TARDIF_A) * (1-IND_RJLJ);
regle corrective 10811:
application :  iliad ;
IRNIN_TLDEC_22=max(0,IRNIN_INR+NIRNIN)*positif(1-V_FLAGR24);
CSG_TLDEC_22=max(0,CSBASE_INR+NCSBASE) *positif(1-V_FLAGR24);
PSOL_TLDEC_22=max(0,PSOLBASE_INR+NPSOLBASE) *positif(1-V_FLAGR24);
RDS_TLDEC_22=max(0,RDBASE_INR+NRDBASE) *positif(1-V_FLAGR24);
TAXA_TLDEC_22=max(0,TAXABASE_INR+NTAXABASE) *positif(1-V_FLAGR24);
CHR_TLDEC_22=max(0,CHRBASE_INR+NCHRBASE) *positif(1-V_FLAGR24);
PCAP_TLDEC_22=max(0,PCAPBASE_INR+NPCAPBASE) *positif(1-V_FLAGR24);
RSE1_TLDEC_22=max(0,RSE1BASE_INR+NRSE1BASE) *positif(1-V_FLAGR24);
RSE2_TLDEC_22=max(0,RSE2BASE_INR+NRSE2BASE) *positif(1-V_FLAGR24);
RSE3_TLDEC_22=max(0,RSE3BASE_INR+NRSE3BASE) *positif(1-V_FLAGR24);
RSE4_TLDEC_22=max(0,RSE4BASE_INR+NRSE4BASE) *positif(1-V_FLAGR24);
CDIS_TLDEC_22=max(0,CDISBASE_INR+NCDISBASE) *positif(1-V_FLAGR24);
RSE5_TLDEC_22=max(0,RSE5BASE_INR+NRSE5BASE) *positif(1-V_FLAGR24);
RSE6_TLDEC_22=max(0,RSE6BASE_INR+NRSE6BASE) *positif(1-V_FLAGR24);
RSE8_TLDEC_22=max(0,RSE8BASE_INR+NRSE8BASE) *positif(1-V_FLAGR24);
CVN_TLDEC_22=max(0,CVNBASE_INR+NCVNBASE) *positif(1-V_FLAGR24);
GLO_TLDEC_22=max(0,GLOBASE_INR+NGLOBASE) *positif(1-V_FLAGR24);
C820_TLDEC_22=max(0,C820BASE_INR+NC820BASE) *positif(1-V_FLAGR24);
IFI_TLDEC_22=IFIBASE_INR *positif(1-V_FLAGR24);

regle corrective 12000:
application :  iliad ;
IRNIN_TLDEC_24=max(0,IRNIN_INR+NIRNIN)*positif(V_FLAGR24);
CSG_TLDEC_24=max(0,CSBASE_INR+NCSBASE)*positif(V_FLAGR24);
PSOL_TLDEC_24=max(0,PSOLBASE_INR+NPSOLBASE)*positif(V_FLAGR24);
RDS_TLDEC_24=max(0,RDBASE_INR+NRDBASE)*positif(V_FLAGR24);
TAXA_TLDEC_24=max(0,TAXABASE_INR+NTAXABASE)*positif(V_FLAGR24);
CHR_TLDEC_24=max(0,CHRBASE_INR+NCHRBASE)*positif(V_FLAGR24);
PCAP_TLDEC_24=max(0,PCAPBASE_INR+NPCAPBASE)*positif(V_FLAGR24);
RSE1_TLDEC_24=max(0,RSE1BASE_INR+NRSE1BASE)*positif(V_FLAGR24);
RSE2_TLDEC_24=max(0,RSE2BASE_INR+NRSE2BASE)*positif(V_FLAGR24);
RSE3_TLDEC_24=max(0,RSE3BASE_INR+NRSE3BASE)*positif(V_FLAGR24);
RSE4_TLDEC_24=max(0,RSE4BASE_INR+NRSE4BASE)*positif(V_FLAGR24);
CDIS_TLDEC_24=max(0,CDISBASE_INR+NCDISBASE)*positif(V_FLAGR24);
RSE5_TLDEC_24=max(0,RSE5BASE_INR+NRSE5BASE)*positif(V_FLAGR24);
RSE6_TLDEC_24=max(0,RSE6BASE_INR+NRSE6BASE)*positif(V_FLAGR24);
RSE8_TLDEC_24=max(0,RSE8BASE_INR+NRSE8BASE)*positif(V_FLAGR24);
CVN_TLDEC_24=max(0,CVNBASE_INR+NCVNBASE)*positif(V_FLAGR24);
GLO_TLDEC_24=max(0,GLOBASE_INR+NGLOBASE)*positif(V_FLAGR24);
C820_TLDEC_24=max(0,C820BASE_INR+NC820BASE)*positif(V_FLAGR24);
IFI_TLDEC_24=IFIBASE_INR*positif(V_FLAGR24);

regle corrective 12100:
application :  iliad ;
IRNIN_TLDEC_1=IRNIN_TLDEC_22+IRNIN_TLDEC_24;
CSG_TLDEC_1=CSG_TLDEC_22+CSG_TLDEC_24;
PSOL_TLDEC_1=PSOL_TLDEC_22+PSOL_TLDEC_24;
RDS_TLDEC_1=RDS_TLDEC_22+RDS_TLDEC_24;
TAXA_TLDEC_1=TAXA_TLDEC_22+TAXA_TLDEC_24;
CHR_TLDEC_1=CHR_TLDEC_22+CHR_TLDEC_24;
PCAP_TLDEC_1=PCAP_TLDEC_22+PCAP_TLDEC_24;
RSE1_TLDEC_1=RSE1_TLDEC_22+RSE1_TLDEC_24;
RSE2_TLDEC_1=RSE2_TLDEC_22+RSE2_TLDEC_24;
RSE3_TLDEC_1=RSE3_TLDEC_22+RSE3_TLDEC_24;
RSE4_TLDEC_1=RSE4_TLDEC_22+RSE4_TLDEC_24;
RSE5_TLDEC_1=RSE5_TLDEC_22+RSE5_TLDEC_24;
RSE6_TLDEC_1=RSE6_TLDEC_22+RSE6_TLDEC_24;
RSE8_TLDEC_1=RSE8_TLDEC_22+RSE8_TLDEC_24;
CDIS_TLDEC_1=CDIS_TLDEC_22+CDIS_TLDEC_24;
CVN_TLDEC_1=CVN_TLDEC_22+CVN_TLDEC_24;
GLO_TLDEC_1=GLO_TLDEC_22+GLO_TLDEC_24;
C820_TLDEC_1=C820_TLDEC_22+C820_TLDEC_24;
IFI_TLDEC_1=IFI_TLDEC_22+IFI_TLDEC_24;
regle corrective 12150:
application : iliad ;
INRIR_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) *  (
			 null(2-FLAG_INR) * positif(IRNIN_INR+NIRNIN - IRNIN_REF)
                       * (
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_REF * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0) )
            * arr(max(0,IRNIN_INR+NIRNIN - max(IRNIN_REF * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0)) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_REF * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0))
            * positif(IRNIN_INR+NIRNIN-max(IRNIN_RECT * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0))
            * arr((IRNIN_INR+NIRNIN - max(IRNIN_REF * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0)) * (TXINR / 100))
            * positif(FLAG_DEFAUT+FLAG_RETARD) * positif(IND_PASSAGE - 1))
                             )
                   +  null(3-FLAG_INR) * positif(IRNIN_INR+NIRNIN- IRNIN_REF)
                       * (
            (positif(IRNIN_INR+NIRNIN- max(IRNIN_REF * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0) )
            * arr((min(max(0,IRNIN_INR+NIRNIN),IRNIN_TLDEC_1) - max(IRNIN_REF * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0)) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_REF * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0))
            * positif(IRNIN_INR+NIRNIN-max(IRNIN_RECT * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0))
            * arr((min(max(0,IRNIN_INR+NIRNIN),IRNIN_TLDEC_1) - max(IRNIN_REF * (1-present(IRNIN_NTLDEC_198)),IRNIN_NTLDEC_198+0)) * (TXINR / 100))
            * positif(FLAG_DEFAUT+FLAG_RETARD) * positif(IND_PASSAGE - 1))
                             )
            + INRIR_RETDEF * null(IND_PASSAGE - 1)
                                                )
                                               ;
INRCSG_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
			 null(2 - FLAG_INR) * positif(CSBASE_INR+NCSBASE-CSG_REF) 
			* (
            (positif((CSBASE_INR+NCSBASE) 
	    * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CSG_NTLDEC_198,CSG_REF* (1-present(CSG_NTLDEC_198))))
            * arr(max(0,CSBASE_INR+NCSBASE - max(CSG_NTLDEC_198,CSG_REF* (1-present(CSG_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((CSBASE_INR+NCSBASE)* positif(CSG+PRS+RDSN-SEUIL_61)  - max(CSG_NTLDEC_198,CSG_REF* (1-present(CSG_NTLDEC_198))))
            * arr(max(0,CSBASE_INR+NCSBASE - max(CSG_NTLDEC_198,CSG_REF* (1-present(CSG_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                              )
                      + null(3 - FLAG_INR) * positif(CSG-CSG_REF) 
			* (
            (positif((CSBASE_INR+NCSBASE-SEUIL_61) - max(CSG_NTLDEC_198,CSG_REF* (1-present(CSG_NTLDEC_198))))
            * arr(max(0,min(CSBASE_INR+NCSBASE,CSG_TLDEC_1) - max(CSG_NTLDEC_198,CSG_REF* (1-present(CSG_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((CSBASE_INR+NCSBASE-SEUIL_61)  - max(CSG_NTLDEC_198,CSG_REF* (1-present(CSG_NTLDEC_198))+0))
            * arr(max(0,min(CSBASE_INR+NCSBASE,CSG_TLDEC_1) - max(CSG_NTLDEC_198,CSG_REF* (1-present(CSG_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
			    )
            + INRCSG_RETDEF * null(IND_PASSAGE - 1)
                              )
             ;
INRPSOL_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
			   null(2 - FLAG_INR) * positif(PSOLBASE_INR+NPSOLBASE-PSOL_REF) 
			   * (
            (positif((PSOLBASE_INR+NPSOLBASE-SEUIL_61)  - max(PSOL_NTLDEC_198,PSOL_REF* (1-present(PSOL_NTLDEC_198))+0)) 
            * arr(max(0,(PSOLBASE_INR+NPSOLBASE)  - max(PSOL_NTLDEC_198,PSOL_REF* (1-present(PSOL_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((PSOLBASE_INR+NPSOLBASE-SEUIL_61) - max(PSOL_NTLDEC_198,PSOL_REF* (1-present(PSOL_NTLDEC_198))+0))
            * arr(max(0,(PSOLBASE_INR+NPSOLBASE) - max(PSOL_NTLDEC_198,PSOL_REF* (1-present(PSOL_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                        )
                        + null(3 - FLAG_INR) * positif(PSOL-PSOL_REF) 
			   * (
            (positif((PSOLBASE_INR+NPSOLBASE-SEUIL_61)  - max(PSOL_NTLDEC_198,PSOL_REF* (1-present(PSOL_NTLDEC_198))+0)) 
            * arr(max(0,min((PSOLBASE_INR+NPSOLBASE),PSOL_TLDEC_1)  - max(PSOL_NTLDEC_198,PSOL_REF* (1-present(PSOL_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((PSOLBASE_INR+NPSOLBASE-SEUIL_61) - max(PSOL_NTLDEC_198,PSOL_REF* (1-present(PSOL_NTLDEC_198))+0))
            * arr(max(0,min((PSOLBASE_INR+NPSOLBASE),PSOL_TLDEC_1) - max(PSOL_NTLDEC_198,PSOL_REF* (1-present(PSOL_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                        )
            + INRPSOL_RETDEF * null(IND_PASSAGE - 1)
                            )
             ;
INRCRDS_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RDBASE_INR+NRDBASE- RDS_REF) 
		      * (
            (positif((RDBASE_INR+NRDBASE-SEUIL_61) - max(CRDS_NTLDEC_198,RDS_REF* (1-present(CRDS_NTLDEC_198))+0))
            * arr(max(0,(RDBASE_INR+NRDBASE) - max(CRDS_NTLDEC_198,RDS_REF* (1-present(CRDS_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((RDBASE_INR+NRDBASE-SEUIL_61) - max(CRDS_NTLDEC_198,RDS_REF* (1-present(CRDS_NTLDEC_198))+0))
            * arr(max(0,(RDBASE_INR+NRDBASE) -max(CRDS_NTLDEC_198,RDS_REF* (1-present(CRDS_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                         )
                    +  null(3 - FLAG_INR) * positif(RDSN - RDS_REF) 
		      * (
            (positif((RDBASE_INR+NRDBASE-SEUIL_61) - max(CRDS_NTLDEC_198,RDS_REF* (1-present(CRDS_NTLDEC_198))+0))
            * arr(max(0,min((RDBASE_INR+NRDBASE),RDS_TLDEC_1) - max(CRDS_NTLDEC_198,RDS_REF* (1-present(CRDS_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((RDBASE_INR+NRDBASE-SEUIL_61) - max(CRDS_NTLDEC_198,RDS_REF* (1-present(CRDS_NTLDEC_198))+0))
            * arr(max(0,min((RDBASE_INR+NRDBASE),RDS_TLDEC_1) -max(CRDS_NTLDEC_198,RDS_REF* (1-present(CRDS_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                         )
            + INRCRDS_RETDEF * null(IND_PASSAGE - 1)
                            )
             ;
INRTAXA_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		     null(2 - FLAG_INR) * positif(TAXABASE_INR +NTAXABASE- TAXA_REF) 
		     * (
             (positif(TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC_198,TAXA_REF* (1-present(TAXA_NTLDEC_198))+0))
            * arr(max(0,TAXABASE_INR+NTAXABASE - max(TAXA_NTLDEC_198,TAXA_REF* (1-present(TAXA_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC_198,TAXA_REF* (1-present(TAXA_NTLDEC_198))+0))
            * arr(max(0,TAXABASE_INR+NTAXABASE - max(TAXA_NTLDEC_198,TAXA_REF* (1-present(TAXA_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		     + null(3 - FLAG_INR) * positif(TAXABASE_INR +NTAXABASE- TAXA_REF) 
		     * (
             (positif(TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC_198,TAXA_REF* (1-present(TAXA_NTLDEC_198))+0))
            * arr(max(0,min(TAXABASE_INR+NTAXABASE,TAXA_TLDEC_1) - max(TAXA_NTLDEC_198,TAXA_REF* (1-present(TAXA_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(TAXABASE_INR+NTAXABASE - max(TAXA_NTLDEC_198,TAXA_REF* (1-present(TAXA_NTLDEC_198))+0))
            * arr(max(0,min(TAXABASE_INR+NTAXABASE,TAXA_TLDEC_1) - max(TAXA_NTLDEC_198,TAXA_REF* (1-present(TAXA_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRTAXA_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRCDIS_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(CDISBASE_INR+NCDISBASE - CDIS_REF) 
		       * (
             (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC_198,CDIS_REF* (1-present(CDIS_NTLDEC_198))+0))
            * arr(max(0,CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC_198,CDIS_REF* (1-present(CDIS_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC_198,CDIS_REF* (1-present(CDIS_NTLDEC_198))+0)) 
            * arr(max(0,CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC_198,CDIS_REF* (1-present(CDIS_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(CDISBASE_INR+NCDISBASE - CDIS_REF) 
		       * (
             (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC_198,CDIS_REF+0))
            * arr(max(0,min(CDISBASE_INR+NCDISBASE,CDIS_TLDEC_1) - max(CDIS_NTLDEC_198,CDIS_REF* (1-present(CDIS_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC_198,CDIS_REF* (1-present(CDIS_NTLDEC_198))+0))
            * arr(max(0,min(CDISBASE_INR+NCDISBASE,CDIS_TLDEC_1) - max(CDIS_NTLDEC_198,CDIS_REF* (1-present(CDIS_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRCDIS_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRCHR_NTLPROV = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(CHRBASE_INR +NCHRBASE- CHR_REF) 
		       * (
             (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTLDEC_198,CHR_REF* (1-present(CHR_NTLDEC_198))+0))
            * arr(max(0,CHRBASE_INR+NCHRBASE - max(CHR_NTLDEC_198,CHR_REF* (1-present(CHR_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CHRBASE_INR +NCHRBASE- max(CHR_NTLDEC_198,CHR_REF* (1-present(CHR_NTLDEC_198))+0)) 
            * arr(max(0,CHRBASE_INR+NCHRBASE - max(CHR_NTLDEC_198,CHR_REF* (1-present(CHR_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(CHRBASE_INR+NCHRBASE - CHR_REF) 
		       * (
             (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTLDEC_198,CHR_REF+0))
            * arr(max(0,min(CHRBASE_INR+NCHRBASE,CHR_TLDEC_1) - max(CHR_NTLDEC_198,CHR_REF* (1-present(CHR_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CHRBASE_INR +NCHRBASE- max(CHR_NTLDEC_198,CHR_REF* (1-present(CHR_NTLDEC_198))+0))
            * arr(max(0,min(CHRBASE_INR+NCHRBASE,CHR_TLDEC_1) - max(CHR_NTLDEC_198,CHR_REF* (1-present(CHR_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRCHR_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRCHR_NTL15 = (INRCHR_NTLPROV - INRCHR_NTL10) * null(2 - FLAG_INR); 
INRCHR_NTL = INRCHR_NTL10;
INRPCAP_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(PCAPBASE_INR+NPCAPBASE - PCAP_REF) 
		       * (
             (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC_198,PCAP_REF* (1-present(PCAP_NTLDEC_198))+0))
            * arr(max(0,PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC_198,PCAP_REF* (1-present(PCAP_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(PCAPBASE_INR +NPCAPBASE- max(PCAP_NTLDEC_198,PCAP_REF* (1-present(PCAP_NTLDEC_198))+0)) 
            * arr(max(0,PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC_198,PCAP_REF* (1-present(PCAP_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(PCAPBASE_INR+NPCAPBASE - PCAP_REF) 
		       * (
             (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC_198,PCAP_REF+0))
            * arr(max(0,min(PCAPBASE_INR+NPCAPBASE,PCAP_TLDEC_1) - max(PCAP_NTLDEC_198,PCAP_REF* (1-present(PCAP_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC_198,PCAP_REF* (1-present(PCAP_NTLDEC_198))+0))
            * arr(max(0,min(PCAPBASE_INR+NPCAPBASE,PCAP_TLDEC_1) - max(PCAP_NTLDEC_198,PCAP_REF* (1-present(PCAP_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRPCAP_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRRSE1_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE1BASE_INR +NRSE1BASE- RSE1_REF) 
		       * (
             (positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC_198,RSE1_REF* (1-present(RSE1_NTLDEC_198))+0))
            * arr(max(0,RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC_198,RSE1_REF* (1-present(RSE1_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE1BASE_INR +NRSE1BASE- max(RSE1_NTLDEC_198,RSE1_REF* (1-present(RSE1_NTLDEC_198))+0)) 
            * arr(max(0,RSE1BASE_INR +NRSE1BASE- max(RSE1_NTLDEC_198,RSE1_REF* (1-present(RSE1_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE1BASE_INR+NRSE1BASE - RSE1_REF) 
		       * (
             (positif(RSE1BASE_INR +NRSE1BASE- max(RSE1_NTLDEC_198,RSE1_REF+0))
            * arr(max(0,min(RSE1BASE_INR+NRSE1BASE,RSE1_TLDEC_1) - max(RSE1_NTLDEC_198,RSE1_REF* (1-present(RSE1_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC_198,RSE1_REF* (1-present(RSE1_NTLDEC_198))+0))
            * arr(max(0,min(RSE1BASE_INR+NRSE1BASE,RSE1_TLDEC_1) - max(RSE1_NTLDEC_198,RSE1_REF* (1-present(RSE1_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRRSE1_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRRSE2_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE2BASE_INR+NRSE2BASE - RSE2_REF) 
		       * (
             (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC_198,RSE2_REF* (1-present(RSE2_NTLDEC_198))+0))
            * arr(max(0,RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC_198,RSE2_REF* (1-present(RSE2_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC_198,RSE2_REF* (1-present(RSE2_NTLDEC_198))+0)) 
            * arr(max(0,RSE2BASE_INR +NRSE2BASE- max(RSE2_NTLDEC_198,RSE2_REF* (1-present(RSE2_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE2BASE_INR +NRSE2BASE- RSE2_REF) 
		       * (
             (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC_198,RSE2_REF+0))
            * arr(max(0,min(RSE2BASE_INR+NRSE2BASE,RSE2_TLDEC_1) - max(RSE2_NTLDEC_198,RSE2_REF* (1-present(RSE2_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC_198,RSE2_REF* (1-present(RSE2_NTLDEC_198))+0))
            * arr(max(0,min(RSE2BASE_INR+NRSE2BASE,RSE2_TLDEC_1) - max(RSE2_NTLDEC_198,RSE2_REF* (1-present(RSE2_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRRSE2_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRRSE3_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE3BASE_INR+NRSE3BASE - RSE3_REF) 
		       * (
             (positif(RSE3BASE_INR+NRSE3BASE - max(RSE3_NTLDEC_198,RSE3_REF* (1-present(RSE3_NTLDEC_198))+0))
            * arr(max(0,RSE3BASE_INR+NRSE3BASE - max(RSE3_NTLDEC_198,RSE3_REF* (1-present(RSE3_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC_198,RSE3_REF* (1-present(RSE3_NTLDEC_198))+0)) 
            * arr(max(0,RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC_198,RSE3_REF* (1-present(RSE3_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE3BASE_INR +NRSE3BASE- RSE3_REF) 
		       * (
             (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC_198,RSE3_REF+0))
            * arr(max(0,min(RSE3BASE_INR+NRSE3BASE,RSE3_TLDEC_1) - max(RSE3_NTLDEC_198,RSE3_REF* (1-present(RSE3_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC_198,RSE3_REF* (1-present(RSE3_NTLDEC_198))+0))
            * arr(max(0,min(RSE3BASE_INR+NRSE3BASE,RSE3_TLDEC_1) - max(RSE3_NTLDEC_198,RSE3_REF* (1-present(RSE3_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRRSE3_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRRSE4_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE4BASE_INR+NRSE4BASE - RSE4_REF) 
		       * (
             (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC_198,RSE4_REF* (1-present(RSE4_NTLDEC_198))+0))
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC_198,RSE4_REF* (1-present(RSE4_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC_198,RSE4_REF* (1-present(RSE4_NTLDEC_198))+0)) 
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC_198,RSE4_REF* (1-present(RSE4_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE4BASE_INR+NRSE4BASE - RSE4_REF) 
		       * (
             (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC_198,RSE4_REF+0))
            * arr(max(0,min(RSE4BASE_INR+NRSE4BASE,RSE4_TLDEC_1) - max(RSE4_NTLDEC_198,RSE4_REF* (1-present(RSE4_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC_198,RSE4_REF* (1-present(RSE4_NTLDEC_198))+0))
            * arr(max(0,min(RSE4BASE_INR+NRSE4BASE,RSE4_TLDEC_1) - max(RSE4_NTLDEC_198,RSE4_REF* (1-present(RSE4_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRRSE4_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRRSE5_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE5BASE_INR +NRSE5BASE - RSE5_REF) 
		       * (
             (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC_198,RSE5_REF* (1-present(RSE5_NTLDEC_198))+0))
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC_198,RSE5_REF* (1-present(RSE5_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC_198,RSE5_REF* (1-present(RSE5_NTLDEC_198))+0)) 
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC_198,RSE5_REF* (1-present(RSE5_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE5BASE_INR +NRSE5BASE - RSE5_REF) 
		       * (
             (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC_198,RSE5_REF+0))
            * arr(max(0,min(RSE5BASE_INR +NRSE5BASE,RSE5_TLDEC_1) - max(RSE5_NTLDEC_198,RSE5_REF* (1-present(RSE5_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC_198,RSE5_REF* (1-present(RSE5_NTLDEC_198))+0))
            * arr(max(0,min(RSE5BASE_INR +NRSE5BASE,RSE5_TLDEC_1) - max(RSE5_NTLDEC_198,RSE5_REF* (1-present(RSE5_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRRSE5_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRRSE6_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE6BASE_INR +NRSE6BASE - RSE6_REF) 
		       * (
             (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC_198,RSE6_REF* (1-present(RSE6_NTLDEC_198))+0))
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC_198,RSE6_REF* (1-present(RSE6_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC_198,RSE6_REF* (1-present(RSE6_NTLDEC_198))+0)) 
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC_198,RSE6_REF* (1-present(RSE6_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE6BASE_INR +NRSE6BASE - RSE6_REF) 
		       * (
             (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC_198,RSE6_REF+0))
            * arr(max(0,min(RSE6BASE_INR +NRSE6BASE,RSE6_TLDEC_1) - max(RSE6_NTLDEC_198,RSE6_REF* (1-present(RSE6_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC_198,RSE6_REF* (1-present(RSE6_NTLDEC_198))+0))
            * arr(max(0,min(RSE6BASE_INR +NRSE6BASE,RSE6_TLDEC_1) - max(RSE6_NTLDEC_198,RSE6_REF* (1-present(RSE6_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRRSE6_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRRSE8_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE8BASE_INR +NRSE8BASE - RSE8_REF) 
		       * (
             (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC_198,RSE8_REF* (1-present(RSE8_NTLDEC_198))+0))
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC_198,RSE8_REF* (1-present(RSE8_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC_198,RSE8_REF* (1-present(RSE8_NTLDEC_198))+0)) 
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC_198,RSE8_REF* (1-present(RSE8_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE8BASE_INR +NRSE8BASE - RSE8_REF) 
		       * (
             (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC_198,RSE8_REF+0))
            * arr(max(0,min(RSE8BASE_INR +NRSE8BASE,RSE8_TLDEC_1) - max(RSE8_NTLDEC_198,RSE8_REF* (1-present(RSE8_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC_198,RSE8_REF* (1-present(RSE8_NTLDEC_198))+0))
            * arr(max(0,min(RSE8BASE_INR +NRSE8BASE,RSE8_TLDEC_1) - max(RSE8_NTLDEC_198,RSE8_REF* (1-present(RSE8_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRRSE8_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRCVN_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(CVNBASE_INR +NCVNBASE - CVN_REF) 
		       * (
             (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC_198,CVN_REF* (1-present(CVN_NTLDEC_198))+0))
            * arr(max(0,CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC_198,CVN_REF* (1-present(CVN_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC_198,CVN_REF* (1-present(CVN_NTLDEC_198))+0)) 
            * arr(max(0,CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC_198,CVN_REF* (1-present(CVN_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(CVNBASE_INR +NCVNBASE - CVN_REF) 
		       * (
             (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC_198,CVN_REF+0))
            * arr(max(0,min(CVNBASE_INR +NCVNBASE,CVN_TLDEC_1) - max(CVN_NTLDEC_198,CVN_REF* (1-present(CVN_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC_198,CVN_REF* (1-present(CVN_NTLDEC_198))+0))
            * arr(max(0,min(CVNBASE_INR +NCVNBASE,CVN_TLDEC_1) - max(CVN_NTLDEC_198,CVN_REF* (1-present(CVN_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRCVN_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRGLO_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(GLOBASE_INR+NGLOBASE - GLO_REF) 
		       * (
             (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC_198,GLO_REF* (1-present(GLO_NTLDEC_198))+0))
            * arr(max(0,GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC_198,GLO_REF* (1-present(GLO_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC_198,GLO_REF* (1-present(GLO_NTLDEC_198))+0)) 
            * arr(max(0,GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC_198,GLO_REF* (1-present(GLO_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(GLOBASE_INR+NGLOBASE - GLO_REF) 
		       * (
             (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC_198,GLO_REF+0))
            * arr(max(0,min(GLOBASE_INR+NGLOBASE,GLO_TLDEC_1) - max(GLO_NTLDEC_198,GLO_REF* (1-present(GLO_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC_198,GLO_REF* (1-present(GLO_NTLDEC_198))+0))
            * arr(max(0,min(GLOBASE_INR+NGLOBASE,GLO_TLDEC_1) - max(GLO_NTLDEC_198,GLO_REF* (1-present(GLO_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRGLO_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRC820_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(C820BASE_INR+NC820BASE - C820_REF) 
		       * (
             (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC_198,C820_REF* (1-present(C820_NTLDEC_198))+0))
            * arr(max(0,C820BASE_INR+NC820BASE - max(C820_NTLDEC_198,C820_REF* (1-present(C820_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC_198,C820_REF* (1-present(C820_NTLDEC_198))+0)) 
            * arr(max(0,C820BASE_INR+NC820BASE - max(C820_NTLDEC_198,C820_REF* (1-present(C820_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(C820BASE_INR+NC820BASE - C820_REF) 
		       * (
             (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC_198,C820_REF+0))
            * arr(max(0,min(C820BASE_INR+NC820BASE,C820_TLDEC_1) - max(C820_NTLDEC_198,C820_REF* (1-present(C820_NTLDEC_198)))) * (TXINR / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC_198,C820_REF* (1-present(C820_NTLDEC_198))+0))
            * arr(max(0,min(C820BASE_INR+NC820BASE,C820_TLDEC_1) - max(C820_NTLDEC_198,C820_REF* (1-present(C820_NTLDEC_198)))) * (TXINR / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRC820_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
INRIFI_NTL = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(IFIBASE_INR - IFI_REF) 
		       * (
             (positif(IFIBASE_INR - max(IFI_NTLDEC_198,IFI_REF* (1-present(IFI_NTLDEC_198))+0))
            * arr(max(0,IFIBASE_INR - max(IFI_NTLDEC_198,IFI_REF* (1-present(IFI_NTLDEC_198)))) * (TXINRISF / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IFIBASE_INR - max(IFI_NTLDEC_198,IFI_REF* (1-present(IFI_NTLDEC_198))+0)) 
            * arr(max(0,IFIBASE_INR - max(IFI_NTLDEC_198,IFI_REF* (1-present(IFI_NTLDEC_198)))) * (TXINRISF / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(IFIBASE_INR - IFI_REF) 
		       * (
             (positif(IFIBASE_INR - max(IFI_NTLDEC_198,IFI_REF+0))
            * arr(max(0,min(IFIBASE_INR,IFI_TLDEC_1) - max(IFI_NTLDEC_198,IFI_REF* (1-present(IFI_NTLDEC_198)))) * (TXINRISF / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IFIBASE_INR - max(IFI_NTLDEC_198,IFI_REF* (1-present(IFI_NTLDEC_198))+0))
            * arr(max(0,min(IFIBASE_INR,IFI_TLDEC_1) - max(IFI_NTLDEC_198,IFI_REF* (1-present(IFI_NTLDEC_198)))) * (TXINRISF / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
            + INRIFI_RETDEF * null(IND_PASSAGE - 1)
                            )
	     ; 
regle corrective 12170:
application : iliad ;

INRIR_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
			 null(2-FLAG_INR) * positif(IRNIN_INR+NIRNIN - IRNIN_REF)
                       * (
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_REF+0,IRNIN_NTLDEC)) 
            * arr((max(0,IRNIN_INR+NIRNIN) - max(IRNIN_REF,IRNIN_NTLDEC)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_NTLDEC,IRNIN_REF+0))
            * positif(IRNIN_INR+NIRNIN-max(IRNIN_NTLDEC,IRNIN_RECT))
            * arr(max(0,IRNIN_INR+NIRNIN - max(IRNIN_NTLDEC,IRNIN_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT+FLAG_RETARD) * positif(IND_PASSAGE - 1))
                             )
                   +  null(3-FLAG_INR) * positif(IRNIN_INR+NIRNIN - IRNIN_REF)
                       * (
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_REF,IRNIN_NTLDEC)) 
            * arr((min(max(0,IRNIN_INR+NIRNIN),IRNIN_TLDEC_22) - max(IRNIN_REF,IRNIN_NTLDEC)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_NTLDEC,IRNIN_REF+0))
            * positif(IRNIN_INR+NIRNIN-max(IRNIN_NTLDEC,IRNIN_RECT))
            * arr((min(max(0,IRNIN_INR+NIRNIN),IRNIN_TLDEC_22) - max(IRNIN_NTLDEC,IRNIN_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT+FLAG_RETARD) * positif(IND_PASSAGE - 1))
                             )
                                                )
                                               ;
INRCSG_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
			 null(2 - FLAG_INR) * positif(CSBASE_INR+NCSBASE-CSG_REF) 
			* (
            (positif((CSBASE_INR+NCSBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CSG_NTLDEC,CSG_REF+0))
            * arr(max(0,(CSBASE_INR+NCSBASE) - max(CSG_NTLDEC,CSG_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((CSBASE_INR+NCSBASE)* positif(CSG+PSOL+RDSN-SEUIL_61)  - max(CSG_NTLDEC,CSG_REF+0))
            * arr(max(0,(CSBASE_INR+NCSBASE) - max(CSG_NTLDEC,CSG_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                              )
                      + null(3 - FLAG_INR) * positif((CSBASE_INR+NCSBASE)-CSG_REF) 
			* (
            (positif((CSBASE_INR+NCSBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CSG_NTLDEC,CSG_REF+0))
            * arr(max(0,min(max(0,CSBASE_INR+NCSBASE),CSG_TLDEC_22) - max(CSG_NTLDEC,CSG_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((CSBASE_INR+NCSBASE)* positif(CSG+PSOL+RDSN-SEUIL_61)  - max(CSG_NTLDEC,CSG_REF+0))
            * arr(max(0,min(max(0,CSBASE_INR+NCSBASE),CSG_TLDEC_22) - max(CSG_NTLDEC,CSG_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                              )
                            )
             ;
INRPSOL_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
			   null(2 - FLAG_INR) * positif((PSOLBASE_INR+NPSOLBASE)-PSOL_REF) 
			   * (
            (positif((PSOLBASE_INR+NPSOLBASE)* positif(CSG+PSOL+RDSN-SEUIL_61)  - max(PSOL_NTLDEC,PSOL_REF+0)) 
            * arr(max(0,(PSOLBASE_INR+NPSOLBASE)  - max(PSOL_NTLDEC,PSOL_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((PSOLBASE_INR+NPSOLBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(PSOL_NTLDEC,PSOL_REF+0))
            * arr(max(0,(PSOLBASE_INR+NPSOLBASE) - max(PSOL_NTLDEC,PSOL_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                        )
                        + null(3 - FLAG_INR) * positif((PSOLBASE_INR+NPSOLBASE)-PSOL_REF) 
			   * (
            (positif((PSOLBASE_INR+NPSOLBASE)* positif(CSG+PSOL+RDSN-SEUIL_61)  - max(PSOL_NTLDEC,PSOL_REF+0)) 
            * arr(max(0,min(max(0,PSOLBASE_INR+NPSOLBASE),PSOL_TLDEC_22)  - max(PSOL_NTLDEC,PSOL_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((PSOLBASE_INR+NPSOLBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(PSOL_NTLDEC,PSOL_REF+0))
            * arr(max(0,min(max(0,PSOLBASE_INR+NPSOLBASE),PSOL_TLDEC_22) - max(PSOL_NTLDEC,PSOL_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                        )
                            )
             ;
INRCRDS_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif((RDBASE_INR+NRDBASE) - RDS_REF) 
		      * (
            (positif((RDBASE_INR+NRDBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CRDS_NTLDEC,RDS_REF+0))
            * arr(max(0,(RDBASE_INR+NRDBASE) - max(CRDS_NTLDEC,RDS_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((RDBASE_INR+NRDBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CRDS_NTLDEC,RDS_REF+0))
            * arr(max(0,(RDBASE_INR+NRDBASE) -max(CRDS_NTLDEC,RDS_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                         )
                    +  null(3 - FLAG_INR) * positif((RDBASE_INR+NRDBASE) - RDS_REF) 
		      * (
            (positif((RDBASE_INR+NRDBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CRDS_NTLDEC,RDS_REF+0))
            * arr(max(0,min(max(0,RDBASE_INR+NRDBASE),RDS_TLDEC_22) - max(CRDS_NTLDEC,RDS_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((RDBASE_INR+NRDBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CRDS_NTLDEC,RDS_REF+0))
            * arr(max(0,min(max(0,RDBASE_INR+NRDBASE),RDS_TLDEC_22) -max(CRDS_NTLDEC,RDS_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                         )
                            )
             ;
INRTAXA_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		     null(2 - FLAG_INR) * positif(TAXABASE_INR+NTAXABASE - TAXA_REF) 
		     * (
             (positif(TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC,TAXA_REF+0))
            * arr(max(0,TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC,TAXA_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(TAXABASE_INR+NTAXABASE - max(TAXA_NTLDEC,TAXA_REF+0))
            * arr(max(0,TAXABASE_INR+NTAXABASE - max(TAXA_NTLDEC,TAXA_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		     + null(3 - FLAG_INR) * positif(TAXABASE_INR +NTAXABASE- TAXA_REF) 
		     * (
             (positif(TAXABASE_INR+NTAXABASE - max(TAXA_NTLDEC,TAXA_REF+0))
            * arr(max(0,min(TAXABASE_INR+NTAXABASE,TAXA_TLDEC_22) - max(TAXA_NTLDEC,TAXA_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(TAXABASE_INR+NTAXABASE - max(TAXA_NTLDEC,TAXA_REF+0))
            * arr(max(0,min(TAXABASE_INR+NTAXABASE,TAXA_TLDEC_22) - max(TAXA_NTLDEC,TAXA_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRCDIS_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(CDISBASE_INR+NCDISBASE - CDIS_REF) 
		       * (
             (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF+0))
            * arr(max(0,CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF+0)) 
            * arr(max(0,CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(CDISBASE_INR+NCDISBASE - CDIS_REF) 
		       * (
             (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF+0))
            * arr(max(0,min(CDISBASE_INR+NCDISBASE,CDIS_TLDEC_22) - max(CDIS_NTLDEC,CDIS_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF+0))
            * arr(max(0,min(CDISBASE_INR+NCDISBASE,CDIS_TLDEC_22) - max(CDIS_NTLDEC,CDIS_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRCHR_NTLPROV_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(CHRBASE_INR+NCHRBASE - CHR_REF) 
		       * (
             (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTLDEC,CHR_REF+0))
            * arr(max(0,CHRBASE_INR+NCHRBASE - max(CHR_NTL15,CHR_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTL15,CHR_REF+0)) 
            * arr(max(0,CHRBASE_INR+NCHRBASE - max(CHR_NTL15,CHR_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(CHRBASE_INR+NCHRBASE - CHR_REF) 
		       * (
             (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTL15,CHR_REF+0))
            * arr(max(0,min(CHRBASE_INR+NCHRBASE,CHR_TLDEC_22) - max(CHR_NTL15,CHR_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTL15,CHR_REF+0))
            * arr(max(0,min(CHRBASE_INR+NCHRBASE,CHR_TLDEC_22) - max(CHR_NTL15,CHR_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRCHR_NTL15_22 = (INRCHR_NTLPROV_22 - INRCHR_NTL10_22) * null(2 - FLAG_INR); 
INRCHR_NTL_22 = INRCHR_NTL10_22;
INRPCAP_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(PCAPBASE_INR +NPCAPBASE- PCAP_REF) 
		       * (
             (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF+0))
            * arr(max(0,PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(PCAPBASE_INR +NPCAPBASE- max(PCAP_NTLDEC,PCAP_REF+0)) 
            * arr(max(0,PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(PCAPBASE_INR+NPCAPBASE - PCAP_REF) 
		       * (
             (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF+0))
            * arr(max(0,min(PCAPBASE_INR+NPCAPBASE,PCAP_TLDEC_22) - max(PCAP_NTLDEC,PCAP_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF+0))
            * arr(max(0,min(PCAPBASE_INR+NPCAPBASE,PCAP_TLDEC_22) - max(PCAP_NTLDEC,PCAP_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE1_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE1BASE_INR+NRSE1BASE - RSE1_REF) 
		       * (
             (positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC,RSE1_REF+0))
            * arr(max(0,RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC,RSE1_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC,RSE1_REF+0)) 
            * arr(max(0,RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC,RSE1_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE1BASE_INR+NRSE1BASE - RSE1_REF) 
		       * (
             (positif(RSE1BASE_INR +NRSE1BASE- max(RSE1_NTLDEC,RSE1_REF+0))
            * arr(max(0,min(RSE1BASE_INR+NRSE1BASE,RSE1_TLDEC_22) - max(RSE1_NTLDEC,RSE1_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC,RSE1_REF+0))
            * arr(max(0,min(RSE1BASE_INR+NRSE1BASE,RSE1_TLDEC_22) - max(RSE1_NTLDEC,RSE1_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE2_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE2BASE_INR+NRSE2BASE - RSE2_REF) 
		       * (
             (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF+0))
            * arr(max(0,RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF+0)) 
            * arr(max(0,RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE2BASE_INR+NRSE2BASE - RSE2_REF) 
		       * (
             (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF+0))
            * arr(max(0,min(RSE2BASE_INR+NRSE2BASE,RSE2_TLDEC_22) - max(RSE2_NTLDEC,RSE2_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE2BASE_INR +NRSE2BASE- max(RSE2_NTLDEC,RSE2_REF+0))
            * arr(max(0,min(RSE2BASE_INR+NRSE2BASE,RSE2_TLDEC_22) - max(RSE2_NTLDEC,RSE2_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE3_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE3BASE_INR+NRSE3BASE - RSE3_REF) 
		       * (
             (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF+0))
            * arr(max(0,RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF+0)) 
            * arr(max(0,RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE3BASE_INR+NRSE3BASE - RSE3_REF) 
		       * (
             (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF+0))
            * arr(max(0,min(RSE3BASE_INR+NRSE3BASE,RSE3_TLDEC_22) - max(RSE3_NTLDEC,RSE3_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF+0))
            * arr(max(0,min(RSE3BASE_INR+NRSE3BASE,RSE3_TLDEC_22) - max(RSE3_NTLDEC,RSE3_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE4_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE4BASE_INR+NRSE4BASE - RSE4_REF) 
		       * (
             (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF+0))
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF+0)) 
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE4BASE_INR+NRSE4BASE - RSE4_REF) 
		       * (
             (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF+0))
            * arr(max(0,min(RSE4BASE_INR+NRSE4BASE,RSE4_TLDEC_22) - max(RSE4_NTLDEC,RSE4_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF+0))
            * arr(max(0,min(RSE4BASE_INR+NRSE4BASE,RSE4_TLDEC_22) - max(RSE4_NTLDEC,RSE4_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE5_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE5BASE_INR +NRSE5BASE - RSE5_REF) 
		       * (
             (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF+0))
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF+0)) 
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE5BASE_INR +NRSE5BASE - RSE5_REF) 
		       * (
             (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF+0))
            * arr(max(0,min(RSE5BASE_INR +NRSE5BASE,RSE5_TLDEC_22) - max(RSE5_NTLDEC,RSE5_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF+0))
            * arr(max(0,min(RSE5BASE_INR +NRSE5BASE,RSE5_TLDEC_22) - max(RSE5_NTLDEC,RSE5_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE6_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE6BASE_INR +NRSE6BASE - RSE6_REF) 
		       * (
             (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF+0))
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF+0)) 
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE6BASE_INR +NRSE6BASE - RSE6_REF) 
		       * (
             (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF+0))
            * arr(max(0,min(RSE6BASE_INR +NRSE6BASE,RSE6_TLDEC_22) - max(RSE6_NTLDEC,RSE6_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF+0))
            * arr(max(0,min(RSE6BASE_INR +NRSE6BASE,RSE6_TLDEC_22) - max(RSE6_NTLDEC,RSE6_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE8_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE8BASE_INR +NRSE8BASE - RSE8_REF) 
		       * (
             (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF+0))
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF+0)) 
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE8BASE_INR +NRSE8BASE - RSE8_REF) 
		       * (
             (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF+0))
            * arr(max(0,min(RSE8BASE_INR +NRSE8BASE,RSE8_TLDEC_22) - max(RSE8_NTLDEC,RSE8_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF+0))
            * arr(max(0,min(RSE8BASE_INR +NRSE8BASE,RSE8_TLDEC_22) - max(RSE8_NTLDEC,RSE8_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRCVN_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(CVNBASE_INR +NCVNBASE - CVN_REF) 
		       * (
             (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF+0))
            * arr(max(0,CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF+0)) 
            * arr(max(0,CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(CVNBASE_INR +NCVNBASE - CVN_REF) 
		       * (
             (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF+0))
            * arr(max(0,min(CVNBASE_INR +NCVNBASE,CVN_TLDEC_22) - max(CVN_NTLDEC,CVN_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF+0))
            * arr(max(0,min(CVNBASE_INR +NCVNBASE,CVN_TLDEC_22) - max(CVN_NTLDEC,CVN_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRGLO_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(GLOBASE_INR+NGLOBASE - GLO_REF) 
		       * (
             (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF+0))
            * arr(max(0,GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF+0)) 
            * arr(max(0,GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(GLOBASE_INR+NGLOBASE - GLO_REF) 
		       * (
             (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF+0))
            * arr(max(0,min(GLOBASE_INR+NGLOBASE,GLO_TLDEC_22) - max(GLO_NTLDEC,GLO_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF+0))
            * arr(max(0,min(GLOBASE_INR+NGLOBASE,GLO_TLDEC_22) - max(GLO_NTLDEC,GLO_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRC820_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(C820BASE_INR+NC820BASE - C820_REF) 
		       * (
             (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF+0))
            * arr(max(0,C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF+0)) 
            * arr(max(0,C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(C820BASE_INR+NC820BASE - C820_REF) 
		       * (
             (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF+0))
            * arr(max(0,min(C820BASE_INR+NC820BASE,C820_TLDEC_22) - max(C820_NTLDEC,C820_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF+0))
            * arr(max(0,min(C820BASE_INR+NC820BASE,C820_TLDEC_22) - max(C820_NTLDEC,C820_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRIFI_NTL_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(IFIBASE_INR - IFI_REF) 
		       * (
             (positif(IFIBASE_INR - max(IFI_NTLDEC,IFI_REF+0))
            * arr(max(0,IFIBASE_INR - max(IFI_NTLDEC,IFI_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IFIBASE_INR - max(IFI_NTLDEC,IFI_REF+0)) 
            * arr(max(0,IFIBASE_INR - max(IFI_NTLDEC,IFI_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(IFIBASE_INR - IFI_REF) 
		       * (
             (positif(IFIBASE_INR - max(IFI_NTLDEC,IFI_REF+0))
            * arr(max(0,min(IFIBASE_INR,IFI_TLDEC_22) - max(IFI_NTLDEC,IFI_REF)) * (TXINRRED22 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IFIBASE_INR - max(IFI_NTLDEC,IFI_REF+0))
            * arr(max(0,min(IFIBASE_INR,IFI_TLDEC_22) - max(IFI_NTLDEC,IFI_REF)) * (TXINRRED22 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
regle corrective 12190:
application : iliad ;
INRIR_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
			 null(2-FLAG_INR) * positif(IRNIN_INR+NIRNIN - IRNIN_REF)
                       * (
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_REF+0,IRNIN_NTLDEC)) 
            * arr((max(0,IRNIN_INR+NIRNIN) - max(IRNIN_REF,IRNIN_NTLDEC)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_NTLDEC,IRNIN_REF+0))
            * positif(IRNIN_INR+NIRNIN-max(IRNIN_NTLDEC,IRNIN_RECT))
            * arr(max(0,IRNIN_INR+NIRNIN - max(IRNIN_NTLDEC,IRNIN_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT+FLAG_RETARD) * positif(IND_PASSAGE - 1))
                             )
                   +  null(3-FLAG_INR) * positif(IRNIN_INR+NIRNIN- IRNIN_REF)
                       * (
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_REF+0,IRNIN_NTLDEC)) 
            * arr(max(0,min(IRNIN_INR+NIRNIN,IRNIN_TLDEC_24) - max(IRNIN_REF,IRNIN_NTLDEC)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IRNIN_INR+NIRNIN - max(IRNIN_NTLDEC,IRNIN_REF+0))
            * positif(IRNIN_INR+NIRNIN-max(IRNIN_NTLDEC,IRNIN_RECT))
            * arr(max(0,min(IRNIN_INR+NIRNIN,IRNIN_TLDEC_24) - max(IRNIN_NTLDEC,IRNIN_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT+FLAG_RETARD) * positif(IND_PASSAGE - 1))
                             )
                                                )
                                               ;
INRCSG_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
			 null(2 - FLAG_INR) * positif((CSBASE_INR+NCSBASE)-CSG_REF) 
			* (
            (positif((CSBASE_INR+NCSBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CSG_NTLDEC,CSG_REF+0))
            * arr(max(0,(CSBASE_INR+NCSBASE) - max(CSG_NTLDEC,CSG_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((CSBASE_INR+NCSBASE)* positif(CSG+PSOL+RDSN-SEUIL_61)  - max(CSG_NTLDEC,CSG_REF+0))
            * arr(max(0,(CSBASE_INR+NCSBASE) - max(CSG_NTLDEC,CSG_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                              )
                      + null(3 - FLAG_INR) * positif(CSBASE_INR+NCSBASE-CSG_REF) 
			* (
            (positif((CSBASE_INR+NCSBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CSG_NTLDEC,CSG_REF+0))
            * arr(max(0,min(max(0,CSBASE_INR+NCSBASE),CSG_TLDEC_24) - max(CSG_NTLDEC,CSG_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((CSBASE_INR+NCSBASE)* positif(CSG+PSOL+RDSN-SEUIL_61)  - max(CSG_NTLDEC,CSG_REF+0))
            * arr(max(0,min(max(0,CSBASE_INR+NCSBASE),CSG_TLDEC_24) - max(CSG_NTLDEC,CSG_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                              )
                            )
             ;
INRPSOL_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
			   null(2 - FLAG_INR) * positif((PSOLBASE_INR+NPSOLBASE)-PSOL_REF) 
			   * (
            (positif((PSOLBASE_INR+NPSOLBASE)* positif(CSG+PSOL+RDSN-SEUIL_61)  - max(PSOL_NTLDEC,PSOL_REF+0)) 
            * arr(max(0,(PSOLBASE_INR+NPSOLBASE)  - max(PSOL_NTLDEC,PSOL_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((PSOLBASE_INR+NPSOLBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(PSOL_NTLDEC,PSOL_REF+0))
            * arr(max(0,(PSOLBASE_INR+NPSOLBASE) - max(PSOL_NTLDEC,PSOL_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                        )
                        + null(3 - FLAG_INR) * positif(PSOLBASE_INR+NPSOLBASE-PSOL_REF) 
			   * (
            (positif((PSOLBASE_INR+NPSOLBASE)* positif(CSG+PSOL+RDSN-SEUIL_61)  - max(PSOL_NTLDEC,PSOL_REF+0)) 
            * arr(max(0,min(max(0,PSOLBASE_INR+NPSOLBASE),PSOL_TLDEC_24)  - max(PSOL_NTLDEC,PSOL_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((PSOLBASE_INR+NPSOLBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(PSOL_NTLDEC,PSOL_REF+0))
            * arr(max(0,min(max(0,PSOLBASE_INR+NPSOLBASE),PSOL_TLDEC_24) - max(PSOL_NTLDEC,PSOL_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                        )
                            )
             ;
INRCRDS_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif((RDBASE_INR+NRDBASE) - RDS_REF) 
		      * (
            (positif((RDBASE_INR+NRDBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CRDS_NTLDEC,RDS_REF+0))
            * arr(max(0,(RDBASE_INR+NRDBASE) - max(CRDS_NTLDEC,RDS_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((RDBASE_INR+NRDBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CRDS_NTLDEC,RDS_REF+0))
            * arr(max(0,(RDBASE_INR+NRDBASE) -max(CRDS_NTLDEC,RDS_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                         )
                    +  null(3 - FLAG_INR) * positif(RDBASE_INR+NRDBASE - RDS_REF) 
		      * (
            (positif((RDBASE_INR+NRDBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CRDS_NTLDEC,RDS_REF+0))
            * arr(max(0,min(max(0,RDBASE_INR+NRDBASE),RDS_TLDEC_24) - max(CRDS_NTLDEC,RDS_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif((RDBASE_INR+NRDBASE) * positif(CSG+PSOL+RDSN-SEUIL_61) - max(CRDS_NTLDEC,RDS_REF+0))
            * arr(max(0,min(max(0,RDBASE_INR+NRDBASE),RDS_TLDEC_24) -max(CRDS_NTLDEC,RDS_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                         )
                            )
             ;
INRTAXA_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		     null(2 - FLAG_INR) * positif(TAXABASE_INR +NTAXABASE- TAXA_REF) 
		     * (
             (positif(TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC,TAXA_REF+0))
            * arr(max(0,TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC,TAXA_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC,TAXA_REF+0))
            * arr(max(0,TAXABASE_INR+NTAXABASE - max(TAXA_NTLDEC,TAXA_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		     + null(3 - FLAG_INR) * positif(TAXABASE_INR+NTAXABASE - TAXA_REF) 
		     * (
             (positif(TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC,TAXA_REF+0))
            * arr(max(0,min(TAXABASE_INR+NTAXABASE,TAXA_TLDEC_24) - max(TAXA_NTLDEC,TAXA_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(TAXABASE_INR +NTAXABASE- max(TAXA_NTLDEC,TAXA_REF+0))
            * arr(max(0,min(TAXABASE_INR+NTAXABASE,TAXA_TLDEC_24) - max(TAXA_NTLDEC,TAXA_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRCDIS_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(CDISBASE_INR+NCDISBASE - CDIS_REF) 
		       * (
             (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF+0))
            * arr(max(0,CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF+0)) 
            * arr(max(0,CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(CDISBASE_INR+NCDISBASE - CDIS_REF) 
		       * (
             (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF+0))
            * arr(max(0,min(CDISBASE_INR+NCDISBASE,CDIS_TLDEC_24) - max(CDIS_NTLDEC,CDIS_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CDISBASE_INR+NCDISBASE - max(CDIS_NTLDEC,CDIS_REF+0))
            * arr(max(0,min(CDISBASE_INR+NCDISBASE,CDIS_TLDEC_24) - max(CDIS_NTLDEC,CDIS_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRCHR_NTLPROV_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(CHRBASE_INR+NCHRBASE - CHR_REF) 
		       * (
             (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTLDEC,CHR_REF+0))
            * arr(max(0,CHRBASE_INR +NCHRBASE- max(CHR_NTL15,CHR_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTL15,CHR_REF+0)) 
            * arr(max(0,CHRBASE_INR+NCHRBASE- max(CHR_NTL15,CHR_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(CHRBASE_INR+NCHRBASE - CHR_REF) 
		       * (
             (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTL15,CHR_REF+0))
            * arr(max(0,min(CHRBASE_INR+NCHRBASE,CHR_TLDEC_24) - max(CHR_NTL15,CHR_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CHRBASE_INR+NCHRBASE - max(CHR_NTL15,CHR_REF+0))
            * arr(max(0,min(CHRBASE_INR+NCHRBASE,CHR_TLDEC_24) - max(CHR_NTL15,CHR_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRCHR_NTL15_24 = (INRCHR_NTLPROV_24 - INRCHR_NTL10_24) * null(2 - FLAG_INR); 
INRCHR_NTL_24 = INRCHR_NTL10_24;
INRPCAP_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(PCAPBASE_INR +NPCAPBASE- PCAP_REF) 
		       * (
             (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF+0))
            * arr(max(0,PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF+0)) 
            * arr(max(0,PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(PCAPBASE_INR+NPCAPBASE - PCAP_REF) 
		       * (
             (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF+0))
            * arr(max(0,min(PCAPBASE_INR+NPCAPBASE,PCAP_TLDEC_24) - max(PCAP_NTLDEC,PCAP_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_NTLDEC,PCAP_REF+0))
            * arr(max(0,min(PCAPBASE_INR+NPCAPBASE,PCAP_TLDEC_24) - max(PCAP_NTLDEC,PCAP_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE1_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE1BASE_INR+NRSE1BASE - RSE1_REF) 
		       * (
             (positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC,RSE1_REF+0))
            * arr(max(0,RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC,RSE1_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE1BASE_INR +NRSE1BASE- max(RSE1_NTLDEC,RSE1_REF+0)) 
            * arr(max(0,RSE1BASE_INR +NRSE1BASE- max(RSE1_NTLDEC,RSE1_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE1BASE_INR+NRSE1BASE - RSE1_REF) 
		       * (
             (positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC,RSE1_REF+0))
            * arr(max(0,min(RSE1BASE_INR+NRSE1BASE,RSE1_TLDEC_24) - max(RSE1_NTLDEC,RSE1_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_NTLDEC,RSE1_REF+0))
            * arr(max(0,min(RSE1BASE_INR+NRSE1BASE,RSE1_TLDEC_24) - max(RSE1_NTLDEC,RSE1_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE2_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE2BASE_INR+NRSE2BASE - RSE2_REF) 
		       * (
             (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF+0))
            * arr(max(0,RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE2BASE_INR +NRSE2BASE- max(RSE2_NTLDEC,RSE2_REF+0)) 
            * arr(max(0,RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE2BASE_INR +NRSE2BASE- RSE2_REF) 
		       * (
             (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF+0))
            * arr(max(0,min(RSE2BASE_INR+NRSE2BASE,RSE2_TLDEC_24) - max(RSE2_NTLDEC,RSE2_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_NTLDEC,RSE2_REF+0))
            * arr(max(0,min(RSE2BASE_INR+NRSE2BASE,RSE2_TLDEC_24) - max(RSE2_NTLDEC,RSE2_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE3_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE3BASE_INR +NRSE3BASE- RSE3_REF) 
		       * (
             (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF+0))
            * arr(max(0,RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF+0)) 
            * arr(max(0,RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE3BASE_INR +NRSE3BASE- RSE3_REF) 
		       * (
             (positif(RSE3BASE_INR +NRSE3BASE- max(RSE3_NTLDEC,RSE3_REF+0))
            * arr(max(0,min(RSE3BASE_INR+NRSE3BASE,RSE3_TLDEC_24) - max(RSE3_NTLDEC,RSE3_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE3BASE_INR+NRSE3BASE - max(RSE3_NTLDEC,RSE3_REF+0))
            * arr(max(0,min(RSE3BASE_INR+NRSE3BASE,RSE3_TLDEC_24) - max(RSE3_NTLDEC,RSE3_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE4_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE4BASE_INR+NRSE4BASE - RSE4_REF) 
		       * (
             (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF+0))
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF+0)) 
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE4BASE_INR+NRSE4BASE - RSE4_REF) 
		       * (
             (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF+0))
            * arr(max(0,min(RSE4BASE_INR+NRSE4BASE,RSE4_TLDEC_24) - max(RSE4_NTLDEC,RSE4_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_NTLDEC,RSE4_REF+0))
            * arr(max(0,min(RSE4BASE_INR+NRSE4BASE,RSE4_TLDEC_24) - max(RSE4_NTLDEC,RSE4_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE5_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE5BASE_INR +NRSE5BASE - RSE5_REF) 
		       * (
             (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF+0))
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF+0)) 
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE5BASE_INR +NRSE5BASE - RSE5_REF) 
		       * (
             (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF+0))
            * arr(max(0,min(RSE5BASE_INR +NRSE5BASE,RSE5_TLDEC_24) - max(RSE5_NTLDEC,RSE5_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_NTLDEC,RSE5_REF+0))
            * arr(max(0,min(RSE5BASE_INR +NRSE5BASE,RSE5_TLDEC_24) - max(RSE5_NTLDEC,RSE5_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE6_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE6BASE_INR +NRSE6BASE - RSE6_REF) 
		       * (
             (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF+0))
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF+0)) 
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE6BASE_INR +NRSE6BASE - RSE6_REF) 
		       * (
             (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF+0))
            * arr(max(0,min(RSE6BASE_INR +NRSE6BASE,RSE6_TLDEC_24) - max(RSE6_NTLDEC,RSE6_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_NTLDEC,RSE6_REF+0))
            * arr(max(0,min(RSE6BASE_INR +NRSE6BASE,RSE6_TLDEC_24) - max(RSE6_NTLDEC,RSE6_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRRSE8_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(RSE8BASE_INR +NRSE8BASE - RSE8_REF) 
		       * (
             (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF+0))
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF+0)) 
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(RSE8BASE_INR +NRSE8BASE - RSE8_REF) 
		       * (
             (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF+0))
            * arr(max(0,min(RSE8BASE_INR +NRSE8BASE,RSE8_TLDEC_24) - max(RSE8_NTLDEC,RSE8_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_NTLDEC,RSE8_REF+0))
            * arr(max(0,min(RSE8BASE_INR +NRSE8BASE,RSE8_TLDEC_24) - max(RSE8_NTLDEC,RSE8_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRCVN_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(CVNBASE_INR +NCVNBASE - CVN_REF) 
		       * (
             (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF+0))
            * arr(max(0,CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF+0)) 
            * arr(max(0,CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(CVNBASE_INR +NCVNBASE - CVN_REF) 
		       * (
             (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF+0))
            * arr(max(0,min(CVNBASE_INR +NCVNBASE,CVN_TLDEC_24) - max(CVN_NTLDEC,CVN_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(CVNBASE_INR +NCVNBASE - max(CVN_NTLDEC,CVN_REF+0))
            * arr(max(0,min(CVNBASE_INR +NCVNBASE,CVN_TLDEC_24) - max(CVN_NTLDEC,CVN_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRGLO_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(GLOBASE_INR+NGLOBASE - GLO_REF) 
		       * (
             (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF+0))
            * arr(max(0,GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF+0)) 
            * arr(max(0,GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(GLOBASE_INR+NGLOBASE - GLO_REF) 
		       * (
             (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF+0))
            * arr(max(0,min(GLOBASE_INR+NGLOBASE,GLO_TLDEC_24) - max(GLO_NTLDEC,GLO_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(GLOBASE_INR+NGLOBASE - max(GLO_NTLDEC,GLO_REF+0))
            * arr(max(0,min(GLOBASE_INR+NGLOBASE,GLO_TLDEC_24) - max(GLO_NTLDEC,GLO_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRC820_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(C820BASE_INR+NC820BASE - C820_REF) 
		       * (
             (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF+0))
            * arr(max(0,C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF+0)) 
            * arr(max(0,C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(C820BASE_INR+NC820BASE - C820_REF) 
		       * (
             (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF+0))
            * arr(max(0,min(C820BASE_INR+NC820BASE,C820_TLDEC_24) - max(C820_NTLDEC,C820_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(C820BASE_INR+NC820BASE - max(C820_NTLDEC,C820_REF+0))
            * arr(max(0,min(C820BASE_INR+NC820BASE,C820_TLDEC_24) - max(C820_NTLDEC,C820_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
INRIFI_NTL_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * (
		       null(2 - FLAG_INR) * positif(IFIBASE_INR - IFI_REF) 
		       * (
             (positif(IFIBASE_INR - max(IFI_NTLDEC,IFI_REF+0))
            * arr(max(0,IFIBASE_INR - max(IFI_NTLDEC,IFI_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IFIBASE_INR - max(IFI_NTLDEC,IFI_REF+0)) 
            * arr(max(0,IFIBASE_INR - max(IFI_NTLDEC,IFI_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
		       + null(3 - FLAG_INR) * positif(IFIBASE_INR - IFI_REF) 
		       * (
             (positif(IFIBASE_INR - max(IFI_NTLDEC,IFI_REF+0))
            * arr(max(0,min(IFIBASE_INR,IFI_TLDEC_24) - max(IFI_NTLDEC,IFI_REF)) * (TXINRRED24 / 100))
            * null(FLAG_DEFAUT + FLAG_RETARD))
            +
            (positif(IFIBASE_INR - max(IFI_NTLDEC,IFI_REF+0))
            * arr(max(0,min(IFIBASE_INR,IFI_TLDEC_24) - max(IFI_NTLDEC,IFI_REF)) * (TXINRRED24 / 100))
            * positif(FLAG_DEFAUT + FLAG_RETARD) * positif(IND_PASSAGE - 1))
                                                             )
                            )
	     ; 
regle corrective 1082:
application :  iliad ;
INRIR_TLACQ = (positif(IRNIN_INR + NIRNIN *positif(IRNIN_INR+NIRNIN) - max(max(IRNIN_REF , max(0,IRNIN_RECT)), IRNIN_NTLDEC_24))
                                                                                                              * null(3 - FLAG_INR)
            * arr(max(0 ,IRNIN_INR+ NIRNIN *positif(IRNIN_INR+NIRNIN) - max(max(IRNIN_REF , IRNIN_RECT), IRNIN_NTLDEC_24))
                                                                                              * (TXINR / 100)) );
INRIR_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRIR_TLACQ;
INRCSG_TLACQ = positif(CSBASE_INR+NCSBASE*positif(CSBASE_INR+NCSBASE)  - max(CSG_REF, CSG_NTLDEC_24 )) * null(3 - FLAG_INR)
            * arr((max(0,CSBASE_INR+NCSBASE*positif(CSBASE_INR+NCSBASE) ) - max(CSG_REF, CSG_NTLDEC_24)) * (TXINR / 100));
INRCSG_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCSG_TLACQ;
INRPSOL_TLACQ = positif(PSOLBASE_INR+NPSOLBASE*positif(PSOLBASE_INR+NPSOLBASE) - max(PSOL_REF, PSOL_NTLDEC_24)) * null(3 - FLAG_INR)
            * arr((max(0,PSOLBASE_INR+NPSOLBASE*positif(PSOLBASE_INR+NPSOLBASE) ) - max(PSOL_REF, PSOL_NTLDEC_24)) * (TXINR / 100))  ;
INRPSOL_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRPSOL_TLACQ;
INRCRDS_TLACQ = positif(RDBASE_INR+NRDBASE*positif(RDBASE_INR+NRDBASE) - max(RDS_REF, CRDS_NTLDEC_24)) * null(3 - FLAG_INR)
            * arr((max(0,RDBASE_INR+NRDBASE*positif(RDBASE_INR+NRDBASE)) - max(RDS_REF, CRDS_NTLDEC_24)) * (TXINR / 100))  ;
INRCRDS_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCRDS_TLACQ;
INRTAXA_TLACQ = positif(TAXABASE_INR+NTAXABASE - max(TAXA_REF, TAXA_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,TAXABASE_INR+NTAXABASE - max(TAXA_REF, TAXA_NTLDEC_24)) * (TXINR / 100))  ;
INRTAXA_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRTAXA_TLACQ;
INRCDIS_TLACQ = positif(CDISBASE_INR+NCDISBASE - max(CDIS_REF, CDIS_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,CDISBASE_INR+NCDISBASE - max(CDIS_REF, CDIS_NTLDEC_24)) * (TXINR / 100))  ;
INRCDIS_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCDIS_TLACQ;
INRCHR_TLACQ = positif(CHRBASE_INR +NCHRBASE- max(CHR_REF, CHR_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,CHRBASE_INR +NCHRBASE- max(CHR_REF, CHR_NTLDEC_24)) * (TXINR / 100))  ;
INRCHR_TLA = (1 - IND_RJLJ) * ((1-FLAG_NINR) * INRCHR_TLACQ+INRCHR_TL15);
INRPCAP_TLACQ = positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_REF, PCAP_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,PCAPBASE_INR +NPCAPBASE- max(PCAP_REF, PCAP_NTLDEC_24)) * (TXINR / 100))  ;
INRPCAP_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRPCAP_TLACQ;
INRRSE1_TLACQ = positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_REF, RSE1_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,RSE1BASE_INR+NRSE1BASE - max(RSE1_REF, RSE1_NTLDEC_24)) * (TXINR / 100))  ;
INRRSE1_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE1_TLACQ;
INRRSE2_TLACQ = positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_REF, RSE2_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,RSE2BASE_INR+NRSE2BASE - max(RSE2_REF, RSE2_NTLDEC_24)) * (TXINR / 100))  ;
INRRSE2_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE2_TLACQ;
INRRSE3_TLACQ = positif(RSE3BASE_INR+NRSE3BASE - max(RSE3_REF, RSE3_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,RSE3BASE_INR+NRSE3BASE - max(RSE3_REF, RSE3_NTLDEC_24)) * (TXINR / 100))  ;
INRRSE3_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE3_TLACQ;
INRRSE4_TLACQ = positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_REF, RSE4_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - max(RSE4_REF, RSE4_NTLDEC_24)) * (TXINR / 100))  ;
INRRSE4_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE4_TLACQ;
INRRSE5_TLACQ = positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_REF, RSE5_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - max(RSE5_REF, RSE5_NTLDEC_24)) * (TXINR / 100))  ;
INRRSE5_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE5_TLACQ;
INRRSE6_TLACQ = positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_REF, RSE6_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - max(RSE6_REF, RSE6_NTLDEC_24)) * (TXINR / 100))  ;
INRRSE6_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE6_TLACQ;
INRRSE8_TLACQ = positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_REF, RSE8_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - max(RSE8_REF, RSE8_NTLDEC_24)) * (TXINR / 100))  ;
INRRSE8_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE8_TLACQ;
INRCVN_TLACQ = positif(CVNBASE_INR +NCVNBASE - max(CVN_REF, CVN_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,CVNBASE_INR +NCVNBASE - max(CVN_REF, CVN_NTLDEC_24)) * (TXINR / 100))  ;
INRCVN_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCVN_TLACQ;
INRGLO_TLACQ = positif(GLOBASE_INR+NGLOBASE - max(GLO_REF,GLO_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,GLOBASE_INR+NGLOBASE - max(GLO_REF, GLO_NTLDEC_24)) * (TXINR / 100))  ;
INRGLO_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRGLO_TLACQ;
INRC820_TLACQ = positif(C820BASE_INR+NC820BASE - max(C820_REF, C820_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,C820BASE_INR+NC820BASE - max(C820_REF, C820_NTLDEC_24)) * (TXINR / 100))  ;
INRC820_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRC820_TLACQ;
INRIFI_TLACQ = positif(IFIBASE_INR - max(IFI_REF, IFI_NTLDEC_24))*null(3- FLAG_INR)
            * arr(max(0,IFIBASE_INR - max(IFI_REF,IFI_NTLDEC_24)) * (TXINRISF / 100))  ;
INRIFI_TLA = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRIFI_TLACQ;
regle corrective 108212:
application : iliad ;

INRIR_TLACQ_22 = (positif(IRNIN_INR+NIRNIN*positif(IRNIN_INR+NIRNIN) - max(max(IRNIN_REF , IRNIN_RECT),IRNIN_TLDEC+0)) * null(3-FLAG_INR)
            * arr(max(0,IRNIN_INR+NIRNIN *positif(IRNIN_INR+NIRNIN)- max(max(IRNIN_REF ,IRNIN_RECT),IRNIN_TLDEC)) * (TXINRRED22 / 100)) * (1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(IRNIN_INR+NIRNIN *positif(IRNIN_INR+NIRNIN)- IRNIN_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,IRNIN_INR+NIRNIN *positif(IRNIN_INR+NIRNIN)- IRNIN_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02));
INRIR_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRIR_TLACQ_22;
INRCSG_TLACQ_22 = positif(CSBASE_INR+NCSBASE*positif(CSBASE_INR+NCSBASE) - max(CSG_REF,CSG_TLDEC+0)) * null(3 - FLAG_INR)
            * arr(max(0,CSBASE_INR+NCSBASE*positif(CSBASE_INR+NCSBASE) - max(CSG_REF,CSG_TLDEC)) * (TXINRRED22 / 100)) * (1 - positif(FLAG_C22+FLAG_C02))
	    +
               positif(CSBASE_INR+NCSBASE *positif(CSBASE_INR+NCSBASE)- CSG_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,CSBASE_INR+NCSBASE *positif(CSBASE_INR+NCSBASE)- CSG_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCSG_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCSG_TLACQ_22;
INRPSOL_TLACQ_22 = positif(PSOLBASE_INR+NPSOLBASE*positif(PSOLBASE_INR+NPSOLBASE)- max(PSOL_REF,PSOL_TLDEC+0)) * null(3 - FLAG_INR)
            * arr(max(0,PSOLBASE_INR+NPSOLBASE *positif(PSOLBASE_INR+NPSOLBASE)- max(PSOL_REF,PSOL_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(PSOLBASE_INR+NPSOLBASE*positif(PSOLBASE_INR+NPSOLBASE) - PSOL_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,PSOLBASE_INR+NPSOLBASE*positif(PSOLBASE_INR+NPSOLBASE) - PSOL_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRPSOL_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRPSOL_TLACQ_22;
INRCRDS_TLACQ_22 = positif(RDBASE_INR+NRDBASE*positif(RDBASE_INR+NRDBASE) - max(RDS_REF,RDS_TLDEC+0)) * null(3 - FLAG_INR)
            * arr(max(0,RDBASE_INR+NRDBASE*positif(RDBASE_INR+NRDBASE)- max(RDS_REF,RDS_TLDEC)) * (TXINRRED22 / 100))* (1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RDBASE_INR+NRDBASE *positif(RDBASE_INR+NRDBASE)- RDS_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RDBASE_INR+NRDBASE*positif(RDBASE_INR+NRDBASE)- RDS_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCRDS_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCRDS_TLACQ_22;
INRTAXA_TLACQ_22 = positif(TAXABASE_INR +NTAXABASE- max(TAXA_REF,TAXA_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,TAXABASE_INR+NTAXABASE - max(TAXA_REF,TAXA_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(TAXABASE_INR+NTAXABASE - TAXA_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,TAXABASE_INR +NTAXABASE- TAXA_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRTAXA_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRTAXA_TLACQ_22;
INRCDIS_TLACQ_22 = positif(CDISBASE_INR+NCDISBASE - max(CDIS_REF,CDIS_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,CDISBASE_INR+NCDISBASE - max(CDIS_REF,CDIS_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(CDISBASE_INR+NCDISBASE - CDIS_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,CDISBASE_INR+NCDISBASE - CDIS_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCDIS_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCDIS_TLACQ_22;
INRCHR_TLACQ_22 = positif(CHRBASE_INR+NCHRBASE - max(CHR_REF,CHR_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,CHRBASE_INR +NCHRBASE- max(CHR_REF,CHR_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(CHRBASE_INR+NCHRBASE - CHR_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,CHRBASE_INR+NCHRBASE - CHR_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCHR_TLA_22 = (1 - IND_RJLJ) * ((1-FLAG_NINR) * INRCHR_TLACQ_22+INRCHR_TL15_22);
INRPCAP_TLACQ_22 = positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_REF,PCAP_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,PCAPBASE_INR+NPCAPBASE - max(PCAP_REF,PCAP_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(PCAPBASE_INR+NPCAPBASE- PCAP_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,PCAPBASE_INR+NPCAPBASE - PCAP_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRPCAP_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRPCAP_TLACQ_22;
INRRSE1_TLACQ_22 = positif(RSE1BASE_INR +NRSE1BASE- max(RSE1_REF,RSE1_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE1BASE_INR+NRSE1BASE - max(RSE1_REF,RSE1_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE1BASE_INR+NRSE1BASE - RSE1_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE1BASE_INR+NRSE1BASE - RSE1_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE1_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE1_TLACQ_22;
INRRSE2_TLACQ_22 = positif(RSE2BASE_INR +NRSE2BASE- max(RSE2_REF,RSE2_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE2BASE_INR+NRSE2BASE - max(RSE2_REF,RSE2_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE2BASE_INR +NRSE2BASE- RSE2_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE2BASE_INR+NRSE2BASE - RSE2_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE2_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE2_TLACQ_22;
INRRSE3_TLACQ_22 = positif(RSE3BASE_INR+NRSE3BASE - max(RSE3_REF,RSE3_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE3BASE_INR+NRSE3BASE - max(RSE3_REF,RSE3_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE3BASE_INR+NRSE3BASE - RSE3_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE3BASE_INR +NRSE3BASE- RSE3_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE3_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE3_TLACQ_22;
INRRSE4_TLACQ_22 = positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_REF,RSE4_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - max(RSE4_REF,RSE4_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE4BASE_INR+NRSE4BASE - RSE4_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - RSE4_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE4_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE4_TLACQ_22;
INRRSE5_TLACQ_22 = positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_REF,RSE5_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - max(RSE5_REF,RSE5_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE5BASE_INR +NRSE5BASE - RSE5_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - RSE5_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE5_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE5_TLACQ_22;
INRRSE6_TLACQ_22 = positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_REF,RSE6_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - max(RSE6_REF,RSE6_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE6BASE_INR +NRSE6BASE - RSE6_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - RSE6_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE6_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE6_TLACQ_22;
INRRSE8_TLACQ_22 = positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_REF,RSE8_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - max(RSE8_REF,RSE8_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE8BASE_INR +NRSE8BASE - RSE8_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - RSE8_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE8_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE8_TLACQ_22;
INRCVN_TLACQ_22 = positif(CVNBASE_INR +NCVNBASE - max(CVN_REF,CVN_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,CVNBASE_INR +NCVNBASE - max(CVN_REF,CVN_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(CVNBASE_INR +NCVNBASE - CVN_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,CVNBASE_INR +NCVNBASE - CVN_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCVN_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCVN_TLACQ_22;
INRGLO_TLACQ_22 = positif(GLOBASE_INR+NGLOBASE - max(GLO_REF,GLO_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,GLOBASE_INR+NGLOBASE - max(GLO_REF,GLO_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(GLOBASE_INR+NGLOBASE - GLO_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,GLOBASE_INR+NGLOBASE - GLO_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRGLO_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRGLO_TLACQ_22;
INRC820_TLACQ_22 = positif(C820BASE_INR+NC820BASE - max(C820_REF,C820_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,C820BASE_INR+NC820BASE - max(C820_REF,C820_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(C820BASE_INR+NC820BASE - C820_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,C820BASE_INR+NC820BASE - C820_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRC820_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRC820_TLACQ_22;
INRIFI_TLACQ_22 = positif(IFIBASE_INR - max(IFI_REF,IFI_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,IFIBASE_INR - max(IFI_REF,IFI_TLDEC)) * (TXINRRED22 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
              positif(IFIBASE_INR - IFI_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,IFIBASE_INR - IFI_TLDEC) * (TXINRRED22 / 100)) * positif(FLAG_C22+FLAG_C02);
INRIFI_TLA_22 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRIFI_TLACQ_22;
regle corrective 1082121:
application : iliad ;
INRIR_TLACQ_24 = (positif(IRNIN_INR+NIRNIN*positif(IRNIN_INR+NIRNIN)- max(max(IRNIN_REF , IRNIN_RECT),IRNIN_TLDEC)) * null(3-FLAG_INR)
            * arr(max(0,IRNIN_INR+NIRNIN *positif(IRNIN_INR+NIRNIN)- max(max(IRNIN_REF ,IRNIN_RECT),IRNIN_TLDEC)) * (TXINRRED24 / 100)) * (1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(IRNIN_INR+NIRNIN*positif(IRNIN_INR+NIRNIN) - IRNIN_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,IRNIN_INR+NIRNIN *positif(IRNIN_INR+NIRNIN)- IRNIN_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02));
INRIR_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRIR_TLACQ_24;
INRCSG_TLACQ_24 = positif(CSBASE_INR+NCSBASE*positif(CSBASE_INR+NCSBASE) - max(CSG_REF,CSG_TLDEC+0)) * null(3 - FLAG_INR)
            * arr(max(0,CSBASE_INR+NCSBASE*positif(CSBASE_INR+NCSBASE) - max(CSG_REF,CSG_TLDEC)) * (TXINRRED24 / 100)) * (1 - positif(FLAG_C22+FLAG_C02))
	    +
               positif(CSBASE_INR+NCSBASE*positif(CSBASE_INR+NCSBASE) - CSG_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,CSBASE_INR+NCSBASE*positif(CSBASE_INR+NCSBASE) - CSG_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCSG_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCSG_TLACQ_24;
INRPSOL_TLACQ_24 = positif(PSOLBASE_INR+NPSOLBASE*positif(PSOLBASE_INR+NPSOLBASE) - max(PSOL_REF,PSOL_TLDEC+0)) * null(3 - FLAG_INR)
            * arr(max(0,PSOLBASE_INR+NPSOLBASE *positif(PSOLBASE_INR+NPSOLBASE)- max(PSOL_REF,PSOL_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(PSOLBASE_INR+NPSOLBASE *positif(PSOLBASE_INR+NPSOLBASE)- PSOL_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,PSOLBASE_INR+NPSOLBASE*positif(PSOLBASE_INR+NPSOLBASE) - PSOL_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRPSOL_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRPSOL_TLACQ_24;
INRCRDS_TLACQ_24 = positif(RDBASE_INR+NRDBASE*positif(RDBASE_INR+NRDBASE) - max(RDS_REF,RDS_TLDEC+0)) * null(3 - FLAG_INR)
            * arr(max(0,RDBASE_INR+NRDBASE*positif(RDBASE_INR+NRDBASE) - max(RDS_REF,RDS_TLDEC)) * (TXINRRED24 / 100))* (1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RDBASE_INR+NRDBASE*positif(RDBASE_INR+NRDBASE) - RDS_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RDBASE_INR+NRDBASE *positif(RDBASE_INR+NRDBASE)- RDS_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCRDS_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCRDS_TLACQ_24;
INRTAXA_TLACQ_24 = positif(TAXABASE_INR+NTAXABASE - max(TAXA_REF,TAXA_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,TAXABASE_INR +NTAXABASE- max(TAXA_REF,TAXA_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(TAXABASE_INR+NTAXABASE - TAXA_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,TAXABASE_INR+NTAXABASE - TAXA_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRTAXA_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRTAXA_TLACQ_24;
INRCDIS_TLACQ_24 = positif(CDISBASE_INR+NCDISBASE - max(CDIS_REF,CDIS_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,CDISBASE_INR+NCDISBASE - max(CDIS_REF,CDIS_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(CDISBASE_INR+NCDISBASE - CDIS_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,CDISBASE_INR+NCDISBASE - CDIS_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCDIS_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCDIS_TLACQ_24;
INRCHR_TLACQ_24 = positif(CHRBASE_INR+NCHRBASE - max(CHR_REF,CHR_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,CHRBASE_INR+NCHRBASE - max(CHR_REF,CHR_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(CHRBASE_INR +NCHRBASE- CHR_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,CHRBASE_INR+NCHRBASE - CHR_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCHR_TLA_24 = (1 - IND_RJLJ) * ((1-FLAG_NINR) * INRCHR_TLACQ_24+INRCHR_TL15_24);
INRPCAP_TLACQ_24 = positif(PCAPBASE_INR+NPCAPBASE - max(PCAP_REF,PCAP_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,PCAPBASE_INR+NPCAPBASE - max(PCAP_REF,PCAP_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(PCAPBASE_INR+NPCAPBASE - PCAP_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,PCAPBASE_INR +NPCAPBASE- PCAP_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRPCAP_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRPCAP_TLACQ_24;
INRRSE1_TLACQ_24 = positif(RSE1BASE_INR+NRSE1BASE - max(RSE1_REF,RSE1_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE1BASE_INR+NRSE1BASE - max(RSE1_REF,RSE1_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE1BASE_INR+NRSE1BASE - RSE1_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE1BASE_INR+NRSE1BASE - RSE1_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE1_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE1_TLACQ_24;
INRRSE2_TLACQ_24 = positif(RSE2BASE_INR+NRSE2BASE - max(RSE2_REF,RSE2_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE2BASE_INR +NRSE2BASE- max(RSE2_REF,RSE2_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE2BASE_INR+NRSE2BASE - RSE2_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE2BASE_INR+NRSE2BASE - RSE2_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE2_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE2_TLACQ_24;
INRRSE3_TLACQ_24 = positif(RSE3BASE_INR+NRSE3BASE - max(RSE3_REF,RSE3_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE3BASE_INR +NRSE3BASE- max(RSE3_REF,RSE3_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE3BASE_INR +NRSE3BASE- RSE3_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE3BASE_INR +NRSE3BASE- RSE3_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE3_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE3_TLACQ_24;
INRRSE4_TLACQ_24 = positif(RSE4BASE_INR+NRSE4BASE - max(RSE4_REF,RSE4_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - max(RSE4_REF,RSE4_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE4BASE_INR+NRSE4BASE - RSE4_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE4BASE_INR+NRSE4BASE - RSE4_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE4_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE4_TLACQ_24;
INRRSE5_TLACQ_24 = positif(RSE5BASE_INR +NRSE5BASE - max(RSE5_REF,RSE5_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - max(RSE5_REF,RSE5_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE5BASE_INR +NRSE5BASE - RSE5_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE5BASE_INR +NRSE5BASE - RSE5_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE5_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE5_TLACQ_24;
INRRSE6_TLACQ_24 = positif(RSE6BASE_INR +NRSE6BASE - max(RSE6_REF,RSE6_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - max(RSE6_REF,RSE6_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE6BASE_INR +NRSE6BASE - RSE6_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE6BASE_INR +NRSE6BASE - RSE6_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE6_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE6_TLACQ_24;
INRRSE8_TLACQ_24 = positif(RSE8BASE_INR +NRSE8BASE - max(RSE8_REF,RSE8_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - max(RSE8_REF,RSE8_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(RSE8BASE_INR +NRSE8BASE - RSE8_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,RSE8BASE_INR +NRSE8BASE - RSE8_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRRSE8_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRRSE8_TLACQ_24;
INRCVN_TLACQ_24 = positif(CVNBASE_INR +NCVNBASE - max(CVN_REF,CVN_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,CVNBASE_INR +NCVNBASE - max(CVN_REF,CVN_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(CVNBASE_INR +NCVNBASE - CVN_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,CVNBASE_INR +NCVNBASE - CVN_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRCVN_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRCVN_TLACQ_24;
INRGLO_TLACQ_24 = positif(GLOBASE_INR+NGLOBASE - max(GLO_REF,GLO_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,GLOBASE_INR+NGLOBASE - max(GLO_REF,GLO_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(GLOBASE_INR+NGLOBASE - GLO_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,GLOBASE_INR+NGLOBASE - GLO_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRGLO_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRGLO_TLACQ_24;
INRC820_TLACQ_24 = positif(C820BASE_INR+NC820BASE - max(C820_REF,C820_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,C820BASE_INR+NC820BASE - max(C820_REF,C820_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
               positif(C820BASE_INR+NC820BASE - C820_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,C820BASE_INR+NC820BASE - C820_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRC820_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRC820_TLACQ_24;
INRIFI_TLACQ_24 = positif(IFIBASE_INR - max(IFI_REF,IFI_TLDEC+0))*null(3- FLAG_INR)
            * arr(max(0,IFIBASE_INR - max(IFI_REF,IFI_TLDEC)) * (TXINRRED24 / 100))*(1-positif(FLAG_C22+FLAG_C02))
	    +
              positif(IFIBASE_INR - IFI_TLDEC) * null(3-FLAG_INR)
            * arr(max(0,IFIBASE_INR - IFI_TLDEC) * (TXINRRED24 / 100)) * positif(FLAG_C22+FLAG_C02);
INRIFI_TLA_24 = (1 - IND_RJLJ) * (1-FLAG_NINR) * INRIFI_TLACQ_24;
regle corrective 1083:
application :  iliad ;

INRIR_TLADEC_22 = INRIR_TLACQ_22;
INRIR_TL_22_AD=INRIR_TL_22_A;
INRCSG_TLADEC_22 = INRCSG_TLACQ_22;
INRCSG_TL_22_AD = INRCSG_TL_22_A;
INRPSOL_TLADEC_22 = INRPSOL_TLACQ_22;
INRPSOL_TL_22_AD = INRPSOL_TL_22_A;
INRCRDS_TLADEC_22 = INRCRDS_TLACQ_22;
INRCRDS_TL_22_AD = INRCRDS_TL_22_A;
INRTAXA_TLADEC_22 = INRTAXA_TLACQ_22;
INRTAXA_TL_22_AD = INRTAXA_TL_22_A;
INRCDIS_TLADEC_22 = INRCDIS_TLACQ_22;
INRCDIS_TL_22_AD = INRCDIS_TL_22_A;
INRCHR_TLADEC_22 = INRCHR_TLACQ_22+INRCHR_TL15_22;
INRCHR_TL_22_AD = INRCHR_TL_22_A;
INRPCAP_TLADEC_22 = INRPCAP_TLACQ_22;
INRPCAP_TL_22_AD = INRPCAP_TL_22_A;
INRRSE1_TLADEC_22 = INRRSE1_TLACQ_22;
INRRSE1_TL_22_AD = INRRSE1_TL_22_A;
INRRSE2_TLADEC_22 = INRRSE2_TLACQ_22;
INRRSE2_TL_22_AD = INRRSE2_TL_22_A;
INRRSE3_TLADEC_22 = INRRSE3_TLACQ_22;
INRRSE3_TL_22_AD = INRRSE3_TL_22_A;
INRRSE4_TLADEC_22 = INRRSE4_TLACQ_22;
INRRSE4_TL_22_AD = INRRSE4_TL_22_A;
INRRSE5_TLADEC_22 = INRRSE5_TLACQ_22;
INRRSE5_TL_22_AD = INRRSE5_TL_22_A;
INRRSE6_TLADEC_22 = INRRSE6_TLACQ_22;
INRRSE6_TL_22_AD = INRRSE6_TL_22_A;
INRRSE8_TLADEC_22 = INRRSE8_TLACQ_22;
INRRSE8_TL_22_AD = INRRSE8_TL_22_A;
INRCVN_TLADEC_22 = INRCVN_TLACQ_22;
INRCVN_TL_22_AD = INRCVN_TL_22_A;
INRGLO_TLADEC_22 = INRGLO_TLACQ_22;
INRGLO_TL_22_AD = INRGLO_TL_22_A;
INRC820_TLADEC_22 = INRC820_TLACQ_22;
INRC820_TL_22_AD = INRC820_TL_22_A;
INRIFI_TLADEC_22 = INRIFI_TLACQ_22;
INRIFI_TL_22_AD = INRIFI_TL_22_A;
regle corrective 13000:
application :  iliad ;
INRIR_TLDEC_22 = INRIR_TLA_22+INRIR_RETDEF*null(INRIR_RETDEF_A);
INRCSG_TLDEC_22 = INRCSG_TLA_22 + INRCSG_RETDEF * null(INRCSG_RETDEF_A);
INRPSOL_TLDEC_22 = INRPSOL_TLA_22 + INRPSOL_RETDEF * null(INRPSOL_RETDEF_A);
INRCRDS_TLDEC_22 = INRCRDS_TLA_22 + INRCRDS_RETDEF * null(INRCRDS_RETDEF_A);
INRTAXA_TLDEC_22 = INRTAXA_TLA_22 + INRTAXA_RETDEF * null(INRTAXA_RETDEF_A);
INRCDIS_TLDEC_22 = INRCDIS_TLA_22 + INRCDIS_RETDEF * null(INRCDIS_RETDEF_A);
INRCHR_TLDEC_22 = INRCHR_TLA_22 + INRCHR_RETDEF * null(INRCHR_RETDEF_A);
INRPCAP_TLDEC_22 = INRPCAP_TLA_22 + INRPCAP_RETDEF * null(INRPCAP_RETDEF_A);
INRRSE1_TLDEC_22 = INRRSE1_TLA_22 + INRRSE1_RETDEF * null(INRRSE1_RETDEF_A);
INRRSE2_TLDEC_22 = INRRSE2_TLA_22 + INRRSE2_RETDEF * null(INRRSE2_RETDEF_A);
INRRSE3_TLDEC_22 = INRRSE3_TLA_22 + INRRSE3_RETDEF * null(INRRSE3_RETDEF_A);
INRRSE4_TLDEC_22 = INRRSE4_TLA_22 + INRRSE4_RETDEF * null(INRRSE4_RETDEF_A);
INRRSE5_TLDEC_22 = INRRSE5_TLA_22 + INRRSE5_RETDEF * null(INRRSE5_RETDEF_A);
INRRSE6_TLDEC_22 = INRRSE6_TLA_22 + INRRSE6_RETDEF * null(INRRSE6_RETDEF_A);
INRRSE8_TLDEC_22 = INRRSE8_TLA_22 + INRRSE8_RETDEF * null(INRRSE8_RETDEF_A);
INRCVN_TLDEC_22 = INRCVN_TLA_22 + INRCVN_RETDEF * null(INRCVN_RETDEF_A);
INRGLO_TLDEC_22 = INRGLO_TLA_22 + INRGLO_RETDEF * null(INRGLO_RETDEF_A);
INRC820_TLDEC_22 = INRC820_TLA_22 + INRC820_RETDEF * null(INRC820_RETDEF_A);
INRIFI_TLDEC_22 = INRIFI_TLA_22 + INRIFI_RETDEF * null(INRIFI_RETDEF_A);
regle corrective 13100:
application :  iliad ;
INRIR_TLADEC_24 = INRIR_TLACQ_24;
INRIR_TL_24_AD=INRIR_TL_24_A;
INRCSG_TLADEC_24 = INRCSG_TLACQ_24;
INRCSG_TL_24_AD = INRCSG_TL_24_A;
INRPSOL_TLADEC_24 = INRPSOL_TLACQ_24;
INRPSOL_TL_24_AD = INRPSOL_TL_24_A;
INRCRDS_TLADEC_24 = INRCRDS_TLACQ_24;
INRCRDS_TL_24_AD = INRCRDS_TL_24_A;
INRTAXA_TLADEC_24 = INRTAXA_TLACQ_24;
INRTAXA_TL_24_AD = INRTAXA_TL_24_A;
INRCDIS_TLADEC_24 = INRCDIS_TLACQ_24;
INRCDIS_TL_24_AD = INRCDIS_TL_24_A;
INRCHR_TLADEC_24 = INRCHR_TLACQ_24+INRCHR_TL15_24;
INRCHR_TL_24_AD = INRCHR_TL_24_A;
INRPCAP_TLADEC_24 = INRPCAP_TLACQ_24;
INRPCAP_TL_24_AD = INRPCAP_TL_24_A;
INRRSE1_TLADEC_24 = INRRSE1_TLACQ_24;
INRRSE1_TL_24_AD = INRRSE1_TL_24_A;
INRRSE2_TLADEC_24 = INRRSE2_TLACQ_24;
INRRSE2_TL_24_AD = INRRSE2_TL_24_A;
INRRSE3_TLADEC_24 = INRRSE3_TLACQ_24;
INRRSE3_TL_24_AD = INRRSE3_TL_24_A;
INRRSE4_TLADEC_24 = INRRSE4_TLACQ_24;
INRRSE4_TL_24_AD = INRRSE4_TL_24_A;
INRRSE5_TLADEC_24 = INRRSE5_TLACQ_24;
INRRSE5_TL_24_AD = INRRSE5_TL_24_A;
INRRSE6_TLADEC_24 = INRRSE6_TLACQ_24;
INRRSE6_TL_24_AD = INRRSE6_TL_24_A;
INRRSE8_TLADEC_24 = INRRSE8_TLACQ_24;
INRRSE8_TL_24_AD = INRRSE8_TL_24_A;
INRCVN_TLADEC_24 = INRCVN_TLACQ_24;
INRCVN_TL_24_AD = INRCVN_TL_24_A;
INRGLO_TLADEC_24 = INRGLO_TLACQ_24;
INRGLO_TL_24_AD = INRGLO_TL_24_A;
INRC820_TLADEC_24 = INRC820_TLACQ_24;
INRC820_TL_24_AD = INRC820_TL_24_A;
INRIFI_TLADEC_24 = INRIFI_TLACQ_24;
INRIFI_TL_24_AD = INRIFI_TL_24_A;
regle corrective 13210:
application :  iliad ;
INRIR_TLDEC_24 = INRIR_TLA_24+INRIR_RETDEF*null(INRIR_RETDEF_A);
INRCSG_TLDEC_24 = INRCSG_TLA_24 + INRCSG_RETDEF * null(INRCSG_RETDEF_A);
INRPSOL_TLDEC_24 = INRPSOL_TLA_24 + INRPSOL_RETDEF * null(INRPSOL_RETDEF_A);
INRCRDS_TLDEC_24 = INRCRDS_TLA_24 + INRCRDS_RETDEF * null(INRCRDS_RETDEF_A);
INRTAXA_TLDEC_24 = INRTAXA_TLA_24 + INRTAXA_RETDEF * null(INRTAXA_RETDEF_A);
INRCDIS_TLDEC_24 = INRCDIS_TLA_24 + INRCDIS_RETDEF * null(INRCDIS_RETDEF_A);
INRCHR_TLDEC_24 = INRCHR_TLA_24 + INRCHR_RETDEF * null(INRCHR_RETDEF_A);
INRPCAP_TLDEC_24 = INRPCAP_TLA_24 + INRPCAP_RETDEF * null(INRPCAP_RETDEF_A);
INRRSE1_TLDEC_24 = INRRSE1_TLA_24 + INRRSE1_RETDEF * null(INRRSE1_RETDEF_A);
INRRSE2_TLDEC_24 = INRRSE2_TLA_24 + INRRSE2_RETDEF * null(INRRSE2_RETDEF_A);
INRRSE3_TLDEC_24 = INRRSE3_TLA_24 + INRRSE3_RETDEF * null(INRRSE3_RETDEF_A);
INRRSE4_TLDEC_24 = INRRSE4_TLA_24 + INRRSE4_RETDEF * null(INRRSE4_RETDEF_A);
INRRSE5_TLDEC_24 = INRRSE5_TLA_24 + INRRSE5_RETDEF * null(INRRSE5_RETDEF_A);
INRRSE6_TLDEC_24 = INRRSE6_TLA_24 + INRRSE6_RETDEF * null(INRRSE6_RETDEF_A);
INRRSE8_TLDEC_24 = INRRSE8_TLA_24 + INRRSE8_RETDEF * null(INRRSE8_RETDEF_A);
INRCVN_TLDEC_24 = INRCVN_TLA_24 + INRCVN_RETDEF * null(INRCVN_RETDEF_A);
INRGLO_TLDEC_24 = INRGLO_TLA_24 + INRGLO_RETDEF * null(INRGLO_RETDEF_A);
INRC820_TLDEC_24 = INRC820_TLA_24 + INRC820_RETDEF * null(INRC820_RETDEF_A);
INRIFI_TLDEC_24 = INRIFI_TLA_24 + INRIFI_RETDEF * null(INRIFI_RETDEF_A);
regle corrective 13250:
application :  iliad ;
INRIR_NTLDECD = INRIR_NTLDEC;
INRCSG_NTLDECD = INRCSG_NTLDEC;
INRCRDS_NTLDECD = INRCRDS_NTLDEC;
INRPSOL_NTLDECD = INRPSOL_NTLDEC;
INRCDIS_NTLDECD = INRCDIS_NTLDEC;
INRTAXA_NTLDECD = INRTAXA_NTLDEC;
INRCHR_NTLDECD = INRCHR_NTLDEC ;
INRPCAP_NTLDECD = INRPCAP_NTLDEC;
INRRSE1_NTLDECD = INRRSE1_NTLDEC;
INRRSE2_NTLDECD = INRRSE2_NTLDEC;
INRRSE3_NTLDECD = INRRSE3_NTLDEC;
INRRSE4_NTLDECD = INRRSE4_NTLDEC;
INRRSE5_NTLDECD = INRRSE5_NTLDEC;
INRRSE6_NTLDECD = INRRSE6_NTLDEC;
INRRSE8_NTLDECD = INRRSE8_NTLDEC;
INRCVN_NTLDECD = INRCVN_NTLDEC;
INRGLO_NTLDECD = INRGLO_NTLDEC;
INRC820_NTLDECD = INRC820_NTLDEC;
INRIFI_NTLDECD = INRIFI_NTLDEC ;

regle corrective 13300:
application :  iliad ;
INRIR_NTLDECD_22 = INRIR_NTLDEC_22;
INRCSG_NTLDECD_22 = INRCSG_NTLDEC_22;
INRCRDS_NTLDECD_22 = INRCRDS_NTLDEC_22;
INRPSOL_NTLDECD_22 = INRPSOL_NTLDEC_22;
INRCDIS_NTLDECD_22 = INRCDIS_NTLDEC_22;
INRTAXA_NTLDECD_22 = INRTAXA_NTLDEC_22;
INRCHR_NTLDECD_22 = INRCHR_NTLDEC_22 ;
INRPCAP_NTLDECD_22 = INRPCAP_NTLDEC_22;
INRRSE1_NTLDECD_22 = INRRSE1_NTLDEC_22;
INRRSE2_NTLDECD_22 = INRRSE2_NTLDEC_22;
INRRSE3_NTLDECD_22 = INRRSE3_NTLDEC_22;
INRRSE4_NTLDECD_22 = INRRSE4_NTLDEC_22;
INRRSE5_NTLDECD_22 = INRRSE5_NTLDEC_22;
INRRSE6_NTLDECD_22 = INRRSE6_NTLDEC_22;
INRRSE8_NTLDECD_22 = INRRSE8_NTLDEC_22;
INRCVN_NTLDECD_22 = INRCVN_NTLDEC_22;
INRGLO_NTLDECD_22 = INRGLO_NTLDEC_22;
INRC820_NTLDECD_22 = INRC820_NTLDEC_22;
INRIFI_NTLDECD_22 = INRIFI_NTLDEC_22 ;
regle corrective 13350:
application :  iliad ;
INRIR_NTLDECD_24 = INRIR_NTLDEC_24;
INRCSG_NTLDECD_24 = INRCSG_NTLDEC_24;
INRCRDS_NTLDECD_24 = INRCRDS_NTLDEC_24;
INRPSOL_NTLDECD_24 = INRPSOL_NTLDEC_24;
INRCDIS_NTLDECD_24 = INRCDIS_NTLDEC_24;
INRTAXA_NTLDECD_24 = INRTAXA_NTLDEC_24;
INRCHR_NTLDECD_24 = INRCHR_NTLDEC_24 ;
INRPCAP_NTLDECD_24 = INRPCAP_NTLDEC_24;
INRRSE1_NTLDECD_24 = INRRSE1_NTLDEC_24;
INRRSE2_NTLDECD_24 = INRRSE2_NTLDEC_24;
INRRSE3_NTLDECD_24 = INRRSE3_NTLDEC_24;
INRRSE4_NTLDECD_24 = INRRSE4_NTLDEC_24;
INRRSE5_NTLDECD_24 = INRRSE5_NTLDEC_24;
INRRSE6_NTLDECD_24 = INRRSE6_NTLDEC_24;
INRRSE8_NTLDECD_24 = INRRSE8_NTLDEC_24;
INRCVN_NTLDECD_24 = INRCVN_NTLDEC_24;
INRGLO_NTLDECD_24 = INRGLO_NTLDEC_24;
INRC820_NTLDECD_24 = INRC820_NTLDEC_24;
INRIFI_NTLDECD_24 = INRIFI_NTLDEC_24 ;
regle corrective 13370:
application :  iliad ;
INRIR_TLDECD = INRIR_TLDEC;
INRCSG_TLDECD = INRCSG_TLDEC;
INRCRDS_TLDECD = INRCRDS_TLDEC;
INRPSOL_TLDECD = INRPSOL_TLDEC;
INRCDIS_TLDECD = INRCDIS_TLDEC;
INRTAXA_TLDECD = INRTAXA_TLDEC;
INRCHR_TLDECD = INRCHR_TLDEC ;
INRPCAP_TLDECD = INRPCAP_TLDEC;
INRRSE1_TLDECD = INRRSE1_TLDEC;
INRRSE2_TLDECD = INRRSE2_TLDEC;
INRRSE3_TLDECD = INRRSE3_TLDEC;
INRRSE4_TLDECD = INRRSE4_TLDEC;
INRRSE5_TLDECD = INRRSE5_TLDEC;
INRRSE6_TLDECD = INRRSE6_TLDEC;
INRRSE8_TLDECD = INRRSE8_TLDEC;
INRCVN_TLDECD = INRCVN_TLDEC ;
INRGLO_TLDECD = INRGLO_TLDEC ;
INRC820_TLDECD = INRC820_TLDEC;
INRIFI_TLDECD = INRIFI_TLDEC ;
regle corrective 13400:
application :  iliad ;
INRIR_R99RA = INRIR_R99R_A;
INRIR_R99R = arr(IRNIN_R99R * (TXINR_PA/100)-INCIR_NET_A) * positif(IRNIN_R99R- IRNIN_R99R_A)
             * positif(IND_PASSAGE-1) * positif(IRNIN_TLDEC-IRNIN_PA) * FLAG_RETARD0718;
INRIR_R9901A = INRIR_R9901_A;
INRIR_R9901 = arr(IRNIN_R9901 * (TXINR_PA/100)-INCIR_NET_A) * positif(IRNIN_R9901- IRNIN_R9901_A)
              * positif(IND_PASSAGE-1) * positif(IRNIN_TLDEC-IRNIN_R9901) * positif(IRNIN_R9901_A)
             + (arr(IRNIN_R9901 * (TXINR_PA/100))-INCIR_NET_A) * positif(IRNIN_R9901- IRNIN_INR)
              * positif(IND_PASSAGE-1) * positif(IRNIN_TLDEC-IRNIN_R9901) * (1-positif(IRNIN_R9901_A))
             + (INCIR_NET_A - arr(IRNIN_R9901 * (TXINR_PA/100))) * positif(IRNIN_INR- IRNIN_R9901)
              * positif(IND_PASSAGE-1) * positif(IRNIN_TLDEC-IRNIN_R9901) * (1-positif(IRNIN_R9901_A)) * positif(IRNIN_R9901)
	     ;

DO_INR_IRC=DO_INR_IR_A;
INR_NTL_GLOB_IR2 = INRIR_NTLDECD+ INRIR_NTL_A+ INRIR_NTLDECD_22 +INRIR_NTLDECD_24+ INRIR_NTL_22_A+INRIR_NTL_24_A ;
INR_TL_GLOB_IR2 = INRIR_TLDECD + INRIR_TL_A + INRIR_TLDEC_22 + INRIR_TL_22_A+INRIR_TLDEC_24 + INRIR_TL_24_A;
INR_TOT_GLOB_IR2 = (INR_NTL_GLOB_IR2 + INR_TL_GLOB_IR2*TL_IR+INRIR_R99R+INRIR_R99R_A) * (1-IND_RJLJ) ;
INR_TOT_GLOB_IRC = (INRIR_NTLDECD+ INRIR_NTL_A+ (INRIR_TLDECD + INRIR_TL_A)*TL_IR +INRIR_R99R+INRIR_R99R_A) * (1-IND_RJLJ) ;

DO_INR_IR2 = (1-null(IRNIN_REF_A-min(IRNIN_REF,max(IRNIN_TLDEC_22,IRNIN_TLDEC_24)))) * max(0,
          arr(((INRIR_TL_A+INRIR_TL_22_A+INRIR_TL_24_A)*TL_IR_A *TL_IR+ INRIR_NTL_A+INRIR_NTL_22_A+INRIR_NTL_24_A) 
            * min(1,((IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24)/(IRNIN_REF-max(0,IRNIN_R9901))))) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
            *positif(IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24) * (1 - positif(FLAG_C02+FLAG_C22))
            *(1-positif_ou_nul(IRNIN_TLDEC_22 +IRNIN_TLDEC_24- IRNIN_INR_A))
        + arr(((INRIR_TL_A+INRIR_TL_22_A+INRIR_TL_24_A)*TL_IR_A *TL_IR) 
            * min(1,((IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24)/(IRNIN_REF-max(0,IRNIN_R9901))))) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
            *positif(IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24) * positif(FLAG_C02+FLAG_C22)
            *(1-positif_ou_nul(IRNIN_TLDEC_22+IRNIN_TLDEC_24 - IRNIN_INR_A))
            * (1-positif(INRIR_NTL_A + INRIR_NTL_22_A+INRIR_NTL_24_A))
         + (INRIR_NTL_A*FLAG_C02+(INRIR_NTL_22_A+INRIR_NTL_24_A)*FLAG_C22) 
            *positif(IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24) * positif(FLAG_C02+FLAG_C22)
            *positif(INRIR_NTL_A)*positif(INRIR_NTL_22_A+INRIR_NTL_24_A) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
         + arr((INRIR_NTL_A*FLAG_C02+(INRIR_NTL_22_A+INRIR_NTL_24_A)*FLAG_C22) 
            *positif(IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24) * positif(FLAG_C02+FLAG_C22)
            * min(1,((IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24)/(IRNIN_REF-max(0,IRNIN_R9901)))))
            * (1-positif(positif(INRIR_NTL_A)*positif(INRIR_NTL_22_A+INRIR_NTL_24_A))) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
         + ((INRIR_TL_A+INRIR_TL_22_A+INRIR_TL_24_A)*null(TL_IR) * TL_IR_A
            * (1- FLAG_DEFAUT)
             *positif(IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24)* positif(IRNIN_REF - (max(0,IRNIN_R9901))))
         +  positif(FLAG_RETARD + FLAG_DEFAUT) * (
	      ( (1-positif(IR9YI)) * ( 
	    (arr((INRIR_TL_A*TL_IR_A *TL_IR+(INRIR_NTL_A +INRIR_R99R+INRIR_R9901-INRIR_RETDEF) 
            * ((IRNIN_REF - IRNIN_TLDEC)/(IRNIN_REF-max(0,IRNIN_REF_A)))))
            * positif(IRNIN_REF - IRNIN_TLDEC)  * positif(IRNIN_TLDEC - IRNIN_R99R) 
            * positif(INRIR_R99R_A+INRIR_R9901_A+0)
            + arr(max(0,IRNIN_REF - IRNIN_TLDEC -NIRNINBIS) * TXINR_A/100)
             * positif(IRNIN_REF - IRNIN_TLDEC+NIRNINBIS-NIRNINBIS_A)  ) * (1- positif(SOM9YI))
	     + arr(max(0,IRNIN_REF_A - IRNIN_REF) * TXINR_PA/100)
               * positif(IRNIN_REF_A - IRNIN_REF)  * positif(SOM9YI) 
		                            )
            + positif(IR9YI)*(
          (INRIR_TL_A + INRIR_R99R_A+INRIR_NTL_A - max(0,arr(IRNIN_TLDEC * TXINR_PA/100))) * positif(IRNIN_R99R - IRNIN_TLDEC)
         + (INRIR_R99R_A+INRIR_NTL_A - max(0,arr((IRNIN_R99R) * TXINR_PA/100))) * positif(IRNIN_TLDEC-IRNIN_R99R)
         + (INRIR_TL_A + INRIR_R99R_A+INRIR_NTL_A - max(0,arr(IRNIN_R99R * TXINR_PA/100))) * null(IRNIN_TLDEC-(IRNIN_R99R))
            )) * (1-positif(FLAG_RETARD22))
            + ((1-positif(IR9YI)) * (
              (INRIR_NTL_22_A+ INRIR_TL_22_A- DO_INR_IR_A - arr((max(0,IRNIN_TLDEC_22 -NIRNINBIS)) * TXINR_PA22/100))
                  * positif(IRNIN_REF - max(IRNIN_TLDEC_24,IRNIN_TLDEC_22)+NIRNINBIS-NIRNINBIS_A)  
             + (arr((INRIR_TL_22_A*TL_IR_A *TL_IR+(INRIR_NTL_22_A +INRIR_R99R+INRIR_R9901-INRIR_RETDEF)
                  * ((IRNIN_REF - IRNIN_TLDEC_22)/(IRNIN_REF-max(0,IRNIN_REF_A)))))
              * positif(IRNIN_REF - max(IRNIN_TLDEC_24,IRNIN_TLDEC_22))  * positif(max(IRNIN_TLDEC_24,IRNIN_TLDEC_22) - IRNIN_R9901)
                  * positif(INRIR_R99R_A+INRIR_R9901_A+0))
		      )
                + positif(IR9YI) * (
           (INRIR_TL_22_A + INRIR_R99R_A+INRIR_NTL_22_A - max(0,arr(IRNIN_TLDEC_22* TXINR_PA22/100))) 
	   * positif(IRNIN_R9901 +CODCOA-CODCOA_A- max(IRNIN_TLDEC_24,IRNIN_TLDEC_22))
            + (INRIR_R99R_A+INRIR_NTL_22_A - max(0,arr(IRNIN_R99R * TXINR_PA22/100))) * positif(max(IRNIN_TLDEC_24,IRNIN_TLDEC_22)-(IRNIN_R9901))
               ))*positif(FLAG_RETARD22) ));
RECUP_INR_IR = max(0,(min(max(0,DO_INR_IR_A-RECUP_INR_IR_A),arr(max(0,DO_INR_IR_A-RECUP_INR_IR_A) * (IRNIN_TLDEC - IRNIN_INR)/DO_IR_A))
                      *positif(IRNIN_TLDEC-IRNIN_INR)*positif(IRNIN_REF-IRNIN_INR)
                    * positif(IRNIN_PA - IRNIN_TLDEC))
                      *positif(FLAG_RETARD + FLAG_DEFAUT)
		    + min(max(0,DO_INR_IR_A-RECUP_INR_IR_A),arr((IRNIN_PA - IRNIN_INR) * TXINR_PA/100))
                    * (1-positif(IRNIN_PA - IRNIN_TLDEC))
                      * positif(IRNIN_TLDEC-IRNIN_A)
                      * positif(max(0,DO_INR_IR_A-RECUP_INR_IR_A))
                      *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_IR2 = (IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24) * positif(IRNIN_REF - IRNIN_TLDEC_22-IRNIN_TLDEC_24)* positif(IRNIN_INR);
DO_INR_IR982 = max(0,
          (arr((IRNIN_REF - IRNIN_NTLDEC_198) * TXINRRED22_A/100) + arr((IRNIN_REF - IRNIN_NTLDEC_198) * TXINRRED24_A/100))
            *positif(IRNIN_REF - IRNIN_NTLDEC_198))*(1-positif(DO_INR_IR2)) * present(IRNIN_NTLDEC_198);
DO_INR_IR992 = max(0,
          (arr((IRNIN_REF - IRNIN_TLDEC_199) * TXINRRED22_A/100) + arr((IRNIN_REF - IRNIN_TLDEC_199) * TXINRRED24_A/100))
            *positif(IRNIN_REF - IRNIN_TLDEC_199))*(1-positif(DO_INR_IR2)) * present(IRNIN_TLDEC_199);
SUP_IR_MAX2 = (IRNIN_REF - max(0,IRNIN_R9901)) * positif(IRNIN_REF - max(0,IRNIN_R9901))* positif(IRNIN_INR);
INRIR_RECT= arr(IRNIN_RECT * (TXINR_PA/100)) * positif(IRNIN_RECT) * FLAG_RECTIF;
INR_IR_TOT = max(INRIR_NTLDECD_22+INRIR_NTLDECD_24+INRIR_NTLDECD + (INRIR_TLDECD+INRIR_TLDEC_22+INRIR_TLDEC_24)*TL_IR-INR_IR_TARDIF*null(1-IND_PASSAGE)+INRIR_R99R+RECUP_INR_IR,0)* (1-IND_RJLJ);
INCIR_TL2 = INRIR_TLDECD;
INCIR_TL_222 = INRIR_TLDEC_22;
INRIR_NET2 = max(INRIR_NTLDECD +INRIR_TLDECD*TL_IR+INRIR_R99R+RECUP_INR_IR,0)* (1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_IR2 * (-1);
INRIR_NET_222 = max(INRIR_NTLDECD_22 +INRIR_NTLDECD_24+(INRIR_TLDEC_22+INRIR_TLDEC_24)*TL_IR,0)*(1-FLAG_NINR)* (1-IND_RJLJ) + (DO_INR_IR982 + DO_INR_IR992)*(-1);
INIR_TL2 = INRIR_TLA * TL_IR;
INIR_TL_222 = (INRIR_TLA_22+INRIR_TLA_24) * TL_IR;
INCIR_NET2 = max(0,(INRIR_NET2 +INRIR_NET_222
                  + (INCIR_NET_A-(INR_IR_TARDIF_A+INRIR_RETDEF_A)*positif(INRIR_NET2+INRIR_NET_222-INR_IR_TARDIF_A-INRIR_RETDEF_A)
                                                                                 *positif(IR9YI)*(1-positif(INDACOINR_A)))
                  + ((INRIR_TL_A+INRIR_TL_22_A+INRIR_TL_24_A)*(1-null(TL_IR_A-TL_IR))*TL_IR))) *positif(IRNIN_INR+NIRNIN)* (1-IND_RJLJ) ;
PIRBR = PIR_A;
PCSGBR = PCSG_A;
PRDSBR = PRDS_A;
PPSOLBR = PPSOL_A;
CSGBR = CSGC_A;
CRDSBR = CRDS_A;
PSOLBR = MPSOL_A;
INRIR_NET1A = INRIR_NET_1_A;
INRCSG_NET1A = INRCSG_NET_1_A;
INRRDS_NET1A = INRRDS_NET_1_A;
INRPSOL_NET1A = INRPSOL_NET_1_A;
INRCDIS_NET1A = INRCDIS_NET_1_A;
INRC820_NET1A = INRC820_NET_1_A;
INRGLO_NET1A = INRGLO_NET_1_A;
INRTAXA_NET1A = INRTAXA_NET_1_A;
INRCHR_NET1A = INRCHR_NET_1_A;
INRRSE1_NET1A = INRRSE1_NET_1_A;
INRRSE2_NET1A = INRRSE2_NET_1_A;
INRRSE3_NET1A = INRRSE3_NET_1_A;
INRRSE4_NET1A = INRRSE4_NET_1_A;
INRRSE5_NET1A = INRRSE5_NET_1_A;
INRRSE6_NET1A = INRRSE6_NET_1_A;
INRRSE8_NET1A = INRRSE8_NET_1_A;
INRIFI_NET1A = INRIFI_NET_1_A;
regle corrective 13430:
application :  iliad ;
INRIR_NETA = INRIR_NET_A;
INRCSG_NETA = INRCSG_NET_A;
INRRDS_NETA = INRRDS_NET_A;
INRPSOL_NETA = INRPSOL_NET_A;
INRCDIS_NETA = INRCDIS_NET_A;
INRC820_NETA = INRC820_NET_A;
INRGLO_NETA = INRGLO_NET_A;
INRTAXA_NETA = INRTAXA_NET_A;
INRCHR_NETA = INRCHR_NET_A;
INRRSE1_NETA = INRRSE1_NET_A;
INRRSE2_NETA = INRRSE2_NET_A;
INRRSE3_NETA = INRRSE3_NET_A;
INRRSE4_NETA = INRRSE4_NET_A;
INRRSE5_NETA = INRRSE5_NET_A;
INRRSE6_NETA = INRRSE6_NET_A;
INRRSE8_NETA = INRRSE8_NET_A;
INRIFI_NETA = INRIFI_NET_A;
regle corrective 13450:
application :  iliad ;
IR_PRI2=IRNIN_R9901;
IR_ANT2=IRNIN_INR;
IR_NTL2=IRNIN_NTLDEC;
IR_TL2=IRNIN_TLDEC;
IR_NTL_222=IRNIN_NTLDEC_22;
IR_TL_222=IRNIN_TLDEC_22;
IR_REF_INR=IRNIN_REF;
INRCSG_R99RA = INRCSG_R99R_A;
INRCSG_R99R = arr((CSG_R99R-CSGIM) * (TXINR_PA/100)-INCCS_NET_A) * positif(CSG_R99R-CSG_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRCSG_R9901A = INRCSG_R9901_A;
INRCSG_R9901 = arr(CSG_R9901 * (TXINR_PA/100)-INCCS_NET_A) * positif(CSG_R9901- CSG_R9901_A)
              * positif(IND_PASSAGE-1) * positif(CSG_TLDEC-CSG_R9901) * positif(CSG_R9901_A)
             + (arr(CSG_R9901 * (TXINR_PA/100))-INCCS_NET_A) * positif(CSG_R9901- CSG_A)
              * positif(IND_PASSAGE-1) * positif(CSG_TLDEC-CSG_R9901) * (1-positif(CSG_R9901_A))
             + (INCCS_NET_A - arr(CSG_R9901 * (TXINR_PA/100))) * positif(CSG_A- CSG_R9901) * positif(CSG_R9901)
              * positif(IND_PASSAGE-1) * positif(CSG_TLDEC-CSG_R9901) * (1-positif(CSG_R9901_A))
	     ;
DO_INR_CSGC=DO_INR_CSG_A;
INR_NTL_GLOB_CSG2 = INRCSG_NTLDECD + INRCSG_NTL_A+ INRCSG_NTLDECD_22 +INRCSG_NTLDECD_24+ INRCSG_NTL_22_A+INRCSG_NTL_24_A;
INR_TL_GLOB_CSG2 = INRCSG_TLDECD + INRCSG_TL_A+INRCSG_TLDEC_22 + INRCSG_TL_22_A+INRCSG_TLDEC_24 + INRCSG_TL_24_A;
INR_TOT_GLOB_CSG2 = (INR_NTL_GLOB_CSG2 + INR_TL_GLOB_CSG2*TL_CS+INRCSG_R99R+INRCSG_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_CSGC = (INRCSG_NTLDECD+ INRCSG_NTL_A+ (INRCSG_TLDECD + INRCSG_TL_A)*TL_CS +INRCSG_R99R+INRCSG_R99R_A) * (1-IND_RJLJ) ;
DO_INR_CSG2 = (1-null(CSG_REF_A-min(CSG_REF,max(CSG_TLDEC_22,CSG_TLDEC_24)))) * max(0,
           (arr(((INRCSG_TL_A+INRCSG_TL_22_A+INRCSG_TL_24_A)*TL_CS_A*TL_CS + INRCSG_NTL_A+INRCSG_NTL_22_A+INRCSG_NTL_24_A)
              * min(1,((CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24)/(CSG_REF-max(0,CSG_R9901))))) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
              * positif(CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24)* positif(CSG_REF - (max(0,CSG_R9901)))) * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(CSG_TLDEC_22 +CSG_TLDEC_24- CSG_A))
          + arr(((INRCSG_TL_A+INRCSG_TL_22_A+INRCSG_TL_24_A)*TL_CS_A*TL_CS)
	     * min(1,((CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24)/(CSG_REF-max(0,CSG_R9901))))) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
	     * positif(CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24)* positif(CSG_REF - (max(0,CSG_R9901))) * positif(FLAG_C02+FLAG_C22)
	     *(1-positif_ou_nul(CSG_TLDEC_22 +CSG_TLDEC_24- CSG_A))
	     * (1-positif(INRCSG_NTL_A+INRCSG_NTL_22_A+INRCSG_NTL_24_A))
          + (INRCSG_NTL_A*FLAG_C02+(INRCSG_NTL_22_A+INRCSG_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24)* positif(CSG_REF- (max(0,CSG_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRCSG_NTL_A)*positif(INRCSG_NTL_22_A+INRCSG_NTL_24_A) 
          + arr((INRCSG_NTL_A*FLAG_C02+(INRCSG_NTL_22_A+INRCSG_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24)* positif(CSG_REF - (max(0,CSG_R9901))) * positif(FLAG_C02+FLAG_C22)
	     * min(1,((CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24)/(CSG_REF-max(0,CSG_R9901)))))
             * (1-positif(INRCSG_NTL_A)*positif(INRCSG_NTL_22_A+INRCSG_NTL_24_A))
          + ((INRCSG_TL_A+INRCSG_TL_22_A+INRCSG_TL_24_A)*null(TL_CS) * TL_CS_A
          * (1- FLAG_DEFAUT)
             *positif(CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24)* positif(CSG_REF - (max(0,CSG_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT) * (
	    (positif(SOM9YI) * (
	  (INRCSG_TL_A + INRCSG_R99R_A+INRCSG_NTL_A - max(0,arr(CSG_TLDEC * TXINR_PA/100))) * positif(CSG_R99R - CSG_TLDEC)
         + (INRCSG_R99R_A+INRCSG_NTL_A - max(0,arr(CSG_R99R* TXINR_PA/100))) * positif(CSG_TLDEC-CSG_R99R)
         + (INRCSG_TL_A + INRCSG_R99R_A+INRCSG_NTL_A - max(0,arr(CSG_R99R * TXINR_PA/100))) * null(CSG_TLDEC-CSG_R99R))
         + (1-positif(SOM9YI+0)) * (
          arr((INRCSG_TL_A*TL_CS_A *TL_CS+(INRCSG_NTL_A +INRCSG_R99R+INRCSG_R9901-INRCSG_RETDEF) 
                       * ((CSG_REF - CSG_TLDEC)/(CSG_REF-max(0,CSG_REF_A)))))
                       * positif(CSG_REF - CSG_TLDEC)  * positif(CSG_TLDEC - CSG_R99R) 
                       * positif(INRCSG_R99R_A+INRCSG_R9901_A+0)
         + (INR_TOT_GLOB_CSGC - DO_INR_CSG_A - arr((max(0,CSG_TLDEC -NCSBASEBIS)) * TXINR_PA/100))
                       * positif(CSG_REF - CSG_TLDEC+NCSBASEBIS-NCSBASEBIS_A)
                        )) * (1-positif(FLAG_RETARD22))
           + (positif(SOM9YI) * (
             (INRCSG_TL_22_A + INRCSG_R99R_A+INRCSG_NTL_22_A - max(0,arr(CSG_TLDEC_22* TXINR_PA22/100))) 
	                                      * positif(CSG_R9901 - max(CSG_TLDEC_24,CSG_TLDEC_22))
             + (INRCSG_R99R_A+INRCSG_NTL_22_A - max(0,arr(CSG_R99R  * TXINR_PA22/100)))
                                                * positif(max(CSG_TLDEC_24,CSG_TLDEC_22)-(CSG_R9901))
               + (INRCSG_TL_22_A + INRCSG_R99R_A+INRCSG_NTL_22_A - max(0,arr(CSG_R99R * TXINR_PA22/100))) 
	                                     * null(max(CSG_TLDEC_24,CSG_TLDEC_22)-(CSG_R9901)))
              + (1-positif(SOM9YI+0)) * (
          arr((INRCSG_TL_22_A*TL_CS_A *TL_CS+(INRCSG_NTL_22_A +INRCSG_R99R+INRCSG_R9901-INRCSG_RETDEF)
                        * ((CSG_REF - CSG_TLDEC_22)/(CSG_REF-max(0,CSG_REF_A)))))
                       * positif(CSG_REF - max(CSG_TLDEC_24,CSG_TLDEC_22))  * positif(max(CSG_TLDEC_24,CSG_TLDEC_22) - CSG_R9901)
                       * positif(INRCSG_R99R_A+INRCSG_R9901_A+0)
              + (INRCSG_TL_22_A+INRCSG_NTL_22_A - DO_INR_CSG_A - arr((max(0,CSG_TLDEC_22 -NCSBASEBIS))* TXINR_PA22/100))
                            * positif(CSG_REF- max(CSG_TLDEC_24,CSG_TLDEC_22)+NCSBASEBIS-NCSBASEBIS_A)
                         )) * positif(FLAG_RETARD22)
               ));

RECUP_INR_CSG = max(0,(min(max(0,DO_INR_CSG_A-RECUP_INR_CSG_A),arr(max(0,DO_INR_CSG_A-RECUP_INR_CSG_A) * (CSG_TLDEC - CSG_A)/DO_CSG_A))
                    *positif(CSG_TLDEC-CSG_A)*positif(CSG_REF-CSG_A)
                    * positif(CSG_PA - CSG_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_CSG_A-RECUP_INR_CSG_A),arr((CSG_R99R - CSG_A) * TXINR_PA/100))
                    * (1-positif(CSG_PA - CSG_TLDEC))
                    * positif(CSG_TLDEC - CSG_A)
                    * positif(max(0,DO_INR_CSG_A-RECUP_INR_CSG_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_CSG2 = (CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24) * positif(CSG_REF - CSG_TLDEC_22-CSG_TLDEC_24)* positif(CSG_A);
SUP_CSG_MAX2 = (CSG_REF - max(0,CSG_R9901)) * positif(CSG_REF - max(0,CSG_R9901))* positif(CSG_A);
DO_INR_CSG982 = max(0,
          arr((CSG_REF - CSG_NTLDEC_198) * TXINRRED_A/100) 
            *positif(CSG_REF - CSG_NTLDEC_198))*(1-positif(DO_INR_CSG2)) * present(CSG_NTLDEC_198);
DO_INR_CSG992 = max(0,
          arr((CSG_REF - CSG_TLDEC_199) * TXINRRED_A/100)
            *positif(CSG_REF - CSG_TLDEC_199))*(1-positif(DO_INR_CSG2)) * present(CSG_TLDEC_199);
INRCSG_RECT= arr((CSG_RECT-CSGIM) * (TXINR_PA/100)) * positif(CSG_RECT) * FLAG_RECTIF;
INR_CSG_TOT = max(INRCSG_NTLDECD+INRCSG_NTLDECD_22+INRCSG_NTLDECD_24+(INRCSG_TLDECD+INRCSG_TLDEC_22+INRCSG_TLDEC_24)*TL_CS-INR_CSG_TARDIF*null(1-IND_PASSAGE)+INRCSG_R99R+RECUP_INR_CSG,0)*(1-IND_RJLJ);
INRCSG_NET2 = max(INRCSG_NTLDECD+INRCSG_TLDECD*TL_CS+INRCSG_R99R+RECUP_INR_CSG,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_CSG2 * (-1);
INRCSG_NET_222 = max(INRCSG_NTLDECD_22+INRCSG_NTLDECD_24+(INRCSG_TLDEC_22+INRCSG_TLDEC_24)*TL_CS,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_CSG982 + DO_INR_CSG992)*(-1);
INCCS_NET2 = max(0,(INRCSG_NET2 +INRCSG_NET_222+ INCCS_NET_A+(INRCSG_TL_A+INRCSG_TL_22_A+INRCSG_TL_24_A)*(1-null(TL_CS_A-TL_CS))*positif(TL_CS))) * positif(CSBASE_INR+NCSBASE)* (1-IND_RJLJ);
INCS_TL2 = INRCSG_TLA * TL_CS;
INCS_TL_222 = INRCSG_TLA_22 * TL_CS;
INCCS_TL2 = INRCSG_TLDECD;
INCCS_TL_222 = INRCSG_TLDEC_22;
CSG_PRI2=CSG_R9901;
CSG_ANT2=CSG_A;
CSG_NTL2=CSG_NTLDEC;
CSG_NTL_222=CSG_NTLDEC_22;
CSG_TL2=CSG_TLDEC;
CSG_TL_222=CSG_TLDEC_22;
CSG_REF_INR=CSG_REF;
INRCRDS_R99RA = INRCRDS_R99R_A;
INRCRDS_R99R = arr((RDS_R99R - CRDSIM) * (TXINR_PA/100)-INCRD_NET_A)
                  * positif(RDS_R99R-RDS_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRCRDS_R9901A = INRCRDS_R9901_A;
INRCRDS_R9901 = arr(RDS_R9901 * (TXINR_PA/100)-INCRD_NET_A) * positif(RDS_R9901- RDS_R9901_A)
              * positif(IND_PASSAGE-1) * positif(RDS_TLDEC-RDS_R9901) * positif(RDS_R9901_A)
             + (arr(RDS_R9901 * (TXINR_PA/100))-INCRD_NET_A) * positif(RDS_R9901- RDS_A)
              * positif(IND_PASSAGE-1) * positif(RDS_TLDEC-RDS_R9901) * (1-positif(RDS_R9901_A))
             + (INCRD_NET_A - arr(RDS_R9901 * (TXINR_PA/100))) * positif(RDS_A- RDS_R9901)
              * positif(IND_PASSAGE-1) * positif(RDS_TLDEC-RDS_R9901) * (1-positif(RDS_R9901_A)) * positif(RDS_R9901)
	     ;
DO_INR_CRDSC=DO_INR_CRDS_A;
INR_NTL_GLOB_CRDS2 = INRCRDS_NTLDECD + INRCRDS_NTL_A+INRCRDS_NTLDECD_22+INRCRDS_NTL_22_A+INRCRDS_NTLDECD_24+INRCRDS_NTL_24_A;
INR_TL_GLOB_CRDS2 = INRCRDS_TLDECD + INRCRDS_TL_A+INRCRDS_TLDEC_22+INRCRDS_TL_22_A+INRCRDS_TLDEC_24+INRCRDS_TL_24_A;
INR_TOT_GLOB_CRDS2 = (INR_NTL_GLOB_CRDS2 + INR_TL_GLOB_CRDS2*TL_RD+INRCRDS_R99R+INRCRDS_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_CRDSC= (INRCRDS_NTLDECD+INRCRDS_NTL_A+(INRCRDS_TLDECD+INRCRDS_TL_A)*TL_RD +INRCRDS_R99R+INRCRDS_R99R_A) * (1-IND_RJLJ) ;

DO_INR_CRDS2 = (1-null(RDS_REF_A-min(RDS_REF,max(RDS_TLDEC_22,RDS_TLDEC_24)))) * max(0,
           (arr(((INRCRDS_TL_A+INRCRDS_TL_22_A+INRCRDS_TL_24_A)*TL_RD_A*TL_RD + INRCRDS_NTL_A+INRCRDS_NTL_22_A+INRCRDS_NTL_24_A)
              * min(1,((RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24)/(RDS_REF-max(0,RDS_R9901))))) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
              * positif(RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24)* positif(RDS_REF - (max(0,RDS_R9901)))) * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(RDS_TLDEC_22 +RDS_TLDEC_24- RDS_A))
          + arr(((INRCRDS_TL_A+INRCRDS_TL_22_A+INRCRDS_TL_24_A)*TL_RD_A*TL_RD)
	     * min(1,((RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24)/(RDS_REF-max(0,RDS_R9901))))) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
	     * positif(RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24)* positif(RDS_REF - (max(0,RDS_R9901))) * positif(FLAG_C02+FLAG_C22)
	     *(1-positif_ou_nul(RDS_TLDEC_22 +RDS_TLDEC_24- RDS_A))
	     * (1-positif(INRCRDS_NTL_A+INRCRDS_NTL_22_A+INRCRDS_NTL_24_A))
          + (INRCRDS_NTL_A*FLAG_C02+(INRCRDS_NTL_22_A+INRCRDS_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24)* positif(RDS_REF- (max(0,RDS_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRCRDS_NTL_A)*positif(INRCRDS_NTL_22_A+INRCRDS_NTL_24_A) 
          + arr((INRCRDS_NTL_A*FLAG_C02+(INRCRDS_NTL_22_A+INRCRDS_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24)* positif(RDS_REF - (max(0,RDS_R9901))) * positif(FLAG_C02+FLAG_C22)
	     * min(1,((RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24)/(RDS_REF-max(0,RDS_R9901)))))
             * (1-positif(INRCRDS_NTL_A)*positif(INRCRDS_NTL_22_A+INRCRDS_NTL_24_A))
          + ((INRCRDS_TL_A+INRCRDS_TL_22_A+INRCRDS_TL_24_A)*null(TL_RD) * TL_RD_A
          * (1- FLAG_DEFAUT)
             *positif(RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24)* positif(RDS_REF - (max(0,RDS_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT) * 
	    ((positif(SOM9YI) * 
	    (
	  (INRCRDS_TL_A + INRCRDS_R99R_A+INRCRDS_NTL_A - max(0,arr(RDS_TLDEC * TXINR_PA/100))) * positif(RDS_R99R - RDS_TLDEC)
         + (INRCRDS_R99R_A+INRCRDS_NTL_A - max(0,arr(RDS_R99R * TXINR_PA/100))) * positif(RDS_TLDEC-RDS_R99R)
         + (INRCRDS_TL_A + INRCRDS_R99R_A+INRCRDS_NTL_A - max(0,arr(RDS_R99R * TXINR_PA/100))) * null(RDS_TLDEC-RDS_R99R))
         + (1-positif(SOM9YI+0)) * (
         arr((INRCRDS_TL_A*TL_RD_A *TL_RD+(INRCRDS_NTL_A +INRCRDS_R99R+INRCRDS_R9901-INRCRDS_RETDEF) 
                      * ((RDS_REF - RDS_TLDEC)/(RDS_REF-max(0,RDS_REF_A)))))
                      * positif(RDS_REF - RDS_TLDEC)  * positif(RDS_TLDEC - RDS_R99R) 
                      * positif(INRCRDS_R99R_A+INRCRDS_R9901_A+0)
         + (INR_TOT_GLOB_CRDSC - DO_INR_CRDS_A - arr((max(0,RDS_TLDEC -NRDBASEBIS)) * TXINR_PA/100))
                       * positif(RDS_REF - RDS_TLDEC+NRDBASEBIS-NRDBASEBIS_A)
                        )) * (1-positif(FLAG_RETARD22))
           + (positif(SOM9YI) * (
             (INRCRDS_TL_22_A + INRCRDS_R99R_A+INRCRDS_NTL_22_A - max(0,arr(RDS_TLDEC_22* TXINR_PA22/100))) 
	                                      * positif(RDS_R9901 - max(RDS_TLDEC_24,RDS_TLDEC_22))
             + (INRCRDS_R99R_A+INRCRDS_NTL_22_A - max(0,arr(RDS_R99R * TXINR_PA22/100)))
                                                * positif(max(RDS_TLDEC_24,RDS_TLDEC_22)-(RDS_R9901))
               + (INRCRDS_TL_22_A + INRCRDS_R99R_A+INRCRDS_NTL_22_A - max(0,arr(RDS_R99R* TXINR_PA22/100))) 
	                                     * null(max(RDS_TLDEC_24,RDS_TLDEC_22)-(RDS_R9901)))
	       + (1-positif(SOM9YI+0)) * (
              arr((INRCRDS_TL_22_A*TL_RD_A *TL_RD+(INRCRDS_NTL_22_A +INRCRDS_R99R+INRCRDS_R9901-INRCRDS_RETDEF)
                       * ((RDS_REF - RDS_TLDEC_22)/(RDS_REF-max(0,RDS_REF_A)))))
                      * positif(RDS_REF - max(RDS_TLDEC_24,RDS_TLDEC_22))  * positif(max(RDS_TLDEC_24,RDS_TLDEC_22) - RDS_R9901)
                      * positif(INRCRDS_R99R_A+INRCRDS_R9901_A+0)
          +   (INRCRDS_TL_22_A+INRCRDS_NTL_22_A - DO_INR_CRDS_A - arr((max(0,RDS_TLDEC_22 -NRDBASEBIS ))* TXINR_PA22/100))
                            * positif(RDS_REF- max(RDS_TLDEC_24,RDS_TLDEC_22)+NRDBASEBIS-NRDBASEBIS_A)
                         )) * positif(FLAG_RETARD22)
               ));


RECUP_INR_CRDS = max(0,(min(max(0,DO_INR_CRDS_A-RECUP_INR_CRDS_A),arr(max(0,DO_INR_CRDS_A-RECUP_INR_CRDS_A) * (RDS_TLDEC - RDS_A)/DO_CRDS_A))
                    *positif(RDS_TLDEC-RDS_A)*positif(RDS_REF-RDS_A)
                    * positif(CRDS_PA - RDS_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_CRDS_A-RECUP_INR_CRDS_A),arr((RDS_R99R - RDS_A) * TXINR_PA/100))*positif(RDS_TLDEC - RDS_A)
                    * (1-positif(CRDS_PA - RDS_TLDEC))
		* positif(max(0,DO_INR_CRDS_A-RECUP_INR_CRDS_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_CRDS2 = (RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24) * positif(RDS_REF - RDS_TLDEC_22-RDS_TLDEC_24)* positif(RDS_A);
SUP_CRDS_MAX2 = (RDS_REF - max(0,RDS_R9901)) * positif(RDS_REF - max(0,RDS_R9901))* positif(RDS_A);
DO_INR_CRDS982 = max(0,
          arr((RDS_REF - CRDS_NTLDEC_198) * TXINRRED_A/100) 
            *positif(RDS_REF - CRDS_NTLDEC_198))*(1-positif(DO_INR_CRDS2)) * present(CRDS_NTLDEC_198);
DO_INR_CRDS992 = max(0,
          arr((RDS_REF - RDS_TLDEC_199) * TXINRRED_A/100)
            *positif(RDS_REF - RDS_TLDEC_199))*(1-positif(DO_INR_CRDS2)) * present(RDS_TLDEC_199);
INRCRDS_RECT= arr((CRDS_RECT-CRDSIM) * (TXINR_PA/100)) * positif(CRDS_RECT) * FLAG_RECTIF;
INR_CRDS_TOT = max(INRCRDS_NTLDECD+INRCRDS_NTLDECD_22+INRCRDS_NTLDECD_24+(INRCRDS_TLDECD+INRCRDS_TLDEC_22+INRCRDS_TLDEC_24)*TL_RD-INR_CRDS_TARDIF*null(1-IND_PASSAGE)+INRCRDS_R99R+RECUP_INR_CRDS,0) 
	       * (1-IND_RJLJ);
INCRD_TL2 = INRCRDS_TLDEC;
INCRD_TL_222 = INRCRDS_TLDEC_22;
INRRDS_NET2 = max(INRCRDS_NTLDECD+INRCRDS_TLDECD*TL_RD+INRCRDS_R99R+RECUP_INR_CRDS,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_CRDS2 * (-1);
INRRDS_NET_222 = max(INRCRDS_NTLDECD_22+INRCRDS_NTLDECD_24+(INRCRDS_TLDEC_22+INRCRDS_TLDEC_24)*TL_RD,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_CRDS982 + DO_INR_CRDS992)*(-1);
INRD_TL2 = INRCRDS_TLA * TL_RD;
INRD_TL_222 = INRCRDS_TLA_22 * TL_RD;
INCRD_NET2 = max(0,(INRRDS_NET2 +INRRDS_NET_222+ INCRD_NET_A+(INRCRDS_TL_A+INRCRDS_TL_22_A+INRCRDS_TL_24_A)*(1-null(TL_RD_A-TL_RD))*positif(TL_RD))) * positif(RDBASE_INR+NRDBASE)* (1-IND_RJLJ);
CRDS_PRI2=RDS_R9901;
CRDS_ANT2=RDS_A;
CRDS_NTL2=CRDS_NTLDEC;
CRDS_NTL_222=CRDS_NTLDEC_22;
CRDS_TL2=RDS_TLDEC;
CRDS_TL_222=RDS_TLDEC_22;
CRDS_REF_INR=RDS_REF;
INRPSOL_R99RA = INRPSOL_R99R_A;
INRPSOL_R99R = arr((PSOL_R99R -PRSPROV) * (TXINR_PA/100)-INCPSOL_NET_A)
                  * positif(PSOL_R99R-PSOL_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRPSOL_R9901A = INRPSOL_R9901_A;
INRPSOL_R9901 = arr(PSOL_R9901 * (TXINR_PA/100)-INCPSOL_NET_A) * positif(PSOL_R9901- PSOL_R9901_A)
              * positif(IND_PASSAGE-1) * positif(PSOL_TLDEC-PSOL_R9901) * positif(PSOL_R9901_A)
             + (arr(PSOL_R9901 * (TXINR_PA/100))-INCPSOL_NET_A) * positif(PSOL_R9901- PSOL_A)
              * positif(IND_PASSAGE-1) * positif(PSOL_TLDEC-PSOL_R9901) * (1-positif(PSOL_R9901_A))
             + (INCPSOL_NET_A - arr(PSOL_R9901 * (TXINR_PA/100))) * positif(PSOL_A- PSOL_R9901)
              * positif(IND_PASSAGE-1) * positif(PSOL_TLDEC-PSOL_R9901) * (1-positif(PSOL_R9901_A)) * positif(PSOL_R9901)
	     ;
DO_INR_PSOLC=DO_INR_PSOL_A;
INR_NTL_GLOB_PSOL2 = INRPSOL_NTLDECD + INRPSOL_NTL_A+ INRPSOL_NTLDECD_22 + INRPSOL_NTL_22_A+INRPSOL_NTLDECD_24 + INRPSOL_NTL_24_A;
INR_TL_GLOB_PSOL2 = INRPSOL_TLDECD + INRPSOL_TL_A+INRPSOL_TLDEC_22 + INRPSOL_TL_22_A+INRPSOL_TLDEC_24 + INRPSOL_TL_24_A;
INR_TOT_GLOB_PSOL2 = (INR_NTL_GLOB_PSOL2 + INR_TL_GLOB_PSOL2*TL_PS+INRPSOL_R99R+INRPSOL_R99R_A)  * (1-IND_RJLJ);
INR_TOT_GLOB_PSOLC = (INRPSOL_NTLDECD+ INRPSOL_NTL_A+ (INRPSOL_TLDECD + INRPSOL_TL_A)*TL_PSOL +INRPSOL_R99R+INRPSOL_R99R_A) * (1-IND_RJLJ) ;
DO_INR_PSOL2 = (1-null(PSOL_REF_A-min(PSOL_REF,max(PSOL_TLDEC_22,PSOL_TLDEC_24)))) * max(0,
           (arr(((INRPSOL_TL_A+INRPSOL_TL_22_A+INRPSOL_TL_24_A)*TL_PSOL_A*TL_PSOL + INRPSOL_NTL_A+INRPSOL_NTL_22_A+INRPSOL_NTL_24_A)
           * min(1,((PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24)/(PSOL_REF-max(0,PSOL_R9901)))) )
            * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
            * positif(PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24)* positif(PSOL_REF - (max(0,PSOL_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(PSOL_TLDEC_22 +PSOL_TLDEC_24- PSOL_A))
           +arr(((INRPSOL_TL_A+INRPSOL_TL_22_A+INRPSOL_TL_24_A)*TL_PSOL_A*TL_PSOL)
             * min(1,((PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24)/(PSOL_REF-max(0,PSOL_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24)* positif(PSOL_REF - (max(0,PSOL_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(PSOL_TLDEC_22 +PSOL_TLDEC_24- PSOL_A))
             * (1-positif(INRPSOL_NTL_A+INRPSOL_NTL_22_A+INRPSOL_NTL_24_A))
          + (INRPSOL_NTL_A*FLAG_C02+(INRPSOL_NTL_22_A+INRPSOL_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24)* positif(PSOL_REF - (max(0,PSOL_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRPSOL_NTL_A)*positif(INRPSOL_NTL_22_A+INRPSOL_NTL_24_A)
          + arr((INRPSOL_NTL_A*FLAG_C02+(INRPSOL_NTL_22_A+INRPSOL_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24)* positif(PSOL_REF - (max(0,PSOL_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24)/(PSOL_REF-max(0,PSOL_R9901)))))
             * (1-positif(INRPSOL_NTL_A)*positif(INRPSOL_NTL_22_A+INRPSOL_NTL_24_A))
          + ((INRPSOL_TL_A+INRPSOL_TL_22_A+INRPSOL_TL_24_A)*null(TL_PSOL) * TL_PSOL_A
          * (1- FLAG_DEFAUT)
             *positif(PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24)* positif(PSOL_REF - (max(0,PSOL_R9901))))
          + positif(FLAG_RETARD + FLAG_DEFAUT) * (
                (positif(SOM9YI) * ( 
	   (INRPSOL_TL_A + INRPSOL_R99R_A+INRPSOL_NTL_A - max(0,arr(PSOL_TLDEC* TXINR_PA/100))) * positif(PSOL_R99R - PSOL_TLDEC)
         + (INRPSOL_R99R_A+INRPSOL_NTL_A - max(0,arr(PSOL_R99R * TXINR_PA/100))) * positif(PSOL_TLDEC-PSOL_R99R)
         + (INRPSOL_TL_A + INRPSOL_R99R_A+INRPSOL_NTL_A - max(0,arr(PSOL_R99R* TXINR_PA/100))) * null(PSOL_TLDEC-(PSOL_R99R)))
         + (1-positif(SOM9YI)) * (
	    arr((INRPSOL_TL_A*TL_PSOL_A *TL_PSOL+(INRPSOL_NTL_A +INRPSOL_R99R+INRPSOL_R9901-INRPSOL_RETDEF) 
                       * ((PSOL_REF - PSOL_TLDEC)/(PSOL_REF-max(0,PSOL_REF_A)))))
                       * positif(PSOL_REF - PSOL_TLDEC)  * positif(PSOL_TLDEC - PSOL_R99R) 
                       * positif(INRPSOL_R99R_A+INRPSOL_R9901_A+0)
         + (INR_TOT_GLOB_PSOLC - DO_INR_PSOL_A - arr((max(0,PSOL_TLDEC -NPSOLBASEBIS))* TXINR_PA/100))
                       * positif(PSOL_REF - PSOL_TLDEC+NPSOLBASEBIS-NPSOLBASEBIS_A)
                           )
		       )* (1-positif(FLAG_RETARD22))
          +      (positif(SOM9YI) * (
             (INRPSOL_TL_22_A + INRPSOL_R99R_A+INRPSOL_NTL_22_A - max(0,arr(PSOL_TLDEC_22 * TXINR_PA22/100))) * positif(PSOL_R9901 - max(PSOL_TLDEC_24,PSOL_TLDEC_22))
              + (INRPSOL_R99R_A+INRPSOL_NTL_A - max(0,arr(PSOL_R99R * TXINR_PA22/100))) * positif(max(PSOL_TLDEC_24,PSOL_TLDEC_22)-(PSOL_R9901))
               + (INRPSOL_TL_22_A + INRPSOL_R99R_A+INRPSOL_NTL_22_A - max(0,arr(PSOL_R99R* TXINR_PA22/100))) * null(max(PSOL_TLDEC_24,PSOL_TLDEC_22)-(PSOL_R9901)))
                + (1-positif(SOM9YI)) * (
                arr((INRPSOL_TL_22_A*TL_PSOL_A *TL_PSOL+(INRPSOL_NTL_22_A +INRPSOL_R99R+INRPSOL_R9901-INRPSOL_RETDEF)
                          * ((PSOL_REF - PSOL_TLDEC_22)/(PSOL_REF-max(0,PSOL_REF_A)))))
                          * positif(PSOL_REF - max(PSOL_TLDEC_24,PSOL_TLDEC_22))  * positif(max(PSOL_TLDEC_24,PSOL_TLDEC_22) - PSOL_R9901)
                         * positif(INRPSOL_R99R_A+INRPSOL_R9901_A+0)
                + (INRPSOL_TL_22_A+INRPSOL_NTL_22_A - DO_INR_PSOL_A - arr((max(0,PSOL_TLDEC_22-NPSOLBASEBIS)) * TXINR_PA22/100))
                              * positif(PSOL_REF - max(PSOL_TLDEC_24,PSOL_TLDEC_22)+NPSOLBASEBIS-NPSOLBASEBIS_A)
                       )
                        ) * positif(FLAG_RETARD22)
                                            ));

RECUP_INR_PSOL = max(0,(min(max(0,DO_INR_PSOL_A-RECUP_INR_PSOL_A),arr(max(0,DO_INR_PSOL_A-RECUP_INR_PSOL_A) * (PSOL_TLDEC - PSOL_A)/DO_PSOL_A))
                    *positif(PSOL_TLDEC-PSOL_A)*positif(PSOL_REF-PSOL_A)
                    * positif(PSOL_PA - PSOL_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_PSOL_A-RECUP_INR_PSOL_A),arr((PSOL_R99R - PSOL_A) * TXINR_PA/100))*positif(PSOL_TLDEC - PSOL_A)
                    * (1-positif(PSOL_PA - PSOL_TLDEC))
		* positif(max(0,DO_INR_PSOL_A-RECUP_INR_PSOL_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_PSOL2 = (PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24) * positif(PSOL_REF - PSOL_TLDEC_22-PSOL_TLDEC_24)* positif(PSOL_A);
SUP_PSOL_MAX2 = (PSOL_REF - max(0,PSOL_R9901)) * positif(PSOL_REF - max(0,PSOL_R9901))* positif(PSOL_A);
DO_INR_PSOL982 = max(0,
          arr((PSOL_REF - PSOL_NTLDEC_198) * TXINRRED_A/100) 
            *positif(PSOL_REF - PSOL_NTLDEC_198))*(1-positif(DO_INR_PSOL2)) * present(PSOL_NTLDEC_198);
DO_INR_PSOL992 = max(0,
          arr((PSOL_REF - PSOL_TLDEC_199) * TXINRRED_A/100)
            *positif(PSOL_REF - PSOL_TLDEC_199))*(1-positif(DO_INR_PSOL2)) * present(PSOL_TLDEC_199);
INRPSOL_RECT= arr((PSOL_RECT-PRSPROV) * (TXINR_PA/100)) * positif(PSOL_RECT) * FLAG_RECTIF;
INR_PSOL_TOT = max(INRPSOL_NTLDECD+INRPSOL_NTLDECD_22+INRPSOL_NTLDECD_24+(INRPSOL_TLDECD+INRPSOL_TLDEC_22+INRPSOL_TLDEC_22)*TL_PSOL-INR_PSOL_TARDIF*null(1-IND_PASSAGE)+INRPSOL_R99R+RECUP_INR_PSOL,0) * (1-IND_RJLJ);
INCPSOL_TL2 = INRPSOL_TLDECD;
INCPSOL_TL_222 = INRPSOL_TLDEC_22;
INRPSOL_NET2 = max(INRPSOL_NTLDECD+INRPSOL_TLDECD*TL_PSOL+INRPSOL_R99R+RECUP_INR_PSOL,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_PSOL2 * (-1);
INRPSOL_NET_222 = max(INRPSOL_NTLDECD_22+INRPSOL_NTLDECD_24+(INRPSOL_TLDEC_22+INRPSOL_TLDEC_24)*TL_PSOL,0)*(1-IND_RJLJ)*(1-FLAG_NINR) + (DO_INR_PSOL982 + DO_INR_PSOL992)*(-1);
INPSOL_TL2 = INRPSOL_TLA * TL_PSOL;
INPSOL_TL_222 = INRPSOL_TLA_22 * TL_PSOL;
INCPSOL_NET2 = max(0,(INRPSOL_NET2 + INRPSOL_NET_222 + INCPSOL_NET_A+(INRPSOL_TL_A+INRPSOL_TL_22_A+INRPSOL_TL_24_A)*(1-null(TL_PSOL_A-TL_PSOL))*positif(TL_PSOL))) * positif(PSOLBASE_INR+NPSOLBASE) * (1-IND_RJLJ);
PSOL_PRI2=PSOL_R9901;
PSOL_ANT2=PSOL_A;
PSOL_NTL2=PSOL_NTLDEC;
PSOL_NTL_222=PSOL_NTLDEC_22;
PSOL_TL2=PSOL_TLDEC;
PSOL_TL_222=PSOL_TLDEC_22;
PSOL_REF_INR=PSOL_REF;
INRTAXA_R99RA = INRTAXA_R99R_A;
INRTAXA_R99R = arr(TAXA_R99R * (TXINR_PA/100)-INCTAXA_NET_A) * positif(TAXA_R99R-TAXA_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRTAXA_R9901A = INRTAXA_R9901_A;
INRTAXA_R9901 = arr(TAXA_R9901 * (TXINR_PA/100)-INCTAXA_NET_A) * positif(TAXA_R9901- TAXA_R9901_A)
              * positif(IND_PASSAGE-1) * positif(TAXA_TLDEC-TAXA_R9901) * positif(TAXA_R9901_A)
             + (arr(TAXA_R9901 * (TXINR_PA/100))-INCTAXA_NET_A) * positif(TAXA_R9901- TAXABASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(TAXA_TLDEC-TAXA_R9901) * (1-positif(TAXA_R9901_A))
             + (INCTAXA_NET_A - arr(TAXA_R9901 * (TXINR_PA/100))) * positif(TAXABASE_INR_A- TAXA_R9901)
              * positif(IND_PASSAGE-1) * positif(TAXA_TLDEC-TAXA_R9901) * (1-positif(TAXA_R9901_A)) * positif(TAXA_R9901)
	     ;
DO_INR_TAXAC=DO_INR_TAXA_A;
INR_NTL_GLOB_TAXA2 = INRTAXA_NTLDECD + INRTAXA_NTL_A+ INRTAXA_NTLDECD_22 + INRTAXA_NTL_22_A+INRTAXA_NTLDECD_24 + INRTAXA_NTL_24_A;
INR_TL_GLOB_TAXA2 = INRTAXA_TLDECD + INRTAXA_TL_A+ INRTAXA_TLDEC_22 + INRTAXA_TL_22_A+INRTAXA_TLDEC_24 + INRTAXA_TL_24_A;
INR_TOT_GLOB_TAXA2 = max(0,INR_NTL_GLOB_TAXA2 + INR_TL_GLOB_TAXA2*TL_TAXAGA+INRTAXA_R99R+INRTAXA_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_TAXAC= (INRTAXA_NTLDECD+ INRTAXA_NTL_A+ (INRTAXA_TLDECD + INRTAXA_TL_A)*TL_TAXAGA +INRTAXA_R99R+INRTAXA_R99R_A) * (1-IND_RJLJ) ;

DO_INR_TAXA2 = (1-null(TAXA_REF_A-min(TAXA_REF,max(TAXA_TLDEC_22,TAXA_TLDEC_24)))) * max(0,
           (arr(((INRTAXA_TL_A+INRTAXA_TL_22_A+INRTAXA_TL_24_A)*TL_TAXAGA_A*TL_TAXAGA + INRTAXA_NTL_A+INRTAXA_NTL_22_A+INRTAXA_NTL_24_A)
           * min(1,((TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24)/(TAXA_REF-max(0,TAXA_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24)* positif(TAXA_REF - (max(0,TAXA_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(TAXA_TLDEC_22+TAXA_TLDEC_24 - TAXABASE_INR_A))
           +arr(((INRTAXA_TL_A+INRTAXA_TL_22_A+INRTAXA_TL_24_A)*TL_TAXAGA_A*TL_TAXAGA)
             * min(1,((TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24)/(TAXA_REF-max(0,TAXA_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24)* positif(TAXA_REF - (max(0,TAXA_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(TAXA_TLDEC_22 +TAXA_TLDEC_24- TAXABASE_INR_A))
             * (1-positif(INRTAXA_NTL_A+INRTAXA_NTL_22_A+INRTAXA_NTL_24_A))
          + (INRTAXA_NTL_A*FLAG_C02+(INRTAXA_NTL_22_A+INRTAXA_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24)* positif(TAXA_REF - (max(0,TAXA_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRTAXA_NTL_A)*positif(INRTAXA_NTL_22_A+INRTAXA_NTL_24_A)
          + arr((INRTAXA_NTL_A*FLAG_C02+(INRTAXA_NTL_22_A+INRTAXA_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24)* positif(TAXA_REF - (max(0,TAXA_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24)/(TAXA_REF-max(0,TAXA_R9901)))))
             * (1-positif(INRTAXA_NTL_A)*positif(INRTAXA_NTL_22_A+INRTAXA_NTL_24_A))
          + ((INRTAXA_TL_A+INRTAXA_TL_22_A+INRTAXA_TL_24_A)*null(TL_TAXAGA) * TL_TAXAGA_A
          * (1- FLAG_DEFAUT)
             *positif(TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24)* positif(TAXA_REF - (max(0,TAXA_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* 
	   ((INRTAXA_TL_A + INRTAXA_R99R_A+INRTAXA_NTL_A - max(0,arr(TAXA_TLDEC * TXINR_PA/100))) * positif(TAXA_R99R - TAXA_TLDEC)
         + (INRTAXA_R99R_A+INRTAXA_NTL_A - max(0,arr(TAXA_R99R * TXINR_PA/100))) * positif(TAXA_TLDEC-(TAXA_R99R))
         + (INRTAXA_TL_A + INRTAXA_R99R_A+INRTAXA_NTL_A - max(0,arr(TAXA_R99R * TXINR_PA/100))) * null(TAXA_TLDEC-(TAXA_R99R)))
         +                        (1-positif(SOM9YI+0)) * (
	   arr((INRTAXA_TL_A*TL_TAXAGA_A *TL_TAXAGA+(INRTAXA_NTL_A +INRTAXA_R99R+INRTAXA_R9901-INRTAXA_RETDEF) 
                       * ((TAXA_REF - TAXA_TLDEC)/(TAXA_REF-max(0,TAXA_REF_A)))))
                       * positif(TAXA_REF - TAXA_TLDEC)  * positif(TAXA_TLDEC - TAXA_R99R) 
                       * positif(INRTAXA_R99R_A+INRTAXA_R9901_A+0)
         + (INR_TOT_GLOB_TAXAC - DO_INR_TAXA_A - arr(TAXA_TLDEC * TXINR_PA/100))
		       * positif(TAXA_REF - TAXA_TLDEC)
		                          ))* (1-positif(FLAG_RETARD22))
             +                    (positif(SOM9YI)*
                ((INRTAXA_TL_22_A + INRTAXA_R99R_A+INRTAXA_NTL_22_A - max(0,arr(TAXA_TLDEC_22 * TXINR_PA22/100))) * positif(TAXA_R9901 - max(TAXA_TLDEC_24,TAXA_TLDEC_22))
         + (INRTAXA_R99R_A+INRTAXA_NTL_22_A - max(0,arr(TAXA_R99R * TXINR_PA22/100))) * positif(max(TAXA_TLDEC_24,TAXA_TLDEC_22)-(TAXA_R9901))
         + (INRTAXA_TL_22_A + INRTAXA_R99R_A+INRTAXA_NTL_22_A - max(0,arr(TAXA_R99R * TXINR_PA22/100))) * null(max(TAXA_TLDEC_24,TAXA_TLDEC_22)-(TAXA_R9901)))
         +                        (1-positif(SOM9YI+0)) * (
              arr((INRTAXA_TL_22_A*TL_TAXAGA_A *TL_TAXAGA+(INRTAXA_NTL_22_A +INRTAXA_R99R+INRTAXA_R9901-INRTAXA_RETDEF)
                         * ((TAXA_REF - TAXA_TLDEC_22)/(TAXA_REF-max(0,TAXA_REF_A)))))
                      * positif(TAXA_REF - max(TAXA_TLDEC_24,TAXA_TLDEC_22))  * positif(max(TAXA_TLDEC_24,TAXA_TLDEC_22) - TAXA_R9901)
                         * positif(INRTAXA_R99R_A+INRTAXA_R9901_A+0)
          + (INRTAXA_TL_22_A+INRTAXA_NTL_22_A - DO_INR_TAXA_A - arr(TAXA_TLDEC_22 * TXINR_PA22/100))
			* positif(TAXA_REF - max(TAXA_TLDEC_24,TAXA_TLDEC_22))
                                        ))* positif(FLAG_RETARD22)
               ));

RECUP_INR_TAXA = max(0,(min(max(0,DO_INR_TAXA_A-RECUP_INR_TAXA_A),arr(max(0,DO_INR_TAXA_A-RECUP_INR_TAXA_A) * (TAXA_TLDEC - TAXABASE_INR_A)/DO_TAXA_A))
                    *positif(TAXA_TLDEC-TAXABASE_INR_A)*positif(TAXA_REF-TAXABASE_INR_A)
                    * positif(TAXA_PA - TAXA_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ max(0,DO_INR_TAXA_A-RECUP_INR_TAXA_A)*positif(TAXA_TLDEC - TAXABASE_INR_A)
                   * (1-positif(TAXA_PA - TAXA_TLDEC))
                    *positif(max(0,DO_INR_TAXA_A-RECUP_INR_TAXA_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_TAXA2 = (TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24) * positif(TAXA_REF - TAXA_TLDEC_22-TAXA_TLDEC_24)* positif(TAXABASE_INR_A);
SUP_TAXA_MAX2 = (TAXA_REF - max(0,TAXA_R9901)) * positif(TAXA_REF - max(0,TAXA_R9901))* positif(TAXABASE_INR_A);
DO_INR_TAXA982 = max(0,
          arr((TAXA_REF - TAXA_NTLDEC_198) * TXINRRED_A/100) 
            *positif(TAXA_REF - TAXA_NTLDEC_198))*(1-positif(DO_INR_TAXA2)) * present(TAXA_NTLDEC_198);
DO_INR_TAXA992 = max(0,
          arr((TAXA_REF - TAXA_TLDEC_199) * TXINRRED_A/100)
            *positif(TAXA_REF - TAXA_TLDEC_199))*(1-positif(DO_INR_TAXA2)) * present(TAXA_TLDEC_199);
INR_TAXAGA_TOT = max(INRTAXA_NTLDECD+INRTAXA_NTLDEC_22+INRTAXA_NTLDEC_24 + (INRTAXA_TLDEC+INRTAXA_TLDEC_22+INRTAXA_TLDEC_24)*TL_TAXAGA-INR_TAXAGA_TARDIF*null(1-IND_PASSAGE)+INRTAXA_R99R+RECUP_INR_TAXA,0) 
	      * (1-IND_RJLJ);
INRTAXA_RECT= arr(TAXAGA_RECT * (TXINR_PA/100)) * positif(TAXAGA_RECT) * FLAG_RECTIF;
INCTAXA_TL2 = INRTAXA_TLDECD;
INCTAXA_TL_222 = INRTAXA_TLDEC_22;
INTAXA_TL2 = INRTAXA_TLA * TL_TAXAGA;
INTAXA_TL_222 = INRTAXA_TLA_22 * TL_TAXAGA;
INRTAXA_NET2 = max(INRTAXA_NTLDECD+INRTAXA_TLDECD*TL_TAXAGA+INRTAXA_R99R+RECUP_INR_TAXA,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_TAXA2 * (-1);
INRTAXA_NET_222 = max(INRTAXA_NTLDECD_22+INRTAXA_NTLDECD_24+(INRTAXA_TLDEC_22+INRTAXA_TLDEC_24)*TL_TAXAGA,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_TAXA982 + DO_INR_TAXA992)*(-1);
INCTAXA_NET2 = max(0,(INRTAXA_NET2 + INRTAXA_NET_222 + INCTAXA_NET_A+(INRTAXA_TL_A+INRTAXA_TL_22_A+INRTAXA_TL_24_A)*(1-null(TL_TAXAGA_A-TL_TAXAGA))*positif(TL_TAXAGA))) * positif(TAXABASE_INR+NTAXABASE)* (1-IND_RJLJ);
TAXAGA_PRI2=TAXA_R9901;
TAXAGA_ANT2=TAXABASE_INR_A;
TAXAGA_NTL2=TAXA_NTLDEC;
TAXAGA_NTL_222=TAXA_NTLDEC_22;
TAXAGA_TL2=TAXA_TLDEC;
TAXAGA_TL_222=TAXA_TLDEC_22;
TAXA_REF_INR=TAXA_REF;

regle corrective 1084:
application : iliad ;
INRCDIS_R99RA = INRCDIS_R99R_A;
INRCDIS_R99R = arr((CDIS_R99R-CDISPROV) * (TXINR_PA/100)-INCCDIS_NET_A) * positif(CDIS_R99R-CDIS_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRCDIS_R9901A = INRCDIS_R9901_A;
INRCDIS_R9901 = arr(CDIS_R9901 * (TXINR_PA/100)-INCCDIS_NET_A) * positif(CDIS_R9901- CDIS_R9901_A)
              * positif(IND_PASSAGE-1) * positif(CDIS_TLDEC-CDIS_R9901) * positif(CDIS_R9901_A)
             + (arr(CDIS_R9901 * (TXINR_PA/100))-INCCDIS_NET_A) * positif(CDIS_R9901- CDISBASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(CDIS_TLDEC-CDIS_R9901) * (1-positif(CDIS_R9901_A))
             + (INCCDIS_NET_A - arr(CDIS_R9901 * (TXINR_PA/100))) * positif(CDISBASE_INR_A- CDIS_R9901)
              * positif(IND_PASSAGE-1) * positif(CDIS_TLDEC-CDIS_R9901) * (1-positif(CDIS_R9901_A)) * positif(CDIS_R9901)
	     ;
DO_INR_CDISC=DO_INR_CDIS_A;
INR_NTL_GLOB_CDIS2 = INRCDIS_NTLDECD + INRCDIS_NTL_A+ INRCDIS_NTLDECD_22 + INRCDIS_NTL_22_A+INRCDIS_NTLDECD_24 + INRCDIS_NTL_24_A;
INR_TL_GLOB_CDIS2 = INRCDIS_TLDECD + INRCDIS_TL_A+ INRCDIS_TLDEC_22 + INRCDIS_TL_22_A+INRCDIS_TLDEC_24 + INRCDIS_TL_24_A;
INR_TOT_GLOB_CDIS2 = max(0,INR_NTL_GLOB_CDIS2 + INR_TL_GLOB_CDIS2*TL_CDIS+INRCDIS_R99R+INRCDIS_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_CDISC= (INRCDIS_NTLDECD+ INRCDIS_NTL_A+ (INRCDIS_TLDECD + INRCDIS_TL_A)*TL_CDIS +INRCDIS_R99R+INRCDIS_R99R_A) * (1-IND_RJLJ) ;
DO_INR_CDIS2 = (1-null(CDIS_REF_A-min(CDIS_REF,max(CDIS_TLDEC_22,CDIS_TLDEC_24)))) * max(0,
           (arr(((INRCDIS_TL_A+INRCDIS_TL_22_A+INRCDIS_TL_24_A)*TL_CDIS_A*TL_CDIS + INRCDIS_NTL_A+INRCDIS_NTL_22_A+INRCDIS_NTL_24_A)
           * min(1,((CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24)/(CDIS_REF-max(0,CDIS_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24)* positif(CDIS_REF - (max(0,CDIS_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(CDIS_TLDEC_22 +CDIS_TLDEC_24- CDISBASE_INR_A))
           +arr(((INRCDIS_TL_A+INRCDIS_TL_22_A+INRCDIS_TL_24_A)*TL_CDIS_A*TL_CDIS)
             * min(1,((CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24)/(CDIS_REF-max(0,CDIS_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24)* positif(CDIS_REF - (max(0,CDIS_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(CDIS_TLDEC_22 +CDIS_TLDEC_24- CDISBASE_INR_A))
             * (1-positif(INRCDIS_NTL_A+INRCDIS_NTL_22_A+INRCDIS_NTL_24_A))
          + (INRCDIS_NTL_A*FLAG_C02+(INRCDIS_NTL_22_A+INRCDIS_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24)* positif(CDIS_REF - (max(0,CDIS_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRCDIS_NTL_A)*positif(INRCDIS_NTL_22_A+INRCDIS_NTL_24_A)
          + arr((INRCDIS_NTL_A*FLAG_C02+(INRCDIS_NTL_22_A+INRCDIS_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24)* positif(CDIS_REF - (max(0,CDIS_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24)/(CDIS_REF-max(0,CDIS_R9901)))))
             * (1-positif(INRCDIS_NTL_A)*positif(INRCDIS_NTL_22_A+INRCDIS_NTL_24_A))
          + ((INRCDIS_TL_A+INRCDIS_TL_22_A+INRCDIS_TL_24_A)*null(TL_CDIS) * TL_CDIS_A
          * (1- FLAG_DEFAUT)
             *positif(CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24)* positif(CDIS_REF - (max(0,CDIS_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRCDIS_TL_A + INRCDIS_R99R_A+INRCDIS_NTL_A - max(0,arr(CDIS_TLDEC * TXINR_PA/100))) * positif(CDIS_R99R - CDIS_TLDEC)
         + (INRCDIS_R99R_A+INRCDIS_NTL_A - max(0,arr(CDIS_R99R * TXINR_PA/100))) * positif(CDIS_TLDEC-(CDIS_R99R))
         + (INRCDIS_TL_A + INRCDIS_R99R_A+INRCDIS_NTL_A - max(0,arr(CDIS_R99R * TXINR_PA/100))) * null(CDIS_TLDEC-(CDIS_R99R)))
             +           (1-positif(SOM9YI)) * (
          arr((INRCDIS_TL_A*TL_CDIS_A *TL_CDIS+(INRCDIS_NTL_A +INRCDIS_R99R+INRCDIS_R9901-INRCDIS_RETDEF) 
                       * ((CDIS_REF - CDIS_TLDEC)/(CDIS_REF-max(0,CDIS_REF_A)))))
                       * positif(CDIS_REF - CDIS_TLDEC)  * positif(CDIS_TLDEC - CDIS_R99R) 
                       * positif(INRCDIS_R99R_A+INRCDIS_R9901_A+0)
         + (INR_TOT_GLOB_CDISC - DO_INR_CDIS_A - arr(CDIS_TLDEC * TXINR_PA/100))
                       * positif(CDIS_REF - CDIS_TLDEC)
                       )) * (1-positif(FLAG_RETARD22))
                  +               (positif(SOM9YI)* (
            (INRCDIS_TL_22_A + INRCDIS_R99R_A+INRCDIS_NTL_22_A - max(0,arr(CDIS_TLDEC_22 * TXINR_PA22/100))) * positif(CDIS_R9901 - max(CDIS_TLDEC_24,CDIS_TLDEC_22))
             + (INRCDIS_R99R_A+INRCDIS_NTL_22_A - max(0,arr(CDIS_R99R * TXINR_PA22/100))) * positif(max(CDIS_TLDEC_24,CDIS_TLDEC_22)-(CDIS_R9901))
             + (INRCDIS_TL_22_A + INRCDIS_R99R_A+INRCDIS_NTL_22_A - max(0,arr(CDIS_R99R * TXINR_PA22/100))) * null(max(CDIS_TLDEC_24,CDIS_TLDEC_22)-(CDIS_R9901)))
             +           (1-positif(SOM9YI)) * (
             arr((INRCDIS_TL_22_A*TL_CDIS_A *TL_CDIS+(INRCDIS_NTL_22_A +INRCDIS_R99R+INRCDIS_R9901-INRCDIS_RETDEF)
                          * ((CDIS_REF - CDIS_TLDEC_22)/(CDIS_REF-max(0,CDIS_REF_A)))))
                      * positif(CDIS_REF - max(CDIS_TLDEC_24,CDIS_TLDEC_22))  * positif(max(CDIS_TLDEC_24,CDIS_TLDEC_22) - CDIS_R9901)
                          * positif(INRCDIS_R99R_A+INRCDIS_R9901_A+0)
         + (INRCDIS_TL_22_A+INRCDIS_NTL_22_A - DO_INR_CDIS_A - arr(CDIS_TLDEC_22 * TXINR_PA22/100))
                      * positif(CDIS_REF - max(CDIS_TLDEC_24,CDIS_TLDEC_22))
                             )) * positif(FLAG_RETARD22)
                  ));


RECUP_INR_CDIS = max(0,(min(max(0,DO_INR_CDIS_A-RECUP_INR_CDIS_A),arr(max(0,DO_INR_CDIS_A-RECUP_INR_CDIS_A) * (CDIS_TLDEC - CDISBASE_INR_A)/DO_CDIS_A))
                    *positif(CDIS_TLDEC-CDISBASE_INR_A)*positif(CDIS_REF-CDISBASE_INR_A)
                    * positif(CDIS_PA - CDIS_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_CDIS_A-RECUP_INR_CDIS_A),arr((CDIS_R99R - CDISBASE_INR_A) * TXINR_PA/100))*positif(CDIS_TLDEC - CDISBASE_INR_A)
                    * (1-positif(CDIS_PA - CDIS_TLDEC))
                    *positif(max(0,DO_INR_CDIS_A-RECUP_INR_CDIS_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_CDIS2 = (CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24) * positif(CDIS_REF - CDIS_TLDEC_22-CDIS_TLDEC_24)* positif(CDISBASE_INR_A);
SUP_CDIS_MAX2 = (CDIS_REF - max(0,CDIS_R9901)) * positif(CDIS_REF - max(0,CDIS_R9901))* positif(CDISBASE_INR_A);
DO_INR_CDIS982 = max(0,
          arr((CDIS_REF - CDIS_NTLDEC_198) * TXINRRED_A/100) 
            *positif(CDIS_REF - CDIS_NTLDEC_198))*(1-positif(DO_INR_CDIS2)) * present(CDIS_NTLDEC_198);
DO_INR_CDIS992 = max(0,
          arr((CDIS_REF - CDIS_TLDEC_199) * TXINRRED_A/100)
            *positif(CDIS_REF - CDIS_TLDEC_199))*(1-positif(DO_INR_CDIS2)) * present(CDIS_TLDEC_199);
INR_CDIS_TOT = max(INRCDIS_NTLDECD+INRCDIS_NTLDECD_22 +INRCDIS_NTLDECD_24+ (INRCDIS_TLDECD+INRCDIS_TLDEC_22+INRCDIS_TLDEC_24)*TL_CDIS-INR_CDIS_TARDIF*null(1-IND_PASSAGE)+INRCDIS_R99R+RECUP_INR_CDIS,0) 
	      * (1-IND_RJLJ);
INRCDIS_RECT= arr((CDIS_RECT -CDISPROV)* (TXINR_PA/100)) * positif(CDIS_RECT) * FLAG_RECTIF;
INCCDIS_TL2 = INRCDIS_TLDECD;
INCCDIS_TL_222 = INRCDIS_TLDEC_22;
INCDIS_TL2 = INRCDIS_TLA * TL_CDIS;
INCDIS_TL_222 = INRCDIS_TLA_22 * TL_CDIS;
INRCDIS_NET2 = max(INRCDIS_NTLDECD+INRCDIS_TLDECD*TL_CDIS+INRCDIS_R99R+RECUP_INR_CDIS,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_CDIS2 * (-1);
INRCDIS_NET_222 = max(INRCDIS_NTLDECD_22+INRCDIS_NTLDECD_24+(INRCDIS_TLDEC_22+INRCDIS_TLDEC_24)*TL_CDIS,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_CDIS982 + DO_INR_CDIS992)*(-1);
INCCDIS_NET2 = max(0,(INRCDIS_NET2 + INRCDIS_NET_222 + INCCDIS_NET_A+(INRCDIS_TL_A+INRCDIS_TL_22_A+INRCDIS_TL_24_A)*(1-null(TL_CDIS_A-TL_CDIS))*positif(TL_CDIS))) * positif(CDISBASE_INR+NCDISBASE)* (1-IND_RJLJ);
INCCDIS_NET2 = max(0,(INRCDIS_NET2 + INRCDIS_NET_222 + INCCDIS_NET_A+(INRCDIS_TL_A+INRCDIS_TL_22_A+INRCDIS_TL_24_A)*(1-null(TL_CDIS_A-TL_CDIS))*positif(TL_CDIS))) * positif(CDISBASE_INR+NCDISBASE)* (1-IND_RJLJ);
CDIS_PRI2=CDIS_R9901;
CDIS_ANT2=CDISBASE_INR_A;
CDIS_NTL2=CDIS_NTLDEC;
CDIS_NTL_222=CDIS_NTLDEC_22;
CDIS_TL2=CDIS_TLDEC;
CDIS_TL_222=CDIS_TLDEC_22;
CDIS_REF_INR=CDIS_REF;
INRCHR_R99RA = INRCHR_R99R_A;
INRCHR_R99R = arr((CHR_R99R) * (TXINR_PA/100)-INCCHR_NET_A) * positif(CHR_R99R-CHR_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRCHR_R9901A = INRCHR_R9901_A;
INRCHR_R9901 = arr(CHR_R9901 * (TXINR_PA/100)-INCCHR_NET_A) * positif(CHR_R9901- CHR_R9901_A)
              * positif(IND_PASSAGE-1) * positif(CHR_TLDEC-CHR_R9901) * positif(CHR_R9901_A)
             + (arr(CHR_R9901 * (TXINR_PA/100))-INCCHR_NET_A) * positif(CHR_R9901- CHRBASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(CHR_TLDEC-CHR_R9901) * (1-positif(CHR_R9901_A))
             + (INCCHR_NET_A - arr(CHR_R9901 * (TXINR_PA/100))) * positif(CHRBASE_INR_A- CHR_R9901)
              * positif(IND_PASSAGE-1) * positif(CHR_TLDEC-CHR_R9901) * (1-positif(CHR_R9901_A)) * positif(CHR_R9901)
	     ;
DO_INR_CHRC=DO_INR_CHR_A;
INR_NTL_GLOB_CHR2 = INRCHR_NTLDECD + INRCHR_NTL_A+ INRCHR_NTLDECD_22 + INRCHR_NTL_22_A+INRCHR_NTLDECD_24 + INRCHR_NTL_24_A;
INR_TL_GLOB_CHR2 = INRCHR_TLDECD + INRCHR_TL_A+ INRCHR_TLDEC_22 + INRCHR_TL_22_A+INRCHR_TLDEC_24 + INRCHR_TL_24_A;
INR_TOT_GLOB_CHR2 = max(0,INR_NTL_GLOB_CHR2 + INR_TL_GLOB_CHR2*TL_CHR+INRCHR_R99R+INRCHR_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_CHRC= (INRCHR_NTLDECD+ INRCHR_NTL_A+ (INRCHR_TLDECD + INRCHR_TL_A)*TL_CHR +INRCHR_R99R+INRCHR_R99R_A) * (1-IND_RJLJ) ;

DO_INR_CHR2 = (1-null(CHR_REF_A-min(CHR_REF,max(CHR_TLDEC_22,CHR_TLDEC_24)))) * max(0,
           (arr(((INRCHR_TL_A+INRCHR_TL_22_A+INRCHR_TL_24_A)*TL_CHR_A*TL_CHR + INRCHR_NTL_A+INRCHR_NTL_22_A+INRCHR_NTL_24_A)
           * min(1,((CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24)/(CHR_REF-max(0,CHR_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24)* positif(CHR_REF - (max(0,CHR_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(CHR_TLDEC_22 +CHR_TLDEC_24- CHRBASE_INR_A))
           +arr(((INRCHR_TL_A+INRCHR_TL_22_A+INRCHR_TL_24_A)*TL_CHR_A*TL_CHR)
             * min(1,((CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24)/(CHR_REF-max(0,CHR_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24)* positif(CHR_REF - (max(0,CHR_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(CHR_TLDEC_22 +CHR_TLDEC_24- CHRBASE_INR_A))
             * (1-positif(INRCHR_NTL_A+INRCHR_NTL_22_A+INRCHR_NTL_24_A))
          + (INRCHR_NTL_A*FLAG_C02+(INRCHR_NTL_22_A+INRCHR_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24)* positif(CHR_REF - (max(0,CHR_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRCHR_NTL_A)*positif(INRCHR_NTL_22_A+INRCHR_NTL_24_A)
          + arr((INRCHR_NTL_A*FLAG_C02+(INRCHR_NTL_22_A+INRCHR_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24)* positif(CHR_REF - (max(0,CHR_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24)/(CHR_REF-max(0,CHR_R9901)))))
             * (1-positif(INRCHR_NTL_A)*positif(INRCHR_NTL_22_A+INRCHR_NTL_24_A))
          + ((INRCHR_TL_A+INRCHR_TL_22_A+INRCHR_TL_24_A)*null(TL_CHR) * TL_CHR_A
          * (1- FLAG_DEFAUT)
             *positif(CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24)* positif(CHR_REF - (max(0,CHR_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRCHR_TL_A + INRCHR_R99R_A+INRCHR_NTL_A - max(0,arr(CHR_TLDEC * TXINR_PA/100))) * positif(CHR_R99R - CHR_TLDEC)
         + (INRCHR_R99R_A+INRCHR_NTL_A - max(0,arr(CHR_R99R * TXINR_PA/100))) * positif(CHR_TLDEC-(CHR_R99R))
         + (INRCHR_TL_A + INRCHR_R99R_A+INRCHR_NTL_A - max(0,arr(CHR_R99R * TXINR_PA/100))) * null(CHR_TLDEC-(CHR_R99R)))
             +	      (1-positif(SOM9YI+0))* (
          arr((INRCHR_TL_A*TL_CHR_A *TL_CHR+(INRCHR_NTL_A +INRCHR_R99R+INRCHR_R9901-INRCHR_RETDEF) 
                       * ((CHR_REF - CHR_TLDEC)/(CHR_REF-max(0,CHR_REF_A)))))
                       * positif(CHR_REF - CHR_TLDEC)  * positif(CHR_TLDEC - CHR_R99R) 
                       * positif(INRCHR_R99R_A+INRCHR_R9901_A+0)
         + (INR_TOT_GLOB_CHRC - DO_INR_CHR_A - arr(CHR_TLDEC * TXINR_PA/100))
                       * positif(CHR_REF - CHR_TLDEC)
                         )
		       )* (1-FLAG_RETARD22)
                       +          (positif(SOM9YI)* (
                 (INRCHR_TL_22_A + INRCHR_R99R_A+INRCHR_NTL_22_A - max(0,arr(CHR_TLDEC_22 * TXINR_PA22/100))) * positif(CHR_R9901 - max(CHR_TLDEC_24,CHR_TLDEC_22))
          + (INRCHR_R99R_A+INRCHR_NTL_22_A - max(0,arr(CHR_R99R * TXINR_PA22/100))) * positif(max(CHR_TLDEC_24,CHR_TLDEC_22)-(CHR_R9901))
          + (INRCHR_TL_22_A + INRCHR_R99R_A+INRCHR_NTL_22_A - max(0,arr(CHR_R99R * TXINR_PA22/100))) * null(max(CHR_TLDEC_24,CHR_TLDEC_22)-(CHR_R9901)))
          +        (1-positif(SOM9YI+0))* (
      arr((INRCHR_TL_22_A*TL_CHR_A *TL_CHR+(INRCHR_NTL_22_A +INRCHR_R99R+INRCHR_R9901-INRCHR_RETDEF)
                       * ((CHR_REF - CHR_TLDEC_22)/(CHR_REF-max(0,CHR_REF_A)))))
                  * positif(CHR_REF - max(CHR_TLDEC_24,CHR_TLDEC_22))  * positif(max(CHR_TLDEC_24,CHR_TLDEC_22) - CHR_R9901)
                    * positif(INRCHR_R99R_A+INRCHR_R9901_A+0)
              + (INRCHR_TL_22_A+INRCHR_NTL_22_A - DO_INR_CHR_A - arr(CHR_TLDEC_22 * TXINR_PA22/100))
                         * positif(CHR_REF - max(CHR_TLDEC_24,CHR_TLDEC_22))
          + (INRCHR_R99R_A + INRCHR_NTL_22_A- arr(CHR_R9901 * TXINR_PA22/100)) * null(max(CHR_TLDEC_24,CHR_TLDEC_22) - CHR_R9901)
                           * positif(CHR_REF - max(CHR_TLDEC_24,CHR_TLDEC_22)) ))* FLAG_RETARD22
                ));


RECUP_INR_CHR = max(0,(min(max(0,DO_INR_CHR_A-RECUP_INR_CHR_A),arr(max(0,DO_INR_CHR_A-RECUP_INR_CHR_A) * (CHR_TLDEC - CHRBASE_INR_A)/DO_CHR_A))
                    *positif(CHR_TLDEC-CHRBASE_INR_A)*positif(CHR_REF-CHRBASE_INR_A)
                    * positif(CHR_PA - CHR_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_CHR_A-RECUP_INR_CHR_A),arr((CHR_R99R - CHRBASE_INR_A) * TXINR_PA/100))*positif(CHR_TLDEC - CHRBASE_INR_A)
                    * (1-positif(CHR_PA - CHR_TLDEC))
                    *positif(max(0,DO_INR_CHR_A-RECUP_INR_CHR_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_CHR2 = (CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24) * positif(CHR_REF - CHR_TLDEC_22-CHR_TLDEC_24)* positif(CHRBASE_INR_A);
SUP_CHR_MAX2 = (CHR_REF - max(0,CHR_R9901)) * positif(CHR_REF - max(0,CHR_R9901))* positif(CHRBASE_INR_A);
DO_INR_CHR982 = max(0,
          arr((CHR_REF - CHR_NTLDEC_198) * TXINRRED_A/100) 
            *positif(CHR_REF - CHR_NTLDEC_198))*(1-positif(DO_INR_CHR2)) * present(CHR_NTLDEC_198);
DO_INR_CHR992 = max(0,
          arr((CHR_REF - CHR_TLDEC_199) * TXINRRED_A/100)
            *positif(CHR_REF - CHR_TLDEC_199))*(1-positif(DO_INR_CHR2)) * present(CHR_TLDEC_199);
INR_CHR_TOT = max(INRCHR_NTLDECD+INRCHR_NTLDECD_22+INRCHR_NTLDECD_24 + (INRCHR_TLDECD+INRCHR_TLDEC_22+INRCHR_TLDEC_24)*TL_CHR-INR_CHR_TARDIF*null(1-IND_PASSAGE)+INRCHR_R99R+RECUP_INR_CHR,0) 
	      * (1-IND_RJLJ);
INRCHR_RECT= arr((CHR_RECT)* (TXINR_PA/100)) * positif(CHR_RECT) * FLAG_RECTIF;
INCCHR_TL2 = INRCHR_TLDECD;
INCCHR_TL_222 = INRCHR_TLDEC_22;
INCHR_TL2 = INRCHR_TLA * TL_CHR;
INCHR_TL_222 = INRCHR_TLA_22 * TL_CHR;
INRCHR_NET2 = max(INRCHR_NTLDECD+INRCHR_TLDECD*TL_CHR+INRCHR_R99R+RECUP_INR_CHR,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_CHR2 * (-1);
INRCHR_NET_222 = max(INRCHR_NTLDECD_22+INRCHR_NTLDECD_24+(INRCHR_TLDEC_22+INRCHR_TLDEC_24)*TL_CHR,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_CHR982 + DO_INR_CHR992)*(-1);
INCCHR_NET2 = max(0,(INRCHR_NET2 + INRCHR_NET_222 + INCCHR_NET_A+(INRCHR_TL_A+INRCHR_TL_22_A+INRCHR_TL_24_A)*(1-null(TL_CHR_A-TL_CHR))*positif(TL_CHR))) * positif(CHRBASE_INR+NCHRBASE)* (1-IND_RJLJ);
CHR_PRI2=CHR_R9901;
CHR_ANT2=CHRBASE_INR_A;
CHR_NTL2=CHR_NTLDEC;
CHR_NTL_222=CHR_NTLDEC_22;
CHR_TL2=CHR_TLDEC;
CHR_TL_222=CHR_TLDEC_22;
CHR_REF_INR=CHR_REF;
INRPCAP_R99RA = INRPCAP_R99R_A;
INRPCAP_R99R = arr((PCAP_R99R) * (TXINR_PA/100)-INCPCAP_NET_A) * positif(PCAP_R99R-PCAP_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRPCAP_R9901A = INRPCAP_R9901_A;
INRPCAP_R9901 = arr(PCAP_R9901 * (TXINR_PA/100)-INCPCAP_NET_A) * positif(PCAP_R9901- PCAP_R9901_A)
              * positif(IND_PASSAGE-1) * positif(PCAP_TLDEC-PCAP_R9901) * positif(PCAP_R9901_A)
             + (arr(PCAP_R9901 * (TXINR_PA/100))-INCPCAP_NET_A) * positif(PCAP_R9901- PCAPBASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(PCAP_TLDEC-PCAP_R9901) * (1-positif(PCAP_R9901_A))
             + (INCPCAP_NET_A - arr(PCAP_R9901 * (TXINR_PA/100))) * positif(PCAPBASE_INR_A- PCAP_R9901)
              * positif(IND_PASSAGE-1) * positif(PCAP_TLDEC-PCAP_R9901) * (1-positif(PCAP_R9901_A)) * positif(PCAP_R9901)
	     ;
DO_INR_PCAPC=DO_INR_PCAP_A;
INR_NTL_GLOB_PCAP2 = INRPCAP_NTLDECD + INRPCAP_NTL_A+ INRPCAP_NTLDECD_22 + INRPCAP_NTL_22_A+INRPCAP_NTLDECD_24 + INRPCAP_NTL_24_A;
INR_TL_GLOB_PCAP2 = INRPCAP_TLDECD + INRPCAP_TL_A+ INRPCAP_TLDEC_22 + INRPCAP_TL_22_A+INRPCAP_TLDEC_24 + INRPCAP_TL_24_A;
INR_TOT_GLOB_PCAP2 = max(0,INR_NTL_GLOB_PCAP2 + INR_TL_GLOB_PCAP2*TL_CAP+INRPCAP_R99R+INRPCAP_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_PCAPC= (INRPCAP_NTLDECD+ INRPCAP_NTL_A+ (INRPCAP_TLDECD + INRPCAP_TL_A)*TL_CAP +INRPCAP_R99R+INRPCAP_R99R_A) * (1-IND_RJLJ) ;

DO_INR_PCAP2 = (1-null(PCAP_REF_A-min(PCAP_REF,max(PCAP_TLDEC_22,PCAP_TLDEC_24)))) * max(0,
           (arr(((INRPCAP_TL_A+INRPCAP_TL_22_A+INRPCAP_TL_24_A)*TL_CAP_A*TL_CAP+INRPCAP_NTL_A+INRPCAP_NTL_22_A+INRPCAP_NTL_24_A)
           * min(1,((PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24)/(PCAP_REF-max(0,PCAP_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24)* positif(PCAP_REF - (max(0,PCAP_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(PCAP_TLDEC_22 +PCAP_TLDEC_24- PCAPBASE_INR_A))
          +arr(((INRPCAP_TL_A+INRPCAP_TL_22_A+INRPCAP_TL_24_A)*TL_CAP_A*TL_CAP)
             * min(1,((PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24)/(PCAP_REF-max(0,PCAP_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24)* positif(PCAP_REF - (max(0,PCAP_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(PCAP_TLDEC_22 +PCAP_TLDEC_24- PCAPBASE_INR_A))
             * (1-positif(INRPCAP_NTL_A+INRPCAP_NTL_22_A+INRPCAP_NTL_24_A))
          + (INRPCAP_NTL_A*FLAG_C02+(INRPCAP_NTL_22_A+INRPCAP_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24)* positif(PCAP_REF - (max(0,PCAP_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRPCAP_NTL_A)*positif(INRPCAP_NTL_22_A+INRPCAP_NTL_24_A)
          + arr((INRPCAP_NTL_A*FLAG_C02+(INRPCAP_NTL_22_A+INRPCAP_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24)* positif(PCAP_REF - (max(0,PCAP_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24)/(PCAP_REF-max(0,PCAP_R9901)))))
             * (1-positif(INRPCAP_NTL_A)*positif(INRPCAP_NTL_22_A+INRPCAP_NTL_24_A))
          + ((INRPCAP_TL_A+INRPCAP_TL_22_A+INRPCAP_TL_24_A)*null(TL_CAP) * TL_CAP_A
          * (1- FLAG_DEFAUT)
             *positif(PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24)* positif(PCAP_REF - (max(0,PCAP_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
         (INRPCAP_TL_A + INRPCAP_R99R_A+INRPCAP_NTL_A - max(0,arr(PCAP_TLDEC * TXINR_PA/100))) * positif(PCAP_R99R - PCAP_TLDEC)
         + (INRPCAP_R99R_A+INRPCAP_NTL_A - max(0,arr(PCAP_R99R * TXINR_PA/100))) * positif(PCAP_TLDEC-(PCAP_R99R))
         + (INRPCAP_TL_A + INRPCAP_R99R_A+INRPCAP_NTL_A - max(0,arr(PCAP_R99R * TXINR_PA/100))) * null(PCAP_TLDEC-(PCAP_R99R)))
                       +(1-positif(SOM9YI)) * (
          arr((INRPCAP_TL_A*TL_CAP_A *TL_CAP+(INRPCAP_NTL_A +INRPCAP_R99R+INRPCAP_R9901-INRPCAP_RETDEF) 
                       * ((PCAP_REF - PCAP_TLDEC)/(PCAP_REF-max(0,PCAP_REF_A)))))
                       * positif(PCAP_REF - PCAP_TLDEC)  * positif(PCAP_TLDEC - PCAP_R99R) 
                        * positif(INRPCAP_R99R_A+INRPCAP_R9901_A+0)
         + (INR_TOT_GLOB_PCAPC - DO_INR_PCAP_A - arr(PCAP_TLDEC * TXINR_PA/100))
                       * positif(PCAP_REF - PCAP_TLDEC)
                        )
		       ) * (1-FLAG_RETARD22)
                    +             (positif(SOM9YI)* (
              (INRPCAP_TL_22_A + INRPCAP_R99R_A+INRPCAP_NTL_22_A - max(0,arr(PCAP_TLDEC_22 * TXINR_PA22/100))) * positif(PCAP_R9901 - max(PCAP_TLDEC_24,PCAP_TLDEC_22))
               + (INRPCAP_R99R_A+INRPCAP_NTL_22_A - max(0,arr(PCAP_R99R * TXINR_PA22/100))) * positif( max(PCAP_TLDEC_24,PCAP_TLDEC_22)-(PCAP_R9901))
               + (INRPCAP_TL_22_A + INRPCAP_R99R_A+INRPCAP_NTL_22_A - max(0,arr(PCAP_R99R * TXINR_PA22/100))) * null( max(PCAP_TLDEC_24,PCAP_TLDEC_22)-(PCAP_R9901)))
                  +(1-positif(SOM9YI)) * (
                 arr((INRPCAP_TL_22_A*TL_CAP_A *TL_CAP+(INRPCAP_NTL_22_A +INRPCAP_R99R+INRPCAP_R9901-INRPCAP_RETDEF)
                       * ((PCAP_REF - PCAP_TLDEC_22)/(PCAP_REF-max(0,PCAP_REF_A)))))
                       * positif(PCAP_REF -  max(PCAP_TLDEC_24,PCAP_TLDEC_22))  * positif( max(PCAP_TLDEC_24,PCAP_TLDEC_22) - PCAP_R9901)
                           * positif(INRPCAP_R99R_A+INRPCAP_R9901_A+0)
            + (INRPCAP_TL_22_A+INRPCAP_NTL_22_A - DO_INR_PCAP_A - arr(PCAP_TLDEC_22 * TXINR_PA22/100))
                         * positif(PCAP_REF -  max(PCAP_TLDEC_24,PCAP_TLDEC_22))
            + (INRPCAP_R99R_A + INRPCAP_NTL_22_A- arr(PCAP_R9901 * TXINR_PA22/100)) * null( max(PCAP_TLDEC_24,PCAP_TLDEC_22) - PCAP_R9901)
                           * positif(PCAP_REF -  max(PCAP_TLDEC_24,PCAP_TLDEC_22)))) * FLAG_RETARD22
               ));
RECUP_INR_PCAP = max(0,(min(max(0,DO_INR_PCAP_A-RECUP_INR_PCAP_A),arr(max(0,DO_INR_PCAP_A-RECUP_INR_PCAP_A) * (PCAP_TLDEC - PCAPBASE_INR_A)/DO_PCAP_A))
                    *positif(PCAP_TLDEC-PCAPBASE_INR_A)*positif(PCAP_REF-PCAPBASE_INR_A)
                    * positif(PCAP_PA - PCAP_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_PCAP_A-RECUP_INR_PCAP_A),arr((PCAP_R99R - PCAPBASE_INR_A) * TXINR_PA/100))*positif(PCAP_TLDEC - PCAPBASE_INR_A)
                    * (1-positif(PCAP_PA - PCAP_TLDEC))
                    *positif(max(0,DO_INR_PCAP_A-RECUP_INR_PCAP_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_PCAP2 = (PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24) * positif(PCAP_REF - PCAP_TLDEC_22-PCAP_TLDEC_24)* positif(PCAPBASE_INR_A);
SUP_PCAP_MAX2 = (PCAP_REF - max(0,PCAP_R9901)) * positif(PCAP_REF - max(0,PCAP_R9901))* positif(PCAPBASE_INR_A);
DO_INR_PCAP982 = max(0,
          arr((PCAP_REF - PCAP_NTLDEC_198) * TXINRRED_A/100) 
            *positif(PCAP_REF - PCAP_NTLDEC_198))*(1-positif(DO_INR_PCAP2)) * present(PCAP_NTLDEC_198);
DO_INR_PCAP992 = max(0,
          arr((PCAP_REF - PCAP_TLDEC_199) * TXINRRED_A/100)
            *positif(PCAP_REF - PCAP_TLDEC_199))*(1-positif(DO_INR_PCAP2)) * present(PCAP_TLDEC_199);
INR_PCAP_TOT = max(INRPCAP_NTLDECD+INRPCAP_NTLDECD_22 +INRPCAP_NTLDECD_24+ (INRPCAP_TLDECD+INRPCAP_TLDEC_22+INRPCAP_TLDEC_24)*TL_CAP-INR_PCAP_TARDIF*null(1-IND_PASSAGE)+INRPCAP_R99R+RECUP_INR_PCAP,0) 
	      * (1-IND_RJLJ);
INRPCAP_RECT= arr((PCAP_RECT)* (TXINR_PA/100)) * positif(PCAP_RECT) * FLAG_RECTIF;
INCPCAP_TL2 = INRPCAP_TLDECD;
INCPCAP_TL_222 = INRPCAP_TLDEC_22;
INPCAP_TL2 = INRPCAP_TLA * TL_CAP;
INPCAP_TL_222 = INRPCAP_TLA_22 * TL_CAP;
INRPCAP_NET2 = max(INRPCAP_NTLDECD+INRPCAP_TLDECD*TL_CAP+INRPCAP_R99R+RECUP_INR_PCAP,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_PCAP2 * (-1);
INRPCAP_NET_222 = max(INRPCAP_NTLDECD_22+INRPCAP_NTLDECD_24+(INRPCAP_TLDEC_22+INRPCAP_TLDEC_24)*TL_CAP,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_PCAP982 + DO_INR_PCAP992)*(-1);
INCPCAP_NET2 = max(0,(INRPCAP_NET2 + INRPCAP_NET_222 + INCPCAP_NET_A+(INRPCAP_TL_A+INRPCAP_TL_22_A+INRPCAP_TL_24_A)*(1-null(TL_CAP_A-TL_CAP))*positif(TL_CAP))) 
                                                        * positif(PCAPBASE_INR+NPCAPBASE)* (1-IND_RJLJ);
PCAP_PRI2=PCAP_R9901;
PCAP_ANT2=PCAPBASE_INR_A;
PCAP_NTL2=PCAP_NTLDEC;
PCAP_NTL_222=PCAP_NTLDEC_22;
PCAP_TL2=PCAP_TLDEC;
PCAP_TL_222=PCAP_TLDEC_22;
PCAP_REF_INR=PCAP_REF;
INRRSE1_R99RA = INRRSE1_R99R_A;
INRRSE1_R99R = arr((RSE1_R99R-CSPROVYD) * (TXINR_PA/100)-INCRSE1_NET_A) * positif(RSE1_R99R-RSE1_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRRSE1_R9901A = INRRSE1_R9901_A;
INRRSE1_R9901 = arr(RSE1_R9901 * (TXINR_PA/100)-INCRSE1_NET_A) * positif(RSE1_R9901- RSE1_R9901_A)
              * positif(IND_PASSAGE-1) * positif(RSE1_TLDEC-RSE1_R9901) * positif(RSE1_R9901_A)
             + (arr(RSE1_R9901 * (TXINR_PA/100))-INCRSE1_NET_A) * positif(RSE1_R9901- RSE1BASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(RSE1_TLDEC-RSE1_R9901) * (1-positif(RSE1_R9901_A))
             + (INCRSE1_NET_A - arr(RSE1_R9901 * (TXINR_PA/100))) * positif(RSE1BASE_INR_A- RSE1_R9901)
              * positif(IND_PASSAGE-1) * positif(RSE1_TLDEC-RSE1_R9901) * (1-positif(RSE1_R9901_A)) * positif(RSE1_R9901)
	     ;
DO_INR_RSE1C=DO_INR_RSE1_A;
INR_NTL_GLOB_RSE12 = INRRSE1_NTLDECD + INRRSE1_NTL_A+ INRRSE1_NTLDECD_22 + INRRSE1_NTL_22_A+INRRSE1_NTLDECD_24 + INRRSE1_NTL_24_A;
INR_TL_GLOB_RSE12 = INRRSE1_TLDECD + INRRSE1_TL_A+ INRRSE1_TLDEC_22 + INRRSE1_TL_22_A+INRRSE1_TLDEC_24 + INRRSE1_TL_24_A;
INR_TOT_GLOB_RSE12 = max(0,INR_NTL_GLOB_RSE12 + INR_TL_GLOB_RSE12*TL_RSE1+INRRSE1_R99R+INRRSE1_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_RSE1C= (INRRSE1_NTLDECD+ INRRSE1_NTL_A+ (INRRSE1_TLDECD + INRRSE1_TL_A)*TL_RSE1 +INRRSE1_R99R+INRRSE1_R99R_A) * (1-IND_RJLJ) ;
DO_INR_RSE12 = (1-null(RSE1_REF_A-min(RSE1_REF,max(RSE1_TLDEC_22,RSE1_TLDEC_24)))) * max(0,
           (arr(((INRRSE1_TL_A+INRRSE1_TL_22_A+INRRSE1_TL_24_A)*TL_RSE1_A*TL_RSE1 + INRRSE1_NTL_A+INRRSE1_NTL_22_A+INRRSE1_NTL_24_A)
           * min(1,((RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24)/(RSE1_REF-max(0,RSE1_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24)* positif(RSE1_REF - (max(0,RSE1_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(RSE1_TLDEC_22 +RSE1_TLDEC_24- RSE1BASE_INR_A))
           +arr(((INRRSE1_TL_A+INRRSE1_TL_22_A+INRRSE1_TL_24_A)*TL_RSE1_A*TL_RSE1)
             * min(1,((RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24)/(RSE1_REF-max(0,RSE1_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24)* positif(RSE1_REF - (max(0,RSE1_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(RSE1_TLDEC_22 +RSE1_TLDEC_24- RSE1BASE_INR_A))
             * (1-positif(INRRSE1_NTL_A+INRRSE1_NTL_22_A+INRRSE1_NTL_24_A))
          + (INRRSE1_NTL_A*FLAG_C02+(INRRSE1_NTL_22_A+INRRSE1_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24)* positif(RSE1_REF - (max(0,RSE1_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRRSE1_NTL_A)*positif(INRRSE1_NTL_22_A+INRRSE1_NTL_24_A)
          + arr((INRRSE1_NTL_A*FLAG_C02+(INRRSE1_NTL_22_A+INRRSE1_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24)* positif(RSE1_REF - (max(0,RSE1_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24)/(RSE1_REF-max(0,RSE1_R9901)))))
             * (1-positif(INRRSE1_NTL_A)*positif(INRRSE1_NTL_22_A+INRRSE1_NTL_24_A))
          + ((INRRSE1_TL_A+INRRSE1_TL_22_A+INRRSE1_TL_24_A)*null(TL_RSE1) * TL_RSE1_A
          * (1- FLAG_DEFAUT)
             *positif(RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24)* positif(RSE1_REF - (max(0,RSE1_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRRSE1_TL_A + INRRSE1_R99R_A+INRRSE1_NTL_A - max(0,arr(RSE1_TLDEC * TXINR_PA/100))) * positif(RSE1_R99R - RSE1_TLDEC)
         + (INRRSE1_R99R_A+INRRSE1_NTL_A - max(0,arr(RSE1_R99R * TXINR_PA/100))) * positif(RSE1_TLDEC-(RSE1_R99R))
         + (INRRSE1_TL_A + INRRSE1_R99R_A+INRRSE1_NTL_A - max(0,arr(RSE1_R99R * TXINR_PA/100))) * null(RSE1_TLDEC-(RSE1_R99R)))
                      + (1-positif(SOM9YI)) * ( 
          arr((INRRSE1_TL_A*TL_RSE1_A *TL_RSE1+(INRRSE1_NTL_A +INRRSE1_R99R+INRRSE1_R9901-INRRSE1_RETDEF) 
                       * ((RSE1_REF - RSE1_TLDEC)/(RSE1_REF-max(0,RSE1_REF_A)))))
                       * positif(RSE1_REF - RSE1_TLDEC)  * positif(RSE1_TLDEC - RSE1_R99R) 
                       * positif(INRRSE1_R99R_A+INRRSE1_R9901_A+0)
         + (INR_TOT_GLOB_RSE1C - DO_INR_RSE1_A - arr(RSE1_TLDEC * TXINR_PA/100))
                       * positif(RSE1_REF - RSE1_TLDEC)
		       )
                       ) * (1-positif(FLAG_RETARD22))
                     +                  (positif(SOM9YI)* (
                (INRRSE1_TL_22_A + INRRSE1_R99R_A+INRRSE1_NTL_22_A - max(0,arr(RSE1_TLDEC_22 * TXINR_PA22/100))) * positif(RSE1_R9901 - max(RSE1_TLDEC_24,RSE1_TLDEC_22))
         + (INRRSE1_R99R_A+INRRSE1_NTL_22_A - max(0,arr(RSE1_R99R * TXINR_PA22/100))) * positif(max(RSE1_TLDEC_24,RSE1_TLDEC_22)-(RSE1_R9901))
         + (INRRSE1_TL_22_A + INRRSE1_R99R_A+INRRSE1_NTL_22_A - max(0,arr(RSE1_R99R * TXINR_PA22/100))) * null(max(RSE1_TLDEC_24,RSE1_TLDEC_22)-(RSE1_R9901)))
                   + (1-positif(SOM9YI)) * (
          arr((INRRSE1_TL_22_A*TL_RSE1_A *TL_RSE1+(INRRSE1_NTL_22_A +INRRSE1_R99R+INRRSE1_R9901-INRRSE1_RETDEF)
                        * ((RSE1_REF - RSE1_TLDEC_22)/(RSE1_REF-max(0,RSE1_REF_A)))))
                        * positif(RSE1_REF - max(RSE1_TLDEC_24,RSE1_TLDEC_22))  * positif(max(RSE1_TLDEC_24,RSE1_TLDEC_22) - RSE1_R9901)
                       * positif(INRRSE1_R99R_A+INRRSE1_R9901_A+0)
              + (INRRSE1_TL_22_A+INRRSE1_NTL_22_A - DO_INR_RSE1_A - arr(RSE1_TLDEC_22 * TXINR_PA22/100))
                            * positif(RSE1_REF - max(RSE1_TLDEC_24,RSE1_TLDEC_22))
			)
                          ) * positif(FLAG_RETARD22)
               ));

RECUP_INR_RSE1 = max(0,(min(max(0,DO_INR_RSE1_A-RECUP_INR_RSE1_A),arr(max(0,DO_INR_RSE1_A-RECUP_INR_RSE1_A) * (RSE1_TLDEC - RSE1BASE_INR_A)/DO_RSE1_A))
                    *positif(RSE1_TLDEC-RSE1BASE_INR_A)*positif(RSE1_REF-RSE1BASE_INR_A)
                    * positif(RSE1_PA - RSE1_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_RSE1_A-RECUP_INR_RSE1_A),arr((RSE1_R99R - RSE1BASE_INR_A) * TXINR_PA/100))*positif(RSE1_TLDEC - RSE1BASE_INR_A)
                    * (1-positif(RSE1_PA - RSE1_TLDEC))
                    *positif(max(0,DO_INR_RSE1-RECUP_INR_RSE1_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_RSE12 = (RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24) * positif(RSE1_REF - RSE1_TLDEC_22-RSE1_TLDEC_24)* positif(RSE1BASE_INR_A);
SUP_RSE1_MAX2 = (RSE1_REF - max(0,RSE1_R9901)) * positif(RSE1_REF - max(0,RSE1_R9901))* positif(RSE1BASE_INR_A);
DO_INR_RSE1982 = max(0,
          arr((RSE1_REF - RSE1_NTLDEC_198) * TXINRRED_A/100) 
            *positif(RSE1_REF - RSE1_NTLDEC_198))*(1-positif(DO_INR_RSE12)) * present(RSE1_NTLDEC_198);
DO_INR_RSE1992 = max(0,
          arr((RSE1_REF - RSE1_TLDEC_199) * TXINRRED_A/100)
            *positif(RSE1_REF - RSE1_TLDEC_199))*(1-positif(DO_INR_RSE12)) * present(RSE1_TLDEC_199);
INR_RSE1_TOT = max(INRRSE1_NTLDECD+INRRSE1_NTLDECD_22+INRRSE1_NTLDECD_24 + (INRRSE1_TLDECD+INRRSE1_TLDEC_22+INRRSE1_TLDEC_24)*TL_RSE1-INR_RSE1_TARDIF*null(1-IND_PASSAGE)+INRRSE1_R99R+RECUP_INR_RSE1,0) 
	      * (1-IND_RJLJ);
INRRSE1_RECT= arr((RSE1_RECT-CSPROVYD)* (TXINR_PA/100)) * positif(RSE1_RECT) * FLAG_RECTIF;
INCRSE1_TL2 = INRRSE1_TLDECD;
INCRSE1_TL_222 = INRRSE1_TLDEC_22;
INRSE1_TL2 = INRRSE1_TLA * TL_RSE1;
INRSE1_TL_222 = INRRSE1_TLA_22 * TL_RSE1;
INRRSE1_NET2 = max(INRRSE1_NTLDECD+INRRSE1_TLDECD*TL_RSE1+INRRSE1_R99R+RECUP_INR_RSE1,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_RSE12 * (-1);
INRRSE1_NET_222 = max(INRRSE1_NTLDECD_22+INRRSE1_NTLDECD_24+(INRRSE1_TLDEC_22+INRRSE1_TLDEC_24)*TL_RSE1,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_RSE1982 + DO_INR_RSE1992)*(-1);
INCRSE1_NET2 = max(0,(INRRSE1_NET2 + INRRSE1_NET_222 + INCRSE1_NET_A+(INRRSE1_TL_A+INRRSE1_TL_22_A+INRRSE1_TL_24_A)*(1-null(TL_RSE1_A-TL_RSE1))*positif(TL_RSE1))) * positif(RSE1BASE_INR+NRSE1BASE)* (1-IND_RJLJ);
RSE1_PRI2=RSE1_R9901;
RSE1_ANT2=RSE1BASE_INR_A;
RSE1_NTL2=RSE1_NTLDEC;
RSE1_NTL_222=RSE1_NTLDEC_22;
RSE1_TL2=RSE1_TLDEC;
RSE1_TL_222=RSE1_TLDEC_22;
RSE1_REF_INR=RSE1_REF;
INRRSE2_R99RA = INRRSE2_R99R_A;
INRRSE2_R99R = arr((RSE2_R99R-CSPROVYF-CSPROVYN) * (TXINR_PA/100)-INCRSE2_NET_A) * positif(RSE2_R99R-RSE2_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRRSE2_R9901A = INRRSE2_R9901_A;
INRRSE2_R9901 = arr(RSE2_R9901 * (TXINR_PA/100)-INCRSE2_NET_A) * positif(RSE2_R9901- RSE2_R9901_A)
              * positif(IND_PASSAGE-1) * positif(RSE2_TLDEC-RSE2_R9901) * positif(RSE2_R9901_A)
             + (arr(RSE2_R9901 * (TXINR_PA/100))-INCRSE2_NET_A) * positif(RSE2_R9901- RSE2BASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(RSE2_TLDEC-RSE2_R9901) * (1-positif(RSE2_R9901_A))
             + (INCRSE2_NET_A - arr(RSE2_R9901 * (TXINR_PA/100))) * positif(RSE2BASE_INR_A- RSE2_R9901)
              * positif(IND_PASSAGE-1) * positif(RSE2_TLDEC-RSE2_R9901) * (1-positif(RSE2_R9901_A)) * positif(RSE2_R9901)
	     ;
DO_INR_RSE2C=DO_INR_RSE2_A;
INR_NTL_GLOB_RSE22 = INRRSE2_NTLDECD + INRRSE2_NTL_A+ INRRSE2_NTLDECD_22 + INRRSE2_NTL_22_A+INRRSE2_NTLDECD_24 + INRRSE2_NTL_24_A;
INR_TL_GLOB_RSE22 = INRRSE2_TLDECD + INRRSE2_TL_A+ INRRSE2_TLDEC_22 + INRRSE2_TL_22_A+INRRSE2_TLDEC_24 + INRRSE2_TL_24_A;
INR_TOT_GLOB_RSE22 = max(0,INR_NTL_GLOB_RSE22 + INR_TL_GLOB_RSE22*TL_RSE2+INRRSE2_R99R+INRRSE2_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_RSE2C= (INRRSE2_NTLDECD+ INRRSE2_NTL_A+ (INRRSE2_TLDECD + INRRSE2_TL_A)*TL_RSE2 +INRRSE2_R99R+INRRSE2_R99R_A) * (1-IND_RJLJ) ;
DO_INR_RSE22 = (1-null(RSE2_REF_A-min(RSE2_REF,max(RSE2_TLDEC_22,RSE2_TLDEC_24)))) * max(0,
           (arr(((INRRSE2_TL_A+INRRSE2_TL_22_A+INRRSE2_TL_24_A)*TL_RSE2_A*TL_RSE2 + INRRSE2_NTL_A+INRRSE2_NTL_22_A+INRRSE2_NTL_24_A)
           * min(1,((RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24)/(RSE2_REF-max(0,RSE2_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24)* positif(RSE2_REF - (max(0,RSE2_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(RSE2_TLDEC_22 +RSE2_TLDEC_24- RSE2BASE_INR_A))
           +arr(((INRRSE2_TL_A+INRRSE2_TL_22_A+INRRSE2_TL_24_A)*TL_RSE2_A*TL_RSE2)
             * min(1,((RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24)/(RSE2_REF-max(0,RSE2_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24)* positif(RSE2_REF - (max(0,RSE2_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(RSE2_TLDEC_22 +RSE2_TLDEC_24- RSE2BASE_INR_A))
             * (1-positif(INRRSE2_NTL_A+INRRSE2_NTL_22_A+INRRSE2_NTL_24_A))
          + (INRRSE2_NTL_A*FLAG_C02+(INRRSE2_NTL_22_A+INRRSE2_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24)* positif(RSE2_REF - (max(0,RSE2_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRRSE2_NTL_A)*positif(INRRSE2_NTL_22_A+INRRSE2_NTL_24_A)
          + arr((INRRSE2_NTL_A*FLAG_C02+(INRRSE2_NTL_22_A+INRRSE2_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24)* positif(RSE2_REF - (max(0,RSE2_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24)/(RSE2_REF-max(0,RSE2_R9901)))))
             * (1-positif(INRRSE2_NTL_A)*positif(INRRSE2_NTL_22_A+INRRSE2_NTL_24_A))
          + ((INRRSE2_TL_A+INRRSE2_TL_22_A+INRRSE2_TL_24_A)*null(TL_RSE2) * TL_RSE2_A
          * (1- FLAG_DEFAUT)
             *positif(RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24)* positif(RSE2_REF - (max(0,RSE2_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRRSE2_TL_A + INRRSE2_R99R_A+INRRSE2_NTL_A - max(0,arr(RSE2_TLDEC * TXINR_PA/100))) * positif(RSE2_R99R - RSE2_TLDEC)
         + (INRRSE2_R99R_A+INRRSE2_NTL_A - max(0,arr(RSE2_R99R * TXINR_PA/100))) * positif(RSE2_TLDEC-(RSE2_R99R))
         + (INRRSE2_TL_A + INRRSE2_R99R_A+INRRSE2_NTL_A - max(0,arr(RSE2_R99R * TXINR_PA/100))) * null(RSE2_TLDEC-(RSE2_R99R)))
                      + (1-positif(SOM9YI)) * ( 
          arr((INRRSE2_TL_A*TL_RSE2_A *TL_RSE2+(INRRSE2_NTL_A +INRRSE2_R99R+INRRSE2_R9901-INRRSE2_RETDEF) 
                       * ((RSE2_REF - RSE2_TLDEC)/(RSE2_REF-max(0,RSE2_REF_A)))))
                       * positif(RSE2_REF - RSE2_TLDEC)  * positif(RSE2_TLDEC - RSE2_R99R) 
                       * positif(INRRSE2_R99R_A+INRRSE2_R9901_A+0)
         + (INR_TOT_GLOB_RSE2C - DO_INR_RSE2_A - arr(RSE2_TLDEC * TXINR_PA/100))
                       * positif(RSE2_REF - RSE2_TLDEC)
		       )
                       ) * (1-positif(FLAG_RETARD22))
               +                  (positif(SOM9YI)* (
                (INRRSE2_TL_22_A + INRRSE2_R99R_A+INRRSE2_NTL_22_A - max(0,arr(RSE2_TLDEC_22 * TXINR_PA22/100))) * positif(RSE2_R9901 - max(RSE2_TLDEC_24,RSE2_TLDEC_22))
         + (INRRSE2_R99R_A+INRRSE2_NTL_22_A - max(0,arr(RSE2_R99R * TXINR_PA22/100))) * positif(max(RSE2_TLDEC_24,RSE2_TLDEC_22)-(RSE2_R9901))
           + (INRRSE2_TL_22_A + INRRSE2_R99R_A+INRRSE2_NTL_22_A - max(0,arr(RSE2_R99R * TXINR_PA22/100))) * null(max(RSE2_TLDEC_24,RSE2_TLDEC_22)-(RSE2_R9901)))
                         + (1-positif(SOM9YI)) * (
           arr((INRRSE2_TL_22_A*TL_RSE2_A *TL_RSE2+(INRRSE2_NTL_22_A +INRRSE2_R99R+INRRSE2_R9901-INRRSE2_RETDEF)
                          * ((RSE2_REF - RSE2_TLDEC_22)/(RSE2_REF-max(0,RSE2_REF_A)))))
                         * positif(RSE2_REF - max(RSE2_TLDEC_24,RSE2_TLDEC_22))  * positif(max(RSE2_TLDEC_24,RSE2_TLDEC_22) - RSE2_R9901)
                        * positif(INRRSE2_R99R_A+INRRSE2_R9901_A+0)
               + (INRRSE2_TL_22_A+INRRSE2_NTL_22_A - DO_INR_RSE2_A - arr(RSE2_TLDEC_22 * TXINR_PA22/100))
                             * positif(RSE2_REF - max(RSE2_TLDEC_24,RSE2_TLDEC_22))
			     )
                           ) * positif(FLAG_RETARD22)
                ));

RECUP_INR_RSE2 = max(0,(min(max(0,DO_INR_RSE2_A-RECUP_INR_RSE2_A),arr(max(0,DO_INR_RSE2_A-RECUP_INR_RSE2_A) * (RSE2_TLDEC - RSE2BASE_INR_A)/DO_RSE2_A))
                    *positif(RSE2_TLDEC-RSE2BASE_INR_A)*positif(RSE2_REF-RSE2BASE_INR_A)
                    * positif(RSE2_PA - RSE2_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_RSE2_A-RECUP_INR_RSE2_A),arr((RSE2_R99R - RSE2BASE_INR_A) * TXINR_PA/100))*positif(RSE2_TLDEC - RSE2BASE_INR_A)
                    * (1-positif(RSE2_PA - RSE2_TLDEC))
                    *positif(max(0,DO_INR_RSE2_A-RECUP_INR_RSE2_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_RSE22 = (RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24) * positif(RSE2_REF - RSE2_TLDEC_22-RSE2_TLDEC_24)* positif(RSE2BASE_INR_A);
SUP_RSE2_MAX2 = (RSE2_REF - max(0,RSE2_R9901)) * positif(RSE2_REF - max(0,RSE2_R9901))* positif(RSE2BASE_INR_A);
DO_INR_RSE2982 = max(0,
          arr((RSE2_REF - RSE2_NTLDEC_198) * TXINRRED_A/100) 
            *positif(RSE2_REF - RSE2_NTLDEC_198))*(1-positif(DO_INR_RSE22)) * present(RSE2_NTLDEC_198);
DO_INR_RSE2992 = max(0,
          arr((RSE2_REF - RSE2_TLDEC_199) * TXINRRED_A/100)
            *positif(RSE2_REF - RSE2_TLDEC_199))*(1-positif(DO_INR_RSE22)) * present(RSE2_TLDEC_199);
INR_RSE2_TOT = max(INRRSE2_NTLDECD+INRRSE2_NTLDECD_22 +INRRSE2_NTLDECD_24+ (INRRSE2_TLDECD+INRRSE2_TLDEC_22+INRRSE2_TLDEC_24)*TL_RSE2-INR_RSE2_TARDIF*null(1-IND_PASSAGE)+INRRSE2_R99R+RECUP_INR_RSE2,0) 
	      * (1-IND_RJLJ);
INRRSE2_RECT= arr((RSE2_RECT-CSPROVYF-CSPROVYN)* (TXINR_PA/100)) * positif(RSE2_RECT) * FLAG_RECTIF;
INCRSE2_TL2 = INRRSE2_TLDECD;
INCRSE2_TL_222 = INRRSE2_TLDEC_22;
INRSE2_TL2 = INRRSE2_TLA * TL_RSE2;
INRSE2_TL_222 = INRRSE2_TLA_22 * TL_RSE2;
INRRSE2_NET2 = max(INRRSE2_NTLDECD+INRRSE2_TLDECD*TL_RSE2+INRRSE2_R99R+RECUP_INR_RSE2,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_RSE22 * (-1);
INRRSE2_NET_222 = max(INRRSE2_NTLDECD_22+INRRSE2_NTLDECD_24+(INRRSE2_TLDEC_22+INRRSE2_TLDEC_24)*TL_RSE2,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_RSE2982 + DO_INR_RSE2992)*(-1);
INCRSE2_NET2 = max(0,(INRRSE2_NET2 + INRRSE2_NET_222 + INCRSE2_NET_A+(INRRSE2_TL_A+INRRSE2_TL_22_A+INRRSE2_TL_24_A)*(1-null(TL_RSE2_A-TL_RSE2))*positif(TL_RSE2))) * positif(RSE2BASE_INR+NRSE2BASE)* (1-IND_RJLJ);
RSE2_PRI2=RSE2_R9901;
RSE2_ANT2=RSE2BASE_INR_A;
RSE2_NTL2=RSE2_NTLDEC;
RSE2_NTL_222=RSE2_NTLDEC_22;
RSE2_TL2=RSE2_TLDEC;
RSE2_TL_222=RSE2_TLDEC_22;
RSE2_REF_INR=RSE2_REF;
INRRSE3_R99RA = INRRSE3_R99R_A;
INRRSE3_R99R = arr((RSE3_R99R-CSPROVYG) * (TXINR_PA/100)-INCRSE3_NET_A) * positif(RSE3_R99R-RSE3_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRRSE3_R9901A = INRRSE3_R9901_A;
INRRSE3_R9901 = arr(RSE3_R9901 * (TXINR_PA/100)-INCRSE3_NET_A) * positif(RSE3_R9901- RSE3_R9901_A)
              * positif(IND_PASSAGE-1) * positif(RSE3_TLDEC-RSE3_R9901) * positif(RSE3_R9901_A)
             + (arr(RSE3_R9901 * (TXINR_PA/100))-INCRSE3_NET_A) * positif(RSE3_R9901- RSE3BASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(RSE3_TLDEC-RSE3_R9901) * (1-positif(RSE3_R9901_A))
             + (INCRSE3_NET_A - arr(RSE3_R9901 * (TXINR_PA/100))) * positif(RSE3BASE_INR_A- RSE3_R9901)
              * positif(IND_PASSAGE-1) * positif(RSE3_TLDEC-RSE3_R9901) * (1-positif(RSE3_R9901_A)) * positif(RSE3_R9901)
	     ;
DO_INR_RSE3C=DO_INR_RSE3_A;
INR_NTL_GLOB_RSE32 = INRRSE3_NTLDECD + INRRSE3_NTL_A+ INRRSE3_NTLDECD_22 + INRRSE3_NTL_22_A+INRRSE3_NTLDECD_24 + INRRSE3_NTL_24_A;
INR_TL_GLOB_RSE32 = INRRSE3_TLDECD + INRRSE3_TL_A+ INRRSE3_TLDEC_22 + INRRSE3_TL_22_A+INRRSE3_TLDEC_24 + INRRSE3_TL_24_A;
INR_TOT_GLOB_RSE32 = max(0,INR_NTL_GLOB_RSE32 + INR_TL_GLOB_RSE32*TL_RSE3+INRRSE3_R99R+INRRSE3_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_RSE3C= (INRRSE3_NTLDECD+ INRRSE3_NTL_A+ (INRRSE3_TLDECD + INRRSE3_TL_A)*TL_RSE3 +INRRSE3_R99R+INRRSE3_R99R_A) * (1-IND_RJLJ) ;

DO_INR_RSE32 = (1-null(RSE3_REF_A-min(RSE3_REF,max(RSE3_TLDEC_22,RSE3_TLDEC_24)))) * max(0,
           (arr(((INRRSE3_TL_A+INRRSE3_TL_22_A+INRRSE3_TL_24_A)*TL_RSE3_A*TL_RSE3 + INRRSE3_NTL_A+INRRSE3_NTL_22_A+INRRSE3_NTL_24_A)
           * min(1,((RSE3_REF - RSE3_TLDEC_22-RSE3_TLDEC_24)/(RSE3_REF-max(0,RSE3_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE3_REF - RSE3_TLDEC_22-RSE3_TLDEC_24)* positif(RSE3_REF - (max(0,RSE3_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(RSE3_TLDEC_22 - RSE3BASE_INR_A))
           +arr(((INRRSE3_TL_A+INRRSE3_TL_22_A+INRRSE3_TL_24_A)*TL_RSE3_A*TL_RSE3)
             * min(1,((RSE3_REF - RSE3_TLDEC_22-RSE3_TLDEC_24)/(RSE3_REF-max(0,RSE3_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE3_REF - RSE3_TLDEC_22-RSE3_TLDEC_24)* positif(RSE3_REF - (max(0,RSE3_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(RSE3_TLDEC_22 - RSE3BASE_INR_A))
             * (1-positif(INRRSE3_NTL_A+INRRSE3_NTL_22_A+INRRSE3_NTL_24_A))
          + (INRRSE3_NTL_A*FLAG_C02+(INRRSE3_NTL_22_A+INRRSE3_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE3_REF - RSE3_TLDEC_22-RSE3_TLDEC_24)* positif(RSE3_REF - (max(0,RSE3_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRRSE3_NTL_A)*positif(INRRSE3_NTL_22_A)
          + arr((INRRSE3_NTL_A*FLAG_C02+(INRRSE3_NTL_22_A+INRRSE3_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE3_REF - RSE3_TLDEC_22-RSE3_TLDEC_24)* positif(RSE3_REF - (max(0,RSE3_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((RSE3_REF - RSE3_TLDEC_22-RSE3_TLDEC_24)/(RSE3_REF-max(0,RSE3_R9901)))))
             * (1-positif(INRRSE3_NTL_A)*positif(INRRSE3_NTL_22_A+INRRSE3_NTL_24_A))
          + ((INRRSE3_TL_A+INRRSE3_TL_22_A+INRRSE3_TL_24_A)*null(TL_RSE3) * TL_RSE3_A
          * (1- FLAG_DEFAUT)
             *positif(RSE3_REF - RSE3_TLDEC_22-RSE3_TLDEC_24)* positif(RSE3_REF - (max(0,RSE3_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRRSE3_TL_A + INRRSE3_R99R_A+INRRSE3_NTL_A - max(0,arr(RSE3_TLDEC * TXINR_PA/100))) * positif(RSE3_R99R - RSE3_TLDEC)
         + (INRRSE3_R99R_A+INRRSE3_NTL_A - max(0,arr(RSE3_R99R * TXINR_PA/100))) * positif(RSE3_TLDEC-(RSE3_R99R))
         + (INRRSE3_TL_A + INRRSE3_R99R_A+INRRSE3_NTL_A - max(0,arr(RSE3_R99R * TXINR_PA/100))) * null(RSE3_TLDEC-(RSE3_R99R)))
                      + (1-positif(SOM9YI)) * ( 
          arr((INRRSE3_TL_A*TL_RSE3_A *TL_RSE3+(INRRSE3_NTL_A +INRRSE3_R99R+INRRSE3_R9901-INRRSE3_RETDEF) 
                       * ((RSE3_REF - RSE3_TLDEC)/(RSE3_REF-max(0,RSE3_REF_A)))))
                       * positif(RSE3_REF - RSE3_TLDEC)  * positif(RSE3_TLDEC - RSE3_R99R) 
                       * positif(INRRSE3_R99R_A+INRRSE3_R9901_A+0)
         + (INR_TOT_GLOB_RSE3C - DO_INR_RSE3_A - arr(RSE3_TLDEC * TXINR_PA/100))
                       * positif(RSE3_REF - RSE3_TLDEC)
		       )
                       ) * (1-positif(FLAG_RETARD22))
               +                  (positif(SOM9YI)* (
                 (INRRSE3_TL_22_A + INRRSE3_R99R_A+INRRSE3_NTL_22_A - max(0,arr(RSE3_TLDEC_22 * TXINR_PA22/100))) * positif(RSE3_R9901 - max(RSE3_TLDEC_24,RSE3_TLDEC_22))
          + (INRRSE3_R99R_A+INRRSE3_NTL_22_A - max(0,arr(RSE3_R99R * TXINR_PA22/100))) * positif(max(RSE3_TLDEC_24,RSE3_TLDEC_22)-(RSE3_R9901))
          + (INRRSE3_TL_22_A + INRRSE3_R99R_A+INRRSE3_NTL_22_A - max(0,arr(RSE3_R99R * TXINR_PA22/100))) * null(max(RSE3_TLDEC_24,RSE3_TLDEC_22)-(RSE3_R9901)))
                         + (1-positif(SOM9YI)) * (
           arr((INRRSE3_TL_22_A*TL_RSE3_A *TL_RSE3+(INRRSE3_NTL_22_A +INRRSE3_R99R+INRRSE3_R9901-INRRSE3_RETDEF)
                         * ((RSE3_REF - RSE3_TLDEC_22)/(RSE3_REF-max(0,RSE3_REF_A)))))
                        * positif(RSE3_REF - max(RSE3_TLDEC_24,RSE3_TLDEC_22))  * positif(max(RSE3_TLDEC_24,RSE3_TLDEC_22) - RSE3_R9901)
                       * positif(INRRSE3_R99R_A+INRRSE3_R9901_A+0)
               + (INRRSE3_TL_22_A+INRRSE3_NTL_22_A - DO_INR_RSE3_A - arr(RSE3_TLDEC_22 * TXINR_PA22/100))
                            * positif(RSE3_REF - max(RSE3_TLDEC_24,RSE3_TLDEC_22))
                            )
                            ) * positif(FLAG_RETARD22)
                ));

RECUP_INR_RSE3 = max(0,(min(max(0,DO_INR_RSE3_A-RECUP_INR_RSE3_A),arr(max(0,DO_INR_RSE3_A-RECUP_INR_RSE3_A) * (RSE3_TLDEC - RSE3BASE_INR_A)/DO_RSE3_A))
                    *positif(RSE3_TLDEC-RSE3BASE_INR_A)*positif(RSE3_REF-RSE3BASE_INR_A)
                    * positif(RSE3_PA - RSE3_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_RSE3_A-RECUP_INR_RSE3_A),arr((RSE3_R99R - RSE3BASE_INR_A) * TXINR_PA/100))*positif(RSE3_TLDEC - RSE3BASE_INR_A)
                    * (1-positif(RSE3_PA - RSE3_TLDEC))
                    *positif(max(0,DO_INR_RSE3_A-RECUP_INR_RSE3_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_RSE32 = (RSE3_REF - RSE3_TLDEC_22+RSE3_TLDEC_24) * positif(RSE3_REF - RSE3_TLDEC_22+RSE3_TLDEC_24)* positif(RSE3BASE_INR_A);
SUP_RSE3_MAX2 = (RSE3_REF - max(0,RSE3_R9901)) * positif(RSE3_REF - max(0,RSE3_R9901))* positif(RSE3BASE_INR_A);
DO_INR_RSE3982 = max(0,
          arr((RSE3_REF - RSE3_NTLDEC_198) * TXINRRED_A/100) 
            *positif(RSE3_REF - RSE3_NTLDEC_198))*(1-positif(DO_INR_RSE32)) * present(RSE3_NTLDEC_198);
DO_INR_RSE3992 = max(0,
          arr((RSE3_REF - RSE3_TLDEC_199) * TXINRRED_A/100)
            *positif(RSE3_REF - RSE3_TLDEC_199))*(1-positif(DO_INR_RSE32)) * present(RSE3_TLDEC_199);
INR_RSE3_TOT = max(INRRSE3_NTLDECD+INRRSE3_NTLDECD_22 +INRRSE3_NTLDECD_24+ (INRRSE3_TLDECD+INRRSE3_TLDEC_22+INRRSE3_TLDEC_24)*TL_RSE3-INR_RSE3_TARDIF*null(1-IND_PASSAGE)+INRRSE3_R99R+RECUP_INR_RSE3,0) 
	      * (1-IND_RJLJ);
INRRSE3_RECT= arr((RSE3_RECT-CSPROVYG)* (TXINR_PA/100)) * positif(RSE3_RECT) * FLAG_RECTIF;
INCRSE3_TL2 = INRRSE3_TLDECD;
INCRSE3_TL_222 = INRRSE3_TLDEC_22;
INRSE3_TL2 = INRRSE3_TLA * TL_RSE3;
INRSE3_TL_222 = INRRSE3_TLA_22 * TL_RSE3;
INRRSE3_NET2 = max(INRRSE3_NTLDECD+INRRSE3_TLDECD*TL_RSE3+INRRSE3_R99R+RECUP_INR_RSE3,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_RSE32 * (-1);
INRRSE3_NET_222 = max(INRRSE3_NTLDECD_22+INRRSE3_NTLDECD_24+(INRRSE3_TLDEC_22+INRRSE3_TLDEC_24)*TL_RSE3,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_RSE3982 + DO_INR_RSE3992)*(-1);
INCRSE3_NET2 = max(0,(INRRSE3_NET2 + INRRSE3_NET_222 + INCRSE3_NET_A+(INRRSE3_TL_A+INRRSE3_TL_22_A+INRRSE3_TL_24_A)*(1-null(TL_RSE3_A-TL_RSE3))*positif(TL_RSE3))) * positif(RSE3BASE_INR+NRSE3BASE)* (1-IND_RJLJ);
RSE3_PRI2=RSE3_R9901;
RSE3_ANT2=RSE3BASE_INR_A;
RSE3_NTL2=RSE3_NTLDEC;
RSE3_NTL_222=RSE3_NTLDEC_22;
RSE3_TL2=RSE3_TLDEC;
RSE3_TL_222=RSE3_TLDEC_22;
RSE3_REF_INR=RSE3_REF;
INRRSE4_R99RA = INRRSE4_R99R_A;
INRRSE4_R99R = arr((RSE4_R99R-CSPROVYH-CSPROVYP) * (TXINR_PA/100)-INCRSE4_NET_A) * positif(RSE4_R99R-RSE4_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRRSE4_R9901A = INRRSE4_R9901_A;
INRRSE4_R9901 = arr(RSE4_R9901 * (TXINR_PA/100)-INCRSE4_NET_A) * positif(RSE4_R9901- RSE4_R9901_A)
              * positif(IND_PASSAGE-1) * positif(RSE4_TLDEC-RSE4_R9901) * positif(RSE4_R9901_A)
             + (arr(RSE4_R9901 * (TXINR_PA/100))-INCRSE4_NET_A) * positif(RSE4_R9901- RSE4BASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(RSE4_TLDEC-RSE4_R9901) * (1-positif(RSE4_R9901_A))
             + (INCRSE4_NET_A - arr(RSE4_R9901 * (TXINR_PA/100))) * positif(RSE4BASE_INR_A- RSE4_R9901)
              * positif(IND_PASSAGE-1) * positif(RSE4_TLDEC-RSE4_R9901) * (1-positif(RSE4_R9901_A)) * positif(RSE4_R9901)
	     ;
DO_INR_RSE4C=DO_INR_RSE4_A;
INR_NTL_GLOB_RSE42 = INRRSE4_NTLDECD + INRRSE4_NTL_A+ INRRSE4_NTLDECD_22 + INRRSE4_NTL_22_A+INRRSE4_NTLDECD_24 + INRRSE4_NTL_24_A;
INR_TL_GLOB_RSE42 = INRRSE4_TLDECD + INRRSE4_TL_A+ INRRSE4_TLDEC_22 + INRRSE4_TL_22_A+INRRSE4_TLDEC_24 + INRRSE4_TL_24_A;
INR_TOT_GLOB_RSE42 = max(0,INR_NTL_GLOB_RSE42 + INR_TL_GLOB_RSE42*TL_RSE4+INRRSE4_R99R+INRRSE4_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_RSE4C= (INRRSE4_NTLDECD+ INRRSE4_NTL_A+ (INRRSE4_TLDECD + INRRSE4_TL_A)*TL_RSE4 +INRRSE4_R99R+INRRSE4_R99R_A) * (1-IND_RJLJ) ;

DO_INR_RSE42 = (1-null(RSE4_REF_A-min(RSE4_REF,max(RSE4_TLDEC_22,RSE4_TLDEC_24)))) * max(0,
           (arr(((INRRSE4_TL_A+INRRSE4_TL_22_A+INRRSE4_TL_24_A)*TL_RSE4_A*TL_RSE4 + INRRSE4_NTL_A+INRRSE4_NTL_22_A+INRRSE4_NTL_24_A)
           * min(1,((RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24)/(RSE4_REF-max(0,RSE4_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24)* positif(RSE4_REF - (max(0,RSE4_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(RSE4_TLDEC_22 - RSE4BASE_INR_A))
           +arr(((INRRSE4_TL_A+INRRSE4_TL_22_A+INRRSE4_TL_24_A)*TL_RSE4_A*TL_RSE4)
             * min(1,((RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24)/(RSE4_REF-max(0,RSE4_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24)* positif(RSE4_REF - (max(0,RSE4_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(RSE4_TLDEC_22 - RSE4BASE_INR_A))
             * (1-positif(INRRSE4_NTL_A+INRRSE4_NTL_22_A+INRRSE4_NTL_24_A))
          + (INRRSE4_NTL_A*FLAG_C02+(INRRSE4_NTL_22_A+INRRSE4_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24)* positif(RSE4_REF - (max(0,RSE4_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRRSE4_NTL_A)*positif(INRRSE4_NTL_22_A+INRRSE4_NTL_24_A)
          + arr((INRRSE4_NTL_A*FLAG_C02+(INRRSE4_NTL_22_A+INRRSE4_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24)* positif(RSE4_REF - (max(0,RSE4_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24)/(RSE4_REF-max(0,RSE4_R9901)))))
             * (1-positif(INRRSE4_NTL_A)*positif(INRRSE4_NTL_22_A+INRRSE4_NTL_24_A))
          + ((INRRSE4_TL_A+INRRSE4_TL_22_A+INRRSE4_TL_24_A)*null(TL_RSE4) * TL_RSE4_A
          * (1- FLAG_DEFAUT)
             *positif(RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24)* positif(RSE4_REF - (max(0,RSE4_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRRSE4_TL_A + INRRSE4_R99R_A+INRRSE4_NTL_A - max(0,arr(RSE4_TLDEC * TXINR_PA/100))) * positif(RSE4_R99R - RSE4_TLDEC)
         + (INRRSE4_R99R_A+INRRSE4_NTL_A - max(0,arr(RSE4_R99R * TXINR_PA/100))) * positif(RSE4_TLDEC-(RSE4_R99R))
         + (INRRSE4_TL_A + INRRSE4_R99R_A+INRRSE4_NTL_A - max(0,arr(RSE4_R99R * TXINR_PA/100))) * null(RSE4_TLDEC-(RSE4_R99R)))
                      + (1-positif(SOM9YI)) * ( 
          arr((INRRSE4_TL_A*TL_RSE4_A *TL_RSE4+(INRRSE4_NTL_A +INRRSE4_R99R+INRRSE4_R9901-INRRSE4_RETDEF) 
                       * ((RSE4_REF - RSE4_TLDEC)/(RSE4_REF-max(0,RSE4_REF_A)))))
                       * positif(RSE4_REF - RSE4_TLDEC)  * positif(RSE4_TLDEC - RSE4_R99R) 
                       * positif(INRRSE4_R99R_A+INRRSE4_R9901_A+0)
         + (INR_TOT_GLOB_RSE4C - DO_INR_RSE4_A - arr(RSE4_TLDEC * TXINR_PA/100))
                       * positif(RSE4_REF - RSE4_TLDEC)
                         )
                       ) * (1-positif(FLAG_RETARD22))
               +                  (positif(SOM9YI)* (
                (INRRSE4_TL_22_A + INRRSE4_R99R_A+INRRSE4_NTL_22_A - max(0,arr(RSE4_TLDEC_22 * TXINR_PA22/100))) * positif(RSE4_R9901 - max(RSE4_TLDEC_24,RSE4_TLDEC_22))
          + (INRRSE4_R99R_A+INRRSE4_NTL_22_A - max(0,arr(RSE4_R99R * TXINR_PA22/100))) * positif(max(RSE4_TLDEC_24,RSE4_TLDEC_22)-(RSE4_R9901))
          + (INRRSE4_TL_22_A + INRRSE4_R99R_A+INRRSE4_NTL_22_A - max(0,arr(RSE4_R99R * TXINR_PA22/100))) * null(max(RSE4_TLDEC_24,RSE4_TLDEC_22)-(RSE4_R9901)))
                         + (1-positif(SOM9YI)) * (
           arr((INRRSE4_TL_22_A*TL_RSE4_A *TL_RSE4+(INRRSE4_NTL_22_A +INRRSE4_R99R+INRRSE4_R9901-INRRSE4_RETDEF)
                          * ((RSE4_REF - RSE4_TLDEC_22)/(RSE4_REF-max(0,RSE4_REF_A)))))
                         * positif(RSE4_REF - max(RSE4_TLDEC_24,RSE4_TLDEC_22))  * positif(max(RSE4_TLDEC_24,RSE4_TLDEC_22) - RSE4_R9901)
                        * positif(INRRSE4_R99R_A+INRRSE4_R9901_A+0)
             + (INRRSE4_TL_22_A+INRRSE4_NTL_22_A - DO_INR_RSE4_A - arr(RSE4_TLDEC_22 * TXINR_PA22/100))
                            * positif(RSE4_REF - max(RSE4_TLDEC_24,RSE4_TLDEC_22))
                          )
                         ) * positif(FLAG_RETARD22)
             ));

RECUP_INR_RSE4 = max(0,(min(max(0,DO_INR_RSE4_A-RECUP_INR_RSE4_A),arr(max(0,DO_INR_RSE4_A-RECUP_INR_RSE4_A) * (RSE4_TLDEC - RSE4BASE_INR_A)/DO_RSE4_A))
                    *positif(RSE4_TLDEC-RSE4BASE_INR_A)*positif(RSE4_REF-RSE4BASE_INR_A)
                    * positif(RSE4_PA - RSE4_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_RSE4_A-RECUP_INR_RSE4_A),arr((RSE4_R99R - RSE4BASE_INR_A) * TXINR_PA/100))*positif(RSE4_TLDEC - RSE4BASE_INR_A)
                    * (1-positif(RSE4_PA - RSE4_TLDEC))
                    *positif(max(0,DO_INR_RSE4_A-RECUP_INR_RSE4_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_RSE42 = (RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24) * positif(RSE4_REF - RSE4_TLDEC_22-RSE4_TLDEC_24)* positif(RSE4BASE_INR_A);
SUP_RSE4_MAX2 = (RSE4_REF - max(0,RSE4_R9901)) * positif(RSE4_REF - max(0,RSE4_R9901))* positif(RSE4BASE_INR_A);
DO_INR_RSE4982 = max(0,
          arr((RSE4_REF - RSE4_NTLDEC_198) * TXINRRED_A/100) 
            *positif(RSE4_REF - RSE4_NTLDEC_198))*(1-positif(DO_INR_RSE42)) * present(RSE4_NTLDEC_198);
DO_INR_RSE4992 = max(0,
          arr((RSE4_REF - RSE4_TLDEC_199) * TXINRRED_A/100)
            *positif(RSE4_REF - RSE4_TLDEC_199))*(1-positif(DO_INR_RSE42)) * present(RSE4_TLDEC_199);
INR_RSE4_TOT = max(INRRSE4_NTLDECD+INRRSE4_NTLDECD_22+INRRSE4_NTLDECD_24 + (INRRSE4_TLDECD+INRRSE4_TLDEC_22+INRRSE4_TLDEC_24)*TL_RSE4-INR_RSE4_TARDIF*null(1-IND_PASSAGE)+INRRSE4_R99R+RECUP_INR_RSE4,0) 
	      * (1-IND_RJLJ);
INRRSE4_RECT= arr((RSE4_RECT -CSPROVYH)* (TXINR_PA/100)) * positif(RSE4_RECT) * FLAG_RECTIF;
INCRSE4_TL2 = INRRSE4_TLDECD;
INCRSE4_TL_222 = INRRSE4_TLDEC_22;
INRSE4_TL2 = INRRSE4_TLA * TL_RSE4;
INRSE4_TL_222 = INRRSE4_TLA_22 * TL_RSE4;
INRRSE4_NET2 = max(INRRSE4_NTLDECD+INRRSE4_TLDECD*TL_RSE4+INRRSE4_R99R+RECUP_INR_RSE4,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_RSE42 * (-1);
INRRSE4_NET_222 = max(INRRSE4_NTLDECD_22+INRRSE4_NTLDECD_24+(INRRSE4_TLDEC_22+INRRSE4_TLDEC_24)*TL_RSE4,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_RSE4982 + DO_INR_RSE4992)*(-1);
INCRSE4_NET2 = max(0,(INRRSE4_NET2 + INRRSE4_NET_222 + INCRSE4_NET_A+(INRRSE4_TL_A+INRRSE4_TL_22_A+INRRSE4_TL_24_A)*(1-null(TL_RSE4_A-TL_RSE4))*positif(TL_RSE4))) * positif(RSE4BASE_INR+NRSE4BASE)* (1-IND_RJLJ);
RSE4_PRI2=RSE4_R9901;
RSE4_ANT2=RSE4BASE_INR_A;
RSE4_NTL2=RSE4_NTLDEC;
RSE4_NTL_222=RSE4_NTLDEC_22;
RSE4_TL2=RSE4_TLDEC;
RSE4_TL_222=RSE4_TLDEC_22;
RSE4_REF_INR=RSE4_REF;
INRRSE5_R99RA = INRRSE5_R99R_A;
INRRSE5_R99R = arr((RSE5_R99R-CSPROVYH) * (TXINR_PA/100)-INCRSE5_NET_A) * positif(RSE5_R99R-RSE5_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRRSE5_R9901A = INRRSE5_R9901_A;
INRRSE5_R9901 = arr(RSE5_R9901 * (TXINR_PA/100)-INCRSE5_NET_A) * positif(RSE5_R9901- RSE5_R9901_A)
              * positif(IND_PASSAGE-1) * positif(RSE5_TLDEC-RSE5_R9901) * positif(RSE5_R9901_A)
             + (arr(RSE5_R9901 * (TXINR_PA/100))-INCRSE5_NET_A) * positif(RSE5_R9901- RSE5BASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(RSE5_TLDEC-RSE5_R9901) * (1-positif(RSE5_R9901_A))
             + (INCRSE5_NET_A - arr(RSE5_R9901 * (TXINR_PA/100))) * positif(RSE5BASE_INR_A- RSE5_R9901)
              * positif(IND_PASSAGE-1) * positif(RSE5_TLDEC-RSE5_R9901) * (1-positif(RSE5_R9901_A)) * positif(RSE5_R9901)
	     ;
DO_INR_RSE5C=DO_INR_RSE5_A;
INR_NTL_GLOB_RSE52 = INRRSE5_NTLDECD + INRRSE5_NTL_A+ INRRSE5_NTLDECD_22 + INRRSE5_NTL_22_A+INRRSE5_NTLDECD_24 + INRRSE5_NTL_24_A;
INR_TL_GLOB_RSE52 = INRRSE5_TLDECD + INRRSE5_TL_A+ INRRSE5_TLDEC_22 + INRRSE5_TL_22_A+INRRSE5_TLDEC_24 + INRRSE5_TL_24_A;
INR_TOT_GLOB_RSE52 = max(0,INR_NTL_GLOB_RSE52 + INR_TL_GLOB_RSE52*TL_RSE5+INRRSE5_R99R+INRRSE5_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_RSE5C= (INRRSE5_NTLDECD+ INRRSE5_NTL_A+ (INRRSE5_TLDECD + INRRSE5_TL_A)*TL_RSE5 +INRRSE5_R99R+INRRSE5_R99R_A) * (1-IND_RJLJ) ;

DO_INR_RSE52 = (1-null(RSE5_REF_A-min(RSE5_REF,max(RSE5_TLDEC_22,RSE5_TLDEC_24)))) * max(0,
           (arr(((INRRSE5_TL_A+INRRSE5_TL_22_A+INRRSE5_TL_24_A)*TL_RSE5_A*TL_RSE5 + INRRSE5_NTL_A+INRRSE5_NTL_22_A+INRRSE5_NTL_24_A)
           * min(1,((RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24)/(RSE5_REF-max(0,RSE5_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24)* positif(RSE5_REF - (max(0,RSE5_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(RSE5_TLDEC_22 - RSE5BASE_INR_A))
           +arr(((INRRSE5_TL_A+INRRSE5_TL_22_A+INRRSE5_TL_24_A)*TL_RSE5_A*TL_RSE5)
             * min(1,((RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24)/(RSE5_REF-max(0,RSE5_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24)* positif(RSE5_REF - (max(0,RSE5_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(RSE5_TLDEC_22 - RSE5BASE_INR_A))
             * (1-positif(INRRSE5_NTL_A+INRRSE5_NTL_22_A+INRRSE5_NTL_24_A))
          + (INRRSE5_NTL_A*FLAG_C02+(INRRSE5_NTL_22_A+INRRSE5_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24)* positif(RSE5_REF - (max(0,RSE5_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRRSE5_NTL_A)*positif(INRRSE5_NTL_22_A+INRRSE5_NTL_24_A)
          + arr((INRRSE5_NTL_A*FLAG_C02+(INRRSE5_NTL_22_A+INRRSE5_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24)* positif(RSE5_REF - (max(0,RSE5_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24)/(RSE5_REF-max(0,RSE5_R9901)))))
             * (1-positif(INRRSE5_NTL_A)*positif(INRRSE5_NTL_22_A+INRRSE5_NTL_24_A))
          + ((INRRSE5_TL_A+INRRSE5_TL_22_A+INRRSE5_TL_24_A)*null(TL_RSE5) * TL_RSE5_A
          * (1- FLAG_DEFAUT)
             *positif(RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24)* positif(RSE5_REF - (max(0,RSE5_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRRSE5_TL_A + INRRSE5_R99R_A+INRRSE5_NTL_A - max(0,arr(RSE5_TLDEC * TXINR_PA/100))) * positif(RSE5_R99R - RSE5_TLDEC)
         + (INRRSE5_R99R_A+INRRSE5_NTL_A - max(0,arr(RSE5_R99R * TXINR_PA/100))) * positif(RSE5_TLDEC-(RSE5_R99R))
         + (INRRSE5_TL_A + INRRSE5_R99R_A+INRRSE5_NTL_A - max(0,arr(RSE5_R99R * TXINR_PA/100))) * null(RSE5_TLDEC-(RSE5_R99R)))
                      + (1-positif(SOM9YI)) * ( 
          arr((INRRSE5_TL_A*TL_RSE5_A *TL_RSE5+(INRRSE5_NTL_A +INRRSE5_R99R+INRRSE5_R9901-INRRSE5_RETDEF) 
                       * ((RSE5_REF - RSE5_TLDEC)/(RSE5_REF-max(0,RSE5_REF_A)))))
                       * positif(RSE5_REF - RSE5_TLDEC)  * positif(RSE5_TLDEC - RSE5_R99R) 
                       * positif(INRRSE5_R99R_A+INRRSE5_R9901_A+0)
         + (INR_TOT_GLOB_RSE5C - DO_INR_RSE5_A - arr(RSE5_TLDEC * TXINR_PA/100))
                       * positif(RSE5_REF - RSE5_TLDEC)
		       )
                       ) * (1-positif(FLAG_RETARD22))
               +                  (positif(SOM9YI)* (
                (INRRSE5_TL_22_A + INRRSE5_R99R_A+INRRSE5_NTL_22_A - max(0,arr(RSE5_TLDEC_22 * TXINR_PA22/100))) * positif(RSE5_R9901 - max(RSE5_TLDEC_24,RSE5_TLDEC_22))
          + (INRRSE5_R99R_A+INRRSE5_NTL_22_A - max(0,arr(RSE5_R99R * TXINR_PA22/100))) * positif(max(RSE5_TLDEC_24,RSE5_TLDEC_22)-(RSE5_R9901))
        + (INRRSE5_TL_22_A + INRRSE5_R99R_A+INRRSE5_NTL_22_A - max(0,arr(RSE5_R99R * TXINR_PA22/100))) * null(max(RSE5_TLDEC_24,RSE5_TLDEC_22)-(RSE5_R9901)))
                        + (1-positif(SOM9YI)) * (
          arr((INRRSE5_TL_22_A*TL_RSE5_A *TL_RSE5+(INRRSE5_NTL_22_A +INRRSE5_R99R+INRRSE5_R9901-INRRSE5_RETDEF)
                        * ((RSE5_REF - RSE5_TLDEC_22)/(RSE5_REF-max(0,RSE5_REF_A)))))
                      * positif(RSE5_REF - max(RSE5_TLDEC_24,RSE5_TLDEC_22))  * positif(max(RSE5_TLDEC_24,RSE5_TLDEC_22) - RSE5_R9901)
                        * positif(INRRSE5_R99R_A+INRRSE5_R9901_A+0)
          + (INRRSE5_TL_22_A+INRRSE5_NTL_22_A - DO_INR_RSE5_A - arr(RSE5_TLDEC_22 * TXINR_PA22/100))
                            * positif(RSE5_REF - max(RSE5_TLDEC_24,RSE5_TLDEC_22))
                        )
                      ) * positif(FLAG_RETARD22)
              ));

RECUP_INR_RSE5 = max(0,(min(max(0,DO_INR_RSE5_A-RECUP_INR_RSE5_A),arr(max(0,DO_INR_RSE5_A-RECUP_INR_RSE5_A) * (RSE5_TLDEC - RSE5BASE_INR_A)/DO_RSE5_A))
                    *positif(RSE5_TLDEC-RSE5BASE_INR_A)*positif(RSE5_REF-RSE5BASE_INR_A)
                    * positif(RSE5_PA - RSE5_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_RSE5_A-RECUP_INR_RSE5_A),arr((RSE5_R99R - RSE5BASE_INR_A) * TXINR_PA/100))*positif(RSE5_TLDEC - RSE5BASE_INR_A)
                    * (1-positif(RSE5_PA - RSE5_TLDEC))
                    *positif(max(0,DO_INR_RSE5_A-RECUP_INR_RSE5_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_RSE52 = (RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24) * positif(RSE5_REF - RSE5_TLDEC_22-RSE5_TLDEC_24)* positif(RSE5BASE_INR_A);
SUP_RSE5_MAX2 = (RSE5_REF - max(0,RSE5_R9901)) * positif(RSE5_REF - max(0,RSE5_R9901))* positif(RSE5BASE_INR_A);
DO_INR_RSE5982 = max(0,
          arr((RSE5_REF - RSE5_NTLDEC_198) * TXINRRED_A/100) 
            *positif(RSE5_REF - RSE5_NTLDEC_198))*(1-positif(DO_INR_RSE52)) * present(RSE5_NTLDEC_198);
DO_INR_RSE5992 = max(0,
          arr((RSE5_REF - RSE5_TLDEC_199) * TXINRRED_A/100)
            *positif(RSE5_REF - RSE5_TLDEC_199))*(1-positif(DO_INR_RSE52)) * present(RSE5_TLDEC_199);
INR_RSE5_TOT = max(INRRSE5_NTLDECD+INRRSE5_NTLDECD_22 +INRRSE5_NTLDECD_24+ (INRRSE5_TLDECD+INRRSE5_TLDEC_22+INRRSE5_TLDEC_24)*TL_RSE5-INR_RSE5_TARDIF*null(1-IND_PASSAGE)+INRRSE5_R99R+RECUP_INR_RSE5,0) 
	      * (1-IND_RJLJ);
INRRSE5_RECT= arr((RSE5_RECT-CSPROVYH)* (TXINR_PA/100)) * positif(RSE5_RECT) * FLAG_RECTIF;
INCRSE5_TL2 = INRRSE5_TLDECD;
INCRSE5_TL_222 = INRRSE5_TLDEC_22;
INRSE5_TL2 = INRRSE5_TLA * TL_RSE5;
INRSE5_TL_222 = INRRSE5_TLA_22 * TL_RSE5;
INRRSE5_NET2 = max(INRRSE5_NTLDECD+INRRSE5_TLDECD*TL_RSE5+INRRSE5_R99R+RECUP_INR_RSE5,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_RSE52 * (-1);
INRRSE5_NET_222 = max(INRRSE5_NTLDECD_22+INRRSE5_NTLDECD_24+(INRRSE5_TLDEC_22+INRRSE5_TLDEC_24)*TL_RSE5,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_RSE5982 + DO_INR_RSE5992)*(-1);
INCRSE5_NET2 = max(0,(INRRSE5_NET2 + INRRSE5_NET_222 + INCRSE5_NET_A+(INRRSE5_TL_A+INRRSE5_TL_22_A+INRRSE5_TL_24_A)*(1-null(TL_RSE5_A-TL_RSE5))*positif(TL_RSE5))) * positif(RSE5BASE_INR +NRSE5BASE)* (1-IND_RJLJ);
RSE5_PRI2=RSE5_R9901;
RSE5_ANT2=RSE5BASE_INR_A;
RSE5_NTL2=RSE5_NTLDEC;
RSE5_NTL_222=RSE5_NTLDEC_22;
RSE5_TL2=RSE5_TLDEC;
RSE5_TL_222=RSE5_TLDEC_22;
RSE5_REF_INR=RSE5_REF;
INRRSE6_R99RA = INRRSE6_R99R_A;
INRRSE6_R99R = arr((RSE6_R99R-CSPROVYH) * (TXINR_PA/100)-INCRSE6_NET_A) * positif(RSE6_R99R-RSE6_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRRSE6_R9901A = INRRSE6_R9901_A;
INRRSE6_R9901 = arr(RSE6_R9901 * (TXINR_PA/100)-INCRSE6_NET_A) * positif(RSE6_R9901- RSE6_R9901_A)
              * positif(IND_PASSAGE-1) * positif(RSE6_TLDEC-RSE6_R9901) * positif(RSE6_R9901_A)
             + (arr(RSE6_R9901 * (TXINR_PA/100))-INCRSE6_NET_A) * positif(RSE6_R9901- RSE6BASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(RSE6_TLDEC-RSE6_R9901) * (1-positif(RSE6_R9901_A))
             + (INCRSE6_NET_A - arr(RSE6_R9901 * (TXINR_PA/100))) * positif(RSE6BASE_INR_A- RSE6_R9901)
              * positif(IND_PASSAGE-1) * positif(RSE6_TLDEC-RSE6_R9901) * (1-positif(RSE6_R9901_A)) * positif(RSE6_R9901)
	     ;
DO_INR_RSE6C=DO_INR_RSE6_A;
INR_NTL_GLOB_RSE62 = INRRSE6_NTLDECD + INRRSE6_NTL_A+ INRRSE6_NTLDECD_22 + INRRSE6_NTL_22_A+INRRSE6_NTLDECD_24 + INRRSE6_NTL_24_A;
INR_TL_GLOB_RSE62 = INRRSE6_TLDECD + INRRSE6_TL_A+ INRRSE6_TLDEC_22 + INRRSE6_TL_22_A+INRRSE6_TLDEC_24 + INRRSE6_TL_24_A;
INR_TOT_GLOB_RSE62 = max(0,INR_NTL_GLOB_RSE62 + INR_TL_GLOB_RSE62*TL_RSE6+INRRSE6_R99R+INRRSE6_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_RSE6C= (INRRSE6_NTLDECD+ INRRSE6_NTL_A+ (INRRSE6_TLDECD + INRRSE6_TL_A)*TL_RSE6 +INRRSE6_R99R+INRRSE6_R99R_A) * (1-IND_RJLJ) ;

DO_INR_RSE62 = (1-null(RSE6_REF_A-min(RSE6_REF,max(RSE6_TLDEC_22,RSE6_TLDEC_24)))) * max(0,
           (arr(((INRRSE6_TL_A+INRRSE6_TL_22_A+INRRSE6_TL_24_A)*TL_RSE6_A*TL_RSE6 + INRRSE6_NTL_A+INRRSE6_NTL_22_A+INRRSE6_NTL_24_A)
           * min(1,((RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24)/(RSE6_REF-max(0,RSE6_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24)* positif(RSE6_REF - (max(0,RSE6_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(RSE6_TLDEC_22 - RSE6BASE_INR_A))
           +arr(((INRRSE6_TL_A+INRRSE6_TL_22_A+INRRSE6_TL_24_A)*TL_RSE6_A*TL_RSE6)
             * min(1,((RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24)/(RSE6_REF-max(0,RSE6_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24)* positif(RSE6_REF - (max(0,RSE6_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(RSE6_TLDEC_22 - RSE6BASE_INR_A))
             * (1-positif(INRRSE6_NTL_A+INRRSE6_NTL_22_A+INRRSE6_NTL_24_A))
          + (INRRSE6_NTL_A*FLAG_C02+(INRRSE6_NTL_22_A+INRRSE6_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24)* positif(RSE6_REF - (max(0,RSE6_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRRSE6_NTL_A)*positif(INRRSE6_NTL_22_A+INRRSE6_NTL_24_A)
          + arr((INRRSE6_NTL_A*FLAG_C02+(INRRSE6_NTL_22_A+INRRSE6_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24)* positif(RSE6_REF - (max(0,RSE6_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24)/(RSE6_REF-max(0,RSE6_R9901)))))
             * (1-positif(INRRSE6_NTL_A)*positif(INRRSE6_NTL_22_A+INRRSE6_NTL_24_A))
          + ((INRRSE6_TL_A+INRRSE6_TL_22_A+INRRSE6_TL_24_A)*null(TL_RSE6) * TL_RSE6_A
          * (1- FLAG_DEFAUT)
             *positif(RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24)* positif(RSE6_REF - (max(0,RSE6_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRRSE6_TL_A + INRRSE6_R99R_A+INRRSE6_NTL_A - max(0,arr(RSE6_TLDEC * TXINR_PA/100))) * positif(RSE6_R99R - RSE6_TLDEC)
         + (INRRSE6_R99R_A+INRRSE6_NTL_A - max(0,arr(RSE6_R99R * TXINR_PA/100))) * positif(RSE6_TLDEC-(RSE6_R99R))
         + (INRRSE6_TL_A + INRRSE6_R99R_A+INRRSE6_NTL_A - max(0,arr(RSE6_R99R * TXINR_PA/100))) * null(RSE6_TLDEC-(RSE6_R99R)))
                      + (1-positif(SOM9YI)) * ( 
          arr((INRRSE6_TL_A*TL_RSE6_A *TL_RSE6+(INRRSE6_NTL_A +INRRSE6_R99R+INRRSE6_R9901-INRRSE6_RETDEF) 
                       * ((RSE6_REF - RSE6_TLDEC)/(RSE6_REF-max(0,RSE6_REF_A)))))
                       * positif(RSE6_REF - RSE6_TLDEC)  * positif(RSE6_TLDEC - RSE6_R99R) 
                       * positif(INRRSE6_R99R_A+INRRSE6_R9901_A+0)
         + (INR_TOT_GLOB_RSE6C - DO_INR_RSE6_A - arr(RSE6_TLDEC * TXINR_PA/100))
                       * positif(RSE6_REF - RSE6_TLDEC)
		       )
                       ) * (1-positif(FLAG_RETARD22))
               +                  (positif(SOM9YI)* (
                 (INRRSE6_TL_22_A + INRRSE6_R99R_A+INRRSE6_NTL_22_A - max(0,arr(RSE6_TLDEC_22 * TXINR_PA22/100))) * positif(RSE6_R9901 - max(RSE6_TLDEC_24,RSE6_TLDEC_22))
          + (INRRSE6_R99R_A+INRRSE6_NTL_22_A - max(0,arr(RSE6_R99R * TXINR_PA22/100))) * positif(max(RSE6_TLDEC_24,RSE6_TLDEC_22)-(RSE6_R9901))
           + (INRRSE6_TL_22_A + INRRSE6_R99R_A+INRRSE6_NTL_22_A - max(0,arr(RSE6_R99R * TXINR_PA22/100))) * null(max(RSE6_TLDEC_24,RSE6_TLDEC_22)-(RSE6_R9901)))
                         + (1-positif(SOM9YI)) * (
           arr((INRRSE6_TL_22_A*TL_RSE6_A *TL_RSE6+(INRRSE6_NTL_22_A +INRRSE6_R99R+INRRSE6_R9901-INRRSE6_RETDEF)
                         * ((RSE6_REF - RSE6_TLDEC_22)/(RSE6_REF-max(0,RSE6_REF_A)))))
                      * positif(RSE6_REF - max(RSE6_TLDEC_24,RSE6_TLDEC_22))  * positif(max(RSE6_TLDEC_24,RSE6_TLDEC_22) - RSE6_R9901)
                        * positif(INRRSE6_R99R_A+INRRSE6_R9901_A+0)
          + (INRRSE6_TL_22_A+INRRSE6_NTL_22_A - DO_INR_RSE6_A - arr(RSE6_TLDEC_22 * TXINR_PA22/100))
                             * positif(RSE6_REF - max(RSE6_TLDEC_24,RSE6_TLDEC_22))
                      )
                        ) * positif(FLAG_RETARD22)
                ));

RECUP_INR_RSE6 = max(0,(min(max(0,DO_INR_RSE6_A-RECUP_INR_RSE6_A),arr(max(0,DO_INR_RSE6_A-RECUP_INR_RSE6_A) * (RSE6_TLDEC - RSE6BASE_INR_A)/DO_RSE6_A))
                    *positif(RSE6_TLDEC-RSE6BASE_INR_A)*positif(RSE6_REF-RSE6BASE_INR_A)
                    * positif(RSE6_PA - RSE6_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_RSE6_A-RECUP_INR_RSE6_A),arr((RSE6_R99R - RSE6BASE_INR_A) * TXINR_PA/100))*positif(RSE6_TLDEC - RSE6BASE_INR_A)
                    * (1-positif(RSE6_PA - RSE6_TLDEC))
                    *positif(max(0,DO_INR_RSE6_A-RECUP_INR_RSE6_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_RSE62 = (RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24) * positif(RSE6_REF - RSE6_TLDEC_22-RSE6_TLDEC_24)* positif(RSE6BASE_INR_A);
SUP_RSE6_MAX2 = (RSE6_REF - max(0,RSE6_R9901)) * positif(RSE6_REF - max(0,RSE6_R9901))* positif(RSE6BASE_INR_A);
DO_INR_RSE6982 = max(0,
          arr((RSE6_REF - RSE6_NTLDEC_198) * TXINRRED_A/100) 
            *positif(RSE6_REF - RSE6_NTLDEC_198))*(1-positif(DO_INR_RSE62)) * present(RSE6_NTLDEC_198);
DO_INR_RSE6992 = max(0,
          arr((RSE6_REF - RSE6_TLDEC_199) * TXINRRED_A/100)
            *positif(RSE6_REF - RSE6_TLDEC_199))*(1-positif(DO_INR_RSE62)) * present(RSE6_TLDEC_199);
INR_RSE6_TOT = max(INRRSE6_NTLDECD+INRRSE6_NTLDECD_22 +INRRSE6_NTLDECD_24+ (INRRSE6_TLDECD+INRRSE6_TLDEC_22+INRRSE6_TLDEC_24)*TL_RSE6-INR_RSE6_TARDIF*null(1-IND_PASSAGE)+INRRSE6_R99R+RECUP_INR_RSE6,0) 
	      * (1-IND_RJLJ);
INRRSE6_RECT= arr((RSE6_RECT-CSPROVYH)* (TXINR_PA/100)) * positif(RSE6_RECT) * FLAG_RECTIF;
INCRSE6_TL2 = INRRSE6_TLDECD;
INCRSE6_TL_222 = INRRSE6_TLDEC_22;
INRSE6_TL2 = INRRSE6_TLA * TL_RSE6;
INRSE6_TL_222 = INRRSE6_TLA_22 * TL_RSE6;
INRRSE6_NET2 = max(INRRSE6_NTLDECD+INRRSE6_TLDECD*TL_RSE6+INRRSE6_R99R+RECUP_INR_RSE6,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_RSE62 * (-1);
INRRSE6_NET_222 = max(INRRSE6_NTLDECD_22+INRRSE6_NTLDECD_24+(INRRSE6_TLDEC_22+INRRSE6_TLDEC_24)*TL_RSE6,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_RSE6982 + DO_INR_RSE6992)*(-1);
INCRSE6_NET2 = max(0,(INRRSE6_NET2 + INRRSE6_NET_222 + INCRSE6_NET_A+(INRRSE6_TL_A+INRRSE6_TL_22_A+INRRSE6_TL_24_A)*(1-null(TL_RSE6_A-TL_RSE6))*positif(TL_RSE6))) * positif(RSE6BASE_INR +NRSE6BASE)* (1-IND_RJLJ);
RSE6_PRI2=RSE6_R9901;
RSE6_ANT2=RSE6BASE_INR_A;
RSE6_NTL2=RSE6_NTLDEC;
RSE6_NTL_222=RSE6_NTLDEC_22;
RSE6_TL2=RSE6_TLDEC;
RSE6_TL_222=RSE6_TLDEC_22;
RSE6_REF_INR=RSE6_REF;
INRRSE8_R99RA = INRRSE8_R99R_A;
INRRSE8_R99R = arr((RSE8_R99R- COD8YV - COD8YX - CSPROVYH) * (TXINR_PA/100)-INCRSE8_NET_A) * positif(RSE8_R99R-RSE8_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRRSE8_R9901A = INRRSE8_R9901_A;
INRRSE8_R9901 = arr(RSE8_R9901 * (TXINR_PA/100)-INCRSE8_NET_A) * positif(RSE8_R9901- RSE8_R9901_A)
              * positif(IND_PASSAGE-1) * positif(RSE8_TLDEC-RSE8_R9901) * positif(RSE8_R9901_A)
             + (arr(RSE8_R9901 * (TXINR_PA/100))-INCRSE8_NET_A) * positif(RSE8_R9901- RSE8BASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(RSE8_TLDEC-RSE8_R9901) * (1-positif(RSE8_R9901_A))
             + (INCRSE8_NET_A - arr(RSE8_R9901 * (TXINR_PA/100))) * positif(RSE8BASE_INR_A- RSE8_R9901)
              * positif(IND_PASSAGE-1) * positif(RSE8_TLDEC-RSE8_R9901) * (1-positif(RSE8_R9901_A)) * positif(RSE8_R9901)
	     ;
DO_INR_RSE8C=DO_INR_RSE8_A;
INR_NTL_GLOB_RSE82 = INRRSE8_NTLDECD + INRRSE8_NTL_A+ INRRSE8_NTLDECD_22 + INRRSE8_NTL_22_A+INRRSE8_NTLDECD_24 + INRRSE8_NTL_24_A;
INR_TL_GLOB_RSE82 = INRRSE8_TLDECD + INRRSE8_TL_A+ INRRSE8_TLDEC_22 + INRRSE8_TL_22_A+INRRSE8_TLDEC_24 + INRRSE8_TL_24_A;
INR_TOT_GLOB_RSE82 = max(0,INR_NTL_GLOB_RSE82 + INR_TL_GLOB_RSE82*TL_RSE8+INRRSE8_R99R+INRRSE8_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_RSE8C= (INRRSE8_NTLDECD+ INRRSE8_NTL_A+ (INRRSE8_TLDECD + INRRSE8_TL_A)*TL_RSE8 +INRRSE8_R99R+INRRSE8_R99R_A) * (1-IND_RJLJ) ;
DO_INR_RSE82 = (1-null(RSE8_REF_A-min(RSE8_REF,max(RSE8_TLDEC_22,RSE8_TLDEC_24)))) * max(0,
           (arr(((INRRSE8_TL_A+INRRSE8_TL_22_A+INRRSE8_TL_24_A)*TL_RSE8_A*TL_RSE8 + INRRSE8_NTL_A+INRRSE8_NTL_22_A+INRRSE8_NTL_24_A)
           * min(1,((RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24)/(RSE8_REF-max(0,RSE8_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24)* positif(RSE8_REF - (max(0,RSE8_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(RSE8_TLDEC_22 - RSE8BASE_INR_A))
           +arr(((INRRSE8_TL_A+INRRSE8_TL_22_A+INRRSE8_TL_24_A)*TL_RSE8_A*TL_RSE8)
             * min(1,((RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24)/(RSE8_REF-max(0,RSE8_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24)* positif(RSE8_REF - (max(0,RSE8_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(RSE8_TLDEC_22 +RSE8_TLDEC_24- RSE8BASE_INR_A))
             * (1-positif(INRRSE8_NTL_A+INRRSE8_NTL_22_A+INRRSE8_NTL_24_A))
          + (INRRSE8_NTL_A*FLAG_C02+(INRRSE8_NTL_22_A+INRRSE8_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24)* positif(RSE8_REF - (max(0,RSE8_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRRSE8_NTL_A)*positif(INRRSE8_NTL_22_A+INRRSE8_NTL_24_A)
          + arr((INRRSE8_NTL_A*FLAG_C02+(INRRSE8_NTL_22_A+INRRSE8_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24)* positif(RSE8_REF - (max(0,RSE8_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24)/(RSE8_REF-max(0,RSE8_R9901)))))
             * (1-positif(INRRSE8_NTL_A)*positif(INRRSE8_NTL_22_A+INRRSE8_NTL_24_A))
          + ((INRRSE8_TL_A+INRRSE8_TL_22_A+INRRSE8_NTL_24_A)*null(TL_RSE8) * TL_RSE8_A
          * (1- FLAG_DEFAUT)
             *positif(RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24)* positif(RSE8_REF - (max(0,RSE8_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRRSE8_TL_A + INRRSE8_R99R_A+INRRSE8_NTL_A - max(0,arr(RSE8_TLDEC * TXINR_PA/100))) * positif(RSE8_R99R - RSE8_TLDEC)
         + (INRRSE8_R99R_A+INRRSE8_NTL_A - max(0,arr(RSE8_R99R * TXINR_PA/100))) * positif(RSE8_TLDEC-(RSE8_R99R))
         + (INRRSE8_TL_A + INRRSE8_R99R_A+INRRSE8_NTL_A - max(0,arr(RSE8_R99R * TXINR_PA/100))) * null(RSE8_TLDEC-(RSE8_R99R)))
                      + (1-positif(SOM9YI)) * ( 
          arr((INRRSE8_TL_A*TL_RSE8_A *TL_RSE8+(INRRSE8_NTL_A +INRRSE8_R99R+INRRSE8_R9901-INRRSE8_RETDEF) 
                       * ((RSE8_REF - RSE8_TLDEC)/(RSE8_REF-max(0,RSE8_REF_A)))))
                       * positif(RSE8_REF - RSE8_TLDEC)  * positif(RSE8_TLDEC - RSE8_R99R) 
                       * positif(INRRSE8_R99R_A+INRRSE8_R9901_A+0)
         + (INR_TOT_GLOB_RSE8C - DO_INR_RSE8_A - arr(RSE8_TLDEC * TXINR_PA/100))
                       * positif(RSE8_REF - RSE8_TLDEC)
		       )
                       ) * (1-positif(FLAG_RETARD22))
               +                  (positif(SOM9YI)* (
                 (INRRSE8_TL_22_A + INRRSE8_R99R_A+INRRSE8_NTL_22_A - max(0,arr(RSE8_TLDEC_22 * TXINR_PA22/100))) * positif(RSE8_R9901 - max(RSE8_TLDEC_24,RSE8_TLDEC_22))
          + (INRRSE8_R99R_A+INRRSE8_NTL_22_A - max(0,arr(RSE8_R99R * TXINR_PA22/100))) * positif(max(RSE8_TLDEC_24,RSE8_TLDEC_22)-(RSE8_R9901))
         + (INRRSE8_TL_22_A + INRRSE8_R99R_A+INRRSE8_NTL_22_A - max(0,arr(RSE8_R99R * TXINR_PA22/100))) * null(max(RSE8_TLDEC_24,RSE8_TLDEC_22)-(RSE8_R9901)))
                         + (1-positif(SOM9YI)) * (
           arr((INRRSE8_TL_22_A*TL_RSE8_A *TL_RSE8+(INRRSE8_NTL_22_A +INRRSE8_R99R+INRRSE8_R9901-INRRSE8_RETDEF)
                         * ((RSE8_REF - RSE8_TLDEC_22)/(RSE8_REF-max(0,RSE8_REF_A)))))
                       * positif(RSE8_REF - max(RSE8_TLDEC_24,RSE8_TLDEC_22))  * positif(max(RSE8_TLDEC_24,RSE8_TLDEC_22) - RSE8_R9901)
                       * positif(INRRSE8_R99R_A+INRRSE8_R9901_A+0)
               + (INRRSE8_TL_22_A+INRRSE8_NTL_22_A - DO_INR_RSE8_A - arr(RSE8_TLDEC_22 * TXINR_PA22/100))
                             * positif(RSE8_REF - max(RSE8_TLDEC_24,RSE8_TLDEC_22))
                           )
                           ) * positif(FLAG_RETARD22)
            ));

RECUP_INR_RSE8 = max(0,(min(max(0,DO_INR_RSE8_A-RECUP_INR_RSE8_A),arr(max(0,DO_INR_RSE8_A-RECUP_INR_RSE8_A) * (RSE8_TLDEC - RSE8BASE_INR_A)/DO_RSE8_A))
                    *positif(RSE8_TLDEC-RSE8BASE_INR_A)*positif(RSE8_REF-RSE8BASE_INR_A)
                    * positif(RSE8_PA - RSE8_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_RSE8_A-RECUP_INR_RSE8_A),arr((RSE8_R99R - RSE8BASE_INR_A) * TXINR_PA/100))*positif(RSE8_TLDEC - RSE8BASE_INR_A)
                    * (1-positif(RSE8_PA - RSE8_TLDEC))
                    *positif(max(0,DO_INR_RSE8_A-RECUP_INR_RSE8_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_RSE82 = (RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24) * positif(RSE8_REF - RSE8_TLDEC_22-RSE8_TLDEC_24)* positif(RSE8BASE_INR_A);
SUP_RSE8_MAX2 = (RSE8_REF - max(0,RSE8_R9901)) * positif(RSE8_REF - max(0,RSE8_R9901))* positif(RSE8BASE_INR_A);
DO_INR_RSE8982 = max(0,
          arr((RSE8_REF - RSE8_NTLDEC_198) * TXINRRED_A/100) 
            *positif(RSE8_REF - RSE8_NTLDEC_198))*(1-positif(DO_INR_RSE82)) * present(RSE8_NTLDEC_198);
DO_INR_RSE8992 = max(0,
          arr((RSE8_REF - RSE8_TLDEC_199) * TXINRRED_A/100)
            *positif(RSE8_REF - RSE8_TLDEC_199))*(1-positif(DO_INR_RSE82)) * present(RSE8_TLDEC_199);
INR_RSE8_TOT = max(INRRSE8_NTLDECD+INRRSE8_NTLDECD_22 +INRRSE8_NTLDECD_24+ (INRRSE8_TLDECD+INRRSE8_TLDEC_22+INRRSE8_TLDEC_24)*TL_RSE8-INR_RSE8_TARDIF*null(1-IND_PASSAGE)+INRRSE8_R99R+RECUP_INR_RSE8,0) 
	      * (1-IND_RJLJ);
INRRSE8_RECT= arr((RSE8_RECT-COD8YV- COD8YX - CSPROVYH)* (TXINR_PA/100)) * positif(RSE8_RECT) * FLAG_RECTIF;
INCRSE8_TL2 = INRRSE8_TLDECD;
INCRSE8_TL_222 = INRRSE8_TLDEC_22;
INRSE8_TL2 = INRRSE8_TLA * TL_RSE8;
INRSE8_TL_222 = INRRSE8_TLA_22 * TL_RSE8;
INRRSE8_NET2 = max(INRRSE8_NTLDECD+INRRSE8_TLDECD*TL_RSE8+INRRSE8_R99R+RECUP_INR_RSE8,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_RSE82 * (-1);
INRRSE8_NET_222 = max(INRRSE8_NTLDECD_22+INRRSE8_NTLDECD_24+(INRRSE8_TLDEC_22+INRRSE8_TLDEC_24)*TL_RSE8,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_RSE8982 + DO_INR_RSE8992)*(-1);
INCRSE8_NET2 = max(0,(INRRSE8_NET2 + INRRSE8_NET_222 + INCRSE8_NET_A+(INRRSE8_TL_A+INRRSE8_TL_22_A+INRRSE8_TL_24_A)*(1-null(TL_RSE8_A-TL_RSE8))*positif(TL_RSE8))) * positif(RSE8BASE_INR +NRSE8BASE)* (1-IND_RJLJ);
RSE8_PRI2=RSE8_R9901;
RSE8_ANT2=RSE8BASE_INR_A;
RSE8_NTL2=RSE8_NTLDEC;
RSE8_NTL_222=RSE8_NTLDEC_22;
RSE8_TL2=RSE8_TLDEC;
RSE8_TL_222=RSE8_TLDEC_22;
RSE8_REF_INR=RSE8_REF;
INRCVN_R99RA = INRCVN_R99R_A;
INRCVN_R99R = arr((CVN_R99R) * (TXINR_PA/100)-INCCVN_NET_A) * positif(CVN_R99R-CVN_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRCVN_R9901A = INRCVN_R9901_A;
INRCVN_R9901 = arr(CVN_R9901 * (TXINR_PA/100)-INCCVN_NET_A) * positif(CVN_R9901- CVN_R9901_A)
              * positif(IND_PASSAGE-1) * positif(CVN_TLDEC-CVN_R9901) * positif(CVN_R9901_A)
             + (arr(CVN_R9901 * (TXINR_PA/100))-INCCVN_NET_A) * positif(CVN_R9901- CVNBASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(CVN_TLDEC-CVN_R9901) * (1-positif(CVN_R9901_A))
             + (INCCVN_NET_A - arr(CVN_R9901 * (TXINR_PA/100))) * positif(CVNBASE_INR_A- CVN_R9901)
              * positif(IND_PASSAGE-1) * positif(CVN_TLDEC-CVN_R9901) * (1-positif(CVN_R9901_A)) * positif(CVN_R9901)
	     ;
DO_INR_CVNC=DO_INR_CVN_A;
INR_NTL_GLOB_CVN2 = INRCVN_NTLDECD + INRCVN_NTL_A+ INRCVN_NTLDECD_22 + INRCVN_NTL_22_A+INRCVN_NTLDECD_24 + INRCVN_NTL_24_A;
INR_TL_GLOB_CVN2 = INRCVN_TLDECD + INRCVN_TL_A+ INRCVN_TLDEC_22 + INRCVN_TL_22_A+INRCVN_TLDEC_24 + INRCVN_TL_24_A;
INR_TOT_GLOB_CVN2 = max(0,INR_NTL_GLOB_CVN2 + INR_TL_GLOB_CVN2*TL_CVN+INRCVN_R99R+INRCVN_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_CVNC= (INRCVN_NTLDECD+ INRCVN_NTL_A+ (INRCVN_TLDECD + INRCVN_TL_A)*TL_CVN +INRCVN_R99R+INRCVN_R99R_A) * (1-IND_RJLJ) ;
DO_INR_CVN2 = (1-null(CVN_REF_A-min(CVN_REF,max(CVN_TLDEC_22,CVN_TLDEC_24)))) * max(0,
           (arr(((INRCVN_TL_A+INRCVN_TL_22_A+INRCVN_TL_24_A)*TL_CVN_A*TL_CVN + INRCVN_NTL_A+INRCVN_NTL_22_A+INRCVN_NTL_24_A)
           * min(1,((CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24)/(CVN_REF-max(0,CVN_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24)* positif(CVN_REF - (max(0,CVN_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(CVN_TLDEC_22 +CVN_TLDEC_24- CVNBASE_INR_A))
           +arr(((INRCVN_TL_A+INRCVN_TL_22_A+INRCVN_TL_24_A)*TL_CVN_A*TL_CVN)
             * min(1,((CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24)/(CVN_REF-max(0,CVN_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24)* positif(CVN_REF - (max(0,CVN_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(CVN_TLDEC_22 +CVN_TLDEC_24- CVNBASE_INR_A))
             * (1-positif(INRCVN_NTL_A+INRCVN_NTL_22_A+INRCVN_NTL_24_A))
          + (INRCVN_NTL_A*FLAG_C02+(INRCVN_NTL_22_A+INRCVN_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24)* positif(CVN_REF - (max(0,CVN_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRCVN_NTL_A)*positif(INRCVN_NTL_22_A+INRCVN_NTL_24_A)
          + arr((INRCVN_NTL_A*FLAG_C02+(INRCVN_NTL_22_A+INRCVN_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24)* positif(CVN_REF - (max(0,CVN_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24)/(CVN_REF-max(0,CVN_R9901)))))
             * (1-positif(INRCVN_NTL_A)*positif(INRCVN_NTL_22_A+INRCVN_NTL_24_A))
          + ((INRCVN_TL_A+INRCVN_TL_22_A+INRCVN_TL_24_A)*null(TL_CVN) * TL_CVN_A
          * (1- FLAG_DEFAUT)
             *positif(CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24)* positif(CVN_REF - (max(0,CVN_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRCVN_TL_A + INRCVN_R99R_A+INRCVN_NTL_A - max(0,arr(CVN_TLDEC * TXINR_PA/100))) * positif(CVN_R99R - CVN_TLDEC)
         + (INRCVN_R99R_A+INRCVN_NTL_A - max(0,arr(CVN_R99R * TXINR_PA/100))) * positif(CVN_TLDEC-(CVN_R99R))
         + (INRCVN_TL_A + INRCVN_R99R_A+INRCVN_NTL_A - max(0,arr(CVN_R99R * TXINR_PA/100))) * null(CVN_TLDEC-(CVN_R99R)))
	               +          (1-positif(SOM9YI))* (
          arr((INRCVN_TL_A*TL_CVN_A *TL_CVN+(INRCVN_NTL_A +INRCVN_R99R+INRCVN_R9901-INRCVN_RETDEF) 
                       * ((CVN_REF - CVN_TLDEC)/(CVN_REF-max(0,CVN_REF_A)))))
                       * positif(CVN_REF - CVN_TLDEC)  * positif(CVN_TLDEC - CVN_R99R) 
                       * positif(INRCVN_R99R_A+INRCVN_R9901_A+0)
         + (INR_TOT_GLOB_CVNC - DO_INR_CVN_A - arr(CVN_TLDEC * TXINR_PA/100))
                       * positif(CVN_REF - CVN_TLDEC)
		       )) * (1-positif(FLAG_RETARD22))
                   +              (positif(SOM9YI)* (
             (INRCVN_TL_22_A + INRCVN_R99R_A+INRCVN_NTL_22_A - max(0,arr(CVN_TLDEC_22 * TXINR_PA22/100))) * positif(CVN_R9901 - max(CVN_TLDEC_24,CVN_TLDEC_22))
              + (INRCVN_R99R_A+INRCVN_NTL_22_A - max(0,arr(CVN_R99R * TXINR_PA22/100))) * positif(max(CVN_TLDEC_24,CVN_TLDEC_22)-(CVN_R9901))
              + (INRCVN_TL_22_A + INRCVN_R99R_A+INRCVN_NTL_22_A - max(0,arr(CVN_R99R * TXINR_PA22/100))) * null(max(CVN_TLDEC_24,CVN_TLDEC_22)-(CVN_R9901)))
                            +          (1-positif(SOM9YI))* (
                arr((INRCVN_TL_22_A*TL_CVN_A *TL_CVN+(INRCVN_NTL_22_A +INRCVN_R99R+INRCVN_R9901-INRCVN_RETDEF)
                       * ((CVN_REF - CVN_TLDEC_22)/(CVN_REF-max(0,CVN_REF_A)))))
                             * positif(CVN_REF - max(CVN_TLDEC_24,CVN_TLDEC_22))  * positif(max(CVN_TLDEC_24,CVN_TLDEC_22) - CVN_R9901)
                           * positif(INRCVN_R99R_A+INRCVN_R9901_A+0)
            + (INRCVN_TL_22_A+INRCVN_NTL_22_A - DO_INR_CVN_A - arr(CVN_TLDEC_22 * TXINR_PA22/100))
                          * positif(CVN_REF - max(CVN_TLDEC_24,CVN_TLDEC_22))
                        )) * positif(FLAG_RETARD22)
             ));

RECUP_INR_CVN = max(0,(min(max(0,DO_INR_CVN_A-RECUP_INR_CVN_A),arr(max(0,DO_INR_CVN_A-RECUP_INR_CVN_A) * (CVN_TLDEC - CVNBASE_INR_A)/DO_CVN_A))
                    *positif(CVN_TLDEC-CVNBASE_INR_A)*positif(CVN_REF-CVNBASE_INR_A)
                    * positif(CVN_PA - CVN_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_CVN_A-RECUP_INR_CVN_A),arr((CVN_R99R - CVNBASE_INR_A) * TXINR_PA/100))*positif(CVN_TLDEC - CVNBASE_INR_A)
                    * (1-positif(CVN_PA - CVN_TLDEC))
                    *positif(max(0,DO_INR_CVN_A-RECUP_INR_CVN_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_CVN2 = (CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24) * positif(CVN_REF - CVN_TLDEC_22-CVN_TLDEC_24)* positif(CVNBASE_INR_A);
SUP_CVN_MAX2 = (CVN_REF - max(0,CVN_R9901)) * positif(CVN_REF - max(0,CVN_R9901))* positif(CVNBASE_INR_A);
DO_INR_CVN982 = max(0,
          arr((CVN_REF - CVN_NTLDEC_198) * TXINRRED_A/100) 
            *positif(CVN_REF - CVN_NTLDEC_198))*(1-positif(DO_INR_CVN2)) * present(CVN_NTLDEC_198);
DO_INR_CVN992 = max(0,
          arr((CVN_REF - CVN_TLDEC_199) * TXINRRED_A/100)
            *positif(CVN_REF - CVN_TLDEC_199))*(1-positif(DO_INR_CVN2)) * present(CVN_TLDEC_199);
INR_CVN_TOT = max(INRCVN_NTLDECD+INRCVN_NTLDECD_22 +INRCVN_NTLDECD_24+ (INRCVN_TLDECD+INRCVN_TLDEC_22+INRCVN_TLDEC_24)*TL_CVN-INR_CVN_TARDIF*null(1-IND_PASSAGE)+INRCVN_R99R+RECUP_INR_CVN,0) 
	      * (1-IND_RJLJ);
INRCVN_RECT= arr((CVN_RECT )* (TXINR_PA/100)) * positif(CVN_RECT) * FLAG_RECTIF;
INCCVN_TL2 = INRCVN_TLDECD;
INCCVN_TL_222 = INRCVN_TLDEC_22;
INCVN_TL2 = INRCVN_TLA * TL_CVN;
INCVN_TL_222 = INRCVN_TLA_22 * TL_CVN;
INRCVN_NET2 = max(INRCVN_NTLDECD+INRCVN_TLDECD*TL_CVN+INRCVN_R99R+RECUP_INR_CVN,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_CVN2 * (-1);
INRCVN_NET_222 = max(INRCVN_NTLDECD_22+INRCVN_NTLDECD_24+(INRCVN_TLDEC_22+INRCVN_TLDEC_24)*TL_CVN,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_CVN982 + DO_INR_CVN992)*(-1);
INCCVN_NET2 = max(0,(INRCVN_NET2 + INRCVN_NET_222 + INCCVN_NET_A+(INRCVN_TL_A+INRCVN_TL_22_A+INRCVN_TL_24_A)*(1-null(TL_CVN_A-TL_CVN))*positif(TL_CVN))) * positif(CVNBASE_INR +NCVNBASE)* (1-IND_RJLJ);
CVN_PRI2=CVN_R9901;
CVN_ANT2=CVNBASE_INR_A;
CVN_NTL2=CVN_NTLDEC;
CVN_NTL_222=CVN_NTLDEC_22;
CVN_TL2=CVN_TLDEC;
CVN_TL_222=CVN_TLDEC_22;
CVN_REF_INR=CVN_REF;
INRGLO_R99RA = INRGLO_R99R_A;
INRGLO_R99R = arr((GLO_R99R-COD8YL) * (TXINR_PA/100)-INCGLO_NET_A) * positif(GLO_R99R-GLO_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRGLO_R9901A = INRGLO_R9901_A;
INRGLO_R9901 = arr(GLO_R9901 * (TXINR_PA/100)-INCGLO_NET_A) * positif(GLO_R9901- GLO_R9901_A)
              * positif(IND_PASSAGE-1) * positif(GLO_TLDEC-GLO_R9901) * positif(GLO_R9901_A)
             + (arr(GLO_R9901 * (TXINR_PA/100))-INCGLO_NET_A) * positif(GLO_R9901- GLOBASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(GLO_TLDEC-GLO_R9901) * (1-positif(GLO_R9901_A))
             + (INCGLO_NET_A - arr(GLO_R9901 * (TXINR_PA/100))) * positif(GLOBASE_INR_A- GLO_R9901)
              * positif(IND_PASSAGE-1) * positif(GLO_TLDEC-GLO_R9901) * (1-positif(GLO_R9901_A)) * positif(GLO_R9901)
	     ;
DO_INR_GLOC=DO_INR_GLO_A;
INR_NTL_GLOB_GLO2 = INRGLO_NTLDECD + INRGLO_NTL_A+ INRGLO_NTLDECD_22 + INRGLO_NTL_22_A+INRGLO_NTLDECD_24 + INRGLO_NTL_24_A;
INR_TL_GLOB_GLO2 = INRGLO_TLDECD + INRGLO_TL_A+ INRGLO_TLDEC_22 + INRGLO_TL_22_A+INRGLO_TLDEC_24 + INRGLO_TL_24_A;
INR_TOT_GLOB_GLO2 = max(0,INR_NTL_GLOB_GLO2 + INR_TL_GLOB_GLO2*TL_GLO+INRGLO_R99R+INRGLO_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_GLOC= (INRGLO_NTLDECD+ INRGLO_NTL_A+ (INRGLO_TLDECD + INRGLO_TL_A)*TL_GLO +INRGLO_R99R+INRGLO_R99R_A) * (1-IND_RJLJ) ;
DO_INR_GLO2 = (1-null(GLO_REF_A-min(GLO_REF,max(GLO_TLDEC_22,GLO_TLDEC_24)))) * max(0,
           (arr(((INRGLO_TL_A+INRGLO_TL_22_A+INRGLO_TL_24_A)*TL_GLO_A*TL_GLO + INRGLO_NTL_A+INRGLO_NTL_22_A+INRGLO_NTL_24_A)
           * min(1,((GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24)/(GLO_REF-max(0,GLO_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24)* positif(GLO_REF - (max(0,GLO_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(GLO_TLDEC_22 +GLO_TLDEC_24- GLOBASE_INR_A))
           +arr(((INRGLO_TL_A+INRGLO_TL_22_A+INRGLO_TL_24_A)*TL_GLO_A*TL_GLO)
             * min(1,((GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24)/(GLO_REF-max(0,GLO_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24)* positif(GLO_REF - (max(0,GLO_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(GLO_TLDEC_22 +GLO_TLDEC_24- GLOBASE_INR_A))
             * (1-positif(INRGLO_NTL_A+INRGLO_NTL_22_A+INRGLO_NTL_24_A))
          + (INRGLO_NTL_A*FLAG_C02+(INRGLO_NTL_22_A+INRGLO_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24)* positif(GLO_REF - (max(0,GLO_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRGLO_NTL_A)*positif(INRGLO_NTL_22_A+INRGLO_NTL_24_A)
          + arr((INRGLO_NTL_A*FLAG_C02+(INRGLO_NTL_22_A+INRGLO_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24)* positif(GLO_REF - (max(0,GLO_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24)/(GLO_REF-max(0,GLO_R9901)))))
             * (1-positif(INRGLO_NTL_A)*positif(INRGLO_NTL_22_A+INRGLO_NTL_24_A))
          + ((INRGLO_TL_A+INRGLO_TL_22_A+INRGLO_TL_24_A)*null(TL_GLO) * TL_GLO_A
          * (1- FLAG_DEFAUT)
             *positif(GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24)* positif(GLO_REF - (max(0,GLO_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRGLO_TL_A + INRGLO_R99R_A+INRGLO_NTL_A - max(0,arr(GLO_TLDEC * TXINR_PA/100))) * positif(GLO_R99R - GLO_TLDEC)
         + (INRGLO_R99R_A+INRGLO_NTL_A - max(0,arr(GLO_R99R * TXINR_PA/100))) * positif(GLO_TLDEC-(GLO_R99R))
         + (INRGLO_TL_A + INRGLO_R99R_A+INRGLO_NTL_A - max(0,arr(GLO_R99R * TXINR_PA/100))) * null(GLO_TLDEC-(GLO_R99R)))
              + (1-positif(SOM9YI)) * (
          arr((INRGLO_TL_A*TL_GLO_A *TL_GLO+(INRGLO_NTL_A +INRGLO_R99R+INRGLO_R9901-INRGLO_RETDEF) 
                       * ((GLO_REF - GLO_TLDEC)/(GLO_REF-max(0,GLO_REF_A)))))
                       * positif(GLO_REF - GLO_TLDEC)  * positif(GLO_TLDEC - GLO_R99R) 
                       * positif(INRGLO_R99R_A+INRGLO_R9901_A+0)
         + (INR_TOT_GLOB_GLOC - DO_INR_GLO_A - arr(GLO_TLDEC * TXINR_PA/100))
                       * positif(GLO_REF - GLO_TLDEC)
                       )) * (1-positif(FLAG_RETARD22))
                    +        (positif(SOM9YI)* (
              (INRGLO_TL_22_A + INRGLO_R99R_A+INRGLO_NTL_22_A - max(0,arr(GLO_TLDEC_22 * TXINR_PA22/100))) * positif(GLO_R9901 - max(GLO_TLDEC_24,GLO_TLDEC_22))
               + (INRGLO_R99R_A+INRGLO_NTL_22_A - max(0,arr(GLO_R99R * TXINR_PA22/100))) * positif(max(GLO_TLDEC_24,GLO_TLDEC_22)-(GLO_R9901))
             + (INRGLO_TL_22_A + INRGLO_R99R_A+INRGLO_NTL_22_A - max(0,arr(GLO_R99R * TXINR_PA22/100))) * null(max(GLO_TLDEC_24,GLO_TLDEC_22)-(GLO_R9901)))
              + (1-positif(SOM9YI)) * (
                arr((INRGLO_TL_22_A*TL_GLO_A *TL_GLO+(INRGLO_NTL_22_A +INRGLO_R99R+INRGLO_R9901-INRGLO_RETDEF)
                      * ((GLO_REF - GLO_TLDEC_22)/(GLO_REF-max(0,GLO_REF_A)))))
                              * positif(GLO_REF - max(GLO_TLDEC_24,GLO_TLDEC_22))  * positif(max(GLO_TLDEC_24,GLO_TLDEC_22) - GLO_R9901)
                         * positif(INRGLO_R99R_A+INRGLO_R9901_A+0)
        + (INRGLO_TL_22_A+INRGLO_NTL_22_A - DO_INR_GLO_A - arr(GLO_TLDEC_22 * TXINR_PA22/100))
                          * positif(GLO_REF - max(GLO_TLDEC_24,GLO_TLDEC_22))
                      )) * positif(FLAG_RETARD22)
             ));

RECUP_INR_GLO = max(0,(min(max(0,DO_INR_GLO_A-RECUP_INR_GLO_A),arr(max(0,DO_INR_GLO_A-RECUP_INR_GLO_A) * (GLO_TLDEC - GLOBASE_INR_A)/DO_GLO_A))
                    *positif(GLO_TLDEC-GLOBASE_INR_A)*positif(GLO_REF-GLOBASE_INR_A)
                    * positif(GLO_PA - GLO_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_GLO_A-RECUP_INR_GLO_A),arr((GLO_R99R - GLOBASE_INR_A) * TXINR_PA/100))*positif(GLO_TLDEC - GLOBASE_INR_A)
                    * (1-positif(GLO_PA - GLO_TLDEC))
                    *positif(max(0,DO_INR_GLO_A-RECUP_INR_GLO_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_GLO2 = (GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24) * positif(GLO_REF - GLO_TLDEC_22-GLO_TLDEC_24)* positif(GLOBASE_INR_A);
SUP_GLO_MAX2 = (GLO_REF - max(0,GLO_R9901)) * positif(GLO_REF - max(0,GLO_R9901))* positif(GLOBASE_INR_A);
DO_INR_GLO982 = max(0,
          arr((GLO_REF - GLO_NTLDEC_198) * TXINRRED_A/100) 
            *positif(GLO_REF - GLO_NTLDEC_198))*(1-positif(DO_INR_GLO2)) * present(GLO_NTLDEC_198);
DO_INR_GLO992 = max(0,
          arr((GLO_REF - GLO_TLDEC_199) * TXINRRED_A/100)
            *positif(GLO_REF - GLO_TLDEC_199))*(1-positif(DO_INR_GLO2)) * present(GLO_TLDEC_199);
INR_GLO_TOT = max(INRGLO_NTLDECD+INRGLO_NTLDECD_22 +INRGLO_NTLDECD_24+ (INRGLO_TLDECD+INRGLO_TLDEC_22+INRGLO_TLDEC_24)*TL_GLO-INR_GLO_TARDIF*null(1-IND_PASSAGE)+INRGLO_R99R+RECUP_INR_GLO,0) 
	      * (1-IND_RJLJ);
INRGLO_RECT= arr((GLO_RECT-COD8YL )* (TXINR_PA/100)) * positif(GLO_RECT) * FLAG_RECTIF;
INCGLO_TL2 = INRGLO_TLDECD;
INCGLO_TL_222 = INRGLO_TLDEC_22;
INGLO_TL2 = INRGLO_TLA * TL_GLO;
INGLO_TL_222 = INRGLO_TLA_22 * TL_GLO;
INRGLO_NET2 = max(INRGLO_NTLDECD+INRGLO_TLDECD*TL_GLO+INRGLO_R99R+RECUP_INR_GLO,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_GLO2 * (-1);
INRGLO_NET_222 = max(INRGLO_NTLDECD_22+INRGLO_NTLDECD_24+(INRGLO_TLDEC_22+INRGLO_TLDEC_24)*TL_GLO,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_GLO982 + DO_INR_GLO992)*(-1);
INCGLO_NET2 = max(0,(INRGLO_NET2 + INRGLO_NET_222 + INCGLO_NET_A+(INRGLO_TL_A+INRGLO_TL_22_A+INRGLO_TL_24_A)*(1-null(TL_GLO_A-TL_GLO))*positif(TL_GLO))) * positif(GLOBASE_INR+NGLOBASE)* (1-IND_RJLJ);
GLO_PRI2=GLO_R9901;
GLO_ANT2=GLOBASE_INR_A;
GLO_NTL2=GLO_NTLDEC;
GLO_NTL_222=GLO_NTLDEC_22;
GLO_TL2=GLO_TLDEC;
GLO_TL_222=GLO_TLDEC_22;
GLO_REF_INR=GLO_REF;
INRC820_R99RA = INRC820_R99R_A;
INRC820_R99R = arr((C820_R99R-COD8YL) * (TXINR_PA/100)-INCC820_NET_A) * positif(C820_R99R-C820_R99R_A)*positif(IND_PASSAGE-1) * FLAG_RETARD0718;
INRC820_R9901A = INRC820_R9901_A;
INRC820_R9901 = arr(C820_R9901 * (TXINR_PA/100)-INCC820_NET_A) * positif(C820_R9901- C820_R9901_A)
              * positif(IND_PASSAGE-1) * positif(C820_TLDEC-C820_R9901) * positif(C820_R9901_A)
             + (arr(C820_R9901 * (TXINR_PA/100))-INCC820_NET_A) * positif(C820_R9901- C820BASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(C820_TLDEC-C820_R9901) * (1-positif(C820_R9901_A))
             + (INCC820_NET_A - arr(C820_R9901 * (TXINR_PA/100))) * positif(C820BASE_INR_A- C820_R9901)
              * positif(IND_PASSAGE-1) * positif(C820_TLDEC-C820_R9901) * (1-positif(C820_R9901_A)) * positif(C820_R9901)
	     ;
DO_INR_C820C=DO_INR_C820_A;
INR_NTL_GLOB_C8202 = INRC820_NTLDECD + INRC820_NTL_A+ INRC820_NTLDECD_22 + INRC820_NTL_22_A+INRC820_NTLDECD_24 + INRC820_NTL_24_A;
INR_TL_GLOB_C8202 = INRC820_TLDECD + INRC820_TL_A+ INRC820_TLDEC_22 + INRC820_TL_22_A+INRC820_TLDEC_24 + INRC820_TL_24_A;
INR_TOT_GLOB_C8202 = max(0,INR_NTL_GLOB_C8202 + INR_TL_GLOB_C8202*TL_MCSG820+INRC820_R99R+INRC820_R99R_A) * (1-IND_RJLJ);
INR_TOT_GLOB_C820C= (INRC820_NTLDECD+ INRC820_NTL_A+ (INRC820_TLDECD + INRC820_TL_A)*TL_MCSG820 +INRC820_R99R+INRC820_R99R_A) * (1-IND_RJLJ) ;

DO_INR_C8202 = (1-null(C820_REF_A-min(C820_REF,max(C820_TLDEC_22,C820_TLDEC_24)))) * max(0,
           (arr(((INRC820_TL_A+INRC820_TL_22_A+INRC820_TL_24_A)*TL_MCSG820_A*TL_MCSG820 + INRC820_NTL_A+INRC820_NTL_22_A+INRC820_NTL_24_A)
           * min(1,((C820_REF - C820_TLDEC_22-C820_TLDEC_24)/(C820_REF-max(0,C820_R9901))))) 
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(C820_REF - C820_TLDEC_22-C820_TLDEC_24)* positif(C820_REF - (max(0,C820_R9901))))
              * (1-positif(FLAG_C02+FLAG_C22))
	     *(1-positif_ou_nul(C820_TLDEC_22 - C820BASE_INR_A))
           +arr(((INRC820_TL_A+INRC820_TL_22_A+INRC820_TL_24_A)*TL_MCSG820_A*TL_MCSG820)
             * min(1,((C820_REF - C820_TLDEC_22-C820_TLDEC_24)/(C820_REF-max(0,C820_R9901)))))
             * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             * positif(C820_REF - C820_TLDEC_22-C820_TLDEC_24)* positif(C820_REF - (max(0,C820_R9901)))
             * positif(FLAG_C02+FLAG_C22)
             *(1-positif_ou_nul(C820_TLDEC_22 - C820BASE_INR_A))
             * (1-positif(INRC820_NTL_A+INRC820_NTL_22_A+INRC820_NTL_24_A))
          + (INRC820_NTL_A*FLAG_C02+(INRC820_NTL_22_A+INRC820_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(C820_REF - C820_TLDEC_22-C820_TLDEC_24)* positif(C820_REF - (max(0,C820_R9901))) * positif(FLAG_C02+FLAG_C22)
             * positif(INRC820_NTL_A)*positif(INRC820_NTL_22_A+INRC820_NTL_24_A)
          + arr((INRC820_NTL_A*FLAG_C02+(INRC820_NTL_22_A+INRC820_NTL_24_A)*FLAG_C22) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
             *positif(C820_REF - C820_TLDEC_22-C820_TLDEC_24)* positif(C820_REF - (max(0,C820_R9901))) * positif(FLAG_C02+FLAG_C22)
             * min(1,((C820_REF - C820_TLDEC_22-C820_TLDEC_24)/(C820_REF-max(0,C820_R9901)))))
             * (1-positif(INRC820_NTL_A)*positif(INRC820_NTL_22_A+INRC820_NTL_24_A))
          + ((INRC820_TL_A+INRC820_TL_22_A+INRC820_TL_24_A)*null(TL_MCSG820) * TL_MCSG820_A
          * (1- FLAG_DEFAUT)
             *positif(C820_REF - C820_TLDEC_22-C820_TLDEC_24)* positif(C820_REF - (max(0,C820_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
	                         (positif(SOM9YI)* (
          (INRC820_TL_A + INRC820_R99R_A+INRC820_NTL_A - max(0,arr(C820_TLDEC * TXINR_PA/100))) * positif(C820_R99R - C820_TLDEC)
         + (INRC820_R99R_A+INRC820_NTL_A - max(0,arr(C820_R99R * TXINR_PA/100))) * positif(C820_TLDEC-(C820_R99R))
         + (INRC820_TL_A + INRC820_R99R_A+INRC820_NTL_A - max(0,arr(C820_R99R * TXINR_PA/100))) * null(C820_TLDEC-(C820_R99R)))
                      + (1-positif(SOM9YI+0)) * (
          arr((INRC820_TL_A*TL_MCSG820_A *TL_MCSG820+(INRC820_NTL_A +INRC820_R99R+INRC820_R9901-INRC820_RETDEF) 
                       * ((C820_REF - C820_TLDEC)/(C820_REF-max(0,C820_REF_A)))))
                       * positif(C820_REF - C820_TLDEC)  * positif(C820_TLDEC - C820_R99R) 
                       * positif(INRC820_R99R_A+INRC820_R9901_A+0)
         + (INR_TOT_GLOB_C820C - DO_INR_C820_A - arr(C820_TLDEC * TXINR_PA/100))
                       * positif(C820_REF - C820_TLDEC)
                       )) * (1-positif(FLAG_RETARD22))
                      +      (positif(SOM9YI)* (
                 (INRC820_TL_22_A + INRC820_R99R_A+INRC820_NTL_22_A - max(0,arr(C820_TLDEC_22 * TXINR_PA22/100))) * positif(C820_R9901 - max(C820_TLDEC_24,C820_TLDEC_22))
          + (INRC820_R99R_A+INRC820_NTL_22_A - max(0,arr(C820_R99R * TXINR_PA22/100))) * positif(max(C820_TLDEC_24,C820_TLDEC_22)-(C820_R9901))
           + (INRC820_TL_22_A + INRC820_R99R_A+INRC820_NTL_22_A - max(0,arr(C820_R99R * TXINR_PA22/100))) * null(max(C820_TLDEC_24,C820_TLDEC_22)-(C820_R9901)))
                         + (1-positif(SOM9YI+0)) * (
           arr((INRC820_TL_22_A*TL_MCSG820_A *TL_MCSG820+(INRC820_NTL_22_A +INRC820_R99R+INRC820_R9901-INRC820_RETDEF)
                         * ((C820_REF - C820_TLDEC_22)/(C820_REF-max(0,C820_REF_A)))))
                         * positif(C820_REF - max(C820_TLDEC_24,C820_TLDEC_22))  * positif(max(C820_TLDEC_24,C820_TLDEC_22) - C820_R9901)
                        * positif(INRC820_R99R_A+INRC820_R9901_A+0)
               + (INRC820_TL_22_A+INRC820_NTL_22_A - DO_INR_C820_A - arr(C820_TLDEC_22 * TXINR_PA22/100))
                            * positif(C820_REF - max(C820_TLDEC_24,C820_TLDEC_22))
                       )) * positif(FLAG_RETARD22)
                ));

RECUP_INR_C820 = max(0,(min(max(0,DO_INR_C820_A-RECUP_INR_C820_A),arr(max(0,DO_INR_C820_A-RECUP_INR_C820_A) * (C820_TLDEC - C820BASE_INR_A)/DO_C820_A))
                    *positif(C820_TLDEC-C820BASE_INR_A)*positif(C820_REF-C820BASE_INR_A)
                    * positif(C820_PA - C820_TLDEC))
                    *positif(FLAG_RETARD + FLAG_DEFAUT)
		+ min(max(0,DO_INR_C820_A-RECUP_INR_C820_A),arr((C820_R99R - C820BASE_INR_A) * TXINR_PA/100))*positif(C820_TLDEC - C820BASE_INR_A)
                    * (1-positif(C820_PA - C820_TLDEC))
                    *positif(max(0,DO_INR_C820_A-RECUP_INR_C820_A))
		    *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_C8202 = (C820_REF - C820_TLDEC_22-C820_TLDEC_24) * positif(C820_REF - C820_TLDEC_22-C820_TLDEC_24)* positif(C820BASE_INR_A);
SUP_C820_MAX2 = (C820_REF - max(0,C820_R9901)) * positif(C820_REF - max(0,C820_R9901))* positif(C820BASE_INR_A);
DO_INR_C820982 = max(0,
          arr((C820_REF - C820_NTLDEC_198) * TXINRRED_A/100) 
            *positif(C820_REF - C820_NTLDEC_198))*(1-positif(DO_INR_C8202)) * present(C820_NTLDEC_198);
DO_INR_C820992 = max(0,
          arr((C820_REF - C820_TLDEC_199) * TXINRRED_A/100)
            *positif(C820_REF - C820_TLDEC_199))*(1-positif(DO_INR_C8202)) * present(C820_TLDEC_199);
INR_C820_TOT = max(INRC820_NTLDECD+INRC820_NTLDECD_22 +INRC820_NTLDECD_24+ (INRC820_TLDECD+INRC820_TLDEC_22+INRC820_TLDEC_24)*TL_MCSG820-INR_C820_TARDIF*null(1-IND_PASSAGE)+INRC820_R99R+RECUP_INR_C820,0) 
	      * (1-IND_RJLJ);
INRC820_RECT= arr((C820_RECT-COD8YL )* (TXINR_PA/100)) * positif(C820_RECT) * FLAG_RECTIF;
INCC820_TL2 = INRC820_TLDECD;
INCC820_TL_222 = INRC820_TLDEC_22;
INC820_TL2 = INRC820_TLA * TL_MCSG820;
INC820_TL_222 = INRC820_TLA_22 * TL_MCSG820;
INRC820_NET2 = max(INRC820_NTLDECD+INRC820_TLDECD*TL_MCSG820+INRC820_R99R+RECUP_INR_C820,0)*(1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_C8202 * (-1);
INRC820_NET_222 = max(INRC820_NTLDECD_22+INRC820_NTLDECD_24+(INRC820_TLDEC_22+INRC820_TLDEC_24)*TL_MCSG820,0)*(1-IND_RJLJ)*(1-FLAG_NINR)+ (DO_INR_C820982 + DO_INR_C820992)*(-1);
INCC820_NET2 = max(0,(INRC820_NET2 + INRC820_NET_222 + INCC820_NET_A+(INRC820_TL_A+INRC820_TL_22_A+INRC820_TL_24_A)*(1-null(TL_MCSG820_A-TL_MCSG820))*positif(TL_MCSG820))) * positif(C820BASE_INR+NC820BASE)* (1-IND_RJLJ);
C820_PRI2=C820_R9901;
C820_ANT2=C820BASE_INR_A;
C820_NTL2=C820_NTLDEC;
C820_NTL_222=C820_NTLDEC_22;
C820_TL2=C820_TLDEC;
C820_TL_222=C820_TLDEC_22;
C820_REF_INR=C820_REF;
INRIFI_R99RA = INRIFI_R99R_A;
INRIFI_R99R = arr(IFI_R99R * (TXINRISF_PA/100)-INCIFI_NET_A) * positif(IFI_R99R-IFI_R99R_A)*positif(IND_PASSAGE-1);
INRIFI_R9901A = INRIFI_R9901_A;
INRIFI_R9901 = arr(IFI_R9901 * (TXINRISF_PA/100)-INCIFI_NET_A) * positif(IFI_R9901- IFI_R9901_A)
              * positif(IND_PASSAGE-1) * positif(IFI_NTLDEC_22+IFI_NTLDEC_24-IFI_R9901) * positif(IFI_R9901_A)
             + (arr(IFI_R9901 * (TXINRISF_PA/100))-INCIFI_NET_A) * positif(IFI_R9901- IFIBASE_INR_A)
              * positif(IND_PASSAGE-1) * positif(IFI_NTLDEC_22+IFI_NTLDEC_24-IFI_R9901) * (1-positif(IFI_R9901_A))
             + (INCIFI_NET_A - arr(IFI_R9901 * (TXINRISF_PA/100))) * positif(IFIBASE_INR_A- IFI_R9901)
              * positif(IND_PASSAGE-1) * positif(IFI_NTLDEC_22+IFI_NTLDEC_24-IFI_R9901) * (1-positif(IFI_R9901_A)) * positif(IFI_R9901)
	     ;
DO_INR_IFIC=DO_INR_IFI_A;
INR_NTL_GLOB_IFI2 = INRIFI_NTLDECD+ INRIFI_NTL_A+ INRIFI_NTLDECD_22 + INRIFI_NTLDECD_24 + INRIFI_NTL_22_A  + INRIFI_NTL_24_A;
INR_TL_GLOB_IFI2 = INRIFI_TLDECD + INRIFI_TL_A + INRIFI_TLDEC_22 + INRIFI_TLDEC_24 + INRIFI_TL_22_A + INRIFI_TL_24_A;
INR_TOT_GLOB_IFI2 = (INR_NTL_GLOB_IFI2 + INR_TL_GLOB_IFI2*TL_IFI+INRIFI_R99R+INRIFI_R99R_A) * (1-IND_RJLJ) ;
INR_TOT_GLOB_IFIC = (INRIFI_NTLDECD+ INRIFI_NTL_A+ (INRIFI_TLDECD + INRIFI_TL_A)*TL_IFI +INRIFI_R99R+INRIFI_R99R_A) * (1-IND_RJLJ) ;

DO_INR_IFI2 = (1-null(IFI_REF_A-min(IFI_REF,max(IFI_TLDEC_22,IFI_TLDEC_24)))) * max(0,
          arr(((INRIFI_TL_A+INRIFI_TL_22_A+INRIFI_TL_24_A)*TL_IFI_A *TL_IFI+ INRIFI_NTL_A+INRIFI_NTL_22_A+INRIFI_NTL_24_A)
              * min(1,((IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24)/(IFI_REF-max(0,IFI_R9901))))) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
                  *positif(IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24) * (1 - positif(FLAG_C02+FLAG_C22))
              *(1-positif_ou_nul(IFI_TLDEC_22 +IFI_TLDEC_24- IFIBASE_INR_A))
              + arr(((INRIFI_TL_A+INRIFI_TL_22_A+INRIFI_TL_24_A)*TL_IFI_A *TL_IFI)
                  * min(1,((IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24)/(IFI_REF-max(0,IFI_R9901))))) * (1-positif(FLAG_RETARD + FLAG_DEFAUT))
              *positif(IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24) * positif(FLAG_C02+FLAG_C22)
                  *(1-positif_ou_nul(IFI_TLDEC_22+IFI_TLDEC_24 - IFIBASE_INR_A))
              * (1-positif(INRIFI_NTL_A + INRIFI_NTL_22_A+INRIFI_NTL_24_A))
               + (INRIFI_NTL_A*FLAG_C02+(INRIFI_NTL_22_A+INRIFI_NTL_24_A)*FLAG_C22)* (1-positif(FLAG_RETARD + FLAG_DEFAUT))
                   *positif(IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24) * positif(FLAG_C02+FLAG_C22)
               *positif(INRIFI_NTL_A)*positif(INRIFI_NTL_22_A+INRIFI_NTL_24_A)
               + arr((INRIFI_NTL_A*FLAG_C02+(INRIFI_NTL_22_A+INRIFI_NTL_24_A)*FLAG_C22)* (1-positif(FLAG_RETARD + FLAG_DEFAUT))
            *positif(IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24) * positif(FLAG_C02+FLAG_C22)
                * min(1,((IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24)/(IFI_REF-max(0,IFI_R9901)))))
            * (1-positif(positif(INRIFI_NTL_A)*positif(INRIFI_NTL_22_A+INRIFI_NTL_24_A)))
             + ((INRIFI_TL_A+INRIFI_TL_22_A+INRIFI_TL_24_A)*null(TL_IFI) * TL_IFI_A
                 * (1- FLAG_DEFAUT)
              *positif(IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24)* positif(IFI_REF - (max(0,IFI_R9901))))
         + positif(FLAG_RETARD + FLAG_DEFAUT)*(
                                  (positif(SOM9YI)* (
            (INRIFI_TL_A + INRIFI_R99R_A+INRIFI_NTL_A - max(0,arr(IFI_TLDEC * TXINR_PA/100))) * positif(IFI_R99R - IFI_TLDEC)
             + (INRIFI_R99R_A+INRIFI_NTL_A - max(0,arr(IFI_R99R * TXINR_PA/100))) * positif(IFI_TLDEC-(IFI_R99R))
              + (INRIFI_TL_A + INRIFI_R99R_A+INRIFI_NTL_A - max(0,arr(IFI_R99R * TXINR_PA/100))) * null(IFI_TLDEC-(IFI_R99R)))
                            + (1-positif(SOM9YI+0)) * (
              arr((INRIFI_TL_A*TL_IFI_A *TL_IFI+(INRIFI_NTL_A +INRIFI_R99R+INRIFI_R9901-INRIFI_RETDEF)
                            * ((IFI_REF - IFI_TLDEC)/(IFI_REF-max(0,IFI_REF_A)))))
                            * positif(IFI_REF - IFI_TLDEC)  * positif(IFI_TLDEC - IFI_R99R)
                         * positif(INRIFI_R99R_A+INRIFI_R9901_A+0)
           +  (INR_TOT_GLOB_IFIC - DO_INR_IFI_A - arr(IFI_TLDEC * TXINR_PA/100))
                          * positif(IFI_REF - IFI_TLDEC)
                       * (1-positif(IFI4BASE-IFI_PA) * positif(IFI4BASE_A-IFI4BASE))
           + arr((IFI4BASE_A-IFI4BASE) * TXINR_PA/100) * positif(IFI4BASE-IFI_PA) * positif(IFI4BASE_A-IFI4BASE)
                         )) * (1-positif(FLAG_RETARD22))
                        +      (positif(SOM9YI)* (
             (INRIFI_TL_22_A + INRIFI_R99R_A+INRIFI_NTL_22_A - max(0,arr(IFI_TLDEC_22 * TXINR_PA22/100))) * positif(IFI_R9901 - max(IFI_TLDEC_24,IFI_TLDEC_22))
           + (INRIFI_R99R_A+INRIFI_NTL_22_A - max(0,arr(IFI_R99R * TXINR_PA22/100))) * positif(max(IFI_TLDEC_24,IFI_TLDEC_22)-(IFI_R9901))
           + (INRIFI_TL_22_A + INRIFI_R99R_A+INRIFI_NTL_22_A - max(0,arr(IFI_R99R * TXINR_PA22/100))) * null(max(IFI_TLDEC_24,IFI_TLDEC_22)-(IFI_R9901)))
                               + (1-positif(SOM9YI+0)) * (
                arr((INRIFI_TL_22_A*TL_IFI_A *TL_IFI+(INRIFI_NTL_22_A +INRIFI_R99R+INRIFI_R9901-INRIFI_RETDEF)
                           * ((IFI_REF - IFI_TLDEC_22)/(IFI_REF-max(0,IFI_REF_A)))))
                            * positif(IFI_REF - max(IFI_TLDEC_24,IFI_TLDEC_22))  * positif(max(IFI_TLDEC_24,IFI_TLDEC_22) - IFI_R9901)
                            * positif(INRIFI_R99R_A+INRIFI_R9901_A+0)
           + (INRIFI_TL_22_A+INRIFI_NTL_22_A - DO_INR_IFI_A - arr(IFI_TLDEC_22 * TXINR_PA22/100))
                            * positif(IFI_REF - max(IFI_TLDEC_24,IFI_TLDEC_22))
                       * (1-positif(IFI4BASE-IFI_PA) * positif(IFI4BASE_A-IFI4BASE))
           + arr((IFI4BASE_A-IFI4BASE) * TXINR_PA/100) * positif(IFI4BASE-IFI_PA) * positif(IFI4BASE_A-IFI4BASE)
                         )) * positif(FLAG_RETARD22)
                      ));

RECUP_INR_IFI = max(0,(min(max(0,DO_INR_IFI_A-RECUP_INR_IFI_A),arr(max(0,DO_INR_IFI_A-RECUP_INR_IFI_A) * (IFI_TLDEC - IFIBASE_INR_A)/DO_IFI_A))
                      *positif(IFI_TLDEC-IFIBASE_INR_A)*positif(IFI_REF-IFIBASE_INR_A)
                          * positif(IFI_PA - IFI_TLDEC))
                        *positif(FLAG_RETARD + FLAG_DEFAUT)
                    + min(max(0,DO_INR_IFI_A-RECUP_INR_IFI_A),arr((IFI_PA-SOM9YI - IFIBASE_INR_A) * TXINR_PA/100))
                        * (1-positif(IFI_PA - IFI_TLDEC))
                      * positif(IFI_TLDEC-IFIBASE_INR_A)
                            * positif(max(0,DO_INR_IFI_A-RECUP_INR_IFI_A))
                          *positif(FLAG_RETARD + FLAG_DEFAUT));
DO_IFI2 = (IFI_REF - IFI_NTLDEC_22-IFI_NTLDEC_24) * positif(IFI_REF - IFI_NTLDEC_22-IFI_NTLDEC_24)* positif(IFIBASE_INR_A);
SUP_IFI_MAX2 = (IFI_REF - max(0,IFI_R9901)) * positif(IFI_REF - max(0,IFI_R9901))* positif(IFIBASE_INR_A);
DO_IFI2 = (IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24) * positif(IFI_REF - IFI_TLDEC_22-IFI_TLDEC_24)* positif(IFIBASE_INR_A);
DO_INR_IFI982 = max(0,
          (arr((IFI_REF - IFI_NTLDEC_198) * TXINRRED22_A/100) + arr((IFI_REF - IFI_NTLDEC_198) * TXINRRED24_A/100))
             *positif(IFI_REF - IFI_NTLDEC_198))*(1-positif(DO_INR_IFI2)) * present(IFI_NTLDEC_198);
DO_INR_IFI992 = max(0,
                (arr((IFI_REF - IFI_TLDEC_199) * TXINRRED22_A/100) + arr((IFI_REF - IFI_TLDEC_199) * TXINRRED24_A/100))
            *positif(IFI_REF - IFI_TLDEC_199))*(1-positif(DO_INR_IFI2)) * present(IFI_TLDEC_199);
SUP_IFI_MAX2 = (IFI_REF - max(0,IFI_R9901)) * positif(IFI_REF - max(0,IFI_R9901))* positif(IFIBASE_INR_A);
INRIFI_RECT= arr(IFI_RECT * (TXINR_PA/100)) * positif(IFI_RECT) * FLAG_RECTIF;
INR_IFI_TOT = max(INRIFI_NTLDECD_22+INRIFI_NTLDECD_24+INRIFI_NTLDECD + (INRIFI_TLDECD+INRIFI_TLDEC_22+INRIFI_TLDEC_24)*TL_IFI-INR_IFI_TARDIF*null(1-IND_PASSAGE)+INRIFI_R99R+RECUP_INR_IFI,0)* (1-IND_RJLJ);
INCIFI_TL2 = INRIFI_TLDECD;
INCIFI_TL_222 = INRIFI_TLDEC_22;
INRIFI_NET2 = max(INRIFI_NTLDECD +INRIFI_TLDECD*TL_IFI+INRIFI_R99R+RECUP_INR_IFI,0)* (1-IND_RJLJ)* (1-FLAG_NINR)+DO_INR_IFI2 * (-1);
INRIFI_NET_222 = max(INRIFI_NTLDECD_22 +INRIFI_NTLDECD_24+(INRIFI_TLDEC_22+INRIFI_TLDEC_24)*TL_IFI,0)*(1-FLAG_NINR)* (1-IND_RJLJ) + (DO_INR_IFI982 + DO_INR_IFI992)*(-1);
INIFI_TL2 = INRIFI_TLA * TL_IFI;
INIFI_TL_222 = (INRIFI_TLA_22+INRIFI_TLA_24) * TL_IFI;
INCIFI_NET2 = max(0,(INRIFI_NET2 +INRIFI_NET_222
                      + (INCIFI_NET_A-(INR_IFI_TARDIF_A+INRIFI_RETDEF_A)*positif(INRIFI_NET2+INRIFI_NET_222-INR_IFI_TARDIF_A-INRIFI_RETDEF_A)
                                                                                  *positif(SOM9YI)*(1-positif(INDACOINR_A)))
                       + ((INRIFI_TL_A+INRIFI_TL_22_A+INRIFI_TL_24_A)*(1-null(TL_IFI_A-TL_IFI))*TL_IFI))) *positif(IFIBASE_INR)* (1-IND_RJLJ) ;
IFI_PRI2=IFI_R9901;
IFI_ANT2=IFIBASE_INR_A;
IFI_NTL2=IFI_NTLDEC;
IFI_TL2=IFI_TLDEC;
IFI_NTL_222=IFI_NTLDEC_22;
IFI_TL_222=IFI_TLDEC_22;
IFI_REF_INR=IFI_REF;


regle 1108001:
application :  iliad ;
NBMOIS = max(0,NBMOISI);
NBMOIS2 = max(0,NBMOISI+NBMOISI2);
TXINRISF = (max(0,NBMOISI2+NBMOISI) * TXMOISRETARD2) *null(2017 - V_ANREV)
        +  max(0,NBMOISI2 * TXMOISRETARD2) * positif(V_ANREV-2017);

TXINRREDISF = ((max(0,NBMOISI2+NBMOISI) * TXMOISRETARD2 *TXMOISRED)*(1-positif(V_FLAGR24)) + (max(0,NBMOISI2+NBMOISI) * TXMOISRETARD2 *TXMOISRED2)*(positif(V_FLAGR24))) * null(2017 - V_ANREV)
          + ((NBMOISI2 * TXMOISRETARD2 *TXMOISRED)*(1-positif(V_FLAGR24)) + (NBMOISI2 * TXMOISRETARD2 *TXMOISRED2)*(positif(V_FLAGR24))) * positif(V_ANREV - 2017);
regle 11081:
application : iliad ;
IND_PASSAGEISF = positif(FLAG_DEFAUT + FLAG_RETARD) + IND_PASSAGEISF_A;
IND_PASSR9901ISF = 1 + IND_PASSR9901ISF_A;
regle corrective 11090:
application : iliad ;


TINR2 = TXINR;
TINR_222=TXINRRED22 ;
TINR_242=TXINRRED24 ;
NBREMOIS222 = (NBMOISI + max(0,NBMOISI2))
		  * positif(INRIR_NET_222+INRCSG_NET_222+INRRDS_NET_222+INRPSOL_NET_222+INRTAXA_NET_222+INRCDIS_NET_222+INRIFI_NET_222
                          +INRCHR_NET_222+INRPCAP_NET_222+INRRSE1_NET_222+INRRSE2_NET_222+INRRSE3_NET_222+INRRSE4_NET_222
                          +INRRSE5_NET_222+INRRSE6_NET_222+INRRSE8_NET_222+INRCVN_NET_222+INRGLO_NET_222+INRC820_NET_222
		          +INRIR_NET_242+INRCSG_NET_242+INRRDS_NET_242+INRPSOL_NET_242+INRTAXA_NET_242+INRCDIS_NET_242+INRIFI_NET_242
                          +INRCHR_NET_242+INRPCAP_NET_242+INRRSE1_NET_242+INRRSE2_NET_242+INRRSE3_NET_242+INRRSE4_NET_242
                          +INRRSE5_NET_242+INRRSE6_NET_242+INRRSE8_NET_242+INRCVN_NET_242+INRGLO_NET_242+INRC820_NET_242
		  +INRIR_NET2+INRCSG_NET2 +INRRDS_NET2+INRPSOL_NET2+INRTAXA_NET2+INRCDIS_NET2+INRIFI_NET2
                          + INRCHR_NET2+INRPCAP_NET2+INRRSE1_NET2+INRRSE2_NET2+INRRSE3_NET2+INRRSE4_NET2
                          + INRRSE5_NET2+ INRRSE6_NET2+INRRSE8_NET2+INRCVN_NET2+INRGLO_NET2+INRC820_NET2
			    +null(TL_IR)*positif(INRIR_TLDECD+INRIR_TLDEC_22+INRIR_TLDEC_24)
			    +null(TL_TAXAGA)*positif(INRTAXA_TLDECD+INRTAXA_TLDEC_22+INRTAXA_TLDEC_24)
			    +null(TL_CDIS)*positif(INRCDIS_TLDECD+INRCDIS_TLDEC_22+INRCDIS_TLDEC_24)
			    +null(TL_RSE1)*positif(INRRSE1_TLDECD+INRRSE1_TLDEC_22+INRRSE1_TLDEC_24)
			    +null(TL_RSE2)*positif(INRRSE2_TLDECD+INRRSE2_TLDEC_22+INRRSE2_TLDEC_24)
			    +null(TL_RSE3)*positif(INRRSE3_TLDECD+INRRSE3_TLDEC_22+INRRSE3_TLDEC_24)
			    +null(TL_RSE4)*positif(INRRSE4_TLDECD+INRRSE4_TLDEC_22+INRRSE4_TLDEC_24)
			    +null(TL_RSE5)*positif(INRRSE5_TLDECD+INRRSE5_TLDEC_22+INRRSE5_TLDEC_24)
			    +null(TL_RSE6)*positif(INRRSE6_TLDECD+INRRSE6_TLDEC_22+INRRSE6_TLDEC_24)
			    +null(TL_RSE8)*positif(INRRSE8_TLDECD+INRRSE8_TLDEC_22+INRRSE8_TLDEC_24)
			    +null(TL_CAP)*positif(INRPCAP_TLDECD+INRPCAP_TLDEC_22+INRPCAP_TLDEC_24)
			    +null(TL_CVN)*positif(INRCVN_TLDECD+INRCVN_TLDEC_22+INRCVN_TLDEC_24)
			    +null(TL_GLO)*positif(INRGLO_TLDECD+INRGLO_TLDEC_22+INRGLO_TLDEC_24)
			    +null(TL_MCSG820)*positif(INRC820_TLDECD+INRC820_TLDEC_22+INRC820_TLDEC_24)
			    +null(TL_CS)*positif(INRCSG_TLDECD+INRCSG_TLDEC_22+INRCSG_TLDEC_24)
			    +null(TL_RD)*positif(INRCRDS_TLDECD+INRCRDS_TLDEC_22+INRCRDS_TLDEC_24)
			    +null(TL_PSOL)*positif(INRPSOL_TLDECD+INRPSOL_TLDEC_22+INRPSOL_TLDEC_24))
		  + NBREMOIS222_A * (1- positif_ou_nul(INRIR_NET_222+INRCSG_NET_222+INRRDS_NET_222+INRPSOL_NET_222+INRTAXA_NET_222
                          +INRCHR_NET_222+INRPCAP_NET_222+INRRSE1_NET_222+INRRSE2_NET_222+INRRSE3_NET_222+INRRSE4_NET_222+INRCDIS_NET_222
                          +INRRSE5_NET_222+INRRSE6_NET_222+INRRSE8_NET_222+INRCVN_NET_222+INRGLO_NET_222+INRC820_NET_222
		          +INRIR_NET_242+INRCSG_NET_242+INRRDS_NET_242+INRPSOL_NET_242+INRTAXA_NET_242
                          +INRCHR_NET_242+INRPCAP_NET_242+INRRSE1_NET_242+INRRSE2_NET_242+INRRSE3_NET_242+INRRSE4_NET_242+INRCDIS_NET_242
                          +INRRSE5_NET_242+INRRSE6_NET_242+INRRSE8_NET_242+INRCVN_NET_242+INRGLO_NET_242+INRC820_NET_242
                          +INRCHR_NET2+INRPCAP_NET2+INRRSE1_NET2+INRRSE2_NET2+INRRSE3_NET2+INRRSE4_NET2
		          +INRIR_NET2+INRCSG_NET2+INRRDS_NET2+INRPSOL_NET2+INRTAXA_NET2+INRCDIS_NET2+INRIFI_NET2
		          +INRRSE5_NET2+INRRSE6_NET2+INRRSE8_NET2+INRCVN_NET2+INRGLO_NET2+INRC820_NET2));
NBREMOISCS222 = (NBMOISI + max(0,NBMOISI2))
		  * positif(INRCSG_NET_222+INRRDS_NET_222+INRPSOL_NET_222
		          +INRCDIS_NET_222 + INRCVN_NET_222+INRGLO_NET_222+INRC820_NET_222
		          + INRRSE1_NET_222+INRRSE2_NET_222+INRRSE3_NET_222 + INRRSE4_NET_222+INRRSE5_NET_222+INRRSE6_NET_222+INRRSE8_NET_222
		          +INRCSG_NET_242+INRRDS_NET_242+INRPSOL_NET_242
		          +INRCDIS_NET_242 + INRCVN_NET_242+INRGLO_NET_242+INRC820_NET_242
		          + INRRSE1_NET_242+INRRSE2_NET_242+INRRSE3_NET_242 + INRRSE4_NET_242+INRRSE5_NET_242+INRRSE6_NET_242+INRRSE8_NET_242
		            +INRCSG_NET2 +INRRDS_NET2 +INRPSOL_NET2+INRCVN_NET2 +INRGLO_NET2 +INRC820_NET2
		            +INRRSE1_NET2 +INRRSE2_NET2 +INRRSE3_NET2 +INRRSE4_NET2+INRRSE5_NET2+INRRSE6_NET2+INRRSE8_NET2
			    +null(TL_CDIS)*positif(INRCDIS_TLDECD+INRCDIS_TLDEC_22+INRCDIS_TLDEC_24)
			    +null(TL_RSE1)*positif(INRRSE1_TLDECD+INRRSE1_TLDEC_22+INRRSE1_TLDEC_24)
			    +null(TL_RSE2)*positif(INRRSE2_TLDECD+INRRSE2_TLDEC_22+INRRSE2_TLDEC_24)
			    +null(TL_RSE3)*positif(INRRSE3_TLDECD+INRRSE3_TLDEC_22+INRRSE3_TLDEC_24)
			    +null(TL_RSE4)*positif(INRRSE4_TLDECD+INRRSE4_TLDEC_22+INRRSE4_TLDEC_24)
			    +null(TL_RSE5)*positif(INRRSE5_TLDECD+INRRSE5_TLDEC_22+INRRSE5_TLDEC_24)
			    +null(TL_RSE6)*positif(INRRSE6_TLDECD+INRRSE6_TLDEC_22+INRRSE6_TLDEC_24)
			    +null(TL_RSE8)*positif(INRRSE8_TLDECD+INRRSE8_TLDEC_22+INRRSE8_TLDEC_24)
			    +null(TL_CAP)*positif(INRPCAP_TLDECD+INRPCAP_TLDEC_22+INRPCAP_TLDEC_24)
			    +null(TL_CVN)*positif(INRCVN_TLDECD+INRCVN_TLDEC_22+INRCVN_TLDEC_24)
			    +null(TL_GLO)*positif(INRGLO_TLDECD+INRGLO_TLDEC_22+INRGLO_TLDEC_24)
			    +null(TL_MCSG820)*positif(INRC820_TLDECD+INRC820_TLDEC_22+INRC820_TLDEC_24)
			    +null(TL_CS)*positif(INRCSG_TLDECD+INRCSG_TLDEC_22+INRCSG_TLDEC_24)
			    +null(TL_RD)*positif(INRCRDS_TLDECD+INRCRDS_TLDEC_22+INRCRDS_TLDEC_24)
			    +null(TL_PSOL)*positif(INRPSOL_TLDECD+INRPSOL_TLDEC_22+INRPSOL_TLDEC_24))
		  + NBREMOISCS222_A * (1- positif_ou_nul(INRCSG_NET_222+INRRDS_NET_222+INRPSOL_NET_222
		                              +INRCDIS_NET_222 + INRCVN_NET_222+INRGLO_NET_222+INRC820_NET_222
                    		              +INRRSE1_NET_222+INRRSE2_NET_222+INRRSE3_NET_222 + INRRSE4_NET_222+INRRSE5_NET_222+INRRSE6_NET_222+INRRSE8_NET_222
		                              +INRCSG_NET_242+INRRDS_NET_242+INRPSOL_NET_242
		                              +INRCDIS_NET_242 + INRCVN_NET_242+INRGLO_NET_242+INRC820_NET_242
                    		              +INRRSE1_NET_242+INRRSE2_NET_242+INRRSE3_NET_242 + INRRSE4_NET_242+INRRSE5_NET_242+INRRSE6_NET_242+INRRSE8_NET_242
		                              +INRCSG_NET2 +INRRDS_NET2 +INRPSOL_NET2+INRCVN_NET2 +INRGLO_NET2 +INRC820_NET2
		                              +INRRSE1_NET2 +INRRSE2_NET2 +INRRSE3_NET2 +INRRSE4_NET2+INRRSE5_NET2+INRRSE6_NET2+INRRSE8_NET2));

INRTOT = INR_TOT_GLOB_IR+ INR_TOT_GLOB_CSG + INR_TOT_GLOB_CRDS + INR_TOT_GLOB_PS+INR_TOT_GLOB_PSOL
       +INR_TOT_GLOB_TAXA
       - DO_INR_IR - DO_INR_CSG - DO_INR_CRDS - DO_INR_PS -DO_INR_PSOL-DO_INR_TAXAGA;
INRTOT_NET = INCIR_TL + INCCS_TL + INCPS_TL +INCPSOL_TL+ INCRD_TL+INCTAXA_TL;
regle isf 11091:
application : iliad ;
TINRISF2 = positif(DO_INR_IFI2)
      * null(DO_INR_IFI_A)*TXINRISF_A
     + positif(DO_INR_IFI2)
      *positif(DO_INR_IFI2)*TINRISF_A
     + positif(INRIFI_R99R +RECUP_INR_IFI*FLAG_RECTIF)
               *TXINRISF_PA
     + null(DO_INR_IFI2) *null(INRIFI_R99R +RECUP_INR_IFI*FLAG_RECTIF) *TXINRISF;
NBREMOISISF222 = (NBMOISI + max(0,NBMOISI2))
		  * positif(INRIFI_NET2)
		  + NBREMOIS222ISF_A * (1- positif_ou_nul(INRIFI_NET2));

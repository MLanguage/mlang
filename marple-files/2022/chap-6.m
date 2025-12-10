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
                                                                        
  ####   #    #    ##    #####      #     #####  #####   ######          #####
 #    #  #    #   #  #   #    #     #       #    #    #  #              #
 #       ######  #    #  #    #     #       #    #    #  #####          # ####
 #       #    #  ######  #####      #       #    #####   #              #     #
 #    #  #    #  #    #  #          #       #    #   #   #              #     #
  ####   #    #  #    #  #          #       #    #    #  ######   ####   #####
 #
 #
 #
 #
 #
 #
 #
 #
 #                      CALCUL DU NOMBRE DE PARTS
 #
 #
 #
 #
 #
regle 601000:
application : iliad ;

NBPT =  (1 - present(V_9VV)) * ((NSM + NPA + NIN + NSP + NBQAR)* 10)/10 + V_9VV ;

NBPOTE = V_0CF + V_0CH + V_0CR + V_0DJ + V_0DN + V_0DP ;

NBFOTH = (1 + BOOL_0AM) + V_0CF + (V_0CH/2) + V_0CR + V_0DJ + V_0DN ;
NBPERS = (1+BOOL_0AM)+V_0CH+V_0DP+V_0CR+V_0CF+V_0DJ+V_0DN;

NBPERSN1 = V_BT0AC + V_BT0AD + V_BT0AV + positif(V_BT0AM +V_BT0AO)*(1+BOOL_0AMN1) + V_BTALT1 + V_BTCHAR1 ; 

regle 601010:
application : bareme ;

NBPT =  (1 - present(V_9VV)) * ((NSM + NPA + NIN + NSP + NBQAR)* 10)/10 + V_9VV ;

regle 601020:
application : bareme , iliad ;

NSM = 1 + BOOL_0AM + V_0AV * min(BOOL_0AZ + EAC + V_0CH + V_0CR, 1 ) +
    ( V_0AV * BOOL_0AZ * V_0AW * (1 - V_0AP) * (1 - V_0AF) / 2 ) ;

regle 601030:
application : bareme , iliad ;


NPA = PAC - 0.5 * min( PAC, 2 ) ;

regle 601040:
application : bareme , iliad ;


PAC = EAC + V_0CR ;

regle 601050:
application : bareme ;


EAC = ((V_0CF + V_0DJ) * (1 - present(V_9XX)))  + V_9XX ;

regle 601060:
application : iliad ;

EAC = (V_0CF + V_0DJ) ;

regle 601070:
application : bareme , iliad ;


NIN = (V_0AP + V_0AF + V_0CG + V_0CR) * 0.5 ;

regle 601080:
application : bareme , iliad ;


NSP = NPS + NSA + NCC ;

regle 601090:
application : bareme , iliad ;


NCC = V_0AS * BOOL_0AM * (1 - positif(V_0AP + V_0AF)) * 0.5 *
      positif(max( AGV + 1 - LIM_AGE_LET_S, AGC + 1 - LIM_AGE_LET_S ));

regle 601100:
application : bareme , iliad ;


NPS = positif(PAC) * V_0BT * positif( 2 - NSM ) * 0.5 ;

regle 601110:
application : bareme , iliad ;


NSA = (1 - positif(PAC+V_0CH)) 
          * min( V_0AG
	       + (V_0AL * (1 - positif (V_0AN))) * ( 1 - positif(positif(V_0AW)*positif_ou_nul(AGV-LIM_AGE_LET_S)))	
               + V_0AW * positif(max(AGV + 1 - LIM_AGE_LET_S, AGC + 1 - LIM_AGE_LET_S)) 
                , 1 )
      * ( 1 - V_0AP ) * positif(2 - NSM) * 0.5;
 
NSA2 = min( (1 - positif(PAC+V_0CH))
              *
       (null(SFUTILE - 14)
        + null (SFUTILE - 7)
        + null (SFUTILE - 15) * (1-positif(NIN))    
       )
        +
           null(SFUTILE -7) * BOOL_0AZ * V_0AV
       , 1)
* 0.5 ;

regle 601120:
application : bareme , iliad ;



BOOL_0BT = positif ( V_0BT+0 ) * positif ( V_0AC + V_0AD + 0);

NBQAR1 =   null (V_0CF+V_0CR+V_0DJ+0)    * (  1/2 * (V_0CH-2) * positif (V_0CH- 2) 
                                + 1/4 * positif (V_0CH+0) 
                                + 1/4 * positif(V_0CH-1)
                               );
NBQAR2 =   null(V_0CF+V_0CR+V_0DJ-1) * (1/2 * (V_0CH -1 ) * positif(V_0CH -1) 
                               + 1/4 * positif (V_0CH+0) 
                           );
NBQAR3 =   positif_ou_nul(V_0CF+V_0CR+V_0DJ-2) * 1/2 * V_0CH ;

NBQAR4 =  1/4 * V_0CI ;

NBQART =  BOOL_0BT * null(V_0CF+V_0CR+V_0DJ+0) 
                     * (1/4 * null (V_0CH  -1) + 1/2 *  positif_ou_nul (V_0CH  - 2)) ;

NBQAR = NBQAR1 + NBQAR2 + NBQAR3 + NBQAR4 + NBQART ;


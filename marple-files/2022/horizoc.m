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
verif corrective horizontale 760 :
application : iliad ;
si          (
            V_IND_TRAIT > 0
	    et
            SENS_RAPPEL non dans (1, 2, 3, 4)
	    )
alors erreur A760;
verif corrective horizontale 770 :
application : iliad ;
si          (
            V_IND_TRAIT > 0
	    et
            SENS_RAPPEL = 4 
	    et 
            PEN_RAPPEL non dans (07, 08, 09, 10, 11, 12, 17, 18, 31)
	    )
alors erreur A770;
verif corrective horizontale 780 :
application : iliad ;
si
 (
            V_IND_TRAIT > 0
	    et
	    (
            ANNEE_RAPPEL <= ANNEEREV
            ou
            MOIS_RAPPEL non dans ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 )
	    )
 )

alors erreur A780;

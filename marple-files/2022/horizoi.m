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

verif corrective horizontale 666:
application : iliad ;
si
   (
      ( COD_RAPPEL = 1 et MONT_RAPPEL > 25) ou
      ( COD_RAPPEL = 2 et MONT_RAPPEL > 25) ou
      ( COD_RAPPEL = 3 et MONT_RAPPEL > 25) ou
      ( COD_RAPPEL = 4 et MONT_RAPPEL > 25) ou
      ( COD_RAPPEL = 5 et MONT_RAPPEL > 25) ou
      ( COD_RAPPEL = 6 et MONT_RAPPEL > 25) ou
      ( COD_RAPPEL = 7 et MONT_RAPPEL > 25) ou
      ( COD_RAPPEL = 8 et MONT_RAPPEL > 25)
   )
alors erreur A019;
verif corrective horizontale 777:
application : iliad ;
si
    (
      COD_RAPPEL = 1401 et MONT_RAPPEL > 45 
     )
alors erreur A14001;
verif corrective horizontale 888:
application : iliad ;
si
    (
      COD_RAPPEL = 1411 et MONT_RAPPEL > 25 
     )
alors erreur A14101;
verif corrective horizontale 999:
application : iliad ;
si
  (
    COD_RAPPEL = 2020 et MONT_RAPPEL > 9

  )
alors erreur A00101;

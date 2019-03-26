MIB_NETCT : calculee : "BIC  plus ou moins values a court terme du foyer" ;
MIBNETPTOT : calculee restituee : "Avis : micro bic net total foyer (sauf + V 16%)" ;
SPENETCT : calculee : "Net imposable prof. BNC regime special PV a court terme" ;
SPENETNPF : calculee restituee : "Net imposable non prof. BNC regime special" ;
SPENETPF : calculee restituee : "Net imposable prof. BNC regime special" ;
DLMRN1TXM : calculee : "avis IR : deficits non imputes annee N - 1" ;
MIB_NETNPCT : calculee : "BIC NON PROF plus ou moins values a court terme du foyer" ;
DEFBIC5 : saisie revenu classe = 0 priorite = 10 categorie_TL = 20 cotsoc = 5 ind_abat = 0 acompte = 1 avfisc = 0 rapcat = 8 sanction = 3 nat_code = 1 alias 5RO : "BIC non professionnel - Deficit de 2010" ;
DEFBIC4 : saisie revenu classe = 0 priorite = 10 categorie_TL = 20 cotsoc = 5 ind_abat = 0 acompte = 1 avfisc = 0 rapcat = 8 sanction = 3 nat_code = 1 alias 5RP : "BIC non professionnel - Deficit de 2011" ;
DEFBIC3 : saisie revenu classe = 0 priorite = 10 categorie_TL = 20 cotsoc = 5 ind_abat = 0 acompte = 1 avfisc = 0 rapcat = 8 sanction = 3 nat_code = 1 alias 5RQ : "BIC non professionnel - Deficit de 2012" ;
DEFBIC2 : saisie revenu classe = 0 priorite = 10 categorie_TL = 20 cotsoc = 5 ind_abat = 0 acompte = 1 avfisc = 0 rapcat = 8 sanction = 3 nat_code = 1 alias 5RR : "BIC non professionnel - Deficit de 2013" ;
DEFBIC1 : saisie revenu classe = 0 priorite = 10 categorie_TL = 20 cotsoc = 5 ind_abat = 0 acompte = 1 avfisc = 0 rapcat = 8 sanction = 3 nat_code = 1 alias 5RW : "BIC non professionnel - Deficit de 2014" ;
DLMRN6P : calculee : "avis IR : deficits non imputes annee N - 6 primitif" ;
DLMRN5P : calculee : "avis IR : deficits non imputes annee N - 5 primitif" ;
DLMRN4P : calculee : "avis IR : deficits non imputes annee N - 4 primitif" ;
DLMRN3P : calculee : "avis IR : deficits non imputes annee N - 3 primitif" ;
DLMRN2P : calculee : "avis IR : deficits non imputes annee N - 2 primitif" ;
V_IND_TRAIT : saisie contexte classe = 0 priorite = 10 categorie_TL = 20 alias IND_TRAIT : "indicateur de nature de trait. primitif ou correctif" ;
DEFBICNPF : calculee restituee : "BIC non professionnel deficit du foyer" ;
DEFNPI : calculee restituee : "BIC non professionnels (reel) - deficits anterieurs imputes en 97" ;
BINNV : calculee restituee : "&vis ir:Bic non prof net          VOUS" ;
BINNC : calculee restituee : "avis ir: Bic non prof net          CJT" ;
BINNP : calculee restituee : "avis ir : Bic non prof net          PAC" ;
MIBNETNPTOT : calculee restituee : "Avis : micro bic net total foyer (sauf + V 16%)" ;
DLMRN1 : calculee restituee : "avis IR : deficits non imputes annee N - 1" ;
FLAG_RETARD : saisie penalite alias V_FLAG_RETA : "nouveau cor : indicateur de retard 2042" ;
FLAG_RECTIF : saisie penalite alias V_FLAG_RECTF : "nouv cor : indicateur de rectification" ;

regle 111320:
application : iliad , batch ;
BINNC = somme(i=V,C,P:BICNPi);

#DLMRN1TXM = - min(0,MIB_NETCT *(1-positif(MIBNETPTOT))
#                          +SPENETCT * (1 - positif(SPENETPF)));

#DLMRN1 = ((1-positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)) * abs(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)
#                 + positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)
#                 * positif_ou_nul(DEFBIC5+DEFBIC4+DEFBIC3+DEFBIC2+DEFBIC1-(somme(i=V,C,P:BICNPi)+MIB_NETNPCT))
#                 * (DEFBIC5+DEFBIC4+DEFBIC3+DEFBIC2+DEFBIC1-(somme(i=V,C,P:BICNPi)+MIB_NETNPCT))
#                 * null(DLMRN6P+DLMRN5P+DLMRN4P+DLMRN3P+DLMRN2P)) * null(4 - V_IND_TRAIT)
#                 + null(5 - V_IND_TRAIT)*
#                                   ( max(0,DEFBICNPF-DEFNPI) * positif(DEFBICNPF)
#                                    + (max(0,-(BINNV+BINNC+BINNP+MIBNETNPTOT))) * null(DEFBICNPF));

#pour x=0,5;y=1,2;z=1,2:
#DSxyz = max( QFxyz - LIM_BAR1 , 0 ) * (TAUX1   / 100)
#      + max( QFxyz - LIM_BAR2 , 0 ) * (TAUX2   / 100)
#      + max( QFxyz - LIM_BAR3 , 0 ) * (TAUX3   / 100)
#      + max( QFxyz - LIM_BAR4 , 0 ) * (TAUX4   / 100)
#      + max( QFxyz - LIM_BAR5 , 0 ) * (TAUX5   / 100);

#SUPISF[X] = positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
#            * max(ISF4BASE,0)
#            + (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
#	     * max(0,ISF4BASE - (TISF4BASE[FLAG_DERSTTR]));

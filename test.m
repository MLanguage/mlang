regle 111320:
application : iliad , batch ;



DLMRN1TXM = - min(0,MIB_NETCT *(1-positif(MIBNETPTOT))
                          +SPENETCT * (1 - positif(SPENETPF)));

DLMRN1 = ((1-positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)) * abs(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)
                 + positif_ou_nul(somme(i=V,C,P:BICNPi)+MIB_NETNPCT)
                 * positif_ou_nul(DEFBIC5+DEFBIC4+DEFBIC3+DEFBIC2+DEFBIC1-(somme(i=V,C,P:BICNPi)+MIB_NETNPCT))
                 * (DEFBIC5+DEFBIC4+DEFBIC3+DEFBIC2+DEFBIC1-(somme(i=V,C,P:BICNPi)+MIB_NETNPCT))
                 * null(DLMRN6P+DLMRN5P+DLMRN4P+DLMRN3P+DLMRN2P)) * null(4 - V_IND_TRAIT)
                 + null(5 - V_IND_TRAIT)*
                                   ( max(0,DEFBICNPF-DEFNPI) * positif(DEFBICNPF)
                                    + (max(0,-(BINNV+BINNC+BINNP+MIBNETNPTOT))) * null(DEFBICNPF));

pour x=0,5;y=1,2;z=1,2:
DSxyz = max( QFxyz - LIM_BAR1 , 0 ) * (TAUX1   / 100)
      + max( QFxyz - LIM_BAR2 , 0 ) * (TAUX2   / 100)
      + max( QFxyz - LIM_BAR3 , 0 ) * (TAUX3   / 100)
      + max( QFxyz - LIM_BAR4 , 0 ) * (TAUX4   / 100)
      + max( QFxyz - LIM_BAR5 , 0 ) * (TAUX5   / 100);

SUPISF[X] = positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X)
            * max(ISF4BASE,0)
            + (1 - positif(FLAG_RETARD) * positif(FLAG_RECTIF) * null(X))
	     * max(0,ISF4BASE - (TISF4BASE[FLAG_DERSTTR]));


sortie (TRUC);

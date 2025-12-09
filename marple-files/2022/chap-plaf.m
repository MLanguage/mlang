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


regle 8200:
application : iliad  ;


LIMIT12 = 18000 + max(0, arr( max(0, RI1 + TONEQUO1) * (4/100))) 
		     * (1 - positif(VARRMOND))
	        + max(0, 
		      arr( max(0, 
				VARRMOND 
				 + TONEQUOM1
			      )* (4/100))
		      ) 
		      * positif(VARRMOND);



LIMIT11 = 18000 + max(0, arr( max(0, RI1 + TONEQUO1) * (6/100))) 
		     * (1 - positif(VARRMOND))
	        + max(0, 
		      arr( max(0, 
			        VARRMOND
				  + TONEQUOM1
			      ) * (6/100))
		      ) 
		      * positif(VARRMOND);



LIMIT10 = 20000 + max(0, arr( max(0, RI1 + TONEQUO1) * (8/100))) 
		     * (1 - positif(VARRMOND))
	        + max(0, 
		      arr( max(0,
				VARRMOND
				  + TONEQUOM1
			      ) * (8/100))
		     ) 
		     * positif(VARRMOND);



LIMIT9 = 25000 + max(0, arr( max(0, RI1 + TONEQUO1) * (10/100))) 
		    * (1 - positif(VARRMOND))
               + max(0, 
		     arr( max(0,
			       VARRMOND
				 + TONEQUOM1
			     ) * (10/100))
		    ) 
		    * positif(VARRMOND);
		     
regle 82021:
application : iliad  ;

IRBTEO = max(0 , V_IAD11TEO - min(RRISUP , RLOGSOCTEO + RCOLENTTEO) + VERSLIB + ITP + PVMTS + REI + AUTOVERSSUP) ;

IANTEO = max( 0, (IRBTEO - V_INETEO
                 + min(TAXASSUR+0 , max(0,V_INETEO-IRBTEO))
                 + min(IPCAPTAXTOT+0 , max(0,V_INETEO-IRBTEO - min(TAXASSUR+0,max(0,V_INETEO-IRBTEO))))
              )
         )
       ;
NEGIANTEO =  -1 * (min(TAXASSUR+0 , max(0,V_INETEO-IRBTEO))
		 + min(IPCAPTAXTOT+0 , max(0,V_INETEO-IRBTEO - min(TAXASSUR+0,max(0,V_INETEO-IRBTEO))))) ;

IARTEO = min( 0, IANTEO - V_IRETEO ) + max( 0, IANTEO - V_IRETEO ) + NEGIANTEO;
regle 820211:
application : iliad  ;
AVFISCO = (IARTEO - IARAF)* positif_ou_nul(V_INDTEO);

regle 82025:
application : iliad  ;
DIFFTEOREEL = AVFISCO * (1 - V_INDTEO)* positif_ou_nul(V_INDTEO);

regle 8202:
application : iliad  ;
AVFISCOPTER = (AVPLAF9 + AVPLAF10 + AVPLAF11 + AVPLAF12 + AVPLAF13) * positif_ou_nul(V_INDTEO);
regle 82463:
application : iliad  ;


A13RSOC = max(0 , RSOC49 + RSOC54 + RSOC45 + RSOC50 
                  + RSOCHYB + RSOCHYBR + RSOCHYA + RSOCHYAR
		  + RSOCHYD + RSOCHYDR + RSOCHYC + RSOCHYCR + RSOCHYE + RSOCHYER + RSOCHYF + RSOCHYFR + RSOCHYG + RSOCHYGR
                  - arr((((INVRETXT + INVRETXTR) * (1 - INDPLAF) + (INVRETXTA + INVRETXTRA) * INDPLAF)
		       + ((INVRETYA + INVRETYAR) * (1 - INDPLAF) + (INVRETYAA + INVRETYARA) * INDPLAF)
		       + ((INVRETYC + INVRETYCR) * (1 - INDPLAF) + (INVRETYCA + INVRETYCRA) * INDPLAF)) * TX65/100)
                  - arr((((INVRETXU + INVRETXUR) * (1 - INDPLAF) + (INVRETXUA + INVRETXURA) * INDPLAF)
		       + ((INVRETYB + INVRETYBR) * (1 - INDPLAF) + (INVRETYBA + INVRETYBRA) * INDPLAF)
		       + ((INVRETYD + INVRETYDR) * (1 - INDPLAF) + (INVRETYDA + INVRETYDRA) * INDPLAF)
		       + ((INVRETYE + INVRETYER) * (1 - INDPLAF) + (INVRETYEA + INVRETYERA) * INDPLAF)
		       + ((INVRETYF + INVRETYFR) * (1 - INDPLAF) + (INVRETYFA + INVRETYFRA) * INDPLAF)
		       + ((INVRETYG + INVRETYGR) * (1 - INDPLAF) + (INVRETYGA + INVRETYGRA) * INDPLAF)) * TX70/100)
             ) * (1 - V_CNR) ;

regle 82462:
application : iliad  ;


A12RSOC = max(0 , RSOC48 + RSOC53 
                  - arr((((INVRETXS + INVRETXSR) * (1 - INDPLAF) + (INVRETXSA + INVRETXSRA) * INDPLAF)) * TX65/100)
             ) * (1 - V_CNR) ; 

regle 82461:
application : iliad  ;


A11RSOC = max(0 , RSOC47 + RSOC52 
                  - arr((((INVRETXR + INVRETXRR) * (1 - INDPLAF) + (INVRETXRA + INVRETXRRA) * INDPLAF)) * TX65/100)
             ) * (1 - V_CNR) ;

regle 8246:
application : iliad  ;


A10RSOC = max(0 , RSOC46 + RSOC51 
                  - arr((((INVRETXQ + INVRETXQR) * (1 - INDPLAF) + (INVRETXQA + INVRETXQRA) * INDPLAF)) * TX65/100)) * (1 - V_CNR) ;

regle 82473:
application : iliad  ;


A13RENT = (RLOCHFP + RLOCHFR + RLOCHFU + RLOCHGU + RLOCHGW + RLOCHHU + RLOCHHW + RLOCHIU + RLOCHIW + RLOCHFW + RLOCHEP + RLOCHEU + RLOCHER + RLOCHEW + RLOC142 + RLOC145 + RLOC143 + RLOC146 
           + max (0 , RLOCHFN + RLOCHGT + RLOCHGS + RLOCHHT + RLOCHHS + RLOCHIT + RLOCHIS + RLOCHFO + RLOCHFS + RLOCHFT + RLOCHFNR + RLOCHGTR + RLOCHGSR + RLOCHHTR + RLOCHHSR 
	              + RLOCHITR + RLOCHISR + RLOCHFOR + RLOCHFSR + RLOCHFTR 
	              + RLOCHET + RLOCHEO + RLOCHES + RLOCHEN + RLOCHETR + RLOCHEOR + RLOCHESR + RLOCHENR 
	              + RLOC140 + RLOC152 + RLOC137 + RLOC149 + RLOC138 + RLOC150 + RLOC135 + RLOC147 
                    - (
		         arr(((INVRETFN + INVRETFNR) * (1 - INDPLAF) + (INVRETFNA + INVRETFNRA) * INDPLAF) * TX5263/100)
		       + arr(((INVRETFO + INVRETFOR) * (1 - INDPLAF) + (INVRETFOA + INVRETFORA) * INDPLAF) * TX625/100)
		       + arr(((INVRETFS + INVRETFSR) * (1 - INDPLAF) + (INVRETFSA + INVRETFSRA) * INDPLAF) * TX56/100)
		       + arr(((INVRETFT + INVRETFTR) * (1 - INDPLAF) + (INVRETFTA + INVRETFTRA) * INDPLAF) * TX66/100)
		       + arr(((INVRETEN + INVRETENR) * (1 - INDPLAF) + (INVRETENA + INVRETENRA) * INDPLAF) * TX5263/100)
		       + arr(((INVRETEO + INVRETEOR) * (1 - INDPLAF) + (INVRETEOA + INVRETEORA) * INDPLAF) * TX625/100)
		       + arr(((INVRETES + INVRETESR) * (1 - INDPLAF) + (INVRETESA + INVRETESRA) * INDPLAF) * TX56/100)
		       + arr(((INVRETET + INVRETETR) * (1 - INDPLAF) + (INVRETETA + INVRETETRA) * INDPLAF) * TX66/100)
		       + arr(((INVRETDN + INVRETDNR) * (1 - INDPLAF) + (INVRETDNA + INVRETDNRA) * INDPLAF) * TX5263/100)
		       + arr(((INVRETDO + INVRETDOR) * (1 - INDPLAF) + (INVRETDOA + INVRETDORA) * INDPLAF) * TX625/100)
		       + arr(((INVRETDS + INVRETDSR) * (1 - INDPLAF) + (INVRETDSA + INVRETDSRA) * INDPLAF) * TX56/100)
		       + arr(((INVRETDT + INVRETDTR) * (1 - INDPLAF) + (INVRETDTA + INVRETDTRA) * INDPLAF) * TX66/100)
                       + arr(((INVRETGS + INVRETGSR) * (1 - INDPLAF) + (INVRETGSA + INVRETGSRA) * INDPLAF) * TX56/100)
                       + arr(((INVRETGT + INVRETGTR) * (1 - INDPLAF) + (INVRETGTA + INVRETGTRA) * INDPLAF) * TX66/100)
                       + arr(((INVRETHS + INVRETHSR) * (1 - INDPLAF) + (INVRETHSA + INVRETHSRA) * INDPLAF) * TX56/100)
                       + arr(((INVRETHT + INVRETHTR) * (1 - INDPLAF) + (INVRETHTA + INVRETHTRA) * INDPLAF) * TX66/100)
                       + arr(((INVRETIS + INVRETISR) * (1 - INDPLAF) + (INVRETISA + INVRETISRA) * INDPLAF) * TX56/100)
                       + arr(((INVRETIT + INVRETITR) * (1 - INDPLAF) + (INVRETITA + INVRETITRA) * INDPLAF) * TX66/100)
		       )
                  )
             ) * (1 - V_CNR) ;

regle 82472:
application : iliad  ;


A12RENT = (RLOC141 + RLOC144
           + max (0 , RLOC139 + RLOC151 + RLOC136 + RLOC148 
                    - (arr(((INVRETDI + INVRETDIR) * (1 - INDPLAF) + (INVRETDIA + INVRETDIRA) * INDPLAF) * TX5263/100)
		       + arr(((INVRETDJ + INVRETDJR) * (1 - INDPLAF) + (INVRETDJA + INVRETDJRA) * INDPLAF) * TX625/100))
                 )
            ) * (1 - V_CNR);

regle 82492:
application : iliad  ;
PLAFRED_FORTRA = max( 0, PLAF_FOREST1 * (1 + BOOL_0AM) - ACOTFOR_R);
BASE7UN = (min (RDFOREST, PLAF_FOREST * (1 + BOOL_0AM))) * (1 - V_CNR) ;

regle 82493:
application : iliad  ;
A13RFOR_1 = max(0 , min(arr((BASE7UWI + BASE7UN)* TX18/100) , RRI1-RLOGDOMTOT-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT
                                                                  -RNOUV-RPLAFREPME4-RPENTDY-RPENTEY-A10RFOR-A11RFOR)) ;

A13RFOR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * A13RFOR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(A13RFOR_1 , A13RFOR1731)) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;

regle 8249:
application : iliad  ;
BA10RFOR  = arr(BASE7UTF * TX25 / 100 ) ;
A10RFOR_1 = max(0 , min(BA10RFOR , RRI1-RLOGDOMTOT-RCOMP-RRETU-RDONS-CRDIE-RDUFREP-RPINELTOT-RNORMTOT-RNOUV-RPLAFREPME4-RPENTDY-RPENTEY)) ;

A10RFOR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (A10RFOR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(A10RFOR_1,A10RFOR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0 ;

regle 8250:
application : iliad  ;
A13REELA = RCOTFOR + RLOCANAH + RFIPDOM + RFIPC  + RPRESSE + RINNO + RSOUFIP + RRIRENOV + RDUFREP 
           + RPIQW + RPIQX + RPIREPBI + RPIREPCZ + RPIREPRZ + RPIREPRA + RPIREPRB + RPIREPRE 
	   + RPIREPRF + RPIREPJM + RPIREPKM + RPIQA + RPIQB + RPIQI + RPIQJ + RPIQM + RPIQN + RPIJN + RPIJO + RPIJV + RPIJW + RPIRX + RPIRP + RPISX + RPIUY + RPIRI
           + RNORMJA + RNORMJB + RNORMJR + RNORMJS + RNORMLG + RNORMLH + RNONA + RNONB + RNONE + RNONF + RNONI + RNONJ + RNONM + RNONN
	   + A13RFOR 
           + arr(RSNCR + RSNCV + RSNCX + RSNCS + RSNCT + RSNCA + RSNCH + RSNCI + RSNGW + RSNBS)
           + max(0 , arr(RSNDC + RSNBT) + RPENTEK - LIM3000)
	   + RPENTCY + RPENTDY + RPENTEY + RPENTFY + RPENTGY
           + CIGARD + CIADCRE + CIHABPRIN + CIFORET + CIVHELEC + CIPAP
	   + RILMOF + RILMPO + RILMPT + RILMPY + RILMHS + RILMHX + RILMHH + RILMSA + RILMSN + RILMSP + RILMOK 
	   + RILMOP + RILMSM + RILMSS + RILMKI + RCODJZ + RCODOW + RCODOX + RCODOY + RCODPZ + RCODMZ + RCODMW + RCODMN
	   + RPATNAT + RREHAB ; 

A13REELB = RCINE 
           + RPIQQ + RPIQY + RPIREPDI + RPIREPEZ + RPIREPTZ + RPIREPRC + RPIREPRD + RPIREPRG 
	   + RPIREPRH + RPIREPLM + RPIREPMM + RPIQC + RPIQD + RPIQK + RPIQL + RPIQO + RPIQP + RPIJP + RPIJQ + RPIJX + RPIJY + RPIRY + RPIRQ + RPISY + RPIUZ + RPIRJ
	   + RNORMJC + RNORMJD + RNORMJT + RNORMJU + RNORMLI + RNORMLJ + RNONC + RNOND + RNONG + RNONH + RNONK + RNONL + RNOPF + RNOPG 
	   + RLOG32 + RLOG39 + RLOG46 + RLOG53 + RLOG60 + RLOGHVH + RLOGHVI + RLOGHVJ + RLOGHVK + RLOGHVL
           + A13RSOC + A13RENT ;

regle 8254:
application : iliad  ;
AUBAINE13A = max(0 , min(A13REELA , DIFFTEOREEL)) ;
AUBAINE13B = max(0 , min(A13REELB , DIFFTEOREEL - AUBAINE13A)) ;

regle 8255:
application : iliad  ;


A12REEL = RCELRREDLV
          + RCELLY + RCELMV + RCELMR + RCELMD
          + RCELREPYM  + RCELREPYT + RCELREPWT 
	  + RCELRT + RCELXQ + RCELYL + RCELZL + RCELSD + RCELSF + RCELSG 
	  + RCELIF + RCELIH + RCELIO + RCELIP + RCELYA + RCELYR + RCELLT 
	  + RCELLZ + RCELMG + RCELMH + RCELHY + RCELPE + RCELUX 
	  + RCELML + RCELWC + RCELWD + RCELKO + RCELKQ + RCELKR + RCELKS + RCELWZ + RCELAP + RCELAT 
	  + RCELRN + RCELBE + RCELBG + RCELBH + RCELBJ + RCELGX + RCELKV + RCELAU + RCELWF + RCELWG
          + RCODID + RILMOG + RILMPN + RILMOL + RILMPS + RILMPX + RILMHR + RILMHW + RILMHG + RILMOQ + RILMSB 
	  + RILMSO + RILMKH
          + RLOG25 + RLOG31 + RLOG38 + RLOG45 + RLOG52 + RLOG59
          + A12RSOC + A12RENT ;

regle 8256:
application : iliad  ;
AUBAINE12 = max(0 , min(A12REEL , DIFFTEOREEL - AUBAINE13A - AUBAINE13B)) ;

regle 8260:
application : iliad  ;


A11REEL = RLOG16 + RLOG21 + RLOG24 + RLOG28 + RLOG30 
          + RLOG35 + RLOG37 + RLOG42 + RLOG44 + RLOG49 
          + RLOG51 + RLOG56 + RLOG58
          + A11RSOC 
	  + RCELRREDLU + RCELLC + RCELMU + RCELMQ + RCELMC
	  + RCELREPYN + RCELREPYU + RCELREPWU
	  + RCELRU + RCELZO + RCELXO + RCELYK + RCELZK + RCELKC + RCELSE + RCELIG 
	  + RCELSH + RCELSJ + RCELSK + RCELIA + RCELIC + RCELIE 
	  + RCELXN + RCELYG + RCELLD + RCELLF + RCELLN + RCELLX + RCELHJ
	  + RCELHN + RCELPC + RCELUW + RCELYS
	  + RCELMK + RCELWE + RCELXH + RCELXJ + RCELXK + RCELKJ + RCELKL + RCELKN + RCELWY 
	  + RCELAB + RCELAI + RCELAS + RCELRM + RCELBA + RCELBC + RCELBD + RCELBF + RCELGU + RCELKT + RCELQE
	  + RCODIN + RCODIJ + RILMSC + RILMOH + RILMPM + RILMOM + RILMPR + RILMPW + RILMHQ + RILMHV + RILMOR 
	  + RILMHF + RILMKG
          + A11RFOR ;

regle 8261:
application : iliad  ;
AUBAINE11 = max(0 , min(A11REEL , DIFFTEOREEL - AUBAINE13A - AUBAINE13B - AUBAINE12)) ;

regle 8262:
application : iliad  ;

A10REEL = RLOG11 + RLOG13 + RLOG15 + RLOG18 + RLOG20 + RLOG23 + RLOG26 + RLOG27 
          + RLOG29 + RLOG33 + RLOG34 + RLOG36 + RLOG40 + RLOG41 + RLOG43 
          + RLOG47 + RLOG48 + RLOG50 + RLOG54 + RLOG55 + RLOG57
          + A10RSOC 
          + RCELRREDLR + RCELLB + RCELMT + RCELMP + RCELMB
          + RCELREPYO + RCELREPYV + RCELREPWV
	  + RCELZP + RCELXP + RCELYJ + RCELZJ + RCELKD 
	  + RCELXA + RCELXC + RCELXM + RCELYC + RCELLE + RCELHA + RCELHK + RCELPD
	  + RCELUV + RCELSI + RCELXI + RCELIB + RCELSL + RCELYX 
	  + RCELMJ + RCELPI + RCELWX + RCELAD + RCELAH + RCELAR + RCELRL + RCELBB + RCELGS + RCELKU + RCELHL
          + RCODIM + RILMOI + RILMPL + RILMON + RILMPQ + RILMPV + RILMHP + RILMHU + RILMOS + RILMHE + RILMKF
          + A10RFOR ;

regle 8263:
application : iliad  ;
AUBAINE10 = max(0 , min(A10REEL , DIFFTEOREEL - AUBAINE13A - AUBAINE13B - AUBAINE12 - AUBAINE11)) ;

regle 8280:
application : iliad  ;

AUBAINE9 = max(0 , DIFFTEOREEL - AUBAINE13A - AUBAINE13B - AUBAINE12 - AUBAINE11 - AUBAINE10) ;

regle 8290:
application : iliad ;

AVPLAF13A = max(0, AUBAINE13A - LIM10000 ) * positif(DIFFTEOREEL) ;

AVPLAF13B = max(0, min(AUBAINE13A , LIM10000) + AUBAINE13B - LIM18000 ) * positif(DIFFTEOREEL) ;

AVPLAF13 = AVPLAF13A + AVPLAF13B ;

AVPLAF12 = max(0, AUBAINE13A + AUBAINE13B + AUBAINE12 
                  - AVPLAF13 - LIMIT12) * positif(DIFFTEOREEL);
AVPLAF11 = max(0, AUBAINE13A + AUBAINE13B + AUBAINE12 + AUBAINE11 
                  - AVPLAF13 - AVPLAF12 - LIMIT11) * positif(DIFFTEOREEL);
AVPLAF10 = max(0, AUBAINE13A + AUBAINE13B + AUBAINE12 + AUBAINE11 + AUBAINE10 
                  - AVPLAF13 - AVPLAF12 - AVPLAF11 - LIMIT10) * positif(DIFFTEOREEL);
AVPLAF9  = max(0, AUBAINE13A + AUBAINE13B + AUBAINE12 + AUBAINE11 + AUBAINE10 + AUBAINE9 
                  - AVPLAF13 - AVPLAF12 - AVPLAF11 - AVPLAF10 - LIMIT9) * positif(DIFFTEOREEL) ;

regle 8321:
application : iliad  ;
RFTEO = RFORDI + RFROBOR ; 
regle 8331:
application : iliad  ;

RFNTEO = (RFORDI + RFROBOR - min(
                                     min(RFDORD,RFDORD1731+0) * positif(ART1731BIS) + RFDORD * (1 - ART1731BIS)
			           + min(RFDANT,RFDANT1731+0) * positif(ART1731BIS) + RFDANT * (1 - ART1731BIS) ,
                                    RFORDI + RFROBOR
                                ) 
                           - RFDHIS * (1 - ART1731BIS)      
         ) * present(RFROBOR) + RRFI * (1-present(RFROBOR));

regle 8341:
application : iliad  ;
RRFTEO = RFNTEO ;

regle 8400 :
application : iliad  ;

RLOG01_1 = max(min(INVLOG2008 , RRI1) , 0) * (1 - V_CNR) ;
RLOG01 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG01_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG01_1,RLOG011731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = RLOG01 ;

RLOG02_1 = max(min(INVLGDEB2009 , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
RLOG02 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG02_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG02_1,RLOG021731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG02 ;

RLOG03_1 = max(min(INVLGDEB , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
RLOG03 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG03_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG03_1,RLOG031731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG03 ;

RLOG04_1 = max(min(INVOMLOGOA , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
RLOG04 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG04_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG04_1,RLOG041731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG04 ;

RLOG05_1 = max(min(INVOMLOGOH , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
RLOG05 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG05_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG05_1,RLOG051731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG05 ;

RLOG06_1 = max(min(INVOMLOGOL , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
RLOG06 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG06_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG06_1,RLOG061731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG06 ;

RLOG07_1 = max(min(INVOMLOGOO , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
RLOG07 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG07_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG07_1,RLOG071731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG07 ;

RLOG08_1 = max(min(INVOMLOGOS , RRI1-VARTMP1) , 0) * (1 - V_CNR) ;
RLOG08 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG08_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG08_1,RLOG081731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG08 ;

RLOG09_1 = max(min((INVRETQL * (1 - INDPLAF) + INVRETQLA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG09 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG09_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG09_1,RLOG091731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG09 ;

RLOG10_1 = max(min((INVRETQM * (1 - INDPLAF) + INVRETQMA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG10 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG10_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RLOG10_1,RLOG101731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG10 ;

RLOG11_1 = max(min((INVRETQD * (1 - INDPLAF) + INVRETQDA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG11 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG11_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG11_1,RLOG111731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG11 ;

RLOG12_1 = max(min((INVRETOB * (1 - INDPLAF) + INVRETOBA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG12 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG12_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG12_1,RLOG121731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG12 ;

RLOG13_1 = max(min((INVRETOC * (1 - INDPLAF) + INVRETOCA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG13 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG13_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG13_1,RLOG131731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG13 ;

RLOG14_1 = max(min((INVRETOI * (1 - INDPLAF) + INVRETOIA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG14 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG14_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG14_1,RLOG141731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG14 ;

RLOG15_1 = max(min((INVRETOJ * (1 - INDPLAF) + INVRETOJA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG15 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG15_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG15_1,RLOG151731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG15 ;

RLOG16_1 = max(min((INVRETOK * (1 - INDPLAF) + INVRETOKA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG16 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG16_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG16_1,RLOG161731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG16 ;

RLOG17_1 = max(min((INVRETOM * (1 - INDPLAF) + INVRETOMA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG17 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG17_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG17_1,RLOG171731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG17 ;

RLOG18_1 = max(min((INVRETON * (1 - INDPLAF) + INVRETONA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG18 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG18_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG18_1,RLOG181731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG18 ;

RLOG19_1 = max(min((INVRETOP * (1 - INDPLAF) + INVRETOPA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG19 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG19_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG19_1,RLOG191731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG19 ;

RLOG20_1 = max(min((INVRETOQ * (1 - INDPLAF) + INVRETOQA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG20 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG20_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG20_1,RLOG201731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG20 ;

RLOG21_1 = max(min((INVRETOR * (1 - INDPLAF) + INVRETORA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG21 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG21_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG21_1,RLOG211731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG21 ;

RLOG22_1 = max(min((INVRETOT * (1 - INDPLAF) + INVRETOTA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG22 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG22_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG22_1,RLOG221731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG22 ;

RLOG23_1 = max(min((INVRETOU * (1 - INDPLAF) + INVRETOUA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG23 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG23_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG23_1,RLOG231731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG23 ;

RLOG24_1 = max(min((INVRETOV * (1 - INDPLAF) + INVRETOVA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG24 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG24_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG24_1,RLOG241731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG24 ;

RLOG25_1 = max(min((INVRETOW * (1 - INDPLAF) + INVRETOWA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG25 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG25_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG25_1,RLOG251731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG25 ;

RLOG26_1 = max(min((INVRETOD * (1 - INDPLAF) + INVRETODA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG26 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG26_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG26_1,RLOG261731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG26 ;

RLOG27_1 = max(min((INVRETOE * (1 - INDPLAF) + INVRETOEA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG27 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG27_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG27_1,RLOG271731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG27 ;

RLOG28_1 = max(min((INVRETOF * (1 - INDPLAF) + INVRETOFA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG28 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG28_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG28_1,RLOG281731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG28 ;

RLOG29_1 = max(min((INVRETOG * (1 - INDPLAF) + INVRETOGA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG29 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG29_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG29_1,RLOG291731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG29 ;

RLOG30_1 = max(min((INVRETOX * (1 - INDPLAF) + INVRETOXA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG30 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG30_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG30_1,RLOG301731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG30 ;

RLOG31_1 = max(min((INVRETOY * (1 - INDPLAF) + INVRETOYA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG31 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG31_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG31_1,RLOG311731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG31 ;

RLOG32_1 = max(min((INVRETOZ * (1 - INDPLAF) + INVRETOZA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG32 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG32_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG32_1,RLOG321731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG32 ;

RLOG33_1 = max(min((INVRETUA * (1 - INDPLAF) + INVRETUAA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG33 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG33_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG33_1,RLOG331731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG33 ;

RLOG34_1 = max(min((INVRETUB * (1 - INDPLAF) + INVRETUBA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG34 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG34_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG34_1,RLOG341731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG34 ;

RLOG35_1 = max(min((INVRETUC * (1 - INDPLAF) + INVRETUCA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG35 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG35_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG35_1,RLOG351731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG35 ;

RLOG36_1 = max(min((INVRETUD * (1 - INDPLAF) + INVRETUDA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG36 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG36_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG36_1,RLOG361731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG36 ;

RLOG37_1 = max(min((INVRETUE * (1 - INDPLAF) + INVRETUEA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG37 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG37_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG37_1,RLOG371731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG37 ;

RLOG38_1 = max(min((INVRETUF * (1 - INDPLAF) + INVRETUFA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG38 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG38_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG38_1,RLOG381731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG38 ;

RLOG39_1 = max(min((INVRETUG * (1 - INDPLAF) + INVRETUGA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG39 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG39_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG39_1,RLOG391731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG39 ;

RLOG40_1 = max(min((INVRETUH * (1 - INDPLAF) + INVRETUHA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG40 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG40_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG40_1,RLOG401731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG40 ;

RLOG41_1 = max(min((INVRETUI * (1 - INDPLAF) + INVRETUIA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG41 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG41_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG41_1,RLOG411731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG41 ;

RLOG42_1 = max(min((INVRETUJ * (1 - INDPLAF) + INVRETUJA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG42 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG42_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG42_1,RLOG421731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG42 ;

RLOG43_1 = max(min((INVRETUK * (1 - INDPLAF) + INVRETUKA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG43 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG43_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG43_1,RLOG431731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG43 ;

RLOG44_1 = max(min((INVRETUL * (1 - INDPLAF) + INVRETULA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG44 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG44_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG44_1,RLOG441731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG44 ;

RLOG45_1 = max(min((INVRETUM * (1 - INDPLAF) + INVRETUMA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG45 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG45_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG45_1,RLOG451731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG45 ;

RLOG46_1 = max(min((INVRETUN * (1 - INDPLAF) + INVRETUNA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG46 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG46_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG46_1,RLOG461731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG46 ;

RLOG47_1 = max(min((INVRETUO * (1 - INDPLAF) + INVRETUOA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG47 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG47_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG47_1,RLOG471731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG47 ;

RLOG48_1 = max(min((INVRETUP * (1 - INDPLAF) + INVRETUPA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG48 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG48_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG48_1,RLOG481731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG48 ;

RLOG49_1 = max(min((INVRETUQ * (1 - INDPLAF) + INVRETUQA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG49 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG49_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG49_1,RLOG491731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG49 ;

RLOG50_1 = max(min((INVRETUR * (1 - INDPLAF) + INVRETURA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG50 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG50_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG50_1,RLOG501731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG50 ;

RLOG51_1 = max(min((INVRETUS * (1 - INDPLAF) + INVRETUSA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG51 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG51_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG51_1,RLOG511731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG51 ;

RLOG52_1 = max(min((INVRETUT * (1 - INDPLAF) + INVRETUTA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG52 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG52_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG52_1,RLOG521731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG52 ;

RLOG53_1 = max(min((INVRETUU * (1 - INDPLAF) + INVRETUUA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG53 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG53_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG53_1,RLOG531731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG53 ;

RLOG54_1 = max(min((INVRETVA * (1 - INDPLAF) + INVRETVAA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG54 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG54_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG54_1,RLOG541731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG54 ;

RLOG55_1 = max(min((INVRETVB * (1 - INDPLAF) + INVRETVBA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG55 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG55_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG55_1,RLOG551731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG55 ;

RLOG56_1 = max(min((INVRETVC * (1 - INDPLAF) + INVRETVCA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG56 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG56_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG56_1,RLOG561731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG56 ;

RLOG57_1 = max(min((INVRETVD * (1 - INDPLAF) + INVRETVDA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG57 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG57_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG57_1,RLOG571731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG57 ;

RLOG58_1 = max(min((INVRETVE * (1 - INDPLAF) + INVRETVEA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG58 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG58_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG58_1,RLOG581731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG58 ;

RLOG59_1 = max(min((INVRETVF * (1 - INDPLAF) + INVRETVFA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG59 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG59_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG59_1,RLOG591731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG59 ;

RLOG60_1 = max(min((INVRETVG * (1 - INDPLAF) + INVRETVGA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOG60 =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOG60_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOG60_1,RLOG601731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOG60 ;

RLOGHVH_1 = max(min((INVRETVH * (1 - INDPLAF) + INVRETVHA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOGHVH =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOGHVH_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOGHVH_1,RLOGHVH1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOGHVH ;

RLOGHVI_1 = max(min((INVRETVI * (1 - INDPLAF) + INVRETVIA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOGHVI =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOGHVI_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOGHVI_1,RLOGHVI1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOGHVI ;

RLOGHVJ_1 = max(min((INVRETVJ * (1 - INDPLAF) + INVRETVJA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOGHVJ =positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOGHVJ_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOGHVJ_1,RLOGHVJ1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOGHVJ ;

RLOGHVK_1 = max(min((INVRETVK * (1 - INDPLAF) + INVRETVKA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOGHVK = positif(null(V_IND_TRAIT-4) + COD9ZA) * (RLOGHVK_1) * (1 - positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
          + (max(0,min(RLOGHVK_1 , RLOGHVK1731)) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5) + 0 ;
VARTMP1 = VARTMP1 + RLOGHVK ;

RLOGHVL_1 = max(min((INVRETVL * (1 - INDPLAF) + INVRETVLA * INDPLAF) , RRI1 - VARTMP1) , 0) * (1 - V_CNR) ;
RLOGHVL = positif(null(V_IND_TRAIT-4) + COD9ZA) * (RLOGHVL_1) * (1 - positif(null(8-CMAJ) + null(11-CMAJ) + null(34-CMAJ)))
          + (max(0,min(RLOGHVL_1 , RLOGHVL1731)) * positif(1-COD9ZA) * (1-positif(PREM8_11))) * null(V_IND_TRAIT-5) + 0 ;
VARTMP1 = 0 ;

RLOGDOMTOT = (1 - V_INDTEO) * (somme(i=1..60: RLOGi) + RLOGHVH + RLOGHVI + RLOGHVJ + RLOGHVK + RLOGHVL) ;
RLOGDOMTOT_1 = (1 - V_INDTEO) * (somme(i=1..60: RLOGi_1) + RLOGHVH_1 + RLOGHVI_1 + RLOGHVJ_1 + RLOGHVK_1 + RLOGHVL_1) ;

RLOGDOMTEO = (RLOG01 + RLOG02 + RLOG03 + RLOG04 + RLOG05 + RLOG06 + RLOG07 + RLOG08) ;

regle 8401 :
application : iliad  ;


VARTMP1 = 0 ;

RSOC45_1 = arr(max(min((INVRETXU * (1 - INDPLAF) + INVRETXUA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC45 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC45_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC45_1,RSOC451731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC45 ;

RSOC46_1 = arr(max(min((INVRETXQ * (1 - INDPLAF) + INVRETXQA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC46 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC46_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC46_1,RSOC461731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC46 ;

RSOC47_1 = arr(max(min((INVRETXR * (1 - INDPLAF) + INVRETXRA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC47 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC47_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC47_1,RSOC471731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC47 ;

RSOC48_1 = arr(max(min((INVRETXS * (1 - INDPLAF) + INVRETXSA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC48 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC48_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC48_1,RSOC481731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC48 ;

RSOC49_1 = arr(max(min((INVRETXT * (1 - INDPLAF) + INVRETXTA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC49 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC49_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC49_1,RSOC491731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC49 ;

RSOC50_1 = arr(max(min((INVRETXUR * (1 - INDPLAF) + INVRETXURA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC50 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC50_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC50_1,RSOC501731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC50 ;

RSOC51_1 = arr(max(min((INVRETXQR * (1 - INDPLAF) + INVRETXQRA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC51 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC51_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC51_1,RSOC511731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC51 ;

RSOC52_1 = arr(max(min((INVRETXRR * (1 - INDPLAF) + INVRETXRRA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC52 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC52_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC52_1,RSOC521731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC52 ;

RSOC53_1 = arr(max(min((INVRETXSR * (1 - INDPLAF) + INVRETXSRA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC53 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC53_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC53_1,RSOC531731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC53 ;

RSOC54_1 = arr(max(min((INVRETXTR * (1 - INDPLAF) + INVRETXTRA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOC54 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOC54_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
         + (max(0,min(RSOC54_1,RSOC541731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOC54 ;

RSOCHYB_1 = arr(max(min((INVRETYB * (1 - INDPLAF) + INVRETYBA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYB = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOCHYB_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSOCHYB_1,RSOCHYB1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOCHYB ;

RSOCHYA_1 = arr(max(min((INVRETYA * (1 - INDPLAF) + INVRETYAA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYA = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOCHYA_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSOCHYA_1,RSOCHYA1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOCHYA ;

RSOCHYBR_1 = arr(max(min((INVRETYBR * (1 - INDPLAF) + INVRETYBRA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYBR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOCHYBR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RSOCHYBR_1,RSOCHYBR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOCHYBR ;

RSOCHYAR_1 = arr(max(min((INVRETYAR * (1 - INDPLAF) + INVRETYARA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYAR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOCHYAR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RSOCHYAR_1,RSOCHYAR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOCHYAR ;

RSOCHYD_1 = arr(max(min((INVRETYD * (1 - INDPLAF) + INVRETYDA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYD = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOCHYD_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSOCHYD_1,RSOCHYD1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOCHYD ;

RSOCHYC_1 = arr(max(min((INVRETYC * (1 - INDPLAF) + INVRETYCA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYC = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOCHYC_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RSOCHYC_1,RSOCHYC1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOCHYC ;

RSOCHYDR_1 = arr(max(min((INVRETYDR * (1 - INDPLAF) + INVRETYDRA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYDR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOCHYDR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RSOCHYDR_1,RSOCHYDR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOCHYDR ;

RSOCHYCR_1 = arr(max(min((INVRETYCR * (1 - INDPLAF) + INVRETYCRA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYCR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RSOCHYCR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RSOCHYCR_1,RSOCHYCR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RSOCHYCR ;

RSOCHYE_1 = arr(max(min((INVRETYE * (1 - INDPLAF) + INVRETYEA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYE = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RSOCHYE_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RSOCHYE_1 , RSOCHYE1731)) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RSOCHYE ;

RSOCHYER_1 = arr(max(min((INVRETYER * (1 - INDPLAF) + INVRETYERA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYER = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RSOCHYER_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(RSOCHYER_1 , RSOCHYER1731)) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RSOCHYER ;

RSOCHYF_1 = arr(max(min((INVRETYF * (1 - INDPLAF) + INVRETYFA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYF = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RSOCHYF_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RSOCHYF_1 , RSOCHYF1731)) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RSOCHYF ;

RSOCHYFR_1 = arr(max(min((INVRETYFR * (1 - INDPLAF) + INVRETYFRA * INDPLAF) , RRISUP - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYFR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RSOCHYFR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(RSOCHYFR_1 , RSOCHYFR1731)) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

regle 8402 :
application : iliad  ;


VARTMP1 = 0 ;

RSOCHYG_1 = arr(max(min((INVRETYG * (1 - INDPLAF) + INVRETYGA * INDPLAF) , RRISUP - RDOMSOC1 - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYG = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RSOCHYG_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
          + (max(0 , min(RSOCHYG_1 , RSOCHYG1731)) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = VARTMP1 + RSOCHYG ;

RSOCHYGR_1 = arr(max(min((INVRETYGR * (1 - INDPLAF) + INVRETYGRA * INDPLAF) , RRISUP - RDOMSOC1 - VARTMP1) , 0)) * (1 - V_CNR) ;
RSOCHYGR = positif(null(V_IND_TRAIT - 4) + COD9ZA) * RSOCHYGR_1 * (1 - positif(null(8 - CMAJ) + null(11 - CMAJ) + null(34 - CMAJ)))
           + (max(0 , min(RSOCHYGR_1 , RSOCHYGR1731)) * positif(1 - COD9ZA) * (1 - positif(PREM8_11))) * null(V_IND_TRAIT - 5) + 0 ;
VARTMP1 = 0 ;

regle 8403 :
application : iliad  ;


VARTMP1 = 0 ;
RRILOC = RRISUP - RDOMSOC1 - RLOGSOC ;

RLOC135_1 = max(min((INVRETDT * (1 - INDPLAF) + INVRETDTA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC135 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC135_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC135_1,RLOC1351731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC135 ;

RLOC136_1 = max(min((INVRETDJ * (1 - INDPLAF) + INVRETDJA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC136 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC136_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC136_1,RLOC1361731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC136 ;

RLOC137_1 = max(min((INVRETDO * (1 - INDPLAF) + INVRETDOA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC137 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC137_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC137_1,RLOC1371731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC137 ;

RLOC138_1 = max(min((INVRETDS * (1 - INDPLAF) + INVRETDSA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC138 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC138_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC138_1,RLOC1381731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC138 ;

RLOC139_1 = max(min((INVRETDI * (1 - INDPLAF) + INVRETDIA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC139 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC139_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC139_1,RLOC1391731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC139 ;

RLOC140_1 = max(min((INVRETDN * (1 - INDPLAF) + INVRETDNA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC140 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC140_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC140_1,RLOC1401731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC140 ;

RLOC141_1 = max(min((INVRETDK * (1 - INDPLAF) + INVRETDKA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC141 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC141_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC141_1,RLOC1411731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC141 ;

RLOC142_1 = max(min((INVRETDP * (1 - INDPLAF) + INVRETDPA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC142 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC142_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC142_1,RLOC1421731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC142 ;

RLOC143_1 = max(min((INVRETDU * (1 - INDPLAF) + INVRETDUA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC143 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC143_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC143_1,RLOC1431731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC143 ;

RLOC144_1 = max(min((INVRETDM * (1 - INDPLAF) + INVRETDMA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC144 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC144_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC144_1,RLOC1441731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC144 ;

RLOC145_1 = max(min((INVRETDR * (1 - INDPLAF) + INVRETDRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC145 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC145_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC145_1,RLOC1451731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC145 ;

RLOC146_1 = max(min((INVRETDW * (1 - INDPLAF) + INVRETDWA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC146 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC146_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC146_1,RLOC1461731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC146 ;

RLOC147_1 = max(min((INVRETDTR * (1 - INDPLAF) + INVRETDTRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC147 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC147_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC147_1,RLOC1471731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC147 ;

RLOC148_1 = max(min((INVRETDJR * (1 - INDPLAF) + INVRETDJRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC148 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC148_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC148_1,RLOC1481731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC148 ;

RLOC149_1 = max(min((INVRETDOR * (1 - INDPLAF) + INVRETDORA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC149 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC149_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC149_1,RLOC1491731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC149 ;

RLOC150_1 = max(min((INVRETDSR * (1 - INDPLAF) + INVRETDSRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC150 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC150_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC150_1,RLOC1501731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC150 ;

RLOC151_1 = max(min((INVRETDIR * (1 - INDPLAF) + INVRETDIRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC151 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC151_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC151_1,RLOC1511731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC151 ;

RLOC152_1 = max(min((INVRETDNR * (1 - INDPLAF) + INVRETDNRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOC152 = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOC152_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOC152_1,RLOC1521731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOC152 ;

RLOCHET_1 = max(min((INVRETET * (1 - INDPLAF) + INVRETETA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHET = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHET_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHET_1,RLOCHET1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHET ;

RLOCHEO_1 = max(min((INVRETEO * (1 - INDPLAF) + INVRETEOA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHEO = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHEO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHEO_1,RLOCHEO1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHEO ;

RLOCHES_1 = max(min((INVRETES * (1 - INDPLAF) + INVRETESA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHES = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHES_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHES_1,RLOCHES1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHES ;

RLOCHEN_1 = max(min((INVRETEN * (1 - INDPLAF) + INVRETENA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHEN = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHEN_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHEN_1,RLOCHEN1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHEN ;

RLOCHEP_1 = max(min((INVRETEP * (1 - INDPLAF) + INVRETEPA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHEP = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHEP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHEP_1,RLOCHEP1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHEP ;

RLOCHEU_1 = max(min((INVRETEU * (1 - INDPLAF) + INVRETEUA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHEU = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHEU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHEU_1,RLOCHEU1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHEU ;

RLOCHER_1 = max(min((INVRETER * (1 - INDPLAF) + INVRETERA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHER = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHER_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHER_1,RLOCHER1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHER ;

RLOCHEW_1 = max(min((INVRETEW * (1 - INDPLAF) + INVRETEWA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHEW = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHEW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHEW_1,RLOCHEW1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHEW ;

RLOCHFT_1 = max(min((INVRETFT * (1 - INDPLAF) + INVRETFTA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFT = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHFT_1,RLOCHFT1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFT ;

RLOCHFO_1 = max(min((INVRETFO * (1 - INDPLAF) + INVRETFOA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFO = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFO_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHFO_1,RLOCHFO1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFO ;

RLOCHFS_1 = max(min((INVRETFS * (1 - INDPLAF) + INVRETFSA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFS = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHFS_1,RLOCHFS1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFS ;

RLOCHFN_1 = max(min((INVRETFN * (1 - INDPLAF) + INVRETFNA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFN = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFN_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHFN_1,RLOCHFN1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFN ;

RLOCHFP_1 = max(min((INVRETFP * (1 - INDPLAF) + INVRETFPA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFP = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFP_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHFP_1,RLOCHFP1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFP ;

RLOCHFU_1 = max(min((INVRETFU * (1 - INDPLAF) + INVRETFUA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFU = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHFU_1,RLOCHFU1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFU ;

RLOCHFR_1 = max(min((INVRETFR * (1 - INDPLAF) + INVRETFRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHFR_1,RLOCHFR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFR ;

RLOCHFW_1 = max(min((INVRETFW * (1 - INDPLAF) + INVRETFWA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFW = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHFW_1,RLOCHFW1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFW ;

RLOCHGT_1 = max(min((INVRETGT * (1 - INDPLAF) + INVRETGTA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHGT = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHGT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHGT_1,RLOCHGT1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHGT ;

RLOCHGS_1 = max(min((INVRETGS * (1 - INDPLAF) + INVRETGSA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHGS = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHGS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHGS_1,RLOCHGS1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHGS ;

RLOCHGU_1 = max(min((INVRETGU * (1 - INDPLAF) + INVRETGUA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHGU = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHGU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHGU_1,RLOCHGU1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHGU ;

RLOCHGW_1 = max(min((INVRETGW * (1 - INDPLAF) + INVRETGWA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHGW = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHGW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHGW_1,RLOCHGW1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHGW ;

RLOCHETR_1 = max(min((INVRETETR * (1 - INDPLAF) + INVRETETRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHETR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHETR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHETR_1,RLOCHETR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHETR ;

RLOCHEOR_1 = max(min((INVRETEOR * (1 - INDPLAF) + INVRETEORA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHEOR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHEOR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHEOR_1,RLOCHEOR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHEOR ;

RLOCHESR_1 = max(min((INVRETESR * (1 - INDPLAF) + INVRETESRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHESR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHESR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHESR_1,RLOCHESR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHESR ;

RLOCHENR_1 = max(min((INVRETENR * (1 - INDPLAF) + INVRETENRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHENR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHENR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHENR_1,RLOCHENR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHENR ;

RLOCHFTR_1 = max(min((INVRETFTR * (1 - INDPLAF) + INVRETFTRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFTR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFTR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHFTR_1,RLOCHFTR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFTR ;

RLOCHFOR_1 = max(min((INVRETFOR * (1 - INDPLAF) + INVRETFORA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFOR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFOR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHFOR_1,RLOCHFOR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFOR ;

RLOCHFSR_1 = max(min((INVRETFSR * (1 - INDPLAF) + INVRETFSRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFSR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFSR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHFSR_1,RLOCHFSR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFSR ;

RLOCHFNR_1 = max(min((INVRETFNR * (1 - INDPLAF) + INVRETFNRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHFNR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHFNR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHFNR_1,RLOCHFNR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHFNR ;

RLOCHGTR_1 = max(min((INVRETGTR * (1 - INDPLAF) + INVRETGTRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHGTR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHGTR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHGTR_1,RLOCHGTR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHGTR ;

RLOCHGSR_1 = max(min((INVRETGSR * (1 - INDPLAF) + INVRETGSRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHGSR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHGSR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHGSR_1,RLOCHGSR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHGSR ;

RLOCHHT_1 = max(min((INVRETHT * (1 - INDPLAF) + INVRETHTA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHHT = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHHT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHHT_1,RLOCHHT1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHHT ;

RLOCHHS_1 = max(min((INVRETHS * (1 - INDPLAF) + INVRETHSA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHHS = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHHS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHHS_1,RLOCHHS1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHHS ;

RLOCHHU_1 = max(min((INVRETHU * (1 - INDPLAF) + INVRETHUA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHHU = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHHU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHHU_1,RLOCHHU1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHHU ;

RLOCHHW_1 = max(min((INVRETHW * (1 - INDPLAF) + INVRETHWA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHHW = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHHW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHHW_1,RLOCHHW1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHHW ;

RLOCHHTR_1 = max(min((INVRETHTR * (1 - INDPLAF) + INVRETHTRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHHTR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHHTR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHHTR_1,RLOCHHTR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHHTR ;

RLOCHHSR_1 = max(min((INVRETHSR * (1 - INDPLAF) + INVRETHSRA * INDPLAF) , RRILOC - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHHSR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHHSR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHHSR_1,RLOCHHSR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = 0 ;

regle 8404 :
application : iliad  ;

VARTMP1 = 0 ;
RRIRENT = RRISUP - RDOMSOC1 - RLOGSOC - RCOLENT ;

RLOCHIT_1 = max(min((INVRETIT * (1 - INDPLAF) + INVRETITA * INDPLAF) , RRIRENT - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHIT = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHIT_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHIT_1,RLOCHIT1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHIT ;

RLOCHIS_1 = max(min((INVRETIS * (1 - INDPLAF) + INVRETISA * INDPLAF) , RRIRENT - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHIS = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHIS_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHIS_1,RLOCHIS1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHIS ;

RLOCHIU_1 = max(min((INVRETIU * (1 - INDPLAF) + INVRETIUA * INDPLAF) , RRIRENT - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHIU = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHIU_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHIU_1,RLOCHIU1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHIU ;

RLOCHIW_1 = max(min((INVRETIW * (1 - INDPLAF) + INVRETIWA * INDPLAF) , RRIRENT - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHIW = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHIW_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
          + (max(0,min(RLOCHIW_1,RLOCHIW1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHIW ;

RLOCHITR_1 = max(min((INVRETITR * (1 - INDPLAF) + INVRETITRA * INDPLAF) , RRIRENT - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHITR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHITR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHITR_1,RLOCHITR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = VARTMP1 + RLOCHITR ;

RLOCHISR_1 = max(min((INVRETISR * (1 - INDPLAF) + INVRETISRA * INDPLAF) , RRIRENT - VARTMP1) , 0) * (1 - V_CNR) ;
RLOCHISR = positif(null(V_IND_TRAIT-4)+COD9ZA) * (RLOCHISR_1) * (1-positif(null(8-CMAJ)+null(11-CMAJ)+null(34-CMAJ)))
           + (max(0,min(RLOCHISR_1,RLOCHISR1731))*positif(1-COD9ZA)*(1-positif(PREM8_11))) * null(V_IND_TRAIT-5)+0;
VARTMP1 = 0;


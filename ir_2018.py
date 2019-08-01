# -*- coding: utf-8 -*-

from math import floor

class Undefined:
    def __init__(self):
        pass

    def __add__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __radd__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __sub__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __rsub__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __mul__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __rmul__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return rhs

    def __truediv__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return 0.0

    def __rtruediv__(self, lhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __lt__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __lte__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __gt__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __gte__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __eq__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()

    def __neq__(self, rhs):
        if isinstance(rhs, Undefined):
            return Undefined()
        else:
            return Undefined()


# 1BJ: Salaires - Declarant 2
# 1AJ: Salaires - Declarant 1
# 0CF: Nombre d'enfants mineurs ou handicapes
def main(var_1bj, var_1aj, var_0cf):

    # TPS10V: abattement 10% brut
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 117:10 to 117:40
    tps10v_811010_0 = round((var_1aj * 0.100000))

    # INDEFTSV: indicateur deficit TS frais reel sup declare TS
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 217:12 to 217:39
    indeftsv_811030_33 = (1 if (var_1aj >= 0) else 0)

    # TTPS10V: tx effectif abattement 10% brut
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 657:11 to 657:42
    ttps10v_99992100_0 = round((var_1aj * 0.100000))

    # NPA: Nombre de parts du aux personnes a charge
    # Defined in file ir-calcul/sources2017m_6_10/chap-6.m, from 67:7 to 67:32
    npa_601030_0 = (var_0cf - (0.500000 * (2 if (2 < var_0cf) else var_0cf)))

    # TPS10C: abattement 10% brut
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 118:10 to 118:40
    tps10c_811010_1 = round((var_1bj * 0.100000))

    # INDEFTSC: indicateur deficit TS frais reel sup declare TS
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 218:12 to 218:39
    indeftsc_811030_34 = (1 if (var_1bj >= 0) else 0)

    # TTPS10C: tx effectif abattement 10% brut
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 658:11 to 658:42
    ttps10c_99992100_1 = round((var_1bj * 0.100000))

    # DFNV: Deduction forfaitaire de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 126:9 to 126:39
    dfnv_811010_9 = (tps10v_811010_0 if (tps10v_811010_0 < 12305.000000) else 12305.000000)

    # TDFNV: tx effectif Deduction forfaitaire de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 663:10 to 663:41
    tdfnv_99992100_6 = (ttps10v_99992100_0 if (ttps10v_99992100_0 < 12305.000000) else 12305.000000)

    # NBPT: Nombre de parts
    # Defined in file ir-calcul/sources2017m_6_10/chap-6.m, from 46:9 to 46:49
    nbpt_601000_0 = ((((2 + npa_601030_0) * 10) / 10) if 10 != 0.0 else Undefined())

    # DFNC: Deduction forfaitaire de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 127:9 to 127:39
    dfnc_811010_10 = (tps10c_811010_1 if (tps10c_811010_1 < 12305.000000) else 12305.000000)

    # TDFNC: tx effectif Deduction forfaitaire de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 664:10 to 664:41
    tdfnc_99992100_7 = (ttps10c_99992100_1 if (ttps10c_99992100_1 < 12305.000000) else 12305.000000)

    # 10MINSV: deductions hors droits d'auteur plafonnees
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 151:11 to 151:42
    var_10minsv_811020_9 = (lambda v13563: (dfnv_811010_9 if (dfnv_811010_9 > v13563) else v13563))((430.000000 if (430.000000 < var_1aj) else var_1aj))

    # IND_10MIN_0V: booleen d'existence d'une deduction minimale hors D.A.
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 160:16 to 160:58
    ind_10min_0v_811020_18 = ((1 if ((430.000000 - dfnv_811010_9) > 0) else 0) * (1 if (var_1aj > 0) else 0))

    # T10MINSV: tx eff. deductions plafonnees
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 677:12 to 677:46
    t10minsv_99992200_6 = (lambda v22848: (tdfnv_99992100_6 if (tdfnv_99992100_6 > v22848) else v22848))((430.000000 if (430.000000 < var_1aj) else var_1aj))

    # TIND_10MIN_0V: booleen d'existence d'une deduction minimale tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 683:17 to 683:62
    tind_10min_0v_99992200_12 = ((1 if ((430.000000 - tdfnv_99992100_6) > 0) else 0) * (1 if (var_1aj > 0) else 0))

    # PLANT: Plafond de l'avantage QF anterieur a 98
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 1101:10 to 1106:26
    plant_511350_1 = (1527.000000 * (2 * ((nbpt_601000_0 - 1) - 1)))

    # Verification condition in file ir-calcul/sources2017m_6_10/coc1.m, from 338:4 to 338:13
    cond = (nbpt_601000_0 > 20)
    if cond:
        raise TypeError("Error triggered\nA015: A:015:00:ATTENTION, CALCUL NON EFFECTUE PAR L'ESI:N")

    # 10MINSC: deductions hors droits d'auteur plafonnees
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 152:11 to 152:42
    var_10minsc_811020_10 = (lambda v13567: (dfnc_811010_10 if (dfnc_811010_10 > v13567) else v13567))((430.000000 if (430.000000 < var_1bj) else var_1bj))

    # IND_10MIN_0C: booleen d'existence d'une deduction minimale hors D.A.
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 161:16 to 161:58
    ind_10min_0c_811020_19 = ((1 if ((430.000000 - dfnc_811010_10) > 0) else 0) * (1 if (var_1bj > 0) else 0))

    # T10MINSC: tx eff. deductions plafonnees
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 678:12 to 678:46
    t10minsc_99992200_7 = (lambda v22852: (tdfnc_99992100_7 if (tdfnc_99992100_7 > v22852) else v22852))((430.000000 if (430.000000 < var_1bj) else var_1bj))

    # TIND_10MIN_0C: booleen d'existence d'une deduction minimale tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 684:17 to 684:62
    tind_10min_0c_99992200_13 = ((1 if ((430.000000 - tdfnc_99992100_7) > 0) else 0) * (1 if (var_1bj > 0) else 0))

    # IND_10V: Indicateur d'affichage deduction 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 201:11 to 201:42
    ind_10v_811030_17 = (1 if (var_10minsv_811020_9 >= 0) else 0)

    # FPTV: deduction de 10% ou frais reels
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 211:8 to 211:26
    fptv_811030_27 = (var_10minsv_811020_9 if (var_10minsv_811020_9 > False) else False)

    # IND_MINV: 
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 170:12 to 170:39
    ind_minv_811020_27 = (1 - (1 if (ind_10min_0v_811020_18 > 0) else 0))

    # TIND_10V: Indicateur d'affichage deduction 10% tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 719:12 to 719:55
    tind_10v_99992300_15 = (1 if (t10minsv_99992200_6 >= 0) else 0)

    # TFPTV: deduction de 10% ou frais reels tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 725:9 to 725:29
    tfptv_99992300_21 = (t10minsv_99992200_6 if (t10minsv_99992200_6 > False) else False)

    # TIND_MINV: pour tx effectif 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 689:13 to 689:41
    tind_minv_99992200_18 = (1 - (1 if (tind_10min_0v_99992200_12 > 0) else 0))

    # DEFRITS: Vaut 1 si deficit et ou reduction different de 0 et majo 1731 bis 
    # Defined in file ir-calcul/sources2017m_6_10/chap-ini.m, from 1054:11 to 1060:62
    defrits_951210_0 = (1 if (((1 if ((- (var_10minsv_811020_9 if (var_10minsv_811020_9 > False) else False)) > 0) else 0) + (1 if ((- (var_10minsc_811020_10 if (var_10minsc_811020_10 > False) else False)) > 0) else 0)) > 0) else 0)

    # IND_10C: Indicateur d'affichage deduction 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 202:11 to 202:42
    ind_10c_811030_18 = (1 if (var_10minsc_811020_10 >= 0) else 0)

    # FPTC: deduction de 10% ou frais reels
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 212:8 to 212:26
    fptc_811030_28 = (var_10minsc_811020_10 if (var_10minsc_811020_10 > False) else False)

    # IND_MINC: 
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 171:12 to 171:39
    ind_minc_811020_28 = (1 - (1 if (ind_10min_0c_811020_19 > 0) else 0))

    # TIND_10C: Indicateur d'affichage deduction 10% tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 720:12 to 720:55
    tind_10c_99992300_16 = (1 if (t10minsc_99992200_7 >= 0) else 0)

    # TFPTC: deduction de 10% ou frais reels tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 726:9 to 726:29
    tfptc_99992300_22 = (t10minsc_99992200_7 if (t10minsc_99992200_7 > False) else False)

    # TIND_MINC: pour tx effectif 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 690:13 to 690:41
    tind_minc_99992200_19 = (1 - (1 if (tind_10min_0c_99992200_13 > 0) else 0))

    # D10MV: deduction 10% hors DA apres prise en compte de la deduction minimale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 226:9 to 226:49
    d10mv_811030_42 = ((ind_minv_811020_27 * dfnv_811010_9) + ((1 - ind_minv_811020_27) * var_10minsv_811020_9))

    # TD10MV: deduction 10% apres prise en compte de la deduction minimale  effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 731:10 to 731:54
    td10mv_99992300_27 = ((tind_minv_99992200_18 * tdfnv_99992100_6) + ((1 - tind_minv_99992200_18) * t10minsv_99992200_6))

    # DEFRI: Vaut 1 si deficit et ou reduction different de 0 et majo 1731 bis 
    # Defined in file ir-calcul/sources2017m_6_10/chap-ini.m, from 990:9 to 991:75
    defri_951180_0 = (1 if (defrits_951210_0 > 0) else 0)

    # BCOS: Base reduction cotisations syndicales
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 441:8 to 445:64
    bcos_301257_3 = ((lambda v15767: (v15767 if (v15767 < 0) else 0))(round((0.010000 * (var_1aj * ind_10v_811030_17)))) + (lambda v15769: (v15769 if (v15769 < 0) else 0))(round((0.010000 * (var_1bj * ind_10c_811030_18)))))

    # D10MC: deduction 10% hors DA apres prise en compte de la deduction minimale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 227:9 to 227:49
    d10mc_811030_43 = ((ind_minc_811020_28 * dfnc_811010_10) + ((1 - ind_minc_811020_28) * var_10minsc_811020_10))

    # TD10MC: deduction 10% apres prise en compte de la deduction minimale  effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 732:10 to 732:54
    td10mc_99992300_28 = ((tind_minc_99992200_19 * tdfnc_99992100_7) + ((1 - tind_minc_99992200_19) * t10minsc_99992200_7))

    # REP10V: frais de 10% servant aux abtt. de 10% pour GL et SA
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 238:11 to 238:47
    rep10v_811030_51 = ((ind_10v_811030_17 * d10mv_811030_42) + ((1 - ind_10v_811030_17) * fptv_811030_27))

    # TREP10V: frais de 10% servant aux abtt. de 10% pour GL et SA eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 737:12 to 737:52
    trep10v_99992300_33 = ((tind_10v_99992300_15 * td10mv_99992300_27) + ((1 - tind_10v_99992300_15) * tfptv_99992300_21))

    # DEFRIMAJ: valorise DEFRI a chaque strate pour message IM43
    # Defined in file ir-calcul/sources2017m_6_10/chap-ini.m, from 993:12 to 993:41
    defrimaj_951180_1 = (1 if (defri_951180_0 > 0) else 0)

    # CISYND: CI cotis. syndicale
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 447:10 to 447:52
    cisynd_301257_4 = round((0.660000 * bcos_301257_3))

    # REP10C: frais de 10% servant aux abtt. de 10% pour GL et SA
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 239:11 to 239:47
    rep10c_811030_52 = ((ind_10c_811030_18 * d10mc_811030_43) + ((1 - ind_10c_811030_18) * fptc_811030_28))

    # TREP10C: frais de 10% servant aux abtt. de 10% pour GL et SA eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 738:12 to 738:52
    trep10c_99992300_34 = ((tind_10c_99992300_16 * td10mc_99992300_28) + ((1 - tind_10c_99992300_16) * tfptc_99992300_22))

    # ABTS1AJ: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 252:9 to 252:36
    abts1aj_811040_0 = round((rep10v_811030_51 * ((var_1aj / var_1aj) if var_1aj != 0.0 else Undefined())))

    # TABTS1PM: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 745:11 to 746:158
    tabts1pm_99992300_40 = (lambda v22881: (v22881 if (v22881 > 0) else 0))((trep10v_99992300_33 - trep10v_99992300_33))

    # TTSN1AJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 895:11 to 895:30
    ttsn1aj_899999999_0 = (var_1aj - trep10v_99992300_33)

    # Verification condition in file ir-calcul/sources2017m_6_10/coi3.m, from 234:24 to 234:54
    cond = ((defri_951180_0 == 0) and (defrimaj_951180_1 == 1))
    if cond:
        raise TypeError("Error triggered\nIM43: I:M43:00:ART. 1731 BIS : APPLICATION POUR LE CALCUL DE CERTAINES MAJORATIONS:N")

    # ABTS1BJ: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 296:9 to 296:36
    abts1bj_811040_17 = round((rep10c_811030_52 * ((var_1bj / var_1bj) if var_1bj != 0.0 else Undefined())))

    # TABTS1QM: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 776:11 to 777:158
    tabts1qm_99992300_56 = (lambda v22909: (v22909 if (v22909 > 0) else 0))((trep10c_99992300_34 - trep10c_99992300_34))

    # TTSN1BJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 912:11 to 912:30
    ttsn1bj_899999999_17 = (var_1bj - trep10c_99992300_34)

    # ABTS1PM: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 253:10 to 254:157
    abts1pm_811040_1 = (lambda v13614: (v13614 if (v13614 > 0) else 0))((rep10v_811030_51 - abts1aj_811040_0))

    # TSN1AJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 415:10 to 415:28
    tsn1aj_811060_0 = (var_1aj - abts1aj_811040_0)

    # TTSN1PM: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 896:11 to 896:28
    ttsn1pm_899999999_1 = (- tabts1pm_99992300_40)

    # TABTS1TP: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 747:11 to 748:160
    tabts1tp_99992300_41 = (lambda v22883: (v22883 if (v22883 > 0) else 0))(((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40))

    # ABTS1QM: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 297:10 to 298:157
    abts1qm_811040_18 = (lambda v13644: (v13644 if (v13644 > 0) else 0))((rep10c_811030_52 - abts1bj_811040_17))

    # TSN1BJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 431:10 to 431:28
    tsn1bj_811060_16 = (var_1bj - abts1bj_811040_17)

    # TTSN1QM: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 913:11 to 913:28
    ttsn1qm_899999999_18 = (- tabts1qm_99992300_56)

    # TABTS1UP: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 778:11 to 779:160
    tabts1up_99992300_57 = (lambda v22911: (v22911 if (v22911 > 0) else 0))(((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56))

    # TSN1PM: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 416:10 to 416:26
    tsn1pm_811060_1 = (- abts1pm_811040_1)

    # ABTS1TP: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 256:10 to 257:158
    abts1tp_811040_2 = (lambda v13616: (v13616 if (v13616 > 0) else 0))(((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1))

    # TTSN1TP: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 897:11 to 897:28
    ttsn1tp_899999999_2 = (- tabts1tp_99992300_41)

    # TABTS1NX: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 749:11 to 750:162
    tabts1nx_99992300_42 = (lambda v22885: (v22885 if (v22885 > 0) else 0))((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41))

    # TSN1QM: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 432:10 to 432:26
    tsn1qm_811060_17 = (- abts1qm_811040_18)

    # ABTS1UP: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 300:10 to 301:158
    abts1up_811040_19 = (lambda v13646: (v13646 if (v13646 > 0) else 0))(((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18))

    # TTSN1UP: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 914:11 to 914:28
    ttsn1up_899999999_19 = (- tabts1up_99992300_57)

    # TABTS1OX: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 780:11 to 781:162
    tabts1ox_99992300_58 = (lambda v22913: (v22913 if (v22913 > 0) else 0))((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57))

    # TSN1TP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 417:10 to 417:26
    tsn1tp_811060_2 = (- abts1tp_811040_2)

    # ABTS1NX: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 259:10 to 260:159
    abts1nx_811040_3 = (lambda v13618: (v13618 if (v13618 > 0) else 0))((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2))

    # TTSN1NX: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 898:11 to 898:28
    ttsn1nx_899999999_3 = (- tabts1nx_99992300_42)

    # TABTS1AF: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 751:11 to 752:163
    tabts1af_99992300_43 = (lambda v22887: (v22887 if (v22887 > 0) else 0))(((((rep10v_811030_51 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42))

    # TSN1UP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 433:10 to 433:26
    tsn1up_811060_18 = (- abts1up_811040_19)

    # ABTS1OX: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 303:10 to 304:159
    abts1ox_811040_20 = (lambda v13648: (v13648 if (v13648 > 0) else 0))((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19))

    # TTSN1OX: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 915:11 to 915:28
    ttsn1ox_899999999_20 = (- tabts1ox_99992300_58)

    # TABTS1BF: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 782:11 to 783:164
    tabts1bf_99992300_59 = (lambda v22915: (v22915 if (v22915 > 0) else 0))(((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58))

    # TSN1NX: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 418:10 to 418:26
    tsn1nx_811060_3 = (- abts1nx_811040_3)

    # ABTS1AF: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 262:10 to 263:160
    abts1af_811040_4 = (lambda v13620: (v13620 if (v13620 > 0) else 0))(((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3))

    # TTSN1AF: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 899:11 to 899:28
    ttsn1af_899999999_4 = (- tabts1af_99992300_43)

    # TABTS1AG: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 753:11 to 754:166
    tabts1ag_99992300_44 = (lambda v22889: (v22889 if (v22889 > 0) else 0))((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43))

    # TSN1OX: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 434:10 to 434:26
    tsn1ox_811060_19 = (- abts1ox_811040_20)

    # ABTS1BF: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 306:10 to 307:160
    abts1bf_811040_21 = (lambda v13650: (v13650 if (v13650 > 0) else 0))(((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20))

    # TTSN1BF: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 916:11 to 916:28
    ttsn1bf_899999999_21 = (- tabts1bf_99992300_59)

    # TABTS1BG: part de l'abattement pour frais sur TS normaux tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 784:11 to 785:166
    tabts1bg_99992300_60 = (lambda v22917: (v22917 if (v22917 > 0) else 0))((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59))

    # TSN1AF: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 419:10 to 419:26
    tsn1af_811060_4 = (- abts1af_811040_4)

    # ABTS1AG: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 265:10 to 266:161
    abts1ag_811040_5 = (lambda v13622: (v13622 if (v13622 > 0) else 0))((((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3) - abts1af_811040_4))

    # TTSN1AG: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 900:11 to 900:28
    ttsn1ag_899999999_5 = (- tabts1ag_99992300_44)

    # TABTS1AC: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 755:10 to 756:167
    tabts1ac_99992300_45 = (lambda v22891: (v22891 if (v22891 > 0) else 0))(((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43) - tabts1ag_99992300_44))

    # TSN1BF: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 435:10 to 435:26
    tsn1bf_811060_20 = (- abts1bf_811040_21)

    # ABTS1BG: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 309:10 to 310:161
    abts1bg_811040_22 = (lambda v13652: (v13652 if (v13652 > 0) else 0))((((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20) - abts1bf_811040_21))

    # TTSN1BG: Traitements et salaires nets de frais tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 917:11 to 917:28
    ttsn1bg_899999999_22 = (- tabts1bg_99992300_60)

    # TABTS1BC: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 786:10 to 787:167
    tabts1bc_99992300_61 = (lambda v22919: (v22919 if (v22919 > 0) else 0))(((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59) - tabts1bg_99992300_60))

    # TSN1AG: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 420:10 to 420:26
    tsn1ag_811060_5 = (- abts1ag_811040_5)

    # ABTS1AP: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 268:10 to 269:163
    abts1ap_811040_6 = (lambda v13624: (v13624 if (v13624 > 0) else 0))(((((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3) - abts1af_811040_4) - abts1ag_811040_5))

    # TTSN1AC: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 901:11 to 901:28
    ttsn1ac_899999999_6 = (- tabts1ac_99992300_45)

    # TABTS1AP: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 757:10 to 758:170
    tabts1ap_99992300_46 = (lambda v22893: (v22893 if (v22893 > 0) else 0))((((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43) - tabts1ag_99992300_44) - tabts1ac_99992300_45))

    # TSN1BG: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 436:10 to 436:26
    tsn1bg_811060_21 = (- abts1bg_811040_22)

    # ABTS1BP: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 312:10 to 313:163
    abts1bp_811040_23 = (lambda v13654: (v13654 if (v13654 > 0) else 0))(((((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20) - abts1bf_811040_21) - abts1bg_811040_22))

    # TTSN1BC: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 918:11 to 918:28
    ttsn1bc_899999999_23 = (- tabts1bc_99992300_61)

    # TABTS1BP: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 788:10 to 789:170
    tabts1bp_99992300_62 = (lambda v22921: (v22921 if (v22921 > 0) else 0))((((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59) - tabts1bg_99992300_60) - tabts1bc_99992300_61))

    # TSN1AP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 421:10 to 421:25
    tsn1ap_811060_6 = (- abts1ap_811040_6)

    # ABTS3VJ: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 271:10 to 272:163
    abts3vj_811040_7 = (lambda v13626: (v13626 if (v13626 > 0) else 0))((((((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3) - abts1af_811040_4) - abts1ag_811040_5) - abts1ap_811040_6))

    # TTSN1AP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 902:11 to 902:27
    ttsn1ap_899999999_7 = (- tabts1ap_99992300_46)

    # TABTS3VJ: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 759:10 to 760:171
    tabts3vj_99992300_47 = (lambda v22895: (v22895 if (v22895 > 0) else 0))(((((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43) - tabts1ag_99992300_44) - tabts1ac_99992300_45) - tabts1ap_99992300_46))

    # TSN1BP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 437:10 to 437:25
    tsn1bp_811060_22 = (- abts1bp_811040_23)

    # ABTS3VK: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 315:10 to 316:163
    abts3vk_811040_24 = (lambda v13656: (v13656 if (v13656 > 0) else 0))((((((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20) - abts1bf_811040_21) - abts1bg_811040_22) - abts1bp_811040_23))

    # TTSN1BP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 919:11 to 919:27
    ttsn1bp_899999999_24 = (- tabts1bp_99992300_62)

    # TABTS3VK: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 790:10 to 791:171
    tabts3vk_99992300_63 = (lambda v22923: (v22923 if (v22923 > 0) else 0))(((((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1bc_99992300_61) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59) - tabts1bg_99992300_60) - tabts1bp_99992300_62))

    # TSN3VJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 422:10 to 422:27
    tsn3vj_811060_7 = (- abts3vj_811040_7)

    # ABTS1TT: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 274:10 to 275:162
    abts1tt_811040_8 = (lambda v13628: (v13628 if (v13628 > 0) else 0))(((((((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3) - abts1af_811040_4) - abts1ag_811040_5) - abts1ap_811040_6) - abts3vj_811040_7))

    # TTSN3VJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 903:11 to 903:29
    ttsn3vj_899999999_8 = (- tabts3vj_99992300_47)

    # TABTS1TT: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 761:10 to 762:171
    tabts1tt_99992300_48 = (lambda v22897: (v22897 if (v22897 > 0) else 0))((((((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43) - tabts1ag_99992300_44) - tabts1ac_99992300_45) - tabts1ap_99992300_46) - tabts3vj_99992300_47))

    # TSN3VK: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 438:10 to 438:27
    tsn3vk_811060_23 = (- abts3vk_811040_24)

    # ABTS1UT: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 318:10 to 319:162
    abts1ut_811040_25 = (lambda v13658: (v13658 if (v13658 > 0) else 0))(((((((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20) - abts1bf_811040_21) - abts1bg_811040_22) - abts1bp_811040_23) - abts3vk_811040_24))

    # TTSN3VK: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 920:11 to 920:29
    ttsn3vk_899999999_25 = (- tabts3vk_99992300_63)

    # TABTS1UT: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 792:10 to 793:171
    tabts1ut_99992300_64 = (lambda v22925: (v22925 if (v22925 > 0) else 0))((((((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59) - tabts1bg_99992300_60) - tabts1bc_99992300_61) - tabts1bp_99992300_62) - tabts3vk_99992300_63))

    # TABTS1HB: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 794:11 to 795:190
    tabts1hb_99992300_65 = (lambda v22927: (v22927 if (v22927 > 0) else 0))((((((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59) - tabts1bg_99992300_60) - tabts1bc_99992300_61) - tabts1bp_99992300_62) - tabts3vk_99992300_63))

    # TSN1TT: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 423:10 to 423:28
    tsn1tt_811060_8 = (- abts1tt_811040_8)

    # ABTS1GB: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 277:10 to 278:163
    abts1gb_811040_9 = (lambda v13630: (v13630 if (v13630 > 0) else 0))((((((((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3) - abts1af_811040_4) - abts1ag_811040_5) - abts1ap_811040_6) - abts3vj_811040_7) - abts1tt_811040_8))

    # TTSN1TT: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 904:11 to 904:30
    ttsn1tt_899999999_9 = (- tabts1tt_99992300_48)

    # TABTS1GB: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 763:11 to 764:196
    tabts1gb_99992300_49 = (lambda v22899: (v22899 if (v22899 > 0) else 0))(((((((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43) - tabts1ag_99992300_44) - tabts1ac_99992300_45) - tabts1ap_99992300_46) - tabts3vj_99992300_47) - tabts1tt_99992300_48))

    # TSN1UT: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 439:10 to 439:28
    tsn1ut_811060_24 = (- abts1ut_811040_25)

    # ABTS1HB: part de l'abattement pour frais sur TS 
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 321:10 to 322:163
    abts1hb_811040_26 = (lambda v13660: (v13660 if (v13660 > 0) else 0))((((((((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20) - abts1bf_811040_21) - abts1bg_811040_22) - abts1bp_811040_23) - abts3vk_811040_24) - abts1ut_811040_25))

    # TTSN1UT: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 921:11 to 921:30
    ttsn1ut_899999999_26 = (- tabts1ut_99992300_64)

    # TTSN1HB: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 926:12 to 926:29
    ttsn1hb_899999999_31 = (- tabts1hb_99992300_65)

    # TABTSRBJ: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 796:10 to 797:175
    tabtsrbj_99992300_66 = (lambda v22929: (v22929 if (v22929 > 0) else 0))((((((((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59) - tabts1bg_99992300_60) - tabts1bc_99992300_61) - tabts1bp_99992300_62) - tabts3vk_99992300_63) - tabts1ut_99992300_64) - tabts1hb_99992300_65))

    # TSN1GB: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 428:11 to 428:27
    tsn1gb_811060_13 = (- abts1gb_811040_9)

    # ABTSRAJ: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 280:10 to 281:164
    abtsraj_811040_10 = (lambda v13632: (v13632 if (v13632 > 0) else 0))(((((((((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3) - abts1af_811040_4) - abts1ag_811040_5) - abts1ap_811040_6) - abts3vj_811040_7) - abts1tt_811040_8) - abts1gb_811040_9))

    # TTSN1GB: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 909:12 to 909:29
    ttsn1gb_899999999_14 = (- tabts1gb_99992300_49)

    # TABTSRAJ: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 765:10 to 766:175
    tabtsraj_99992300_50 = (lambda v22901: (v22901 if (v22901 > 0) else 0))((((((((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43) - tabts1ag_99992300_44) - tabts1ac_99992300_45) - tabts1ap_99992300_46) - tabts3vj_99992300_47) - tabts1tt_99992300_48) - tabts1gb_99992300_49))

    # TSN1HB: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 444:11 to 444:27
    tsn1hb_811060_29 = (- abts1hb_811040_26)

    # ABTSRBJ: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 324:10 to 325:164
    abtsrbj_811040_27 = (lambda v13662: (v13662 if (v13662 > 0) else 0))(((((((((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20) - abts1bf_811040_21) - abts1bg_811040_22) - abts1bp_811040_23) - abts3vk_811040_24) - abts1ut_811040_25) - abts1hb_811040_26))

    # TTSNRBJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 922:12 to 922:29
    ttsnrbj_899999999_27 = (- tabtsrbj_99992300_66)

    # TABTSRBP: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 798:10 to 799:176
    tabtsrbp_99992300_67 = (lambda v22931: (v22931 if (v22931 > 0) else 0))(((((((((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59) - tabts1bg_99992300_60) - tabts1bc_99992300_61) - tabts1bp_99992300_62) - tabts3vk_99992300_63) - tabts1ut_99992300_64) - tabtsrbj_99992300_66) - tabts1hb_99992300_65))

    # TSNRAJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 424:11 to 424:27
    tsnraj_811060_9 = (- abtsraj_811040_10)

    # ABTSRAP: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 283:10 to 284:164
    abtsrap_811040_11 = (lambda v13634: (v13634 if (v13634 > 0) else 0))((((((((((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3) - abts1af_811040_4) - abts1ag_811040_5) - abts1ap_811040_6) - abts1tt_811040_8) - abts1gb_811040_9) - abts3vj_811040_7) - abtsraj_811040_10))

    # TTSNRAJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 905:12 to 905:29
    ttsnraj_899999999_10 = (- tabtsraj_99992300_50)

    # TABTSRAP: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 767:10 to 768:170
    tabtsrap_99992300_51 = (lambda v22903: (v22903 if (v22903 > 0) else 0))(((((((((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43) - tabts1ag_99992300_44) - tabts1ac_99992300_45) - tabts1ap_99992300_46) - tabts3vj_99992300_47) - tabts1tt_99992300_48) - tabts1gb_99992300_49) - tabtsraj_99992300_50))

    # TSNRBJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 440:11 to 440:27
    tsnrbj_811060_25 = (- abtsrbj_811040_27)

    # ABTSRBP: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 327:10 to 328:164
    abtsrbp_811040_28 = (lambda v13664: (v13664 if (v13664 > 0) else 0))((((((((((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20) - abts1bf_811040_21) - abts1bg_811040_22) - abts1bp_811040_23) - abts3vk_811040_24) - abts1ut_811040_25) - abts1hb_811040_26) - abtsrbj_811040_27))

    # TTSNRBP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 923:12 to 923:30
    ttsnrbp_899999999_28 = (- tabtsrbp_99992300_67)

    # TABTSRBF: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 800:11 to 801:190
    tabtsrbf_99992300_68 = (lambda v22933: (v22933 if (v22933 > 0) else 0))((((((((((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59) - tabts1bg_99992300_60) - tabts1bc_99992300_61) - tabts1bp_99992300_62) - tabts3vk_99992300_63) - tabts1ut_99992300_64) - tabts1hb_99992300_65) - tabtsrbj_99992300_66) - tabtsrbp_99992300_67))

    # TSNRAP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 425:11 to 425:28
    tsnrap_811060_10 = (- abtsrap_811040_11)

    # ABTSRAF: part de l'abattement pour frais sur TS 
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 286:10 to 287:165
    abtsraf_811040_12 = (lambda v13636: (v13636 if (v13636 > 0) else 0))(((((((((((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3) - abts1af_811040_4) - abts1ag_811040_5) - abts1ap_811040_6) - abts1tt_811040_8) - abts1gb_811040_9) - abts3vj_811040_7) - abtsraj_811040_10) - abtsrap_811040_11))

    # TTSNRAP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 906:12 to 906:30
    ttsnrap_899999999_11 = (- tabtsrap_99992300_51)

    # TABTSRAF: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 769:11 to 770:187
    tabtsraf_99992300_52 = (lambda v22905: (v22905 if (v22905 > 0) else 0))((((((((((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43) - tabts1ag_99992300_44) - tabts1ac_99992300_45) - tabts1ap_99992300_46) - tabts1tt_99992300_48) - tabts1gb_99992300_49) - tabts3vj_99992300_47) - tabtsraj_99992300_50) - tabtsrap_99992300_51))

    # TSNRBP: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 441:11 to 441:28
    tsnrbp_811060_26 = (- abtsrbp_811040_28)

    # ABTSRBF: part de l'abattement pour frais sur TS 
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 330:10 to 331:165
    abtsrbf_811040_29 = (lambda v13666: (v13666 if (v13666 > 0) else 0))(((((((((((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20) - abts1bf_811040_21) - abts1bg_811040_22) - abts1bp_811040_23) - abts3vk_811040_24) - abts1ut_811040_25) - abts1hb_811040_26) - abtsrbj_811040_27) - abtsrbp_811040_28))

    # TTSNRBF: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 927:12 to 927:29
    ttsnrbf_899999999_32 = (- tabtsrbf_99992300_68)

    # TABTSRBG: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 802:11 to 803:191
    tabtsrbg_99992300_69 = (lambda v22935: (v22935 if (v22935 > 0) else 0))(((((((((((((((trep10c_99992300_34 - trep10c_99992300_34) - tabts1qm_99992300_56) - tabts1up_99992300_57) - tabts1ox_99992300_58) - tabts1bf_99992300_59) - tabts1bg_99992300_60) - tabts1bc_99992300_61) - tabts1bp_99992300_62) - tabts3vk_99992300_63) - tabts1ut_99992300_64) - tabts1hb_99992300_65) - tabtsrbj_99992300_66) - tabtsrbf_99992300_68) - tabtsrbp_99992300_67))

    # TSNRAF: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 429:11 to 429:27
    tsnraf_811060_14 = (- abtsraf_811040_12)

    # ABTSRAG: part de l'abattement pour frais sur TS 
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 289:10 to 290:166
    abtsrag_811040_13 = (lambda v13638: (v13638 if (v13638 > 0) else 0))((((((((((((((rep10v_811030_51 - abts1aj_811040_0) - abts1pm_811040_1) - abts1tp_811040_2) - abts1nx_811040_3) - abts1af_811040_4) - abts1ag_811040_5) - abts1ap_811040_6) - abts1tt_811040_8) - abts1gb_811040_9) - abts3vj_811040_7) - abtsraj_811040_10) - abtsrap_811040_11) - abtsraf_811040_12))

    # TTSNRAF: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 910:12 to 910:29
    ttsnraf_899999999_15 = (- tabtsraf_99992300_52)

    # TABTSRAG: part de l'abattement pour frais sur TS normaux eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 771:11 to 772:191
    tabtsrag_99992300_53 = (lambda v22907: (v22907 if (v22907 > 0) else 0))(((((((((((((((trep10v_99992300_33 - trep10v_99992300_33) - tabts1pm_99992300_40) - tabts1tp_99992300_41) - tabts1nx_99992300_42) - tabts1af_99992300_43) - tabts1ag_99992300_44) - tabts1ac_99992300_45) - tabts1ap_99992300_46) - tabts1tt_99992300_48) - tabts1gb_99992300_49) - tabts3vj_99992300_47) - tabtsraj_99992300_50) - tabtsrap_99992300_51) - tabtsraf_99992300_52))

    # TSNRBF: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 445:11 to 445:27
    tsnrbf_811060_30 = (- abtsrbf_811040_29)

    # ABTSRBG: part de l'abattement pour frais sur TS 
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 333:10 to 334:166
    abtsrbg_811040_30 = (lambda v13668: (v13668 if (v13668 > 0) else 0))((((((((((((((rep10c_811030_52 - abts1bj_811040_17) - abts1qm_811040_18) - abts1up_811040_19) - abts1ox_811040_20) - abts1bf_811040_21) - abts1bg_811040_22) - abts1bp_811040_23) - abts3vk_811040_24) - abts1ut_811040_25) - abts1hb_811040_26) - abtsrbj_811040_27) - abtsrbf_811040_29) - abtsrbp_811040_28))

    # TTSNRBG: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 928:12 to 928:29
    ttsnrbg_899999999_33 = (- tabtsrbg_99992300_69)

    # TABTSC: part de l'abattement pour frais sur TS normaux tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 804:10 to 804:150
    tabtsc_99992300_70 = (trep10c_99992300_34 + (tabts1qm_99992300_56 + (tabts1up_99992300_57 + (tabts1ox_99992300_58 + (tabts1bf_99992300_59 + (tabts1bg_99992300_60 + (tabts1bc_99992300_61 + (tabts1bp_99992300_62 + (tabts3vk_99992300_63 + (tabts1ut_99992300_64 + (tabtsrbj_99992300_66 + (tabtsrbp_99992300_67 + (tabts1hb_99992300_65 + (tabtsrbf_99992300_68 + tabtsrbg_99992300_69))))))))))))))

    # TSNRAG: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 430:11 to 430:27
    tsnrag_811060_15 = (- abtsrag_811040_13)

    # ABTSV: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 292:9 to 292:135
    abtsv_811040_14 = (abts1aj_811040_0 + (abts1pm_811040_1 + (abts1tp_811040_2 + (abts1nx_811040_3 + (abts1af_811040_4 + (abts1ag_811040_5 + (abts1ap_811040_6 + (abts3vj_811040_7 + (abts1tt_811040_8 + (abts1gb_811040_9 + (abtsraj_811040_10 + (abtsrap_811040_11 + (abtsraf_811040_12 + abtsrag_811040_13)))))))))))))

    # TTSNRAG: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 911:12 to 911:29
    ttsnrag_899999999_16 = (- tabtsrag_99992300_53)

    # TABTSV: part de l'abattement pour frais sur TS normaux tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 773:10 to 773:150
    tabtsv_99992300_54 = (trep10v_99992300_33 + (tabts1pm_99992300_40 + (tabts1tp_99992300_41 + (tabts1nx_99992300_42 + (tabts1af_99992300_43 + (tabts1ag_99992300_44 + (tabts1ac_99992300_45 + (tabts1ap_99992300_46 + (tabts3vj_99992300_47 + (tabts1tt_99992300_48 + (tabtsraj_99992300_50 + (tabtsrap_99992300_51 + (tabts1gb_99992300_49 + (tabtsraf_99992300_52 + tabtsrag_99992300_53))))))))))))))

    # TSNRBG: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 446:11 to 446:27
    tsnrbg_811060_31 = (- abtsrbg_811040_30)

    # ABTSC: part de l'abattement pour frais sur TS normaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 336:9 to 336:125
    abtsc_811040_31 = (abts1bj_811040_17 + (abts1qm_811040_18 + (abts1up_811040_19 + (abts1ox_811040_20 + (abts1bf_811040_21 + (abts1bg_811040_22 + (abts1bp_811040_23 + (abts3vk_811040_24 + (abts1ut_811040_25 + (abtsrbj_811040_27 + (abtsrbp_811040_28 + (abts1hb_811040_26 + (abtsrbg_811040_30 + abtsrbf_811040_29)))))))))))))

    # TABDOMDBJ: Abattement frais pro proratise pour EAJ tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 810:13 to 811:59
    tabdomdbj_99992500_2 = (lambda v22941: (v22941 if (v22941 > 0) else 0))((trep10c_99992300_34 - tabtsc_99992300_70))

    # ABDOMDAJ: Abattement frais pro proratise pour DAJ
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 293:12 to 294:50
    abdomdaj_811040_15 = (lambda v13640: (v13640 if (v13640 > 0) else 0))((rep10v_811030_51 - abtsv_811040_14))

    # TABDOMDAJ: Abattement frais pro proratise pour EAJ tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 807:13 to 808:59
    tabdomdaj_99992500_0 = (lambda v22937: (v22937 if (v22937 > 0) else 0))((trep10v_99992300_33 - tabtsv_99992300_54))

    # ABDOMDBJ: Abattement frais pro proratise pour DBJ
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 337:12 to 338:50
    abdomdbj_811040_32 = (lambda v13670: (v13670 if (v13670 > 0) else 0))((rep10c_811030_52 - abtsc_811040_31))

    # TTSNDBJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 924:12 to 924:30
    ttsndbj_899999999_29 = (- tabdomdbj_99992500_2)

    # TABDOMEBJ: Abattement frais pro proratise pour EAJ tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 812:13 to 812:46
    tabdomebj_99992500_3 = (lambda v22943: (v22943 if (v22943 > 0) else 0))(((trep10c_99992300_34 - tabtsc_99992300_70) - tabdomdbj_99992500_2))

    # TSNDAJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 426:11 to 426:28
    tsndaj_811060_11 = (- abdomdaj_811040_15)

    # ABDOMEAJ: Abattement frais pro proratise pour EAJ
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 295:12 to 295:42
    abdomeaj_811040_16 = (lambda v13642: (v13642 if (v13642 > 0) else 0))(((rep10v_811030_51 - abtsv_811040_14) - abdomdaj_811040_15))

    # TTSNDAJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 907:12 to 907:30
    ttsndaj_899999999_12 = (- tabdomdaj_99992500_0)

    # TABDOMEAJ: Abattement frais pro proratise pour EAJ tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 809:14 to 809:47
    tabdomeaj_99992500_1 = (lambda v22939: (v22939 if (v22939 > 0) else 0))(((trep10v_99992300_33 - tabtsv_99992300_54) - tabdomdaj_99992500_0))

    # TSNDBJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 442:11 to 442:28
    tsndbj_811060_27 = (- abdomdbj_811040_32)

    # ABDOMEBJ: Abattement frais pro proratise pour EBJ
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 339:12 to 339:42
    abdomebj_811040_33 = (lambda v13672: (v13672 if (v13672 > 0) else 0))(((rep10c_811030_52 - abtsc_811040_31) - abdomdbj_811040_32))

    # TTSNEBJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 925:12 to 925:30
    ttsnebj_899999999_30 = (- tabdomebj_99992500_3)

    # TSNEAJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 427:11 to 427:28
    tsneaj_811060_12 = (- abdomeaj_811040_16)

    # GLN4V: Salaires DOM quotient 4
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 29:9 to 29:71
    gln4v_881000_5 = (((lambda v21267: (0 if (0 > v21267) else v21267))((- abdomdaj_811040_15)) + (lambda v21269: (0 if (0 > v21269) else v21269))((- abdomeaj_811040_16))) * indeftsv_811030_33)

    # TTSNEAJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 908:12 to 908:30
    ttsneaj_899999999_13 = (- tabdomeaj_99992500_1)

    # TSNEBJ: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 443:11 to 443:28
    tsnebj_811060_28 = (- abdomebj_811040_33)

    # GLN4C: Salaires DOM quotient 4
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 31:9 to 31:71
    gln4c_881000_7 = (((lambda v21273: (0 if (0 > v21273) else v21273))((- abdomdbj_811040_32)) + (lambda v21275: (0 if (0 > v21275) else v21275))((- abdomebj_811040_33))) * indeftsc_811030_34)

    # TTSNTC: salaires apres deduction des frais professionnels tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1236:10 to 1237:49
    ttsntc_99992120_1 = (ttsn1bj_899999999_17 + (ttsn1qm_899999999_18 + (ttsn1up_899999999_19 + (ttsn1ox_899999999_20 + (ttsn1bf_899999999_21 + (ttsn1bg_899999999_22 + (ttsn1bc_899999999_23 + (ttsn1bp_899999999_24 + (ttsn3vk_899999999_25 + (ttsn1ut_899999999_26 + (ttsnrbj_899999999_27 + (ttsnrbp_899999999_28 + (ttsndbj_899999999_29 + (ttsnebj_899999999_30 + (ttsn1hb_899999999_31 + (ttsnrbf_899999999_32 + ttsnrbg_899999999_33))))))))))))))))

    # TSNTV: salaires apres deduction des frais professionnels
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 738:9 to 741:49
    tsntv_811110_0 = (tsn1aj_811060_0 + (tsn1pm_811060_1 + (tsn1tp_811060_2 + (tsn1nx_811060_3 + (tsn1af_811060_4 + (tsn1ag_811060_5 + (tsn1ap_811060_6 + (tsn3vj_811060_7 + (tsn1tt_811060_8 + (tsnraj_811060_9 + (tsnrap_811060_10 + (tsndaj_811060_11 + (tsneaj_811060_12 + (tsn1gb_811060_13 + (tsnraf_811060_14 + tsnrag_811060_15)))))))))))))))

    # TTSNTV: salaires apres deduction des frais professionnels tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1234:11 to 1235:50
    ttsntv_99992120_0 = (ttsn1aj_899999999_0 + (ttsn1pm_899999999_1 + (ttsn1tp_899999999_2 + (ttsn1nx_899999999_3 + (ttsn1af_899999999_4 + (ttsn1ag_899999999_5 + (ttsn1ac_899999999_6 + (ttsn1ap_899999999_7 + (ttsn3vj_899999999_8 + (ttsn1tt_899999999_9 + (ttsnraj_899999999_10 + (ttsnrap_899999999_11 + (ttsndaj_899999999_12 + (ttsneaj_899999999_13 + (ttsn1gb_899999999_14 + (ttsnraf_899999999_15 + ttsnrag_899999999_16))))))))))))))))

    # TSNTC: salaires apres deduction des frais professionnels
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 744:9 to 747:49
    tsntc_811110_3 = (tsn1bj_811060_16 + (tsn1qm_811060_17 + (tsn1up_811060_18 + (tsn1ox_811060_19 + (tsn1bf_811060_20 + (tsn1bg_811060_21 + (tsn1bp_811060_22 + (tsn3vk_811060_23 + (tsn1ut_811060_24 + (tsnrbj_811060_25 + (tsnrbp_811060_26 + (tsndbj_811060_27 + (tsnebj_811060_28 + (tsn1hb_811060_29 + (tsnrbf_811060_30 + tsnrbg_811060_31)))))))))))))))

    # REV4:  revenus au quotient 4
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 260:8 to 261:81
    rev4_701040_6 = (gln4v_881000_5 + gln4c_881000_7)

    # REV4HT: revenus au quotient 4 hors tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 262:10 to 262:83
    rev4ht_701040_7 = (gln4v_881000_5 + gln4c_881000_7)

    # TREV4:  revenus au quotient 4 tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 585:9 to 586:73
    trev4_99991025_0 = (gln4v_881000_5 + gln4c_881000_7)

    # TTSNC: Traitements et salaires nets de frais tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1245:9 to 1245:89
    ttsnc_99992130_1 = (((1 if ((- ttsntc_99992120_1) > 0) else 0) * (ttsntc_99992120_1 if (ttsntc_99992120_1 < 0) else 0)) + ((1 if (ttsntc_99992120_1 >= 0) else 0) * ttsntc_99992120_1))

    # TPRNDEF1BS: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1275:14 to 1276:134
    tprndef1bs_99992130_18 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (- ttsntc_99992120_1))

    # PRNDEF1AS: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 764:13 to 765:126
    prndef1as_811120_0 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (- tsntv_811110_0))

    # TSNV: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 987:8 to 988:40
    tsnv_811130_0 = (((1 if ((- tsntv_811110_0) > 0) else 0) * (tsntv_811110_0 if (tsntv_811110_0 < 0) else 0)) + ((1 if (tsntv_811110_0 >= 0) else 0) * tsntv_811110_0))

    # TTSNV: Traitements et salaires nets de frais tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1244:9 to 1244:89
    ttsnv_99992130_0 = (((1 if ((- ttsntv_99992120_0) > 0) else 0) * (ttsntv_99992120_0 if (ttsntv_99992120_0 < 0) else 0)) + ((1 if (ttsntv_99992120_0 >= 0) else 0) * ttsntv_99992120_0))

    # TPRNDEF1AS: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1251:14 to 1252:134
    tprndef1as_99992130_6 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (- ttsntv_99992120_0))

    # PRNDEF1BS: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 786:13 to 787:126
    prndef1bs_811120_11 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (- tsntc_811110_3))

    # TSNC: Traitements et salaires nets de frais
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 989:8 to 990:40
    tsnc_811130_1 = (((1 if ((- tsntc_811110_3) > 0) else 0) * (tsntc_811110_3 if (tsntc_811110_3 < 0) else 0)) + ((1 if (tsntc_811110_3 >= 0) else 0) * tsntc_811110_3))

    # TTSC: tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1489:8 to 1489:67
    ttsc_99992230_1 = ((ttsnc_99992130_1 - (lambda v23300: (0 if (0 > v23300) else v23300))((- tabdomdbj_99992500_2))) - (lambda v23302: (0 if (0 > v23302) else v23302))((- tabdomebj_99992500_3)))

    # TPRN1BS: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1409:11 to 1409:32
    tprn1bs_99992130_91 = (- tprndef1bs_99992130_18)

    # TPRNDEF1BL: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1277:14 to 1278:143
    tprndef1bl_99992130_19 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23237: (v23237 if (v23237 > 0) else 0))(((- ttsntc_99992120_1) - tprndef1bs_99992130_18)))

    # PRN1AS: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 895:10 to 895:29
    prn1as_811120_66 = (- prndef1as_811120_0)

    # PRNDEF1AL: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 766:13 to 767:134
    prndef1al_811120_1 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13928: (v13928 if (v13928 > 0) else 0))(((- tsntv_811110_0) - prndef1as_811120_0)))

    # TSV: 
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 36:7 to 36:62
    tsv_881010_0 = ((tsnv_811130_0 - (lambda v21277: (0 if (0 > v21277) else v21277))((- abdomdaj_811040_15))) - (lambda v21279: (0 if (0 > v21279) else v21279))((- abdomeaj_811040_16)))

    # TTSV: tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1488:8 to 1488:67
    ttsv_99992230_0 = ((ttsnv_99992130_0 - (lambda v23296: (0 if (0 > v23296) else v23296))((- tabdomdaj_99992500_0))) - (lambda v23298: (0 if (0 > v23298) else v23298))((- tabdomeaj_99992500_1)))

    # TPRN1AS: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1394:11 to 1394:32
    tprn1as_99992130_78 = (- tprndef1as_99992130_6)

    # TPRNDEF1AL: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1253:14 to 1254:143
    tprndef1al_99992130_7 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23215: (v23215 if (v23215 > 0) else 0))(((- ttsntv_99992120_0) - tprndef1as_99992130_6)))

    # PRN1BS: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 911:10 to 911:29
    prn1bs_811120_80 = (- prndef1bs_811120_11)

    # PRNDEF1BL: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 788:13 to 789:134
    prndef1bl_811120_12 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13948: (v13948 if (v13948 > 0) else 0))(((- tsntc_811110_3) - prndef1bs_811120_11)))

    # TSC: 
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 37:7 to 37:62
    tsc_881010_1 = ((tsnc_811130_1 - (lambda v21281: (0 if (0 > v21281) else v21281))((- abdomdbj_811040_32))) - (lambda v21283: (0 if (0 > v21283) else v21283))((- abdomebj_811040_33)))

    # TTSNNC: salaires normaux hors droits d'auteur imposables - tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1502:11 to 1503:38
    ttsnnc_99992230_13 = (((1 if (ttsc_99992230_1 > 0) else 0) * round((ttsc_99992230_1 * ((var_1bj / var_1bj) if var_1bj != 0.0 else Undefined())))) + ((1 - (1 if (ttsc_99992230_1 > 0) else 0)) * ttsc_99992230_1))

    # TPRN1BL: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1410:11 to 1410:32
    tprn1bl_99992130_92 = (- tprndef1bl_99992130_19)

    # TPRNDEF1BM: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1279:14 to 1280:147
    tprndef1bm_99992130_20 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23239: (v23239 if (v23239 > 0) else 0))((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19)))

    # PRN1AL: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 896:10 to 896:29
    prn1al_811120_67 = (- prndef1al_811120_1)

    # PRNDEF1AM: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 768:13 to 769:137
    prndef1am_811120_2 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13930: (v13930 if (v13930 > 0) else 0))((((- tsntv_811110_0) - prndef1as_811120_0) - prndef1al_811120_1)))

    # TSNNV: salaires normaux hors droits d'auteur imposables - Vous
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 48:10 to 49:36
    tsnnv_881010_12 = (((1 if (tsv_881010_0 > 0) else 0) * round((tsv_881010_0 * ((var_1aj / var_1aj) if var_1aj != 0.0 else Undefined())))) + ((1 - (1 if (tsv_881010_0 > 0) else 0)) * tsv_881010_0))

    # TTSNNV: salaires normaux hors droits d'auteur imposables - tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1500:11 to 1501:38
    ttsnnv_99992230_12 = (((1 if (ttsv_99992230_0 > 0) else 0) * round((ttsv_99992230_0 * ((var_1aj / var_1aj) if var_1aj != 0.0 else Undefined())))) + ((1 - (1 if (ttsv_99992230_0 > 0) else 0)) * ttsv_99992230_0))

    # TPRN1AL: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1395:11 to 1395:32
    tprn1al_99992130_79 = (- tprndef1al_99992130_7)

    # TPRNDEF1AM: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1255:14 to 1256:147
    tprndef1am_99992130_8 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23217: (v23217 if (v23217 > 0) else 0))((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7)))

    # PRN1BL: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 912:10 to 912:29
    prn1bl_811120_81 = (- prndef1bl_811120_12)

    # PRNDEF1BM: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 790:13 to 791:137
    prndef1bm_811120_13 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13950: (v13950 if (v13950 > 0) else 0))((((- tsntc_811110_3) - prndef1bs_811120_11) - prndef1bl_811120_12)))

    # TSNNC: salaires normaux hors droits d'auteur imposables - Cjt
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 50:10 to 51:36
    tsnnc_881010_13 = (((1 if (tsc_881010_1 > 0) else 0) * round((tsc_881010_1 * ((var_1bj / var_1bj) if var_1bj != 0.0 else Undefined())))) + ((1 - (1 if (tsc_881010_1 > 0) else 0)) * tsc_881010_1))

    # TTSNN2TSC: salaires normaux quot. 2 hors droits d'auteur imposables  tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1540:15 to 1541:77
    ttsnn2tsc_99992230_25 = ((1 if (ttsc_99992230_1 > 0) else 0) * (ttsc_99992230_1 - ttsnnc_99992230_13))

    # TPRN1BM: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1411:11 to 1411:32
    tprn1bm_99992130_93 = (- tprndef1bm_99992130_20)

    # TPRNDEF1BZ: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1281:14 to 1282:151
    tprndef1bz_99992130_21 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23241: (v23241 if (v23241 > 0) else 0))(((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19) - tprndef1bm_99992130_20)))

    # PRN1AM: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 897:10 to 897:29
    prn1am_811120_68 = (- prndef1am_811120_2)

    # PRNDEF1AZ: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 770:13 to 771:140
    prndef1az_811120_3 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13932: (v13932 if (v13932 > 0) else 0))(((((- tsntv_811110_0) - prndef1as_811120_0) - prndef1al_811120_1) - prndef1am_811120_2)))

    # TSNN2TSV: salaires normaux quot. 2 hors droits d'auteur imposables - Vous
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 120:13 to 121:75
    tsnn2tsv_881010_31 = ((1 if (tsv_881010_0 > 0) else 0) * (tsv_881010_0 - tsnnv_881010_12))

    # TTSNN2TSV: salaires normaux quot. 2 hors droits d'auteur imposables  tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1538:15 to 1539:77
    ttsnn2tsv_99992230_24 = ((1 if (ttsv_99992230_0 > 0) else 0) * (ttsv_99992230_0 - ttsnnv_99992230_12))

    # TPRN1AM: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1396:11 to 1396:32
    tprn1am_99992130_80 = (- tprndef1am_99992130_8)

    # TPRNDEF1AZ: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1257:14 to 1258:151
    tprndef1az_99992130_9 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23219: (v23219 if (v23219 > 0) else 0))(((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7) - tprndef1am_99992130_8)))

    # PRN1BM: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 913:10 to 913:29
    prn1bm_811120_82 = (- prndef1bm_811120_13)

    # PRNDEF1BZ: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 792:13 to 793:140
    prndef1bz_811120_14 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13952: (v13952 if (v13952 > 0) else 0))(((((- tsntc_811110_3) - prndef1bs_811120_11) - prndef1bl_811120_12) - prndef1bm_811120_13)))

    # TSPRT: Salaires, pensions imposables TOTAL
    # Defined in file ir-calcul/sources2017m_6_10/res-ser1.m, from 262:11 to 267:23
    tsprt_111190_0 = (tsnnv_881010_12 + tsnnc_881010_13)

    # TSNN2TSC: salaires normaux quot. 2 hors droits d'auteur imposables - 
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 122:13 to 123:75
    tsnn2tsc_881010_32 = ((1 if (tsc_881010_1 > 0) else 0) * (tsc_881010_1 - tsnnc_881010_13))

    # TTSNN2RBF: salaires normaux quot. 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1552:14 to 1553:80
    ttsnn2rbf_99992230_31 = ((1 if (ttsc_99992230_1 > 0) else 0) * ((ttsc_99992230_1 - ttsnnc_99992230_13) - ttsnn2tsc_99992230_25))

    # TPRN1BZ: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1412:11 to 1412:32
    tprn1bz_99992130_94 = (- tprndef1bz_99992130_21)

    # TPRNDEF1BO: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1283:14 to 1284:156
    tprndef1bo_99992130_22 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23243: (v23243 if (v23243 > 0) else 0))((((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19) - tprndef1bm_99992130_20) - tprndef1bz_99992130_21)))

    # PRN1AZ: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 898:10 to 898:29
    prn1az_811120_69 = (- prndef1az_811120_3)

    # PRNDEF1AO: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 772:13 to 773:144
    prndef1ao_811120_4 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13934: (v13934 if (v13934 > 0) else 0))((((((- tsntv_811110_0) - prndef1as_811120_0) - prndef1al_811120_1) - prndef1am_811120_2) - prndef1az_811120_3)))

    # TSNN2TSV_1731: salaires normaux quot. 2 hors droits d'auteur imposables - Vous 1731
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 159:17 to 159:185
    tsnn2tsv_1731_701037_0 = (lambda v3480: (v3480 if (v3480 > 0) else 0))((- tsnn2tsv_881010_31))

    # TSNN2RAF: salaires normaux quot. 2 hors droits d'auteur imposables - Vous
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 133:13 to 134:77
    tsnn2raf_881010_37 = ((1 if (tsv_881010_0 > 0) else 0) * ((tsv_881010_0 - tsnnv_881010_12) - tsnn2tsv_881010_31))

    # TTSNN2RAF: salaires normaux quot. 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1550:14 to 1551:80
    ttsnn2raf_99992230_30 = ((1 if (ttsv_99992230_0 > 0) else 0) * ((ttsv_99992230_0 - ttsnnv_99992230_12) - ttsnn2tsv_99992230_24))

    # TPRN1AZ: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1397:11 to 1397:32
    tprn1az_99992130_81 = (- tprndef1az_99992130_9)

    # TPRNDEF1AO: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1259:14 to 1260:156
    tprndef1ao_99992130_10 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23221: (v23221 if (v23221 > 0) else 0))((((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7) - tprndef1am_99992130_8) - tprndef1az_99992130_9)))

    # PRN1BZ: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 914:10 to 914:29
    prn1bz_811120_83 = (- prndef1bz_811120_14)

    # PRNDEF1BO: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 794:13 to 795:144
    prndef1bo_811120_15 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13954: (v13954 if (v13954 > 0) else 0))((((((- tsntc_811110_3) - prndef1bs_811120_11) - prndef1bl_811120_12) - prndef1bm_811120_13) - prndef1bz_811120_14)))

    # SDD: solde deficits non imputes sur RG
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1867:7 to 1867:23
    sdd_701250_0 = (lambda v3714: (v3714 if (v3714 > 0) else 0))((- tsprt_111190_0))

    # TSNN2TSC_1731: salaires normaux quot. 2 hors droits d'auteur imposables -  1731
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 160:17 to 160:185
    tsnn2tsc_1731_701037_1 = (lambda v3482: (v3482 if (v3482 > 0) else 0))((- tsnn2tsc_881010_32))

    # TSNN2RBF: salaires quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 135:13 to 136:77
    tsnn2rbf_881010_38 = ((1 if (tsc_881010_1 > 0) else 0) * ((tsc_881010_1 - tsnnc_881010_13) - tsnn2tsc_881010_32))

    # TTSNN2RBG: salaires normaux quot. 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1565:14 to 1566:83
    ttsnn2rbg_99992230_37 = ((1 if (ttsc_99992230_1 > 0) else 0) * (((ttsc_99992230_1 - ttsnnc_99992230_13) - ttsnn2tsc_99992230_25) - ttsnn2rbf_99992230_31))

    # TPRN1BO: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1413:11 to 1413:32
    tprn1bo_99992130_95 = (- tprndef1bo_99992130_22)

    # TPRNDEFRBS: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1285:14 to 1286:160
    tprndefrbs_99992130_23 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23245: (v23245 if (v23245 > 0) else 0))(((((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19) - tprndef1bm_99992130_20) - tprndef1bz_99992130_21) - tprndef1bo_99992130_22)))

    # PRN1AO: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 899:10 to 899:29
    prn1ao_811120_70 = (- prndef1ao_811120_4)

    # PRNDEFRAS: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 774:13 to 775:147
    prndefras_811120_5 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13936: (v13936 if (v13936 > 0) else 0))(((((((- tsntv_811110_0) - prndef1as_811120_0) - prndef1al_811120_1) - prndef1am_811120_2) - prndef1az_811120_3) - prndef1ao_811120_4)))

    # TSNN2RAG: salaires normaux quot. 2 hors droits d'auteur imposables - Vous
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 146:13 to 147:79
    tsnn2rag_881010_43 = ((1 if (tsv_881010_0 > 0) else 0) * (((tsv_881010_0 - tsnnv_881010_12) - tsnn2tsv_881010_31) - tsnn2raf_881010_37))

    # TTSNN2RAG: salaires normaux quot. 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1563:14 to 1564:83
    ttsnn2rag_99992230_36 = ((1 if (ttsv_99992230_0 > 0) else 0) * (((ttsv_99992230_0 - ttsnnv_99992230_12) - ttsnn2tsv_99992230_24) - ttsnn2raf_99992230_30))

    # TPRN1AO: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1398:11 to 1398:32
    tprn1ao_99992130_82 = (- tprndef1ao_99992130_10)

    # TPRNDEFRAS: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1261:14 to 1262:160
    tprndefras_99992130_11 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23223: (v23223 if (v23223 > 0) else 0))(((((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7) - tprndef1am_99992130_8) - tprndef1az_99992130_9) - tprndef1ao_99992130_10)))

    # PRN1BO: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 915:10 to 915:29
    prn1bo_811120_84 = (- prndef1bo_811120_15)

    # PRNDEFRBS: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 796:13 to 797:147
    prndefrbs_811120_16 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13956: (v13956 if (v13956 > 0) else 0))(((((((- tsntc_811110_3) - prndef1bs_811120_11) - prndef1bl_811120_12) - prndef1bm_811120_13) - prndef1bz_811120_14) - prndef1bo_811120_15)))

    # TSNN2TST_1731: salaires normaux quot. 2 hors droits d'auteur imposables -  1731 total
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 165:17 to 165:100
    tsnn2tst_1731_701037_6 = (tsnn2tsv_1731_701037_0 + tsnn2tsc_1731_701037_1)

    # TSNN2RBG: salaires quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 148:13 to 149:79
    tsnn2rbg_881010_44 = ((1 if (tsc_881010_1 > 0) else 0) * (((tsc_881010_1 - tsnnc_881010_13) - tsnn2tsc_881010_32) - tsnn2rbf_881010_38))

    # TTSNN2REMPC: Tx effectif salaires normaux quot. 2 imposables - 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1576:16 to 1576:77
    ttsnn2rempc_99992230_43 = ((1 if (ttsc_99992230_1 > 0) else 0) * ((((ttsc_99992230_1 - ttsnnc_99992230_13) - ttsnn2tsc_99992230_25) - ttsnn2rbf_99992230_31) - ttsnn2rbg_99992230_37))

    # TPRNRBS: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1414:11 to 1414:32
    tprnrbs_99992130_96 = (- tprndefrbs_99992130_23)

    # TPRNDEFRBL: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1287:14 to 1288:157
    tprndefrbl_99992130_24 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23247: (v23247 if (v23247 > 0) else 0))((((((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19) - tprndef1bm_99992130_20) - tprndef1bz_99992130_21) - tprndef1bo_99992130_22) - tprndefrbs_99992130_23)))

    # PRNRAS: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 900:10 to 900:29
    prnras_811120_71 = (- prndefras_811120_5)

    # PRNDEFRAL: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 776:13 to 777:150
    prndefral_811120_6 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13938: (v13938 if (v13938 > 0) else 0))((((((((- tsntv_811110_0) - prndef1as_811120_0) - prndef1al_811120_1) - prndef1am_811120_2) - prndef1az_811120_3) - prndef1ao_811120_4) - prndefras_811120_5)))

    # TSNN2REMPV: salaires normaux quot. 2 hors droits d'auteur imposables - 
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 158:15 to 158:70
    tsnn2rempv_881010_49 = ((1 if (tsv_881010_0 > 0) else 0) * ((((tsv_881010_0 - tsnnv_881010_12) - tsnn2tsv_881010_31) - tsnn2raf_881010_37) - tsnn2rag_881010_43))

    # TTSNN2REMPV: Tx effectif salaires normaux quot. 2 imposables - 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1575:16 to 1575:77
    ttsnn2rempv_99992230_42 = ((1 if (ttsv_99992230_0 > 0) else 0) * ((((ttsv_99992230_0 - ttsnnv_99992230_12) - ttsnn2tsv_99992230_24) - ttsnn2raf_99992230_30) - ttsnn2rag_99992230_36))

    # TPRNRAS: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1399:11 to 1399:32
    tprnras_99992130_83 = (- tprndefras_99992130_11)

    # TPRNDEFRAL: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1263:14 to 1264:157
    tprndefral_99992130_12 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23225: (v23225 if (v23225 > 0) else 0))((((((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7) - tprndef1am_99992130_8) - tprndef1az_99992130_9) - tprndef1ao_99992130_10) - tprndefras_99992130_11)))

    # PRNRBS: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 916:10 to 916:29
    prnrbs_811120_85 = (- prndefrbs_811120_16)

    # PRNDEFRBL: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 798:13 to 799:150
    prndefrbl_811120_17 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13958: (v13958 if (v13958 > 0) else 0))((((((((- tsntc_811110_3) - prndef1bs_811120_11) - prndef1bl_811120_12) - prndef1bm_811120_13) - prndef1bz_811120_14) - prndef1bo_811120_15) - prndefrbs_811120_16)))

    # REVTS: total TS 
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 269:9 to 270:185
    revts_701040_13 = (((tsnn2tsv_881010_31 + tsnn2tsc_881010_32) + (tsnn2raf_881010_37 + (tsnn2rbf_881010_38 + (tsnn2rag_881010_43 + tsnn2rbg_881010_44)))) + tsnn2tst_1731_701037_6)

    # REVTSQHT: total TS hors tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 271:12 to 271:161
    revtsqht_701040_14 = ((tsnn2tsv_881010_31 + tsnn2tsc_881010_32) + (tsnn2tst_1731_701037_6 + (tsnn2raf_881010_37 + (tsnn2rbf_881010_38 + (tsnn2rag_881010_43 + tsnn2rbg_881010_44)))))

    # TSNN2REMPC: salaires normaux quot. 2 hors droits d'auteur imposables - 
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 159:15 to 159:70
    tsnn2rempc_881010_50 = ((1 if (tsc_881010_1 > 0) else 0) * ((((tsc_881010_1 - tsnnc_881010_13) - tsnn2tsc_881010_32) - tsnn2rbf_881010_38) - tsnn2rbg_881010_44))

    # TPRNRBL: Pensions et rentes nettes tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1415:11 to 1415:32
    tprnrbl_99992130_97 = (- tprndefrbl_99992130_24)

    # TPRNDEFRBM: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1289:14 to 1290:161
    tprndefrbm_99992130_25 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23249: (v23249 if (v23249 > 0) else 0))(((((((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19) - tprndef1bm_99992130_20) - tprndef1bz_99992130_21) - tprndef1bo_99992130_22) - tprndefrbs_99992130_23) - tprndefrbl_99992130_24)))

    # PRNRAL: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 901:10 to 901:29
    prnral_811120_72 = (- prndefral_811120_6)

    # PRNDEFRAM: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 778:13 to 779:153
    prndefram_811120_7 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13940: (v13940 if (v13940 > 0) else 0))(((((((((- tsntv_811110_0) - prndef1as_811120_0) - prndef1al_811120_1) - prndef1am_811120_2) - prndef1az_811120_3) - prndef1ao_811120_4) - prndefras_811120_5) - prndefral_811120_6)))

    # TSNN2REMPV_1731: salaires normaux quot. 2 hors droits d'auteur imposables -  1731 
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 167:19 to 167:191
    tsnn2rempv_1731_701037_7 = (lambda v3492: (v3492 if (v3492 > 0) else 0))((- tsnn2rempv_881010_49))

    # TPRNRAL: Pensions et rentes nettes tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1400:11 to 1400:32
    tprnral_99992130_84 = (- tprndefral_99992130_12)

    # TPRNDEFRAM: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1265:14 to 1266:161
    tprndefram_99992130_13 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23227: (v23227 if (v23227 > 0) else 0))(((((((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7) - tprndef1am_99992130_8) - tprndef1az_99992130_9) - tprndef1ao_99992130_10) - tprndefras_99992130_11) - tprndefral_99992130_12)))

    # PRNRBL: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 917:10 to 917:29
    prnrbl_811120_86 = (- prndefrbl_811120_17)

    # PRNDEFRBM: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 800:13 to 801:153
    prndefrbm_811120_18 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13960: (v13960 if (v13960 > 0) else 0))(((((((((- tsntc_811110_3) - prndef1bs_811120_11) - prndef1bl_811120_12) - prndef1bm_811120_13) - prndef1bz_811120_14) - prndef1bo_811120_15) - prndefrbs_811120_16) - prndefrbl_811120_17)))

    # TSNN2REMPC_1731: salaires normaux quot. 2 hors droits d'auteur imposables -  1731 
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 168:19 to 168:191
    tsnn2rempc_1731_701037_8 = (lambda v3494: (v3494 if (v3494 > 0) else 0))((- tsnn2rempc_881010_50))

    # TPRNRBM: Pensions et rentes nettes tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1416:11 to 1416:32
    tprnrbm_99992130_98 = (- tprndefrbm_99992130_25)

    # TPRNDEFRBZ: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1291:14 to 1292:166
    tprndefrbz_99992130_26 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23251: (v23251 if (v23251 > 0) else 0))((((((((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19) - tprndef1bm_99992130_20) - tprndef1bz_99992130_21) - tprndef1bo_99992130_22) - tprndefrbs_99992130_23) - tprndefrbl_99992130_24) - tprndefrbm_99992130_25)))

    # PRNRAM: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 902:10 to 902:29
    prnram_811120_73 = (- prndefram_811120_7)

    # PRNDEFRAZ: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 780:13 to 781:156
    prndefraz_811120_8 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13942: (v13942 if (v13942 > 0) else 0))((((((((((- tsntv_811110_0) - prndef1as_811120_0) - prndef1al_811120_1) - prndef1am_811120_2) - prndef1az_811120_3) - prndef1ao_811120_4) - prndefras_811120_5) - prndefral_811120_6) - prndefram_811120_7)))

    # TPRNRAM: Pensions et rentes nettes tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1401:11 to 1401:32
    tprnram_99992130_85 = (- tprndefram_99992130_13)

    # TPRNDEFRAZ: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1267:14 to 1268:172
    tprndefraz_99992130_14 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23229: (v23229 if (v23229 > 0) else 0))((((((((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7) - tprndef1am_99992130_8) - tprndef1az_99992130_9) - tprndef1ao_99992130_10) - tprndefras_99992130_11) - tprndefral_99992130_12) - tprndefram_99992130_13)))

    # PRNRBM: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 918:10 to 918:29
    prnrbm_811120_87 = (- prndefrbm_811120_18)

    # PRNDEFRBZ: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 802:13 to 803:156
    prndefrbz_811120_19 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13962: (v13962 if (v13962 > 0) else 0))((((((((((- tsntc_811110_3) - prndef1bs_811120_11) - prndef1bl_811120_12) - prndef1bm_811120_13) - prndef1bz_811120_14) - prndef1bo_811120_15) - prndefrbs_811120_16) - prndefrbl_811120_17) - prndefrbm_811120_18)))

    # TSNN2REMPT_1731: salaires normaux quot. 2 hors droits d'auteur imposables -  1731 total
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 173:19 to 173:114
    tsnn2rempt_1731_701037_13 = (tsnn2rempv_1731_701037_7 + tsnn2rempc_1731_701037_8)

    # TPRNRBZ: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1417:11 to 1417:32
    tprnrbz_99992130_99 = (- tprndefrbz_99992130_26)

    # TPRNDEF1BH: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1293:14 to 1294:176
    tprndef1bh_99992130_27 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23253: (v23253 if (v23253 > 0) else 0))(((((((((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19) - tprndef1bm_99992130_20) - tprndef1bz_99992130_21) - tprndef1bo_99992130_22) - tprndefrbs_99992130_23) - tprndefrbz_99992130_26) - tprndefrbl_99992130_24) - tprndefrbm_99992130_25)))

    # PRNRAZ: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 903:10 to 903:29
    prnraz_811120_74 = (- prndefraz_811120_8)

    # PRNDEFRAO: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 782:13 to 783:158
    prndefrao_811120_9 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13944: (v13944 if (v13944 > 0) else 0))(((((((((((- tsntv_811110_0) - prndef1as_811120_0) - prndef1al_811120_1) - prndef1am_811120_2) - prndef1az_811120_3) - prndef1ao_811120_4) - prndefras_811120_5) - prndefraz_811120_8) - prndefral_811120_6) - prndefram_811120_7)))

    # TPRNRAZ: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1402:11 to 1402:32
    tprnraz_99992130_86 = (- tprndefraz_99992130_14)

    # TPRNDEF1AH: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1269:14 to 1270:176
    tprndef1ah_99992130_15 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23231: (v23231 if (v23231 > 0) else 0))(((((((((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7) - tprndef1am_99992130_8) - tprndef1az_99992130_9) - tprndef1ao_99992130_10) - tprndefras_99992130_11) - tprndefraz_99992130_14) - tprndefral_99992130_12) - tprndefram_99992130_13)))

    # PRNRBZ: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 919:10 to 919:29
    prnrbz_811120_88 = (- prndefrbz_811120_19)

    # PRNDEFRBO: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 804:13 to 805:158
    prndefrbo_811120_20 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13964: (v13964 if (v13964 > 0) else 0))(((((((((((- tsntc_811110_3) - prndef1bs_811120_11) - prndef1bl_811120_12) - prndef1bm_811120_13) - prndef1bz_811120_14) - prndef1bo_811120_15) - prndefrbs_811120_16) - prndefrbl_811120_17) - prndefrbz_811120_19) - prndefrbm_811120_18)))

    # DFANTIMPUQUO: Deficits globaux des annees anterieures quotient
    # Defined in file ir-calcul/sources2017m_6_10/res-ser1.m, from 519:16 to 520:42
    dfantimpuquo_90433_0 = (tsnn2tst_1731_701037_6 + tsnn2rempt_1731_701037_13)

    # REVTSREMP: total TS remplacement
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 272:13 to 272:110
    revtsremp_701040_15 = ((tsnn2rempv_881010_49 + tsnn2rempc_881010_50) + tsnn2rempt_1731_701037_13)

    # REVTSREMPQHT: total TS remplacement hors tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 273:16 to 273:61
    revtsrempqht_701040_16 = ((tsnn2rempv_881010_49 + tsnn2rempc_881010_50) + tsnn2rempt_1731_701037_13)

    # TPRN1BH: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1419:11 to 1419:32
    tprn1bh_99992130_101 = (- tprndef1bh_99992130_27)

    # TPRNDEFRBO: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1295:14 to 1296:179
    tprndefrbo_99992130_28 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23255: (v23255 if (v23255 > 0) else 0))((((((((((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19) - tprndef1bm_99992130_20) - tprndef1bz_99992130_21) - tprndef1bo_99992130_22) - tprndefrbs_99992130_23) - tprndefrbz_99992130_26) - tprndef1bh_99992130_27) - tprndefrbl_99992130_24) - tprndefrbm_99992130_25)))

    # PRNRAO: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 904:10 to 904:29
    prnrao_811120_75 = (- prndefrao_811120_9)

    # PRNDEFFAS: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 784:13 to 784:145
    prndeffas_811120_10 = ((1 if ((- tsntv_811110_0) > 0) else 0) * (lambda v13946: (v13946 if (v13946 > 0) else 0))((((((((((((- tsntv_811110_0) - prndef1as_811120_0) - prndef1al_811120_1) - prndef1am_811120_2) - prndef1az_811120_3) - prndef1ao_811120_4) - prndefras_811120_5) - prndefraz_811120_8) - prndefrao_811120_9) - prndefral_811120_6) - prndefram_811120_7)))

    # TPRN1AH: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1404:11 to 1404:32
    tprn1ah_99992130_88 = (- tprndef1ah_99992130_15)

    # TPRNDEFRAO: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1271:14 to 1272:179
    tprndefrao_99992130_16 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23233: (v23233 if (v23233 > 0) else 0))((((((((((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7) - tprndef1am_99992130_8) - tprndef1az_99992130_9) - tprndef1ao_99992130_10) - tprndefras_99992130_11) - tprndefraz_99992130_14) - tprndef1ah_99992130_15) - tprndefral_99992130_12) - tprndefram_99992130_13)))

    # PRNRBO: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 920:10 to 920:29
    prnrbo_811120_89 = (- prndefrbo_811120_20)

    # PRNDEFFBS: Part deficit TS dans pensions nettes d'abattement de 10%
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 806:13 to 806:145
    prndeffbs_811120_21 = ((1 if ((- tsntc_811110_3) > 0) else 0) * (lambda v13966: (v13966 if (v13966 > 0) else 0))((((((((((((- tsntc_811110_3) - prndef1bs_811120_11) - prndef1bl_811120_12) - prndef1bm_811120_13) - prndef1bz_811120_14) - prndef1bo_811120_15) - prndefrbs_811120_16) - prndefrbz_811120_19) - prndefrbo_811120_20) - prndefrbl_811120_17) - prndefrbm_811120_18)))

    # DFANTIMPUBAR: Deficits globaux des annees anterieures bareme
    # Defined in file ir-calcul/sources2017m_6_10/res-ser1.m, from 516:16 to 516:47
    dfantimpubar_90432_0 = (lambda v3228: (v3228 if (v3228 > 0) else 0))((- dfantimpuquo_90433_0))

    # TPRNRBO: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1418:11 to 1418:32
    tprnrbo_99992130_100 = (- tprndefrbo_99992130_28)

    # TPRNDEFFBS: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1297:14 to 1297:169
    tprndeffbs_99992130_29 = ((1 if ((- ttsntc_99992120_1) > 0) else 0) * (lambda v23257: (v23257 if (v23257 > 0) else 0))(((((((((((((- ttsntc_99992120_1) - tprndef1bs_99992130_18) - tprndef1bl_99992130_19) - tprndef1bm_99992130_20) - tprndef1bz_99992130_21) - tprndef1bo_99992130_22) - tprndefrbs_99992130_23) - tprndefrbz_99992130_26) - tprndef1bh_99992130_27) - tprndefrbo_99992130_28) - tprndefrbl_99992130_24) - tprndefrbm_99992130_25)))

    # PRNFAS: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 905:10 to 905:29
    prnfas_811120_76 = (- prndeffas_811120_10)

    # TPRNRAO: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1403:11 to 1403:32
    tprnrao_99992130_87 = (- tprndefrao_99992130_16)

    # TPRNDEFFAS: part def ts sur pensions nettes d'abattement de 10% tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1273:14 to 1273:169
    tprndeffas_99992130_17 = ((1 if ((- ttsntv_99992120_0) > 0) else 0) * (lambda v23235: (v23235 if (v23235 > 0) else 0))(((((((((((((- ttsntv_99992120_0) - tprndef1as_99992130_6) - tprndef1al_99992130_7) - tprndef1am_99992130_8) - tprndef1az_99992130_9) - tprndef1ao_99992130_10) - tprndefras_99992130_11) - tprndefraz_99992130_14) - tprndef1ah_99992130_15) - tprndefrao_99992130_16) - tprndefral_99992130_12) - tprndefram_99992130_13)))

    # PRNFBS: pensions nettes totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 921:10 to 921:29
    prnfbs_811120_90 = (- prndeffbs_811120_21)

    # RGRFRHR: Revenu global pour rfr hauts revenus
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1970:11 to 1975:49
    rgrfrhr_701340_3 = (tsprt_111190_0 + dfantimpubar_90432_0)

    # TPRNFBS: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1420:11 to 1420:32
    tprnfbs_99992130_102 = (- tprndeffbs_99992130_29)

    # PRNV: Pensions et rentes nettes
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 906:8 to 906:91
    prnv_811120_77 = (lambda v13976: (v13976 if (v13976 > 0) else 0))((prn1as_811120_66 + (prn1al_811120_67 + (prn1am_811120_68 + (prn1az_811120_69 + (prn1ao_811120_70 + (prnras_811120_71 + (prnraz_811120_74 + (prnrao_811120_75 + (prnfas_811120_76 + (prnral_811120_72 + prnram_811120_73)))))))))))

    # TPRNFAS: pensions nettes totale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1405:11 to 1405:32
    tprnfas_99992130_89 = (- tprndeffas_99992130_17)

    # PRNC: Pensions et rentes nettes
    # Defined in file ir-calcul/sources2017m_6_10/chap-81.m, from 922:8 to 922:91
    prnc_811120_91 = (lambda v13982: (v13982 if (v13982 > 0) else 0))((prn1bs_811120_80 + (prn1bl_811120_81 + (prn1bm_811120_82 + (prn1bz_811120_83 + (prn1bo_811120_84 + (prnrbs_811120_85 + (prnrbz_811120_88 + (prnrbo_811120_89 + (prnfbs_811120_90 + (prnrbl_811120_86 + prnrbm_811120_87)))))))))))

    # TPRNC: Pensions et rentes nettes tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1421:9 to 1421:111
    tprnc_99992130_103 = (lambda v23269: (v23269 if (v23269 > 0) else 0))((tprn1bs_99992130_91 + (tprn1bl_99992130_92 + (tprn1bm_99992130_93 + (tprn1bz_99992130_94 + (tprn1bo_99992130_95 + (tprnrbs_99992130_96 + (tprnrbz_99992130_99 + (tprnrbo_99992130_100 + (tprn1bh_99992130_101 + (tprnfbs_99992130_102 + (tprnrbl_99992130_97 + tprnrbm_99992130_98))))))))))))

    # PRR2ZV: Pensions nettes taxees au quotient  - code RxZ
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 187:10 to 188:77
    prr2zv_881020_12 = (prnv_811120_77 - prnv_811120_77)

    # TPRNV: Pensions et rentes nettes tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1406:9 to 1406:111
    tprnv_99992130_90 = (lambda v23267: (v23267 if (v23267 > 0) else 0))((tprn1as_99992130_78 + (tprn1al_99992130_79 + (tprn1am_99992130_80 + (tprn1az_99992130_81 + (tprn1ao_99992130_82 + (tprnras_99992130_83 + (tprnraz_99992130_86 + (tprnrao_99992130_87 + (tprn1ah_99992130_88 + (tprnfas_99992130_89 + (tprnral_99992130_84 + tprnram_99992130_85))))))))))))

    # PRR2ZC: Pensions nettes taxees au quotient  - code RxZ
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 189:10 to 190:77
    prr2zc_881020_13 = (prnc_811120_91 - prnc_811120_91)

    # TPRR2ZC: tx efectif Pensions nettes taxees au quotient code RxZ
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1604:11 to 1605:80
    tprr2zc_99992240_13 = (tprnc_99992130_103 - tprnc_99992130_103)

    # PENFV: Pensions nettes footballeurs - Vous
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 200:11 to 201:82
    penfv_881020_19 = (lambda v21292: (v21292 if (v21292 > 0) else 0))(((prnv_811120_77 - prnv_811120_77) - prr2zv_881020_12))

    # TPRR2ZV: tx efectif Pensions nettes taxees au quotient code RxZ
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1602:11 to 1603:80
    tprr2zv_99992240_12 = (tprnv_99992130_90 - tprnv_99992130_90)

    # PENFC: Pensions nettes footballeurs - Conjoint
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 202:11 to 203:82
    penfc_881020_20 = (lambda v21294: (v21294 if (v21294 > 0) else 0))(((prnc_811120_91 - prnc_811120_91) - prr2zc_881020_13))

    # TPENFC: Pensions nettes footballeurs - tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1616:11 to 1617:86
    tpenfc_99992240_19 = (lambda v23305: (v23305 if (v23305 > 0) else 0))(((tprnc_99992130_103 - tprnc_99992130_103) - tprr2zc_99992240_13))

    # PRR2RAL: Pensions nettes taxees au quotient  - code Rxx
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 212:11 to 213:77
    prr2ral_881020_25 = (((prnv_811120_77 - prnv_811120_77) - prr2zv_881020_12) - penfv_881020_19)

    # TPENFV: Pensions nettes footballeurs - tx eff.
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1614:11 to 1615:86
    tpenfv_99992240_18 = (lambda v23303: (v23303 if (v23303 > 0) else 0))(((tprnv_99992130_90 - tprnv_99992130_90) - tprr2zv_99992240_12))

    # REVF: total pensions nettes footballeurs
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 265:8 to 265:96
    revf_701040_9 = (penfv_881020_19 + penfc_881020_20)

    # REVFHT: total hors tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 266:10 to 266:45
    revfht_701040_10 = (penfv_881020_19 + penfc_881020_20)

    # PRR2RBL: Pensions nettes taxees au quotient  - code Rxx
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 214:11 to 215:77
    prr2rbl_881020_26 = (((prnc_811120_91 - prnc_811120_91) - prr2zc_881020_13) - penfc_881020_20)

    # TPRR2RBL: tx efectif Pensions nettes taxees au quotient 2 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1628:12 to 1629:82
    tprr2rbl_99992240_25 = (((tprnc_99992130_103 - tprnc_99992130_103) - tprr2zc_99992240_13) - tpenfc_99992240_19)

    # PRR2RAM: Pensions nettes taxees au quotient  - code Rxx
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 224:11 to 225:78
    prr2ram_881020_31 = ((((prnv_811120_77 - prnv_811120_77) - penfv_881020_19) - prr2zv_881020_12) - prr2ral_881020_25)

    # TPRR2RAL: tx efectif Pensions nettes taxees au quotient 2 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1626:12 to 1627:82
    tprr2ral_99992240_24 = (((tprnv_99992130_90 - tprnv_99992130_90) - tprr2zv_99992240_12) - tpenfv_99992240_18)

    # PRR2RBM: Pensions nettes taxees au quotient  - code Rxx
    # Defined in file ir-calcul/sources2017m_6_10/chap-88.m, from 226:11 to 227:78
    prr2rbm_881020_32 = ((((prnc_811120_91 - prnc_811120_91) - penfc_881020_20) - prr2zc_881020_13) - prr2rbl_881020_26)

    # TPRR2RBM: tx efectif Pensions nettes taxees au quotient 2 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1640:12 to 1641:84
    tprr2rbm_99992240_31 = ((((tprnc_99992130_103 - tprnc_99992130_103) - tpenfc_99992240_19) - tprr2zc_99992240_13) - tprr2rbl_99992240_25)

    # TPRR2RAM: tx efectif Pensions nettes taxees au quotient 2 
    # Defined in file ir-calcul/sources2017m_6_10/chap-teff.m, from 1638:12 to 1639:84
    tprr2ram_99992240_30 = ((((tprnv_99992130_90 - tprnv_99992130_90) - tpenfv_99992240_18) - tprr2zv_99992240_12) - tprr2ral_99992240_24)

    # REVPRR: total 
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 274:10 to 275:186
    revprr_701040_17 = (((prnv_811120_77 + prr2zv_881020_12) + (prnc_811120_91 + prr2zc_881020_13)) + (prr2ral_881020_25 + (prr2rbl_881020_26 + (prr2ram_881020_31 + prr2rbm_881020_32))))

    # REVPRRQHT: total  hors tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 276:13 to 276:164
    revprrqht_701040_18 = (((prnv_811120_77 + prr2zv_881020_12) + (prnc_811120_91 + prr2zc_881020_13)) + (prr2ral_881020_25 + (prr2rbl_881020_26 + (prr2ram_881020_31 + prr2rbm_881020_32))))

    # REVQTOT: somme des revevus aux quotient :REVx REVAFN REVF
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 280:11 to 280:172
    revqtot_701040_22 = (trev4_99991025_0 + (revf_701040_9 + (revts_701040_13 + (revtsremp_701040_15 + revprr_701040_17))))

    # REVQTOTQHT: somme des revevus aux quotient :REVx REVAFN REVF hors tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 281:14 to 281:187
    revqtotqht_701040_23 = (revfht_701040_10 + (revtsqht_701040_14 + (revtsrempqht_701040_16 + revprrqht_701040_18)))

    # RBG1: Revenu brut global
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1902:8 to 1909:13
    rbg1_701300_0 = (((1 - (1 if (tsprt_111190_0 > 0) else 0)) * (lambda v3738: (v3738 if (v3738 < 0) else 0))((tsprt_111190_0 + revqtot_701040_22))) + ((1 if (tsprt_111190_0 > 0) else 0) * tsprt_111190_0))

    # RBGRFRHR: Revenu brut global pour rfr hauts revenus
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1914:12 to 1915:66
    rbgrfrhr_701300_3 = (((1 - (1 if (rgrfrhr_701340_3 > 0) else 0)) * (lambda v3746: (v3746 if (v3746 < 0) else 0))((rgrfrhr_701340_3 + revqtot_701040_22))) + ((1 if (rgrfrhr_701340_3 > 0) else 0) * rgrfrhr_701340_3))

    # RPALE: Pension enfants majeurs : montant retenu
    # Defined in file ir-calcul/sources2017m_6_10/res-ser1.m, from 613:9 to 614:53
    rpale_111490_1 = (lambda v3302: (v3302 if (v3302 > 0) else 0))((lambda v3300: (v3300 if (v3300 < 0) else 0))((rbg1_701300_0 + (revqtot_701040_22 - sdd_701250_0))))

    # RBG2: Revenu brut global moins la CSg deductible
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1846:8 to 1846:60
    rbg2_701240_0 = (rbg1_701300_0 - (False if (False < rbg1_701300_0) else rbg1_701300_0))

    # RBG2RFRHR: Revenu brut global moins la CSg deductible pour rfr hauts revenus
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1849:13 to 1849:73
    rbg2rfrhr_701240_3 = (rbgrfrhr_701300_3 - (False if (False < rbgrfrhr_701300_3) else rbgrfrhr_701300_3))

    # APERPV: bse retenu PERP
    # Defined in file ir-calcul/sources2017m_6_10/chap-perp.m, from 692:10 to 693:45
    aperpv_31022_6 = (lambda v17894: (0 if (0 > v17894) else v17894))((lambda v17891: (v17891 if (v17891 < False) else False))(((rbg1_701300_0 - rpale_111490_1) + (revqtot_701040_22 - sdd_701250_0))))

    # APERPC: bse retenu PERP
    # Defined in file ir-calcul/sources2017m_6_10/chap-perp.m, from 694:10 to 695:54
    aperpc_31022_7 = (lambda v17898: (0 if (0 > v17898) else v17898))((lambda v17895: (v17895 if (v17895 < False) else False))(((rbg1_701300_0 - rpale_111490_1) + ((revqtot_701040_22 - sdd_701250_0) - aperpv_31022_6))))

    # APERPP: bse retenu PERP
    # Defined in file ir-calcul/sources2017m_6_10/chap-perp.m, from 696:10 to 697:63
    aperpp_31022_8 = (lambda v17902: (0 if (0 > v17902) else v17902))((lambda v17899: (v17899 if (v17899 < False) else False))(((rbg1_701300_0 - rpale_111490_1) + (((revqtot_701040_22 - sdd_701250_0) - aperpv_31022_6) - aperpc_31022_7))))

    # RBL1: Revenu base des limitations de charges deductibles
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1852:10 to 1853:57
    rbl1_701240_5 = (rbg2_701240_0 - (lambda v3694: (v3694 if (v3694 < rbg2_701240_0) else rbg2_701240_0))((aperpv_31022_6 + (aperpc_31022_7 + aperpp_31022_8))))

    # RBLRFRHR: Revenu base des limitations de charges deductibles pour rfr hauts rev
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1858:14 to 1859:62
    rblrfrhr_701240_8 = (rbg2rfrhr_701240_3 - (lambda v3706: (v3706 if (v3706 < rbg2rfrhr_701240_3) else rbg2rfrhr_701240_3))((aperpv_31022_6 + (aperpc_31022_7 + aperpp_31022_8))))

    # SDC1: solde charges hors DCI non imputes sur RG
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1873:8 to 1875:63
    sdc1_701260_0 = (lambda v3718: (v3718 if (v3718 > 0) else 0))(((((- aperpv_31022_6) - aperpc_31022_7) - aperpp_31022_8) - (rbg1_701300_0 if (rbg1_701300_0 > 0) else 0)))

    # SDC: solde charges non imputees sur RG
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1881:7 to 1883:61
    sdc_701270_0 = (lambda v3726: (v3726 if (v3726 > 0) else 0))((aperpv_31022_6 + (aperpc_31022_7 + (aperpp_31022_8 - (rbg1_701300_0 if (rbg1_701300_0 > 0) else 0)))))

    # RI1: Revenu imposable
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1766:7 to 1766:68
    ri1_701170_0 = (0 if (0 > rbl1_701240_5) else rbl1_701240_5)

    # SDV: solde abattements APA non imputes sur RNG
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1889:7 to 1889:26
    sdv_701280_0 = (lambda v3732: (v3732 if (v3732 > 0) else 0))((- rbl1_701240_5))

    # SDM: solde abattements APA et AMA non imputes sur RNG
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1891:7 to 1891:42
    sdm_701280_1 = (lambda v3736: (v3736 if (v3736 > 0) else 0))((- (rbl1_701240_5 if (rbl1_701240_5 > 0) else 0)))

    # REVDON: Rev. servant de base au calcul des plaf. dons aux oeuvres
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3500:10 to 3500:41
    revdon_401850_0 = (lambda v7229: (v7229 if (v7229 > 0) else 0))((rbl1_701240_5 + ((revqtotqht_701040_23 - sdd_701250_0) - sdc1_701260_0)))

    # QF011: Quotient familial
    # Defined in file ir-calcul/sources2017m_6_10/chap-52.m, from 102:9 to 102:26
    qf011_521050_0 = ((round(ri1_701170_0) / nbpt_601000_0) if nbpt_601000_0 != 0.0 else Undefined())

    # QF021: Quotient familial
    # Defined in file ir-calcul/sources2017m_6_10/chap-52.m, from 103:9 to 103:26
    qf021_521050_1 = ((round(ri1_701170_0) / 2) if 2 != 0.0 else Undefined())

    # TONEQUO:  revenus au quotient >=2 nets
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1789:11 to 1790:113
    tonequo_701190_3 = (lambda v3620: (v3620 if (v3620 > 0) else 0))((revqtot_701040_22 - (sdd_701250_0 + (sdc_701270_0 + (sdv_701280_0 + sdm_701280_1)))))

    # REVQUOQHT:  revenus au quotient >=2 nets hors tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1786:13 to 1786:55
    revquoqht_701190_1 = (lambda v3616: (v3616 if (v3616 > 0) else 0))(((((revqtotqht_701040_23 - sdd_701250_0) - sdc_701270_0) - sdv_701280_0) - sdm_701280_1))

    # TONEQUOHT:  revenus au quotient >=2 nets tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 1787:13 to 1788:113
    tonequoht_701190_2 = (lambda v3618: (v3618 if (v3618 > 0) else 0))((revqtotqht_701040_23 - (sdd_701250_0 + (sdc_701270_0 + (sdv_701280_0 + sdm_701280_1)))))

    # BON: Base reduction dons aux oeuvres
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3504:7 to 3504:122
    bon_401850_4 = round((lambda v7233: (v7233 if (v7233 < 0) else 0))((revdon_401850_0 * 0.200000)))

    # DS011: Droits simples issus du bareme
    # Defined in file ir-calcul/sources2017m_6_10/chap-52.m, from 50:9 to 50:132
    ds011_521010_0 = (((((lambda v17090: (0 if (0 > v17090) else v17090))((qf011_521050_0 - 9807.000000)) * 0.140000) + ((lambda v17092: (0 if (0 > v17092) else v17092))((qf011_521050_0 - 27086.000000)) * 0.160000)) + ((lambda v17094: (0 if (0 > v17094) else v17094))((qf011_521050_0 - 72617.000000)) * 0.110000)) + ((lambda v17096: (0 if (0 > v17096) else v17096))((qf011_521050_0 - 153783.000000)) * 0.040000))

    # DS021: Droits simples issus du bareme
    # Defined in file ir-calcul/sources2017m_6_10/chap-52.m, from 50:9 to 50:132
    ds021_521010_2 = (((((lambda v17170: (0 if (0 > v17170) else v17170))((qf021_521050_1 - 9807.000000)) * 0.140000) + ((lambda v17172: (0 if (0 > v17172) else v17172))((qf021_521050_1 - 27086.000000)) * 0.160000)) + ((lambda v17174: (0 if (0 > v17174) else v17174))((qf021_521050_1 - 72617.000000)) * 0.110000)) + ((lambda v17176: (0 if (0 > v17176) else v17176))((qf021_521050_1 - 153783.000000)) * 0.040000))

    # TGL4: Revenus au quotient 1 hors revenus footballeurs nets imposables
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 662:8 to 662:38
    tgl4_701060_9 = floor((tonequo_701190_3 * ((rev4_701040_6 / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLFV: pensions footballeurs imposables - Vous
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 664:9 to 664:54
    tglfv_701060_11 = floor((tonequo_701190_3 * ((tpenfv_99992240_18 / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLTSV: Traitements AFN imposables tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 678:10 to 678:61
    tgltsv_701060_25 = floor((tonequo_701190_3 * (((ttsnn2tsv_99992230_24 + tsnn2tsv_1731_701037_0) / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLTSREMPV: Traitements  imposables quotient tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 698:14 to 698:69
    tgltsrempv_701060_45 = floor((tonequo_701190_3 * (((ttsnn2rempv_99992230_42 + tsnn2rempv_1731_701037_7) / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLPRRV: Revenus au quotient pension normale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 705:11 to 705:56
    tglprrv_701060_52 = floor((tonequo_701190_3 * ((tprnv_99992130_90 / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLPRRZV: Revenus au quotient pension normale code RxZ tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 711:12 to 711:59
    tglprrzv_701060_58 = floor((tonequo_701190_3 * ((tprr2zv_99992240_12 / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLPRRC: Revenus au quotient pension normale tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 706:11 to 706:56
    tglprrc_701060_53 = floor((tonequo_701190_3 * ((tprnc_99992130_103 / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLPRRZC: Revenus au quotient pension normale code RxZ tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 712:12 to 712:59
    tglprrzc_701060_59 = floor((tonequo_701190_3 * ((tprr2zc_99992240_13 / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLFC: pensions footballeurs imposables - 
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 665:9 to 665:54
    tglfc_701060_12 = floor((tonequo_701190_3 * ((tpenfc_99992240_19 / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLTSC: Traitements AFN imposables tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 679:10 to 679:61
    tgltsc_701060_26 = floor((tonequo_701190_3 * (((ttsnn2tsc_99992230_25 + tsnn2tsc_1731_701037_1) / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # TGLTSREMPC: Traitements  imposables quotient tx effectif
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 699:14 to 699:69
    tgltsrempc_701060_46 = floor((tonequo_701190_3 * (((ttsnn2rempc_99992230_43 + tsnn2rempc_1731_701037_8) / revqtot_701040_22) if revqtot_701040_22 != 0.0 else Undefined())))

    # GL4: Revenus au quotient 4 hors revenus footballeurs nets imposables
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 365:7 to 365:43
    gl4_701050_9 = floor((tonequoht_701190_2 * ((rev4ht_701040_7 / revqtotqht_701040_23) if revqtotqht_701040_23 != 0.0 else Undefined())))

    # TGLRAF: TS et PR quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 685:10 to 685:51
    tglraf_701060_32 = floor((tonequoht_701190_2 * ((ttsnn2raf_99992230_30 / revqtotqht_701040_23) if revqtotqht_701040_23 != 0.0 else Undefined())))

    # TGLRAG: TS et PR quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 691:10 to 691:51
    tglrag_701060_38 = floor((tonequoht_701190_2 * ((ttsnn2rag_99992230_36 / revqtotqht_701040_23) if revqtotqht_701040_23 != 0.0 else Undefined())))

    # TGLRAL: TS et PR quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 717:10 to 717:50
    tglral_701060_64 = floor((tonequoht_701190_2 * ((tprr2ral_99992240_24 / revqtotqht_701040_23) if revqtotqht_701040_23 != 0.0 else Undefined())))

    # TGLRAM: TS et PR quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 723:10 to 723:50
    tglram_701060_70 = floor((tonequoht_701190_2 * ((tprr2ram_99992240_30 / revqtotqht_701040_23) if revqtotqht_701040_23 != 0.0 else Undefined())))

    # TGLRBL: TS et PR quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 718:10 to 718:50
    tglrbl_701060_65 = floor((tonequoht_701190_2 * ((tprr2rbl_99992240_25 / revqtotqht_701040_23) if revqtotqht_701040_23 != 0.0 else Undefined())))

    # TGLRBM: TS et PR quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 724:10 to 724:50
    tglrbm_701060_71 = floor((tonequoht_701190_2 * ((tprr2rbm_99992240_31 / revqtotqht_701040_23) if revqtotqht_701040_23 != 0.0 else Undefined())))

    # TGLRBF: TS et PR quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 686:10 to 686:51
    tglrbf_701060_33 = floor((tonequoht_701190_2 * ((ttsnn2rbf_99992230_31 / revqtotqht_701040_23) if revqtotqht_701040_23 != 0.0 else Undefined())))

    # TGLRBG: TS et PR quotient
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 692:10 to 692:51
    tglrbg_701060_39 = floor((tonequoht_701190_2 * ((ttsnn2rbg_99992230_37 / revqtotqht_701040_23) if revqtotqht_701040_23 != 0.0 else Undefined())))

    # RON: Reduction dons aux oeuvres
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3543:7 to 3543:48
    ron_401870_0 = round((bon_401850_4 * 0.660000))

    # IS011: Droits simples avant plafonnement du quotient familial
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 1161:9 to 1161:27
    is011_511380_0 = round((ds011_521010_0 * nbpt_601000_0))

    # IS021: Droits simples avant plafonnement du quotient familial
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 1162:9 to 1162:27
    is021_511380_1 = round((ds021_521010_2 * 2))

    # TGLFTOT: pensions footballeurs imposables - 
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 670:11 to 670:34
    tglftot_701060_17 = (tglfv_701060_11 + tglfc_701060_12)

    # TGLTSREMPTOT: gain levee  quotient total foyer  tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 704:16 to 704:44
    tgltsremptot_701060_51 = (tgltsrempv_701060_45 + tgltsrempc_701060_46)

    # RPQ4: Revenu imposable suivant le quotient soumis au bar}me
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 960:8 to 960:27
    rpq4_701090_3 = floor(((gl4_701050_9 / 4.000000) if 4.000000 != 0.0 else Undefined()))

    # TGLPRRTOT: gain levee  quotient total foyer  tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 729:13 to 729:131
    tglprrtot_701060_76 = (((tglprrv_701060_52 + tglprrzv_701060_58) + (tglprrc_701060_53 + tglprrzc_701060_59)) + (tglral_701060_64 + (tglrbl_701060_65 + (tglram_701060_70 + tglrbm_701060_71))))

    # TGLTSTOT: gain levee  quotient total foyer  tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 697:12 to 697:120
    tgltstot_701060_44 = ((tgltsv_701060_25 + tgltsc_701060_26) + (tglraf_701060_32 + (tglrbf_701060_33 + (tglrag_701060_38 + tglrbg_701060_39))))

    # IN01: Droits simples apres plafonnement QF
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 1070:6 to 1070:26
    in01_511320_0 = (lambda v15414: (v15414 if (v15414 > is011_511380_0) else is011_511380_0))((is021_511380_1 - plant_511350_1))

    # RB51: Revenu imposable pour le calcul du quotient familial
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 47:8 to 47:34
    rb51_701000_0 = (lambda v3458: (v3458 if (v3458 > 0) else 0))((ri1_701170_0 + rpq4_701090_3))

    # TGLRF2: Revenus au quotient hors rev foot nets imposables tx eff
    # Defined in file ir-calcul/sources2017m_6_10/chap-7.m, from 747:12 to 747:105
    tglrf2_701060_94 = (((((tonequo_701190_3 - tgl4_701060_9) - tglftot_701060_17) - tgltstot_701060_44) - tgltsremptot_701060_51) - tglprrtot_701060_76)

    # QF511: Quotient familial
    # Defined in file ir-calcul/sources2017m_6_10/chap-52.m, from 106:9 to 106:26
    qf511_521050_4 = ((round(rb51_701000_0) / nbpt_601000_0) if nbpt_601000_0 != 0.0 else Undefined())

    # QF521: Quotient familial
    # Defined in file ir-calcul/sources2017m_6_10/chap-52.m, from 107:9 to 107:26
    qf521_521050_5 = ((round(rb51_701000_0) / 2) if 2 != 0.0 else Undefined())

    # QUOKIREHR: revenus au quotient>>=2 servant pour REVKIRE haut revenu
    # Defined in file ir-calcul/sources2017m_6_10/chap-87.m, from 357:15 to 364:85
    quokirehr_871125_4 = (tgl4_701060_9 + (((tglprrv_701060_52 + (tglprrzv_701060_58 + (tglfv_701060_11 + (tgltsv_701060_25 + tgltsrempv_701060_45)))) + (tglprrc_701060_53 + (tglprrzc_701060_59 + (tglfc_701060_12 + (tgltsc_701060_26 + tgltsrempc_701060_46))))) + (tglrf2_701060_94 + (tglraf_701060_32 + (tglrbf_701060_33 + (tglrag_701060_38 + (tglrbg_701060_39 + (tglral_701060_64 + (tglrbl_701060_65 + (tglram_701060_70 + tglrbm_701060_71))))))))))

    # DS511: Droits simples issus du bareme
    # Defined in file ir-calcul/sources2017m_6_10/chap-52.m, from 50:9 to 50:132
    ds511_521010_4 = (((((lambda v17250: (0 if (0 > v17250) else v17250))((qf511_521050_4 - 9807.000000)) * 0.140000) + ((lambda v17252: (0 if (0 > v17252) else v17252))((qf511_521050_4 - 27086.000000)) * 0.160000)) + ((lambda v17254: (0 if (0 > v17254) else v17254))((qf511_521050_4 - 72617.000000)) * 0.110000)) + ((lambda v17256: (0 if (0 > v17256) else v17256))((qf511_521050_4 - 153783.000000)) * 0.040000))

    # DS521: Droits simples issus du bareme
    # Defined in file ir-calcul/sources2017m_6_10/chap-52.m, from 50:9 to 50:132
    ds521_521010_6 = (((((lambda v17330: (0 if (0 > v17330) else v17330))((qf521_521050_5 - 9807.000000)) * 0.140000) + ((lambda v17332: (0 if (0 > v17332) else v17332))((qf521_521050_5 - 27086.000000)) * 0.160000)) + ((lambda v17334: (0 if (0 > v17334) else v17334))((qf521_521050_5 - 72617.000000)) * 0.110000)) + ((lambda v17336: (0 if (0 > v17336) else v17336))((qf521_521050_5 - 153783.000000)) * 0.040000))

    # REVKIREHR: Revenu de reference haut revenus
    # Defined in file ir-calcul/sources2017m_6_10/chap-87.m, from 365:14 to 386:47
    revkirehr_871125_5 = round((lambda v12398: (v12398 if (v12398 > 0) else 0))(((rblrfrhr_701240_8 if (rblrfrhr_701240_8 > 0) else 0) + (quokirehr_871125_4 + (aperpv_31022_6 + (aperpc_31022_7 + aperpp_31022_8))))))

    # IS511: Droits simples avant plafonnement du quotient familial
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 1167:9 to 1167:27
    is511_511380_6 = round((ds511_521010_4 * nbpt_601000_0))

    # IS521: Droits simples avant plafonnement du quotient familial
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 1168:9 to 1168:27
    is521_511380_7 = round((ds521_521010_6 * 2))

    # CHRREEL1: Haut revenu calcul contrib.
    # Defined in file ir-calcul/sources2017m_6_10/chap-thr.m, from 40:12 to 41:108
    chrreel1_80000_10 = (((1 if ((1000000.000000 - revkirehr_871125_5) >= 0) else 0) * ((revkirehr_871125_5 - 500000.000000) * 0.030000)) + (15000.000000 * (1 if ((revkirehr_871125_5 - 1000000.000000) > 0) else 0)))

    # CHRREEL2: Haut revenu calcul contrib.
    # Defined in file ir-calcul/sources2017m_6_10/chap-thr.m, from 42:12 to 42:62
    chrreel2_80000_11 = (lambda v22211: (v22211 if (v22211 > 0) else 0))(((revkirehr_871125_5 - 1000000.000000) * 0.040000))

    # IN51: Droits simples apres plafonnement QF pour calcul reduc compl.
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 1072:6 to 1072:26
    in51_511320_2 = (lambda v15418: (v15418 if (v15418 > is511_511380_6) else is511_511380_6))((is521_511380_7 - plant_511350_1))

    # CHRREELTOT: Haut revenu calcul contrib. theorique total
    # Defined in file ir-calcul/sources2017m_6_10/chap-thr.m, from 43:14 to 43:45
    chrreeltot_80000_12 = round((lambda v22213: (v22213 if (v22213 > 0) else 0))((chrreel1_80000_10 + chrreel2_80000_11)))

    # IQUOTOT1: Impot au quotient tous quotients confondus
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 129:12 to 129:23
    iquotot1_511090_0 = (in51_511320_2 - in01_511320_0)

    # ZIPQ41: Droit sur les bases de quotient 1 @ x
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 175:12 to 175:42
    zipq41_511100_18 = (iquotot1_511090_0 * ((rpq4_701090_3 / rpq4_701090_3) if rpq4_701090_3 != 0.0 else Undefined()))

    # IPQ41: Droit sur les bases de quotient 1 a x
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 664:9 to 664:19
    ipq41_511300_8 = (zipq41_511100_18 * 4)

    # IPQ1001: Droit sur les bases de quotient 1 a x
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 833:11 to 843:88
    ipq1001_511300_174 = round(ipq41_511300_8)

    # IPQ1: Droits simples totaux
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 78:8 to 78:23
    ipq1_511020_0 = (in01_511320_0 + ipq1001_511300_174)

    # ID11: Droits simples avant abattement DOM
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 65:9 to 65:61
    id11_511010_0 = round((ipq1_511020_0 * ((round((ri1_701170_0 + revquoqht_701190_1)) / round((ri1_701170_0 + tonequo_701190_3))) if round((ri1_701170_0 + tonequo_701190_3)) != 0.0 else Undefined())))

    # IDOM11: Droits simples definitifs
    # Defined in file ir-calcul/sources2017m_6_10/chap-51.m, from 1083:11 to 1084:59
    idom11_511330_0 = (False if (False > id11_511010_0) else id11_511010_0)

    # DEC11: Decote totale
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 266:9 to 266:129
    dec11_401140_0 = (lambda v4342: (idom11_511330_0 if (idom11_511330_0 < v4342) else v4342))((lambda v4340: (0 if (0 > v4340) else v4340))(round((1939.000000 - (idom11_511330_0 * 0.750000)))))

    # RCOTFOR_1: Cotisations assurance pour la foret - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3698:13 to 3698:116
    rcotfor_1_401940_3 = (lambda v7316: (0 if (0 > v7316) else v7316))((lambda v7313: (v7313 if (v7313 < 0) else 0))((idom11_511330_0 - dec11_401140_0)))

    # RREPA_1:  reduction non plaf pour 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2243:11 to 2243:102
    rrepa_1_401410_5 = (lambda v6226: (0 if (0 > v6226) else v6226))((lambda v6223: (v6223 if (v6223 < 0) else 0))(((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3)))

    # RDIFAGRI_1:  reduction non plaf pour 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3774:14 to 3774:112
    rdifagri_1_401980_4 = (lambda v7398: (0 if (0 > v7398) else v7398))((lambda v7395: (v7395 if (v7395 < 0) else 0))((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5)))

    # RPRESSE_1: pour simplifier programmation - investissement presse
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 317:13 to 317:126
    rpresse_1_401180_5 = (lambda v4366: (0 if (0 > v4366) else v4366))((lambda v4363: (v4363 if (v4363 < 0) else 0))(((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4)))

    # RFORET_1: Pour alleger formule : Reduction cotisation pour defense des forets
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 334:13 to 334:138
    rforet_1_401185_4 = (lambda v4378: (0 if (0 > v4378) else v4378))((lambda v4375: (v4375 if (v4375 < 0) else 0))((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5)))

    # RCINE_1: pour simplifier programmation - Cinema audiovisuel
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 409:11 to 410:47
    rcine_1_401220_7 = (lambda v4458: (0 if (0 > v4458) else v4458))((lambda v4455: (v4455 if (v4455 < 0) else 0))(((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4)))

    # RFIPDOM_1:  reduction non plaf pour 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 350:13 to 350:155
    rfipdom_1_401190_4 = (lambda v4392: (0 if (0 > v4392) else v4392))((lambda v4389: (v4389 if (v4389 < 0) else 0))((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rcine_1_401220_7)))

    # RCINE: Cinema audiovisuel
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 412:9 to 412:80
    rcine_401220_8 = (rcine_1_401220_7 if (rcine_1_401220_7 > 0) else 0)

    # RFIPC_1: Pour alleger programmation - Reduction FIP Corse
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 367:11 to 368:86
    rfipc_1_401200_4 = (lambda v4406: (0 if (0 > v4406) else v4406))((lambda v4403: (v4403 if (v4403 < 0) else 0))(((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rcine_1_401220_7) - rfipdom_1_401190_4)))

    # RRESTIMO_1: pour alleger prog - Travaux de restauration immobiliere - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3455:14 to 3456:84
    rrestimo_1_401845_3 = (lambda v7200: (0 if (0 > v7200) else v7200))((lambda v7197: (v7197 if (v7197 < 0) else 0))((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rfipdom_1_401190_4) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipc_1_401200_4) - rcine_1_401220_7)))

    # RSOCREPR_1: pour simplifier programmation - Reduction emprunt reprise societe
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3369:14 to 3370:99
    rsocrepr_1_401830_4 = (lambda v7146: (0 if (0 > v7146) else v7146))((lambda v7143: (v7143 if (v7143 < 0) else 0))(((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipdom_1_401190_4) - rfipc_1_401200_4) - rcine_1_401220_7) - rrestimo_1_401845_3)))

    # RRESTIMO: Travaux de restauration immobiliere avant 2017 - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3458:12 to 3460:17
    rrestimo_401845_4 = (rrestimo_1_401845_3 if (rrestimo_1_401845_3 > 0) else 0)

    # RSOCREPR: Reduction emprunt reprise societe
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3372:12 to 3374:16
    rsocrepr_401830_5 = (rsocrepr_1_401830_4 if (rsocrepr_1_401830_4 > 0) else 0)

    # RRPRESCOMP_1: pour alleger prog - reduction prestations compensatoires deduit
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3676:16 to 3677:94
    rrprescomp_1_401930_5 = (lambda v7300: (0 if (0 > v7300) else v7300))((lambda v7297: (v7297 if (v7297 < 0) else 0))((((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rfipdom_1_401190_4) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipc_1_401200_4) - rcine_1_401220_7) - rrestimo_1_401845_3) - rsocrepr_1_401830_4)))

    # RHEBE_1: pour alleger programmation - Reduction retenue depense d'hebergement
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2220:11 to 2221:54
    rhebe_1_401400_4 = (lambda v6212: (0 if (0 > v6212) else v6212))((lambda v6209: (v6209 if (v6209 < 0) else 0))(((((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipdom_1_401190_4) - rfipc_1_401200_4) - rcine_1_401220_7) - rrestimo_1_401845_3) - rsocrepr_1_401830_4) - rrprescomp_1_401930_5)))

    # RRPRESCOMP: reduction prestations compensatoires deduit
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3679:14 to 3680:75
    rrprescomp_401930_6 = (rrprescomp_1_401930_5 if (rrprescomp_1_401930_5 > 0) else 0)

    # RSURV_1: pour alleger programmation - Rente-survie, contrat handicap
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 386:11 to 387:77
    rsurv_1_401210_4 = (lambda v4416: (0 if (0 > v4416) else v4416))((lambda v4413: (v4413 if (v4413 < 0) else 0))((((((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipdom_1_401190_4) - rfipc_1_401200_4) - rcine_1_401220_7) - rrestimo_1_401845_3) - rsocrepr_1_401830_4) - rrprescomp_1_401930_5) - rhebe_1_401400_4)))

    # RHEBE: Reduction retenue depense d'hebergement
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2223:9 to 2224:59
    rhebe_401400_5 = (rhebe_1_401400_4 if (rhebe_1_401400_4 > 0) else 0)

    # RSURV: Rente-survie, contrat handicap
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 389:9 to 391:13
    rsurv_401210_5 = (rsurv_1_401210_4 if (rsurv_1_401210_4 > 0) else 0)

    # RINNO_1: pour alleger prog - reduc. Souscrip Parts Fonds Communs Plac Innov.
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3609:11 to 3610:72
    rinno_1_401920_3 = (lambda v7286: (0 if (0 > v7286) else v7286))((lambda v7283: (v7283 if (v7283 < 0) else 0))(((((((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rfipdom_1_401190_4) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipc_1_401200_4) - rcine_1_401220_7) - rrestimo_1_401845_3) - rsocrepr_1_401830_4) - rrprescomp_1_401930_5) - rhebe_1_401400_4) - rsurv_1_401210_4)))

    # RSOUFIP_1: pour alleger la programmation - Investissement de proximite
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 428:13 to 429:81
    rsoufip_1_401230_4 = (lambda v4470: (0 if (0 > v4470) else v4470))((lambda v4467: (v4467 if (v4467 < 0) else 0))((((((((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipdom_1_401190_4) - rfipc_1_401200_4) - rcine_1_401220_7) - rrestimo_1_401845_3) - rsocrepr_1_401830_4) - rrprescomp_1_401930_5) - rhebe_1_401400_4) - rsurv_1_401210_4) - rinno_1_401920_3)))

    # RINNO: Reduction FCP dans l'innovation- Affichage
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3612:9 to 3614:13
    rinno_401920_4 = (rinno_1_401920_3 if (rinno_1_401920_3 > 0) else 0)

    # RSOUFIP: Investissement de proximite
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 431:11 to 433:15
    rsoufip_401230_5 = (rsoufip_1_401230_4 if (rsoufip_1_401230_4 > 0) else 0)

    # RRIRENOV_1: pour alleger prog - Travaux de restauration objets classes - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 449:14 to 450:85
    rrirenov_1_401240_4 = (lambda v4482: (0 if (0 > v4482) else v4482))((lambda v4479: (v4479 if (v4479 < 0) else 0))(((((((((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipdom_1_401190_4) - rfipc_1_401200_4) - rcine_1_401220_7) - rrestimo_1_401845_3) - rsocrepr_1_401830_4) - rrprescomp_1_401930_5) - rhebe_1_401400_4) - rsurv_1_401210_4) - rinno_1_401920_3) - rsoufip_1_401230_4)))

    # RRIRENOV: Travaux de restauration des objets classes - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 452:12 to 452:92
    rrirenov_401240_5 = (rrirenov_1_401240_4 if (rrirenov_1_401240_4 > 0) else 0)

    # RRI1: Reductions impot lot 1
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2204:8 to 2205:94
    rri1_401390_0 = (((((((((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipdom_1_401190_4) - rfipc_1_401200_4) - rcine_401220_8) - rrestimo_401845_4) - rsocrepr_401830_5) - rrprescomp_401930_6) - rhebe_401400_5) - rsurv_401210_5) - rinno_401920_4) - rsoufip_401230_5) - rrirenov_401240_5)

    # RLOGDOM: Investissements outre mer logement - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 6704:11 to 6704:61
    rlogdom_402060_0 = (rri1_401390_0 if (rri1_401390_0 < False) else False)

    # RCOMP_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 470:11 to 470:44
    rcomp_1_401260_0 = (lambda v4502: (0 if (0 > v4502) else v4502))((lambda v4499: (v4499 if (v4499 < False) else False))((rri1_401390_0 - rlogdom_402060_0)))

    # RCOMP: Frais de comptabilite
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 472:9 to 474:13
    rcomp_401260_1 = (rcomp_1_401260_0 if (rcomp_1_401260_0 > 0) else 0)

    # RRETU_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3593:11 to 3593:53
    rretu_1_401910_0 = (lambda v7272: (0 if (0 > v7272) else v7272))((lambda v7269: (v7269 if (v7269 < 0) else 0))(((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0)))

    # RRETU: Reduct. etudiants: montant retenu
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3595:9 to 3595:80
    rretu_401910_1 = (rretu_1_401910_0 if (rretu_1_401910_0 > 0) else 0)

    # RDONS_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3554:11 to 3554:62
    rdons_1_401880_0 = (lambda v7254: (0 if (0 > v7254) else v7254))((lambda v7251: (v7251 if (v7251 < ron_401870_0) else ron_401870_0))((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0)))

    # RDONS: Dons aux oeuvres
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3556:9 to 3558:13
    rdons_401880_1 = (rdons_1_401880_0 if (rdons_1_401880_0 > 0) else 0)

    # CRDIE_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3568:11 to 3568:72
    crdie_1_401887_0 = (lambda v7262: (0 if (0 > v7262) else v7262))((lambda v7259: (v7259 if (v7259 < 0) else 0))(((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0)))

    # CRDIE: Credit d'impot convention franco-allemande
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3569:9 to 3569:70
    crdie_401887_1 = (lambda v7266: (0 if (0 > v7266) else v7266))((lambda v7263: (v7263 if (v7263 < 0) else 0))(((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0)))

    # RRI1DUPI: Reductions impot lot 1 pr art. 1731 bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 678:12 to 678:66
    rri1dupi_401280_0 = (((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0)

    # RDUFREPFI_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 680:15 to 680:51
    rdufrepfi_1_401280_1 = (lambda v4701: (v4701 if (v4701 > 0) else 0))((rri1dupi_401280_0 if (rri1dupi_401280_0 < 0) else 0))

    # RDUFREPFI: Ivt Duflot realise acheve 2013 - report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 681:13 to 681:98
    rdufrepfi_401280_2 = (rdufrepfi_1_401280_1 if (rdufrepfi_1_401280_1 > 0) else 0)

    # RDUFREPFK_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 684:15 to 684:61
    rdufrepfk_1_401280_4 = (lambda v4709: (v4709 if (v4709 > 0) else 0))((lambda v4707: (v4707 if (v4707 < 0) else 0))((rri1dupi_401280_0 - rdufrepfi_1_401280_1)))

    # RDUFREPFK: Ivt Duflot realise acheve 2014 - report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 685:13 to 685:98
    rdufrepfk_401280_5 = (rdufrepfk_1_401280_4 if (rdufrepfk_1_401280_4 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 686:11 to 686:32
    vartmp1_401280_6 = (rdufrepfi_1_401280_1 + rdufrepfk_1_401280_4)

    # RDUFREPFR_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 688:15 to 688:61
    rdufrepfr_1_401280_7 = (lambda v4717: (v4717 if (v4717 > 0) else 0))((lambda v4715: (v4715 if (v4715 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_6)))

    # RDUFREPFR: Ivt Duflot realise acheve 2015 - report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 689:13 to 689:99
    rdufrepfr_401280_8 = (rdufrepfr_1_401280_7 if (rdufrepfr_1_401280_7 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 690:11 to 690:32
    vartmp1_401280_9 = (vartmp1_401280_6 + rdufrepfr_1_401280_7)

    # RDUFREPFV_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 692:15 to 692:61
    rdufrepfv_1_401280_10 = (lambda v4725: (v4725 if (v4725 > 0) else 0))((lambda v4723: (v4723 if (v4723 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_9)))

    # RDUFREPFV: Ivt Duflot realise acheve 2016 - report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 693:13 to 693:99
    rdufrepfv_401280_11 = (rdufrepfv_1_401280_10 if (rdufrepfv_1_401280_10 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 694:11 to 694:32
    vartmp1_401280_12 = (vartmp1_401280_9 + rdufrepfv_1_401280_10)

    # RDUFREP: Investissement Duflot reports - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 786:11 to 786:56
    rdufrep_401280_81 = (rdufrepfi_401280_2 + (rdufrepfk_401280_5 + (rdufrepfr_401280_8 + rdufrepfv_401280_11)))

    # RDUFLOEKL_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 696:15 to 696:60
    rdufloekl_1_401280_13 = (lambda v4733: (v4733 if (v4733 > 0) else 0))((lambda v4731: (v4731 if (v4731 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_12)))

    # RDUFLOEKL: Ivt Duflot realise acheve 2014 -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 697:13 to 697:101
    rdufloekl_401280_14 = (rdufloekl_1_401280_13 if (rdufloekl_1_401280_13 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 698:11 to 698:32
    vartmp1_401280_15 = (vartmp1_401280_12 + rdufloekl_1_401280_13)

    # RDUFLOGIH_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 700:15 to 700:60
    rduflogih_1_401280_16 = (lambda v4741: (v4741 if (v4741 > 0) else 0))((lambda v4739: (v4739 if (v4739 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_15)))

    # RDUFLOGIH: Ivt Duflot realise acheve 2013 -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 701:13 to 701:100
    rduflogih_401280_17 = (rduflogih_1_401280_16 if (rduflogih_1_401280_16 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 702:11 to 702:32
    vartmp1_401280_18 = (vartmp1_401280_15 + rduflogih_1_401280_16)

    # RDUFLOTOT_1: somme investissement Duflot 
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 794:15 to 794:96
    rduflotot_1_401280_86 = (rdufrepfi_1_401280_1 + (rdufrepfk_1_401280_4 + (rdufrepfr_1_401280_7 + (rdufrepfv_1_401280_10 + (rduflogih_1_401280_16 + rdufloekl_1_401280_13)))))

    # RDUFLO: Investissement Duflot - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 790:10 to 790:31
    rduflo_401280_84 = (rduflogih_401280_17 + rdufloekl_401280_14)

    # RPIREPAI_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 704:14 to 704:59
    rpirepai_1_401280_19 = (lambda v4749: (v4749 if (v4749 > 0) else 0))((lambda v4747: (v4747 if (v4747 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_18)))

    # RDUFLOTOT: somme investissement Duflot 
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 792:13 to 792:29
    rduflotot_401280_85 = (rdufrep_401280_81 + rduflo_401280_84)

    # RPIREPAI: Ivt Pinel realise acheve 2014 -engagement 6 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 705:12 to 705:95
    rpirepai_401280_20 = (rpirepai_1_401280_19 if (rpirepai_1_401280_19 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 706:11 to 706:31
    vartmp1_401280_21 = (vartmp1_401280_18 + rpirepai_1_401280_19)

    # RPIREPBI_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 708:14 to 708:59
    rpirepbi_1_401280_22 = (lambda v4757: (v4757 if (v4757 > 0) else 0))((lambda v4755: (v4755 if (v4755 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_21)))

    # RPIREPBI: Ivt Pinel realise acheve 2014 -engagement 9 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 709:12 to 709:95
    rpirepbi_401280_23 = (rpirepbi_1_401280_22 if (rpirepbi_1_401280_22 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 710:11 to 710:31
    vartmp1_401280_24 = (vartmp1_401280_21 + rpirepbi_1_401280_22)

    # RPIREPCI_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 712:14 to 712:59
    rpirepci_1_401280_25 = (lambda v4765: (v4765 if (v4765 > 0) else 0))((lambda v4763: (v4763 if (v4763 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_24)))

    # RPIREPCI: Ivt Pinel DOM realise acheve 2014 -engagement 6 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 713:12 to 713:95
    rpirepci_401280_26 = (rpirepci_1_401280_25 if (rpirepci_1_401280_25 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 714:11 to 714:31
    vartmp1_401280_27 = (vartmp1_401280_24 + rpirepci_1_401280_25)

    # RPIREPDI_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 716:14 to 716:59
    rpirepdi_1_401280_28 = (lambda v4773: (v4773 if (v4773 > 0) else 0))((lambda v4771: (v4771 if (v4771 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_27)))

    # RPIREPDI: Ivt Pinel DOM realise acheve 2014 -engagement 9 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 717:12 to 717:95
    rpirepdi_401280_29 = (rpirepdi_1_401280_28 if (rpirepdi_1_401280_28 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 718:11 to 718:31
    vartmp1_401280_30 = (vartmp1_401280_27 + rpirepdi_1_401280_28)

    # RPIREPBZ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 720:14 to 720:59
    rpirepbz_1_401280_31 = (lambda v4781: (v4781 if (v4781 > 0) else 0))((lambda v4779: (v4779 if (v4779 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_30)))

    # RPIREPBZ: Ivt Pinel realise acheve 2015 -engagement 6 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 721:12 to 721:95
    rpirepbz_401280_32 = (rpirepbz_1_401280_31 if (rpirepbz_1_401280_31 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 722:11 to 722:31
    vartmp1_401280_33 = (vartmp1_401280_30 + rpirepbz_1_401280_31)

    # RPIREPCZ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 724:14 to 724:59
    rpirepcz_1_401280_34 = (lambda v4789: (v4789 if (v4789 > 0) else 0))((lambda v4787: (v4787 if (v4787 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_33)))

    # RPIREPCZ: Ivt Pinel realise acheve 2015 -engagement 9 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 725:12 to 725:95
    rpirepcz_401280_35 = (rpirepcz_1_401280_34 if (rpirepcz_1_401280_34 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 726:11 to 726:31
    vartmp1_401280_36 = (vartmp1_401280_33 + rpirepcz_1_401280_34)

    # RPIREPDZ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 728:14 to 728:60
    rpirepdz_1_401280_37 = (lambda v4797: (v4797 if (v4797 > 0) else 0))((lambda v4795: (v4795 if (v4795 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_36)))

    # RPIREPDZ: Ivt Pinel realise acheve 2015 -engagement 6 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 729:12 to 729:94
    rpirepdz_401280_38 = (rpirepdz_1_401280_37 if (rpirepdz_1_401280_37 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 730:11 to 730:31
    vartmp1_401280_39 = (vartmp1_401280_36 + rpirepdz_1_401280_37)

    # RPIREPEZ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 732:14 to 732:59
    rpirepez_1_401280_40 = (lambda v4805: (v4805 if (v4805 > 0) else 0))((lambda v4803: (v4803 if (v4803 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_39)))

    # RPIREPEZ: Ivt Pinel realise acheve 2009 -engagement 9 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 733:12 to 733:94
    rpirepez_401280_41 = (rpirepez_1_401280_40 if (rpirepez_1_401280_40 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 734:11 to 734:31
    vartmp1_401280_42 = (vartmp1_401280_39 + rpirepez_1_401280_40)

    # RPIREPQZ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 736:14 to 736:59
    rpirepqz_1_401280_43 = (lambda v4813: (v4813 if (v4813 > 0) else 0))((lambda v4811: (v4811 if (v4811 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_42)))

    # RPIREPQZ: Ivt Pinel realise acheve 2016 -engagement 6 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 737:12 to 737:94
    rpirepqz_401280_44 = (rpirepqz_1_401280_43 if (rpirepqz_1_401280_43 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 738:11 to 738:31
    vartmp1_401280_45 = (vartmp1_401280_42 + rpirepqz_1_401280_43)

    # RPIREPRZ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 740:14 to 740:59
    rpireprz_1_401280_46 = (lambda v4821: (v4821 if (v4821 > 0) else 0))((lambda v4819: (v4819 if (v4819 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_45)))

    # RPIREPRZ: Ivt Pinel realise acheve 2016 -engagement 6 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 741:12 to 741:94
    rpireprz_401280_47 = (rpireprz_1_401280_46 if (rpireprz_1_401280_46 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 742:11 to 742:31
    vartmp1_401280_48 = (vartmp1_401280_45 + rpireprz_1_401280_46)

    # RPIREPSZ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 744:14 to 744:59
    rpirepsz_1_401280_49 = (lambda v4829: (v4829 if (v4829 > 0) else 0))((lambda v4827: (v4827 if (v4827 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_48)))

    # RPIREPSZ: Ivt Pinel realise acheve 2016 -engagement 6 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 745:12 to 745:94
    rpirepsz_401280_50 = (rpirepsz_1_401280_49 if (rpirepsz_1_401280_49 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 746:11 to 746:31
    vartmp1_401280_51 = (vartmp1_401280_48 + rpirepsz_1_401280_49)

    # RPIREPTZ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 748:14 to 748:59
    rpireptz_1_401280_52 = (lambda v4837: (v4837 if (v4837 > 0) else 0))((lambda v4835: (v4835 if (v4835 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_51)))

    # RPIREPTZ: Ivt Pinel realise acheve 2016 -engagement 6 ans- report -Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 749:12 to 749:94
    rpireptz_401280_53 = (rpireptz_1_401280_52 if (rpireptz_1_401280_52 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 750:11 to 750:31
    vartmp1_401280_54 = (vartmp1_401280_51 + rpireptz_1_401280_52)

    # RPIREP: Investissement Pinel reports - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 798:10 to 798:139
    rpirep_401280_89 = (rpirepai_401280_20 + (rpirepbi_401280_23 + (rpirepci_401280_26 + (rpirepdi_401280_29 + (rpirepbz_401280_32 + (rpirepcz_401280_35 + (rpirepdz_401280_38 + (rpirepez_401280_41 + (rpirepqz_401280_44 + (rpireprz_401280_47 + (rpirepsz_401280_50 + rpireptz_401280_53)))))))))))

    # RPIQOP_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 752:12 to 752:53
    rpiqop_1_401280_55 = (lambda v4845: (v4845 if (v4845 > 0) else 0))((lambda v4843: (v4843 if (v4843 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_54)))

    # RPIQOP: Ivt Pinel 7QO 7QP -Reduc apres imputation sur droits 
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 753:10 to 753:86
    rpiqop_401280_56 = (rpiqop_1_401280_55 if (rpiqop_1_401280_55 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 754:11 to 754:29
    vartmp1_401280_57 = (vartmp1_401280_54 + rpiqop_1_401280_55)

    # RPIQMN_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 756:12 to 756:53
    rpiqmn_1_401280_58 = (lambda v4853: (v4853 if (v4853 > 0) else 0))((lambda v4851: (v4851 if (v4851 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_57)))

    # RPIQMN: Ivt Pinel 7QM 7QN -Reduc apres imputation sur droits 
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 757:10 to 757:86
    rpiqmn_401280_59 = (rpiqmn_1_401280_58 if (rpiqmn_1_401280_58 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 758:11 to 758:29
    vartmp1_401280_60 = (vartmp1_401280_57 + rpiqmn_1_401280_58)

    # RPIQKL_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 760:12 to 760:53
    rpiqkl_1_401280_61 = (lambda v4861: (v4861 if (v4861 > 0) else 0))((lambda v4859: (v4859 if (v4859 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_60)))

    # RPIQKL: Ivt Pinel 7QK 7QL -Reduc apres imputation sur droits 
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 761:10 to 761:86
    rpiqkl_401280_62 = (rpiqkl_1_401280_61 if (rpiqkl_1_401280_61 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 762:11 to 762:29
    vartmp1_401280_63 = (vartmp1_401280_60 + rpiqkl_1_401280_61)

    # RPIQIJ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 764:12 to 764:53
    rpiqij_1_401280_64 = (lambda v4869: (v4869 if (v4869 > 0) else 0))((lambda v4867: (v4867 if (v4867 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_63)))

    # RPIQIJ: Ivt Pinel 7QI 7QJ -Reduc apres imputation sur droits 
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 765:10 to 765:86
    rpiqij_401280_65 = (rpiqij_1_401280_64 if (rpiqij_1_401280_64 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 766:11 to 766:29
    vartmp1_401280_66 = (vartmp1_401280_63 + rpiqij_1_401280_64)

    # RPIQGH_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 768:12 to 768:53
    rpiqgh_1_401280_67 = (lambda v4877: (v4877 if (v4877 > 0) else 0))((lambda v4875: (v4875 if (v4875 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_66)))

    # RPIQGH: Ivt Pinel realise acheve 2015 -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 769:10 to 769:86
    rpiqgh_401280_68 = (rpiqgh_1_401280_67 if (rpiqgh_1_401280_67 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 770:11 to 770:29
    vartmp1_401280_69 = (vartmp1_401280_66 + rpiqgh_1_401280_67)

    # RPIQEF_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 772:12 to 772:53
    rpiqef_1_401280_70 = (lambda v4885: (v4885 if (v4885 > 0) else 0))((lambda v4883: (v4883 if (v4883 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_69)))

    # RPIQEF: Ivt Pinel realise acheve 2015 -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 773:10 to 773:86
    rpiqef_401280_71 = (rpiqef_1_401280_70 if (rpiqef_1_401280_70 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 774:11 to 774:29
    vartmp1_401280_72 = (vartmp1_401280_69 + rpiqef_1_401280_70)

    # RPIQCD_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 776:12 to 776:53
    rpiqcd_1_401280_73 = (lambda v4893: (v4893 if (v4893 > 0) else 0))((lambda v4891: (v4891 if (v4891 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_72)))

    # RPIQCD: Ivt Pinel realise 2014 acheve 2015 -Reduc apres imputation sur droits 
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 777:10 to 777:86
    rpiqcd_401280_74 = (rpiqcd_1_401280_73 if (rpiqcd_1_401280_73 > 0) else 0)

    # VARTMP1: variable temporaire
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 778:11 to 778:29
    vartmp1_401280_75 = (vartmp1_401280_72 + rpiqcd_1_401280_73)

    # RPIQAB_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 780:12 to 780:53
    rpiqab_1_401280_76 = (lambda v4901: (v4901 if (v4901 > 0) else 0))((lambda v4899: (v4899 if (v4899 < 0) else 0))((rri1dupi_401280_0 - vartmp1_401280_75)))

    # RPIQAB: Ivt Pinel realise 2014 acheve 2015 -Reduc apres imputation sur droits 
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 781:10 to 781:86
    rpiqab_401280_77 = (rpiqab_1_401280_76 if (rpiqab_1_401280_76 > 0) else 0)

    # RPINELTOT_1: somme investissements PINEL
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 806:15 to 807:128
    rpineltot_1_401280_94 = (rpirepai_1_401280_19 + (rpirepbi_1_401280_22 + (rpirepci_1_401280_25 + (rpirepdi_1_401280_28 + (rpirepbz_1_401280_31 + (rpirepcz_1_401280_34 + (rpirepdz_1_401280_37 + (rpirepez_1_401280_40 + (rpirepqz_1_401280_43 + (rpireprz_1_401280_46 + (rpirepsz_1_401280_49 + (rpireptz_1_401280_52 + (rpiqop_1_401280_55 + (rpiqmn_1_401280_58 + (rpiqkl_1_401280_61 + (rpiqij_1_401280_64 + (rpiqgh_1_401280_67 + (rpiqef_1_401280_70 + (rpiqcd_1_401280_73 + rpiqab_1_401280_76)))))))))))))))))))

    # RPINEL: Investissement Pinel - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 802:10 to 802:79
    rpinel_401280_92 = (rpiqop_401280_56 + (rpiqmn_401280_59 + (rpiqkl_401280_62 + (rpiqij_401280_65 + (rpiqgh_401280_68 + (rpiqef_401280_71 + (rpiqcd_401280_74 + rpiqab_401280_77)))))))

    # RNOUV_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2341:11 to 2341:101
    rnouv_1_401430_27 = (lambda v6344: (0 if (0 > v6344) else v6344))((lambda v6341: (v6341 if (v6341 < 0) else 0))((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94)))

    # RPINELTOT: somme investissements PINEL
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 804:13 to 804:28
    rpineltot_401280_93 = (rpirep_401280_89 + rpinel_401280_92)

    # RPENTCY_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2353:13 to 2353:116
    rpentcy_1_401440_3 = (lambda v6352: (0 if (0 > v6352) else v6352))((lambda v6349: (v6349 if (v6349 < False) else False))(((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27)))

    # RPENTDY_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2361:13 to 2361:125
    rpentdy_1_401440_8 = (lambda v6360: (0 if (0 > v6360) else v6360))((lambda v6357: (v6357 if (v6357 < False) else False))((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentcy_1_401440_3)))

    # RPENTEY_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2369:13 to 2370:53
    rpentey_1_401440_13 = (lambda v6368: (0 if (0 > v6368) else v6368))((lambda v6365: (v6365 if (v6365 < False) else False))(((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentcy_1_401440_3) - rpentdy_1_401440_8)))

    # RPENTFY_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2378:13 to 2379:63
    rpentfy_1_401440_18 = (lambda v6376: (0 if (0 > v6376) else v6376))((lambda v6373: (v6373 if (v6373 < False) else False))((((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentcy_1_401440_3) - rpentdy_1_401440_8) - rpentey_1_401440_13)))

    # RPENTOT: Reduction plafonnement PME - total
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2383:11 to 2383:48
    rpentot_401440_20 = (rpentcy_1_401440_3 + (rpentdy_1_401440_8 + (rpentey_1_401440_13 + rpentfy_1_401440_18)))

    # RFOR_1: pour alleger prog - Reduction retenue pour investissement forestier
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3729:10 to 3731:120
    rfor_1_401960_0 = (lambda v7344: (0 if (0 > v7344) else v7344))((lambda v7341: (v7341 if (v7341 < 0) else 0))(((((((((((((((((((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipdom_1_401190_4) - rfipc_1_401200_4) - rcine_1_401220_7) - rrestimo_1_401845_3) - rsocrepr_1_401830_4) - rrprescomp_1_401930_5) - rhebe_1_401400_4) - rsurv_1_401210_4) - rinno_1_401920_3) - rsoufip_1_401230_4) - rrirenov_1_401240_4) - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentot_401440_20)))

    # RTOURREP_1: pour alleger prog - Reduction Investissements locatifs tourisme report
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2140:14 to 2141:81
    rtourrep_1_401340_0 = (lambda v6170: (0 if (0 > v6170) else v6170))((lambda v6167: (v6167 if (v6167 < 0) else 0))(((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentot_401440_20) - rfor_1_401960_0)))

    # RFOR: Reduction retenue pour investissement forestier
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3733:8 to 3733:76
    rfor_401960_1 = (rfor_1_401960_0 if (rfor_1_401960_0 > 0) else 0)

    # RTOUREPA_1: pour alleger prog - Reduction Investissements locatifs tourisme report
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2145:14 to 2146:92
    rtourepa_1_401340_2 = (lambda v6176: (0 if (0 > v6176) else v6176))((lambda v6173: (v6173 if (v6173 < 0) else 0))((((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentot_401440_20) - rfor_1_401960_0) - rtourrep_1_401340_0)))

    # RRREHAP_1: Montant reduction d'impot calcule rehab res tour_imput
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 8043:12 to 8043:182
    rrrehap_1_402161_0 = (lambda v11846: (0 if (0 > v11846) else v11846))((lambda v11843: (v11843 if (v11843 < 0) else 0))((((((((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentcy_1_401440_3) - rpentdy_1_401440_8) - rpentey_1_401440_13) - rpentfy_1_401440_18) - rfor_1_401960_0) - rtourrep_1_401340_0) - rtourepa_1_401340_2)))

    # RRESTIMO_3: pour alleger prog - Travaux de restauration immobiliere - redu 2017 
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3468:14 to 3470:44
    rrestimo_3_401848_2 = (lambda v7208: (0 if (0 > v7208) else v7208))((lambda v7205: (v7205 if (v7205 < 0) else 0))(((((((((((((((((((((((((((((((idom11_511330_0 - dec11_401140_0) - rcotfor_1_401940_3) - rrepa_1_401410_5) - rdifagri_1_401980_4) - rpresse_1_401180_5) - rforet_1_401185_4) - rfipdom_1_401190_4) - rfipc_1_401200_4) - rcine_1_401220_7) - rrestimo_1_401845_3) - rsocrepr_1_401830_4) - rrprescomp_1_401930_5) - rhebe_1_401400_4) - rsurv_1_401210_4) - rinno_1_401920_3) - rsoufip_1_401230_4) - rrirenov_1_401240_4) - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentot_401440_20) - rfor_1_401960_0) - rtourrep_1_401340_0) - rtourepa_1_401340_2) - rrrehap_1_402161_0)))

    # RREHAB: Travaux de rehabilitation residence tourisme - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 8045:10 to 8045:87
    rrehab_402161_1 = (rrrehap_1_402161_0 if (rrrehap_1_402161_0 > 0) else 0)

    # REDUCAVTCEL_1: Art 1731 bis - Invest. Scellier -somme des reduc avant scellier
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1444:18 to 1446:122
    reducavtcel_1_401310_0 = (rcotfor_1_401940_3 + (rrepa_1_401410_5 + (rdifagri_1_401980_4 + (rpresse_1_401180_5 + (rforet_1_401185_4 + (rfipdom_1_401190_4 + (rfipc_1_401200_4 + (rcine_1_401220_7 + (rrestimo_1_401845_3 + (rsocrepr_1_401830_4 + (rrprescomp_1_401930_5 + (rhebe_1_401400_4 + (rsurv_1_401210_4 + (rinno_1_401920_3 + (rsoufip_1_401230_4 + (rrirenov_1_401240_4 + (rlogdom_402060_0 + (rcomp_1_401260_0 + (rretu_1_401910_0 + (rdons_1_401880_0 + (crdie_1_401887_0 + (rduflotot_1_401280_86 + (rpineltot_1_401280_94 + (rnouv_1_401430_27 + (rpentot_401440_20 + (rfor_1_401960_0 + (rtourrep_1_401340_0 + (rtourepa_1_401340_2 + (rrrehap_1_402161_0 + rrestimo_3_401848_2)))))))))))))))))))))))))))))

    # RRESTIMO1: Travaux de restauration immobiliere a/c 2017 - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3472:13 to 3473:68
    rrestimo1_401848_3 = (rrestimo_3_401848_2 if (rrestimo_3_401848_2 > 0) else 0)

    # RCELREPHS_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1566:15 to 1567:28
    rcelrephs_1_401312_7 = (lambda v5496: (0 if (0 > v5496) else v5496))((lambda v5493: (v5493 if (v5493 < False) else False))(((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0)))

    # RCELREPHS: Reduction Scellier  report 2009 - 7HS
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1569:13 to 1569:113
    rcelrephs_401312_8 = (rcelrephs_1_401312_7 if (rcelrephs_1_401312_7 > 0) else 0)

    # RCELREPHR_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1571:15 to 1572:21
    rcelrephr_1_401312_9 = (lambda v5504: (0 if (0 > v5504) else v5504))((lambda v5501: (v5501 if (v5501 < False) else False))((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelrephs_1_401312_7)))

    # RCELREPHR: Scellier metropole-DOM hors plafonds - report 2009 - Reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1574:13 to 1574:112
    rcelrephr_401312_10 = (rcelrephr_1_401312_9 if (rcelrephr_1_401312_9 > 0) else 0)

    # RCELREPHU_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1576:15 to 1577:21
    rcelrephu_1_401312_11 = (lambda v5512: (0 if (0 > v5512) else v5512))((lambda v5509: (v5509 if (v5509 < False) else False))(((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelrephs_1_401312_7) - rcelrephr_1_401312_9)))

    # RCELREPHU: Reduction Scellier  DOM COM engagt 2009 fini 2010 report- 7HU
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1579:13 to 1579:112
    rcelrephu_401312_12 = (rcelrephu_1_401312_11 if (rcelrephu_1_401312_11 > 0) else 0)

    # RCELREPHT_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1581:15 to 1582:74
    rcelrepht_1_401312_13 = (lambda v5520: (0 if (0 > v5520) else v5520))((lambda v5517: (v5517 if (v5517 < False) else False))((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelrephs_1_401312_7) - rcelrephr_1_401312_9) - rcelrephu_1_401312_11)))

    # RCELREPHT: Reduction Scellier metropole DOM report- 7HT
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1584:13 to 1584:113
    rcelrepht_401312_14 = (rcelrepht_1_401312_13 if (rcelrepht_1_401312_13 > 0) else 0)

    # RCELREPHZ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1586:15 to 1587:86
    rcelrephz_1_401312_15 = (lambda v5528: (0 if (0 > v5528) else v5528))((lambda v5525: (v5525 if (v5525 < False) else False))(((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelrephs_1_401312_7) - rcelrephr_1_401312_9) - rcelrephu_1_401312_11) - rcelrepht_1_401312_13)))

    # RCELREPHZ: Reduction Scellier  DOM COM pa 2009 fini 2010 : report- 7HZ
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1589:13 to 1589:113
    rcelrephz_401312_16 = (rcelrephz_1_401312_15 if (rcelrephz_1_401312_15 > 0) else 0)

    # RCELREPHX_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1591:15 to 1592:104
    rcelrephx_1_401312_17 = (lambda v5536: (0 if (0 > v5536) else v5536))((lambda v5533: (v5533 if (v5533 < False) else False))((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelrephs_1_401312_7) - rcelrephr_1_401312_9) - rcelrephu_1_401312_11) - rcelrepht_1_401312_13) - rcelrephz_1_401312_15)))

    # RCELREPHX: Reduction Scellier  metropole pa 2009 fini 2010 : report- 7HX
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1594:13 to 1594:113
    rcelrephx_401312_18 = (rcelrephx_1_401312_17 if (rcelrephx_1_401312_17 > 0) else 0)

    # RCELREPHW_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1596:15 to 1597:110
    rcelrephw_1_401312_19 = (lambda v5544: (0 if (0 > v5544) else v5544))((lambda v5541: (v5541 if (v5541 < False) else False))(((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelrephs_1_401312_7) - rcelrephr_1_401312_9) - rcelrephu_1_401312_11) - rcelrepht_1_401312_13) - rcelrephz_1_401312_15) - rcelrephx_1_401312_17)))

    # RCELREPHW: Reduction Scellier DOM COM report 2010 - 7HW
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1599:13 to 1599:113
    rcelrephw_401312_20 = (rcelrephw_1_401312_19 if (rcelrephw_1_401312_19 > 0) else 0)

    # RCELREPHV_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1601:15 to 1602:122
    rcelrephv_1_401312_21 = (lambda v5552: (0 if (0 > v5552) else v5552))((lambda v5549: (v5549 if (v5549 < False) else False))((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelrephs_1_401312_7) - rcelrephr_1_401312_9) - rcelrephu_1_401312_11) - rcelrepht_1_401312_13) - rcelrephz_1_401312_15) - rcelrephx_1_401312_17) - rcelrephw_1_401312_19)))

    # RCELREPHV: Reduction Scellier metropole report 2010 - 7HV
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1604:13 to 1604:113
    rcelrephv_401312_22 = (rcelrephv_1_401312_21 if (rcelrephv_1_401312_21 > 0) else 0)

    # RCELSOM3: Invest. Scellier - somme des reductions - retenu
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1610:12 to 1610:49
    rcelsom3_401312_25 = (((((((rcelrephs_401312_8 + rcelrephr_401312_10) + rcelrephu_401312_12) + rcelrepht_401312_14) + rcelrephz_401312_16) + rcelrephx_401312_18) + rcelrephw_401312_20) + rcelrephv_401312_22)

    # RCELHJK_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1831:13 to 1832:69
    rcelhjk_1_401316_0 = (lambda v5856: (0 if (0 > v5856) else v5856))((lambda v5853: (v5853 if (v5853 < False) else False))((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25)))

    # RCELHJK: Scellier 7HJ 7HK -Reduction apres imputation sur les droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1834:11 to 1834:104
    rcelhjk_401316_1 = (rcelhjk_1_401316_0 if (rcelhjk_1_401316_0 > 0) else 0)

    # RCELCOM_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1836:13 to 1837:79
    rcelcom_1_401316_2 = (lambda v5864: (0 if (0 > v5864) else v5864))((lambda v5861: (v5861 if (v5861 < False) else False))(((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelhjk_1_401316_0)))

    # RCELCOM: Scellier 2011 COM -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1839:11 to 1839:105
    rcelcom_401316_3 = (rcelcom_1_401316_2 if (rcelcom_1_401316_2 > 0) else 0)

    # RCEL_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1841:10 to 1842:87
    rcel_1_401316_4 = (lambda v5872: (0 if (0 > v5872) else v5872))((lambda v5869: (v5869 if (v5869 < False) else False))((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelhjk_1_401316_0) - rcelcom_1_401316_2)))

    # RCEL: Scellier 2011 hors COM -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1844:8 to 1844:92
    rcel_401316_5 = (rcel_1_401316_4 if (rcel_1_401316_4 > 0) else 0)

    # RCELJOQR_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1846:14 to 1847:98
    rceljoqr_1_401316_6 = (lambda v5880: (0 if (0 > v5880) else v5880))((lambda v5877: (v5877 if (v5877 < False) else False))(((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelhjk_1_401316_0) - rcelcom_1_401316_2) - rcel_1_401316_4)))

    # RCELJOQR: Scellier 2012 COM -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1849:12 to 1849:156
    rceljoqr_401316_7 = (rceljoqr_1_401316_6 if (rceljoqr_1_401316_6 > 0) else 0)

    # RCEL2012_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1851:14 to 1852:110
    rcel2012_1_401316_8 = (lambda v5890: (0 if (0 > v5890) else v5890))((lambda v5887: (v5887 if (v5887 < False) else False))((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelhjk_1_401316_0) - rcelcom_1_401316_2) - rcel_1_401316_4) - rceljoqr_1_401316_6)))

    # RCEL2012: Scellier 2012 hors COM -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1854:12 to 1854:109
    rcel2012_401316_9 = (rcel2012_1_401316_8 if (rcel2012_1_401316_8 > 0) else 0)

    # RCELFD_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1856:12 to 1857:121
    rcelfd_1_401316_10 = (lambda v5898: (0 if (0 > v5898) else v5898))((lambda v5895: (v5895 if (v5895 < False) else False))(((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelhjk_1_401316_0) - rcelcom_1_401316_2) - rcel_1_401316_4) - rceljoqr_1_401316_6) - rcel2012_1_401316_8)))

    # RCELFD: Scellier 2013 polynesie -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1859:10 to 1859:100
    rcelfd_401316_11 = (rcelfd_1_401316_10 if (rcelfd_1_401316_10 > 0) else 0)

    # RCELFABC_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1861:14 to 1862:130
    rcelfabc_1_401316_12 = (lambda v5906: (0 if (0 > v5906) else v5906))((lambda v5903: (v5903 if (v5903 < False) else False))((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelhjk_1_401316_0) - rcelcom_1_401316_2) - rcel_1_401316_4) - rceljoqr_1_401316_6) - rcel2012_1_401316_8) - rcelfd_1_401316_10)))

    # RCELFABC: Scellier 2013  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1864:12 to 1864:108
    rcelfabc_401316_13 = (rcelfabc_1_401316_12 if (rcelfabc_1_401316_12 > 0) else 0)

    # RCELSOM6: Invest. Scellier - somme des reductions - retenu
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1870:12 to 1870:78
    rcelsom6_401316_16 = (rcelhjk_401316_1 + (rcelcom_401316_3 + (rcel_401316_5 + (rceljoqr_401316_7 + (rcel2012_401316_9 + (rcelfd_401316_11 + rcelfabc_401316_13))))))

    # RCELZA_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1875:12 to 1876:74
    rcelza_1_401318_0 = (lambda v5914: (0 if (0 > v5914) else v5914))((lambda v5911: (v5911 if (v5911 < 0) else 0))(((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16)))

    # RCELZA: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1878:10 to 1878:101
    rcelza_401318_1 = (rcelza_1_401318_0 if (rcelza_1_401318_0 > 0) else 0)

    # RCELZB_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1880:12 to 1881:83
    rcelzb_1_401318_2 = (lambda v5922: (0 if (0 > v5922) else v5922))((lambda v5919: (v5919 if (v5919 < 0) else 0))((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0)))

    # RCELZB: Scellier 2016  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1883:10 to 1883:101
    rcelzb_401318_3 = (rcelzb_1_401318_2 if (rcelzb_1_401318_2 > 0) else 0)

    # RCELZC_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1885:12 to 1886:92
    rcelzc_1_401318_4 = (lambda v5930: (0 if (0 > v5930) else v5930))((lambda v5927: (v5927 if (v5927 < 0) else 0))(((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2)))

    # RCELZC: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1888:10 to 1888:101
    rcelzc_401318_5 = (rcelzc_1_401318_4 if (rcelzc_1_401318_4 > 0) else 0)

    # RCELZD_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1890:12 to 1891:101
    rcelzd_1_401318_6 = (lambda v5938: (0 if (0 > v5938) else v5938))((lambda v5935: (v5935 if (v5935 < 0) else 0))((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2) - rcelzc_1_401318_4)))

    # RCELZD: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1893:10 to 1893:101
    rcelzd_401318_7 = (rcelzd_1_401318_6 if (rcelzd_1_401318_6 > 0) else 0)

    # RCELZE_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1895:12 to 1896:110
    rcelze_1_401318_8 = (lambda v5946: (0 if (0 > v5946) else v5946))((lambda v5943: (v5943 if (v5943 < 0) else 0))(((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2) - rcelzc_1_401318_4) - rcelzd_1_401318_6)))

    # RCELZE: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1898:10 to 1898:101
    rcelze_401318_9 = (rcelze_1_401318_8 if (rcelze_1_401318_8 > 0) else 0)

    # RCELZF_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1900:12 to 1901:119
    rcelzf_1_401318_10 = (lambda v5954: (0 if (0 > v5954) else v5954))((lambda v5951: (v5951 if (v5951 < 0) else 0))((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2) - rcelzc_1_401318_4) - rcelzd_1_401318_6) - rcelze_1_401318_8)))

    # RCELZF: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1903:10 to 1903:101
    rcelzf_401318_11 = (rcelzf_1_401318_10 if (rcelzf_1_401318_10 > 0) else 0)

    # RCELZG_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1905:12 to 1906:128
    rcelzg_1_401318_12 = (lambda v5962: (0 if (0 > v5962) else v5962))((lambda v5959: (v5959 if (v5959 < 0) else 0))(((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2) - rcelzc_1_401318_4) - rcelzd_1_401318_6) - rcelze_1_401318_8) - rcelzf_1_401318_10)))

    # RCELZG: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1908:10 to 1908:101
    rcelzg_401318_13 = (rcelzg_1_401318_12 if (rcelzg_1_401318_12 > 0) else 0)

    # RCELZH_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1910:12 to 1911:137
    rcelzh_1_401318_14 = (lambda v5970: (0 if (0 > v5970) else v5970))((lambda v5967: (v5967 if (v5967 < 0) else 0))((((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2) - rcelzc_1_401318_4) - rcelzd_1_401318_6) - rcelze_1_401318_8) - rcelzf_1_401318_10) - rcelzg_1_401318_12)))

    # RCELZH: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1913:10 to 1913:101
    rcelzh_401318_15 = (rcelzh_1_401318_14 if (rcelzh_1_401318_14 > 0) else 0)

    # RCELZI_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1915:12 to 1916:146
    rcelzi_1_401318_16 = (lambda v5978: (0 if (0 > v5978) else v5978))((lambda v5975: (v5975 if (v5975 < 0) else 0))(((((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2) - rcelzc_1_401318_4) - rcelzd_1_401318_6) - rcelze_1_401318_8) - rcelzf_1_401318_10) - rcelzg_1_401318_12) - rcelzh_1_401318_14)))

    # RCELZI: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1918:10 to 1918:101
    rcelzi_401318_17 = (rcelzi_1_401318_16 if (rcelzi_1_401318_16 > 0) else 0)

    # RCELZJ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1920:12 to 1921:155
    rcelzj_1_401318_18 = (lambda v5986: (0 if (0 > v5986) else v5986))((lambda v5983: (v5983 if (v5983 < 0) else 0))((((((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2) - rcelzc_1_401318_4) - rcelzd_1_401318_6) - rcelze_1_401318_8) - rcelzf_1_401318_10) - rcelzg_1_401318_12) - rcelzh_1_401318_14) - rcelzi_1_401318_16)))

    # RCELZJ: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1923:10 to 1923:101
    rcelzj_401318_19 = (rcelzj_1_401318_18 if (rcelzj_1_401318_18 > 0) else 0)

    # RCELZK_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1925:12 to 1926:164
    rcelzk_1_401318_20 = (lambda v5994: (0 if (0 > v5994) else v5994))((lambda v5991: (v5991 if (v5991 < 0) else 0))(((((((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2) - rcelzc_1_401318_4) - rcelzd_1_401318_6) - rcelze_1_401318_8) - rcelzf_1_401318_10) - rcelzg_1_401318_12) - rcelzh_1_401318_14) - rcelzi_1_401318_16) - rcelzj_1_401318_18)))

    # RCELZK: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1928:10 to 1928:101
    rcelzk_401318_21 = (rcelzk_1_401318_20 if (rcelzk_1_401318_20 > 0) else 0)

    # RCELZL_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1930:12 to 1932:37
    rcelzl_1_401318_22 = (lambda v6002: (0 if (0 > v6002) else v6002))((lambda v5999: (v5999 if (v5999 < 0) else 0))((((((((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rcelsom3_401312_25) - rcelsom6_401316_16) - rcelza_1_401318_0) - rcelzb_1_401318_2) - rcelzc_1_401318_4) - rcelzd_1_401318_6) - rcelze_1_401318_8) - rcelzf_1_401318_10) - rcelzg_1_401318_12) - rcelzh_1_401318_14) - rcelzi_1_401318_16) - rcelzj_1_401318_18) - rcelzk_1_401318_20)))

    # RCELZL: Prorogation 2016 Scellier 2011  -Reduc apres imputation sur droits dus
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1934:10 to 1934:101
    rcelzl_401318_23 = (rcelzl_1_401318_22 if (rcelzl_1_401318_22 > 0) else 0)

    # RCELSOM7: Invest. Scellier - somme des reductions - retenu
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1940:12 to 1940:117
    rcelsom7_401318_26 = (rcelza_401318_1 + (rcelzb_401318_3 + (rcelzc_401318_5 + (rcelzd_401318_7 + (rcelze_401318_9 + (rcelzf_401318_11 + (rcelzg_401318_13 + (rcelzh_401318_15 + (rcelzi_401318_17 + (rcelzj_401318_19 + (rcelzk_401318_21 + rcelzl_401318_23)))))))))))

    # RCELTOT: Total des reductions Scellier : cf ordre d imputation et calcul de RED
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 1942:11 to 1942:85
    rceltot_401318_27 = (rcelsom3_401312_25 + (rcelsom6_401316_16 + rcelsom7_401318_26))

    # RMEUBLE_1: pour alleger prog - Inv. loc. meublees non pro - report 2009 - reduc.
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2854:13 to 2854:128
    rmeuble_1_401679_0 = (lambda v6778: (0 if (0 > v6778) else v6778))((lambda v6775: (v6775 if (v6775 < 0) else 0))((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27)))

    # RMEUBLE: Investissements locations meublees non pro - report 2009 - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2856:11 to 2856:90
    rmeuble_401679_1 = (rmeuble_1_401679_0 if (rmeuble_1_401679_0 > 0) else 0)

    # RPROREP_1: Pour alleger prog - Inv. loc. meublees non pro - report (7IR) - reduc
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2860:13 to 2861:27
    rprorep_1_401679_3 = (lambda v6786: (0 if (0 > v6786) else v6786))((lambda v6783: (v6783 if (v6783 < 0) else 0))(((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rmeuble_1_401679_0)))

    # RPROREP: Invest. loc. meublees non pro - report reduction (7IR) - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2863:11 to 2863:90
    rprorep_401679_4 = (rprorep_1_401679_3 if (rprorep_1_401679_3 > 0) else 0)

    # RREPNPRO_1: pour alleger prog - Inv.. loc. meublees non pro - report (7IQ) - reduc
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2867:14 to 2868:32
    rrepnpro_1_401679_6 = (lambda v6794: (0 if (0 > v6794) else v6794))((lambda v6791: (v6791 if (v6791 < 0) else 0))((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rmeuble_1_401679_0) - rprorep_1_401679_3)))

    # RREPNPRO: Invest. loc. meublees non pro - report reduction (7IQ) - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2870:12 to 2870:94
    rrepnpro_401679_7 = (rrepnpro_1_401679_6 if (rrepnpro_1_401679_6 > 0) else 0)

    # RREPMEU_1: Pour alleger prog - Inv. loc. meublees non pro - report (7IP) - reduc
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2874:13 to 2875:48
    rrepmeu_1_401679_9 = (lambda v6802: (0 if (0 > v6802) else v6802))((lambda v6799: (v6799 if (v6799 < 0) else 0))(((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rmeuble_1_401679_0) - rprorep_1_401679_3) - rrepnpro_1_401679_6)))

    # RREPMEU: Invest. loc. meublees non pro - report reduction (7IP) - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2877:11 to 2877:90
    rrepmeu_401679_10 = (rrepmeu_1_401679_9 if (rrepmeu_1_401679_9 > 0) else 0)

    # RILMNP2: Invest. loc. meublees non pro 2009 2010 : report - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2885:11 to 2885:49
    rilmnp2_401679_14 = (rmeuble_401679_1 + (rprorep_401679_4 + (rrepnpro_401679_7 + rrepmeu_401679_10)))

    # RCODIM_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3210:12 to 3211:59
    rcodim_1_401770_0 = (lambda v7068: (0 if (0 > v7068) else v7068))((lambda v7065: (v7065 if (v7065 < 0) else 0))(((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rilmnp2_401679_14)))

    # RCODIM: Invest. loc. meublees non pro - report (7IM) - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3213:10 to 3213:84
    rcodim_401770_1 = (rcodim_1_401770_0 if (rcodim_1_401770_0 > 0) else 0)

    # RCODIN_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3221:12 to 3222:68
    rcodin_1_401770_5 = (lambda v7076: (0 if (0 > v7076) else v7076))((lambda v7073: (v7073 if (v7073 < 0) else 0))((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rilmnp2_401679_14) - rcodim_1_401770_0)))

    # RCODIJ_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3228:12 to 3229:77
    rcodij_1_401770_8 = (lambda v7082: (0 if (0 > v7082) else v7082))((lambda v7079: (v7079 if (v7079 < 0) else 0))(((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rilmnp2_401679_14) - rcodim_1_401770_0) - rcodin_1_401770_5)))

    # RRESINEUV: Investissements locations meublees dans residence neuve - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3236:13 to 3236:28
    rresineuv_401770_12 = (rcodin_1_401770_5 + rcodij_1_401770_8)

    # RCODID_1: reduc non plaf. art 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3239:12 to 3240:86
    rcodid_1_401770_13 = (lambda v7088: (0 if (0 > v7088) else v7088))((lambda v7085: (v7085 if (v7085 < 0) else 0))((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rilmnp2_401679_14) - rcodim_1_401770_0) - rcodin_1_401770_5) - rcodij_1_401770_8)))

    # RCODJT_1: allege prog -Inv. loc. meub non pro - report (7JT) - reduc
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3250:12 to 3251:95
    rcodjt_1_401770_18 = (lambda v7094: (0 if (0 > v7094) else v7094))((lambda v7091: (v7091 if (v7091 < 0) else 0))(((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rilmnp2_401679_14) - rcodim_1_401770_0) - rcodin_1_401770_5) - rcodij_1_401770_8) - rcodid_1_401770_13)))

    # RCODJT: Invest. loc. meublees non pro - report (7JT) - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3253:10 to 3253:84
    rcodjt_401770_19 = (rcodjt_1_401770_18 if (rcodjt_1_401770_18 > 0) else 0)

    # RCODOU_1: pour alleger prog - Inv. loc meublees dans residence neuve - reduc
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3260:12 to 3261:104
    rcodou_1_401770_23 = (lambda v7102: (0 if (0 > v7102) else v7102))((lambda v7099: (v7099 if (v7099 < 0) else 0))((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rilmnp2_401679_14) - rcodim_1_401770_0) - rcodin_1_401770_5) - rcodij_1_401770_8) - rcodid_1_401770_13) - rcodjt_1_401770_18)))

    # RCODOU: Invest. loc. meublees non pro - report (7OU) - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3263:10 to 3263:87
    rcodou_401770_24 = (rcodou_1_401770_23 if (rcodou_1_401770_23 > 0) else 0)

    # RCODOV_1: pour alleger prog - Inv. loc meublees non pro - reduc
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3267:12 to 3268:113
    rcodov_1_401770_26 = (lambda v7110: (0 if (0 > v7110) else v7110))((lambda v7107: (v7107 if (v7107 < 0) else 0))(((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rilmnp2_401679_14) - rcodim_1_401770_0) - rcodin_1_401770_5) - rcodij_1_401770_8) - rcodid_1_401770_13) - rcodjt_1_401770_18) - rcodou_1_401770_23)))

    # RCODOV: Invest. loc. meublees non pro - report (7OV) - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3270:10 to 3270:87
    rcodov_401770_27 = (rcodov_1_401770_26 if (rcodov_1_401770_26 > 0) else 0)

    # RCODOW_1: pour alleger prog - Inv. loc meublees non pro - reduc
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3274:12 to 3275:122
    rcodow_1_401770_29 = (lambda v7118: (0 if (0 > v7118) else v7118))((lambda v7115: (v7115 if (v7115 < 0) else 0))((((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rilmnp2_401679_14) - rcodim_1_401770_0) - rcodin_1_401770_5) - rcodij_1_401770_8) - rcodid_1_401770_13) - rcodjt_1_401770_18) - rcodou_1_401770_23) - rcodov_1_401770_26)))

    # RCODOW: Invest. loc. meublees non pro - report (7OW) - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3277:10 to 3277:86
    rcodow_401770_30 = (rcodow_1_401770_29 if (rcodow_1_401770_29 > 0) else 0)

    # RCODOX_1: pour alleger prog - Inv. loc meublees non pro - reduc
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3281:12 to 3282:131
    rcodox_1_401770_32 = (lambda v7126: (0 if (0 > v7126) else v7126))((lambda v7123: (v7123 if (v7123 < 0) else 0))(((((((((((((idom11_511330_0 - dec11_401140_0) - reducavtcel_1_401310_0) - rceltot_401318_27) - rilmnp2_401679_14) - rcodim_1_401770_0) - rcodin_1_401770_5) - rcodij_1_401770_8) - rcodid_1_401770_13) - rcodjt_1_401770_18) - rcodou_1_401770_23) - rcodov_1_401770_26) - rcodow_1_401770_29)))

    # RCODOX: Invest. loc. meublees non pro - report (7OX) - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3284:10 to 3284:86
    rcodox_401770_33 = (rcodox_1_401770_32 if (rcodox_1_401770_32 > 0) else 0)

    # RILMNP4: Invest. loc. meublees non pro 2017 - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3292:11 to 3292:91
    rilmnp4_401770_37 = (rcodim_401770_1 + (rresineuv_401770_12 + (rcodid_1_401770_13 + (rcodjt_401770_19 + (rcodou_401770_24 + (rcodov_401770_27 + (rcodow_401770_30 + rcodox_401770_33)))))))

    # RLOCNPRO: Reductions impot locations meublees non pro
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3294:12 to 3294:49
    rlocnpro_401770_38 = (rilmnp2_401679_14 + rilmnp4_401770_37)

    # RLOCNPRO_1: allege prog -RLOCNPRO si 1731bis
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 3296:14 to 3296:51
    rlocnpro_1_401770_39 = (rilmnp2_401679_14 + rilmnp4_401770_37)

    # RPATNAT2: Report 2011 Reduction impute depense protection patrimoine naturel
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2174:12 to 2176:66
    rpatnat2_401370_0 = (lambda v6188: (0 if (0 > v6188) else v6188))((lambda v6185: (v6185 if (v6185 < False) else False))(((((((((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentot_401440_20) - rfor_1_401960_0) - rtourrep_1_401340_0) - rtourepa_1_401340_2) - rrrehap_1_402161_0) - rrestimo_3_401848_2) - rceltot_401318_27) - rlocnpro_1_401770_39)))

    # RPATNAT3: Report 2012 Reduction impute depense protection patrimoine naturel
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2178:12 to 2180:74
    rpatnat3_401370_1 = (lambda v6192: (0 if (0 > v6192) else v6192))((lambda v6189: (v6189 if (v6189 < False) else False))((((((((((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentot_401440_20) - rfor_1_401960_0) - rtourrep_1_401340_0) - rtourepa_1_401340_2) - rrrehap_1_402161_0) - rrestimo_3_401848_2) - rceltot_401318_27) - rlocnpro_1_401770_39) - rpatnat2_401370_0)))

    # RPATNAT: Reduction retenue depense protection du patrimoine naturel
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2183:11 to 2185:54
    rpatnat_401370_2 = (lambda v6196: (0 if (0 > v6196) else v6196))((lambda v6193: (v6193 if (v6193 < False) else False))(((((((((((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_1_401260_0) - rretu_1_401910_0) - rdons_1_401880_0) - crdie_1_401887_0) - rduflotot_1_401280_86) - rpineltot_1_401280_94) - rnouv_1_401430_27) - rpentot_401440_20) - rfor_1_401960_0) - rtourrep_1_401340_0) - rtourepa_1_401340_2) - rrrehap_1_402161_0) - rrestimo_3_401848_2) - rceltot_401318_27) - rlocnpro_1_401770_39) - rpatnat2_401370_0) - rpatnat3_401370_1)))

    # RPATNATOT: Colbert : RPATNAT + RPATNAT1
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 2187:13 to 2187:42
    rpatnatot_401370_3 = (rpatnat2_401370_0 + (rpatnat3_401370_1 + rpatnat_401370_2))

    # RRISUP: Reductions impot avant investissement O.M
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 6714:10 to 6715:96
    rrisup_402110_0 = (((((((((((((((((rri1_401390_0 - rlogdom_402060_0) - rcomp_401260_1) - rretu_401910_1) - rdons_401880_1) - crdie_401887_1) - rlocnpro_401770_38) - rduflotot_401280_85) - rpineltot_401280_93) - rnouv_1_401430_27) - rpentot_401440_20) - rfor_401960_1) - rtourrep_1_401340_0) - rtourepa_1_401340_2) - rrehab_402161_1) - rrestimo1_401848_3) - rceltot_401318_27) - rpatnatot_401370_3)

    # RDOMSOC1: Investissements outre mer logement social - report 2009 - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 6718:12 to 6718:65
    rdomsoc1_402110_1 = (rrisup_402110_0 if (rrisup_402110_0 < False) else False)

    # RLOGSOC: Investissements outre mer logement social - reduction
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 6720:11 to 6720:83
    rlogsoc_402110_2 = (lambda v10531: (v10531 if (v10531 < False) else False))((rrisup_402110_0 if (rrisup_402110_0 > 0) else 0))

    # RCOLENT: Investissements outre mer entreprise - report 2009 - retenu
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 6755:11 to 6755:93
    rcolent_402120_0 = (lambda v10535: (v10535 if (v10535 < False) else False))((rrisup_402110_0 if (rrisup_402110_0 > 0) else 0))

    # RLOCENT: Investissements outre mer entreprise - retenu
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 6757:11 to 6757:103
    rlocent_402120_1 = (lambda v10539: (v10539 if (v10539 < False) else False))((rrisup_402110_0 if (rrisup_402110_0 > 0) else 0))

    # RED: Total des reductions d'impot
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 283:7 to 291:47
    red_401160_0 = (rcotfor_1_401940_3 + (rrepa_1_401410_5 + (rdifagri_1_401980_4 + (rpresse_1_401180_5 + (rforet_1_401185_4 + (rfipdom_1_401190_4 + (rfipc_1_401200_4 + (rcine_401220_8 + (rrestimo_401845_4 + (rsocrepr_401830_5 + (rrprescomp_401930_6 + (rhebe_401400_5 + (rsurv_401210_5 + (rinno_401920_4 + (rsoufip_401230_5 + (rrirenov_401240_5 + (rlogdom_402060_0 + (rcomp_401260_1 + (rretu_401910_1 + (rdons_401880_1 + (crdie_401887_1 + (rduflotot_401280_85 + (rpineltot_401280_93 + (rnouv_1_401430_27 + (rpentot_401440_20 + (rfor_401960_1 + (rtourrep_1_401340_0 + (rtourepa_1_401340_2 + (rrehab_402161_1 + (rrestimo1_401848_3 + (rceltot_401318_27 + (rlocnpro_401770_38 + (rpatnat2_401370_0 + (rpatnat3_401370_1 + (rpatnat_401370_2 + (rdomsoc1_402110_1 + (rlogsoc_402110_2 + (rcolent_402120_0 + rlocent_402120_1))))))))))))))))))))))))))))))))))))))

    # IAD11: impot apres decote et apres reduction  d impot
    # Defined in file ir-calcul/sources2017m_6_10/chap-4.m, from 217:9 to 220:51
    iad11_401100_0 = (lambda v4325: (v4325 if (v4325 > 0) else 0))(((idom11_511330_0 - dec11_401140_0) - red_401160_0))

    # IRETS1: Retenue a la source IPSOUR limite IR avant CI
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 259:10 to 259:87
    irets1_301133_0 = (lambda v15648: (v15648 if (v15648 > 0) else 0))((False if (False < iad11_401100_0) else iad11_401100_0))

    # CICHR: Impot Hauts Revenus credit impot
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 394:9 to 394:101
    cichr_301242_0 = (lambda v15747: (v15747 if (v15747 > 0) else 0))((lambda v15746: (lambda v15745: (v15745 if (v15745 < v15746) else v15746))((0 if (0 < chrreeltot_80000_12) else chrreeltot_80000_12)))((iad11_401100_0 + chrreeltot_80000_12)))

    # IRETS2: Retenue a la source IPAE limite 8PA ou (IR avant CI - IRETS1)
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 261:10 to 262:33
    irets2_301133_1 = (lambda v15656: (v15656 if (v15656 > 0) else 0))((lambda v15655: (False if (False < v15655) else v15655))((lambda v15652: (v15652 if (v15652 > 0) else 0))((iad11_401100_0 - irets1_301133_0))))

    # IHAUTREVT: Impot Hauts Revenus total
    # Defined in file ir-calcul/sources2017m_6_10/chap-thr.m, from 76:13 to 76:36
    ihautrevt_80020_0 = (lambda v22239: (v22239 if (v22239 > 0) else 0))((chrreeltot_80000_12 - cichr_301242_0))

    # IRETS: Retenue a la source
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 264:9 to 264:24
    irets_301133_2 = (irets1_301133_0 + irets2_301133_1)

    # ICREREVET: imputation limitee a l impot propor. pour le credit revenus etrangers
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 98:13 to 98:78
    icrerevet_301040_0 = (lambda v15554: (v15554 if (v15554 > 0) else 0))((lambda v15553: (False if (False < v15553) else v15553))((iad11_401100_0 - irets_301133_2)))

    # CIGLO: Credit d'impot sur gain de levee d option retenu
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 86:9 to 86:96
    ciglo_301032_2 = (lambda v15536: (v15536 if (v15536 > 0) else 0))((lambda v15535: (0 if (0 < v15535) else v15535))((iad11_401100_0 + ((- irets_301133_2) - icrerevet_301040_0))))

    # CICULTUR: Credit d'impot investissement biens cultures retenu
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 286:12 to 286:152
    cicultur_301152_0 = (lambda v15674: (v15674 if (v15674 > 0) else 0))((lambda v15673: (lambda v15672: (v15672 if (v15672 < v15673) else v15673))((0 if (0 < iad11_401100_0) else iad11_401100_0)))((iad11_401100_0 + (((- irets_301133_2) - icrerevet_301040_0) - ciglo_301032_2))))

    # INE: Imputations non restituables
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 107:7 to 108:41
    ine_301050_0 = (irets_301133_2 + (icrerevet_301040_0 + (ciglo_301032_2 + cicultur_301152_0)))

    # IAN: Impot apres imputations non restituables
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 110:7 to 126:11
    ian_301050_1 = (lambda v15590: (v15590 if (v15590 > 0) else 0))((iad11_401100_0 + (((((- irets_301133_2) - icrerevet_301040_0) - ciglo_301032_2) - cicultur_301152_0) + ((lambda v15564: (v15564 if (v15564 < 0) else 0))((lambda v15562: (v15562 if (v15562 > 0) else 0))((ine_301050_0 - iad11_401100_0))) + ((lambda v15572: (v15572 if (v15572 < 0) else 0))((lambda v15570: (v15570 if (v15570 > 0) else 0))(((ine_301050_0 - iad11_401100_0) - (lambda v15568: (v15568 if (v15568 < 0) else 0))((lambda v15566: (v15566 if (v15566 > 0) else 0))((ine_301050_0 - iad11_401100_0)))))) + (lambda v15588: (v15588 if (v15588 < 0) else 0))((lambda v15586: (v15586 if (v15586 > 0) else 0))(((ine_301050_0 - iad11_401100_0) + ((- (lambda v15580: (v15580 if (v15580 < 0) else 0))((lambda v15578: (v15578 if (v15578 > 0) else 0))(((ine_301050_0 - iad11_401100_0) - (lambda v15576: (v15576 if (v15576 < 0) else 0))((lambda v15574: (v15574 if (v15574 > 0) else 0))((ine_301050_0 - iad11_401100_0))))))) - (lambda v15584: (v15584 if (v15584 < 0) else 0))((lambda v15582: (v15582 if (v15582 > 0) else 0))((ine_301050_0 - iad11_401100_0))))))))))))

    # IAR: Impot apres imputation restituables
    # Defined in file ir-calcul/sources2017m_6_10/chap-3.m, from 53:7 to 53:76
    iar_301010_0 = ((lambda v15498: (v15498 if (v15498 < 0) else 0))((ian_301050_1 - cisynd_301257_4)) + (lambda v15500: (v15500 if (v15500 > 0) else 0))((ian_301050_1 - cisynd_301257_4)))

    # TAXANEG: taxe + INR + majo
    # Defined in file ir-calcul/sources2017m_6_10/res-ser2.m, from 564:11 to 564:111
    taxaneg_221250_0 = (lambda v24213: (v24213 if (v24213 < 0) else 0))(((- (lambda v24209: (v24209 if (v24209 < 0) else 0))((lambda v24207: (v24207 if (v24207 > 0) else 0))((ine_301050_0 - iad11_401100_0)))) + (iar_301010_0 if (iar_301010_0 < 0) else 0)))

    # PCAPNEG: taxe + INR + majo
    # Defined in file ir-calcul/sources2017m_6_10/res-ser2.m, from 574:12 to 574:118
    pcapneg_221260_0 = (lambda v24229: (v24229 if (v24229 < 0) else 0))(((- (lambda v24225: (v24225 if (v24225 < 0) else 0))((lambda v24223: (v24223 if (v24223 > 0) else 0))((ine_301050_0 - iad11_401100_0)))) + (taxaneg_221250_0 if (taxaneg_221250_0 < 0) else 0)))

    # LOYELEVNEG: taxe + INR + majo
    # Defined in file ir-calcul/sources2017m_6_10/res-ser2.m, from 584:15 to 584:139
    loyelevneg_221270_0 = (lambda v24245: (v24245 if (v24245 < 0) else 0))(((- (lambda v24241: (v24241 if (v24241 < False) else False))((lambda v24239: (v24239 if (v24239 > 0) else 0))((ine_301050_0 - iad11_401100_0)))) + (pcapneg_221260_0 if (pcapneg_221260_0 < 0) else 0)))

    # IRNETTER: IRNET avant bidouille du 8ZI
    # Defined in file ir-calcul/sources2017m_6_10/res-ser2.m, from 537:12 to 545:25
    irnetter_221220_0 = (lambda v24197: (v24197 if (v24197 > 0) else 0))((iar_301010_0 + (((- (lambda v24159: (v24159 if (v24159 < 0) else 0))((lambda v24157: (v24157 if (v24157 > 0) else 0))((ine_301050_0 - iad11_401100_0)))) - (lambda v24167: (v24167 if (v24167 > 0) else 0))(((- (lambda v24163: (v24163 if (v24163 < 0) else 0))((lambda v24161: (v24161 if (v24161 > 0) else 0))((ine_301050_0 - iad11_401100_0)))) + (iar_301010_0 if (iar_301010_0 < 0) else 0)))) + (((- (lambda v24171: (v24171 if (v24171 < 0) else 0))((lambda v24169: (v24169 if (v24169 > 0) else 0))((ine_301050_0 - iad11_401100_0)))) - (lambda v24179: (v24179 if (v24179 > 0) else 0))(((- (lambda v24175: (v24175 if (v24175 < 0) else 0))((lambda v24173: (v24173 if (v24173 > 0) else 0))((ine_301050_0 - iad11_401100_0)))) + (taxaneg_221250_0 if (taxaneg_221250_0 < 0) else 0)))) + (((- (lambda v24183: (v24183 if (v24183 < False) else False))((lambda v24181: (v24181 if (v24181 > 0) else 0))((ine_301050_0 - iad11_401100_0)))) - (lambda v24191: (v24191 if (v24191 > 0) else 0))(((- (lambda v24187: (v24187 if (v24187 < False) else False))((lambda v24185: (v24185 if (v24185 > 0) else 0))((ine_301050_0 - iad11_401100_0)))) + (pcapneg_221260_0 if (pcapneg_221260_0 < 0) else 0)))) + (ihautrevt_80020_0 - (lambda v24195: (v24195 if (v24195 > 0) else 0))((ihautrevt_80020_0 + (loyelevneg_221270_0 if (loyelevneg_221270_0 < 0) else 0)))))))))

    # IRNETBIS: IRNET avant bidouille du 8ZI
    # Defined in file ir-calcul/sources2017m_6_10/res-ser2.m, from 547:12 to 549:54
    irnetbis_221220_1 = (irnetter_221220_0 if (irnetter_221220_0 > 0) else 0)

    # IRNET: Total de votre imposition (si positif)
    # Defined in file ir-calcul/sources2017m_6_10/res-ser2.m, from 554:10 to 554:74
    irnet_221230_0 = (irnetbis_221220_1 * (1 if ((iad11_401100_0 - (lambda v24204: (ine_301050_0 if (ine_301050_0 < v24204) else v24204))((iad11_401100_0 if (iad11_401100_0 > 0) else 0))) >= 0) else 0))

    return irnet_221230_0


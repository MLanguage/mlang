let internal_m = "internal DGFiP M"

let variable_domains_declaration =
  {|
variable saisie contexte
: attribut
  classe,
  priorite,
  categorie_TL,
  modcat,
  primrest;

variable saisie famille
: attribut
  classe,
  priorite,
  categorie_TL,
  nat_code,
  modcat,
  primrest;

variable saisie revenu
: attribut
  classe,
  priorite,
  categorie_TL,
  cotsoc,
  ind_abat,
  acompte,
  avfisc,
  rapcat,
  sanction,
  nat_code,
  modcat,
  primrest;

variable saisie revenu corrective
: attribut
  classe,
  priorite,
  categorie_TL,
  cotsoc,
  ind_abat,
  acompte,
  avfisc,
  rapcat,
  sanction,
  nat_code,
  modcat,
  primrest;

variable saisie variation
: attribut
  classe,
  primrest;

variable saisie penalite
: attribut primrest;

variable calculee
: attribut primrest;
|}

let rule_domains_declaration =
  {|
domaine regle irisf;

domaine regle
  primitive corrective,
  isf corrective,
  taux corrective,
  modul corrective,
  irisf corrective
: specialise irisf
: par_defaut;

domaine regle isf
: specialise irisf
: calculable;

domaine regle primitive
: specialise primitive corrective
: calculable;

domaine regle corrective
: specialise primitive corrective
: calculable;

domaine regle taux
: calculable;

domaine regle modul;

domaine regle corrective base_anterieure_cor
: calculable;

domaine regle corrective base_anterieure
: calculable;

domaine regle corrective base_premier
: calculable;

domaine regle corrective base_HR
: calculable;

domaine regle corrective base_INITIAL
: calculable;

domaine regle corrective base_1728
: calculable;

domaine regle corrective base_TLNUNV
: calculable;

domaine regle corrective base_INR
: calculable;

domaine regle corrective base_MAJO
: calculable;

domaine regle corrective base_ABAT98
: calculable;

domaine regle corrective base_ABAT99
: calculable;

domaine regle corrective base_tl
: calculable;

domaine regle corrective base_tl_init
: calculable;

domaine regle corrective base_tl_rect
: calculable;

domaine regle corrective base_stratemajo
: calculable;

domaine regle corrective base_inr_ref
: calculable;

domaine regle corrective base_inr_ntl
: calculable;

domaine regle corrective base_inr_tl
: calculable;

domaine regle corrective base_inr_tl22
: calculable;

domaine regle corrective base_inr_ntl22
: calculable;

domaine regle corrective base_inr_tl24
: calculable;

domaine regle corrective base_inr_ntl24
: calculable;

domaine regle corrective base_inr_cimr99
: calculable;

domaine regle corrective base_inr_cimr07
: calculable;

domaine regle corrective base_inr_tlcimr07
: calculable;

domaine regle corrective base_inr_cimr24
: calculable;

domaine regle corrective base_inr_tlcimr24
: calculable;

domaine regle corrective base_inr_intertl
: calculable;

domaine regle corrective base_inr_inter22
: calculable;

domaine regle corrective base_inr_r9901
: calculable;

domaine regle corrective base_primitive_penalisee
: calculable;
|}

let verif_domains_declaration =
  {|
domaine verif primitive corrective, isf corrective, non_auto_cc
: autorise
    calculee *,
    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
    saisie variation
: par_defaut;

domaine verif primitive
: autorise
    calculee *,
    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
    saisie variation
: specialise primitive corrective
: verifiable;

domaine verif isf
: autorise
    calculee *,
    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
    saisie variation
: verifiable;

domaine verif corrective
: autorise
    calculee *,
    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
    saisie variation
: specialise primitive corrective
: verifiable;

domaine verif corrective horizontale
: autorise
    calculee *,
    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
    saisie variation, saisie penalite
: verifiable;
|}

let declarations =
  Format.sprintf "%s%s%s" variable_domains_declaration rule_domains_declaration
    verif_domains_declaration

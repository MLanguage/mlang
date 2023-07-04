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
|}

let verif_domains_declaration =
  {|
domaine verif primitive corrective, isf corrective
#: autorise
#    calculee *,
#    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
#    saisie variation
#: auto_cc contexte, famille, revenu, revenu corrective, variation
: par_defaut;

domaine verif primitive
#: autorise
#    calculee *,
#    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
#    saisie variation
: specialise primitive corrective;

domaine verif isf
#: autorise
#    calculee *,
#    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
#    saisie variation
;

domaine verif corrective
#: autorise
#    calculee *,
#    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
#    saisie variation
: specialise primitive corrective;

domaine verif corrective horizontale
#: autorise
#    calculee *,
#    saisie contexte, saisie famille, saisie revenu, saisie revenu corrective,
#    saisie variation, saisie penalite
#: auto_cc penalite
;
|}

let declarations =
  Format.sprintf "%s%s%s" variable_domains_declaration rule_domains_declaration
    verif_domains_declaration

let string_to_rule_domain_id : string -> Mast.DomainId.t = function
  | "primitif" -> Mast.DomainId.from_list [ "primitive" ]
  | "corrective" -> Mast.DomainId.from_list [ "corrective" ]
  | "isf" -> Mast.DomainId.from_list [ "isf" ]
  | "taux" -> Mast.DomainId.from_list [ "taux" ]
  | "irisf" -> Mast.DomainId.from_list [ "irisf" ]
  | "base_HR" -> Mast.DomainId.from_list [ "corrective"; "base_HR" ]
  | "base_tl" -> Mast.DomainId.from_list [ "corrective"; "base_tl" ]
  | "base_tl_init" -> Mast.DomainId.from_list [ "corrective"; "base_tl_init" ]
  | "base_tl_rect" -> Mast.DomainId.from_list [ "corrective"; "base_tl_rect" ]
  | "base_INITIAL" -> Mast.DomainId.from_list [ "corrective"; "base_INITIAL" ]
  | "base_INR" -> Mast.DomainId.from_list [ "corrective"; "base_INR" ]
  | "base_inr_ref" -> Mast.DomainId.from_list [ "corrective"; "base_inr_ref" ]
  | "base_inr_tl" -> Mast.DomainId.from_list [ "corrective"; "base_inr_tl" ]
  | "base_inr_tl22" -> Mast.DomainId.from_list [ "corrective"; "base_inr_tl22" ]
  | "base_inr_tl24" -> Mast.DomainId.from_list [ "corrective"; "base_inr_tl24" ]
  | "base_inr_ntl" -> Mast.DomainId.from_list [ "corrective"; "base_inr_ntl" ]
  | "base_inr_ntl22" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_ntl22" ]
  | "base_inr_ntl24" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_ntl24" ]
  | "base_inr_inter22" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_inter22" ]
  | "base_inr_intertl" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_intertl" ]
  | "base_inr_r9901" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_r9901" ]
  | "base_inr_cimr07" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_cimr07" ]
  | "base_inr_cimr24" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_cimr24" ]
  | "base_inr_cimr99" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_cimr99" ]
  | "base_inr_tlcimr07" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_tlcimr07" ]
  | "base_inr_tlcimr24" ->
      Mast.DomainId.from_list [ "corrective"; "base_inr_tlcimr24" ]
  | "base_ABAT98" -> Mast.DomainId.from_list [ "corrective"; "base_ABAT98" ]
  | "base_ABAT99" -> Mast.DomainId.from_list [ "corrective"; "base_ABAT99" ]
  | "base_MAJO" -> Mast.DomainId.from_list [ "corrective"; "base_MAJO" ]
  | "base_premier" -> Mast.DomainId.from_list [ "corrective"; "base_premier" ]
  | "base_anterieure" ->
      Mast.DomainId.from_list [ "corrective"; "base_anterieure" ]
  | "base_anterieure_cor" ->
      Mast.DomainId.from_list [ "corrective"; "base_anterieure_cor" ]
  | "base_stratemajo" ->
      Mast.DomainId.from_list [ "corrective"; "base_stratemajo" ]
  | "non_auto_cc" -> Mast.DomainId.from_list []
  | "horizontale" -> Mast.DomainId.from_list [ "horizontale" ]
  | str -> Errors.raise_error (Format.sprintf "Unknown rule tag: %s" str)

let string_to_verif_domain_id : string -> Mast.DomainId.t = function
  | "primitif" | "primitive" -> Mast.DomainId.from_list [ "primitive" ]
  | "corrective" -> Mast.DomainId.from_list [ "corrective" ]
  | "isf" -> Mast.DomainId.from_list [ "isf" ]
  | "horizontale" -> Mast.DomainId.from_list [ "corrective"; "horizontale" ]
  | str -> Errors.raise_error (Format.sprintf "Unknown verif tag: %s" str)

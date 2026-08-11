(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2026 DGFiP - INRIA                                      *)
(*                                                                            *)
(* Ce programme est distribué sous la licence CeCILL-C: vous pouvez le        *)
(* redistribuer et/ou le modifier sous les contraintes de celle-ci.           *)
(*                                                                            *)
(* L'accessibilité au code source et les droits de copie, de modification et  *)
(* de redistribution qui découlent de ce contrat ont pour contrepartie de     *)
(* n'offrir aux utilisateurs qu'une garantie limitée et de ne faire peser sur *)
(* l'auteur du logiciel, le titulaire des droits patrimoniaux et les          *)
(* concédants successifs qu'une responsabilité restreinte.                    *)
(*                                                                            *)
(******************************************************************************)

open Utils
open Types

module Fr : LANG = struct
  module Config = struct
    let cannot_display_time_and_force_nondeterminism =
      "Impossible d'afficher le temps et de forcer un affichage non \
       déterministe."

    let dgfip_backend_without_dgfip_options =
      "L'utilisation du dorsal DGFiP est conditionnée par la définition des \
       options DGFiP, ici manquantes."

    let failed_parsing_of_dgfip_options = "Parsing des options dgfip échoué"

    let invalid_long_size ~long_size =
      Format.sprintf "La taille %S n'est pas une taille de 'long' valide"
        long_size

    let invalid_message_format ~message_format =
      Format.sprintf "Le format de message %S n'est pas un format valide"
        message_format

    let invalid_precision_option ~precision =
      Format.sprintf "Précision %S invalide" precision

    let invalid_roundops_option ~roundops =
      Format.sprintf "Stratégie d'arrondi %S invalide" roundops

    let invalid_term_in_dgfip_options = "Terme invalide dans les options dgfip"

    let no_m_files = "Veuillez renseigner au moins un fichier M"

    let option_mpp_function_required = "Option mpp_function requise"

    let uncaught_exception_while_reading_dgfip_options =
      "Exception non attrapée en lisant les options dgfip."

    let unspecified_roundops = "Stratégie d'arrondi non resneignée"
  end

  module Driver = struct
    let raised_in_the = function Errors.Validator -> "le validateur"

    let cmdline_arg_parsing_failed =
      "Lecture des arguments de la ligne de commande échoué."

    let blocking_error_raised_in ri =
      Format.sprintf "Erreur bloquante levée dans %s." (raised_in_the ri)

    let missing_output =
      "Une sortie doit être renseignée avec l'option --output."

    let term_eval_error =
      "Echec lors de l'évaluation d'un terme lors de la lecture des arguments \
       de la ligne de commande"

    let test_passed = "Test exécuté!"

    let uncaught_exception = "Exception non rattrapée"

    let unknown_backend = "Pas de dorsal spécifié (--backend)"
  end

  module Interpreter = struct
    let invalid_expression_value ~expr ~value =
      Format.sprintf "L'expression %S a été évaluée à %S: invalide." expr value

    let str_matched = function
      | `Undefined -> "'indéfini'"
      | `Value v -> Format.sprintf "la valeur %s" v
      | `Var v -> Format.sprintf "la variable %s" v

    let invalid_matching_in_switch ~case ~matched =
      Format.sprintf "Impossible d'associer le cas %s à %s." case
        (str_matched matched)

    let unimplemented ~func = Format.sprintf "Fonction %S non implémentée." func

    let wrong_arity ~func ~arity ~args =
      match arity with
      | None ->
          Format.sprintf
            "Erreur d'arité: la fonction %S est ne peut recevoir\n\
            \                         %i arguments."
            func args
      | Some arity ->
          Format.sprintf
            "Erreur d'arité: la fonction %S est d'arité %i, mais\n\
            \                         %i arguments lui sont donnés."
            func arity args
  end

  module Parser = struct
    let incomplete_attr_definition = "Définition incomplète d'un attribut."

    let incomplete_attr_list =
      "Liste d'attributs incomplète. Avez-vous oublié de terminer la liste \
       avec ':' ?"

    let missing_colon = "Symbole ':' manquant."

    let missing_endif = "Si-alors-sinon incomplet. Avez-vous oublié un 'finsi'?"

    let missing_value_after_equal = "Valeur manquante après le symbole '='."

    let syntax_error ~code = Format.sprintf "Erreur de syntaxe (code %i)." code

    let unexpected_symbol = "Symbole innattendu. Avez-vous oublié un ';'?"

    let unexpected_syntax_error = "Erreur de syntaxe innattendue."
  end

  module Test_interpreter = struct
    let all_good = "Aucun echec!"

    let all_not_good =
      let pp_file_errs fmt name nbErr =
        Format.fprintf fmt "@\n%s: %d erreur%s" name nbErr
          (if nbErr > 1 then "s" else "")
      in
      let pp_file_errs_map fmt m = StrMap.iter (pp_file_errs fmt) m in
      fun map : string -> Format.asprintf "Erreurs:%a" pp_file_errs_map map

    let error_in_test ~test = Format.sprintf "Erreur dans le test %s" test

    let invalid_remainder_direction ~dir =
      Format.sprintf "Sens du rappel: %S devrait être parmi R, C, M or P" dir

    let invalid_test_file = "Fichier de test incorrect"

    let ko_difference ~name ~expected ~evaluated =
      Format.sprintf "KO | %s attendue : %s - evaluée : %s" name expected
        evaluated

    let ko_missing_error ~name = Format.sprintf "KO | erreur manquante: %s" name

    let ko_unexpected_error ~name =
      Format.sprintf "KO | erreur innattendue: %s" name

    let ok_ignored ~name = Format.sprintf "OK | %s ignorée" name

    let ok_non_returned ~name =
      Format.sprintf "OK | %s ignorée car non-restituée" name

    let test_results ~num ~tot =
      Format.sprintf "Résultats des tests: %d succès sur %d" num tot

    let unknown_variable ~name = Format.sprintf "Variable %S inconnue" name

    let unexpected_failure = "Erreur innattendue"

    let variable_absent_from_tgv ~name =
      Format.sprintf "Variable inconnue dans le TGV: %s" name
  end

  module Validator = struct
    module Warning = struct
      let autocycle ~rule_id ~var_name =
        Ppf.fmake "Auto-cycle dans la règle %d avec la variable %S" rule_id
          var_name

      let reference_used_to_set_reference ~var_name ~pos =
        Ppf.fmake
          ~spans:[ (None, pos) ]
          "Variable %s utilisée pour référencer un champ d'événement. \
           Assurez-vous qu'il s'agisse bien d'une variable du TGV et non d'une \
           variable temporaire, sans quoi cette\n\
          \           instruction sera sans effet."
          var_name

      let variable_defined_several_times ~var_name ~pos_list =
        Ppf.fmake
          ~spans:(List.map (fun l -> (None, l)) pos_list)
          "Variable %S définie plus d'une fois dans la même règle" var_name
    end

    module Error = struct
      let print_proc_type (p : Types.proc_type) =
        match p with
        | Rule -> "règle"
        | Verif -> "verification"
        | Func -> "function"
        | Filter -> "filter"
        | Target -> "cible"

      let print_rdom_or_chain = function
        | Types.RuleDomain rdom_id ->
            Format.asprintf "domaine de règles %S" rdom_id
        | Chaining ch -> Format.sprintf "chaînage %S" ch

      let format_scope ppf s =
        Pp.string ppf
        @@
        match s with
        | Types.Tgv -> "variable du TGV"
        | Ref -> "reference vers une variable"
        | Temp -> "variable temporaire"

      (* -- *)

      let alias_already_declared ~alias ~old_pos =
        Format.asprintf "alias %S declaré plusieurs fois : déjà déclaré %a"
          alias Pos.format old_pos

      let alias_already_declared_as_var ~alias ~old_pos =
        Format.asprintf
          "alias %S déclaré plusieurs fois : déjà déclaré comme variable %a"
          alias Pos.format old_pos

      let attribute_already_declared ~attr ~old_pos =
        Format.asprintf "attribut %S declaré plusieurs fois : déjà déclaré %a"
          attr Pos.format old_pos

      let attribute_already_defined ~attr ~old_pos =
        Format.asprintf "attribut %S defini plusieurs fois : déjà défini %a"
          attr Pos.format old_pos

      let attribute_undefined ~var ~attr =
        Format.asprintf "variable %S n'a pas d'attribut %S" var attr

      let category_forbidden_with_space ~sp_name =
        Pp.spr "catégorie de variable interdite avec l'espace de variable %S"
          sp_name

      let default_domain_already_declared ~pc ~old_pos =
        Format.asprintf
          "domaine par défaut %s déclaré plusieurs fois : déjà déclaré %a"
          (print_proc_type pc) Pos.format old_pos

      let default_variable_space_already_declared ~old_pos =
        Pp.spr
          "espace de variable par défaut déclaré plusieurs fois : déjà déclaré \
           %a"
          Pos.format old_pos

      let domain_already_declared ~pc ~old_pos =
        Format.asprintf "domaine %s declaré plusieurs fois : déjà declaré %a"
          (print_proc_type pc) Pos.format old_pos

      let domain_already_used ~pc ~old_pos =
        Format.asprintf "domaine de %s déjà utilisé %a" (print_proc_type pc)
          Pos.format old_pos

      let domain_specialize_itself ~pc ~dom_id =
        Format.asprintf "domaine de %s %S se spécialise lui-même"
          (print_proc_type pc) dom_id

      let error_already_declared ~err ~old_pos =
        Format.asprintf "erreur %S declarée plusieurs fois: déjà déclarée %a"
          err Pos.format old_pos

      let event_already_declared ~old_pos =
        Format.asprintf "champ d'événement déclarés plusieurs fois à %a"
          Pos.format old_pos

      let event_field_already_declared ~event_field ~old_pos =
        Format.asprintf "champ d'événement %S a déjà été déclaré à %a"
          event_field Pos.format old_pos

      let event_field_is_not_a_reference ~name =
        Format.asprintf
          "champ d'événement %S n'est pas une référence de variable" name

      let event_field_need_a_variable ~name =
        Format.asprintf "champ d'événement %S attend une variable" name

      let expression_only_in_filter =
        "expression uniquement autorisée dans les filtres de verification"

      let forbidden_expression_in_filter =
        "expression interdite dans les filtres de vérification"

      let forbidden_value_check_in_switch ~case =
        Format.sprintf
          "branche %S invalide : on ne peut aiguiller une valeur sur un \
           aiguillage de nom"
          case

      let forbidden_variable_check_in_switch ~case =
        Format.sprintf
          "branche %S invalide : on ne peut aiguiller un nom de variable sur \
           un aiguillage de valeur"
          case

      let forbidden_in_var_in_function ~vn ~fn =
        Format.sprintf "variable %S ne peut être lue dans la fonction %S" vn fn

      let forbidden_out_var_in_function ~vn ~fn =
        Format.sprintf "variable %S ne peut être écrite dans la fonction %S" vn
          fn

      let forbidden_variable_in_raise = "variable interdite dans leve_erreur"

      let function_does_not_exist ~fn = Format.sprintf "fonction %S inconnue" fn

      let function_result_missing ~fn =
        Format.sprintf "variable de resultat non définie dans la fonction %S" fn

      let has_no_target = "ce programme n'a pas de cible"

      let instruction_forbidden_in_rules =
        "instruction interdite dans une règle"

      let instruction_forbidden_outside_function =
        "instruction uniquement permise dans une fonction"

      let instruction_forbidden_outside_target =
        "instruction uniquement permise dans une cible"

      let is_base_function ~fn =
        Format.sprintf "fonction %S existe déjà comme fonction de base" fn

      let loop_in_domains ~pc ~cycle =
        Format.asprintf "boucle detectée dans les domaines de %s@;@[<v 2>%a@]"
          (print_proc_type pc)
          (pp_cycle Format.pp_print_string)
          cycle

      let loop_in_rules ~rdom_chain ~cycle =
        let rdom_chain_str = print_rdom_or_chain rdom_chain in
        let pp_cycle fmt cycle =
          let rec aux first = function
            | [] -> ()
            | (v, Some e) :: tl ->
                if first then Format.fprintf fmt "règle %d\n" v
                else Format.fprintf fmt " -(%s)-> règle %d\n" e v;
                aux false tl
            | (v, None) :: tl ->
                if first then Format.fprintf fmt "règle %d\n" v
                else Format.fprintf fmt " -()-> règle %d\n" v;
                aux false tl
          in
          aux true cycle
        in
        Format.asprintf "boucle dans les règles du %s détectée:\n%a"
          rdom_chain_str pp_cycle cycle

      let main_target_not_found ~main_target =
        Format.sprintf "cible principale %S non définie" main_target

      let multimax_require_two_args =
        "fonction multimax a besoin de 2 arguments"

      let no_default_domain ~pc =
        Format.asprintf "domaine de %s par défaut non défini"
          (print_proc_type pc)

      let no_default_variable_space = "espace de variable par défaut non défini"

      let non_exclusive_cases ~case =
        Pp.spr
          "aiguillage non exclusif détecté : branche %S ne peut pas être \
           aiguillée deux fois"
          case

      let pc_already_defined ~pc ~pc_id ~old_pos =
        Format.asprintf "%s %d definie plusieurs fois : déjà définie %a"
          (print_proc_type pc) pc_id Pos.format old_pos

      let rule_domain_incompatible_with_chaining ~ch_name =
        Format.asprintf "domaine de règle incompatible avec l'enchaineur %S"
          ch_name

      let rule_domain_not_computable = "domaine de règle non calculable"

      let second_arg_of_multimax =
        "second argument de la fonction multimax doit être une variable"

      let stop_outside_scope ~scope =
        Format.sprintf
          "instruction 'stop%s;' doit être utilisée dans une iteration"
          (match scope with None -> String.empty | Some s -> " " ^ s)

      let stop_with_invalid_scope ~scope ~current_scopes =
        Pp.spr "cadre %S ne peut être stoppé; les cadres actuels sont: %a" scope
          (Format.pp_print_list
             ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",")
             Format.pp_print_string)
          current_scopes

      let table_used_as_variable ~decl_pos =
        Format.asprintf "table utilisée comme une variable, declarée %a"
          Pos.format decl_pos

      let target_already_declared ~name ~old_pos =
        Format.asprintf "cible %S declarée plusieurs fois: déjà déclarée %a"
          name Pos.format old_pos

      let target_must_not_have_a_result ~tn =
        Format.sprintf "cible %S ne doit pas avoir de résultat" tn

      let temporary_variable_already_declared ~var ~old_pos =
        Format.asprintf
          "variable temporaire %S déclarée plusieurs fois: déjà déclarée %a" var
          Pos.format old_pos

      let tmp_var_has_no_var_space ~var_name =
        Pp.spr
          "variable temporaire %S ne peut pas appartenir à un espace de nom"
          var_name

      let unexpected_variable_scope ~scope ~expected ~var_name =
        Pp.spr "la variable %S est une %a; une %a était attendue." var_name
          format_scope scope format_scope expected

      let unknown_attribut ~attr = Format.sprintf "attribute %S inconnu" attr

      let unknown_attribut_for_var ~attr ~var_name ~category =
        Format.asprintf
          "attribut %S inconnu pour la variable %S de catégorie %S" attr
          var_name category

      let unknown_chaining = "enchaineur inconnu"

      let unknown_domain ~pc = Pp.spr "domaine %S inconnu" (print_proc_type pc)

      let unknown_error = "erreur inconnue"

      let unknown_event_field ~name =
        Format.asprintf "champ d'événement %S inconnu" name

      let unknown_target ~name = Format.asprintf "cible %S inconnue" name

      let unknown_var_space ~name =
        Format.asprintf "espace de variable %S inconnu" name

      let unknown_variable = "variable inconnue"

      let unknown_variable_category = "catégorie de variable inconnue"

      let var_category_already_defined ~category ~old_pos =
        Format.asprintf "categorie %S definie plusieurs fois : déjà définie %a"
          category Pos.format old_pos

      let var_have_no_attrs ~var = Pp.spr "variable %S n'a pas d'attributs" var

      let var_spaces_forbidden ~pc =
        Pp.spr "espaces de variable interdits dans les %ss" (print_proc_type pc)

      let variable_already_declared ~var ~old_pos =
        Format.asprintf "variable %S déclarée plusieurs fois: déjà declarée %a"
          var Pos.format old_pos

      let variable_already_declared_as_alias ~var ~old_pos =
        Format.asprintf
          "variable %S déclarée plusieurs fois: déjà déclarée comme alias %a"
          var Pos.format old_pos

      let variable_forbidden_in_filter =
        "variables interdites dans les filtres de vérification"

      let variable_not_in_var_space ~var_name ~sp_name =
        Pp.spr "variable %S n'appartient pas à l'espace de nom %S" var_name
          sp_name

      let variable_of_unknown_category ~category =
        Format.asprintf "catégorie %S inconnue pour une variable" category

      let variable_space_already_declared ~old_pos =
        Pp.spr "espace de nom déclaré plusieurs fois : déjà défini %a"
          Pos.format old_pos

      let variable_used_as_table ~decl_pos =
        Format.asprintf "variable utilisée comme une table, déclarée %a"
          Pos.format decl_pos

      let variable_with_forbidden_category =
        Format.sprintf
          "variable avec une catégorie interdite dans une verification"

      let verif_domain_not_verifiable = "domain de verfification non vérifiable"

      let wrong_arity_of_function ~func ~arity =
        Format.asprintf "erreur d'arité : la fonction %S attend %d argument%s"
          func arity
          (if arity = 1 then "" else "s")

      let wrong_interval_bounds = "bornes d'intervalle incorectes"

      let wrong_number_of_args ~target_name ~nb_args =
        Format.asprintf "mauvais nombre d'arguments pour %S, %d attendus"
          target_name nb_args
    end
  end
end

include Fr

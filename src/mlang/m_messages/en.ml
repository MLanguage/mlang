open Utils
open Types

module En : LANG = struct
  module Config = struct
    let cannot_display_time_and_force_nondeterminism =
      "Cannot display time and force_nondeterministic display"

    let dgfip_backend_without_dgfip_options =
      "When using DGFiP backend, DGFiP options MUST be provided."

    let failed_parsing_of_dgfip_options = "Failed parsing of dgfip options"

    let invalid_long_size ~long_size =
      Format.sprintf "Invalid long size %S" long_size

    let invalid_message_format ~message_format =
      Format.sprintf "Invalid message format %S" message_format

    let invalid_precision_option ~precision =
      Format.sprintf "Invalid précision %S" precision

    let invalid_roundops_option ~roundops =
      Format.sprintf "Invalid rounding strategy %S" roundops

    let invalid_term_in_dgfip_options = "Invalid term in dgfip options"

    let no_m_files = "Please provide at least 1 M file"

    let option_mpp_function_required = "Required mpp_function option"

    let uncaught_exception_while_reading_dgfip_options =
      "Uncaught exception while reading dgfip options"

    let unspecified_roundops = "Unknown rounding strategy"
  end

  module Driver = struct
    let raised_in_the = function Errors.Validator -> "the validator"

    let cmdline_arg_parsing_failed = "Parsing of command line arguments failed"

    let blocking_error_raised_in ri =
      Format.sprintf "Blocking error raised in %s." (raised_in_the ri)

    let missing_output = "An output file must be defined with --output."

    let term_eval_error = "Term evaluation error"

    let uncaught_exception = "Uncaught exception"

    let unknown_backend = "No backend specified (--backend)"
  end

  module Parser = struct
    let incomplete_attr_definition = "Incomplete attribute definition."

    let incomplete_attr_list =
      "Incomplete attribute list. Did you forget to end the list with ':'?"

    let missing_value_after_equal = "Missing value after symbol '='."

    let syntax_error ~code = Format.sprintf "Syntax error (code %i)." code

    let unexpected_symbol = "Unexpected symbol. Did you forget ';'?"

    let unexpected_syntax_error = "Unexpected syntax error."
  end

  module Validator = struct
    module Warning = struct
      let autocycle ~rule_id ~var_name =
        Log.fmake "Auto-cycle in rule %d with variable %s" rule_id var_name

      let reference_used_to_set_reference ~var_name ~pos =
        Log.fmake
          ~spans:[ (None, pos) ]
          "Variable %s used to set an event reference. Make sure it is not a \
           temporary variable, otherwise this instruction will have no effect."
          var_name

      let variable_defined_several_times ~var_name ~pos_list =
        Log.make
          (Pp.spr "Variable %S is defined more than once in the same rule"
             var_name)
          ~spans:(List.map (fun l -> (None, l)) pos_list)
    end

    module Error = struct
      let print_proc_type (p : Types.proc_type) =
        match p with
        | Rule -> "rule"
        | Verif -> "verification"
        | Func -> "function"
        | Filter -> "filter"
        | Target -> "target"

      let print_rdom_or_chain = function
        | Types.RuleDomain rdom_id -> Format.asprintf "rule domain %S" rdom_id
        | Chaining ch -> Format.sprintf "chaining %S" ch

      let format_scope ppf = function
        | Types.Tgv -> Pp.string ppf "TGV"
        | Temp -> Pp.string ppf "temporary"
        | Ref -> Pp.string ppf "reference"

      (* -- *)

      let alias_already_declared ~alias ~old_pos =
        Format.asprintf "alias %S declared more than once: already declared %a"
          alias Pos.format old_pos

      let alias_already_declared_as_var ~alias ~old_pos =
        Format.asprintf
          "alias %S declared more than once: already declared as variable %a"
          alias Pos.format old_pos

      let attribute_already_declared ~attr ~old_pos =
        Format.asprintf
          "attribute %S declared more than once: already declared %a" attr
          Pos.format old_pos

      let attribute_already_defined ~attr ~old_pos =
        Format.asprintf
          "attribute %S defined more than once: already defined %a" attr
          Pos.format old_pos

      let attribute_undefined ~var ~attr =
        Format.asprintf "variable %S has no attribute %S" var attr

      let category_forbidden_with_space ~sp_name =
        Pp.spr "variable category forbidden with variable space %S" sp_name

      let default_domain_already_declared ~pc ~old_pos =
        Format.asprintf
          "default %s domain declared more than once: already declared %a"
          (print_proc_type pc) Pos.format old_pos

      let default_variable_space_already_declared ~old_pos =
        Pp.spr
          "default variable space declared more than once: already declared %a"
          Pos.format old_pos

      let domain_already_declared ~pc ~old_pos =
        Format.asprintf "%s domain declared more than once: already declared %a"
          (print_proc_type pc) Pos.format old_pos

      let domain_already_used ~pc ~old_pos =
        Format.asprintf "domain of this %s already used %a" (print_proc_type pc)
          Pos.format old_pos

      let domain_specialize_itself ~pc ~dom_id =
        Format.asprintf "%s domain %S specialize itself" (print_proc_type pc)
          dom_id

      let error_already_declared ~err ~old_pos =
        Format.asprintf "error %S declared more than once: already declared %a"
          err Pos.format old_pos

      let event_already_declared ~old_pos =
        Format.asprintf "event fields are already declared at %a" Pos.format
          old_pos

      let event_field_already_declared ~event_field ~old_pos =
        Format.asprintf "event field %S is already declared at %a" event_field
          Pos.format old_pos

      let event_field_is_not_a_reference ~name =
        Format.asprintf "event field %S is not a variable reference" name

      let event_field_need_a_variable ~name =
        Format.asprintf "event field %S require a variable" name

      let expression_only_in_filter =
        "expression authorized only in verif filters"

      let forbidden_expression_in_filter =
        "forbidden expression in verif filter"

      let forbidden_value_check_in_switch ~case =
        Format.sprintf
          "invalid switch case %S: cannot match a value in a name switch" case

      let forbidden_variable_check_in_switch ~case =
        Format.sprintf
          "invalid switch case %S : cannot match a variable in a value switch"
          case

      let forbidden_in_var_in_function ~vn ~fn =
        Format.sprintf "variable %S cannot be read in function %S" vn fn

      let forbidden_out_var_in_function ~vn ~fn =
        Format.sprintf "variable %S cannot be written in function %S" vn fn

      let forbidden_variable_in_raise = "forbidden variable in leve_erreur"

      let function_does_not_exist ~fn =
        Format.sprintf "function %S does not exist" fn

      let function_result_missing ~fn =
        Format.sprintf "result missing in function %S" fn

      let has_no_target = "this program has no target"

      let instruction_forbidden_in_rules = "instruction forbidden in rules"

      let instruction_forbidden_outside_function =
        "instruction only allowed in functions"

      let instruction_forbidden_outside_target =
        "instruction only allowed in targets"

      let is_base_function ~fn =
        Format.sprintf "function %s already exist as base function" fn

      let loop_in_domains ~pc ~cycle =
        Format.asprintf
          "there is a loop in the %s domain hierarchy@;@[<v 2>%a@]"
          (print_proc_type pc)
          (pp_cycle Format.pp_print_string)
          cycle

      let loop_in_rules ~rdom_chain ~cycle =
        let rdom_chain_str = print_rdom_or_chain rdom_chain in
        let pp_cycle fmt cycle =
          let rec aux first = function
            | [] -> ()
            | (v, Some e) :: tl ->
                if first then Format.fprintf fmt "rule %d\n" v
                else Format.fprintf fmt " -(%s)-> rule %d\n" e v;
                aux false tl
            | (v, None) :: tl ->
                if first then Format.fprintf fmt "rule %d\n" v
                else Format.fprintf fmt " -()-> rule %d\n" v;
                aux false tl
          in
          aux true cycle
        in
        Format.asprintf "there is a loop in rules of %s:\n%a" rdom_chain_str
          pp_cycle cycle

      let main_target_not_found ~main_target =
        Format.sprintf "main target %S not found" main_target

      let multimax_require_two_args = "function multimax require two arguments"

      let no_default_domain ~pc =
        Format.asprintf "there are no default %s domain" (print_proc_type pc)

      let no_default_variable_space = "there is no default variable space"

      let non_exclusive_cases ~case =
        Pp.spr "switch cases must be exclusive: %S cannot be used twice" case

      let pc_already_defined ~pc ~pc_id ~old_pos =
        Format.asprintf "%s %d defined more than once: already defined %a"
          (print_proc_type pc) pc_id Pos.format old_pos

      let rule_domain_incompatible_with_chaining ~ch_name =
        Format.asprintf "rule domain incompatible with chaining %S" ch_name

      let rule_domain_not_computable = "rule domain not computable"

      let second_arg_of_multimax =
        "second argument of function multimax must be a variable name"

      let stop_outside_scope ~scope =
        Format.sprintf
          "instruction 'stop%s;' should only be used inside an iteration"
          (match scope with None -> String.empty | Some s -> " " ^ s)

      let stop_with_invalid_scope ~scope ~current_scopes =
        Pp.spr "scope %S cannot be exited; current scopes are: %a" scope
          (Format.pp_print_list
             ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",")
             Format.pp_print_string)
          current_scopes

      let table_used_as_variable ~decl_pos =
        Format.asprintf "table used as a variable, declared %a" Pos.format
          decl_pos

      let target_already_declared ~name ~old_pos =
        Format.asprintf "target %S declared more than once: already declared %a"
          name Pos.format old_pos

      let target_must_not_have_a_result ~tn =
        Format.sprintf "target %S must not have a result" tn

      let temporary_variable_already_declared ~var ~old_pos =
        Format.asprintf
          "temporary variable %S declared more than once: already declared %a"
          var Pos.format old_pos

      let tmp_var_has_no_var_space ~var_name =
        Pp.spr "temporary variable %S does not has a space" var_name

      let unexpected_variable_scope ~scope ~expected ~var_name =
        Pp.spr "Variable %S is a %a variable; expected a %a variable" var_name
          format_scope scope format_scope expected

      let unknown_attribut ~attr = Format.sprintf "unknown attribute %S" attr

      let unknown_attribut_for_var ~attr ~var_name ~category =
        Format.asprintf "unknown attribute %S for variable %S of category %S"
          attr var_name category

      let unknown_chaining = "unknown chaining"

      let unknown_domain ~pc = Pp.spr "unknown %s domain" (print_proc_type pc)

      let unknown_error = "unknown error"

      let unknown_event_field ~name =
        Format.asprintf "unknown event field %S" name

      let unknown_target ~name = Format.asprintf "unknown target %s" name

      let unknown_var_space ~name =
        Format.asprintf "unknown variable space %S" name

      let unknown_variable = "unknown variable"

      let unknown_variable_category = "unknown_variable category"

      let var_category_already_defined ~category ~old_pos =
        Format.asprintf "Category %S defined more than once: already defined %a"
          category Pos.format old_pos

      let var_have_no_attrs ~var = Pp.spr "variable %s have no attributes" var

      let var_spaces_forbidden ~pc =
        Pp.spr "variable spaces are forbidden in %ss" (print_proc_type pc)

      let variable_already_declared ~var ~old_pos =
        Format.asprintf
          "variable %S declared more than once: already declared %a" var
          Pos.format old_pos

      let variable_already_declared_as_alias ~var ~old_pos =
        Format.asprintf
          "variable %S declared more than once: already declared as alias %a"
          var Pos.format old_pos

      let variable_forbidden_in_filter =
        "variables are forbidden in verif filters"

      let variable_not_in_var_space ~var_name ~sp_name =
        Pp.spr "variable %S does not belong to space %S" var_name sp_name

      let variable_of_unknown_category ~category =
        Format.asprintf "variable with unknown category %S" category

      let variable_space_already_declared ~old_pos =
        Pp.spr "variable space declared more than once: already declared %a"
          Pos.format old_pos

      let variable_used_as_table ~decl_pos =
        Format.asprintf "variable used as a table, declared %a" Pos.format
          decl_pos

      let variable_with_forbidden_category =
        Format.sprintf "variable with forbidden category in verif"

      let verif_domain_not_verifiable = "verif domain not verifiable"

      let wrong_arity_of_function ~func ~arity =
        Format.asprintf "wrong arity: function %S expect %d argument%s" func
          arity
          (if arity = 1 then "" else "s")

      let wrong_interval_bounds = "wrong interval bounds"

      let wrong_number_of_args ~target_name ~nb_args =
        Format.asprintf "wrong number of arguments for %S, %d required"
          target_name nb_args
    end
  end
end

include En

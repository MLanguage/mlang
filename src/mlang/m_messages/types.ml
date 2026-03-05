(** {2 Common types} *)

type rdom_or_chain = RuleDomain of string | Chaining of string

type proc_type = Rule | Verif | Func | Filter | Target

type var_scope = Tgv | Temp | Ref

(** A module for all messages used in mlang in a given language. *)
module type LANG = sig
  (** Messages for the Config module *)
  module Config : sig
    val cannot_display_time_and_force_nondeterminism : string

    val dgfip_backend_without_dgfip_options : string

    val failed_parsing_of_dgfip_options : string

    val invalid_long_size : long_size:string -> string

    val invalid_message_format : message_format:string -> string

    val invalid_precision_option : precision:string -> string

    val invalid_roundops_option : roundops:string -> string

    val invalid_term_in_dgfip_options : string

    val no_m_files : string

    val option_mpp_function_required : string

    val uncaught_exception_while_reading_dgfip_options : string

    val unspecified_roundops : string
  end

  (** Messages for the driver*)
  module Driver : sig
    val cmdline_arg_parsing_failed : string

    val blocking_error_raised_in : Errors.raised_in -> string

    val missing_output : string

    val term_eval_error : string

    val uncaught_exception : string

    val unknown_backend : string
  end

  (** Parser error messages *)
  module Parser : sig
    val incomplete_attr_definition : string

    val incomplete_attr_list : string

    val missing_value_after_equal : string

    val syntax_error : code:int -> string

    val unexpected_symbol : string

    val unexpected_syntax_error : string
  end

  (** Validator messages. *)
  module Validator : sig
    module Warning : sig
      val autocycle : rule_id:int -> var_name:string -> Ppf.structured_msg

      val reference_used_to_set_reference :
        var_name:string -> pos:Pos.t -> Ppf.structured_msg

      val variable_defined_several_times :
        var_name:string -> pos_list:Pos.t list -> Ppf.structured_msg
    end

    module Error : sig
      val alias_already_declared : alias:string -> old_pos:Pos.t -> string
      (** Validator error messages. *)

      val alias_already_declared_as_var :
        alias:string -> old_pos:Pos.t -> string

      val attribute_already_declared : attr:string -> old_pos:Pos.t -> string

      val attribute_already_defined : attr:string -> old_pos:Pos.t -> string

      val attribute_undefined : var:string -> attr:string -> string

      val category_forbidden_with_space : sp_name:string -> string

      val default_domain_already_declared :
        pc:proc_type -> old_pos:Pos.t -> string

      val default_variable_space_already_declared : old_pos:Pos.t -> string

      val domain_already_declared : pc:proc_type -> old_pos:Pos.t -> string

      val domain_already_used : pc:proc_type -> old_pos:Pos.t -> string

      val domain_specialize_itself : pc:proc_type -> dom_id:string -> string

      val error_already_declared : err:string -> old_pos:Pos.t -> string

      val event_already_declared : old_pos:Pos.t -> string

      val event_field_already_declared :
        event_field:string -> old_pos:Pos.t -> string

      val event_field_is_not_a_reference : name:string -> string

      val event_field_need_a_variable : name:string -> string

      val expression_only_in_filter : string

      val forbidden_expression_in_filter : string

      val forbidden_value_check_in_switch : case:string -> string

      val forbidden_variable_check_in_switch : case:string -> string

      val forbidden_in_var_in_function : vn:string -> fn:string -> string

      val forbidden_out_var_in_function : vn:string -> fn:string -> string

      val forbidden_variable_in_raise : string

      val function_does_not_exist : fn:string -> string

      val function_result_missing : fn:string -> string

      val has_no_target : string

      val instruction_forbidden_in_rules : string

      val instruction_forbidden_outside_function : string

      val instruction_forbidden_outside_target : string

      val is_base_function : fn:string -> string

      val loop_in_domains : pc:proc_type -> cycle:string list -> string

      val loop_in_rules :
        rdom_chain:rdom_or_chain -> cycle:(int * string option) list -> string

      val main_target_not_found : main_target:string -> string

      val multimax_require_two_args : string

      val no_default_domain : pc:proc_type -> string

      val no_default_variable_space : string

      val non_exclusive_cases : case:string -> string

      val pc_already_defined :
        pc:proc_type -> pc_id:int -> old_pos:Pos.t -> string

      val rule_domain_incompatible_with_chaining : ch_name:string -> string

      val rule_domain_not_computable : string

      val second_arg_of_multimax : string

      val stop_outside_scope : scope:string option -> string

      val stop_with_invalid_scope :
        scope:string -> current_scopes:string list -> string

      val table_used_as_variable : decl_pos:Pos.t -> string

      val target_already_declared : name:string -> old_pos:Pos.t -> string

      val target_must_not_have_a_result : tn:string -> string

      val temporary_variable_already_declared :
        var:string -> old_pos:Pos.t -> string

      val tmp_var_has_no_var_space : var_name:string -> string

      val unexpected_variable_scope :
        scope:var_scope -> expected:var_scope -> var_name:string -> string

      val unknown_attribut : attr:string -> string

      val unknown_attribut_for_var :
        attr:string -> var_name:string -> category:string -> string

      val unknown_chaining : string

      val unknown_domain : pc:proc_type -> string

      val unknown_error : string

      val unknown_event_field : name:string -> string

      val unknown_target : name:string -> string

      val unknown_var_space : name:string -> string

      val unknown_variable : string

      val unknown_variable_category : string

      val var_category_already_defined :
        category:string -> old_pos:Pos.t -> string

      val var_have_no_attrs : var:string -> string

      val var_spaces_forbidden : pc:proc_type -> string

      val variable_already_declared : var:string -> old_pos:Pos.t -> string

      val variable_already_declared_as_alias :
        var:string -> old_pos:Pos.t -> string

      val variable_forbidden_in_filter : string

      val variable_not_in_var_space :
        var_name:string -> sp_name:string -> string

      val variable_of_unknown_category : category:string -> string

      val variable_space_already_declared : old_pos:Pos.t -> string

      val variable_used_as_table : decl_pos:Pos.t -> string

      val variable_with_forbidden_category : string

      val verif_domain_not_verifiable : string

      val wrong_arity_of_function : func:string -> arity:int -> string

      val wrong_interval_bounds : string

      val wrong_number_of_args : target_name:string -> nb_args:int -> string
    end
  end
end

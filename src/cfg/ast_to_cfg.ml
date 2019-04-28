(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

(** AST to CFG translation of M *)

type io_status =
  | Input
  | Output
  | Constant
  | Regular

type var_decl_data = {
  var_decl_typ: Ast.value_typ option;
  var_decl_is_table: int option;
  var_decl_descr: string option;
  var_decl_io: io_status;
  var_pos: Ast.position;
}

let translate_value_typ (typ: Ast.value_typ Ast.marked option) : Cfg.typ option =
  match typ with
  | Some (Ast.Integer, _) -> Some Cfg.Integer
  | Some (Ast.Boolean, _) -> Some Cfg.Boolean
  | Some (Ast.Real, _) -> Some Cfg.Real
  | Some (_ , pos) ->
    raise (Errors.Unimplemented
             ("date type is not supported",
              pos))
  | None -> None

module ParamsMap = Map.Make(Char)

type loop_param_value =
  | VarName of Ast.variable_name
  | RangeInt of int

let format_loop_param_value (v: loop_param_value) : string = match v with
  | VarName v -> v
  | RangeInt i -> string_of_int i

type loop_context = loop_param_value ParamsMap.t

type loop_domain = loop_param_value list ParamsMap.t

let format_loop_context (ld: loop_context) : string = ParamsMap.fold (fun param value acc ->
    acc ^ "; " ^ (Printf.sprintf "%c=" param)  ^
    format_loop_param_value value
  ) ld ""


let format_loop_domain (ld: loop_domain) : string = ParamsMap.fold (fun param values acc ->
    acc ^ "; " ^ (Printf.sprintf "%c=" param)  ^
    (String.concat "," (List.map (fun value -> format_loop_param_value value) values))
  ) ld ""

module VarNameToID = Map.Make(String)
type idmap = Cfg.Variable.t VarNameToID.t

type translating_context = {
  idmap : idmap;
  lc: loop_context option;
}

let rec iterate_all_combinations (ld: loop_domain) : loop_context list =
  try let param, values = ParamsMap.choose ld in
    match values with
    | [] -> []
    | hd::[] ->
      let new_ld = ParamsMap.remove param ld in
      let all_contexts = iterate_all_combinations new_ld in
      if List.length all_contexts = 0 then
        [ParamsMap.singleton param hd]
      else
        (List.map (fun c -> ParamsMap.add param hd c) all_contexts)
    | hd::tl ->
      let new_ld = ParamsMap.add param tl ld in
      let all_contexts_minus_hd_val_for_param = iterate_all_combinations new_ld in
      let new_ld = ParamsMap.add param [hd] ld in
      let all_context_with_hd_val_for_param = iterate_all_combinations new_ld in
      all_context_with_hd_val_for_param@all_contexts_minus_hd_val_for_param
  with
  | Not_found -> []

let rec make_range_list (i1: int) (i2: int) : loop_param_value list =
  if i1 > i2 then [] else
    let tl = make_range_list (i1 + 1) i2 in
    (RangeInt i1)::tl


let translate_loop_variables (ctx: translating_context) (lvs: Ast.loop_variables Ast.marked) :
  ((loop_context -> 'a) -> 'a list) =
  match Ast.unmark lvs with
  | Ast.ValueSets lvs -> (fun translator ->
      let varying_domain = List.fold_left (fun domain (param, values) ->
          let values = List.flatten (List.map (fun value -> match value with
              | Ast.VarParam v -> [VarName (Ast.unmark v)]
              | Ast.IntervalLoop (i1,i2) -> make_range_list (Ast.unmark i1) (Ast.unmark i2)
            ) values) in
          ParamsMap.add (Ast.unmark param) values domain
        ) ParamsMap.empty lvs in
      List.map (fun lc -> translator lc) (iterate_all_combinations varying_domain)
    )
  | Ast.Ranges lvs -> (fun translator ->
      let varying_domain = List.fold_left (fun domain (param, values) ->
          let values = List.map (fun value -> match value with
              | Ast.VarParam v -> assert false (* should not happen *)
              | Ast.IntervalLoop (i1, i2) -> make_range_list (Ast.unmark i1) (Ast.unmark i2)
            ) values in
          ParamsMap.add (Ast.unmark param) (List.flatten values) domain
        ) ParamsMap.empty lvs in
      List.map (fun lc -> translator lc) (iterate_all_combinations varying_domain)
    )

let instantiate_generic_variables_parameters (lc: loop_context) (gen_name:Ast.variable_generic_name) : string =
  ParamsMap.fold (fun param value var_name ->
      match value with
      | VarName value ->
        Str.replace_first (Str.regexp (Printf.sprintf "%c" param)) value var_name
      | RangeInt i ->
        Str.replace_first (Str.regexp (Printf.sprintf "%c" param)) (string_of_int i) var_name
    ) lc gen_name.Ast.base

let get_var_from_name (d:Cfg.Variable.t VarNameToID.t) (name:Ast.variable_name Ast.marked) : Cfg.Variable.t =
  try VarNameToID.find (Ast.unmark name) d with
  | Not_found ->
    raise (Errors.TypeError
             (Errors.Variable
                (Printf.sprintf "variable %s used %s, has not been declared"
                   (Ast.unmark name)
                   (Format_ast.format_position (Ast.get_position name))
                )))

let get_var_or_x (d:Cfg.Variable.t VarNameToID.t) (name:Ast.variable_name Ast.marked) : Cfg.expression =
  if Ast.unmark name = "X" then Cfg.GenericTableIndex else
    Cfg.Var (get_var_from_name d name)

let get_variables_decl (p: Ast.program) : (var_decl_data Cfg.VariableMap.t * idmap) =
  let (vars, idmap, out_list) = List.fold_left (fun (vars, idmap, out_list) source_file ->
      List.fold_left (fun (vars, idmap, out_list) source_file_item ->
          match Ast.unmark source_file_item with
          | Ast.Variable var_decl ->
            begin match var_decl with
              | Ast.ComputedVar cvar ->
                let cvar = Ast.unmark cvar in
                let new_var = Cfg.Variable.new_var cvar.Ast.comp_name in
                let new_var_data = {
                  var_decl_typ = Ast.unmark_option cvar.Ast.comp_typ;
                  var_decl_is_table = Ast.unmark_option cvar.Ast.comp_table;
                  var_decl_descr = Some (Ast.unmark cvar.Ast.comp_description);
                  var_decl_io = Regular;
                  var_pos = Ast.get_position source_file_item;
                }
                in
                let new_vars = Cfg.VariableMap.add new_var new_var_data vars in
                let new_idmap = VarNameToID.add (Ast.unmark cvar.Ast.comp_name) new_var idmap in
                (new_vars, new_idmap, out_list)
              | Ast.InputVar ivar ->
                let ivar = Ast.unmark ivar in
                let new_var = Cfg.Variable.new_var ivar.Ast.input_name in
                let new_var_data = {
                  var_decl_typ = Ast.unmark_option ivar.Ast.input_typ;
                  var_decl_is_table = None;
                  var_decl_descr = Some (Ast.unmark ivar.Ast.input_description);
                  var_decl_io = Input;
                  var_pos = Ast.get_position source_file_item;
                } in
                let new_vars = Cfg.VariableMap.add new_var new_var_data vars in
                let new_idmap = VarNameToID.add (Ast.unmark ivar.Ast.input_name) new_var idmap in
                (new_vars, new_idmap, out_list)
              | Ast.ConstVar (marked_name, _) ->
                let new_var  = Cfg.Variable.new_var marked_name in
                let new_var_data = {
                  var_decl_typ = None;
                  var_decl_is_table = None;
                  var_decl_descr = None;
                  var_decl_io = Constant;
                  var_pos = Ast.get_position source_file_item;
                } in
                let new_vars = Cfg.VariableMap.add new_var new_var_data vars in
                let new_idmap = VarNameToID.add (Ast.unmark marked_name) new_var idmap in
                (new_vars, new_idmap, out_list)
            end
          | Ast.Output out_name -> (vars, idmap, out_name::out_list)
          | _ -> (vars, idmap, out_list)
        ) (vars, idmap, out_list) source_file
    ) (Cfg.VariableMap.empty, VarNameToID.empty, []) p in
  let vars : var_decl_data Cfg.VariableMap.t = List.fold_left (fun vars out ->
      let out_var = get_var_from_name idmap out in
      try
        let data = Cfg.VariableMap.find out_var vars in
        Cfg.VariableMap.add out_var { data with var_decl_io = Output } vars
      with
      | Not_found -> assert false (* should not happen *)
    ) vars out_list in
  (vars, idmap)

let rec translate_variable (ctx: translating_context) (var: Ast.variable Ast.marked) : Cfg.expression Ast.marked =
  match Ast.unmark var with
  | Ast.Normal name ->
    Ast.same_pos_as (get_var_or_x ctx.idmap (Ast.same_pos_as name var)) var
  | Ast.Generic gen_name ->
    if List.length gen_name.Ast.parameters == 0 then
      translate_variable ctx (Ast.same_pos_as (Ast.Normal gen_name.Ast.base) var)
    else match ctx.lc with
      | None ->
        raise (Errors.TypeError
                 (Errors.LoopParam "variable contains loop parameters but is not used inside a loop context"))
      | Some lc ->
        let new_var_name = instantiate_generic_variables_parameters lc gen_name in
        translate_variable ctx (Ast.same_pos_as (Ast.Normal new_var_name) var)

let translate_table_index (ctx: translating_context) (i: Ast.table_index Ast.marked) : Cfg.expression Ast.marked =
  match Ast.unmark i with
  | Ast.LiteralIndex i' -> Ast.same_pos_as (Cfg.Literal (Cfg.Int i')) i
  | Ast.GenericIndex -> Ast.same_pos_as Cfg.GenericTableIndex i
  | Ast.SymbolIndex v ->
    let var = translate_variable ctx (Ast.same_pos_as v i) in
    var

let translate_function_name (f_name : string Ast.marked) = match Ast.unmark f_name with
  | "somme" -> Cfg.SumFunc
  | "min" -> Cfg.MinFunc
  | "max" -> Cfg.MaxFunc
  | "abs" -> Cfg.AbsFunc
  | "positif" -> Cfg.GtzFunc
  | "positif_ou_nul" -> Cfg.GtezFunc
  | "null" -> Cfg.NullFunc
  | "arr" -> Cfg.ArrFunc
  | "inf" -> Cfg.InfFunc
  | "present" -> Cfg.PresentFunc
  | x -> raise (Errors.TypeError (
      Errors.Function (
        Printf.sprintf "unknown function %s %s" x (Format_ast.format_position (Ast.get_position f_name))
      )))

let merge_loop_ctx (ctx: translating_context) (new_lc : loop_context) (pos:Ast.position) : translating_context =
  match ctx.lc with
  | None -> { ctx with lc = Some new_lc }
  | Some old_lc ->
    let merged_lc = ParamsMap.merge (fun param old_val new_val ->
        match (old_val, new_val) with
        | (Some _ , Some _) ->
          raise (Errors.TypeError
                   (Errors.LoopParam
                      (Printf.sprintf "Same loop parameter %c used in two nested loop contexts, %s"
                         param (Format_ast.format_position pos))))
        | (Some v, None) | (None, Some v) -> Some v
        | (None, None) -> assert false (* should not happen *)
      ) old_lc new_lc
    in
    { ctx with lc = Some merged_lc }

let rec translate_func_args (ctx: translating_context) (args: Ast.func_args) : Cfg.expression Ast.marked list =
  match args with
  | Ast.ArgList args -> List.map (fun arg ->
      translate_expression ctx arg
    ) args
  | Ast.LoopList (lvs, e) ->
    let loop_context_provider = translate_loop_variables ctx lvs in
    let translator = fun lc ->
      let new_ctx = merge_loop_ctx ctx lc (Ast.get_position lvs) in
      translate_expression new_ctx e
    in
    loop_context_provider translator

and translate_expression (ctx : translating_context) (f: Ast.expression Ast.marked) : Cfg.expression Ast.marked =
  Ast.same_pos_as
    (match Ast.unmark f with
     | Ast.TestInSet (negative, e, values) ->
       let new_e = translate_expression ctx e in
       let local_var = Cfg.LocalVariable.new_var () in
       let local_var_expr =  Cfg.LocalVar local_var in
       let or_chain = List.fold_left (fun or_chain set_value ->
           let equal_test = match set_value with
             | Ast.VarValue set_var ->  Cfg.Comparison (
                 Ast.same_pos_as Ast.Eq set_var,
                 Ast.same_pos_as local_var_expr e,
                 translate_variable ctx set_var
               )
             | Ast.IntValue i -> Cfg.Comparison (
                 Ast.same_pos_as Ast.Eq i,
                 Ast.same_pos_as local_var_expr e,
                 Ast.same_pos_as (Cfg.Literal (Cfg.Int (Ast.unmark i))) i
               )
             | Ast.Interval (bn,en) ->
               if Ast.unmark bn > Ast.unmark en then
                 raise (Errors.TypeError
                          (Errors.Numeric
                             (Printf.sprintf "wrong interval bounds %s"
                                (Format_ast.format_position (Ast.get_position bn)))))
               else
                 Cfg.Binop (
                   Ast.same_pos_as Ast.And bn,
                   Ast.same_pos_as (Cfg.Comparison (
                       Ast.same_pos_as Ast.Gte bn,
                       Ast.same_pos_as local_var_expr e,
                       Ast.same_pos_as (Cfg.Literal (Cfg.Int (Ast.unmark bn))) bn
                     )) bn,
                   Ast.same_pos_as (Cfg.Comparison (
                       Ast.same_pos_as Ast.Lte en,
                       Ast.same_pos_as local_var_expr e,
                       Ast.same_pos_as (Cfg.Literal (Cfg.Int (Ast.unmark en))) en
                     )) en
                 )
           in
           Ast.same_pos_as (Cfg.Binop (
               Ast.same_pos_as Ast.Or f,
               Ast.same_pos_as equal_test f,
               or_chain
             )) f
         ) (Ast.same_pos_as (Cfg.Literal (Cfg.Bool false)) f) values in
       let or_chain = if negative then
           Ast.same_pos_as (Cfg.Unop (Ast.Not, or_chain)) or_chain
         else or_chain
       in
       Cfg.LocalLet (local_var, new_e, or_chain)
     | Ast.Comparison (op, e1, e2) ->
       let new_e1 = translate_expression ctx e1 in
       let new_e2 = translate_expression ctx e2 in
       Cfg.Comparison (op, new_e1, new_e2)
     | Ast.Binop (op, e1, e2) ->
       let new_e1 = translate_expression ctx e1 in
       let new_e2 = translate_expression ctx e2 in
       Cfg.Binop (op, new_e1, new_e2)
     | Ast.Unop (op, e) ->
       let new_e = translate_expression ctx e in
       Cfg.Unop (op, new_e)
     | Ast.Index (t, i) ->
       let t_var = translate_variable ctx t in
       let new_i = translate_table_index ctx i in
       Cfg.Index ((match Ast.unmark t_var with
           | Cfg.Var v -> Ast.same_pos_as v t_var
           | _ -> assert false (* should not happen *)), new_i)
     | Ast.Conditional (e1, e2, e3) ->
       let new_e1 = translate_expression ctx e1 in
       let new_e2 = translate_expression ctx e2 in
       let new_e3 = match e3 with
         | Some e3 -> translate_expression ctx e3
         | None -> Ast.same_pos_as Cfg.Error e2
         (* the absence of a else branch for a ternary operators can yield a runtime error *)
       in
       let cond_var = Cfg.LocalVariable.new_var () in
       (*
         We put every conditional behind a local var to make it easier for later
         when we infer the bitvec size of every expression for Z3
       *)
       Cfg.LocalLet(cond_var,
                    Ast.same_pos_as (Cfg.Conditional (new_e1, new_e2, new_e3)) f,
                    Ast.same_pos_as (Cfg.LocalVar cond_var) f
                   )
     | Ast.FunctionCall (f_name, args) ->
       let f_correct = translate_function_name f_name in
       let new_args = translate_func_args ctx args in
       Cfg.FunctionCall (f_correct, new_args)
     | Ast.Literal l -> begin match l with
         | Ast.Variable var ->
           let new_var = translate_variable ctx (Ast.same_pos_as var f) in
           Ast.unmark new_var
         | Ast.Int i -> Cfg.Literal (Cfg.Int i)
         | Ast.Float f -> Cfg.Literal (Cfg.Float f)
       end
     (* These loops correspond to "pour un i dans ...: ... so it's OR "*)
     | Ast.Loop (lvs, e) ->
       let loop_context_provider = translate_loop_variables ctx lvs in
       let translator = fun lc ->
         let new_ctx = merge_loop_ctx ctx lc (Ast.get_position lvs) in
         translate_expression new_ctx e
       in
       let loop_exprs = loop_context_provider translator in
       List.fold_left (fun acc loop_expr ->
           Cfg.Binop(
             Ast.same_pos_as Ast.Or e,
             Ast.same_pos_as acc e,
             loop_expr
           )
         ) (Cfg.Literal (Cfg.Bool false)) loop_exprs
    ) f

type index_def =
  | NoIndex
  | SingleIndex of int
  | GenericIndex

let translate_lvalue (ctx: translating_context) (lval: Ast.lvalue Ast.marked) : Cfg.Variable.t * index_def =
  let var = match Ast.unmark (translate_variable ctx  (Ast.unmark lval).Ast.var)
    with
    | Cfg.Var var -> var
    | _ -> assert false (* should not happen *)
  in
  match (Ast.unmark lval).Ast.index with
  | Some ti -> begin match Ast.unmark ti with
      | Ast.GenericIndex -> (var, GenericIndex)
      | Ast.LiteralIndex i -> (var, SingleIndex i)
      | Ast.SymbolIndex _ ->
        raise (Errors.TypeError (Errors.Variable (
            Printf.sprintf "variable definition lvalue %s cannot refer to other variables"
              (Format_ast.format_position (Ast.get_position lval))
          )))
    end
  | None -> (var, NoIndex)

let add_var_def
    (var_data : Cfg.variable_data Cfg.VariableMap.t)
    (var_lvalue: Cfg.Variable.t)
    (var_expr : Cfg.expression Ast.marked)
    (def_kind : index_def)
    (var_decl_data: var_decl_data Cfg.VariableMap.t)
  : Cfg.variable_data Cfg.VariableMap.t =
  let var_decl = try Cfg.VariableMap.find var_lvalue var_decl_data with
    | Not_found -> assert false (* should not happen *)
  in
  let var_typ = translate_value_typ
      (match var_decl.var_decl_typ with
       | Some x -> Some (x, var_decl.var_pos)
       | None -> None)
  in
  try
    let old_var_expr = Cfg.VariableMap.find var_lvalue var_data in
    match (old_var_expr.Cfg.var_definition, def_kind) with
    | (Cfg.SimpleVar old_e, SingleIndex _) | (Cfg.SimpleVar old_e, GenericIndex) ->
      raise (Errors.TypeError (Errors.Variable (
          Printf.sprintf "variable definition %s is indexed but previous definition %s was not"
            (Format_ast.format_position (Ast.get_position var_expr))
            (Format_ast.format_position (Ast.get_position old_e))
        )))
    | (Cfg.InputVar, _) ->
      raise (Errors.TypeError (Errors.Variable (
          Printf.sprintf "input variable %s defined %s is re-defined %s "
            (Ast.unmark (var_lvalue.Cfg.Variable.name))
            (Format_ast.format_position (Ast.get_position var_lvalue.Cfg.Variable.name))
            (Format_ast.format_position (Ast.get_position var_expr))
        )))
    | (Cfg.TableVar (_, Cfg.IndexGeneric _), NoIndex) | (Cfg.TableVar (_, Cfg.IndexTable _), NoIndex) ->
      raise (Errors.TypeError (Errors.Variable (
          Printf.sprintf "variable definition %s is not indexed but previous definitions were"
            (Format_ast.format_position (Ast.get_position var_expr))
        )))
    | (Cfg.TableVar (_, Cfg.IndexGeneric old_e), SingleIndex _) | (Cfg.TableVar (_, Cfg.IndexGeneric old_e), GenericIndex)
    | (Cfg.SimpleVar old_e, NoIndex) ->
      Cli.warning_print
        (Printf.sprintf "Dropping definition %s because variable was previously defined %s"
           (Format_ast.format_position (Ast.get_position var_expr))
           (Format_ast.format_position (Ast.get_position old_e)));
      var_data
    | (Cfg.TableVar (size, Cfg.IndexTable _), GenericIndex) ->
      Cli.warning_print
        (Printf.sprintf "Definition of %s %s will supercede previous partial definitions"
           (Ast.unmark var_lvalue.Cfg.Variable.name)
           (Format_ast.format_position (Ast.get_position var_expr)));
      Cfg.VariableMap.add var_lvalue
        { old_var_expr with
          Cfg.var_definition = Cfg.TableVar (size, Cfg.IndexGeneric var_expr);
        } var_data
    | (Cfg.TableVar (size, Cfg.IndexTable old_defs), SingleIndex i) -> begin try
          let old_def = Cfg.IndexMap.find i old_defs in
          Cli.warning_print
            (Printf.sprintf "Dropping definition %s because variable was previously defined %s"
               (Format_ast.format_position (Ast.get_position var_expr))
               (Format_ast.format_position (Ast.get_position old_def)));
          var_data
        with
        | Not_found ->
          let new_defs = Cfg.IndexMap.add i var_expr old_defs in
          Cfg.VariableMap.add var_lvalue
            { old_var_expr with
              Cfg.var_definition = Cfg.TableVar (size, Cfg.IndexTable new_defs)
            } var_data
      end
  with
  | Not_found -> Cfg.VariableMap.add var_lvalue (
      try
        let decl_data = Cfg.VariableMap.find var_lvalue var_decl_data in
        let io = match decl_data.var_decl_io with
          | Input -> Cfg.Input
          | Constant | Regular -> Cfg.Regular
          | Output -> Cfg.Output
        in
        if def_kind = NoIndex then
          { Cfg.var_definition = Cfg.SimpleVar var_expr;
            Cfg.var_typ = var_typ;
            Cfg.var_io = io }
        else
          match decl_data.var_decl_is_table with
          | Some size -> begin
              match def_kind with
              | NoIndex -> assert false (* should not happen*)
              | SingleIndex i ->
                {
                  Cfg.var_definition = Cfg.TableVar (size, Cfg.IndexTable (Cfg.IndexMap.singleton i var_expr));
                  Cfg.var_typ = var_typ;
                  Cfg.var_io = io
                }
              | GenericIndex ->
                {
                  Cfg.var_definition = Cfg.TableVar (size, Cfg.IndexGeneric var_expr);
                  Cfg.var_typ = var_typ;
                  Cfg.var_io = io
                }
            end
          | None -> raise (Errors.TypeError (
              Errors.Variable (
                Printf.sprintf "variable %s is defined %s as a table but has been declared %s as a non-table"
                  (Ast.unmark var_lvalue.Cfg.Variable.name)
                  (Format_ast.format_position (Ast.get_position var_expr))
                  (Format_ast.format_position (Cfg.VariableMap.find var_lvalue var_decl_data).var_pos)
              )
            ))
      with
      | Not_found -> assert false
        (*
          should not happen since we already looked into idmap to get the var value
          from its name
        *)
    ) var_data

let get_var_data
    (idmap: idmap)
    (var_decl_data: var_decl_data Cfg.VariableMap.t)
    (p: Ast.program)
  : Cfg.variable_data Cfg.VariableMap.t =
  List.fold_left (fun var_data source_file ->
      Cli.debug_print (Printf.sprintf "Expanding definitions in %s" (Ast.get_position (List.hd source_file)).Ast.pos_filename);
      List.fold_left (fun var_data source_file_item -> match Ast.unmark source_file_item with
          | Ast.Rule r -> List.fold_left (fun var_data formula ->
              match Ast.unmark formula with
              | Ast.SingleFormula f ->
                let ctx = { idmap; lc = None } in
                let (var_lvalue, def_kind) = translate_lvalue ctx f.Ast.lvalue in
                let var_expr = translate_expression ctx f.Ast.formula in
                add_var_def var_data var_lvalue var_expr def_kind var_decl_data
              | Ast.MultipleFormulaes (lvs, f) ->
                let ctx = { idmap; lc = None } in
                let loop_context_provider = translate_loop_variables ctx lvs in
                let translator = fun lc ->
                  let new_ctx = { ctx with lc = Some lc } in
                  let var_expr = translate_expression new_ctx f.Ast.formula in
                  let (var_lvalue, def_kind) = translate_lvalue new_ctx f.Ast.lvalue in
                  (var_lvalue, var_expr, def_kind)
                in
                let data_to_add = loop_context_provider translator in
                List.fold_left (fun var_data (var_lvalue, var_expr, def_kind) ->
                    add_var_def var_data var_lvalue var_expr def_kind var_decl_data
                  ) var_data data_to_add
            ) var_data r.Ast.rule_formulaes
          | Ast.Variable (Ast.ConstVar (name, lit)) ->
            let var = get_var_from_name idmap name in
            add_var_def var_data var (Ast.same_pos_as (Cfg.Literal (begin match Ast.unmark lit with
                | Ast.Variable var ->
                  raise (Errors.TypeError (
                      Errors.Variable (
                        Printf.sprintf "const variable %s declared %s cannot be defined as another variable"
                          (Format_ast.format_variable var)
                          (Format_ast.format_position (Ast.get_position source_file_item))
                      )))
                | Ast.Int i -> Cfg.Int i
                | Ast.Float f -> Cfg.Float f
              end)) lit) NoIndex var_decl_data
          | Ast.Variable (Ast.InputVar (var, _)) ->
            let var = get_var_from_name idmap var.Ast.input_name in
            let var_decl = try Cfg.VariableMap.find var var_decl_data with
              | Not_found -> assert false (* should not happen *)
            in
            let typ = translate_value_typ (match var_decl.var_decl_typ  with
                | Some x -> Some (x, Ast.get_position var.Cfg.Variable.name)
                | None -> None
              ) in
            Cfg.VariableMap.add
              var
              { Cfg.var_definition = Cfg.InputVar; Cfg.var_typ = typ; Cfg.var_io = Cfg.Input }
              var_data
          | _ -> var_data
        ) var_data source_file
    ) Cfg.VariableMap.empty (List.rev p)

let check_if_all_variables_defined
    (var_data: Cfg.program)
    (var_decl_data: var_decl_data Cfg.VariableMap.t)
  : unit =
  ignore (Cfg.VariableMap.merge (fun var data decl -> match data, decl with
      | (Some _, Some _) -> None
      | (None, Some decl) -> begin match decl.var_decl_io with
          | Output | Regular | Constant ->
            raise (Errors.TypeError (
                Errors.Variable (
                  Printf.sprintf "variable %s declared %s is never defined"
                    (Ast.unmark var.Cfg.Variable.name)
                    (Format_ast.format_position (Ast.get_position var.Cfg.Variable.name))
                )))
          | Input -> None
        end
      | _ -> assert false (* should not happen *)
    ) var_data var_decl_data)


let translate (p: Ast.program) : Cfg.program =
  let (var_decl_data, idmap) = get_variables_decl p in
  let var_data = get_var_data idmap var_decl_data p in
  check_if_all_variables_defined var_data var_decl_data;
  var_data

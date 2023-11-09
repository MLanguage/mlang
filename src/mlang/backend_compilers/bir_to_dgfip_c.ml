(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

open Bir
module D = DecoupledExpr

let str_escape str =
  let l = String.length str in
  let buf = Buffer.create l in
  let rec aux = function
    | i when i >= l -> Buffer.contents buf
    | i -> begin
        let c = str.[i] in
        let ic = Char.code c in
        match c with
        | '"' | '%' ->
            let cc = Format.sprintf "\\%03o" ic in
            Buffer.add_string buf cc;
            aux (i + 1)
        | _c when ic <= 31 || ic >= 127 ->
            let cc = Format.sprintf "\\%03o" ic in
            Buffer.add_string buf cc;
            aux (i + 1)
        | c ->
            Buffer.add_char buf c;
            aux (i + 1)
      end
  in
  aux 0

let fresh_c_local =
  let c = ref 0 in
  fun name ->
    let s = name ^ string_of_int !c in
    incr c;
    s

let rec generate_c_expr (program : program) (e : expression Pos.marked)
    (var_indexes : Dgfip_varid.var_id_map) : D.expression_composition =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1 = generate_c_expr program e1 var_indexes in
      let se2 = generate_c_expr program e2 var_indexes in
      let safe_def =
        match (Pos.unmark op, Pos.unmark e2) with
        | Mast.Gt, Mir.(Literal (Undefined | Float 0.)) ->
            (* hack to catch positive test in M *) true
        | _ -> false
      in
      let def_test = D.dand se1.def_test se2.def_test in
      let value_comp =
        let op =
          match Pos.unmark op with
          | Mast.Gt -> ">"
          | Mast.Gte -> ">="
          | Mast.Lt -> "<"
          | Mast.Lte -> "<="
          | Mast.Eq -> "=="
          | Mast.Neq -> "!="
        in
        D.comp op se1.value_comp se2.value_comp
      in
      D.build_transitive_composition ~safe_def { def_test; value_comp }
  | Binop ((Mast.Div, _), e1, e2) ->
      let se1 = generate_c_expr program e1 var_indexes in
      let se2 = generate_c_expr program e2 var_indexes in
      let def_test = D.dand se1.def_test se2.def_test in
      let value_comp =
        D.ite se2.value_comp (D.div se1.value_comp se2.value_comp) (D.lit 0.)
      in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | Binop (op, e1, e2) ->
      let se1 = generate_c_expr program e1 var_indexes in
      let se2 = generate_c_expr program e2 var_indexes in
      let def_test =
        match Pos.unmark op with
        | Mast.And | Mast.Mul -> D.dand se1.def_test se2.def_test
        | Mast.Or | Mast.Add | Mast.Sub -> D.dor se1.def_test se2.def_test
        | Mast.Div -> assert false
        (* see above *)
      in
      let op e1 e2 =
        match Pos.unmark op with
        | Mast.And -> D.dand e1 e2
        | Mast.Or -> D.dor e1 e2
        | Mast.Add -> D.plus e1 e2
        | Mast.Sub -> D.sub e1 e2
        | Mast.Mul -> D.mult e1 e2
        | Mast.Div -> assert false
        (* see above *)
      in
      let value_comp = op se1.value_comp se2.value_comp in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | Unop (op, e) ->
      let se = generate_c_expr program e var_indexes in
      let def_test = se.def_test in
      let op, safe_def =
        match op with
        | Mast.Not -> (D.dnot, false)
        | Mast.Minus -> (D.minus, true)
      in
      let value_comp = op se.value_comp in
      D.build_transitive_composition ~safe_def { def_test; value_comp }
  | Index (var, e) ->
      let idx = generate_c_expr program e var_indexes in
      let size =
        Option.get (Bir.var_to_mir (Pos.unmark var)).Mir.Variable.is_table
      in
      let idx_var = D.new_local () in
      let def_test =
        D.let_local idx_var idx.value_comp
          (D.dand
             (D.dand idx.def_test
                (D.comp "<" (D.local_var idx_var) (D.lit (float_of_int size))))
             (D.access (Pos.unmark var) Def (D.local_var idx_var)))
      in
      let value_comp =
        D.let_local idx_var idx.value_comp
          (D.ite
             (D.comp "<" (D.local_var idx_var) (D.lit 0.))
             (D.lit 0.)
             (D.access (Pos.unmark var) Val (D.local_var idx_var)))
      in
      D.build_transitive_composition { def_test; value_comp }
  | Conditional (c, t, f) ->
      let cond = generate_c_expr program c var_indexes in
      let thenval = generate_c_expr program t var_indexes in
      let elseval = generate_c_expr program f var_indexes in
      let def_test =
        D.dand cond.def_test
          (D.ite cond.value_comp thenval.def_test elseval.def_test)
      in
      let value_comp =
        D.ite cond.value_comp thenval.value_comp elseval.value_comp
      in
      D.build_transitive_composition { def_test; value_comp }
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se = generate_c_expr program arg var_indexes in
      let def_test = D.dtrue in
      let value_comp = se.def_test in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (NullFunc, [ arg ]) ->
      let se = generate_c_expr program arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dand def_test (D.comp "==" se.value_comp (D.lit 0.)) in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se = generate_c_expr program arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_arr" [ se.value_comp ] in
      (* Here we boldly assume that rounding value of `undef` will give zero,
         given the invariant. Pretty sure that not true, in case of doubt, turn
         `safe_def` to false *)
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (InfFunc, [ arg ]) ->
      let se = generate_c_expr program arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_floor" [ se.value_comp ] in
      (* same as above *)
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (AbsFunc, [ arg ]) ->
      let se = generate_c_expr program arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "fabs" [ se.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr program e1 var_indexes in
      let se2 = generate_c_expr program e2 var_indexes in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "max" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr program e1 var_indexes in
      let se2 = generate_c_expr program e2 var_indexes in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "min" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let bound = generate_c_expr program e1 var_indexes in
      let def_test =
        D.dfun "multimax_def" [ bound.value_comp; D.m_var v2 PassPointer Def ]
      in
      let value_comp =
        D.dfun "multimax" [ bound.value_comp; D.m_var v2 PassPointer Val ]
      in
      D.build_transitive_composition { def_test; value_comp }
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> { def_test = D.dtrue; value_comp = D.lit f }
  | Literal Undefined -> { def_test = D.dfalse; value_comp = D.lit 0. }
  | Var var ->
      { def_test = D.m_var var None Def; value_comp = D.m_var var None Val }
  | LocalVar lvar ->
      let ldef, lval = D.locals_from_m lvar in
      { def_test = D.local_var ldef; value_comp = D.local_var lval }
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let se1 = generate_c_expr program e1 var_indexes in
      let se2 = generate_c_expr program e2 var_indexes in
      let ldef, lval = D.locals_from_m lvar in
      let declare_local constr =
        D.let_local ldef se1.def_test (D.let_local lval se1.value_comp constr)
      in
      {
        def_test = declare_local se2.def_test;
        value_comp = declare_local se2.value_comp;
      }
  | Attribut (_v, var, a) ->
      let ptr, var_cat_data =
        match Mir.VariableMap.find var.mir_var var_indexes with
        | Dgfip_varid.VarIterate (t, _, vcd) -> (t, vcd)
        | _ -> assert false
      in
      let id_str = var_cat_data.Mir.id_str in
      let def_test =
        D.dinstr
          (Format.sprintf "attribut_%s_def(%s, \"%s\")" id_str ptr
             (Pos.unmark a))
      in
      let value_comp =
        D.dinstr
          (Format.sprintf "attribut_%s(%s, \"%s\")" id_str ptr (Pos.unmark a))
      in
      D.build_transitive_composition { def_test; value_comp }
  | Size var ->
      let ptr =
        match Mir.VariableMap.find var.mir_var var_indexes with
        | Dgfip_varid.VarIterate (t, _, _) -> t
        | _ -> assert false
      in
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr (Format.sprintf "(%s->size)" ptr) in
      D.build_transitive_composition { def_test; value_comp }
  | NbError ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_erreurs_bloquantes(irdata)" in
      D.build_transitive_composition { def_test; value_comp }
  | NbCategory _ -> assert false

let generate_m_assign (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : variable) (offset : D.offset)
    (oc : Format.formatter) (se : D.expression_composition) : unit =
  let def_var = D.generate_variable ~def_flag:true var_indexes offset var in
  let val_var = D.generate_variable var_indexes offset var in
  let locals, def, value = D.build_expression se in
  if D.is_always_true def then
    Format.fprintf oc "%a%a@,@[<v 2>{@,%a@,@]}" D.format_local_declarations
      locals
      (D.format_assign dgfip_flags var_indexes def_var)
      def
      (D.format_assign dgfip_flags var_indexes val_var)
      value
  else
    Format.fprintf oc "%a%a@,@[<v 2>if(%s){@;%a@]@,}@,else %s = 0.;"
      D.format_local_declarations locals
      (D.format_assign dgfip_flags var_indexes def_var)
      def def_var
      (D.format_assign dgfip_flags var_indexes val_var)
      value val_var;
  if dgfip_flags.flg_trace then
    let var = Bir.var_to_mir var in
    Format.fprintf oc "@;aff2(\"%s\", irdata, %s);"
      (Pos.unmark var.Mir.Variable.name)
      (Dgfip_varid.gen_access_pos_from_start var_indexes var)

let generate_var_def (program : program) (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : variable) (def : variable_def)
    (fmt : Format.formatter) : unit =
  match def with
  | SimpleVar e ->
      let se = generate_c_expr program e var_indexes in
      if var.Bir.mir_var.Mir.is_it then (
        let pr form = Format.fprintf fmt form in
        pr "@[<v 2>{";
        let idx = fresh_c_local "idxPROUT" in
        pr "@;int %s;" idx;
        pr "@;@[<v 2>for(%s = 0; %s < %s; %s++) {" idx idx
          (Dgfip_varid.gen_size var_indexes var.Bir.mir_var)
          idx;
        pr "@;%a"
          (generate_m_assign dgfip_flags var_indexes var (GetValueExpr idx))
          se;
        pr "@]@;}";
        pr "@]@;}@;")
      else generate_m_assign dgfip_flags var_indexes var None fmt se
  | TableVar (_, IndexTable es) ->
      Mir.IndexMap.iter
        (fun i v ->
          let sv = generate_c_expr program v var_indexes in
          Format.fprintf fmt "@[<hov 2>{@,%a@]}@,"
            (generate_m_assign dgfip_flags var_indexes var (GetValueConst i))
            sv)
        es
  | TableVar (_size, IndexGeneric (v, e)) ->
      (* TODO: boundary checks *)
      let sv = generate_c_expr program e var_indexes in
      Format.fprintf fmt "if(%s)@[<hov 2>{%a@]@;}"
        (D.generate_variable var_indexes None ~def_flag:true v)
        (generate_m_assign dgfip_flags var_indexes var (GetValueVar v))
        sv
  | InputVar -> assert false

let generate_var_cond (program : program) (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (cond : condition_data)
    (oc : Format.formatter) =
  let econd = generate_c_expr program cond.cond_expr var_indexes in
  let locals, _def, value =
    D.build_expression
    @@ D.build_transitive_composition ~safe_def:true
         {
           def_test = D.dtrue;
           value_comp = D.dand econd.def_test econd.value_comp;
         }
  in
  let erreur = Pos.unmark (fst cond.cond_error).Mir.Error.name in
  let code =
    match snd cond.cond_error with
    | None -> "NULL"
    | Some v ->
        Format.sprintf "\"%s\""
          (Pos.unmark (Bir.var_to_mir v).Mir.Variable.name)
  in
  Format.fprintf oc "%a%a@,@[<v 2>if(cond){@," D.format_local_declarations
    locals
    (D.format_assign dgfip_flags var_indexes "cond")
    value;
  Format.fprintf oc "add_erreur(irdata, &erreur_%s, %s);@]@,}" erreur code

let rec generate_stmt (dgfip_flags : Dgfip_options.flags) (program : program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter) (stmt : stmt)
    =
  match Pos.unmark stmt with
  | SAssign (var, vdata) ->
      Format.fprintf oc "@[<v 2>{@,";
      generate_var_def program dgfip_flags var_indexes var vdata oc;
      Format.fprintf oc "@]@,}"
  | SConditional (cond, iftrue, iffalse) ->
      Format.fprintf oc "@[<v 2>{@,";
      let cond_val = fresh_c_local "mpp_cond" in
      let cond_def = cond_val ^ "_d" in
      let locals, def, value =
        D.build_expression
        @@ generate_c_expr program (Pos.same_pos_as cond stmt) var_indexes
      in
      Format.fprintf oc "char %s;@;double %s;@;%a%a@;%a" cond_def cond_val
        D.format_local_declarations locals
        (D.format_assign dgfip_flags var_indexes cond_def)
        def
        (D.format_assign dgfip_flags var_indexes cond_val)
        value;
      Format.fprintf oc "@[<hov 2>if(%s && %s){@,%a@]@,}" cond_def cond_val
        (generate_stmts dgfip_flags program var_indexes)
        iftrue;
      if iffalse <> [] then
        Format.fprintf oc "@[<hov 2>else if(%s){@,%a@]@,}" cond_def
          (generate_stmts dgfip_flags program var_indexes)
          iffalse;
      Format.fprintf oc "@]@,}"
  | SVerif v -> generate_var_cond program dgfip_flags var_indexes v oc
  | SVerifBlock stmts ->
      let goto_label = fresh_c_local "verif_block" in
      let pr fmt = Format.fprintf oc fmt in
      pr "@[<v 2>{@\n";
      pr "#ifdef FLG_MULTITHREAD@\n";
      pr "  init_erreur(irdata);@\n";
      pr "  if (setjmp(irdata->jmp_bloq) != 0) {@\n";
      pr "    goto %s;@\n" goto_label;
      pr "  }@\n";
      pr "#else@\n";
      pr "  init_erreur();@\n";
      pr "  if (setjmp(jmp_bloq) != 0) {@\n";
      pr "    goto %s;@\n" goto_label;
      pr "  }@\n";
      pr "#endif@\n";
      pr "%a@\n" (generate_stmts dgfip_flags program var_indexes) stmts;
      pr "%s:;@]@\n}@\n" goto_label
  | SRovCall r ->
      let rov = ROVMap.find r program.rules_and_verifs in
      generate_rov_function_header ~definition:false oc rov
  | SFunctionCall (f, _) -> Format.fprintf oc "%s(irdata);" f
  | SPrint (std, args) ->
      let print_std =
        match std with Mast.StdOut -> "stdout" | Mast.StdErr -> "stderr"
      in
      let print_val = fresh_c_local "mpp_print" in
      let print_def = print_val ^ "_d" in
      Format.fprintf oc "@[<v 2>{@,char %s;@;double %s;@;" print_def print_val;
      List.iter
        (function
          | Mir.PrintString s ->
              Format.fprintf oc "fprintf(%s, \"%s\");@;" print_std
                (str_escape s)
          | Mir.PrintName (_, var) -> begin
              match Mir.VariableMap.find var var_indexes with
              | Dgfip_varid.VarIterate (t, _, _) ->
                  Format.fprintf oc "fprintf(%s, \"%%s\", %s->name);@;"
                    print_std t
              | _ -> assert false
            end
          | Mir.PrintAlias (_, var) -> begin
              match Mir.VariableMap.find var var_indexes with
              | Dgfip_varid.VarIterate (t, _, _) ->
                  Format.fprintf oc "fprintf(%s, \"%%s\", %s->alias);@;"
                    print_std t
              | _ -> assert false
            end
          | Mir.PrintExpr (e, min, max) ->
              let locals, def, value =
                D.build_expression @@ generate_c_expr program e var_indexes
              in
              Format.fprintf oc "@[<v 2>{%a%a@;%a@;@]}@;"
                D.format_local_declarations locals
                (D.format_assign dgfip_flags var_indexes print_def)
                def
                (D.format_assign dgfip_flags var_indexes print_val)
                value;
              Format.fprintf oc "@[<v 2>if(%s){@;" print_def;
              Format.fprintf oc "print_double(%s, %s, %d, %d);@]@;" print_std
                print_val min max;
              Format.fprintf oc "@[<v 2>} else {@;";
              Format.fprintf oc "fprintf(%s, \"indefini\");@]@;}@;" print_std)
        args;
      Format.fprintf oc "@]@;}@;"
  | SIterate (var, vcs, expr, stmts) ->
      let it_name = fresh_c_local "iterate" in
      Mir.CatVarSet.iter
        (fun vc ->
          let vcd =
            Mir.CatVarMap.find vc program.mir_program.Mir.program_var_categories
          in
          let var_indexes =
            Mir.VariableMap.add var.mir_var
              (Dgfip_varid.VarIterate ("tab_" ^ it_name, vcd.Mir.loc, vcd))
              var_indexes
          in
          Format.fprintf oc "@[<v 2>{@;";
          Format.fprintf oc
            "T_varinfo_%s *tab_%s = varinfo_%s;@;int nb_%s = 0;@;"
            vcd.Mir.id_str it_name vcd.Mir.id_str it_name;
          Format.fprintf oc "@[<v 2>while (nb_%s < NB_%s) {@;" it_name
            vcd.Mir.id_str;
          let cond_val = "cond_" ^ it_name in
          let cond_def = cond_val ^ "_d" in
          let locals, def, value =
            D.build_expression
            @@ generate_c_expr program (Pos.same_pos_as expr stmt) var_indexes
          in
          Format.fprintf oc "char %s;@;double %s;@;@[<v 2>{@;%a%a@;%a@]@;}@;"
            cond_def cond_val D.format_local_declarations locals
            (D.format_assign dgfip_flags var_indexes cond_def)
            def
            (D.format_assign dgfip_flags var_indexes cond_val)
            value;
          Format.fprintf oc "@[<hov 2>if(%s && %s){@;%a@]@;}@;" cond_def
            cond_val
            (generate_stmts dgfip_flags program var_indexes)
            stmts;
          Format.fprintf oc "tab_%s++;@;nb_%s++;@;" it_name it_name;
          Format.fprintf oc "@]}@;";
          Format.fprintf oc "@]}@;")
        vcs
  | SRestore (vars, var_params, stmts) ->
      Format.fprintf oc "@[<v 2>{@;";
      let rest_name = fresh_c_local "restore" in
      Format.fprintf oc "T_env_sauvegarde %s = NULL;@;" rest_name;
      Bir.VariableSet.iter
        (fun v ->
          Format.fprintf oc "env_sauvegarder(&%s, %s, %s, %s);@;" rest_name
            (Dgfip_varid.gen_access_def_pointer var_indexes v.mir_var)
            (Dgfip_varid.gen_access_pointer var_indexes v.mir_var)
            (Dgfip_varid.gen_size var_indexes v.mir_var))
        vars;
      List.iter
        (fun (var, vcs, expr) ->
          let it_name = fresh_c_local "iterate" in
          Mir.CatVarSet.iter
            (fun vc ->
              let vcd =
                Mir.CatVarMap.find vc
                  program.mir_program.Mir.program_var_categories
              in
              let var_indexes =
                Mir.VariableMap.add var.mir_var
                  (Dgfip_varid.VarIterate ("tab_" ^ it_name, vcd.Mir.loc, vcd))
                  var_indexes
              in
              Format.fprintf oc "@[<v 2>{@;";
              Format.fprintf oc
                "T_varinfo_%s *tab_%s = varinfo_%s;@;int nb_%s = 0;@;"
                vcd.Mir.id_str it_name vcd.Mir.id_str it_name;
              Format.fprintf oc "@[<v 2>while (nb_%s < NB_%s) {@;" it_name
                vcd.Mir.id_str;
              let cond_val = "cond_" ^ it_name in
              let cond_def = cond_val ^ "_d" in
              let locals, def, value =
                D.build_expression
                @@ generate_c_expr program
                     (Pos.same_pos_as expr stmt)
                     var_indexes
              in
              Format.fprintf oc
                "char %s;@;double %s;@;@[<v 2>{@;%a%a@;%a@]@;}@;" cond_def
                cond_val D.format_local_declarations locals
                (D.format_assign dgfip_flags var_indexes cond_def)
                def
                (D.format_assign dgfip_flags var_indexes cond_val)
                value;
              Format.fprintf oc "@[<hov 2>if(%s && %s){@;" cond_def cond_val;
              Format.fprintf oc "env_sauvegarder(&%s, %s, %s, %s);@;" rest_name
                (Dgfip_varid.gen_access_def_pointer var_indexes var.mir_var)
                (Dgfip_varid.gen_access_pointer var_indexes var.mir_var)
                (Dgfip_varid.gen_size var_indexes var.mir_var);
              Format.fprintf oc "@]@;}@;";
              Format.fprintf oc "tab_%s++;@;nb_%s++;@;" it_name it_name;
              Format.fprintf oc "@]}@;";
              Format.fprintf oc "@]}@;")
            vcs)
        var_params;
      Format.fprintf oc "%a@;"
        (generate_stmts dgfip_flags program var_indexes)
        stmts;
      Format.fprintf oc "env_restaurer(&%s);@;" rest_name;
      Format.fprintf oc "@]}@;"
  | SRaiseError (err, var_opt) ->
      let err_name = Pos.unmark err.Mir.Error.name in
      let code =
        match var_opt with
        | Some var -> Format.sprintf "\"%s\"" var
        | None -> "NULL"
      in
      Format.fprintf oc "add_erreur(irdata, &erreur_%s, %s);@;" err_name code
  | SCleanErrors -> Format.fprintf oc "nettoie_erreur(irdata);@;"

and generate_stmts (dgfip_flags : Dgfip_options.flags) (program : program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (stmts : stmt list) =
  Format.fprintf oc "@[<v>";
  Format.pp_print_list (generate_stmt dgfip_flags program var_indexes) oc stmts;
  Format.fprintf oc "@]"

and generate_rov_function_header ~(definition : bool) (oc : Format.formatter)
    (rov : rule_or_verif) =
  let arg_type = if definition then "T_irdata *" else "" in
  let tname, ret_type =
    match rov.rov_code with
    | Rule _ -> ("regle", "int ")
    | Verif _ -> ("verif", "void ")
  in
  let ret_type = if definition then ret_type else "" in
  Format.fprintf oc "%s%s_%s(%sirdata)%s" ret_type tname
    (Pos.unmark rov.rov_name) arg_type
    (if definition then "" else ";")

let generate_rov_function (dgfip_flags : Dgfip_options.flags)
    (program : program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) (rov : rule_or_verif) =
  let decl, ret =
    let noprint _ _ = () in
    match rov.rov_code with
    | Rule _ -> (noprint, fun fmt () -> Format.fprintf fmt "@,return 0;")
    | Verif _ ->
        ((fun fmt () -> Format.fprintf fmt "register char cond;@;"), noprint)
  in
  Format.fprintf oc "@[<v 2>%a{@,%a%a%a@]@,}"
    (generate_rov_function_header ~definition:true)
    rov decl ()
    (generate_stmts (dgfip_flags : Dgfip_options.flags) program var_indexes)
    (Bir.rule_or_verif_as_statements rov)
    ret ()

let generate_rov_functions (dgfip_flags : Dgfip_options.flags)
    (program : program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) (rovs : rule_or_verif list) =
  Format.fprintf oc "@[<v>";
  Format.pp_print_list ~pp_sep:Format.pp_print_cut
    (generate_rov_function
       (dgfip_flags : Dgfip_options.flags)
       program var_indexes)
    oc rovs;
  Format.fprintf oc "@]"

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc
    {|
/* %s */

#ifndef IR_HEADER_
#define IR_HEADER_

#include <stdio.h>

#include "irdata.h"
#include "const.h"
#include "var.h"

#ifndef FLG_MULTITHREAD
#define add_erreur(a,b,c) add_erreur(b,c)
#endif

|}
    Prelude.message

let generate_footer (oc : Format.formatter) () : unit =
  Format.fprintf oc "\n#endif /* IR_HEADER_ */"

let generate_target_protoype (add_semicolon : bool) (return_type : bool)
    (oc : Format.formatter) (function_name : string) =
  let ret_type = if return_type then "struct S_discord *" else "void" in
  Format.fprintf oc "%s %s(T_irdata* irdata)%s" ret_type function_name
    (if add_semicolon then ";" else "")

let generate_var_tmp_decls (oc : Format.formatter)
    (tmp_vars : (Bir.variable * Pos.t * int option) StrMap.t) =
  StrMap.iter
    (fun vn (_, _, size) ->
      let sz = match size with Some i -> i | None -> 1 in
      Format.fprintf oc "char %s_def[%d];@,double %s_val[%d];@," vn sz vn sz)
    tmp_vars;
  if not (StrMap.is_empty tmp_vars) then Format.fprintf oc "@,";
  StrMap.iter
    (fun vn (_, _, size) ->
      match size with
      | Some 1 | None ->
          Format.fprintf oc "%s_def[0] = 0;@,%s_val[0] = 0.0;@," vn vn
      | Some i ->
          Format.fprintf oc "@[<v 2>{@;";
          Format.fprintf oc "int i;@;";
          Format.fprintf oc "for (i = 0; i < %d; i++) {@;" i;
          Format.fprintf oc "%s_def[i] = 0;@,%s_val[i] = 0.0;@," vn vn;
          Format.fprintf oc "@]@;}@;";
          Format.fprintf oc "@]@;}@;")
    tmp_vars;
  if not (StrMap.is_empty tmp_vars) then Format.fprintf oc "@,"

let generate_target (dgfip_flags : Dgfip_options.flags) (program : Bir.program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    ((f, ret_type) : Bir.function_name * bool) =
  let { tmp_vars; stmts; is_verif } = Mir.TargetMap.find f program.targets in
  Format.fprintf oc "@[<v 2>%a{@,%a%a%s@]@,}@,"
    (generate_target_protoype false is_verif)
    f generate_var_tmp_decls tmp_vars
    (generate_stmts dgfip_flags program var_indexes)
    stmts
    (if ret_type then
     {|
#ifdef FLG_MULTITHREAD
      return irdata->discords;
#else
      return discords;
#endif
|}
    else "")

let generate_targets (dgfip_flags : Dgfip_options.flags) (program : Bir.program)
    (oc : Format.formatter) (var_indexes : Dgfip_varid.var_id_map) =
  let targets = Mir.TargetMap.bindings program.Bir.targets in
  List.iter
    (fun (name, { is_verif; _ }) ->
      generate_target
        (dgfip_flags : Dgfip_options.flags)
        program var_indexes oc (name, is_verif))
    targets

let generate_targets_signatures (oc : Format.formatter) (program : Bir.program)
    =
  let targets = Mir.TargetMap.bindings program.Bir.targets in
  Format.fprintf oc "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun ppf (name, { is_verif; _ }) ->
         generate_target_protoype true is_verif ppf name))
    targets

let generate_mpp_function_protoype (add_semicolon : bool) (return_type : bool)
    (oc : Format.formatter) (function_name : Bir.function_name) =
  let ret_type = if return_type then "struct S_discord *" else "void" in
  Format.fprintf oc "%s %s(T_irdata* irdata)%s" ret_type function_name
    (if add_semicolon then ";" else "")

let generate_mpp_function (dgfip_flags : Dgfip_options.flags)
    (program : Bir.program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) ((f, ret_type) : Bir.function_name * bool) =
  let { mppf_stmts; mppf_is_verif } =
    Bir.FunctionMap.find f program.mpp_functions
  in
  Format.fprintf oc "@[<v 2>%a{@,%a%s@]@,}@,"
    (generate_mpp_function_protoype false mppf_is_verif)
    f
    (generate_stmts dgfip_flags program var_indexes)
    mppf_stmts
    (if ret_type then
     {|
#ifdef FLG_MULTITHREAD
      return irdata->discords;
#else
      return discords;
#endif
|}
    else "")

let generate_mpp_functions (dgfip_flags : Dgfip_options.flags)
    (program : Bir.program) (oc : Format.formatter)
    (var_indexes : Dgfip_varid.var_id_map) =
  let funcs = Bir.FunctionMap.bindings program.Bir.mpp_functions in
  List.iter
    (fun (fname, { mppf_is_verif; _ }) ->
      generate_mpp_function
        (dgfip_flags : Dgfip_options.flags)
        program var_indexes oc (fname, mppf_is_verif))
    funcs

let generate_mpp_functions_signatures (oc : Format.formatter)
    (program : Bir.program) =
  let funcs = Bir.FunctionMap.bindings program.Bir.mpp_functions in
  Format.fprintf oc "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun ppf (func, { mppf_is_verif; _ }) ->
         generate_mpp_function_protoype true mppf_is_verif ppf func))
    funcs

let generate_rovs_files (dgfip_flags : Dgfip_options.flags) (program : program)
    (folder : string) (vm : Dgfip_varid.var_id_map) =
  let default_file = "default" in
  let filemap =
    ROVMap.fold
      (fun _rov_id rov filemap ->
        let file =
          let pos = Pos.get_position rov.rov_name in
          if pos = Pos.no_pos then default_file
          else
            (Pos.get_file pos |> Filename.basename |> Filename.remove_extension)
            ^ ".c"
        in
        let filerovs =
          match StrMap.find_opt file filemap with None -> [] | Some fr -> fr
        in
        StrMap.add file (rov :: filerovs) filemap)
      program.rules_and_verifs StrMap.empty
  in
  StrMap.fold
    (fun file rovs orphan ->
      if String.equal file default_file then rovs @ orphan
      else
        let oc = open_out (Filename.concat folder file) in
        let fmt = Format.formatter_of_out_channel oc in
        Format.fprintf fmt
          {|
#include <math.h>
#include <stdio.h>
#include "var.h"

#ifndef FLG_MULTITHREAD
#define add_erreur(a,b,c) add_erreur(b,c)
#endif

|};
        generate_rov_functions dgfip_flags program vm fmt rovs;
        Format.fprintf fmt "@\n@.";
        close_out oc;
        orphan)
    filemap []

let generate_implem_header oc header_filename =
  Format.fprintf oc
    {|
/* %s */

#include <string.h>
#include "enchain_static.c.inc"

#include "%s"


|}
    Prelude.message header_filename

let generate_c_program (dgfip_flags: Dgfip_options.flags) (program : program)
    (_function_spec : Bir_interface.bir_function) (filename : string)
    (vm : Dgfip_varid.var_id_map) : unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)"
         filename);
  let folder = Filename.dirname filename in
  let orphan_rovs = generate_rovs_files dgfip_flags program folder vm in
  let header_filename = Filename.remove_extension filename ^ ".h" in
  let _oc = open_out header_filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a@\n@."
    generate_header ()
    generate_targets_signatures program
    generate_mpp_functions_signatures program
    generate_footer ();
  close_out _oc;
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a@\n@."
    generate_implem_header (Filename.basename header_filename)
    (generate_rov_functions dgfip_flags program vm) orphan_rovs
    (generate_targets dgfip_flags program) vm
    (generate_mpp_functions dgfip_flags program) vm;
  close_out _oc[@@ocamlformat "disable"]

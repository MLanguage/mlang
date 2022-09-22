(*
  Copyright DGFiP, 20 septembre 2022.
  Contributeur: David MICHEL <david.michel1@dgfip.finances.gouv.fr>.

  Ce logiciel est régi par la licence CeCILL soumise au droit français et
  respectant les principes de diffusion des logiciels libres. Vous pouvez
  utiliser, modifier et/ou redistribuer ce programme sous les conditions de la
  licence CeCILL telle que diffusée par le CEA, le CNRS et l'INRIA sur le site
  "http://www.cecill.info".

  Le fait que vous puissiez accéder à cet en-tête signifie que vous avez pris
  connaissance de la licence CeCILL, et que vous en avez accepté les termes.
*)

type input_category = Context | Family | Penality | Income | CorrIncome

type variable =
| Input of {
    name : string;
    alias : string;
    size : int;
    category : input_category;
    is_output : bool;
}
| Calculated of {
    name : string;
    size : int;
    is_output : bool;
}
| Base of {
    name : string;
    size : int;
    is_output : bool;
}

type error_category = Anomaly | Discordance | Information

type error = Error of {
  name : string;
  category : error_category;
  family : string;
  bo_code : string;
  sub_bo_code : string;
  label : string;
  isf_error : string
}

type expr =
| Undef
| Float of float
| Var of variable
| Get of variable * expr
| LocalVar of int
| Let of int * expr * expr
| If of expr * expr * expr
| Not of expr
| Minus of expr
| Gt of expr * expr
| Gte of expr * expr
| Lt of expr * expr
| Lte of expr * expr
| Eq of expr * expr
| Neq of expr * expr
| And of expr * expr
| Or of expr * expr
| Add of expr * expr
| Sub of expr * expr
| Mul of expr * expr
| Div of expr * expr
| Sum of expr list
| Abs of expr
| Min of expr list
| Max of expr list
| Gtz of expr
| Gtez of expr
| Null of expr
| Arr of expr
| Inf of expr list
| Sup of expr list
| Present of expr
| Multimax of expr list
| Supzero of expr
| Call of string * (expr list)
| Abort of error

let pr_input_category ppf = function
| Context -> Format.fprintf ppf "contexte"
| Family -> Format.fprintf ppf "famille"
| Penality -> Format.fprintf ppf "penalite"
| Income -> Format.fprintf ppf "revenu"
| CorrIncome -> Format.fprintf ppf "revenu corrective"

let pr_variable ppf var = 
  let pr_size ppf i = if i > 1 then Format.fprintf ppf "[%d]" i in
  match var with
  | Input v ->
      Format.fprintf
	ppf "%s%a ~ %s: saisie %a%s"
	v.name
        pr_size v.size
	v.alias
	pr_input_category v.category
	(if v.is_output then " restituee" else "")
  | Calculated v ->
      Format.fprintf
	ppf "%s%a: calculee%s"
	v.name
        pr_size v.size
	(if v.is_output then " restituee" else "")
  | Base v ->
      Format.fprintf
	ppf "%s%a: calculee base%s"
	v.name
        pr_size v.size
	(if v.is_output then " restituee" else "")

let pr_error_category ppf = function
| Anomaly -> Format.fprintf ppf "anomalie"
| Discordance -> Format.fprintf ppf "discordance"
| Information -> Format.fprintf ppf "information"

let pr_error ppf = function
| Error e ->
    Format.fprintf
      ppf "%s: %a %s %s %s \"%s\" %s"
      e.name
      pr_error_category e.category
      e.family e.bo_code e.sub_bo_code e.label e.isf_error

let vars_from_mast mast =
  let rec gen_var ret = function
  | [] -> ret
  | mv :: mvl -> begin
      match Pos.unmark mv with
      | Mast.VariableDecl (Mast.InputVar mvd) ->
	  let vd = Pos.unmark mvd in
	  let var = Input {
            name = Pos.unmark vd.Mast.input_name;
            alias = Pos.unmark vd.Mast.input_alias;
            size = 1;
            category = begin
	      match Pos.unmark vd.Mast.input_subtyp with
	      | Mast.Context -> Context
	      | Mast.Family -> Family
	      | Mast.Penality -> Penality
	      | Mast.Income -> Income
	    end;
            is_output = vd.Mast.input_given_back
          } in
          gen_var (var :: ret) mvl
      | Mast.VariableDecl (Mast.ComputedVar mvd) ->
	  let vd = Pos.unmark mvd in
	  let n = Pos.unmark vd.Mast.comp_name in
	  let s =
	    match vd.Mast.comp_table with
	    | Some md -> Pos.unmark md
	    | None -> 1
	  in
	  let typs = List.map Pos.unmark vd.Mast.comp_subtyp in
	  let base = List.mem Mast.Base typs in
	  let rest = List.mem Mast.GivenBack typs in
          let var =
            if base then
	      Base { name = n ; size = s ; is_output = rest }
	    else
	      Calculated { name = n ; size = s ; is_output = rest }
          in
          gen_var (var :: ret) mvl
      | Mast.Output mvn -> failwith "Mast.Output"
      | _ -> gen_var ret mvl
    end
  in
  gen_var [] (List.flatten mast)

let errs_from_mast mast =
  let rec gen_err ret = function
  | [] -> ret
  | me :: mel ->
      match Pos.unmark me with
      | Mast.Error e ->
	  let cat =
	    match Pos.unmark e.Mast.error_typ with
	    | Mast.Anomaly -> Anomaly
	    | Mast.Discordance -> Discordance
	    | Mast.Information -> Information
	  in
          let next l =
            match l with
            | [] -> "", []
            | hd :: tl -> Pos.unmark hd, tl
          in
          let f, fl = next e.Mast.error_descr in
          let c, cl = next fl in
          let sc, scl = next cl in
          let l, ll = next scl in
          let ie, _ = next ll in
	  let err = Error {
	    name = Pos.unmark e.Mast.error_name;
	    category = cat;
	    family = f;
	    bo_code = c;
	    sub_bo_code = sc;
	    label = l;
	    isf_error = ie;
	  } in
          gen_err (err :: ret) mel
      | _ -> gen_err ret mel
  in
  gen_err [] (List.flatten mast)

type program = Truc of int

let pr_boxed b f ppf a =
  Format.fprintf ppf "@[<%s>%a@]" b f a

let pr_vars ppf mast =
  Format.fprintf ppf "@[<v 2>variables:@;%a@]@;@\n"
    (Format.pp_print_list (pr_boxed "h" pr_variable)) (vars_from_mast mast)

let pr_errs ppf mast =
  Format.fprintf ppf "@[<v 2>erreurs:@;%a@]@;@\n"
    (Format.pp_print_list (pr_boxed "h" pr_error)) (errs_from_mast mast)

let rec pr_expr ppf = function
| Mir.Unop (op, e0) ->
    let s_op = (match op with Mast.Not -> "not" | Mast.Minus -> "minus") in
    Format.fprintf ppf "(%s %a)" s_op pr_expr (Pos.unmark e0)
| Mir.Comparison (op, e0, e1) ->
    let s_op = 
      match Pos.unmark op with
      | Mast.Gt -> "gt"
      | Mast.Gte -> "gte"
      | Mast.Lt -> "lt"
      | Mast.Lte -> "lte"
      | Mast.Eq -> "eq"
      | Mast.Neq -> "neq"
    in
    Format.fprintf
      ppf "(%s %a %a)" s_op
      pr_expr (Pos.unmark e0)
      pr_expr (Pos.unmark e1)
| Mir.Binop (op, e0, e1) ->
    let s_op = 
      match Pos.unmark op with
      | Mast.And -> "and"
      | Mast.Or -> "or"
      | Mast.Add -> "add"
      | Mast.Sub -> "sub"
      | Mast.Mul -> "mul"
      | Mast.Div -> "div"
    in
    Format.fprintf
      ppf "(%s %a %a)" s_op
      pr_expr (Pos.unmark e0)
      pr_expr (Pos.unmark e1)
| Index (v, e0) ->
    let s_v = (Pos.unmark v).Bir.on_tgv in
    Format.fprintf ppf "%s[%a]" s_v pr_expr (Pos.unmark e0)
| Conditional (e0, e1, e2) ->
    Format.fprintf
     ppf "(if %a then %a else %a)"
     pr_expr (Pos.unmark e0)
     pr_expr (Pos.unmark e1)
     pr_expr (Pos.unmark e2)
| FunctionCall (f, el) ->
    let s_f = 
      match f with
      | Mir.SumFunc -> "sum"
      | Mir.AbsFunc -> "abs"
      | Mir.MinFunc -> "min"
      | Mir.MaxFunc -> "max"
      | Mir.GtzFunc -> "positif"
      | Mir.GtezFunc -> "positif_ou_nul"
      | Mir.NullFunc -> "null"
      | Mir.ArrFunc -> "arr"
      | Mir.InfFunc -> "inf"
      | Mir.PresentFunc -> "present"
      | Mir.Multimax -> "multimax"
      | Mir.Supzero -> "supzero"
    in
    let pr ppf e = pr_expr ppf (Pos.unmark e) in
  Format.fprintf ppf "(%s %a)" s_f (Format.pp_print_list pr) el
| Literal l -> begin
    match l with
    | Mir.Float r -> Format.fprintf ppf "%g" r
    | Mir.Undefined -> Format.fprintf ppf "indef"
  end
| Var v -> Format.fprintf ppf "%s" (Pos.unmark v.Bir.mir_var.Mir.name)
| LocalVar lv -> Format.fprintf ppf "$%d" lv.Mir.id
| Error -> Format.fprintf ppf "error"
| LocalLet (lv, e0, e1) ->
    Format.fprintf
      ppf "(let loc$%d = %a in %a)" lv.Mir.id
      pr_expr (Pos.unmark e0)
      pr_expr (Pos.unmark e1)

let pr_rov_id ppf = function
| Mir.RuleID id -> Format.fprintf ppf "regle %d" id
| Mir.VerifID id -> Format.fprintf ppf "verif %d" id

let rec pr_stmt_kind ppf = function
| Bir.SAssign (v, vd) ->
  let pr_var ppf = function
  | Mir.SimpleVar me -> pr_expr ppf (Pos.unmark me)
  | Mir.TableVar (i, id) ->
      let s_id =
	match id with
	| Mir.IndexTable _ -> "indexTable"
	| Mir.IndexGeneric _ -> "indexGeneric" 
     in
     Format.fprintf ppf "tableau %d %s" i s_id
  | Mir.InputVar -> Format.fprintf ppf "entree"
  in
  Format.fprintf ppf "%s := %a"
    (Pos.unmark v.Bir.mir_var.Mir.name)
    pr_var (vd.Mir.var_definition)
| Bir.SConditional (e, sl0, sl1) ->
  Format.fprintf
    ppf "if %a then %a else %a"
    pr_expr e
    (Format.pp_print_list pr_stmt) sl0
    (Format.pp_print_list pr_stmt) sl1
| Bir.SVerif cd ->
  Format.fprintf
    ppf "si %a@ alors erreur %s"
    pr_expr (Pos.unmark cd.Mir.cond_expr)
    (Pos.unmark (fst cd.Mir.cond_error).Mir.Error.name)
| Bir.SRovCall rid -> Format.fprintf ppf "%a" pr_rov_id rid
| Bir.SFunctionCall (fn, vl) ->
  let pr_v ppf v =
    Format.fprintf ppf "%s" (Pos.unmark v.Mir.Variable.name) in
  Format.fprintf
    ppf "call %s(%a)"
    fn
    (Format.pp_print_list pr_v) vl
and pr_stmt ppf s = pr_stmt_kind ppf (Pos.unmark s)

let pr_rovs ppf rovs =
  let pr_rov_code ppf = function
  | Bir.Rule sl ->
      Format.fprintf ppf "%a" (Format.pp_print_list (pr_boxed "h" pr_stmt)) sl
  | Bir.Verif s -> Format.fprintf ppf "%a" (pr_boxed "h" pr_stmt) s
  in
  let form ppf rid rov =
    Format.fprintf
      ppf "@[<v 2>%a:@;%a@]@;@\n"
      pr_rov_id rid
      pr_rov_code rov.Bir.rov_code
  in
  Bir.ROVMap.iter (form ppf) rovs

let pr_mpp (oc : Format.formatter) (program : Bir.program) =
(*  let funcs = Bir_interface.context_agnostic_mpp_functions program in *)
  let funcs = program.Bir.mpp_functions in
  let form ppf f prg =
    let ret = if prg.Bir.mppf_is_verif then "verif" else "calc" in
    Format.fprintf
      ppf "@[<v 2>%s %s:@;%a@]@;@\n"
      ret f
      (Format.pp_print_list (pr_boxed "h" pr_stmt)) prg.Bir.mppf_stmts
  in
  Bir.FunctionMap.iter (form oc) funcs

let bir_to_cgir
  (mast : Mast.program)
  (bir : Bir.program)
  (_function_spec : Bir_interface.bir_function)
  (oc : Format.formatter)
  (_ : unit)
: unit =
  ignore _function_spec;
  pr_vars oc mast;
  pr_errs oc mast;
  pr_rovs oc bir.Bir.rules_and_verifs;
  pr_mpp oc bir


module Table = Table_obj
module Type = Type_checker
module Sym = Symbol_table

open Str

(* ------------------------- APPEND HELPERS --------------------------------- *)

let create_idx_string pos = "[" ^ string_of_int pos ^ "]"

(* returns [][]  or [1][2] *)
let rec arrtype_dim_string dim nums is_array = match dim with
    | 0 -> ""
    | _ ->
        begin
        if is_array then (
            try
                "[" ^ (string_of_int (List.hd nums)) ^ "]" ^ (arrtype_dim_string (dim-1) (List.tl nums) is_array)
            with _ as e ->
                print_string "passed param is_array=true to arrtype_dim_string but nums arg is either not a list, or isn't a list composed of ints\n";
                raise e
        )
        else
            "[" ^ "]" ^ (arrtype_dim_string (dim-1) nums is_array)
        end

let isAppend exp =
    match exp with
    | Ast.Append (_,_) -> true
    | _ -> false

(* ------------------------- END APPEND HELPERS ----------------------------- *)
let isBitAndNot exp =
    match exp with
    | Ast.BitAndNot -> true
    | _ -> false

let true_if_cap exp =
    match exp with
    | Ast.Cap(_) -> true
    | Ast.Len(_) -> false
    | _ -> (print_string "Only meant to be used in code_exp where inp is either len or cap\n"; false)

let isIndex = function
    | Ast.Index(_,_) -> true
    | _ -> false

let idx_var_name = function
    | Ast.Index(name,_) -> name
    | _ -> (print_string ("Only meant to be called after checking isIndex!\n"); "")

let indices_used = function
    | Ast.Index(_,el) -> el
    | _ -> (print_string ("Only meant to be called after checking isIndex!\n"); [])

let get_func_name = function
    | Ast.Ident v -> v


let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None)

let add_slice_class oc =
    let project_root = Gen_name_maker.path_to_project_root () in
    let in_channel = open_in (project_root ^ "/src/Slice.txt") in
    try
        Stream.iter (fun line -> Printf.fprintf oc "%s\n" line) (line_stream_of_channel in_channel);
        close_in in_channel
    with e ->
        close_in in_channel;
        raise e
    Printf.fprintf oc "\n"; ()



let rec codeProg p oc fname = match p with
(* main entry point for generation *)
  | [] -> ()
  | h::t -> 
      Printf.fprintf oc "import java.util.Arrays;";
      add_slice_class oc;
      Printf.fprintf oc "\n\nclass %s {\n\n" fname;
      tab_levels := !tab_levels + 1; code_stmnts (h::t) (Table.initialize ()) oc; Printf.fprintf oc "\n%s\n}" (tabs !tab_levels)


and code_binop op exp0 exp1 st oc = match op with
  | Ast.Add           -> code_exp exp0 st oc; Printf.fprintf oc " + "; code_exp exp1 st oc;
  | Ast.Sub           -> code_exp exp0 st oc; Printf.fprintf oc " - "; code_exp exp1 st oc;
  | Ast.Mult          -> code_exp exp0 st oc; Printf.fprintf oc " * "; code_exp exp1 st oc;
  | Ast.Mod           -> code_exp exp0 st oc; (let m = '%' in Printf.fprintf oc " %c " m); code_exp exp1 st oc;
  | Ast.Div           -> code_exp exp0 st oc; Printf.fprintf oc " / "; code_exp exp1 st oc;
  | Ast.Lt            -> (match get_underlying_type (Type.typeFromExp Type.Null exp0 st false true) with
                            | Table.String -> code_exp exp0 st oc; Printf.fprintf oc ".compareTo("; code_exp exp1 st oc; Printf.fprintf oc ") < 0";
                            | _ -> code_exp exp0 st oc; Printf.fprintf oc " < "; code_exp exp1 st oc;)
  | Ast.Leq           -> (match get_underlying_type (Type.typeFromExp Type.Null exp0 st false true) with
                            | Table.String -> code_exp exp0 st oc; Printf.fprintf oc ".compareTo("; code_exp exp1 st oc; Printf.fprintf oc ") <= 0";
                            | _ -> code_exp exp0 st oc; Printf.fprintf oc " <= "; code_exp exp1 st oc;)
  | Ast.Gt            -> (match get_underlying_type (Type.typeFromExp Type.Null exp0 st false true) with
                            | Table.String -> code_exp exp0 st oc; Printf.fprintf oc ".compareTo("; code_exp exp1 st oc; Printf.fprintf oc ") > 0";
                            | _ -> code_exp exp0 st oc; Printf.fprintf oc " > "; code_exp exp1 st oc;)
  | Ast.Geq           -> (match get_underlying_type (Type.typeFromExp Type.Null exp0 st false true) with
                            | Table.String -> code_exp exp0 st oc; Printf.fprintf oc ".compareTo("; code_exp exp1 st oc; Printf.fprintf oc ") >= 0";
                            | _ -> code_exp exp0 st oc; Printf.fprintf oc " >= "; code_exp exp1 st oc;)
  | Ast.Eq            -> (match get_underlying_type (Type.typeFromExp Type.Null exp0 st false true) with
                            | Table.String -> code_exp exp0 st oc; Printf.fprintf oc ".equals("; code_exp exp1 st oc; Printf.fprintf oc ")";
                            | Table.Array(_,_) -> Printf.fprintf oc "Arrays.equals("; code_exp exp0 st oc; Printf.fprintf oc ", "; code_exp exp1 st oc; Printf.fprintf oc ")";
                            | Table.Slice(_,_) -> code_exp exp0 st oc; Printf.fprintf oc ".equals("; code_exp exp1 st oc; Printf.fprintf oc ")";
                            | Table.Struct(_) -> code_exp exp0 st oc; Printf.fprintf oc ".equals("; code_exp exp1 st oc; Printf.fprintf oc ")";
                            | _ -> code_exp exp0 st oc; Printf.fprintf oc " == "; code_exp exp1 st oc;)
  | Ast.Neq           -> (match get_underlying_type (Type.typeFromExp Type.Null exp0 st false true) with
                            | Table.String -> Printf.fprintf oc "!"; code_exp exp0 st oc; Printf.fprintf oc ".equals("; code_exp exp1 st oc; Printf.fprintf oc ")";
                            | Table.Array(_,_) -> Printf.fprintf oc "!Arrays.equals("; code_exp exp0 st oc; Printf.fprintf oc ", "; code_exp exp1 st oc; Printf.fprintf oc ")";
                            | Table.Slice(_,_) -> Printf.fprintf oc "!"; code_exp exp0 st oc; Printf.fprintf oc ".equals("; code_exp exp1 st oc; Printf.fprintf oc ")";
                            | Table.Struct(_) -> Printf.fprintf oc "!"; code_exp exp0 st oc; Printf.fprintf oc ".equals("; code_exp exp1 st oc; Printf.fprintf oc ")";
                            | _ -> code_exp exp0 st oc; Printf.fprintf oc " != "; code_exp exp1 st oc;)
  | Ast.And           -> code_exp exp0 st oc; Printf.fprintf oc " && "; code_exp exp1 st oc;
  | Ast.Or            -> code_exp exp0 st oc; Printf.fprintf oc " || "; code_exp exp1 st oc;
  | Ast.BitAnd        -> code_exp exp0 st oc; Printf.fprintf oc " & "; code_exp exp1 st oc;
  | Ast.BitAndNot     -> code_exp exp0 st oc; Printf.fprintf oc " & ~"; code_exp exp1 st oc;
  | Ast.BitOr         -> code_exp exp0 st oc; Printf.fprintf oc " | "; code_exp exp1 st oc;
  | Ast.BitXorBin     -> code_exp exp0 st oc; Printf.fprintf oc " ^ "; code_exp exp1 st oc;
  | Ast.BitShiftLeft  -> code_exp exp0 st oc; Printf.fprintf oc " << "; code_exp exp1 st oc;
  | Ast.BitShiftRight -> code_exp exp0 st oc; Printf.fprintf oc " >> "; code_exp exp1 st oc;

and code_exp exp t oc = match exp with
  | Ast.Ident v -> code_ident t v false oc
  | Ast.StringLit s -> Printf.fprintf oc "%s" (Str.global_replace (Str.regexp "`") "\"" (String.escaped s))
  | Ast.String s -> Printf.fprintf oc "%s" s
  | Ast.Bool b -> if b then Printf.fprintf oc "true" else Printf.fprintf oc "false"
  | Ast.Rune s -> Printf.fprintf oc "%s" s
  | Ast.Int i -> Printf.fprintf oc "%d" i
  | Ast.Float f -> Printf.fprintf oc "%f" f
  | Ast.Index(name, indices) ->
    begin
        (* print_string "before get underlying type from typeFromExp in index\n"; *)
        let typ = get_underlying_type (Type.typeFromExp Type.Null (Ast.Ident name) t false true) in
        if (Type.isSlice typ) then (
            let tmp = get_underlying_type (Type.typeFromExp Type.Null (Ast.Index (name,indices)) t false true) in
            (* print_string ("name: " ^ name ^ ", ret of getatIndex is: " ^ tmp ^ "\n"); *)
            Printf.fprintf oc "((";
            code_type t false tmp oc;
            Printf.fprintf oc ")";
            code_ident t name false oc;
            Printf.fprintf oc ".getAtIndex(";
            let last_expr = (Gen_name_maker.get_last_elem indices) in
            code_exp last_expr t oc;
            Printf.fprintf oc "))";
        )
        else (
            (* code_ident t name false oc; *)
            (* Printf.fprintf oc "%s" (get_vals_in_index t oc name); *)
            gen_slice_arr_idx t oc name indices;
            let last_expr = (Gen_name_maker.get_last_elem indices) in
            Printf.fprintf oc "[";
            code_exp last_expr t oc;
            Printf.fprintf oc "]";
        )
    end
  | ((Ast.Cap arr) as inp)
  | ((Ast.Len arr) as inp) ->
    begin
        let typ = get_underlying_type (Type.typeFromExp Type.Null arr t false true) in
        code_exp arr t oc;
        if (Type.isSlice typ) then (
            if (true_if_cap inp) then Printf.fprintf oc ".cap"
            else Printf.fprintf oc ".len"
        )
        else (
            Printf.fprintf oc ".length";
            if ((Type.string_of_types typ) = "string") then Printf.fprintf oc "()";
        )
    end
  | Ast.Cast(too,from) ->
    begin
        let typ_too  = get_underlying_type (Type.typeFromExp Type.Null too t false true ) in
        let typ_from = get_underlying_type (Type.typeFromExp Type.Null from t false true) in

        (* in go, doing: string(-1) converts to UTF-8 or something weird, this additional cast replicates the behaviour *)
        if ((Type.string_of_types typ_too) = "string" && (Type.string_of_types typ_from) = "int") then (
            Printf.fprintf oc "(String.valueOf(";
            Printf.fprintf oc "(char)Integer.parseInt(\"\" + ";
            code_exp from t oc;
            Printf.fprintf oc ", 8)))";
        )
        else (
            match typ_too with
            | Table.Func(args,rt) ->
                begin
                    Printf.fprintf oc "%s" (gen_used_name t (get_func_name too) oc true);
                    ( match (Type.typeFromExp Type.Null from t false true) with
                        | Table.Array(_,dims) -> Printf.fprintf oc "(Arrays.copyOf(%s, %d))" (gen_used_name t (Type.nameFromVarUsage from) oc true) (List.hd dims)
                        | _ -> Printf.fprintf oc "(%s)" (gen_used_name t (Type.nameFromVarUsage from) oc true));
                    
(* and gen_used_name st i oc just_return = *)
                end
            | _ ->
                begin
                    Printf.fprintf oc "((";
                    code_type t false typ_too oc;
                    Printf.fprintf oc ") ";
                    code_exp from t oc;
                    Printf.fprintf oc ")";
                end
        )
    end
  | Ast.FuncCallIdent (expName, args, ret_idx) ->  if (Type.isType t (Type.nameFromVarUsage expName)) && ((List.length args) == 1) then code_exp (Ast.Cast(expName, (List.hd args))) t oc
  else
      begin
        code_exp expName t oc;
        Printf.fprintf oc "(";
        let print_arg a st oc = match a with
            | Ast.Ident v -> (match (Type.typeFromExp Type.Null a st false true) with
                                | Table.Array(_,dims) -> Printf.fprintf oc "Arrays.copyOf("; code_exp a st oc; Printf.fprintf oc ", %d)" (List.hd dims);
                                | _ -> code_exp a st oc)
            | _ -> code_exp a st oc
        in
        let rec print_args l = match l with
            | [] -> ()
            | h::[] -> print_arg h t oc
            | h::tl -> print_arg h t oc; Printf.fprintf oc ", "; print_args tl
        in
        print_args args;
        Printf.fprintf oc ")";
      end

      (* is_func_call *)
  (* (typeFromExp parent_id  t is_append) else *)
    (* checkFunc expName args ret_idx parent_id parent is_append *)
  | Ast.Append (arr,e) ->
          begin
            Printf.fprintf oc ".append(";
            code_exp arr t oc;
            Printf.fprintf oc ", ";
            code_exp e t oc;
            Printf.fprintf oc ")";
          end
  | Ast.BinOp (op, exp0, exp1) ->
        begin
          Printf.fprintf oc "(";
          code_binop op exp0 exp1 t oc;
          Printf.fprintf oc ")";
        end
  | Ast.UnaryOp (op, exp) ->
    begin
    ( match op with
      | Ast.UMinus   -> Printf.fprintf oc "-";
      | Ast.Not      -> Printf.fprintf oc "!";
      | Ast.BitXor   -> Printf.fprintf oc "~";
      | Ast.Identity -> ();
    );
      Printf.fprintf oc "(";
      code_exp exp t oc;
      Printf.fprintf oc ")";
    end
  | Ast.Parens exp ->
    begin
      Printf.fprintf oc "(";
      code_exp exp t oc;
      Printf.fprintf oc ")";
    end
  | Ast.ArrayField(var,fld) | Ast.Field(var,fld) ->
    begin
      code_exp var t oc;
      Printf.fprintf oc ".";
      code_exp fld t oc;
    end

and gen_used_name st i oc just_return =
    (* print_string "used name function\n\n"; *)
    let name_formats = [
        (Printf.sprintf "__goLite__%s_%i" i !tab_levels);
        (Printf.sprintf "__goLite__%s_%i" i (!tab_levels - (Table.getLevel st i 0)))
    ]
    in
    let used_name = try string_of_int (int_of_string i) with | Failure(_) -> (
        if (not(Table.isNewDecl st.Table.table (Table.Symbol(i, Table.Void))))
            then List.hd name_formats
            else List.nth name_formats 1
    ) in
    if just_return then used_name
    else (Printf.fprintf oc "%s" used_name; used_name)



and code_ident st i struct_param oc = match i with 
  | "_"     -> if not(struct_param) then (blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount;

  | "true"  -> if (Table.getLevel st i 0) = -1 then Printf.fprintf oc "%s" i
               else let _ = gen_used_name st i oc false in ();
  | "false" -> if (Table.getLevel st i 0) = -1 then Printf.fprintf oc "%s" i
               else let _ = gen_used_name st i oc false in ();
  | _       -> if struct_param then Printf.fprintf oc "__goLite__%s" i
               else let _ = gen_used_name st i oc false in ();


and get_vals_in_index st oc name =
    String.concat "" (List.map (fun n -> "[" ^ (gen_used_name st n oc true) ^ "]") (Type.get_names_in_arr_part name))

and gen_slice_arr_idx st oc name indices = 
    code_ident st name false oc;
    if not((List.length indices) = 1)
    then
        (* the last index in a slice is used in a function. ie go: x[1][0] -> x[1].getAtIndex(0) *)
        let rec gen_all_but_last_idx = function
            (* | [] -> *)
            | hd::[] -> ();
            (* check to make sure this doesn't print the indices in reverse order !!!!!!!!!!!!!!!!!!!!!!!!!!! *)
            | hd::tl -> (Printf.fprintf oc "["; code_exp hd st oc; Printf.fprintf oc "]"; gen_all_but_last_idx tl);
        in 
        gen_all_but_last_idx indices;

            (* plan: do lhs here, and have a printf "var.getAtIndex()" in code_exp *)
and assign_to_slice_idx st oc lhs rhs rhs_append =
    Printf.fprintf oc "%s" (tabs !tab_levels);
    (* if (Type.isSlice typ) then ( *)
    let indices = indices_used lhs in
    let name = idx_var_name lhs in
    (* gen_slice_arr_idx st oc name indices; *)
    (* Printf.fprintf oc " this is from assign_to_slice idx!!!!!"; *)
    (* print_string ("length of indices is: " ^ (string_of_int (List.length indices)) ^ "\n"); *)
    if (List.length indices) > 0 then (
        Printf.fprintf oc "%s" ("((Slice)" ^ (gen_used_name st name oc true) ^ ".getAtIndex(");
        code_exp (List.hd indices) st oc;
        Printf.fprintf oc "))";
    )
    else code_ident st name false oc;
    if not rhs_append then (
        Printf.fprintf oc ".addAtIndex(";
        code_exp (Gen_name_maker.get_last_elem indices) st oc;
        Printf.fprintf oc ", ";
        code_exp rhs st oc;
        Printf.fprintf oc ");\n";
    ) else (
        code_exp rhs st oc;
        Printf.fprintf oc ";\n";
    );


and gen_assign_if_not_append in_struct st oc lhs rhs assign =
    if (isAppend rhs) then (
        if (isIndex lhs) then assign_to_slice_idx st oc lhs rhs true
        else (
        Printf.fprintf oc "%s" (tabs !tab_levels);
        code_exp lhs st oc;
        code_exp rhs st oc;
        Printf.fprintf oc ";\n"; ))
    else
       if (isIndex lhs && (Type.isSlice (get_underlying_type (Type.typeFromExp Type.Null lhs st false true))))
      then assign_to_slice_idx st oc lhs rhs false
      else
        (let name = Type.nameFromVarUsage lhs in
        (* print_string ("before get underlying in gen_assign_if_not, nameFromUsage: " ^ name ^"\n\n"); *)
        let typ = get_underlying_type (Type.typeFromExp Type.Null rhs st false true) in
        (* print_string ("before new dcl\n"); *)
        let new_dcl = if assign then Table.isNewDeclScope st (Table.Symbol(name, typ)) else Table.isNewDecl st.Table.table (Table.Symbol(name, typ)) in
            if new_dcl then Table.genAdd st.Table.table (Table.Symbol(name, typ));
            Printf.fprintf oc "%s" (tabs !tab_levels);
            if (st.Table.parent=None) then Printf.fprintf oc "static ";
            code_type st in_struct typ oc;
            let used_name = Type.remove_arr_part name in
            let has_index = (not(used_name = name)) || (String.contains used_name '.') in
            let suffix = ( if has_index then (get_vals_in_index st oc name) else "") in

            (* print_string ("used_name is: " ^ used_name ^ ", stored type: " ^ (Type.string_of_types typ) ^ ", new_dcl: " ^ (string_of_bool new_dcl) ^"\n"); *)
            Printf.fprintf oc "_tmp%i_ = " (!tmpCount);
            (match get_underlying_type (Type.typeFromExp Type.Null rhs st false true) with
                | Table.Array(_,dims) -> Printf.fprintf oc "Arrays.copyOf("; code_exp rhs st oc; Printf.fprintf oc ", %d)" (List.hd dims);
                | _ -> code_exp rhs st oc
            );
            Printf.fprintf oc ";\n";
            Printf.fprintf oc "%s" (tabs !tab_levels);
            if (st.Table.parent=None) then Printf.fprintf oc "static ";
            if new_dcl then ((if not has_index then code_type st in_struct typ oc); code_exp lhs st oc)
            else
                (match st.parent with 
                            | Some i -> code_exp lhs st oc
                            | None -> code_ident st used_name in_struct oc; Printf.fprintf oc "%s" suffix;);
            Printf.fprintf oc " = ";
            (*code_exp rhs st oc;*)
            Printf.fprintf oc "_tmp%i_" (!tmpCount);
            tmpCount := !tmpCount + 1;
            Printf.fprintf oc ";\n";
      )

and code_type st in_struct t oc = match t with
  | Table.Int                                        -> Printf.fprintf oc "int "
  | Table.Float                                      -> Printf.fprintf oc "double "
  | Table.Bool                                       -> Printf.fprintf oc "boolean "
  | Table.String                                     -> Printf.fprintf oc "String "
  | Table.Rune                                       -> Printf.fprintf oc "char "
  | Table.Void                                       -> Printf.fprintf oc "void "
  | Table.CustomType(id, ((Table.Struct(_)) as typ)) -> code_ident st id in_struct oc
  | Table.CustomType(id, typ)                        -> code_type st in_struct typ oc
  | Table.TypeDef(id, typ)                           -> code_type st in_struct typ oc
  | Table.Slice(t,dim)                               -> Printf.fprintf oc "%s " "Slice"
  | Table.Array(t,dims)                              -> (code_type st in_struct t oc); (Printf.fprintf oc "%s " (String.concat "" (List.map (fun dim -> "[]") dims)))


and get_underlying_type t = match t with
  | Table.CustomType(_, Table.Struct(_)) -> t
  | Table.TypeDef(_, Table.Struct(_)) -> t
  | Table.CustomType(_,u) -> get_underlying_type u
  | Table.TypeDef(_,u) -> get_underlying_type u
  | _ -> t



and code_struct_params params st oc = match params with
  | [] -> ()
  | h::t -> Printf.fprintf oc "%s" (tabs !tab_levels); code_decl h st true oc; code_struct_params t st oc;

and code_equals_method params oc st = match params with
  | [] -> ()
  | h::[] -> (match h with
              | Ast.UndeclaredVar (id, t) -> let typ = Type.type_of_string t st in
                (match typ with
                   | Table.CustomType(name,Table.Struct(params)) -> if not (id="_") then Printf.fprintf oc "this."; code_ident st id true oc; Printf.fprintf oc ".equals(x."; code_ident st id true oc; Printf.fprintf oc ")";
                   | Table.CustomType(_,(Table.Slice(Table.Struct(params),dim))) -> Printf.fprintf oc "true";
                   (* | Table.CustomType(_,(Table.GenSlice(Table.Struct(params),dim,_))) -> Printf.fprintf oc "true"; *)
                   | Table.CustomType(name,(Table.Array(Table.Struct(params),dims))) -> if not(id="_") then Printf.fprintf oc "Arrays.equals(this."; code_ident st id true oc; Printf.fprintf oc ",x."; code_ident st id true oc; Printf.fprintf oc ")";
                   | _ -> let ut = get_underlying_type (Type.type_of_string t st) in if not(id="_") then 
                            Printf.fprintf oc "this."; code_ident st id true oc; if not(ut=Table.String) then Printf.fprintf oc "==x." else Printf.fprintf oc ".equals(x."; code_ident st id true oc; if (ut=Table.String) then Printf.fprintf oc ")";)
              | Ast.ArrayDcl(id,t,dims) -> if not(id="_") then Printf.fprintf oc "Arrays.equals(this."; code_ident st id true oc; Printf.fprintf oc ",x."; code_ident st id true oc; Printf.fprintf oc ")";
              | Ast.SliceDcl(id,t,dim) -> Printf.fprintf oc "true";
              | Ast.StructDcl(id, _) -> if not (id="_") then Printf.fprintf oc "this."; code_ident st id true oc; Printf.fprintf oc ".equals(x."; code_ident st id true oc; Printf.fprintf oc ")";
            ); code_equals_method [] oc st;
  | h::t -> (match h with
              | Ast.UndeclaredVar (id, t) -> let typ = Type.type_of_string t st in
                (match typ with
                   | Table.CustomType(name,Table.Struct(params)) -> if not (id="_") then Printf.fprintf oc "this."; code_ident st id true oc; Printf.fprintf oc ".equals(x."; code_ident st id true oc; Printf.fprintf oc ")";
                   | Table.CustomType(_,(Table.Slice(Table.Struct(params),dim))) -> Printf.fprintf oc "true";
                   | Table.CustomType(name,(Table.Array(Table.Struct(params),dims))) -> if not(id="_") then Printf.fprintf oc "Arrays.equals(this."; code_ident st id true oc; Printf.fprintf oc ",x."; code_ident st id true oc; Printf.fprintf oc ")";
                   | _ -> let ut = get_underlying_type (Type.type_of_string t st) in if not(id="_") then 
                            Printf.fprintf oc "this."; code_ident st id true oc; if not(ut=Table.String) then Printf.fprintf oc "==x." else Printf.fprintf oc ".equals(x."; code_ident st id true oc; if (ut=Table.String) then Printf.fprintf oc ")";)
              | Ast.ArrayDcl(id,t,dims) -> if not(id="_") then Printf.fprintf oc "Arrays.equals(this."; code_ident st id true oc; Printf.fprintf oc ",x."; code_ident st id true oc; Printf.fprintf oc ")";
              | Ast.SliceDcl(id,t,dim) -> Printf.fprintf oc "true";
              | Ast.StructDcl(id, _) -> if not (id="_") then Printf.fprintf oc "this."; code_ident st id true oc; Printf.fprintf oc ".equals(x."; code_ident st id true oc; Printf.fprintf oc ")";
            ); Printf.fprintf oc " && "; code_equals_method t oc st;

and code_copy_constructor params oc st = match params with
  | [] -> ()
  | h::t -> (match h with
              | Ast.UndeclaredVar (id, t) -> let typ = Type.type_of_string t st in
                (match typ with
                   | Table.CustomType(name,Table.Struct(params)) -> if not (id="_") then
                      begin
                        Printf.fprintf oc "%s" (tabs !tab_levels);
                        Printf.fprintf oc "this.";
                        code_ident st id true oc;
                        Printf.fprintf oc " = new ";
                        code_ident st name false oc;
                        Printf.fprintf oc "(x.";
                        code_ident st id true oc;
                        Printf.fprintf oc ");\n";
                      end
                   | Table.CustomType(_,(Table.Slice(Table.Struct(params),dim))) -> if not(id="_") then
                     begin
                       Printf.fprintf oc "%s" (tabs !tab_levels);
                       Printf.fprintf oc "this.";
                       code_ident st id true oc;
                       Printf.fprintf oc " = x.";
                       code_ident st id true oc;
                       Printf.fprintf oc ";\n";
                     end
                   | Table.CustomType(name,(Table.Array(Table.Struct(params),dims))) -> if not(id="_") then
                     begin
                       Printf.fprintf oc "%s" (tabs !tab_levels);
                       Printf.fprintf oc "this.";
                       code_ident st id true oc;
                       Printf.fprintf oc " = Arrays.copyOf(x.";
                       code_ident st id true oc;
                       Printf.fprintf oc ", x.";
                       code_ident st id true oc;
                       Printf.fprintf oc ".length);";
                     end
                   | _ -> Printf.fprintf oc "%s" (tabs !tab_levels); Printf.fprintf oc "this."; code_ident st id true oc; Printf.fprintf oc " = x."; code_ident st id true oc; Printf.fprintf oc ";\n";
              )
            | Ast.ArrayDcl(id, _, _) -> 
                     begin
                       Printf.fprintf oc "%s" (tabs !tab_levels);
                       Printf.fprintf oc "this.";
                       code_ident st id true oc;
                       Printf.fprintf oc " = Arrays.copyOf(x.";
                       code_ident st id true oc;
                       Printf.fprintf oc ", x.";
                       code_ident st id true oc;
                       Printf.fprintf oc ".length);";
                     end
            | Ast.SliceDcl(id, _, _) ->
                     begin
                       Printf.fprintf oc "%s" (tabs !tab_levels);
                       Printf.fprintf oc "this.";
                       code_ident st id true oc;
                       Printf.fprintf oc " = x.";
                       code_ident st id true oc;
                       Printf.fprintf oc ";\n";
                     end
            | Ast.StructDcl(id, _) ->
                      begin
                        Printf.fprintf oc "%s" (tabs !tab_levels);
                        Printf.fprintf oc "this.";
                        code_ident st id true oc;
                        Printf.fprintf oc " = new AnonStruct%d" !anonStructCount;
                        Printf.fprintf oc "(x.";
                        code_ident st id true oc;
                        Printf.fprintf oc ");\n";
                      end
            ); code_copy_constructor t oc st;

and code_struct id params is_anon st oc = let name = String.sub id 1 ((String.length id) - 1) in if not(name="_") then
  begin
    (* Since we're not typechecking we only need to store the name of the struct type, the params are not important. *)
    if not(is_anon) then Table.genAdd st.Table.table (Table.Symbol(name, Table.CustomType(name,Table.Struct(Type.makeParams "" params st [] false false))));
    Printf.fprintf oc "class ";
    if not(is_anon) then code_ident st name false oc else Printf.fprintf oc "%s" name;
    Printf.fprintf oc " {\n";
    tab_levels := !tab_levels + 1;
    code_struct_params params st oc;
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "public ";
    if not(is_anon) then Printf.fprintf oc "__goLite__%s_%i" name (!tab_levels - 1) else Printf.fprintf oc "%s" name;
    Printf.fprintf oc "(){}\n";
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "public ";
    if not(is_anon) then Printf.fprintf oc "__goLite__%s_%i" name (!tab_levels - 1) else Printf.fprintf oc "%s" name;
    Printf.fprintf oc "(";
    if not(is_anon) then Printf.fprintf oc "__goLite__%s_%i" name (!tab_levels - 1) else Printf.fprintf oc "%s" name;
    Printf.fprintf oc " x){\n";
    tab_levels := !tab_levels + 1;
    code_copy_constructor params oc st;
    tab_levels := !tab_levels - 1;
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "}\n";
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "public boolean equals(";
    if not(is_anon) then Printf.fprintf oc "__goLite__%s_%i" name (!tab_levels - 1) else Printf.fprintf oc "%s" name;
    Printf.fprintf oc " x){ return ";
    code_equals_method params oc st;
    Printf.fprintf oc ";}\n";
    tab_levels := !tab_levels - 1;
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "}\n";
  end

and code_anon_struct id params st oc func_arg = if not(id="_") then
  begin
    (* Since we're not typechecking we only need to store the name of the anonymous struct, the params are not important. *)
    (anonStructCount := !anonStructCount + 1);
    if not(func_arg) then Table.genAdd st.Table.table (Table.Symbol(id, Table.CustomType(("AnonStruct" ^ (string_of_int !anonStructCount)), Table.Struct(Type.makeParams "" params st [] false false))));
    code_struct ("-AnonStruct" ^ (string_of_int !anonStructCount)) params true st oc;
    if not(func_arg) then (
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "AnonStruct%i " !anonStructCount;
    code_ident st id false oc;
    Printf.fprintf oc " = new AnonStruct%i();\n" !anonStructCount);
  end

and gen_slice st in_struct oc id dim =
    (* called upon slice dcl *)
    (* appends a slice to be the first element of the declared slice for each dim *)
    let rec link_multi_dim_slices used_name dims_to_add = match dims_to_add with
    | 0 -> ()
    | _ ->
        begin
            (* this function add *)
            (* print_string ("used name for multi-dim slice is: " ^ used_name ^ "\n"); *)
            Printf.fprintf oc "%s" (tabs !tab_levels);
            if not(dim = (dims_to_add - 1)) then Printf.fprintf oc "((Slice)";
            code_ident st used_name in_struct oc;
            if not(dim = (dims_to_add - 1)) then Printf.fprintf oc ".getAtIndex(0))";
            (* code_ident st id in_struct oc; *)
        (* and code_ident st i struct_param oc = match i with *) 
            Printf.fprintf oc ".append(";
            (* we are adding this value to same struct we which stores append result *)
            if not(dim = (dims_to_add - 1)) then Printf.fprintf oc "((Slice)";
            code_ident st used_name false oc;
            if not(dim = (dims_to_add - 1)) then Printf.fprintf oc ".getAtIndex(0))";
            Printf.fprintf oc ", ";
            (* code_exp e t oc; *)
            Printf.fprintf oc "new Slice());\n";

            (* later change used_name to some fxn which returns string: ((Slice)(used_name.getAtIndex(0))) to be used as name in nxt iter *)
            (* this should always pass used_name translated to             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)
            link_multi_dim_slices used_name (dims_to_add - 1);
        end
    in
    Printf.fprintf oc "Slice ";
    (* print_string ("the id before code_ident is: " ^ id ^ "\n"); *)
    code_ident st id in_struct oc;
    Printf.fprintf oc "%s" (" = new Slice();\n");
    (* print_string ("the id in slice maker is: " ^ id ^ "\n"); *)
    if ((dim > 1) && (not in_struct)) then link_multi_dim_slices id (dim-1);

and code_decl d st in_struct oc = match d with 
  | Ast.TypedVar (id, t, e) -> let typ = get_underlying_type (Type.typeFromExp Type.Null e st false true) in 
    begin
      let new_dcl = Table.isNewDecl st.Table.table (Table.Symbol(id, typ)) in
      tmpCount := !tmpCount + 1;
      code_type st in_struct typ oc;
      Printf.fprintf oc "_tmp_typed%i_ = " !tmpCount;
      code_exp e st oc;
      Printf.fprintf oc ";\n";
      Table.genAdd st.Table.table (Table.Symbol(id, typ));
      Printf.fprintf oc "%s" (tabs !tab_levels);
      if (st.Table.parent=None) then Printf.fprintf oc "static ";
      if new_dcl then
        code_type st in_struct typ oc;
      code_ident st id in_struct oc;
      Printf.fprintf oc " = _tmp_typed%i_;\n" !tmpCount;
    end
  | Ast.InferredVar (var_usage, e) -> gen_assign_if_not_append in_struct st oc var_usage e false
  | Ast.ShortAssign(var_usage, e) ->  gen_assign_if_not_append in_struct st oc var_usage e false
  | Ast.UndeclaredVar (id, t) -> let typ = Type.type_of_string t st in
    begin
    match typ with
      | Table.CustomType(name,Table.Struct(params)) -> if not(in_struct && (id="_")) then
             begin
               if not(in_struct) then Table.genAdd st.Table.table (Table.Symbol(id, typ));
               code_ident st name in_struct oc;
               Printf.fprintf oc " ";
               code_ident st id in_struct oc;
               Printf.fprintf oc " = new ";
               code_ident st name in_struct oc;
               Printf.fprintf oc "();\n";
             end
      | Table.CustomType(name,(Table.Slice(Table.Struct(params),dim))) -> if not(in_struct && (id="_")) then
             begin
               if not(in_struct) then Table.genAdd st.Table.table (Table.Symbol(id, typ));
               gen_slice st in_struct oc id dim
             end
      | Table.CustomType(name,(Table.Array(Table.Struct(params),dims))) -> if not(in_struct && (id="_")) then
             begin
               if not(in_struct) then Table.genAdd st.Table.table (Table.Symbol(id, typ));
               code_ident st name in_struct oc;
               Printf.fprintf oc "%s " (String.concat "" (List.map (fun dim -> "[]") dims));
               code_ident st id in_struct oc;
               Printf.fprintf oc " = new ";
               code_ident st name in_struct oc;
               Printf.fprintf oc "%s" (String.concat "" (List.map (fun dim -> "[" ^ (string_of_int dim) ^ "]") dims));
               Printf.fprintf oc ";\n";
               if not(id="_") then
                begin
                  (* (Printf.fprintf oc "%s" (tabs !tab_levels)); *)
                  (* Printf.fprintf oc "Arrays.fill("; *)
                  (* code_ident st id in_struct oc; *)
                  (* Printf.fprintf oc ",new "; *)
                  (* code_ident st name in_struct oc; *)
                  (* Printf.fprintf oc "());\n"; *)
                end
             end
      | _ -> let ut = get_underlying_type typ in if not(in_struct && (id="_")) then
             begin
               if not(in_struct) then Table.genAdd st.Table.table (Table.Symbol(id, ut));
               code_type st in_struct ut oc;
               code_ident st id in_struct oc;
               Printf.fprintf oc " = ";
               (match ut with
                 | Table.Int               -> Printf.fprintf oc "0"
                 | Table.Float             -> Printf.fprintf oc "0"
                 | Table.Bool              -> Printf.fprintf oc "false"
                 | Table.String            -> Printf.fprintf oc "\"\""
                 | Table.Rune              -> Printf.fprintf oc "'\\0'"
                 | Table.Slice(t,dim)      -> Printf.fprintf oc "%s" ("new Slice" ^ "()")
                 | Table.Array(t,dims)     -> (Printf.fprintf oc "new "); (code_type st in_struct t oc); Printf.fprintf oc "%s" (String.concat "" (List.map (fun dim -> "[" ^ (string_of_int dim) ^ "]") dims));
                                          if t=Table.String && not(id="_") then (Printf.fprintf oc ";\n");); (*(Printf.fprintf oc "%s" (tabs !tab_levels)); Printf.fprintf oc "Arrays.fill("; code_ident st id in_struct oc; Printf.fprintf oc ",\"\")";); *)
               Printf.fprintf oc ";\n";
             end
        end
  | Ast.TypeDcl (id, t) -> let typ = get_underlying_type (Type.type_of_string t st) in
      if not(id="_") then Table.genAdd st.Table.table (Table.Symbol(id, Table.TypeDef(id,typ)));
  | Ast.SliceDcl (id, t, dim) ->
        let typ = Type.type_of_string t st in
    (match typ with
      | Table.CustomType(name,Table.Struct(params)) ->
             begin
               if not(in_struct) then Table.genAdd st.Table.table (Table.Symbol(id, (Table.Slice(typ,dim))));
               gen_slice st in_struct oc id dim
             end
      | _ -> let ut = get_underlying_type typ in
             begin
               if not(in_struct) then Table.genAdd st.Table.table (Table.Symbol(id, (Table.Slice(ut,dim))));
               gen_slice st in_struct oc id dim
             end)
  | Ast.ArrayDcl (id, t, dims) -> let typ = Type.type_of_string t st in
    (match typ with
      | Table.CustomType(name,Table.Struct(params)) ->
             begin
               if not(in_struct) then Table.genAdd st.Table.table (Table.Symbol(id, (Table.Array(typ,dims))));
               code_ident st name in_struct oc;
               Printf.fprintf oc "%s " (String.concat "" (List.map (fun dim -> "[]") dims));
               code_ident st id in_struct oc;
               Printf.fprintf oc " = new ";
               code_ident st name in_struct oc;
               Printf.fprintf oc "%s" (String.concat "" (List.map (fun dim -> "[" ^ (string_of_int dim) ^ "]") dims));
               Printf.fprintf oc ";\n";
               if not(id="_") then
                begin
                  (* (Printf.fprintf oc "%s" (tabs !tab_levels)); *)
                  (* Printf.fprintf oc "Arrays.fill("; *)
                  (* code_ident st id in_struct oc; *)
                  (* Printf.fprintf oc ",new "; *)
                  (* code_ident st name in_struct oc; *)
                  (* Printf.fprintf oc "());\n"; *)
                end
             end
      | _ -> let ut = get_underlying_type typ in
             begin
               Table.genAdd st.Table.table (Table.Symbol(id, (Table.Array(ut,dims))));
               code_type st in_struct (Table.Array(ut,dims)) oc;
               code_ident st id in_struct oc;
               Printf.fprintf oc " = new ";
               code_type st in_struct ut oc;
               Printf.fprintf oc "%s" (String.concat "" (List.map (fun dim -> "[" ^ (string_of_int dim) ^ "]") dims));
               if ut=Table.String && not(id="_") then (Printf.fprintf oc ";\n"); (* (Printf.fprintf oc "%s" (tabs !tab_levels)); Printf.fprintf oc "Arrays.fill("; code_ident st id in_struct oc; Printf.fprintf oc ",\"\")"; *)
               Printf.fprintf oc ";\n";
             end)
  | Ast.StructDcl (id, params) ->
    if id.[0] = '-' then code_struct id params false st oc else code_anon_struct id params st oc false;
  | Ast.SliceStructDcl (id, arr, struckt) ->
    begin
      code_decl struckt st in_struct oc;
      (Printf.fprintf oc "%s" (tabs !tab_levels));
      (match arr with
         | Ast.SliceDcl(_,_,dim) ->
            begin
              Table.genAdd st.Table.table (Table.Symbol(id, (Table.Slice((Table.Struct([])),dim))));
              gen_slice st in_struct oc id dim
             end);
    end
  | Ast.ArrayStructDcl (id, arr, struckt) ->
    begin
      code_decl struckt st in_struct oc;
      (Printf.fprintf oc "%s" (tabs !tab_levels));
      (match arr with
         | Ast.ArrayDcl(_,_,dims) ->
            begin
              Table.genAdd st.Table.table (Table.Symbol(id, (Table.Array((Table.Struct([])),dims))));
              Printf.fprintf oc "AnonStruct%i " !anonStructCount;
              Printf.fprintf oc "%s " (String.concat "" (List.map (fun dim -> "[]") dims));
              code_ident st id in_struct oc;
              Printf.fprintf oc " = new AnonStruct%i" !anonStructCount;
              Printf.fprintf oc "%s" (String.concat "" (List.map (fun dim -> "[" ^ (string_of_int dim) ^ "]") dims));
              Printf.fprintf oc ";\n";
              if not(id="_") then
                begin
                  (* (Printf.fprintf oc "%s" (tabs !tab_levels)); *)
                  (* Printf.fprintf oc "Arrays.fill("; *)
                  (* code_ident st id in_struct oc; *)
                  (* Printf.fprintf oc ",new AnonStruct%i());\n" !anonStructCount; *)
                end
            end);
    end
and code_decls dcls t oc =
  match dcls with
  | [] -> ()
  | h::tl ->
    begin
      code_decl h t false oc;
      Printf.fprintf oc "%s" (tabs !tab_levels);
      code_decls tl t oc;
    end;

and tab_levels = ref 0;
and blankIdCount = ref 0;
and anonStructCount = ref 0;
and defaultStatement = ref [Ast.Empty];
and tmpCount = ref 0;

and tabs n = 
  if n = 0 then "" else "\t" ^ tabs (n - 1)

and code_init_func sl t oc =
  begin
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "static {\n";
    tab_levels := !tab_levels + 1;
    code_stmnts sl (Table.scope (Some t) (Some(Table.Void))) oc;
    tab_levels := !tab_levels - 1;
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "}\n\n"
  end

and code_main_func sl t oc =
  begin
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "public static void main(String[] args) {\n";
    tab_levels := !tab_levels + 1;
    code_stmnts sl (Table.scope (Some t) (Some(Table.Void))) oc;
    tab_levels := !tab_levels - 1;
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "}\n\n"
  end

and check_func_args args st oc acc = match args with
  | [] -> List.rev acc
  | h::t -> (match h with
              | Ast.StructDcl(id,params) -> Printf.fprintf oc "%s" (tabs !tab_levels); code_anon_struct id params st oc true; check_func_args t st oc (!anonStructCount::acc)
              | Ast.SliceStructDcl(_,_,sdcl) -> Printf.fprintf oc "%s" (tabs !tab_levels); code_decl sdcl st false oc; check_func_args t st oc (!anonStructCount::acc)
              | Ast.ArrayStructDcl(_,_,sdcl) -> Printf.fprintf oc "%s" (tabs !tab_levels); code_decl sdcl st false oc; check_func_args t st oc (!anonStructCount::acc)
              | _ -> check_func_args t st oc acc)




and code_func_args st args anons anon_count oc = match args with
  | [] -> ()
  | h::[] -> (match h with
              | Ast.StructDcl(id,_) -> Printf.fprintf oc "AnonStruct%i " (List.nth anons anon_count); if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1)); code_func_args st [] anons (anon_count + 1) oc
              | Ast.SliceStructDcl(id,arr,_) -> (match arr with
                                                  | Ast.SliceDcl(_,_,dim) ->
                                                    begin
                                                      Printf.fprintf oc "Slice ";
                                                      if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                                                      code_func_args st [] anons (anon_count + 1) oc
                                                    end)
              | Ast.ArrayStructDcl(id,arr,_) -> (match arr with
                                                  | Ast.ArrayDcl(_,_,dims) ->
                                                    begin
                                                      Printf.fprintf oc "AnonStruct%i" (List.nth anons anon_count);
                                                      Printf.fprintf oc "%s " (String.concat "" (List.map (fun dim -> "[]") dims));
                                                      if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                                                      code_func_args st [] anons (anon_count + 1) oc
                                                    end)
              | Ast.UndeclaredVar(id,typ) -> let ut = Type.type_of_string typ st in
                begin
                  code_type st false ut oc;
                  if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                  code_func_args st [] anons anon_count oc
                end
              |Ast.ArrayDcl(id,typ,dims) ->
                begin
                    let ut = Type.type_of_string typ st in
                    code_type st false ut oc;
                    Printf.fprintf oc "%s " (String.concat "" (List.map (fun dim -> "[]") dims));
                    if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                    (* print_string "did arr dcl in func \n"; *)
                    code_func_args st [] anons anon_count oc
                end
              |Ast.SliceDcl(id,typ,dim) ->
                begin
                    Printf.fprintf oc "Slice ";
                    if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                    code_func_args st [] anons (anon_count + 1) oc
                end
            | _ as e -> (let name = Type.nameFromDcl e in
            (* let typ = Type.string_of_types (get_underlying_type (Type.typeFromExp Type.Null e st false true)) in *)
            print_string ("name of unmatched: " ^ name ^ "\n")(*^ ", type: " ^ typ ^ "\n"); *)
            ))
  | h::t -> (match h with
              | Ast.StructDcl(id,_) -> Printf.fprintf oc "AnonStruct%i " (List.nth anons anon_count); if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1)); Printf.fprintf oc ", "; code_func_args st t anons (anon_count + 1) oc
              | Ast.SliceStructDcl(id,arr,_) -> (match arr with
                                                  | Ast.SliceDcl(_,_,dim) ->
                                                    begin
                                                      Printf.fprintf oc "Slice ";
                                                      if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                                                      Printf.fprintf oc ", ";
                                                      code_func_args st t anons (anon_count + 1) oc
                                                    end)
              | Ast.ArrayStructDcl(id,arr,_) -> (match arr with
                                                  | Ast.ArrayDcl(_,_,dims) ->
                                                    begin
                                                      Printf.fprintf oc "AnonStruct%i" (List.nth anons anon_count);
                                                      Printf.fprintf oc "%s " (String.concat "" (List.map (fun dim -> "[]") dims));
                                                      if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                                                      Printf.fprintf oc ", ";
                                                      code_func_args st t anons (anon_count + 1) oc
                                                    end)
              | Ast.UndeclaredVar(id,typ) -> let ut = Type.type_of_string typ st in
                begin
                  code_type st false ut oc;
                  if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                  Printf.fprintf oc ", ";
                  code_func_args st t anons anon_count oc
                end
              |Ast.ArrayDcl(id,typ,dims) ->
                begin
                    let ut = Type.type_of_string typ st in
                    code_type st false ut oc;
                    Printf.fprintf oc "%s " (String.concat "" (List.map (fun dim -> "[]") dims));
                    if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                    (* print_string "did arr dcl in func \n"; *)
                    Printf.fprintf oc ", ";
                    code_func_args st t anons anon_count oc
                end
              |Ast.SliceDcl(id,typ,dim) ->
                begin
                    Printf.fprintf oc "Slice ";
                    if id="_" then ((blankIdCount := !blankIdCount + 1); Printf.fprintf oc "__goLite__blankId%i" !blankIdCount) else( Printf.fprintf oc "__goLite__%s_%i " id (!tab_levels + 1));
                    Printf.fprintf oc ", ";
                    code_func_args st t anons (anon_count + 1) oc
                end)


and add_func_arg st arg = match arg with
  | Ast.StructDcl(id,params)          -> Table.genAdd st.Table.table (Table.Symbol(id, Table.Struct(Type.makeParams "" params st [] false false)));
  | Ast.SliceStructDcl(id,_,_)   -> ()
  | Ast.ArrayStructDcl(id,arr,_) -> (match arr with
                                    | Ast.ArrayDcl(_,_,dims) -> Table.genAdd st.Table.table (Table.Symbol(id, Table.Array(Table.Struct([]), dims))));
  | Ast.UndeclaredVar(id,typ)      -> let ut = get_underlying_type (Type.type_of_string typ st) in Table.genAdd st.Table.table (Table.Symbol(id, ut));
  | Ast.ArrayDcl(id,typ,dim)       -> let ut = get_underlying_type (Type.type_of_string typ st) in Table.genAdd st.Table.table (Table.Symbol(id, Table.Array(ut,dim)));
  | Ast.SliceDcl(id,typ,dim)       -> let ut = get_underlying_type (Type.type_of_string typ st) in Table.genAdd st.Table.table (Table.Symbol(id, Table.Slice(ut,dim)));

and code_func id args rt sl t oc = let ut = get_underlying_type (Type.type_of_string rt t) in
  let anon_structs = check_func_args args t oc [] in
  begin
    (* Since we're not typechecking we only need to store the name of the function, the args are not important. *)
    let al = Type.makeParams id args t [] false false in
    Table.genAdd t.Table.table (Table.Symbol(id, (Table.Func(al,ut))));
    Printf.fprintf oc "%sstatic " (tabs !tab_levels);
    code_type t false ut oc;
    code_ident t id false oc;
    Printf.fprintf oc "(";
    code_func_args t args anon_structs 0 oc;
    Printf.fprintf oc ") {\n";
    tab_levels := !tab_levels + 1;
    let new_t = (Table.scope (Some t) (Some(Type.type_of_string rt t))) in (
    (List.iter (fun dcl -> add_func_arg new_t dcl)) args;
    code_stmnts sl new_t oc;);
    tab_levels := !tab_levels - 1;
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "}\n\n"
  end

and print_exps el t oc ln = match el with
  | [] -> ()
  | h::[] -> 
      begin
        (match Type.typeFromExp Type.Null h t false true with
          | Table.Rune -> Printf.fprintf oc "(int)"; code_exp h t oc;
          | Table.Float -> 
            begin
              Printf.fprintf oc "(";
              code_exp h t oc;
              Printf.fprintf oc " >= 0 ? \"+\" : \"\") + String.format(\"%s\", " "%.6e";
              code_exp h t oc;
              Printf.fprintf oc ")";
            end
          | _ -> code_exp h t oc;);
        print_exps [] t oc ln;
      end
  | h::tl -> 
      begin
        (match Type.typeFromExp Type.Null h t false true with
          | Table.Rune -> Printf.fprintf oc "(int)"; code_exp h t oc;
          | Table.Float -> 
            begin
              Printf.fprintf oc "(";
              code_exp h t oc;
              Printf.fprintf oc " >= 0 ? \"+\" : \"\") + String.format(\"%s\", " "%.6e";
              code_exp h t oc;
              Printf.fprintf oc ")";
            end
          | _ -> code_exp h t oc;);
        if ln then Printf.fprintf oc " + \" \" + " else Printf.fprintf oc " + \"\" + ";        
        print_exps tl t oc ln;
      end

and code_stmnts s t oc = match s with
  | h::tl -> 
      begin
        code_stmnt h t oc;
        if not(h=Ast.Break) then code_stmnts tl t oc;
      end
  | [] -> ()

and code_stmnt s t oc =
  match s with
  | Ast.Break -> Printf.fprintf oc "%s" (tabs !tab_levels);Printf.fprintf oc "break;\n";
  | Ast.Continue -> Printf.fprintf oc "%s" (tabs !tab_levels);Printf.fprintf oc "continue;\n"
  | Ast.Return(e) -> 
      begin
        match e with 
        | None -> Printf.fprintf oc "%s" (tabs !tab_levels);Printf.fprintf oc "return;\n"
        | Some(ex) ->
            begin 
              Printf.fprintf oc "%s" (tabs !tab_levels);
              Printf.fprintf oc "return ";
              code_exp ex t oc;
              Printf.fprintf oc ";\n";
            end
      end
  | Ast.DeclStmt d -> 
    begin 
      match d with
      | [Ast.InferredVar(_, _)] | [Ast.ShortAssign(_, _)] -> code_decls d t oc; Printf.fprintf oc "\n"
      | _ -> Printf.fprintf oc "%s" (tabs !tab_levels); if (t.Table.parent=None) then Printf.fprintf oc "static ";  code_decls d t oc; Printf.fprintf oc "\n"
    end 
  | Ast.PackageDcl id -> ()
  | Ast.FuncDcl(id,args,rt,sl) -> if not(id="_") then
      begin
        if (id="init") then code_init_func sl t oc else
        if (id="main") then code_main_func sl t oc else
        code_func id args rt sl t oc;
      end
  | Ast.ForInf (sl) -> 
      begin
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "while (true) {\n";
        tab_levels := !tab_levels + 1;
        code_stmnts sl (Table.scope (Some t) t.return_type) oc;
        tab_levels := !tab_levels - 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
      end
  | Ast.ForWhile (e, sl) ->
      begin
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "while (";
        code_exp e t oc;
        Printf.fprintf oc ") {\n";
        tab_levels := !tab_levels + 1;
        code_stmnts sl (Table.scope (Some t) t.return_type) oc;
        tab_levels := !tab_levels - 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
      end
  | Ast.For (s1, e, s2, sl) -> 
      let inner = Table.scope (Some t) t.return_type in
      begin
        Printf.fprintf oc "%s" (tabs !tab_levels);
        tab_levels := !tab_levels + 1;
        Printf.fprintf oc "{\n";
        code_stmnt s1 inner oc;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        code_for s1 e s2 sl inner oc;
        tab_levels := !tab_levels - 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
      end
  | Ast.BlockStmt(sl) -> 
      begin
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "{\n";
        tab_levels := !tab_levels + 1;
        code_stmnts sl (Table.scope (Some t) t.return_type) oc;
        tab_levels := !tab_levels - 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
      end
  | Ast.Print(el) ->
      begin
        match el with 
        | None -> Printf.fprintf oc "%s" (tabs !tab_levels);Printf.fprintf oc "System.out.print();\n";
        | Some (ell) ->
            begin
              Printf.fprintf oc "%s" (tabs !tab_levels);
              Printf.fprintf oc "System.out.print(";
              print_exps ell t oc false;
              Printf.fprintf oc ");\n";
            end
      end
  | Ast.Println(el) -> 
      begin
        match el with 
        | None -> Printf.fprintf oc "%s" (tabs !tab_levels);Printf.fprintf oc "System.out.println();\n";
        | Some (ell) ->
            begin
              Printf.fprintf oc "%s" (tabs !tab_levels);
              Printf.fprintf oc "System.out.println(";
              print_exps ell t oc true;
              Printf.fprintf oc ");\n";
            end
      end
  | Ast.If(e,sl,elif) -> Printf.fprintf oc "%s" (tabs !tab_levels); code_if_stmt e sl elif t oc
    (* here we create a new scope for this if statement so that the possible var assignment in 'stmt' only exists for the if/eif *)
  | Ast.IfSimp (stmt,e,stmts,eif) -> Printf.fprintf oc "%s" (tabs !tab_levels); code_if_simp_stmt stmt e stmts eif t oc
  | Ast.SwitchSimpExpr(s, e, eccl) -> code_switch eccl (Some s) e t oc "tmp"
  | Ast.Switch(eccl) -> code_switch eccl None (Ast.Bool(true)) t oc "tmp"
  | Ast.SwitchExpr(e, eccl) -> code_switch eccl None e t oc "tmp"
  | Ast.SwitchSimp(s, eccl) -> code_switch eccl (Some s) (Ast.Bool(true)) t oc "tmp"
  | _ ->
      begin
        match s with 
        | Ast.Assign(_) -> code_simple_stmt s t oc;
        | _ -> Printf.fprintf oc "%s" (tabs !tab_levels); code_simple_stmt s t oc; Printf.fprintf oc ";\n"
      end

and gen_assign in_struct st oc dcl = match dcl with
  | Ast.InferredVar(lhs,rhs) | Ast.ShortAssign(lhs, rhs) ->
    (* print_string "in multiple assignment\n"; *)
    (* let code_exp do all the printing if this is an append *)
    gen_assign_if_not_append in_struct st oc lhs rhs true
  | _ -> failwith ""

and code_simple_stmt s t oc = match s with 
  | Ast.Empty -> Printf.fprintf oc "\n"
  | Ast.DeclStmt d -> code_decls d t oc;
  | Ast.Assign (dl) -> (List.iter (fun dcl -> gen_assign false t oc dcl)) dl; ()
  | Ast.Assignop(l,op,r) ->
    begin
      match op with 
      | Ast.NandEquals ->
          begin (* l = l & ~r *)
            code_exp l t oc;
            Printf.fprintf oc " = ";
            code_exp l t oc;
            Printf.fprintf oc " & ~";
            code_exp r t oc;
          end
      | _ ->
          begin
            code_exp l t oc;
            code_assign_op l op oc;
            Printf.fprintf oc "(";
            code_exp r t oc;
            Printf.fprintf oc ")";
          end
    end
  | Ast.Increment(e) -> 
      begin
        code_exp e t oc;
        Printf.fprintf oc "++";
      end
  | Ast.Decrement(e) ->
      begin
        code_exp e t oc;
        Printf.fprintf oc "--";
      end
  | Ast.ExpressionStmt(e) -> code_exp e t oc
  | _ -> failwith ""

and code_if_stmt e sl eif t oc = 
  begin
    Printf.fprintf oc "if (";
    code_exp e t oc;
    Printf.fprintf oc ") {\n";
    tab_levels := !tab_levels + 1;
    code_stmnts sl (Table.scope (Some t) t.return_type) oc;
    tab_levels := !tab_levels - 1;
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "}\n";
    code_else_if eif t oc;
  end

and code_if_simp_stmt s e sl eif t oc = 
    let inner = Table.scope (Some t) t.return_type in
    begin 
      Printf.fprintf oc "{\n"; 
      tab_levels := !tab_levels + 1;
      code_simple_stmt s inner oc; 
      code_if_stmt e sl eif inner oc; 
      tab_levels := !tab_levels - 1;
      Printf.fprintf oc "%s" (tabs !tab_levels); 
      Printf.fprintf oc "}\n";
    end

and code_else_if e t oc = match e with
  | Ast.ElseIf(s) -> 
    begin 
    match s with 
    | Ast.If(e, sl, eif) ->
      begin
        Printf.fprintf oc "%s" (tabs (!tab_levels));
        Printf.fprintf oc "else ";
        code_if_stmt e sl eif t oc;
      end
    | Ast.IfSimp(s, e, sl, eif) ->
      begin
        Printf.fprintf oc "%s" (tabs (!tab_levels));
        Printf.fprintf oc "else ";
        code_if_simp_stmt s e sl eif t oc;
      end
    end
  | Ast.Else(sl) ->
    begin
      Printf.fprintf oc "%s" (tabs (!tab_levels));
      Printf.fprintf oc "else {\n";
      code_stmnts sl t oc;
      Printf.fprintf oc "%s" (tabs !tab_levels);
      Printf.fprintf oc "}\n"
    end
  | Ast.EndElse -> Printf.fprintf oc ""

and code_assign_op l op oc = match op with 
  | Ast.PlusEquals   -> Printf.fprintf oc " += "
  | Ast.MinusEquals  -> Printf.fprintf oc " -= "
  | Ast.OrEquals     -> Printf.fprintf oc " |= "
  | Ast.XorEquals    -> Printf.fprintf oc " ^= "
  | Ast.TimesEquals  -> Printf.fprintf oc " *= "
  | Ast.DivEquals    -> Printf.fprintf oc " /= "
  | Ast.ModEquals    -> Printf.fprintf oc " %%= "
  | Ast.LShiftEquals -> Printf.fprintf oc " <<= "
  | Ast.RShiftEquals -> Printf.fprintf oc " >>= "
  | Ast.AndEquals    -> Printf.fprintf oc " &= "
  | _ -> failwith ""
 
and code_for s1 e s2 sl inner oc = 
  match (e, s2) with 
  | None, None -> 
      begin
        Printf.fprintf oc "while (true) {\n";
        tab_levels := !tab_levels + 1;
        code_stmnts sl (Table.scope (Some inner) inner.return_type) oc;
        tab_levels := !tab_levels - 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
     end
  | None, Some s ->
      begin
        Printf.fprintf oc "while (true) {\n";
        tab_levels := !tab_levels + 1;
        code_stmnts sl (Table.scope (Some inner) inner.return_type) oc;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "try {\n";
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "} catch (Exception e) {\n";
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "} finally {\n";
        tab_levels := !tab_levels - 2;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        tab_levels := !tab_levels + 1;
        code_stmnt s inner oc;
        tab_levels := !tab_levels + 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
        tab_levels := !tab_levels - 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
      end        
  | Some ex, None -> 
      begin
        Printf.fprintf oc "while (";
        code_exp ex inner oc;
        Printf.fprintf oc ") {\n";
        tab_levels := !tab_levels + 1;
        code_stmnts sl (Table.scope (Some inner) inner.return_type) oc;
        tab_levels := !tab_levels - 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
     end
  | Some ex, Some s ->
      begin
        Printf.fprintf oc "while (";
        code_exp ex inner oc;
        Printf.fprintf oc ") {\n";
        tab_levels := !tab_levels + 1;
        code_stmnts sl (Table.scope (Some inner) inner.return_type) oc;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "try {\n";
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "} catch (Exception e) {\n";
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "} finally {\n";
        tab_levels := !tab_levels - 2;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        tab_levels := !tab_levels + 1;
        code_stmnt s inner oc;
        tab_levels := !tab_levels + 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
        tab_levels := !tab_levels - 1;
        Printf.fprintf oc "%s" (tabs !tab_levels);
        Printf.fprintf oc "}\n";
     end        

and code_switch eccl s e t oc tmp = 
  begin
    Printf.fprintf oc "%s" (tabs !tab_levels);
(*     tab_levels := !tab_levels + 1; *)
    Printf.fprintf oc "while (true) {\n";
    Printf.fprintf oc "%s" (tabs !tab_levels);
    begin
      match s with 
      | None -> ()
      | Some (init) -> code_simple_stmt init t oc
    end;
   (* code_decl (Ast.InferredVar (Ast.Ident "__tmp0__", e)) t false oc; *)
    let typ = get_underlying_type (Type.typeFromExp Type.Null e t false true) in
      code_type t false typ oc;
      Printf.fprintf oc "tmp = ";
(*       tab_levels := !tab_levels - 1; *)
      code_exp e t oc;
(*       tab_levels := !tab_levels + 1; *)
      Printf.fprintf oc ";\n";
    Printf.fprintf oc "%sif (false) {  }\n" (tabs !tab_levels);
    code_switch_eccl eccl t oc tmp true;
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "else {\n";
(*     tab_levels := !tab_levels + 1; *)
    begin
      match !defaultStatement with 
      | [Ast.Empty] -> code_simple_stmt Ast.Empty t oc
      | _ -> code_stmnts !defaultStatement t oc; defaultStatement := [Ast.Empty]
    end;
(*     tab_levels := !tab_levels - 1; *)
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "}\n";
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "break;\n";
(*     tab_levels := !tab_levels - 1; *)
    Printf.fprintf oc "%s" (tabs !tab_levels);
    Printf.fprintf oc "}\n";
  end

and code_switch_eccl eccl t oc tmp first = match eccl with 
  | Ast.EndList -> ()
  | Ast.ExprCase(ec, l) -> match ec with 
  | Ast.Exprcase(esc, sl) -> 
      begin
        match esc with 
        | Case(el) ->
            begin
              Printf.fprintf oc "%s" (tabs !tab_levels);
              if first then Printf.fprintf oc "if (" else Printf.fprintf oc "else if (";
              code_switch_esc esc t oc tmp; 
              Printf.fprintf oc ") {\n";
              tab_levels := !tab_levels + 1;
              code_stmnts sl (Table.scope (Some t) t.return_type) oc;
              tab_levels := !tab_levels - 1;
              Printf.fprintf oc "%s" (tabs !tab_levels);
              Printf.fprintf oc "}\n";
              code_switch_eccl l t oc tmp false;
            end
       | Default -> defaultStatement := sl; if first then code_switch_eccl l t oc tmp true else code_switch_eccl l t oc tmp false;
     end

and code_switch_esc esc t oc tmp = match esc with
  | Case(el) -> temp_exprs el t oc tmp; 
  | Default -> ()

and temp_exprs el t oc tmp = match el with 
  | [] -> ()
  | h :: [] ->
    begin 
      code_exp h t oc;
      Printf.fprintf oc " == %s" tmp;
      temp_exprs [] t oc tmp;
    end
  | h :: tl -> 
    begin 
      code_exp h t oc;
      Printf.fprintf oc " == %s || " tmp;
      temp_exprs tl t oc tmp;
    end

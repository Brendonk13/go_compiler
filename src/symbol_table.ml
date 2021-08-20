exception InvalidBlankId of string
exception RecursiveType of string
exception TypeDclErr of string*string
exception NoNewVars
exception InvalidExprStmt
exception VoidTypeErr
exception FuncAssign

module Type = Type_checker

module Table = Table_obj

let rec indent i str = match i with
  | 0 -> str
  | _ -> indent (i-1) ("\t" ^ str)


let struct_type_dcl (parent : Table.symbolTable) i id params =
    begin
        if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
        (* print_string ("found custom type (underlying is struct) with name: " ^ id ^ "\n"); *)
        (* let add_all = function *)
        (*     | (id,t)::tl -> *)
        (*                 Table.putSymbol parent.table (Table.Symbol(id, t)); *)

        (*match (Table.getSymbolInScope parent id) with
        | Some (name, typ) -> raise (Table.AlreadyDeclared (id))
        (* let typ = Type.type_of_string t parent in if Type.isType parent t then *)
        | None ->*)
                let params = Type.makeParams id params parent [] false false in
                (* aux params; *)
                (*     let underlying = Table.Struct([(new_param_id,inner)]) in *)
                (*     print_string ("underlying: " ^ (Type.string_of_types underlying) ^ "\n\n"); *)
                (*     Table.putSymbol parent.table (Table.Symbol(id, Table.CustomType(id,underlying))); *)
                (* ) *)
                (* else *)
                    (* print_string ("added name is: " ^ id ^ "\n"); *)
                    let underlying = (Table.Struct(params)) in

                    (* print_string "about to add id to table type struct\n"; *)
                    Table.putSymbol parent.table (Table.Symbol(id, Table.TypeDef(id,underlying)));
                if (Sys.argv.(1) = "symbol") then Printf.printf "%stype %s : underlying type %s\n" (indent i "") (id) (Type.string_of_types underlying);
    end


    (* called by undeclaredVar dcl when the undec var's type is a custom type with underlying type struct/struct array etc *)
let rec dec_underlying_struct (parent : Table.symbolTable) root_id acc is_root = function
    | [] -> List.rev acc
    | (p_id,_)::tl ->
            (* let p_id = (if is_root then (String.sub param_id 1 ((String.length param_id) - 1)) else param_id) in *)
            (* print_string ("in dec underlying, p_id is: " ^ p_id ^ "\n"); *)

            let (child_id,typ) = get_new_params parent root_id p_id is_root in
                Table.putSymbol parent.table (Table.Symbol(child_id, typ));
                (* print_string ("added child_id is: " ^ child_id ^ "\n"); *)
                dec_underlying_struct parent root_id ((child_id, typ)::acc) is_root tl
            (* let child_id = (id ^ "." ^ p_name) in *)
            (* let new_typ = get_new_params parent child_id *)

and get_new_params parent root_id p_id is_root =
    (* print_string ("in get new, p_id is: " ^ p_id ^ "\n"); *)
    (* print_string ( *)
    (* let p_id = (if is_root then ("."^pid) else pid) in *)
    match (Table.getSymbol parent p_id) with
    | Some(id,Table.CustomType(typ_id,Table.Struct(params))) ->
        (* print_string ("id: " ^ id ^ "\n"); *)
        (* print_string "fst\n"; *)
        let new_id = (if is_root then (root_id ^ id) else (root_id ^ "." ^ id)) in
        (new_id, (Table.Struct(dec_underlying_struct parent new_id [] false params)))
    | Some(id,Table.Struct(params)) ->
        (* print_string "snd\n"; *)
        (* print_string ("id: " ^ id ^ "\n"); *)
        let new_id = (if is_root then (root_id ^ id) else (root_id ^ "." ^ id)) in
        (new_id, (Table.Struct(dec_underlying_struct parent root_id [] false params)))

    | Some(id,Table.CustomType(typ_id,Table.Slice(Table.Struct(params),dim))) ->
        (* print_string ("id: " ^ id ^ "\n"); *)
        let new_id = (if is_root then (root_id ^ id) else (root_id ^ "." ^ id)) in
        (new_id, (Table.Slice(Table.Struct(dec_underlying_struct parent new_id [] false params),dim)))
    | Some(id,Table.Slice(Table.Struct(params),dim)) ->
        (* print_string "444\n"; *)
        (* print_string ("id: " ^ id ^ "\n"); *)
        let new_id = (if is_root then (root_id ^ id) else (root_id ^ "." ^ id)) in
        (new_id, (Table.Slice(Table.Struct(dec_underlying_struct parent root_id [] false params),dim)))

    | Some(id,Table.CustomType(typ_id,Table.Array(Table.Struct(params),dim))) ->
        (* print_string "555\n"; *)
        (* print_string ("id: " ^ id ^ "\n"); *)
        let new_id = (if is_root then (root_id ^ id) else (root_id ^ "." ^ id)) in
        (new_id, (Table.Array(Table.Struct(dec_underlying_struct parent new_id [] false params),dim)))
    | Some(id,Table.Array(Table.Struct(params),dim)) ->
        (* print_string ("id: " ^ id ^ "\n"); *)
        let new_id = (if is_root then (root_id ^ id) else (root_id ^ "." ^ id)) in
        (new_id, (Table.Array(Table.Struct(dec_underlying_struct parent root_id [] false params),dim)))

    | Some(id, typ) ->
        let new_id = (if is_root then (root_id ^ id) else (root_id ^ "." ^ id)) in
            (* print_string ("found non struct/arrstruct/sliceStruct with id: " ^ id ^ "\n"); *)
            (* print_string ("its type: " ^ (Type.string_of_types typ) ^ "\n"); *)
            (* if String.contains (Type.string_of_types typ) '{' then (print_string "asdasdas\n"; (new_id, Table.Struct(dec_underlying_struct parent root_id [] false *) 
        (* print_string ("id: " ^ id ^ "\n"); *)
        (new_id, typ)
    | None ->  raise (Type.TypeDclErr (root_id, p_id))



let rec symFromProg p i = 
  match p with
  | [] -> ()
  | h::t -> (if (Sys.argv.(1) = "symbol") then print_string "---Symbol Table---\n"; symFromStatments (h::t) (Table.initialize ()) i)

and symFromStatments stmnts parent i = 
  match stmnts with
  | h::t ->
    begin
      symFromStatment h parent i;
      symFromStatments t parent i;
    end
  | [] -> ()


and symFromStatment stmnt parent i = match stmnt with
  | Ast.Empty -> ()
  | Ast.Break -> ()
  | Ast.Continue -> ()
  | Ast.ExpressionStmt(e) -> if not(Type.isCast e parent) then let _ = (Type.typeFromExp Null e parent false false) in () else raise(InvalidExprStmt)
  | Ast.Return(e) -> 
      begin
        match e with 
        | None -> 
            begin
              match parent.return_type with
              | None -> ()
              | Some(rt) -> if not(rt = Table.Void) then raise (Table.ReturnMismatch(Type.string_of_types Table.Void, Type.string_of_types rt))
            end
        | Some(ex) -> 
            begin
              match parent.return_type with
              | None -> ()
              | Some(rt) -> if not(Type.typeFromExp Null ex parent false false = rt) 
                            then raise (Table.ReturnMismatch(Type.string_of_types (Type.typeFromExp Null ex parent false false), Type.string_of_types rt))
            end
      end
  | Ast.DeclStmt d -> (match d with 
                      | [] -> symFromDecls d parent i false
                      | h :: t -> (Type.shortAssignDup d []) ;let _ = noNewVars := true in symFromDecls d parent i false)
  | Ast.PackageDcl id -> if id = "_" then raise (InvalidBlankId("Package name cannot be the blank identifier."))
  | Ast.FuncDcl(id,args,rt,sl) ->
      begin
        (Type.checkSpecialFunc id args (Type.type_of_string rt parent));
        match (Table.getSymbol parent id) with
          | Some (name, typ) -> raise (Table.AlreadyDeclared (id))
          | None -> let al = Type.makeParams id args parent [] false false in if (id = "init") then () else (Table.putSymbol (parent.table) (Table.Symbol(id,Table.Func(al,(Type.type_of_string rt parent)))));
          if (Sys.argv.(1) = "symbol") then Printf.printf "%sfunc %s : %s\n" (indent i "") (id) (Type.string_of_types (Table.Func(al,(Type.type_of_string rt parent))));
          let inner = Table.scope (Some parent) (Some (Type.type_of_string rt parent)) in symFromDecls args inner (i+1) true; symFromStatments sl inner (i+1);
      end
  | Ast.ForInf (sl) -> let inner = Table.scope (Some parent) parent.return_type in symFromStatments sl inner (i+1)
  | Ast.ForWhile (e, s) ->
      if (Type.isBool(Type.typeFromExp Null e parent false false)) 
      then let inner = Table.scope (Some parent) parent.return_type in
      symFromStatments s inner (i+1)
      else raise (Table.ForIfExpMismatch (Type.string_of_types(Type.typeFromExp Null e parent false false)))
  | Ast.For (s1, e, s2, sl) -> 
  let inner = Table.scope (Some parent) parent.return_type in let _ = symFromStatment s1 inner i in
  begin
    match (e, s2) with
    | None, None -> ()
    | None, Some s -> symFromStatment s inner i
    | Some ex, None -> 
        if (Type.isBool(Type.typeFromExp Null ex inner false false)) then ()
        else raise (Table.ForIfExpMismatch (Type.string_of_types(Type.typeFromExp Null ex inner false false))) 
    | Some ex, Some s -> 
      if (Type.isBool(Type.typeFromExp Null ex inner false false)) then symFromStatment s inner i
      else raise (Table.ForIfExpMismatch (Type.string_of_types(Type.typeFromExp Null ex inner false false))) 
  end; 
  let innerblock = Table.scope (Some inner) parent.return_type in symFromStatments sl innerblock (i+1)
  | Ast.Assign (dl) -> symFromAssigns dl parent i
  | Ast.Assignop(l,op,r) -> Type.checkAssignops l op r parent
  | Ast.BlockStmt(sl) -> let inner = Table.scope (Some parent) parent.return_type in symFromStatments sl inner (i+1)
  | Ast.Print(el) | Ast.Println(el) -> 
      begin
        match el with 
        | None -> ()
        | Some(ell) -> 
        let _ = List.map (fun x -> 
          let t = Type.typeFromExp Null x parent false false in
          begin
            if (Type.isNumericOrString t || Type.isBool t)
            then ()
            else raise (Table.PrintBaseType(Type.string_of_types t))
          end ) ell in ()
      end
  | Ast.If(e,sl,elif) -> symFromIf e sl elif parent i
  | Ast.IfSimp(s, e, sl, elif) -> let inner = Table.scope (Some parent) parent.return_type in 
                                  symFromStatment s inner i; symFromIf e sl elif inner (i+1)
  | Ast.Switch(eccl) -> symFromExpressionClauses eccl Table.Bool parent i
  | Ast.SwitchSimpExpr(s, e, eccl) -> let inner = Table.scope (Some parent) parent.return_type in
                                      let _ = symFromStatment s inner i in 
                                      let typ = Type.typeFromExp Null e inner false false in 
                                      symFromExpressionClauses eccl typ inner i
  | Ast.SwitchExpr(e, eccl) -> let typ = Type.typeFromExp Null e parent false false in if typ = Table.Void then raise(VoidTypeErr) else symFromExpressionClauses eccl typ parent i
  | Ast.SwitchSimp(s, eccl) -> let inner = Table.scope (Some parent) parent.return_type in let _ = symFromStatment s inner i in symFromExpressionClauses eccl Table.Bool inner i
  | Ast.Increment(e) -> let t = Type.typeFromExp Null e parent false false in
                        begin 
                          if (Type.isNumeric t) && (Type.isIdent e) then let _ = Type.checkBinops t Ast.Add t in ()
                          else raise (Table.IncDecError(Type.string_of_types t))  
                        end
  | Ast.Decrement(e) -> let t = Type.typeFromExp Null e parent false false in
                        begin 
                          if (Type.isNumeric t) && (Type.isIdent e) then let _ = Type.checkBinops t Ast.Sub t in ()
                          else raise (Table.IncDecError(Type.string_of_types t))  
                        end

and symFromExpressionClauses eccl typ parent i = match eccl with
  | Ast.EndList -> ()
  | Ast.ExprCase(ec, l) -> match ec with 
    | Ast.Exprcase(esc, sl) -> (symFromExpressionClauses l typ parent i);(symFromExpressionClause esc typ parent i); 
    let inner = Table.scope (Some parent) parent.return_type in symFromStatments sl inner (i+1)

and symFromExpressionClause esc typ parent i = match esc with
  | Case(el) ->  let _ = List.map (fun x -> if not((Type.typeFromExp Null x parent false false) = typ) || (Type.isSlice (Type.typeFromExp Null x parent false false))
                then raise (Table.SwitchMismatch(Type.string_of_types typ, Type.string_of_types (Type.typeFromExp Null x parent false false)))) el in ()
  | Default -> ()



and symFromAssigns dl parent i =
  match dl with
  | [] -> ()
  | h::t ->
    begin
      symFromAssign h parent i;
      symFromAssigns t parent i;
    end 

and symFromAssign d parent i = match d with 
  | Ast.InferredVar (var_usage, e) ->
        (let _ = Type.checkAssign var_usage e parent in ());
        let full_id = (Type.nameFromVarUsage var_usage) in
        let id = (Type.remove_arr_part (Type.getRootStruct full_id)) in
        (* (let _ = Type.checkAssign e1 e2 parent in ()); *)
    (* print_string ("id in assign inferred var is: " ^ id ^ "\n"); *)
      if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
        begin
            match (Table.getSymbol parent id) with
            (* dont assign values to func calls ! *)
            | Some (name, (Table.Func(_,_))) -> ()
            | Some (name, (Table.CustomType(_, typ))) ->
                    let thetype = Type.typeFromExp Null e parent false false
                    in
                    Table.forcePut parent.table (Table.Symbol(full_id, thetype));
                    if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") (full_id) (Type.string_of_types thetype)
            | Some (name, typ) ->
                    let thetype = Type.typeFromExp Null e parent false false
                    in
                    Table.forcePut parent.table (Table.Symbol(full_id, thetype));
                    if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") (full_id) (Type.string_of_types thetype)
            | None             -> if (id = "_") then () else raise (Type.IdentifierNotFound id)

        end

  | _ -> failwith ""

and noNewVars = ref false

and symFromDecls dcls parent i funargs =
  match dcls with
  | [] -> if (!noNewVars) then raise NoNewVars else ()
  | h::t ->
    begin
      symFromDecl h parent i funargs;
      symFromDecls t parent i funargs;
    end;


and symFromDecl d parent i funargs = 
  match d with
  | Ast.TypedVar (id, t, e) ->
      begin
        if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
        match (Table.getSymbolInScope parent id) with
        | Some (name, typ) -> raise (Table.AlreadyDeclared (id))
        | None -> let typ = Type.type_of_string t parent in ((if Type.isType parent t then (Table.putSymbol parent.table (Table.Symbol(id, typ))) else raise((Type.TypeDclErr(id,t))); Type.checkDcl t e parent));
        if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") (id) (Type.string_of_types (Type.type_of_string t parent));
      end; noNewVars := false

  | Ast.InferredVar (var_usage, e) ->
    begin
        let id = (Type.nameFromVarUsage var_usage) in
    (* print_string ("id in inferred var is: " ^ id ^ "\n"); *)
      if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
      match (Table.getSymbolInScope parent id) with
      | Some (name, typ) -> raise (Table.AlreadyDeclared (id))
      | None -> (let thetype = Type.typeFromExp Null e parent false false
        in if thetype = Table.Void then raise(VoidTypeErr) else if Type.isFunc (Some(id,thetype)) then raise(FuncAssign) else if Type.isFunc (Table.getSymbol parent id) then raise(FuncAssign) else
        Table.putSymbol parent.table (Table.Symbol(id, thetype));
        if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") (id) (Type.string_of_types thetype);
      )
        end; noNewVars := false

  | Ast.ShortAssign(var_usage, e) ->
    begin
    let id = Type.nameFromVarUsage var_usage in
    let thetype = Type.typeFromExp Null e parent false false
    in if thetype = Table.Void then raise(VoidTypeErr) else if Type.isFunc (Some(id,thetype)) then raise(FuncAssign) else if Type.isFunc (Table.getSymbol parent id) then raise(FuncAssign) else
      if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
      match (Table.getSymbolInScope parent id) with
      | Some (name, typ) -> Table.forcePut parent.table (Table.Symbol(id, thetype));
      | None -> (Table.forcePut parent.table (Table.Symbol(id, thetype)); if not(id="_") then noNewVars := false;
        if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") (id) (Type.string_of_types thetype);)
    end

  | Ast.UndeclaredVar (id, t) ->
    begin
      if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
      (* check if underlying type of t is struct *)
      (* print_string "undec \n"; *)
      let parent_table = ( match parent.parent with
            | Some(p) -> p
            | None -> Table.scope None None (* Empty scope used only for func args. This case should never have to be used since functions open a new scope. *)
      ) in
      let finished = (
            match (Table.getSymbolInScope parent t) with
            | Some(_, Table.TypeDef(name,Table.Struct(params))) ->
                    let new_type = (Table.CustomType(name,Table.Struct(params))) in
                    Table.putSymbol parent.table (Table.Symbol(id, new_type));
                    if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : %s\n" (indent i "") (id) (Type.string_of_types new_type);
                    true

            | Some(_, Table.CustomType(name,(Table.Slice(Table.Struct(params),dim)))) ->
                    let new_type = (Table.CustomType(name,Table.Slice(Table.Struct(params),dim))) in
                    Table.putSymbol parent.table (Table.Symbol(id, new_type));
                    if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : %s\n" (indent i "") (id) (Type.string_of_types new_type);
                    true

            | Some(_, Table.CustomType(name,(Table.Array(Table.Struct(params),dims)))) ->
                    let new_type = (Table.CustomType(name,Table.Array(Table.Struct(params),dims))) in
                    Table.putSymbol parent.table (Table.Symbol(id, new_type));

                    if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : %s\n" (indent i "") (id) (Type.string_of_types new_type);
                    true
            | Some(_,_) -> false
            | None -> false
            (* traverse params and prepend "id" to every struct field *)
      ) in

      if finished = false then
        match (Table.getSymbolInScope parent id) with
        | Some (name, typ) -> raise (Table.AlreadyDeclared (id))
        | None -> let typ = Type.type_of_string t parent in  if Type.isType (if funargs then parent_table else parent) t then (Table.putSymbol parent.table (Table.Symbol(id, typ))) else raise((Type.TypeDclErr(id,t)));
        if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") (id) (Type.string_of_types (Type.type_of_string t parent));
    end; noNewVars := false

  | Ast.TypeDcl (id, t) ->
    begin
      if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
      (* print_string ("found custom with name: " ^ id ^ "\n"); *)
      if (id = t) then raise (RecursiveType("Cannot create recursive type: " ^ id ^ " -> " ^ t ^ "."));
      match (Table.getSymbolInScope parent id) with
      | Some (name, typ) -> raise (Table.AlreadyDeclared (id))
      | None -> let typ = Type.type_of_string t parent in if Type.isType parent t then Table.putSymbol parent.table (Table.Symbol(id, Table.TypeDef(id,typ))) else raise (Type.TypeDclErr(id,t));
      if (Sys.argv.(1) = "symbol") then Printf.printf "%stype %s : underlying type %s\n" (indent i "") (id) (Type.string_of_types (Type.type_of_string t parent));
    end; noNewVars := false

  | Ast.SliceDcl (id, t, dim) ->
    begin
      if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
      match (Table.getSymbolInScope parent id) with
      | Some (name, typ) -> raise (Table.AlreadyDeclared (id))
      | None -> let typ = Type.type_of_string t parent in if Type.isType parent t then (Table.putSymbol parent.table (Table.Symbol(id, Table.Slice(typ,dim)))) else raise((Type.TypeDclErr(id,t)));
      if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") (id) (Type.string_of_types (Table.Slice((Type.type_of_string t parent),dim)));
    end; noNewVars := false

  | Ast.ArrayDcl (id, t, dims) ->
    begin
      if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
      (* print_string "found an array declaration\n"; *)
      match (Table.getSymbolInScope parent id) with
      | Some (name, typ) -> raise (Table.AlreadyDeclared (id))
      | None -> let typ = Type.type_of_string t parent in if Type.isType parent t then (Table.putSymbol parent.table (Table.Symbol(id, Table.Array(typ, dims)))) else raise((Type.TypeDclErr(id,t)));
      if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") (id) (Type.string_of_types (Table.Array((Type.type_of_string t parent), dims)));
    end; noNewVars := false


  | Ast.StructDcl (id, params) ->
    begin
      if (id = "main" || id = "init") && (parent.parent = None) then raise (Type.SpecialFuncErr("Identifier " ^ id ^ " can only be declared as a function at the toplevel."));
      if (id.[0] == '-') then let real_id = String.sub id 1 ((String.length id) - 1) in
            (* print_string "right place\n"; *)
            struct_type_dcl parent i real_id params
      else
            (* print_string ("found struct with id: " ^ id ^ "\n"); *)
            match (Table.getSymbolInScope parent id) with
            | Some (name, typ) -> raise (Table.AlreadyDeclared (id))
            (* all stored_params have already been added to the symbol table *)
            | None -> let stored_params = Type.makeParams id params parent [] false false in Table.putSymbol parent.table (Table.Symbol(id, Table.Struct(stored_params)));
            if (Sys.argv.(1) = "symbol") then Printf.printf "%stype %s : %s\n" (indent i "") (id) (Type.string_of_types (Table.Struct(stored_params)));
    end; noNewVars := false

  | Ast.SliceStructDcl (id, arr, struckt)
  | Ast.ArrayStructDcl (id, arr, struckt) ->
    begin
      match (Table.getSymbolInScope parent id) with
      | Some (name, typ) -> raise (Table.AlreadyDeclared id)
      | None             -> let typ = (Type.type_from_arrType_struct id arr struckt parent) in
                                Table.putSymbol parent.table (Table.Symbol(id, typ));
      if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") id (Type.string_of_types typ)
      (* if (Sys.argv.(1) = "symbol") then Printf.printf "%svar %s : type %s\n" (indent i "") id (Type.string_of_types (Type.type_from_arrType_struct id arr struckt parent false)) *)
    end; noNewVars := false

  (* | _ -> failwith "ADSasd" *)


and symFromIf e s eif parent i = 
    if (Type.isBool(Type.typeFromExp Null e parent false false))
    then let inner = Table.scope (Some parent) parent.return_type in
    (symFromStatments s inner i; symFromElseIf eif parent i)
    else raise (Table.ForIfExpMismatch (Type.string_of_types(Type.typeFromExp Null e parent false false)))

and symFromElseIf eif parent i = 
  match eif with
  | Ast.ElseIf s -> let inner = Table.scope (Some parent) parent.return_type in symFromStatment s inner i
  | Ast.Else sl -> let inner = Table.scope (Some parent) parent.return_type in symFromStatments sl inner i
  | Ast.EndElse -> ()


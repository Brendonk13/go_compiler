exception IdentifierNotFound of string
exception BinopMismatch of string * string * string
exception SpecialFuncErr of string
exception DuplicateParam of string
(* used when user uses wrong number of [] according to definition *)
exception ArrayDimError of int*int*string
exception NotCompoundStruct of string
exception ArrayDimMismatch of string*string
exception NonIntIndex of string*string
exception NotIndexable of string*string
exception InvalidCast of string*string*string
exception TypeDclErr of string*string
exception AppendError of string*string*string
exception NotExpr of string
(* exception BuiltInError of string *)
exception LenError of string*string*string
exception CapError of string*string*string
exception FuncArgsMismatch of string*string*string
exception WrongNumArgs of string*int*int
exception FuncArrMisuse of string
exception ShadowError of string
exception BadParam of string*string


module Table = Table_obj;;

type field_id = | Null | Id of string

let isBuiltin s = let typ = (
    if (String.length s > 1 && s.[0] = '[') then (
        let start = (String.rindex s ']')+1 in
        String.sub s start ((String.length s) - start)
    )
    else s
) in
    List.mem typ ["int";"float64";"rune";"bool";"string"]


(* -------------------------------------------------- START of conversion methods, ie ast -> symbol table ------------------------------------------------------- *)

let rec nameFromVarUsage e =
  match e with 
  | Ast.Ident v                -> v
  | Ast.Field (id, field)      -> (let x = (nameFromVarUsage id) in x ^ "." ^ (nameFromVarUsage field))
  | Ast.Index (var, idx)       -> (var ^ "[" ^ (String.concat "[" (List.map (fun x -> ((nameFromVarUsage x) ^ "]")) idx)))
  | Ast.Int x                  -> string_of_int x
  | Ast.Float x                -> string_of_float x
  | Ast.String x               -> x
  | Ast.StringLit x            -> x
  | Ast.ArrayField (index, field) -> (let full_name = nameFromVarUsage field in
                        (* we need to strip the variable name from full_name since the var name *)
                        (* should appear in the idx: x[0].y  (y 's var name is also x so we don't repeat that) *)
                                      let idx = nameFromVarUsage index in
                                      let name = (
                                      try
                                            let start = String.index full_name '.' in
                                            String.sub full_name start ((String.length full_name) - start)
                                      with _ ->
                                          full_name
                                      ) in
                                        idx ^ "." ^ name)
  | Ast.Parens x 			   -> nameFromVarUsage x
  | Ast.FuncCallIdent (exp,_,_) -> nameFromVarUsage exp
  | _                          -> ""

and nameFromDcl = function
    | Ast.InferredVar (exp,_) -> nameFromVarUsage exp
	| Ast.TypedVar (id,_,_) -> id
	| Ast.UndeclaredVar (id,_) -> id
	| Ast.ShortAssign (exp,_) ->nameFromVarUsage exp
	| Ast.TypeDcl (id,_) -> id
	| Ast.StructDcl (id,_) -> id
(* stores the dimension of slice ie # of ['s in declaration *)
	| Ast.SliceDcl (id,_,_) -> id
    (* stores the dimension of each array ie decl var z [3][4]int stored as ("z", "int", [3; 4]) *)
	| Ast.ArrayDcl (id,_,_) -> id
	| Ast.ArrayStructDcl (id,_,_) -> id
	| Ast.SliceStructDcl (id,_,_) -> id



let rec type_of_string s tbl = if s = "" then Table.Void else if s.[0] = '[' then getCompundType s tbl else match s with
  | "int"       -> (match (Table.getSymbol tbl s) with
                      | Some(name,typ) -> typ
                      | None -> Table.Int)
  | "float64"   -> (match (Table.getSymbol tbl s) with
                      | Some(name,typ) -> typ
                      | None -> Table.Float)
  | "float"     -> (match (Table.getSymbol tbl s) with
                      | Some(name,typ) -> typ
                      | None -> Table.Float)
  | "bool"      -> (match (Table.getSymbol tbl s) with
                      | Some(name,typ) -> typ
                      | None -> Table.Bool)
  | "string"    -> (match (Table.getSymbol tbl s) with
                      | Some(name,typ) -> typ
                      | None -> Table.String)
  | "stringlit" -> Table.String
  | "rune"      -> (match (Table.getSymbol tbl s) with
                      | Some(name,typ) -> typ
                      | None -> Table.Rune)
  | _           ->  (match (Table.getSymbol tbl s) with
                      | Some(name,typ) -> (match typ with
                                            | Table.CustomType(_,_) -> typ
                                            | Table.TypeDef(n,t) -> Table.CustomType(n,t)
                                            | _ as t -> Table.CustomType("_",t))
                      | None ->  raise (IdentifierNotFound(s)))
and getCompundType s tbl = match s.[1] with
  | ']' -> let (dim,_,typ) = makeCompoundType (String.split_on_char ']' s) tbl 0 [] Table.Void in Table.Slice(typ,dim)
  | _ as l -> let (_,dims,typ) = makeCompoundType (String.split_on_char ']' s) tbl 0 [] Table.Void in Table.Array(typ,dims)
and makeCompoundType l tbl ct dims typ  = match l with
  | [] -> (ct,dims,typ)
  | h::t -> (match h with
               | "[" -> makeCompoundType t tbl (ct+1) dims typ
               | _ -> if h.[0] = '[' then makeCompoundType t tbl ct (dims @ [int_of_string (String.sub h 1 ((String.length h) - 1))]) typ else makeCompoundType t tbl ct dims (type_of_string h tbl))


let rec string_of_types t =
  match t with
  | Table.Int            -> "int"
  | Table.Float          -> "float"
  | Table.Bool           -> "bool"
  | Table.String         -> "string"
  | Table.Rune           -> "rune"
  | Table.Void           -> "void"
  | Table.CustomType(id, ((Table.Struct(_)) as typ))  -> (string_of_types typ)
  | Table.CustomType(id, typ)  -> (string_of_types typ)
  (* | Table.TypeDef(_, typ)  -> string_of_types typ *)
  | Table.TypeDef(id, _)  -> id
  | Table.Slice(t,dim)       ->  (Ast.pretty_print_slice dim) ^ (string_of_types t)
  | Table.Array(t,dims)     -> (let r = ((String.concat "" (List.map (fun dim -> ("[" ^ (string_of_int dim) ^ "]")) dims)) ^ (string_of_types t)) in r)
  | Table.Struct(params) -> "struct {" ^ (List.fold_left (fun s (p,t) -> if s = "" then s ^ p ^ " : " ^ (string_of_types t) else s ^ ", " ^ p ^ " : " ^ (string_of_types t)) "" params) ^ "}"
  | Table.Func(args,rt)  -> "func (" ^ (List.fold_left (fun s (a,t) -> if s = "" then s ^ a ^ " : " ^ (string_of_types t) else s ^ ", " ^ a ^ " : " ^ (string_of_types t)) "" args) ^ ") " ^ (string_of_types rt)


(* -------------------------------------------------- START of conversion methods, ie ast -> symbol table ------------------------------------------------------- *)

let rec isIdent exp =
    match exp with
    | Ast.Ident(_) -> true
    | Ast.Parens(x) -> isIdent x
    | Ast.Index(x,_) -> true
    | _ -> false

let rec isNumericOrString exp =
	match exp with 
	| Table.Int | Table.Float | Table.Rune | Table.String  -> true
	| Table.TypeDef(id,t)                                  -> isNumericOrString t
	| Table.CustomType(_,t)                                -> isNumericOrString t
    | _                                                    -> false

let rec isNumeric exp =
	match exp with 
	| Table.Int | Table.Float | Table.Rune                 -> true
	| Table.TypeDef(id,t)                                  -> isNumeric t
	| Table.CustomType(_,t)                                -> isNumeric t
    | _                                                    -> false

let rec isBool exp =
	match exp with 
	| Table.Bool                                           -> true
	| Table.TypeDef(id,t)                                  -> isBool t
	| Table.CustomType(_,t)                                -> isBool t
    | _                                                    -> false

let rec isIntorRune exp =
	match exp with 
	| Table.Int | Table.Rune                               -> true
	| Table.TypeDef(id,t)                                  -> isIntorRune t
	| Table.CustomType(_,t)                                -> isIntorRune t
    | _                                                    -> false

let rec isString_arrType exp =
	match exp with
	| Table.String _ | Table.Array(_,_) | Table.Slice(_,_) -> true
	| Table.TypeDef(id,t)                                  -> isString_arrType t
	| Table.CustomType(_,t)                                -> isString_arrType t
    | _                                                    -> false


let isArrType exp =
	match exp with
    | Table.Slice(_,_) | Table.Array(_,_)                  -> true
	| Table.TypeDef(id,t)                                  -> isString_arrType t
	| Table.CustomType(_,t)                                -> isString_arrType t
    | _                                                    -> false

let isSlice exp =
    match exp with
    | Table.Slice(_,_) -> true
    | _ -> false

let isFuncCall exp =
    match exp with
    | Ast.FuncCallIdent(_,_,_) -> true
    | _ -> false

let isFunc exp =
    match exp with
    | Some(_,Table.Func(_,_)) -> true
    | _ -> false

let isType tbl t = let typ = String.split_on_char ']' t in match (Table.getSymbol tbl (List.nth typ ((List.length typ) - 1))) with
  | Some(id,s) -> (match s with
                  | Table.TypeDef(_,_) -> true
                  | Table.CustomType(_,Table.Struct(_)) -> true
                  | _ -> false)
  | None -> if isBuiltin (List.nth typ ((List.length typ) - 1)) then true else false

let isCast exp tbl =
    match exp with
    | Ast.Cast(n,_) -> not(isFunc (Table.getSymbol tbl (nameFromVarUsage n)))
    | Ast.FuncCallIdent(n,a,_) -> if (isType tbl (nameFromVarUsage n)) && ((List.length a) = 1) then true else false
    | _ -> false

let string_of_binop op = 
  match op with
  | Ast.Add           -> "+"
  | Ast.Sub           -> "-"
  | Ast.Mult          -> "*"
  | Ast.Div           -> "/"
  | Ast.Mod           -> "%"
  | Ast.BitAnd        -> "&"
  | Ast.BitAndNot     -> "&^"
  | Ast.BitOr         -> "|"
  | Ast.BitXorBin     -> "^"
  | Ast.BitShiftLeft  -> "<<"
  | Ast.BitShiftRight -> ">>"
  | Ast.And           -> "&&"
  | Ast.Or            -> "||"
  | Ast.Eq            -> "=="
  | Ast.Neq           -> "!="
  | Ast.Lt            -> "<"
  | Ast.Gt            -> ">"
  | Ast.Leq           -> "<="
  | Ast.Geq           -> ">="


let checkBinops t1 op t2 = 
    match op with
    | Ast.Lt | Ast.Leq | Ast.Gt | Ast.Geq ->
                if (t1 = t2 && isNumericOrString t1) then Table.Bool
                else raise (BinopMismatch (string_of_types t1, string_of_binop op, string_of_types t2))

    | Ast.And | Ast.Or ->
                if (t1 = t2 && isBool t1) then t1
                else raise (BinopMismatch (string_of_types t1, string_of_binop op, string_of_types t2))

    | Ast.Add -> if (t1 = t2 && isNumericOrString t1) then t1
                else raise (BinopMismatch (string_of_types t1, string_of_binop op, string_of_types t2))

    | Ast.Sub | Ast.Mult | Ast.Div ->
                if (t1 = t2 && isNumeric t1) then t1
                else raise (BinopMismatch (string_of_types t1, string_of_binop op, string_of_types t2))

    | Ast.Mod | Ast.BitAnd | Ast.BitAndNot | Ast.BitOr
    | Ast.BitXorBin | Ast.BitShiftRight | Ast.BitShiftLeft ->
                if (t1 = t2 && isIntorRune t1) then t1
                else raise (BinopMismatch (string_of_types t1, string_of_binop op, string_of_types t2))

    | Ast.Eq | Ast.Neq ->
                if (t1 = t2) && not(isSlice t1) then Table.Bool
                else raise (BinopMismatch (string_of_types t1, string_of_binop op, string_of_types t2))

let checkUnops op t =
  match op with
  | Ast.UMinus   -> if (isNumeric t) then t else raise (Table.TypeMismatch ("Int || Float", string_of_types t))
  | Ast.Not      -> if (isBool t) then t else raise (Table.TypeMismatch (string_of_types Bool, string_of_types t))
  | Ast.Identity -> if (isNumeric t) then t else raise (Table.TypeMismatch ("Int || Float", string_of_types t))
  | Ast.BitXor   -> if (isIntorRune t) then t else raise (Table.TypeMismatch ("Int", string_of_types t))


(* -------------------------------------------------- END of conversion methods, ie ast -> symbol table ------------------------------------------------------- *)


(* -------------------------------------------------- START of field/array/struct helpers ------------------------------------------------------ *)
let extract_tbl_struct_params name = function
    | Table.Struct (params) -> params
    | _                     -> raise (Table.NotStruct (name, ""))

let extract_struct_params name = function
    | Ast.StructDcl (id, params) -> params
    | _                          -> raise (Table.NotStruct (name, ""))

let arr_dim_from_table = function
    | Table.Slice(typ, dim)  -> dim
    | Table.Array(typ, dims) -> List.length dims
    | _                      -> raise (Ast.ParseError "Should not happen")

let getAllFields var =
    try
        String.concat "." (List.tl (String.split_on_char '.' var))
    with _ ->
        ""

let getRootStruct var = 
    try
        List.hd (String.split_on_char '.' var)
    with _ ->
        var

let get_arr_part id =
    try
        let start = (String.index id '[') + 1 in
        String.sub id start ((String.length id) - start)
    with _ ->
        ""

(* returns a list of strings used between '[' and ']' *)
let get_names_in_arr_part id =
    let rec extract acc = function
    | "" -> List.rev acc
    | _ as s ->
        begin
        try
            let start = (String.index s '[') + 1 in
            let last = (String.index s ']') in
            let extracted = String.sub s start (last-start) in
            extract (extracted::acc) (String.sub s (last + 1) ((String.length s) - (last + 1)))
        with _ -> List.rev acc
        end
    in
    let arr_part = ("[" ^ get_arr_part id) in
    extract [] arr_part


let remove_arr_part id =
    try
        let start = String.index id '[' in
        String.sub id 0 start
    with _ ->
        id

(* this function returns all the fields without any indices *)
let all_clean exp =
    List.fold_left (fun x y -> x ^ (remove_arr_part y)) "" (String.split_on_char ']' exp)

let getNextField full_field =
    let ret = getAllFields full_field in
    if ret = "" then full_field
    else getRootStruct full_field

let extract_arrType_struct_params name = function
    | Table.Array(typ, dims) -> extract_tbl_struct_params name typ
    | Table.Slice(typ, dim)  -> extract_tbl_struct_params name typ
    | _ as t ->  raise (NotCompoundStruct name)


(* ----------------- helpers for when the arrtype = "[0][0.0][10]" ---------------------------------------------------------- *)
let arr_dim_from_string arr_type =
        List.fold_left (fun ct str -> if (String.contains str ']') then (ct+1) else ct) 0 (String.split_on_char '[' arr_type)

let check_all_int arr_type_string =
    let isIntorRune inp =
    try
        (let _ = int_of_string inp in ());
        true
    with _ as e->
        false
    in
        let rec check = function
            | hd::tl -> (if ((String.length hd) < 1) then check tl
                            else if (isIntorRune (String.sub hd 0 ((String.length hd) - 1) )) then  check tl
                            else  false)
        | [] -> true
        in
check (String.split_on_char '[' arr_type_string)

let get_indices = function
    | Ast.Index(_,idx) -> idx
    | _ -> (print_string "only call this from field when id is an index\n"; [Ast.Int 10])

(* -------------------------------------------------- END of field/array/struct helpers ------------------------------------------------------ *)

(* ---------- typeFromExp helpers -------------------------- *)
let extract_parent = function
    | Null -> ""
    | Id x -> x

let get_new_parent exp the_parent_id =
        let get_new new_child = function
            | Null -> Id (new_child)
            | Id s -> Id ( s ^ "." ^ new_child)
        in
    let new_root = (nameFromVarUsage exp) in
        let tmp = remove_arr_part (getRootStruct new_root) in
        get_new tmp the_parent_id
        (* extract_parent tmp the_parent_id *)


let rec typeFromExp parent_id e parent is_append from_gen =
  match e with
  | Ast.Ident v ->
    begin
        let used_id = (if parent_id != Null then (extract_parent parent_id) else v) in
        match (Table.getSymbol parent used_id) with
        | Some (name, Table.TypeDef(_,_)) -> raise(NotExpr(used_id))
        (* | Some (name, ((Table.Func(_,_)) as fun)) -> raise(NotExpr(used_id)) *)
        | Some (name, ((Table.Func(_,_)) as func)) -> func
        | Some (name, typ) -> typ
        | None ->
        (
            if (v="true") || (v="false") then Table.Bool else
            if (not from_gen) then raise (IdentifierNotFound used_id)
            else
            (match v with
            | "string"    -> Table.String
            | "int"       -> Table.Int
            | "float64"   -> Table.Float
            | "stringlit" -> Table.String
            | "rune"      -> Table.Rune
            | _           -> raise (IdentifierNotFound used_id)
            )
        )
    end
(* strings and stringlit both have type string *)
  | Ast.StringLit s
  | Ast.String s           -> Table.String
  | Ast.Parens x           -> typeFromExp parent_id x parent is_append from_gen
  | Ast.Int i              -> Table.Int
  | Ast.Float f            -> Table.Float
  | Ast.Bool b             -> Table.Bool
  | Ast.Rune b             -> Table.Rune
  | Ast.BinOp (op, e1, e2) -> checkBinops (typeFromExp parent_id e1 parent is_append from_gen) op (typeFromExp parent_id e2 parent is_append from_gen)
  | Ast.UnaryOp (op, exp)  -> checkUnops op (typeFromExp parent_id exp parent is_append from_gen)

  | Ast.ArrayField (id, field)
  | Ast.Field (id, field)  ->
          let nameFromUse = (nameFromVarUsage e) in
          let parts = String.split_on_char '.' (all_clean nameFromUse) in
            let name = (List.hd parts) in
            (match Table.getSymbol parent name with
                | Some(_,Table.CustomType(_,Table.Struct(params)))                     -> get_field_type name (List.tl parts) params
                | Some(_,Table.CustomType(_,Table.CustomType(_,Table.Struct(params)))) -> get_field_type name (List.tl parts) params
                | Some(_,Table.Struct(params))                                         -> get_field_type name (List.tl parts) params
                | Some(_,Table.CustomType(_,Table.Array(Table.Struct(params),dim)))                     ->
                        begin
                        (let _ = make_idx id nameFromUse parent_id parent is_append from_gen in ());
                        get_field_type name (List.tl parts) params
                        end
                | Some(_,Table.CustomType(_,Table.Slice(Table.Struct(params),dim)))                     ->
                        begin
                        (let _ = make_idx id nameFromUse parent_id parent is_append from_gen in ());
                        get_field_type name (List.tl parts) params
                        end
                | Some(_,Table.CustomType(_,Table.CustomType(_,Table.Array(Table.Struct(params),dim)))) ->
                        begin
                        (let _ = make_idx id nameFromUse parent_id parent is_append from_gen in ());
                        get_field_type name (List.tl parts) params
                        end
                | Some(_,Table.CustomType(_,Table.CustomType(_,Table.Slice(Table.Struct(params),dim)))) ->
                        begin
                        (let _ = make_idx id nameFromUse parent_id parent is_append from_gen in ());
                        get_field_type name (List.tl parts) params
                        end
                | Some(_,Table.Array(Table.Struct(params),dim))                                         ->
                        begin
                        (let _ = make_idx id nameFromUse parent_id parent is_append from_gen in ());
                        get_field_type name (List.tl parts) params
                        end
                | Some(_,Table.Slice(Table.Struct(params),dim))                                         ->
                        begin
                        (let _ = make_idx id nameFromUse parent_id parent is_append from_gen in ());
                        get_field_type name (List.tl parts) params
                        end
                | Some(_,t) -> (print_string ("bad type: " ^ (string_of_types t) ^ "\n");
                raise(Table.NotStruct(name, (nameFromVarUsage e))))
                | None -> raise(Table.NotStruct(name, (nameFromVarUsage e))))
  | Ast.Index (id, idx)   ->
            begin
                let used_id = (if parent_id != Null then (extract_parent parent_id) else id) in
                if not from_gen then (let _ = checkIndicesValid parent_id id parent is_append from_gen idx in ());

                let dim_used = List.length idx in
                    match (Table.getSymbol parent used_id) with
                        (* use get_stored_type since an array's type is: [5][10]int *)
                        (* it just extracts the int part of that *)
                        | Some (name, ((Table.Slice(typ, dim)) as arrType)) -> get_idx_typ used_id idx arrType typ parent_id parent is_append from_gen
                        | Some (name, ((Table.Array(typ, dims)) as arrType)) -> get_idx_typ used_id idx arrType typ parent_id parent is_append from_gen
                        | Some(name, t) -> raise (NotIndexable (name, (string_of_types t)))
                        | None ->  raise (IdentifierNotFound used_id)
            end

| Ast.Append(slice_var, value) ->
begin
    (match (typeFromExp parent_id slice_var parent true from_gen) with
        | Table.Slice(_,_)                                             -> ()
        | Table.CustomType(_,(Table.Slice(_,_)))                       -> ()
        | Table.TypeDef(_,Table.Slice(_,_))                            -> ()
        | Table.CustomType(_,(Table.TypeDef(_,Table.Slice(_,_)))) as t -> ()
        | _ as t ->  raise (AppendError ((nameFromVarUsage slice_var), (string_of_types t), "slice"))
    );
    let result_typ = typeFromExp parent_id slice_var parent true from_gen in
    ( checkAppend slice_var value result_typ parent_id parent from_gen);
    result_typ
end

| Ast.Len exp ->
    let t = (typeFromExp parent_id exp parent is_append from_gen) in
    if (isString_arrType t) then Table.Int
    else raise (LenError ((nameFromVarUsage exp), (string_of_types t), "String | Array | Slice"))

| Ast.Cap exp ->
    let t = (typeFromExp parent_id exp parent is_append from_gen) in
    if (isArrType t) then Table.Int
    else raise (CapError ((nameFromVarUsage exp), (string_of_types t), "Array | Slice"))
    (* func_name, args, array stuff... *)
(* | Ast.FuncCallIdent of expr * (expr list) * string *)

| Ast.FuncCallIdent (expName, args, ret_idx) -> if (isType parent (nameFromVarUsage expName)) && ((List.length args) == 1) then (typeFromExp parent_id (Ast.Cast(expName, (List.hd args))) parent is_append from_gen) else
    checkFunc expName args ret_idx parent_id parent is_append from_gen

| Ast.Cast(typ, id) ->
    match typ with
    | Ast.Ident t -> if isFunc (Table.getSymbol parent t) then funcHack parent t id parent_id else
                let usage = (t ^ "(" ^  (nameFromVarUsage id) ^ ")") in
                     checkCast parent_id t id usage parent is_append from_gen
    | _ as e ->  (print_string ("void cast, type is: " ^ (string_of_types (typeFromExp Null e parent is_append from_gen)) ^ "\n"); Table.Void)

and get_idx_typ used_id idx arrType typ parent_id parent is_append from_gen =
    let dim_used = List.length idx in
    let decl_dims = (arr_dim_from_table arrType) in
    if from_gen && is_append then arrType
    else if from_gen && (is_append = false) then typ
    else if (is_append && (dim_used = (decl_dims - 1))) then arrType
    else if (is_append = false && dim_used = decl_dims) then typ
    else raise (ArrayDimError (decl_dims, dim_used, used_id))

and make_idx id nameFromUse parent_id parent is_append from_gen =
    let new_id = remove_arr_part nameFromUse in
    typeFromExp parent_id (Ast.Index(new_id, (get_indices id))) parent is_append from_gen

and get_field_type n l p = match l with
  | h::[] -> get_param n h p
  | h::t -> let sub_struct = get_param n h p in (match sub_struct with
                                                  | Table.Struct(new_p)                                         -> get_field_type (n ^ "." ^ h) t new_p
                                                  | Table.CustomType(_,Table.Struct(new_p))                     -> get_field_type (n ^ "." ^ h) t new_p
                                                  | Table.CustomType(_,Table.CustomType(_,Table.Struct(new_p))) -> get_field_type (n ^ "." ^ h) t new_p

                                                  | Table.Array(Table.Struct(new_p),dim) -> get_field_type (n ^ "." ^ h) t new_p
                                                  | Table.Slice(Table.Struct(new_p),dim) -> get_field_type (n ^ "." ^ h) t new_p
                                                  | Table.Array(Table.CustomType(_,Table.Struct(new_p)),dim) -> get_field_type (n ^ "." ^ h) t new_p
                                                  | Table.Slice(Table.CustomType(_,Table.Struct(new_p)),dim) -> get_field_type (n ^ "." ^ h) t new_p
                                                  | Table.Array(Table.CustomType(_,Table.CustomType(_,Table.Struct(new_p))),dim) -> get_field_type (n ^ "." ^ h) t new_p
                                                  | Table.Slice(Table.CustomType(_,Table.CustomType(_,Table.Struct(new_p))),dim) -> get_field_type (n ^ "." ^ h) t new_p
                                                  | _ as j -> raise(Table.NotStruct(n, (n ^ "." ^ h))))

and get_param var id params = match params with
    | [] -> raise(BadParam(var,id))
    | (name,typ)::t ->
        begin
            let last_field = Gen_name_maker.get_last_elem (String.split_on_char '.' name) in
            (* print_string ("struct: " ^ var ^ ", param: " ^ name ^ ", want name: " ^ id ^ ", new field searched: " ^ last_field ^  "\n"); *)
            if (last_field=id) then typ else get_param var id t
        end
    (* let  = (List.hd parts) in *)

and checkIndicesValid parent_id var parent is_append from_gen = function
    | hd::tl ->
            begin
                let t = (typeFromExp Null hd parent is_append from_gen) in
                let str = (string_of_types t) in
                if ( t != Table.Int) then raise (NonIntIndex (var, str));
                checkIndicesValid parent_id var parent is_append from_gen tl
            end
    | [] -> ()

and funcHack parent name arg parent_id = match (Table.getSymbol parent name) with
  | Some(_,Table.Func(al,rt)) -> let ut = (typeFromExp parent_id arg parent false false) in
                                    let (n,typ) = List.hd al in if ((List.length al) == 1) && (typ=ut) then rt else raise(FuncArgsMismatch (name,(string_of_types ut),(string_of_types typ)))
  | _ -> Table.Void

and checkCast parent_id typ id usage parent is_append from_gen =
let fst_type = (
    match (Table.getSymbol parent typ) with
    | Some(name, Table.CustomType(_,underlying)) -> underlying
    | Some(name, Table.TypeDef(_,u)) -> u
    | Some(name, typ) -> typ
    | None -> type_of_string typ parent
) in
let snd_typ = (match (typeFromExp parent_id id parent is_append from_gen) with
    | Table.CustomType(_,underlying) -> underlying
    | Table.TypeDef(_,u) -> u
    | _ as u -> u
) in
if (
    (fst_type = snd_typ)
    || ((isNumeric fst_type) && (isNumeric snd_typ))
    || (fst_type = Table.String && (isIntorRune snd_typ))
) && (isType parent typ) && not(isArrType fst_type) then (  (* print_string ("valid cast, new type: " ^ (string_of_types (type_of_string typ parent)) ^ "\n"); *)  (type_of_string typ parent) )
else raise (InvalidCast ((string_of_types fst_type), (string_of_types snd_typ), usage))

and checkAppend slice_var value result_typ parent_id parent from_gen =
    let value_type = typeFromExp parent_id value parent true from_gen in
    match result_typ with
        | Table.Slice(typ,dim) ->
                if (not(value_type = typ)) then  raise (AppendError ((nameFromVarUsage slice_var), (string_of_types value_type), (string_of_types result_typ)))
            else ()
        (* | (Table.Slice(_,_)) as t -> t *)
        | Table.CustomType(_,Table.Slice(typ,_))   ->
            if (not(value_type = typ)) then raise (AppendError ((nameFromVarUsage slice_var), (string_of_types value_type), (string_of_types typ)))
            else ()
        | Table.CustomType(_,(Table.TypeDef(_,Table.Slice(typ,_)))) ->
            if (not(value_type = typ)) then raise (AppendError ((nameFromVarUsage slice_var), (string_of_types value_type), (string_of_types typ)))
            else ()

        | _ ->  raise (AppendError ((nameFromVarUsage slice_var), (string_of_types result_typ), "slice"))

and checkFunc expName args ret_idx parent_id parent is_append from_gen =
    let rec types_are_same dcl_argz argz = match dcl_argz with
        | (_,dcl_t)::tl -> let used_typ = (typeFromExp parent_id (List.hd argz) parent is_append from_gen) in
            if not(used_typ = dcl_t) then raise (FuncArgsMismatch ((nameFromVarUsage expName),(string_of_types used_typ),(string_of_types dcl_t)))
            else types_are_same tl (List.tl argz)
        | [] -> true
    in
    let check_arrPart dcl_ret used_ret =
        (if not((arr_dim_from_table dcl_ret) = (arr_dim_from_string used_ret))
            then raise (ArrayDimError((arr_dim_from_table dcl_ret),(arr_dim_from_string used_ret),(string_of_types (typeFromExp parent_id expName parent is_append from_gen)))));
        check_all_int used_ret

    in
    (* print_string ("exp search for: " ^ (nameFromVarUsage expName) ^ "\n"); *)
    let t = Table.getSymbol parent (nameFromVarUsage expName) in
    match t with
        | Some(_,Table.Func(dcl_args,rt)) -> let dcl_numArgs = (List.length dcl_args) in let used_numArgs = (List.length args) in
                if not(dcl_numArgs = used_numArgs ) then raise (WrongNumArgs ((nameFromVarUsage expName), used_numArgs, dcl_numArgs))
                (* check that every corresponding arg has same type! *)
                else if ((types_are_same dcl_args args) && (ret_idx = "") && (not(isArrType rt))) then rt
                else if ((not(ret_idx = "")) && (isArrType rt) && (check_arrPart rt ret_idx) ) then (
                    match rt with
                        | Table.Array(typ,_) | Table.Slice(typ,_) -> typ
                        | _ as t -> raise (FuncArrMisuse (nameFromVarUsage expName))
                )
                else raise (FuncArrMisuse (nameFromVarUsage expName))
            | Some(id,typ) -> print_string ("name is: " ^ id ^ ", unfound type is: " ^ (string_of_types typ) ^ "\n"); raise (FuncArrMisuse (nameFromVarUsage expName))
            | _ as t -> raise (FuncArrMisuse (nameFromVarUsage expName))


let checkDclShadow t e tbl = match t with
    | Table.CustomType(n,u) -> (match e with
        | Ast.Ident(v) -> if (Table.getLevel tbl v 0) > (Table.getLevel tbl n 0) then raise(ShadowError(n))
        | _ -> ())
    | _ -> ()

let checkAssignShadow t e1 e2 tbl = match t with
    | Table.CustomType(n,u) -> (match e1 with
        | Ast.Ident(v1) -> (match e2 with
            | Ast.Ident(v2) -> let l = (Table.getLevel tbl n 0) in if ((Table.getLevel tbl v1 0) > l) || ((Table.getLevel tbl v2 0) > l) then raise(ShadowError(n))
            | _ -> ())
        | _ -> ())
    | _ -> ()

let checkAssign e1 e2 parent = 
  let is_append =
    (match e2 with
    | Ast.Append(_,_) -> true
    | _ -> false
  ) in
  let is_blank_id =
    (match e1 with
    | Ast.Ident v -> if (v = "_") then true else false
    | _ -> false
  ) in
  (* t2 needs to be first to reject '_ = type'. *)
  let t2 = typeFromExp Null e2 parent is_append false in
  if is_blank_id then () else
  let t1 = typeFromExp Null e1 parent is_append false in
  if (t1 = t2) && not(isFuncCall e1) then checkAssignShadow t1 e1 e2 parent
  else (
      if t1 = t2 then ()
      else raise (Table.TypeMismatch (string_of_types t1, string_of_types t2))
  )

let checkDcl t e parent =
  let t1 = type_of_string t parent in
  let t2 = typeFromExp Null e parent false false in
  if (t1 = t2) then checkDclShadow t1 e parent
  else raise (Table.TypeMismatch (string_of_types t1, string_of_types t2))

let checkSpecialFunc id args rt =
  if (id = "main" || id = "init") then
    ((let l = (List.length args) in if l > 0 then raise (SpecialFuncErr("Special function " ^ id ^ " expects 0 arguments, found " ^ (string_of_int l) ^ ".")));
     (if not(rt = Table.Void) then raise (SpecialFuncErr("Special function " ^ id ^ " must have return void, found " ^ (string_of_types rt) ^ "."))))

let rec getDup l = match l with
  | []   -> None
  | h::t -> if (List.mem h t) && not(h = "_") then Some(h) else getDup t

let rec shortAssignDup dl acc = match dl with
    | [] -> (match (getDup acc) with
              | None -> ()
              | Some(s) -> raise(DuplicateParam(s)))
    | h::t -> (match h with
        | Ast.ShortAssign(v,e) -> shortAssignDup t ((nameFromVarUsage v)::acc)
        | _ -> shortAssignDup t acc)

let get_param_name parent_id id add_children =
    if add_children then (parent_id ^ "." ^ id)
    else id


(* the add_children is there so that we don't have func args be: func_name.var_name *)
let rec makeParams parent_id dcls (tbl : Table.symbolTable) acc add_children prepend_dot = match dcls with
  | [] -> let (ids,typs) = List.split acc in
            (match (getDup ids) with
               | None -> List.rev acc
               | Some(s) -> raise(DuplicateParam(s)))
  | hd::tl ->
          let (child, typ) = (dcl_to_param parent_id hd tbl add_children) in
            let child_id = child in
            makeParams parent_id tl tbl ((child_id, typ)::acc) add_children prepend_dot

and dcl_to_param parent_id dcl tbl add_children = match dcl with
  | Ast.UndeclaredVar(id,t)  ->
          let new_parent = get_param_name parent_id id add_children in
          let typ = type_of_string t tbl in if isType tbl t then (new_parent, typ) else raise (TypeDclErr(id,t))
  | Ast.SliceDcl(id,t,dim)   ->
        let new_parent = get_param_name parent_id id add_children in
        let typ = type_of_string t tbl in if isType tbl t then (new_parent, Table.Slice((typ), dim)) else raise (TypeDclErr(id,t))
  | Ast.ArrayDcl(id,t,dim) ->
        let new_parent = get_param_name parent_id id add_children in
        let typ = type_of_string t tbl in if isType tbl t then (new_parent, Table.Array((typ), dim)) else raise (TypeDclErr(id,t))
  | Ast.StructDcl(id,params) ->
        let new_parent = get_param_name parent_id id add_children in
        (* pass add_children = true always for inner structs since cases where add_children=false I.E: func_args *)
        (* won't have these struct_dcls *)
        (new_parent, Table.Struct(makeParams new_parent params tbl [] true false))

  | Ast.SliceStructDcl(id,Ast.SliceDcl(_,_,dim),Ast.StructDcl(_,params))  ->
        let new_parent = get_param_name parent_id id add_children in
        (new_parent, Table.Slice(Table.Struct(makeParams new_parent params tbl [] true false), dim))

  | Ast.ArrayStructDcl(id,Ast.ArrayDcl(_,_,dims),Ast.StructDcl(_,params))  ->
        let new_parent = get_param_name parent_id id add_children in
        (new_parent, Table.Array(Table.Struct(makeParams new_parent params tbl [] true false), dims))
  | _ -> raise (Ast.ParseError("Error"))


let type_from_arrType_struct id arrType struckt parent =
    let params = (extract_struct_params id struckt) in
    let stored_params = makeParams id params parent [] true false in
        match arrType with
        | Ast.SliceDcl(id,t,dim)  ->  Table.Slice(Table.Struct(stored_params), dim)
        | Ast.ArrayDcl(id,t,dims) ->  Table.Array(Table.Struct(stored_params), dims)
        | _ ->  Table.Void


let checkAssignops l op r  parent =
	let t1 = typeFromExp Null l parent false false in
    let t2 = typeFromExp Null r parent false false in
    let t3 = 
    match op with 
    | Ast.PlusEquals   -> checkBinops t1 Ast.Add t2
    | Ast.MinusEquals  -> checkBinops t1 Ast.Sub t2
    | Ast.OrEquals     -> checkBinops t1 Ast.BitOr t2
    | Ast.XorEquals    -> checkBinops t1 Ast.BitXorBin t2
    | Ast.TimesEquals  -> checkBinops t1 Ast.Mult t2
    | Ast.DivEquals    -> checkBinops t1 Ast.Div t2
    | Ast.ModEquals    -> checkBinops t1 Ast.Mod t2
    | Ast.LShiftEquals -> checkBinops t1 Ast.BitShiftRight t2
    | Ast.RShiftEquals -> checkBinops t1 Ast.BitShiftLeft t2
    | Ast.AndEquals    -> checkBinops t1 Ast.BitAnd t2
    | Ast.NandEquals   -> checkBinops t1 Ast.BitAndNot t2 
	in
	if t1 = t3 then () else raise (Table.TypeMismatch (string_of_types t3, string_of_types t1))


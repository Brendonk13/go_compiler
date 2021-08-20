exception ParseError of string
exception LexError of string
exception WeedError of string
exception TerminateError of string
exception UnreachableCode of string

type ast = stmt list
and stmt =
	  DeclStmt of declaration list
	| PackageDcl of string
	| Empty
	| ExpressionStmt of expr
	| Increment of expr
	| Decrement of expr
	| Assign of declaration list
	| Assignop of expr * assignop * expr
	| BlockStmt of stmt list
	| Print of expr list option 
	| Println of expr list option
	| Return of expr option
	| If of expr * stmt list * elsestmt
	| IfSimp of stmt * expr * stmt list * elsestmt
	| ForInf of stmt list
	| ForWhile of expr * stmt list
	| For of stmt * expr option * stmt option * stmt list
	| SwitchSimpExpr of stmt * expr * exprcaseclauselist
	| SwitchExpr of expr * exprcaseclauselist
	| SwitchSimp of stmt * exprcaseclauselist
	| Switch of exprcaseclauselist
	| Break
	| Continue
	| FuncDcl of string * (declaration list) * string * (stmt list)

and exprcaseclauselist = 
	| EndList
	| ExprCase of exprcaseclause * exprcaseclauselist

and exprcaseclause = Exprcase of exprswitchcase * stmt list

and exprswitchcase = 
	  Case of (expr list)
	| Default

and elsestmt = 
	  EndElse
	| ElseIf of stmt
	| Else of stmt list

and declaration =
	  InferredVar of expr*expr
	| TypedVar of string*string*expr
	| UndeclaredVar of string*string
	| ShortAssign of expr*expr
	| TypeDcl of string*string
	| StructDcl of string*(declaration list)
(* stores the dimension of slice ie # of ['s in declaration *)
	| SliceDcl of string*string*int
    (* stores the dimension of each array ie decl var z [3][4]int stored as ("z", "int", [3; 4]) *)
	| ArrayDcl of string*string*(int list)
	| ArrayStructDcl of string*declaration*declaration
	| SliceStructDcl of string*declaration*declaration
and expr =
	  Ident of string
    | Field of expr*expr
	| Int of int
	| Bool of bool
	| Float of float
	| String of string
	| StringLit of string
	| Rune of string
	| BinOp of binop*expr*expr
	| UnaryOp of unaryop*expr
	| Inferred of expr
	| Typed of string*expr
	| Redeclared of expr
	| FuncBody of (expr list) * string
    (* func_name, args, array ret stuff... *)
	| FuncCallIdent of expr * (expr list) * string
	| Append of expr * expr
	| Len of expr
	| Cap of expr
	| Index of string * (expr list)
    | Cast of expr*expr
    | ArrayField of expr*expr
    | Parens of expr

and assignop =
	  PlusEquals
	| MinusEquals
	| OrEquals
	| XorEquals
	| TimesEquals
	| DivEquals
	| ModEquals
	| LShiftEquals
	| RShiftEquals
	| AndEquals
	| NandEquals
and binop =
	  Add
	| Sub
	| Mult
	| Div
	| Mod
    | BitAnd
    | BitAndNot
    | BitOr
    | BitXorBin
    | BitShiftLeft
    | BitShiftRight
	| And
	| Or
	| Eq
	| Neq
	| Lt
	| Leq
	| Gt
	| Geq
and unaryop =
    | Not
    | BitXor
    | UMinus
    | Identity

(* | `Add of expr*expr | `Sub of expr*expr | `Mult of expr*expr | `Div of expr*expr *) 
(* | `And of expr*expr | `Or of expr*expr | `Eq of expr*expr | `Neq of expr*expr | `RLt of expr*expr *) 
(* | `RLeq of expr*expr | `RGt of expr*expr | `RGeq of expr*expr] [@@deriving sexp] *)

let binop_to_string op = match op with
	  Add -> " + "
	| Sub -> " - "
	| Mult -> " * "
	| Div -> " / "
	| Mod -> " % "
	| BitAnd -> " & "
	| BitAndNot -> " &^ "
	| BitOr -> " | "
	| BitXorBin -> " ^ "
	| BitShiftLeft -> " << "
	| BitShiftRight -> " >> "
	| And -> " && "
	| Or -> " || "
	| Eq -> " == "
	| Neq -> " != "
	| Lt -> " < "
	| Gt -> " > "
	| Leq -> " <= "
	| Geq -> " >= "

let assignop_to_string op = match op with
	  PlusEquals -> " += "
	| MinusEquals -> " -= "
	| OrEquals -> " |= "
	| XorEquals -> " ^= "
	| TimesEquals -> " *= "
	| DivEquals -> " /= "
	| ModEquals -> " %= "
	| LShiftEquals -> " <<= "
	| RShiftEquals -> " >>= "
	| AndEquals -> " &= "
	| NandEquals -> " &^= "


let rec pretty_print_slice dim =
    if dim > 0 then "[]" ^ (pretty_print_slice (dim-1))
    else ""

let rec pretty_print_array = function
    | [] -> ""
    | hd::tl -> ("[" ^ (string_of_int hd) ^ "]") ^ (pretty_print_array tl)

let rec is_not_struct = function
    | StructDcl(_,_)        -> false
    | SliceStructDcl(_,_,_) -> false
    | ArrayStructDcl(_,_,_) -> false
    | _                     -> true

let rec indent i str = match i with
	| 0 -> str
	| _ -> indent (i-1) ("\t" ^ str)

let rec pretty_print_stmts sl i acc = match sl with
	| [] -> acc
	| h::t -> pretty_print_stmts t i (acc ^ (pretty_print_stmt h i))

and pretty_print_stmt st i = match st with
	| Empty -> ""
	| DeclStmt(dls) -> pretty_print_decls dls i ""
	| PackageDcl(id) -> indent i ("package " ^ id ^ ";\n")
	| ExpressionStmt(e) -> indent i ((pretty_print_expr e) ^ ";\n")
	| Increment(e) -> indent i ((pretty_print_expr e) ^ "++\n")
	| Decrement(e) -> indent i ((pretty_print_expr e) ^ "--\n")
	| Assign(ll) -> pretty_print_decls ll i ""
	| Assignop(l,op,r) -> indent i ((pretty_print_expr l) ^ (assignop_to_string op) ^ (pretty_print_expr r) ^ ";\n")
	| BlockStmt(sl) -> pretty_print_block_indent sl i
	| Print(el) -> 
		begin
		match el with 
		| None -> indent i ("print()\n")
		| Some(ell) ->  indent i ("print(" ^ (pretty_print_exprs ell "") ^ ")\n")
		end
	| Println(el) ->
		begin
		match el with 
		| None -> indent i ("println()\n")
		| Some(ell) ->  indent i ("println(" ^ (pretty_print_exprs ell "") ^ ")\n")
		end
	| Return(e) ->
		begin
		match e with 
		| None -> indent i ("return\n")
		| Some(ex) ->  indent i ("return " ^ (pretty_print_expr ex) ^ "\n")
		end
	| If(e, sl, elif) -> indent i (pretty_print_if None e sl elif i)
	| IfSimp(s, e, sl, elif) -> indent i (pretty_print_if (Some s) e sl elif i)
	| ForInf(sl) -> indent i ("for" ^ (pretty_print_block sl (i)))
	| ForWhile(e, sl) -> indent i ("for " ^ (pretty_print_expr e) ^ (pretty_print_block sl (i)))
	| For(s1, e, s2, sl) -> 
	begin
	match (e, s2) with
	| None, None -> indent i ("for " ^ String.trim((pretty_print_stmt s1 i)) ^ "; " ^ "; " ^ (pretty_print_block sl (i)))
	| None, Some s -> indent i ("for " ^ String.trim((pretty_print_stmt s1 (i-1))) ^ "; " ^ "; " ^ String.trim((pretty_print_stmt s (i-1))) ^ (pretty_print_block sl (i-1)))
	| Some ex, None -> indent i ("for " ^ String.trim(pretty_print_stmt s1 i) ^ "; " ^ (pretty_print_expr ex) ^ "; " ^ (pretty_print_block sl (i)))
	| Some ex, Some s -> indent i ("for " ^ String.trim(pretty_print_stmt s1 i) ^ "; " ^ (pretty_print_expr ex) ^ "; " ^ String.trim(pretty_print_stmt s (i)) ^ (pretty_print_block sl (i)))
	end
	| Switch(eccl) -> indent i ("switch" ^ " {\n" ^ (pretty_print_eccls eccl (i+1))) ^ indent i "}\n"
	| SwitchSimpExpr(s, e, eccl) -> indent i 
	("switch " ^ String.trim(pretty_print_stmt s (i)) ^ " " ^ (pretty_print_expr e) ^ " {\n" ^ (pretty_print_eccls eccl (i+1))) ^ indent i "}\n"
	| SwitchExpr(e, eccl) -> indent i ("switch" ^ (pretty_print_expr e) ^ " {\n" ^ (pretty_print_eccls eccl (i+1))) ^ indent i "}\n"
	| SwitchSimp(s, eccl) -> indent i ("switch" ^ String.trim(pretty_print_stmt s (i)) ^ " {\n" ^ (pretty_print_eccls eccl (i+1))) ^ indent i "}\n"
	| Break -> indent i "break\n"
	| Continue -> indent i "continue\n"
    | FuncDcl(id,args,t,sl) -> (indent i ("func " ^ id ^ " (" ^ (pretty_print_args args "" [] i) ^ ") " ^ t ^ pretty_print_block sl i))


and pretty_print_eccls eccl i = match eccl with
	| EndList -> ""
	| ExprCase(ec, l) -> match ec with 
		| Exprcase(esc, sl) -> (pretty_print_esc esc (i)) ^ ": " ^ "\n" ^(pretty_print_stmts sl (i+1) "") ^ (pretty_print_eccls l i)

and pretty_print_esc esc i = match esc with
	| Case(el) -> indent i ("case " ^ pretty_print_exprs el "")
	| Default -> indent i ("default")

and pretty_print_if s e sl elif i = match s with
	| None -> "if " ^ String.trim((pretty_print_expr e)) ^ (" {\n") ^ (pretty_print_stmts sl (i+1) "") ^ (indent i "} ") ^ (pretty_print_elif elif (i))
	| Some(ss) -> "if " ^ String.trim((pretty_print_stmt ss i)) ^ " " ^ (pretty_print_expr e) ^ (" {\n") ^ (pretty_print_stmts sl (i+1) "") ^ (indent i "} ") ^ (pretty_print_elif elif (i))

and pretty_print_elif elif i = match elif with
	| EndElse -> "\n"
	| ElseIf(s) -> ("else " ^ pretty_print_stmt s i)
	| Else(sl) -> ("else" ^ pretty_print_block sl i)

and pretty_print_block sl i = (" {\n") ^ (pretty_print_stmts sl (i+1) "") ^ (indent i "}\n")

and pretty_print_block_indent sl i = (indent i "{\n") ^ (pretty_print_stmts sl (i+1) "") ^ (indent i "}\n")

and pretty_print_decls dl i acc = match dl with
	| [] -> acc
	| h::t -> pretty_print_decls t i (acc ^ (pretty_print_decl h i))

and pretty_print_decl decl i = match decl with
	| InferredVar(id,e) -> indent i ("var " ^ (pretty_print_expr id) ^ " = " ^ (pretty_print_expr e) ^ ";\n")
	| TypedVar(id,t,e) -> indent i ("var " ^ id ^ " " ^ t ^ " = " ^ (pretty_print_expr e) ^ ";\n")
	| UndeclaredVar(id,t) -> indent i ("var " ^ id ^ " " ^ t ^ ";\n")
	| ShortAssign(id,e) -> indent i ((pretty_print_expr id) ^ " := " ^ (pretty_print_expr e) ^ ";\n")
	| TypeDcl(id,t) -> indent i ("type " ^ id ^ " " ^ t ^ ";\n")
    | StructDcl(id,p) -> (indent i ("type " ^ id ^ " struct{\n")) ^ (indent (i+1) (pretty_print_args p "" [] i)) ^ ";\n" ^ (indent i "}\n")
    | SliceDcl(id,t,dim) -> indent i ("var " ^ id ^ " " ^ (pretty_print_slice dim) ^ t ^ ";\n")
	| ArrayDcl(id,t,dims) -> indent i ("var " ^ id ^ " " ^ (pretty_print_array dims) ^ t ^ ";\n")
	| ArrayStructDcl(id,ArrayDcl(_,_,d),StructDcl(_,p)) -> indent i ("type " ^ id ^ " " ^ (pretty_print_array d) ^ "struct{\n" ^ (indent (i+1) (pretty_print_args p "" [] i)) ^ ";\n" ^ (indent i "}\n"))
    | SliceStructDcl(id,SliceDcl(_,_,d),StructDcl(_,p)) -> indent i ("type " ^ id ^ " " ^ (pretty_print_slice d) ^ "struct{\n" ^ (indent (i+1) (pretty_print_args p "" [] i)) ^ ";\n" ^ (indent i "}\n"))
    | _ -> ""

and pretty_print_exprs el acc = match el with
	| [] -> acc
	| h::[] -> pretty_print_exprs [] (acc ^ (pretty_print_expr h))
	| h::t -> pretty_print_exprs t (acc ^ (pretty_print_expr h) ^ ", ")

and pretty_print_args args acc struct_args i =
    (* let rec aux *) 
    let rec structs_at_end new_acc = function
        | [] -> new_acc
        | hd::tl -> structs_at_end (new_acc ^ (pretty_print_arg hd i)) tl
    (* let (non_struct,struct_args) = List.split *)
    in
    match args with
	| [] -> if (List.length struct_args) = 0 then acc
            else (
                if (String.contains acc ',' ) then ((acc ^ "\n" ) ^ (indent (i+1) (structs_at_end "" struct_args)))
                else
                    (acc ^ ( (structs_at_end "" struct_args)))
            )
    | h::[] -> if (is_not_struct h) then pretty_print_args [] (acc ^ (pretty_print_arg h i)) struct_args i
               else
                   pretty_print_args [] acc (h::struct_args) i
	| h::t -> if (is_not_struct h) then pretty_print_args t (acc ^ (pretty_print_arg h i) ^ ", ") struct_args i
              else
                  pretty_print_args t acc (h::struct_args) i

and pretty_print_arg arg i = match arg with
	| UndeclaredVar(id,t) -> id ^ " " ^ t
	| SliceDcl(id,t,dim) -> id ^ " " ^ (pretty_print_slice dim) ^ t
	| StructDcl(id,p) -> id ^ " " ^  ("struct{\n" ^(indent (i+2) (pretty_print_args p "" [] (i+1))) ^ ";\n" ^ (indent (i+1) "}"))
	| ArrayDcl(id,t,dims) ->id ^ " " ^ (pretty_print_array dims) ^ t
    | ArrayStructDcl(id, ArrayDcl(_,_,dim),StructDcl(_,p)) -> ((id ^ " " ^ (pretty_print_array dim)) ^("struct{\n" ^(indent (i+2) (pretty_print_args p "" [] (i+1))) ^ ";\n" ^ (indent (i+1) "}")))
    | SliceStructDcl(id, SliceDcl(_,_,dim),StructDcl(_,p)) -> (( (id ^ " " ^ (pretty_print_slice dim)) ^("struct{\n" ^(indent (i+2) (pretty_print_args p "" [] (i+1))) ^ ";\n" ^ (indent (i+1) "}"))))
    | _ -> raise (ParseError("Error"))

and pretty_print_expr ex = match ex with
	| Int(i') -> string_of_int i'
	| Float(f) -> string_of_float f
	| String(s) ->  s
    | StringLit(s) -> s
	| Rune(c) -> c
	| Ident(id) -> id
	| Field(e,field) -> (pretty_print_expr e) ^ "." ^ (pretty_print_expr field)
	| Bool(b) -> string_of_bool b
	| Cast(too,from) -> (pretty_print_expr too) ^ "(" ^ (pretty_print_expr from) ^ ")"
	| BinOp(op,l,r) -> "(" ^ (pretty_print_expr l) ^ (binop_to_string op) ^ (pretty_print_expr r) ^ ")"
	| UnaryOp(type_op, e) -> pretty_print_unaryop type_op e
	| FuncBody(el, s) -> 
	begin
		if s = "" then ("(" ^ (pretty_print_exprs el "") ^ ")")
		else ("(" ^ (pretty_print_exprs el "") ^ ")[" ^ s ^ "]")
	end
	| FuncCallIdent(e, el, s) -> begin
		if s = "" then (pretty_print_expr e ^ "(" ^ (pretty_print_exprs el "") ^ ")")
		else (pretty_print_expr e ^ "(" ^ (pretty_print_exprs el "") ^ ")[" ^ s ^ "]")
	end
	| Append(e1, e2) -> "append(" ^ (pretty_print_expr e1) ^ ", " ^ (pretty_print_expr e2) ^ ")"
	| Len(e) -> "len(" ^ (pretty_print_expr e) ^ ")"
	| Cap(e) -> "cap(" ^ (pretty_print_expr e) ^ ")"
	| Index(s1, el) -> s1 ^ (List.fold_left (fun acc e -> acc ^ "[" ^ (pretty_print_expr e) ^ "]") "" el)
	| Parens(e) -> "(" ^ pretty_print_expr e ^ ")"
	| ArrayField (e1, e2) -> pretty_print_expr e1 ^ "." ^ pretty_print_expr e2
	| _ -> raise (ParseError("Error"))

	(* | Field(id,field) -> (pretty_print_expr id) ^ "." ^ (pretty_print_expr field) *)

and pretty_print_unaryop type_op ex = match type_op with
	| UMinus -> "-(" ^ (pretty_print_expr ex) ^ ")"
    | Not -> "!(" ^ (pretty_print_expr ex) ^ ")"
    | Identity -> "+(" ^ (pretty_print_expr ex) ^ ")"
    | BitXor -> "^(" ^ (pretty_print_expr ex) ^ ")"

let loop = ref 0
let switch = ref 0
let encountered_break = ref false
let encountered_else = ref false
let encountered_default = ref false

let rec weed_stmts sl acc = match sl with
	| [] -> acc
	| h::t -> weed_stmts t (weed_stmt h)

and weed_stmt st = match st with
	| Empty -> ()
	| DeclStmt(dls) -> weed_dcls dls ()
	| PackageDcl(id) -> ()
	| ExpressionStmt(e) 
	| Increment(e)
	| Decrement(e) -> weed_expr e
	| Assign(ll) -> weed_dcls ll ()
	| Assignop(l,op,r) -> let _wr = weed_expr r in ()
	| BlockStmt(sl) -> weed_stmts sl ()
	| Print(el) -> 
		begin
		match el with 
		| None -> ()
		| Some(ell) ->  weed_exprs ell ()
		end
	| Println(el) ->
		begin
		match el with 
		| None -> ()
		| Some(ell) ->  weed_exprs ell ()
		end
	| Return(e) ->
		begin
		match e with 
		| None -> ()
		| Some(ex) ->  weed_expr ex
		end
	| If(e, sl, elif) -> let _we = weed_expr e in let _ws = weed_stmts sl () in let _welif = weed_elif elif in ()
	| IfSimp(s, e, sl, elif) -> let _ws = weed_stmt s in let _we = weed_expr e in let _wsl = weed_stmts sl () in let _welif = weed_elif in ()
	| ForInf(sl) -> let _ = encountered_break := false in let _ = loop := !loop + 1 in let wsl = weed_stmts sl () in let _ = loop := !loop - 1 in wsl
	| ForWhile(e, sl) -> let _ = loop := !loop + 1 in let _we = weed_expr e in let wsl = weed_stmts sl () in let _ = loop := !loop - 1 in wsl
	| For(s1, e, s2, sl) -> 
	begin
		match (e, s2) with
		| None, None -> 
			let _ = loop := !loop + 1 in 
			let _ws = weed_stmt s1 in 
			let wsl = weed_stmts sl () in 
			let _ = loop := !loop - 1 in 
			wsl
		| None, Some s -> 
			let _ = loop := !loop + 1 in 
			let _ws = weed_stmt s1 in 
			let _wss = weed_stmt s in 
			let wsl = weed_stmts sl () in 
			let _ = loop := !loop - 1 in 
			wsl
		| Some ex, None ->
			let _ = loop := !loop + 1 in 
			let _ws = weed_stmt s1 in 
			let _we = weed_expr ex in
			let wsl = weed_stmts sl () in 
			let _ = loop := !loop - 1 in
			wsl
		| Some ex, Some s -> 
			let _ = loop := !loop + 1 in 
			let _ws = weed_stmt s1 in 
			let _we = weed_expr ex in
			let _wss = weed_stmt s in 
			let wsl = weed_stmts sl () in 
			let _ = loop := !loop - 1 in
			wsl
	end
	| Switch(eccl) -> 
		let _ = encountered_break := false in
		let _ = loop := !loop + 1 in 
		let _ = switch := !switch + 1 in 				
		let weccl = weed_eccl eccl 0 in 
		let _ = switch := !switch - 1 in 		
		let _ = loop := !loop - 1 in 
		weccl
	| SwitchSimpExpr(s, e, eccl) -> 
		let _ = encountered_break := false in
		let _ = loop := !loop + 1 in 
		let _ = switch := !switch + 1 in 		
		let _ws = weed_stmt in 
		let _we = weed_expr e in 
		let weccl = weed_eccl eccl 0 in
		let _ = switch := !switch - 1 in 			
		let _ = loop := !loop - 1 in
		weccl
	| SwitchExpr(e, eccl) -> 
		let _ = encountered_break := false in
		let _ = loop := !loop + 1 in
		let _ = switch := !switch + 1 in 
		let _we = weed_expr e in 
		let weccl = weed_eccl eccl 0 in
		let _ = switch := !switch - 1 in 		
		let _ = loop := !loop - 1 in 
		weccl
	| SwitchSimp(s, eccl) -> 
		let _ = encountered_break := false in
		let _ = loop := !loop + 1 in
		let _ = switch := !switch + 1 in 				
		let _ws = weed_stmt in 
		let weccl = weed_eccl eccl 0 in 
		let _ = switch := !switch - 1 in 				
		let _ = loop := !loop - 1 in
		weccl
	| Break -> let _ = encountered_break := true in if !loop = 0 then raise (WeedError("Break statement outside loop!")) else ()
	| Continue -> if !loop = 0 then raise (WeedError("Continue statement outside loop!")) 
				  else if !switch > 0 then raise (WeedError("Continue statement inside switch!")) 
				  else ()
	| FuncDcl(id,args,t,sl) -> let _ = weed_stmts sl () in if t = "" then () else check_terminate sl id

and check_terminate sl id = if is_terminating sl id then () else raise (TerminateError(id))

and is_terminating sl id = match sl with 
	| [] -> false
	| t :: [] -> 
		begin 
			match t with 
			| Return(e) -> true
			| BlockStmt(sl) -> is_terminating sl id
			| ForInf(sl) -> if (not !encountered_break) then true else false
			| For(_,e,_,sl) -> if (not !encountered_break) && (e=None) then true else false
			| If(e, sl, elif) -> 
				if is_terminating sl id then
					begin
						match elif with
						| EndElse -> if (!encountered_else) then true else false
						| ElseIf(s) -> is_terminating (s :: []) id
						| Else(sl) -> let _ = encountered_else := true in is_terminating sl id
					end
				else false
			| Switch(eccl)
			| SwitchSimpExpr(_, _, eccl)
			| SwitchExpr(_, eccl)
			| SwitchSimp(_, eccl) -> 
				if (!encountered_break || not !encountered_default) then false
				else term_switch eccl id
			| _ -> false
		end
	| h :: t -> if is_terminating (h :: []) id then raise (UnreachableCode(id)) else is_terminating t id

and term_switch eccl id = match eccl with 
	| EndList -> true
	| ExprCase(ec, l) -> match ec with 
		| Exprcase(esc, sl) -> if is_terminating sl id then term_switch l id else false

and weed_dcls dls acc = match dls with
	| [] -> acc
	| h::t -> weed_dcls t (weed_dcl h)

and weed_dcl dcl = match dcl with
	| InferredVar(id,e) ->
		begin
			match id with 
			| Ident(_) -> weed_expr e 
			| Index(_, _) as ex -> let _ = weed_expr ex in weed_expr e 
			| Field(_, _) as ex -> let _ = weed_expr ex in weed_expr e
			| Parens(ex) -> weed_dcl (InferredVar(ex, e))
			| ArrayField (e1, e2) -> let _ = weed_expr e1 in let _ = weed_expr e2 in weed_expr e
			| FuncCallIdent (_,_,_) as ex -> let _ = weed_expr ex in weed_expr e
			| _ -> raise (WeedError("Invalid assignment"))
		end
	| TypedVar(id,t,e) -> weed_expr e
	| UndeclaredVar(id,t) -> ()
	| ShortAssign(id, e) -> 
		begin
			match id with 
			| Ident(_) -> weed_expr e 
			| _ -> raise (WeedError("Can only use identifiers in short assignments"))
		end
	| TypeDcl(id,t) -> ()
	| StructDcl(id,p) -> ()
	| SliceDcl(id,t,dim) -> ()
	| ArrayDcl(id,t,dims) -> ()
	| ArrayStructDcl (id, d1, d2) -> ()
	| SliceStructDcl (id, d1, d2) -> ()

and weed_expr e = match e with
	| Ident(s) -> if s = "_" then raise (WeedError("Cannot use '_' as a value or type!")) else ()
	| UnaryOp(type_op, e) -> weed_expr e
	| FuncBody(el, s) -> weed_exprs el ()
	| FuncCallIdent(e, el, s) -> 
		begin
			match e with 
			| Ident(_) -> ()
			| Parens ex -> let _ = weed_expr ex in weed_exprs el ()
			| _ -> raise (WeedError("Invalid function call"))
		end	
	| Append(e1, e2) -> let _ = weed_expr e1 in let _ = weed_expr e2 in ()
	| Len(e) -> weed_expr e
	| Cap(e) -> weed_expr e
	| Index(s1, el) -> if (List.mem "_" (List.map (pretty_print_expr) el)) then raise (WeedError("Cannot use '_' as a value or type!")) else ()
	| Field(id,f) -> (weed_expr id; if (pretty_print_expr f) = "_" then raise (WeedError("Cannot use '_' as a value or type!")) else ())
	| Parens(e) -> weed_expr e
	| _ -> ()

and weed_exprs el acc = match el with
	| [] -> acc
	| h::t -> weed_exprs t (weed_expr h)

and weed_elif elif = match elif with
	| EndElse -> ()
	| ElseIf(s) -> weed_stmt s
	| Else(sl) -> weed_stmts sl ()

and weed_eccl eccl defaults = 
	match eccl with
	| EndList -> 
		(if defaults > 0 then encountered_default := true);
		if defaults > 1 then raise (WeedError("More than 1 default case!")) 
		else ()
	| ExprCase(ec, l) -> match ec with 
		| Exprcase(esc, sl) -> 
		let defaults = weed_esc esc defaults in 
		let _weccl = weed_eccl l defaults in 
		let wsl = weed_stmts sl () in 
		wsl

and weed_esc esc defaults = match esc with 
	| Case(el) -> let _ = weed_exprs el () in defaults
	| Default -> defaults + 1

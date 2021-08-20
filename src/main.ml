open Printf

module GenName = Gen_name_maker

let load_text () =
    let cin = if Array.length Sys.argv > 2
            then open_in Sys.argv.(2)
            else stdin
    in Lexing.from_channel cin

let with_error_handling fn ok =
    try (fn (); if ok then printf "OK\n") with
        | Ast.LexError(msg)                       -> (fprintf stderr "Error: Line:\n %s.\n" msg; exit 1)
        | Ast.ParseError(msg)                     -> (fprintf stderr "Error: %s.\n" msg; exit 1)
        | Ast.WeedError(msg)                      -> (fprintf stderr "Error: Invalid syntax detected while weeding: %s\n" msg; exit 1)
        | Ast.TerminateError(id)                  -> (fprintf stderr "Error: No terminating statement found at end of function %s!\n" id; exit 1)
        | Ast.UnreachableCode(id)                 -> (fprintf stderr "Error: Unreachable code found in function %s!\n" id; exit 1)
        | Parser.Error                            -> (fprintf stderr "Error: Program contains invalid syntax.\n"; exit 1)
        | Table_obj.AlreadyDeclared(id)           -> (fprintf stderr "Error: \nAlreadyDeclaredError:\n Identifier %s has already been declared in this scope.\n" id; exit 1)
        | Table_obj.TypeMismatch(v,f)             -> (fprintf stderr "Error: \nTypeMismatchError:\n Cannot assign type %s to a variable of type %s.\n" f v; exit 1)
        | Table_obj.SwitchMismatch(t1,t2)         -> (fprintf stderr "Error: \nSwitchMismatchError:\n Expected type %s in case expression but got type %s.\n" t1 t2; exit 1)
        | Table_obj.RedeclaredTypeMismatch(id)    -> (fprintf stderr "Error: \nRedeclaredTypeMismatchError:\n Tried to assign new type to variable: %s.\n" id; exit 1)
        | Table_obj.ReturnMismatch(rt, ft)        -> (fprintf stderr "Error: \nReturnMismatchError:\n Function has return type %s but got type %s\n" ft rt; exit 1)
        | Table_obj.FieldNotFound(id,f)           -> (fprintf stderr "Error: \nFieldNotFoundError:\n Field %s not defined for variable %s.\n" f id; exit 1)
        | Table_obj.IncDecError(t)                -> (fprintf stderr "Error: \nIncDecError:\n Cannot increment/decrement type %s\n" t; exit 1)
        | Table_obj.ForIfExpMismatch(t)           -> (fprintf stderr "Error: \nForIfExpMismatchError:\n Expressions in for/if statements should be of type bool, not type %s.\n" t;exit 1)
        | Table_obj.PrintBaseType(t)              -> (fprintf stderr "Error: \nPrintBaseTypeError:\n Print expects base types but got type %s\n" t; exit 1)
        | Type_checker.IdentifierNotFound(id)     -> (fprintf stderr "Error: \nIdentifierNotFoundError:\n Identifier: '%s' has not been declared.\n" id; exit 1)
        | Type_checker.ArrayDimError(d1,d2,id)    -> (fprintf stderr "Error: \nArrayDimError:\n Identifier: '%s' with dimension: %d used with incorrect dimension: %d.\n" id d1 d2; exit 1)
        | Type_checker.ArrayDimMismatch(id1,id2)  -> (fprintf stderr "Error: \nArrayDimMismatchError:\n Attempted to assign '%s' to variable: '%s' which have incompatible array/slice dimension.\n" id2 id1; exit 1)
        | Type_checker.NotCompoundStruct(id)      -> (fprintf stderr "Error: \nNotCompoundStructError:\n Used variable: '%s' as a slice/array of struct.\n" id; exit 1)
        | Type_checker.BinopMismatch(t1,op,t2)    -> (fprintf stderr "Error: \nBinopMismatchError:\n Cannot use binary operator '%s' with types %s and %s.\n" op t1 t2; exit 1)
        | Type_checker.NonIntIndex(id,bad)        -> (fprintf stderr "Error: \nInvalidIndexError:\n array type variables: '%s' was indexed using non-integer. Type of bad expression is: %s\n" id bad; exit 1)
        | Type_checker.NotIndexable(id,t)         -> (fprintf stderr "Error: \nNotIndexableError:\n variable: '%s' was indexed in an expression but is not an indexable type. Type of bad expression is: %s\n" id t; exit 1)
        | Type_checker.InvalidCast(fst_typ,t,use) -> (fprintf stderr "Error: \nInvalidCastError:\n usage: %s invalid. Tried to cast variable of type: %s to type: %s\n" use t fst_typ; exit 1)
        | Table_obj.NotStruct(id,field)           -> (fprintf stderr "Error: \nNotStructError:\n Used variable: '%s' as a struct: %s\n" id (id^"."^field); exit 1)
        | Type_checker.SpecialFuncErr(msg)        -> (fprintf stderr "Error: \nSpecialFuncError:\n %s\n" msg; exit 1)
        | Symbol_table.InvalidBlankId(msg)        -> (fprintf stderr "Error: \nInvalidBlankIdError:\n Encountered invalid use of the blank identifier: '%s'\n" msg; exit 1)
        | Symbol_table.RecursiveType(msg)         -> (fprintf stderr "Error: \nRecursiveTypeError:\n %s\n" msg; exit 1)
        | Type_checker.DuplicateParam(id)         -> (fprintf stderr "Error: \nDuplicateParamError:\n Cannot have more than one parameter named '%s' in the same parameter list.\n" id; exit 1)
        | Type_checker.TypeDclErr(id,t)           -> (fprintf stderr "Error: \nTypeDclError:\n Cannot declare type %s with non-type identifier %s.\n" id t; exit 1)
        | Symbol_table.NoNewVars                  -> (fprintf stderr "Error: \nNoNewVarsError:\n No new variables on left side of :=\n"; exit 1)
        | Type_checker.AppendError(v,t,want)      -> (fprintf stderr "Error: \nAppendError:\n Used identifier: '%s' with type: %s, expected: %s.\n" v t want; exit 1)
        | Type_checker.LenError(v,t,want)         -> (fprintf stderr "Error: \nLenError:\n Used identifier: '%s' with type: %s, expected: %s.\n" v t want; exit 1)
        | Type_checker.FuncArgsMismatch(v,t,want) -> (fprintf stderr "Error: \nFuncArgsMismatchError:\n Called function: '%s' given argument of type: %s, expected: %s.\n" v t want; exit 1)
        | Type_checker.WrongNumArgs(v,used,dcl)   -> (fprintf stderr "Error: \nWrongNumArgsError:\n Function: '%s' was called with: %d args, expected: %d.\n" v used dcl; exit 1)
        | Type_checker.FuncArrMisuse(name)        -> (fprintf stderr "Error: \nFuncArrMisuseError:\n Called function: '%s' does not handle return type correctly\n" name; exit 1)
        | Type_checker.NotExpr(id)                -> (fprintf stderr "Error: \nNotExprError:\n Identifier '%s' is not a valid expression.\n" id ; exit 1)
        | Type_checker.CapError(id,t,ex)          -> (fprintf stderr "Error: \nCapError:\n Identifier '%s' %s %s.\n" id t ex; exit 1) 
        | Symbol_table.InvalidExprStmt            -> (fprintf stderr "Error: \nA cast is not a valid expression statement.\n"; exit 1) 
        | Symbol_table.VoidTypeErr                -> (fprintf stderr "Error: \nVoid type cannot be assigned to a variable.\n"; exit 1) 
        | Type_checker.ShadowError(n)             -> (fprintf stderr "Error: \nShadowed type '%s' is incompatible with previously declared type of same name.\n" n; exit 1) 
        | Type_checker.BadParam(n,p)              -> (fprintf stderr "Error: \nStruct '%s' has no parameter called '%s'.\n" n p; exit 1)
        | Symbol_table.FuncAssign                 -> (fprintf stderr "Error: \nCannot assign functions to variables or variables to function calls.\n"; exit 1) 

(* | Table_obj.RedeclaredTypeMismatch(id,t,tnew) -> (fprintf stderr "Error: cannot assign type: %s to %s which already has type %s" t id tnew; exit 1) *)
let parse lexbuf = Parser.start Lexer.token lexbuf

let main () =
    let lexbuf = load_text() in
        match Sys.argv.(1) with
        | "scan" -> with_error_handling (fun () -> Lexer.token lexbuf) true
        | "tokens" -> with_error_handling (fun () -> Parser.start Lexer.token lexbuf) false
        | "parse" -> with_error_handling (fun () -> Ast.weed_stmts (Parser.start Lexer.token lexbuf) ()) true
        | "pretty" -> with_error_handling (fun () -> print_string (Ast.pretty_print_stmts (Parser.start Lexer.token lexbuf) 0 "")) false
        | "symbol" -> with_error_handling (fun () -> Symbol_table.symFromProg (Parser.start Lexer.token lexbuf) 0) false
        | "typecheck" -> with_error_handling (fun () -> 
            let prog = Parser.start Lexer.token lexbuf in
            (Ast.weed_stmts prog ());
            (Symbol_table.symFromProg prog 0)
            ) true
        | "codegen" ->
            let generated_file = GenName.get_gen_file() in
            let class_name = GenName.get_java_className() in
                with_error_handling (fun () ->
                    let prog = Parser.start Lexer.token lexbuf in
                    (Ast.weed_stmts prog ());
                    (Symbol_table.symFromProg prog 0);
                    (let file = open_out generated_file in Codegen.codeProg prog file class_name)
                ) true
        (* this mode is meant for the execute.sh script to get the name of the file generated *)
        | "path_to_gen_file" -> print_string (GenName.get_gen_file())
        | "class_name" -> print_string (GenName.get_java_className())
        | "repl" -> with_error_handling (fun () -> print_string (Ast.pretty_print_stmts (parse lexbuf) 0 "")) true
        | _ -> printf "Go away\n"

let _ = main ()

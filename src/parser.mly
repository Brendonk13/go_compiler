%{
	open Ast;;


	let rec order_assign l rl acc = match l with
		| [] -> List.rev acc
		| h::[] -> order_assign [] [] ((makeNode (h,(List.hd rl)))::acc)
		| h::t -> order_assign t (List.tl rl) ((makeNode (h,(List.hd rl)))::acc)
	and makeNode n = match n with
		| (InferredVar(id1,e1),InferredVar(id2,e2)) -> InferredVar(id1, e2)
		| (TypedVar(id1,t1,e1), TypedVar(id2,t2,e2)) -> TypedVar(id1,t1,e2)
		| (ShortAssign(id1,e1),ShortAssign(id2,e2)) -> ShortAssign(id1,e2)
		| _ -> raise (Ast.ParseError("Error"))
%}

%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> IDENT
%token <string> STRING
%token <string> STRINGLITERAL
%token <string> RUNE


%token IF
%token ELSE
%token BREAK
%token CASE
%token CHAN
%token CONST
%token CONTINUE
%token DEFAULT
%token DEFER
%token FALLTHROUGH
%token FOR
%token FUNC
%token GOTO
%token GO
%token IMPORT
%token INTERFACE
%token MAP
%token PACKAGE
%token RANGE
%token RETURN
%token SELECT
%token STRUCT
%token SWITCH
%token TYPE
%token PRINTLN
%token APPEND
%token LEN
%token CAP

%token TTRUE
%token TFALSE
%token TFLOAT
%token TINT
%token TSTRING
%token TBOOL
%token TRUNE
%token VAR

%token PRINT
%token READ

%token COLON

%token LPAREN
%token RPAREN
%token ASSIGNMENT
%token SEMI
%token LOGICALAND



%token EQUALS
%token NOTEQUALS
%token LOGICALOR
%token GREATEQUAL
%token GREATER
%token LESSEQUAL
%token LESSER

%token OPENBLOCK
%token CLOSEBLOCK
%token LBRACKET
%token RBRACKET

%token PLUS
%token MINUS
%token MULT
%token DIV
%token MOD
%token BITWISEOR
%token BITWISEXOR
%token BITWISEAND
%token BITWISEANDNOT

%token UMINUS

%token INC
%token DEC

%token COMMA
%token PERIOD

%token PLUSEQ MINUSEQ OREQ XOREQ 
%token TIMESEQ DIVEQ MODEQ LSHIFTEQ RSHIFTEQ ANDEQ NANDEQ
%token SHORTASSIGN

%token LEFTBITSHIFT RIGHTBITSHIFT LOGICALNOT

%token EOF



%left LOGICALOR
%left LOGICALAND
%left EQUALS NOTEQUALS GREATER GREATEQUAL  LESSEQUAL LESSER
%left PLUS MINUS BITWISEOR
%left MULT DIV MOD LEFTBITSHIFT RIGHTBITSHIFT BITWISEAND BITWISEANDNOT
%left UMINUS LOGICALNOT BITWISEXOR PERIOD




%start start
%type <Ast.ast> start
%%


/* Statements */
start:
| pkgdcl toplevels EOF                             { $1::$2 }
| pkgdcl EOF { [$1] }
;

pkgdcl:
| PACKAGE identifier SEMI                                { PackageDcl($2) }
;
toplevels:
| toplevel                                          { [$1] }
| toplevel toplevels                                { $1::$2 }
;
toplevel:
| dcl SEMI                                          { DeclStmt $1 }
| FUNC funcdcl SEMI                                 { $2 }
;

statement:
| dcl { DeclStmt $1 }
| simple_stmt { $1 }
| block_stmt { BlockStmt $1 }
| print_stmt { $1 }
| return_stmt { $1 }
| if_stmt { $1 }
| switch_stmt { $1 }
| for_stmt { $1 }
| break_stmt { $1 }
| continue_stmt { $1 }
;

simple_stmt:
| { Empty }
| expr_stmt { ExpressionStmt $1 }
| inc_dec_stmt { $1 }
| assign_stmt { $1 }
| short_dcl { DeclStmt (order_assign $1 (List.rev $1) []) }
; 

expr_stmt: func_call { $1 }

expr_list: el = separated_nonempty_list(COMMA, expr)                      { el }

inc_dec_stmt:
| expr INC                                                                { Increment $1 }
| expr DEC                                                                { Decrement $1 }
;

assign_stmt: 
| assigns { Assign (order_assign $1 (List.rev $1) []) }
| expr assign_op expr { Assignop ($1, $2, $3) }

assigns:
| expr ASSIGNMENT expr                               { [InferredVar($1,$3)] }
| expr COMMA assigns COMMA expr                      { InferredVar($1,$5)::$3 }

assign_op: 
| add_op                                                                  { $1 }
| mul_op                                                                  { $1 }

add_op:
| PLUSEQ                                                                  { PlusEquals }
| MINUSEQ                                                                 { MinusEquals }
| OREQ                                                                    { OrEquals }
| XOREQ                                                                   { XorEquals }
;

mul_op:
| TIMESEQ                                                                 { TimesEquals }
| DIVEQ                                                                   { DivEquals }
| MODEQ                                                                   { ModEquals }
| LSHIFTEQ                                                                { LShiftEquals }
| RSHIFTEQ                                                                { RShiftEquals }
| ANDEQ                                                                   { AndEquals }
| NANDEQ                                                                  { NandEquals }
;

block_stmt: OPENBLOCK stmt_list CLOSEBLOCK { $2 }

stmt_list:
| statement SEMI stmt_list { $1 :: $3 }
| { [] }

print_stmt:
| PRINT LPAREN expr_list? RPAREN { Print $3 }
| PRINTLN LPAREN expr_list? RPAREN { Println $3 }
;

return_stmt: RETURN expr? { Return $2 }

if_stmt: 
| IF expr block_stmt else_if_stmt { If ($2, $3, $4) }
| IF simple_stmt SEMI expr block_stmt else_if_stmt { IfSimp ($2, $4, $5, $6) }
;

else_if_stmt:
| ELSE if_stmt { ElseIf $2 }
| ELSE block_stmt { Else $2 }
| { EndElse }
;

switch_stmt:
| SWITCH simple_stmt SEMI expr OPENBLOCK expr_case_clause_list CLOSEBLOCK { SwitchSimpExpr ($2, $4, $6) }
| SWITCH expr OPENBLOCK expr_case_clause_list CLOSEBLOCK { SwitchExpr ($2, $4) }
| SWITCH simple_stmt SEMI OPENBLOCK expr_case_clause_list CLOSEBLOCK { SwitchSimp ($2, $5)}
| SWITCH OPENBLOCK expr_case_clause_list CLOSEBLOCK { Switch $3}
;

expr_case_clause_list: 
| { EndList }
| expr_case_clause expr_case_clause_list { ExprCase($1, $2) }

expr_case_clause: expr_switch_case COLON stmt_list { Exprcase($1, $3) }

expr_switch_case:
| CASE expr_list { Case $2 }
| DEFAULT { Default }
;

for_stmt:
| FOR block_stmt { ForInf $2 } (* infinite loop *)
| FOR expr block_stmt { ForWhile($2, $3) } (* while loop *)
| FOR simple_stmt SEMI expr? SEMI post_stmt? block_stmt { For($2, $4, $6, $7) } (* three part loop *)
;

post_stmt:
| inc_dec_stmt { $1 }
| assign_stmt { $1 }

break_stmt: BREAK { Break }

continue_stmt: CONTINUE { Continue }


/* Function calls */
func_call: 
| LPAREN expr RPAREN func_body                             { 
                                                              match $4 with
                                                              | FuncBody(el, s) ->
                                                                    if ((List.length el) = 1) then (match $2 with
                                                                        (* | (Field(_,_)) *)
                                                                        (* | (ArrayField(_,_)) *)
                                                                        (* | (Index(_,_)) *)
                                                                        | (Ident _) as exp1 ->
                                                                                (match (List.hd el) with
                                                                                (* | (Field(_,_)) *)
                                                                                (* | (ArrayField(_,_)) *)
                                                                                (* | (Index(_,_)) *)
                                                                                | (Ident _) as exp -> Cast (exp1, exp)
                                                                                | _ ->
                                                                                    (match $4 with 
                                                                                    | FuncBody(el, s) -> FuncCallIdent(Parens $2, el, s) 
                                                                                    | _ -> failwith ""
                                                                                    )
                                                                                )
                                                                        | _ ->
                                                                                    (match $4 with 
                                                                                    | FuncBody(el, s) -> FuncCallIdent(Parens $2, el, s) 
                                                                                    | _ -> failwith ""
                                                                                    )
                                                                    )
                                                                    else
                                                                        FuncCallIdent(Parens $2, el, s) 
                                                              | _ -> failwith ""
                                                           }
| identifier func_body                                      {
                                                              match $2 with
                                                              | FuncBody(el, s) ->
                                                                    if ((List.length el) = 1) then match (List.hd el) with
                                                                        | (Field(_,_))
                                                                        | (ArrayField(_,_))
                                                                        | (Index(_,_))
                                                                        | (Ident _) as exp -> Cast (Ident $1, exp)
                                                                        | _ -> FuncCallIdent(Ident $1, el, s)
                                                                    else
                                                                        FuncCallIdent(Ident $1, el, s)
                                                              | _ -> failwith ""
                                                           }


func_body:
| LPAREN RPAREN func_arr { FuncBody([], $3)}
| LPAREN func_call_args RPAREN func_arr { FuncBody($2, $4)}
;

(* | LBRACKET identifier RBRACKET func_arr { $2 ^ "-" ^ $4 } *)
func_arr:
| { "" }
| LBRACKET identifier RBRACKET func_arr { "[" ^ $2 ^ "]" ^ $4 }
| LBRACKET INT RBRACKET func_arr { "[" ^ (string_of_int $2) ^ "]" ^ $4 }
;

func_call_args:
| expr { [$1] }
| expr COMMA func_call_args { [$1] @ $3}
;

/* Declarations */
dcl:
| VAR LPAREN RPAREN                            { [] }
| VAR vardcl                                   { order_assign $2 (List.rev $2) [] }
| VAR uninstantiated_vardecl                   { $2 }
| VAR LPAREN vardcls RPAREN                    { $3 }
| TYPE LPAREN RPAREN                           { [] }
| TYPE typedcl                                 { $2 }
| TYPE LPAREN typedcls RPAREN                  { $3 }
;

vardcls:
| vardcl SEMI                                       { order_assign $1 (List.rev $1) [] }
| uninstantiated_vardecl SEMI                       { $1 }
| vardcl SEMI vardcls                               { (order_assign $1 (List.rev $1) []) @ $3 }
| uninstantiated_vardecl SEMI vardcls               { $1 @ $3 }
;

vardcl:
| identifier ASSIGNMENT expr                             { [InferredVar((Ident $1),$3)] }
| identifier identifier ASSIGNMENT expr                     { [TypedVar($1,$2,$4)] }
| identifier LPAREN identifier RPAREN ASSIGNMENT expr    { [TypedVar($1,$3,$6)] }
| identifier COMMA vardcl COMMA expr                     {
                                                      match List.hd $3 with
                                                        | InferredVar(id,e) -> InferredVar((Ident $1),$5)::$3
                                                        | TypedVar(id,t,e) -> TypedVar($1,t,$5)::$3
                                                        | _ -> raise (Ast.ParseError("Error"))
                                                    }
;

uninstantiated_vardecl:
| identifier identifier                                     { [UndeclaredVar($1,$2)] }
| identifier LPAREN identifier RPAREN                       { [UndeclaredVar($1,$3)] }
| identifier compound_dcl       {
                                let end_arr_part = ((String.index $2 '#')+1) in
                                let typ = String.sub $2 end_arr_part ((String.length $2) - end_arr_part) in

                                    match $2.[0] with
                                    | '[' -> (let dim = ( List.fold_left
                                                    (fun ct str -> if (String.contains str ']') then (ct+1) else ct)
                                                    0 (String.split_on_char '[' $2)
                                                ) in
                                                [SliceDcl($1, typ, dim)]
                                    )

                (* input string to map function should be all the numbers enclosed by [] in declaration *)
                                    | _ -> (let dims =
                                            List.map (fun x -> int_of_string x) (List.rev (List.tl (List.rev (String.split_on_char ']' (String.sub $2 0 (end_arr_part-1))))))
                                            in
                                            [ArrayDcl($1, typ, dims)]
                                    )
                                }
| identifier arr_or_slice_struct structdcl               {
                                let typ = "struct " in
                                    match $2.[0] with
                                    | '[' -> (let dim = (List.fold_left
                                                    (fun ct str -> if (String.contains str ']') then (ct+1) else ct)
                                                    0 (String.split_on_char '[' $2)
                                            ) in
                                            [SliceStructDcl($1, SliceDcl($1, (typ ^ "slice"), dim), StructDcl($1, $3))]
                                    )

                                    | _ -> (let dims = (List.map
                                            (fun x -> int_of_string x) (List.rev (List.tl (List.rev (String.split_on_char ']' (String.sub $2 0 (String.index $2 '#'))))))
                                                ) in
                                                [ArrayStructDcl($1, ArrayDcl($1, (typ ^ "array"), dims), StructDcl($1, $3))]
                                    )
                                }
| identifier structdcl                                   { [StructDcl($1,$2)] }
| identifier COMMA uninstantiated_vardecl                {
                                                      match List.hd $3 with
                                                        | UndeclaredVar(id,t) -> UndeclaredVar($1,t)::$3
                                                        | SliceDcl(id,t,dim) -> SliceDcl($1,t,dim)::$3
                                                        | ArrayDcl(id,t,dims) -> ArrayDcl($1,t,dims)::$3
                                                        | StructDcl(id,params) -> StructDcl($1,params)::$3
                                                        | _ -> raise (Ast.ParseError("Error"))

                                                    }
;




arr_or_slice_struct:
| slice_of_struct                                          { $1 }
| arr_of_struct                                            { $1 }



/* store a list here, ie for [1][5][6] --> store [1;5;6] */
arr_of_struct:
| LBRACKET INT RBRACKET                                    { (string_of_int $2) ^ "]#" }
| LBRACKET INT RBRACKET arr_of_struct                      { (string_of_int $2) ^ "]" ^ $4 }
;


/* store dimension for structs, ie # of []'s */
slice_of_struct:
| LBRACKET RBRACKET                                        { "[]#"      }
| LBRACKET RBRACKET slice_of_struct                        { "[]" ^ $3  }


(* # marks the end of the array portion *)
compound_dcl:
| array_dcl                                                { $1 }
| slice_dcl                                                { $1 }


(* dont have the '[' brackets so that the String.split str ']' returns only #'s between the [] *)
array_dcl:
| LBRACKET INT RBRACKET identifier                         { (string_of_int $2) ^ "]#" ^ $4}
| LBRACKET INT RBRACKET array_dcl                          { (string_of_int $2) ^ "]" ^ $4 }
;


slice_dcl:
| LBRACKET RBRACKET identifier                             { "[]#" ^ $3 }
| LBRACKET RBRACKET slice_dcl                              { "[]" ^ $3  }
| LBRACKET INT RBRACKET slice_dcl                          { "[" ^ (string_of_int $2) ^ "]" ^ $4 }


short_dcl:
| expr SHORTASSIGN expr                              { [ShortAssign($1,$3)] }
| expr COMMA short_dcl COMMA expr                    {
                                                                match List.hd $3 with
                                                                    | ShortAssign(id,e) -> ShortAssign($1,$5)::$3
                                                                    | _ -> raise (Ast.ParseError("Error"))
                                                           }
;
typedcls:
| typedcl SEMI                                             { $1 }
| typedcl SEMI typedcls                                    { $1 @ $3 }
;

typedcl:
| identifier struct_type structdcl                        { [StructDcl(("-" ^ $1),$3)] }
| identifier return_type                                   { [TypeDcl($1,$2)] }
| identifier LPAREN return_type RPAREN                       { [TypeDcl($1,$3)] }
;
structdcl:
| STRUCT OPENBLOCK uninstantiated_vardecl CLOSEBLOCK       { $3 }
| STRUCT OPENBLOCK CLOSEBLOCK                              { [] }
| STRUCT OPENBLOCK struct_params CLOSEBLOCK                { $3 }
;
struct_params:
| uninstantiated_vardecl SEMI                              { $1 }
| uninstantiated_vardecl SEMI struct_params                { $1 @ $3 }
;
funcdcl:
| identifier LPAREN RPAREN return_type block_stmt          { FuncDcl($1,[],$4,$5) }
| identifier LPAREN funcargs RPAREN return_type block_stmt { FuncDcl($1,$3,$5,$6) }
| identifier LPAREN RPAREN block_stmt                      { FuncDcl($1,[],"",$4) }
| identifier LPAREN funcargs RPAREN block_stmt             { FuncDcl($1,$3,"",$5) }
/*| identifier LPAREN RPAREN structdcl block_stmt            { FuncDcl($1,[], pretty_print_decls $4 0 "",$5) }*/
;
funcargs:
| uninstantiated_vardecl                                   { $1 }
| uninstantiated_vardecl COMMA funcargs                    { $1 @ $3 }
;

return_type:
| LBRACKET RBRACKET return_type                            { "[]" ^ $3 }
| LBRACKET INT RBRACKET return_type                        { "[" ^ (string_of_int $2) ^ "]" ^ $4 }
| identifier                                               { $1 }

struct_type:
| LBRACKET RBRACKET struct_type                            { "[]" ^ $3 }
| LBRACKET INT RBRACKET struct_type                        { "[" ^ (string_of_int $2) ^ "]" ^ $4 }
|                                                 { "" }


/* NOTE: these can all be used as valid variable names!! ie: var float64 = 10 */
identifier:
| TFLOAT                                                   { "float64" }
| TINT                                                     { "int" }
| TSTRING                                                  { "string"}
| TBOOL                                                    { "bool" }
| TRUNE                                                    { "rune" }
| TTRUE                                                    { "true" }
| TFALSE                                                   { "false" }
| IDENT                                                    { $1 }
;

var_usage:
| identifier                                               { Ident $1 }
| field                                                    { $1 }
| index_expr                                               { $1 }

allowed_in_field:
| identifier                                    { Ident $1 }
| index_expr                                    { $1 }

field:
| index_expr PERIOD allowed_in_field                       { ArrayField($1, $3)}
| identifier PERIOD allowed_in_field                       { Field((Ident $1), $3)}
| field PERIOD allowed_in_field                            { Field($1, $3)}
| func_call PERIOD allowed_in_field                        { Field($1, $3)}

expr:
| var_usage                                                { $1 }
| complex_expr                                             { $1 }
| LPAREN expr RPAREN                                       { Parens $2 }
| func_call                                                { $1 }
| constant                                                 { $1 }
;

complex_expr:
| binary_expr                                              { $1}
| unary_expr                                               { $1}
| built_in_funcs                                           { $1}
;


index_expr:
| identifier LBRACKET expr RBRACKET                        { Index($1, [$3]) }
| index_expr LBRACKET expr RBRACKET                        { match $1 with
                                                                                | Index(id,el) -> Index(id,(el @ [$3]))
                                                                                | _ -> raise (Ast.ParseError("Error"))
                                                                            }

constant:
/*| field                                                  { $1} */
| INT                                                      { Int $1 }
| BOOL                                                     { Bool $1 }
| FLOAT                                                    { Float $1}
| STRING                                                   { String $1}
| STRINGLITERAL                                            { StringLit $1}
| RUNE                                                     { Rune $1 }
;


built_in_funcs:
| APPEND LPAREN expr COMMA expr RPAREN                     { Append($3, $5)}
| LEN LPAREN expr RPAREN                                   { Len $3}
| CAP LPAREN expr RPAREN                                   { Cap $3}
;

unary_expr:
 /* unary plus is the identity operator, ie x := +(-10),  x has value: -10! */
| MINUS expr        %prec UMINUS                           { UnaryOp(UMinus, $2)}
| PLUS expr         %prec UMINUS                           { UnaryOp(Identity, $2)}
| LOGICALNOT expr   %prec UMINUS                           { UnaryOp(Not, $2)}
| BITWISEXOR expr   %prec UMINUS                           { UnaryOp(BitXor, $2)}
;


binary_expr:
| logical_expr                                             { $1}
| binary_bitwise_expr                                      { $1}
| binary_math_expr                                         { $1}


binary_bitwise_expr:
| expr BITWISEAND expr                                     { BinOp(BitAnd, $1, $3) }
| expr BITWISEANDNOT expr                                  { BinOp(BitAndNot, $1, $3) }
| expr BITWISEOR expr                                      { BinOp(BitOr, $1, $3) }
| expr BITWISEXOR expr                                     { BinOp(BitXorBin, $1, $3) }
| expr LEFTBITSHIFT expr                                   { BinOp(BitShiftLeft, $1, $3) }
| expr RIGHTBITSHIFT expr                                  { BinOp(BitShiftRight, $1, $3) }

;


/* the BinOp variant was used to simplify the AST in the second assignment */
/* Better idea: keep using binop, but make each binop store the operation persistently */
binary_math_expr:
| expr PLUS expr                                           { BinOp(Add, $1, $3)}
| expr MINUS expr                                          { BinOp(Sub, $1, $3)}
| expr MULT expr                                           { BinOp(Mult, $1, $3)}
| expr DIV expr                                            { BinOp(Div, $1, $3)}
| expr MOD expr                                            { BinOp(Mod, $1, $3) }


logical_expr:
| expr LOGICALOR expr                                      { BinOp(Or, $1, $3) }
| expr LOGICALAND expr                                     { BinOp(And, $1, $3)}
| expr EQUALS expr                                         { BinOp(Eq, $1, $3)}
| expr NOTEQUALS expr                                      { BinOp(Neq, $1, $3)}
| expr LESSER expr                                         { BinOp(Lt, $1, $3)}
| expr LESSEQUAL expr                                      { BinOp(Leq, $1, $3) }
| expr GREATER expr                                        { BinOp(Gt, $1, $3)}
| expr GREATEQUAL expr                                     { BinOp(Geq, $1, $3)}
;

{
    exception Eof
    (* short names for important modules *)
    module L     = Lexing
    module B     = Buffer
    let printf   = Printf.printf
    let sprintf  = Printf.sprintf
    open Parser
    open Str

    let check_num_args () : string =
        if Array.length Sys.argv > 2
        then Sys.argv.(1)
        else "0"

    (* check stdin if tokens flag was passed every Lexer.token call *)
    let search_flag x : bool = match x with
    | "tokens" -> true
    | _ -> false

    let print_tok tok value =
        if search_flag (check_num_args ())
            then if String.length value > 0
                then
                    printf "%s(%s)\n" tok value
                else
                    printf "%s\n" tok
        else ()

    let print_val tok =
    if search_flag (check_num_args ()) then printf "%s\n" tok else ()

    let position lexbuf =
        let pos = lexbuf.L.lex_curr_p in
        sprintf "%s:%d:%d"
        pos.L.pos_fname pos.L.pos_lnum (pos.L.pos_cnum - pos.L.pos_bol)


    let error lexbuf fmt =
        Printf.kprintf (fun msg ->
        raise (Ast.LexError ((position lexbuf)^" "^msg))) fmt


(* converts strings to ints by making sure its not a ocaml float first *)
let int_parser tint =
    try
        let idx = String.index tint '.' in
            if idx != 0 then int_of_string ( String.sub tint 0 idx )
            else 0
    (* if there is no period in tint then we are safe to convert it to an int. *)
    with Not_found -> if String.length tint = 1 then int_of_string tint else if (tint.[0]='0') then (match tint.[1] with
                                                                                | 'x' -> int_of_string tint
                                                                                | 'X' -> int_of_string tint
                                                                                | 'o' -> int_of_string tint
                                                                                | 'O' -> int_of_string tint
                                                                                | _ -> int_of_string ("0o" ^ tint)
    ) else int_of_string tint


let print_list prefix list =
    if search_flag (check_num_args ()) then
        (printf "%s" (prefix ^ " ("); printf "%s)\n" (List.fold_left (fun x y -> x ^ y) "" list) )
    else ()
    (* printf "%s" (prefix ^ " ("); List.iter (fun x -> let fst_chr = String.get x 0 in *)
    (* if (fst_chr = '\\' || fst_chr = '\t' || fst_chr = '\n' || fst_chr = '\b' || fst_chr = '\r' || fst_chr = '"') *)
    (* then (if fst_chr = '"' then printf "fst_char is : %c   " fst_chr; printf "SPECIAL:%s" x) *)
    (* else printf "%s" x) list; printf "%s" ")\n" *)

    let last_tok = ref PACKAGE (* Initial value of PACKAGE was chosen since it should be the first token in any file. Actual choice donesn't matter. *)

    let semicolon_needed () = match !last_tok with
        | IDENT(s) -> true
        | INT(i) -> true
        | FLOAT(f) -> true
        | STRING(s) -> true
        | STRINGLITERAL(s) -> true
        | RUNE(c) -> true
        | BREAK -> true
        | CONTINUE -> true
        | FALLTHROUGH -> true
        | RETURN -> true
        | INC -> true
        | DEC -> true
        | RPAREN -> true
        | CLOSEBLOCK -> true
        | RBRACKET -> true
        | TTRUE -> true
        | TFALSE -> true
        | TFLOAT -> true
        | TINT -> true
        | TSTRING -> true
        | TBOOL -> true
        | TRUNE -> true
        | _ -> false
}


let whitespace = [' ' '\t' '\r']

let nums = ['0'-'9']
(* ints are a number plus optionally: '.' followed by >= 0 zero's *)
(* OR no leading number, a '.' followed by at least 1 zero. *)
(* have the OR to disallow: var x int = . *)

let inferred_int = '0' | ['1'-'9']nums* | '0' ['x''X'] ['0'-'9''a'-'f''A'-'F']+ | '0' ['o''O'] ['0'-'7']+ | '0' ['0'-'7']+

let escape = ('a'|'b'|'f'|'n'|'r'|'t'|'v'|'\\'|'\'')
let rune = ("'"['\x00' -'\x09' '\x11'-'\x26'  '\x28'-'\x5b'  '\x5d'-'\x7f']"'") |("'\\"escape"'")

let inferred_float = nums+ '.' nums* | ('.'nums+)

let id = ['_' 'a'-'z''A'-'Z'] ['_' 'a'-'z''A'-'Z''0'-'9']*
let newline = '\n'
(* comments are //, any ammount of non newline, then a newline *)
let single_line_comment = ('/''/')[^ '\n']*newline?
let multi_line_comment_new_line = "/*"([^'*']|('*'+[^'*''/']))*('*'+'/')
let multi_line_comment = "/*"([^'*''\n']|('*'+[^'*''/']))*('*'+'/')

let comment = single_line_comment | multi_line_comment | multi_line_comment_new_line
let inferred_string    = '"' (whitespace | [^'\\''"''\n'] | "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v" | "\\\"" | "\\\\")* '"'
(* TODO: make strings recognize special characters *)
(* let inferred_string = '"' [^ '"' '\n']* '"' *)
(* like a python raw string *)
let inferred_string_literal = '`'[^'`']*'`'
let minus = '-'

(* My print_tok function prints the token passed below as arg if the --token flag was given as input. *)
rule token = parse
    | whitespace+       { token lexbuf }
    (* so this needs to change in some form to handle escape characters. *)
    | newline           { L.new_line lexbuf; if semicolon_needed() then (last_tok := PACKAGE; print_tok "AUTOSEMI" ";"; SEMI) else token lexbuf  }
    | comment as c      { if semicolon_needed() && (String.contains c '\n') then (last_tok := PACKAGE; print_tok "AUTOSEMI" ";"; SEMI) else token lexbuf }

    (* these tokens with no T starting their name are the inferred type from the regex plus the value. *)
    (* constant values ie 5, "asdasd" *)
    | inferred_int as tint           { let no_dot_int = int_parser tint in
                                        last_tok := (INT(no_dot_int));              print_tok "INT" (string_of_int no_dot_int);         (INT(no_dot_int))}
    | inferred_float as tfloat       { last_tok := (FLOAT(float_of_string tfloat)); print_tok "FLOAT" tfloat;           (FLOAT(float_of_string tfloat))}
    | inferred_string as str         { last_tok := (STRING(str));                   print_tok "STRING" str;             STRING(str) }

    | inferred_string_literal as str { last_tok := (STRINGLITERAL(str));            print_tok "STRING" str;             (STRINGLITERAL(str))}
    | rune as c                      { last_tok := (RUNE(c));                       print_tok "RUNE" c;                 (RUNE(c)) }

    (* the things that start with T can also be used as variable names *)
    (* un-reserved keywords *)
    | "true"                         { last_tok := TTRUE;                           print_tok "true" "";                TTRUE}
    | "false"                        { last_tok := TFALSE;                          print_tok "false" "";               TFALSE}
    | "float64"                      { last_tok := TFLOAT;                          print_tok "TFLOAT" "";              TFLOAT}
    | "int"                          { last_tok := TINT;                            print_tok "TINT" "";                TINT}
    | "string"                       { last_tok := TSTRING;                         print_tok "TSTRING" "";             TSTRING}
    | "bool"                         { last_tok := TBOOL;                           print_tok "TBOOL" "";               TBOOL}
    | "rune"                         { last_tok := TRUNE;                           print_tok "TRUNE" "";               TRUNE}

    (* reserved keywords/tokens *)
    | "break"                        { last_tok := BREAK;                           print_tok "BREAK" "";               BREAK }
    | "case"                         { last_tok := CASE;                            print_tok "CASE" "";                CASE }
    | "chan"                         { last_tok := CHAN;                            print_tok "CHAN" "";                CHAN }
    | "const"                        { last_tok := CONST;                           print_tok "CONST" "";               CONST }
    | "continue"                     { last_tok := CONTINUE;                        print_tok "CONTINUE" "";            CONTINUE }
    | "default"                      { last_tok := DEFAULT;                         print_tok "DEFAULT" "";             DEFAULT }
    | "defer"                        { last_tok := DEFER;                           print_tok "DEFER" "";               DEFER }
    | "fallthrough"                  { last_tok := FALLTHROUGH;                     print_tok "FALLTHROUGH" "";         FALLTHROUGH }
    | "for"                          { last_tok := FOR;                             print_tok "FOR" "";                 FOR }
    | "func"                         { last_tok := FUNC;                            print_tok "FUNC" "";                FUNC }
    | "goto"                         { last_tok := GOTO;                            print_tok "GOTO" "";                GOTO }
    | "go"                           { last_tok := GO;                              print_tok "GO" "";                  GO }
    | "import"                       { last_tok := IMPORT;                          print_tok "IMPORT" "";              IMPORT }
    | "interface"                    { last_tok := INTERFACE;                       print_tok "INTERFACE" "";           INTERFACE }
    | "map"                          { last_tok := MAP;                             print_tok "MAP" "";                 MAP }
    | "package"                      { last_tok := PACKAGE;                         print_tok "PACKAGE" "";             PACKAGE }
    | "range"                        { last_tok := RANGE;                           print_tok "RANGE" "";               RANGE }
    | "return"                       { last_tok := RETURN;                          print_tok "RETURN" "";              RETURN }
    | "select"                       { last_tok := SELECT;                          print_tok "SELECT" "";              SELECT }
    | "struct"                       { last_tok := STRUCT;                          print_tok "STRUCT" "";              STRUCT }
    | "switch"                       { last_tok := SWITCH;                          print_tok "SWITCH" "";              SWITCH }
    | "type"                         { last_tok := TYPE;                            print_tok "TYPE" "";                TYPE }
    | "println"                      { last_tok := PRINTLN;                         print_tok "PRINTLN" "";             PRINTLN }
    | "print"                        { last_tok := PRINT;                           print_tok "PRINT" "";               PRINT }
    | "append"                       { last_tok := APPEND;                          print_tok "APPEND" "";              APPEND }
    | "len"                          { last_tok := LEN;                             print_tok "LEN" "";                 LEN }
    | "cap"                          { last_tok := CAP;                             print_tok "CAP" "";                 CAP }
    | "if"                           { last_tok := IF;                              print_tok "IF" "";                  IF}
    | "else"                         { last_tok := ELSE;                            print_tok "ELSE" "";                ELSE}
    | "var"                          { last_tok := VAR;                             print_tok "VAR" "";                 VAR}
    | id as name                     { last_tok := (IDENT(name));                   print_tok "IDENT" name;             (IDENT(name))}

    | '{'                            { last_tok := OPENBLOCK;                       print_tok "OPENBLOCK" "{";          OPENBLOCK}
    | '}'                            { last_tok := CLOSEBLOCK;                      print_tok "CLOSEBLOCK" "}";         CLOSEBLOCK}
    | '('                            { last_tok := LPAREN;                          print_tok "LPAREN" "(";             LPAREN}
    | ')'                            { last_tok := RPAREN;                          print_tok "RPAREN" ")";             RPAREN}
    | '['                            { last_tok := LBRACKET;                        print_tok "LBRACKET" "[";           LBRACKET}
    | ']'                            { last_tok := RBRACKET;                        print_tok "RBRACKET" "]";           RBRACKET}
    (* logical operators *)
    | "&&"                           { last_tok := LOGICALAND;                      print_tok "LOGICALAND" "&&";        LOGICALAND}
    | "=="                           { last_tok := EQUALS;                          print_tok "EQUALS" "==";            EQUALS}
    | "!="                           { last_tok := NOTEQUALS;                       print_tok "NOTEQUALS" "";           NOTEQUALS}
    | "||"                           { last_tok := LOGICALOR;                       print_tok "OR" "";                  LOGICALOR}
    | ">="                           { last_tok := GREATEQUAL;                      print_tok "GREATEQUAL" ">=";        GREATEQUAL}
    | '>'                            { last_tok := GREATER;                         print_tok "GREATER" ">";            GREATER}
    | "<="                           { last_tok := LESSEQUAL;                       print_tok "LESSEQUAL" "<=";         LESSEQUAL}
    | '<'                            { last_tok := LESSER;                          print_tok "LESSER" "<";             LESSER}
    | '!'                            { last_tok := LOGICALNOT;                      print_tok "LOGICALNOT" "!";         LOGICALNOT}

    | ':'                            { last_tok := COLON;                           print_tok "COLON" "";               COLON}
    | ":="                           { last_tok := SHORTASSIGN;                     print_tok "SHORTASSIGN" ":=";       SHORTASSIGN}
    | '='                            { last_tok := ASSIGNMENT;                      print_tok "ASSIGNMENT" "=";         ASSIGNMENT}
    | ','                            { last_tok := COMMA;                           print_tok "COMMA" ",";              COMMA}
    | '.'                            { last_tok := PERIOD;                          print_tok "PERIOD" ".";             PERIOD}
    | ';'                            { last_tok := SEMI;                            print_tok "SEMI" ";";               SEMI}


    (* BIT OPS *)
    | '&'                            { last_tok := BITWISEAND;                      print_tok "BITWISEAND" "&";         BITWISEAND}
    | "&^"                           { last_tok := BITWISEANDNOT;                   print_tok "BITWISEANDNOT" "&^";     BITWISEANDNOT}
    | '|'                            { last_tok := BITWISEOR;                       print_tok "BITWISEOR" "|";          BITWISEOR}
    | '^'                            { last_tok := BITWISEXOR;                      print_tok "BITWISEXOR" "^";         BITWISEXOR}
    | "<<"                           { last_tok := LEFTBITSHIFT;                    print_tok "LEFTBITSHIFT" "<<";      LEFTBITSHIFT}
    | ">>"                           { last_tok := RIGHTBITSHIFT;                   print_tok "RIGHTBITSHIFT" ">>";     RIGHTBITSHIFT}

    (* Math stuff *)
    | '%'                            { last_tok := MOD;                             print_tok "MOD" "";                 MOD}
    | '-'                            { last_tok := MINUS;                           print_tok "MINUS" "";               MINUS}
    | '+'                            { last_tok := PLUS;                            print_tok "PLUS" "";                PLUS}
    | '*'                            { last_tok := MULT;                            print_tok "MULT" "";                MULT}
    | '/'                            { last_tok := DIV;                             print_tok "DIV" "";                 DIV}

    | "+="                           { last_tok := PLUSEQ;                          print_tok "PLUSEQ" ""; PLUSEQ }
    | "-="                           { last_tok := MINUSEQ;                         print_tok "MINUSEQ" ""; MINUSEQ }
    | "*="                           { last_tok := TIMESEQ;                         print_tok "TIMESEQ" ""; TIMESEQ }
    | "/="                           { last_tok := DIVEQ;                           print_tok "DIVEQ" ""; DIVEQ }
    | "%="                           { last_tok := MODEQ;                           print_tok "MODEQ" ""; MODEQ }
    | "^="                           { last_tok := XOREQ;                           print_tok "XOREQ" ""; XOREQ }
    | "|="                           { last_tok := OREQ;                            print_tok "OREQ" ""; OREQ }
    | "&="                           { last_tok := ANDEQ;                           print_tok "ANDEQ" ""; ANDEQ }
    | "<<="                          { last_tok := LSHIFTEQ;                        print_tok "LSHIFTEQ" ""; LSHIFTEQ }
    | ">>="                          { last_tok := RSHIFTEQ;                        print_tok "RSHIFTEQ" ""; RSHIFTEQ }
    | "&^="                          { last_tok := NANDEQ;                          print_tok "NANDEQ" ""; NANDEQ }

    (* Increment/Decrement *)
    | "++"                           { last_tok := INC;                             print_tok "INC" ""; INC}
    | "--"                           { last_tok := DEC;                             print_tok "DEC" ""; DEC}

    | eof                            { if semicolon_needed() then (let buff = L.from_string "\n" in token buff) else (last_tok := EOF; print_tok "EOF" ""; EOF)}
    | _                              { error lexbuf "Found unexpected token: '%s' while lexing" @@ L.lexeme lexbuf }


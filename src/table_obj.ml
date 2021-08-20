exception AlreadyDeclared of string
exception TypeMismatch of string * string
exception SwitchMismatch of string * string
exception ReturnMismatch of string * string
exception FieldNotFound of string * string
(* exception RedeclaredTypeMismatch of string * string * string *)
exception RedeclaredTypeMismatch of string
exception NotStruct of string * string
exception IncDecError of string
exception ForIfExpMismatch of string
exception PrintBaseType of string


type types = 
| Int
| Float
| String
(* | StringLit not a go lite type, can probs remove distinction in lexer but maybe want to distinguish for codegen *)
| Bool
| Rune
| Void
| CustomType of string * types
| Slice of types*int
| Array of types*(int list)
| TypeDef of string * types
| Struct of (string * types) list
| Func of ((string * types) list)*types


type symbol = Symbol of string * types


type symbolTable = {
  table: (string, types) Hashtbl.t;
  return_type: types option;
  parent: symbolTable option
}


let initialize () = {
  table = Hashtbl.create 317;
  return_type = None;
  parent = None
}


let scope s rt = {
  table = Hashtbl.create 317;
  return_type = rt;
  parent = s
}


let genAdd t = function
    | Symbol (name, typ) -> if not(name="_") then Hashtbl.add t name typ

let isNewDecl t = function
    | Symbol (name, typ) -> not( Hashtbl.mem t name)

let rec isNewDeclScope st s = 
  match s with
  | Symbol(name, _) -> (
    if (Hashtbl.mem st.table name) then false
    else match st.parent with 
      | Some i -> isNewDeclScope i s 
      | None   -> true
  )

let forcePut t symbol = match symbol with
    | Symbol (name, typ) -> if (Hashtbl.mem t name) then (
        let prev_type = Hashtbl.find t name
        in
        if not(prev_type = typ) then raise (RedeclaredTypeMismatch name )
    );
    if (name = "_") then () else Hashtbl.add t name typ

let putSymbol t name typ =
  if (Hashtbl.mem t name) then raise (AlreadyDeclared name)
  else if (name = "_") then () else Hashtbl.add t name typ


let putSymbol t symbol =
  match symbol with 
  | Symbol (name, typ) -> putSymbol t name typ


let rec getSymbol st name = 
  if (Hashtbl.mem st.table name) then Some(name, Hashtbl.find st.table name)
  else match st.parent with 
    | Some i -> getSymbol i name
    | None   -> None

let getSymbolInScope st name = if Hashtbl.mem st.table name then Some(name, Hashtbl.find st.table name) else None

let rec getLevel st name l =
  if (Hashtbl.mem st.table name) then l
  else match st.parent with 
    | Some i -> getLevel i name (l+1)
    | None   -> -1

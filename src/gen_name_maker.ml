open Printf
open Str
(* This package: Str must be opam installed *)

let go_file_path () =
    Sys.argv.(2)


let get_basename path_to_file =
    (* This fxn raises an error if the input file does not have an extension *)
    (* desired behaviour since we should be using .go files *)
    Filename.chop_extension (Filename.basename path_to_file)


(* ----------------- COMPUTE JAVA FILE NAME FROM GO FILE -------------------- *)

let programs_regex () =
    Str.regexp "programs\\b"

let programs_solutions_regex () =
    Str.regexp "programs-solution\\b"

let path_to_project_root () =
    Sys.getenv "GOLITEGROUP11"

let test_file_in_dir regex =
    try
        let _ = Str.search_forward regex (go_file_path()) 0 in ();
        true
    with Not_found ->
        false

let rec get_last_elem = function
    | []       -> raise Not_found
    | hd::[]   -> hd
    | hd::tl   -> get_last_elem tl

let create_path regex dir_name is_genfile =
    let split_path = Str.split regex (go_file_path()) in
    (* generated files are stored in the same path structure, but under the generatedCode dir*)
    let path_before_dir = (
        if is_genfile then (path_to_project_root()) ^ "/" ^ "generatedCode/"
        else (path_to_project_root()) ^ "/"
    )
    in path_before_dir ^ dir_name ^ (get_last_elem split_path)


let get_full_path is_genfile =
    let dir_regex = ref (programs_solutions_regex()) in
    if (test_file_in_dir !dir_regex) then create_path !dir_regex "programs-solution" is_genfile
    else (
        dir_regex := programs_regex();
        if (test_file_in_dir !dir_regex) then create_path !dir_regex "programs" is_genfile

        else "Tried to test a file not inside either directories: 'programs', 'programs-solution'"
    )

(* ----------------- COMPUTE CLASS NAME FROM GO FILE ------------------------ *)

let num_regex () = Str.regexp "[0-9]+"

let starts_with_num name =
    (* check if there is a match at position 0 in string *)
    Str.string_match (num_regex()) name 0

let remove_num_prefix name =
    if (starts_with_num name) then
        Str.replace_first (num_regex()) "" name
    else
        name

let starts_with_dash name =
    (* check if there is a match at position 0 in string *)
    Str.string_match (Str.regexp "-") name 0

let remove_dash_prefix name =
    if (starts_with_dash name) then
        Str.global_replace (Str.regexp "-") "" name
    else
        name


let get_java_className () =
    (*
       this function makes the class name of a .java equal to the .go file's name,
       then removes any numbers or '-' from the beginning of the file name so that it is a valid java class name
       EX.) .go file name = 9-1-parenexprs.go
            name = get_basename()  ==  9-1-parenexprs
            output class name = parenexprs
    *)
    let name = ref (get_basename (go_file_path())) in
    let removed_bad_prefixes = ref false in
        while not !removed_bad_prefixes do
            name := remove_num_prefix !name;
            name := remove_dash_prefix !name;
            removed_bad_prefixes := not(
                (starts_with_num !name) || (starts_with_dash !name)
            );
        done;
(*     print_string ("name was: " ^ (get_basename (go_file_path())) ^" new class name: " ^ !name ^ "\n"); *)
    !name


(* This function saves the path of the most recently compile go file and generated java file *)
(* used in the compared_generated.sh script which displays both of these^ files *)
let store_most_recent_run full_path_go full_path_java =
    let file = open_out ((path_to_project_root()) ^ "/" ^ "most_recent_run.txt") in
        Printf.fprintf file "%s\n" full_path_go;
        Printf.fprintf file "%s" full_path_java;
        (* print_string "appended to most recent list\n"; *)
        ()


(* ----------------- entry point function ----------------------------------- *)

let get_gen_file () =
    let to_java_file = true in
    let java_dir = Filename.dirname (get_full_path to_java_file) in
    let class_name = get_java_className() in

        let output_java_file = java_dir ^ "/" ^ class_name ^ ".java" in

(*             print_string ("out file name: " ^ output_java_file ^ "\n"); *)
            let full_go_path   = get_full_path (not to_java_file) in
            store_most_recent_run full_go_path output_java_file;
            output_java_file

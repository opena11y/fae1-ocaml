(**
   Functions for handling WAMT-specific file types
*)

open Filesys
open Graphicfile
open Parser
open Stringlib

let debug = false;;

exception File_Type_Not_Supported;;

(**
   The type wamt_file is used to identify different types
   of files stored in a Site. Each of these documents may be
   dealt with differently (parsed/analyzed separately), and
   stored in different structures.
*)
type wamt_file =
    Html of string
  | Stylesheet of string
  | Image of string
  | Script of string
  | Other of string;;

(**
   Given a directory, return a list of wamt_files contained in
   that directory and its subdirectories.
*)
let get_wamt_file_list dir =
  let flst = Filesys.readdir_list_rec dir in
  let f a b =
    let f_type = Stringlib.get_web_file_suffix b in
    let new_file =
      if f_type = ".HTML"
      then Html(b)
      else if f_type = ".CSS"
      then Stylesheet(b)
      else if f_type = ".GIF" || f_type = ".JPEG" || f_type = ".PNG"
      then Image(b)
      else if f_type = ".JS"
      then Script(b)
      else Other(b)
    in
      [new_file]@a
  in
    List.fold_left f [] flst;;

(**
   Given a list of wamt_files, print the types and names to
   standard output.
*)
let print_file_list lst =
  let f a =
    match a with
        Html s -> Printf.printf "Html: %s\n" s;
      | Stylesheet s -> Printf.printf "Stylesheet: %s\n" s;
      | Image s -> Printf.printf "Image: %s\n" s;
      | Script s -> Printf.printf "Script: %s\n" s;
      | Other s -> Printf.printf "Other: %s\n" s;
  in
    List.iter f lst;;

(** Get the filename of wamt_file f *)
let filename f =
  match f with
      Html s -> s
    | Stylesheet s -> s
    | Image s -> s
    | Script s -> s
    | Other s -> s;;

(**
   Given a wamt_file, read in the file and parse
   the resulting string to an htmlDoc.
*)
let load_html_document_from_file filename =
  if debug
  then print_endline ("loading: " ^ filename);
  let s = Filesys.read_file filename in
    Parser.parse_string s;;

(**
   Given a file path, return the name of the file
   (remove directory information.
*)
let get_file_name f =
  let str_lst = Str.split (Str.regexp "//") f in
  let strs = List.length str_lst in
    if strs <= 1
    then f
    else (
      List.nth str_lst ((List.length str_lst) - 1)
    );;

(**
   Given a file path in a wget download directory, return
   the name of the site.
*)
let get_site_name f =
  let str_lst = Str.split (Str.regexp "//") f in
  let strs = List.length str_lst in
    if strs <= 1
    then f
    else (
      let str = List.nth str_lst ((List.length str_lst) - 1) in
      let s_str_lst = Str.split (Str.regexp "/") str in
        List.hd s_str_lst
    );;

(**
   Container for site-related information, including list of pages
*)

open Html
open Graphicfile
open Page
open Wamtfile
open Wamtml

(**
   Used to store site-related information, including the list of all
   files associated with the site, the list of page-level test results,
   and other lists and hashtables for storing page titles, images, etc.
*)
type site = {
  mutable sitedir : string;
  mutable sitename : string;
  mutable filelist : Wamtfile.wamt_file list;
  mutable page_reports: Wamtml.wamt_page_report list;
  mutable images :  (string, Graphicfile.graphic_file) Hashtbl.t;

  (* For now this would just be stored in a string, so we won't
     bother loading the file in now. Eventually a structure may
     be put in place to analyze scripts. *)
  mutable external_scripts : string list;

  (* Eventually this should be a hashtable with strings
     referencing CSS documents. *)
  mutable external_styles : (string, string) Hashtbl.t;
}

(** Generic constructor for a site *)
let create_site = {
  sitedir = "";
  sitename = "";
  filelist = [];
  page_reports = [];
  images = Hashtbl.create 20;

  external_scripts = [];
  external_styles = Hashtbl.create 20;
};;

(** Set dir of site s to string n. *)
let set_sitedir s n = s.sitedir <- n;;

(** Get dir of site s. *)
let sitedir s = s.sitedir;;

(** Set name of site s to string n. *)
let set_sitename s n = s.sitename <- n;;

(** Get name of site s. *)
let sitename s = s.sitename;;

(** Set files of site s to lst. *)
let set_filelist s lst = s.filelist <- lst;;

(** Get files of site s. *)
let filelist s = s.filelist;;

(** Append page_report pr to site s. *)
let append_page_report s pr =
  s.page_reports <- s.page_reports@[pr];;

(** Get page_reports of site s. *)
let page_reports s = s.page_reports;;

(** Append image i to site s. *)
let append_image s i =
  Hashtbl.add s.images (Graphicfile.get_graphic_file_name i) i;;

(** Get images of site s. *)
let images s = s.images;;

(** Append external stylesheet st to site s. *)
let append_external_style s st =
  Hashtbl.add s.external_styles st st;;

(** Get external stylesheets of site s. *)
let external_styles s = s.external_styles;;

(** Append external script sc to site s. *)
let append_external_script s sc =
  s.external_scripts <- (s.external_scripts@[sc]);;

(** Get external scripts of site s. *)
let external_scripts s = s.external_scripts;;

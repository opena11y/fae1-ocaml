(**
   Contains WAMT main function
*)

open Filesys
open Graphicfile
open Html
open Page
open Site
open Testutil
open Unix
open Wamtfile
open Wamtml
open Wamttest

let debug = false;;
let debug_images = false;;

let site_dir = ref "";;
let out_file = ref "";;
let rpt_name = ref "";;
let rpt_date = ref "";;
let user     = ref "";;
let url_list = ref "";;
let depth    = ref "";;
let span     = ref "";;

let arg_list = [
  ("-dir", Arg.Set_string(site_dir),
   "The directory containing files to be analyzed.");
  ("-out", Arg.Set_string(out_file),
   "The results output XML file.");
  ("-rpt", Arg.Set_string(rpt_name),
   "The user-specified report name.");
  ("-date", Arg.Set_string(rpt_date),
   "The date and time report is generated.");
  ("-user", Arg.Set_string(user),
   "The username of person who generated report.");
  ("-url", Arg.Set_string(url_list),
   "The base URLs used as arguments to wget.");
  ("-depth", Arg.Set_string(depth),
   "The link-following depth used as argument to wget.");
  ("-span", Arg.Set_string(span),
   "The span host option used as argument to wget.")
];;

let anon_fun s = ();;

let get_date_time () =
  let t = Unix.localtime (Unix.time ()) in
  let (day, month, year, hour, min, sec) = (t.tm_mday, t.tm_mon, t.tm_year, t.tm_hour, t.tm_min, t.tm_sec) in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (1900 + year) (month + 1) day hour min sec;;

(**
   Given the top-level of a directory structure containing
   files to be analyzed and the list of wamt_files, create
   and initialize site structure.
*)
let init_site dir files =
  let s = Site.create_site in
    Site.set_sitedir s dir;
    Site.set_sitename s (Wamtfile.get_site_name (Wamtfile.filename (List.hd files)));
    Site.set_filelist s files;
    s;;

(**
   Given a wamt_file of type HTML and its associated
   site directory, create and initialize page structure.
*)
let init_page fname site_dir =
  let doc = Wamtfile.load_html_document_from_file fname in
  let normal_name = Stringlib.normalize_path (Stringlib.normalize_filename fname) site_dir in
  let p = Page.create_page doc in
    Page.set_pagename p normal_name;
    Html.generate_tag_table doc;
    p;;

(**
   Given a site, a page and a list of page_tests (functions),
   get the page title, run the page tests and return the
   wamt_page_report.
*)
let run_page_report site page page_tests =
  let tbl = Html.tag_tbl (Page.document page) in
  let html_title =
    if (Hashtbl.mem tbl "TITLE")
    then (
      let tag = Html.Tag(List.hd (Hashtbl.find tbl "TITLE")) in
        Testutil.normalize_space (Html.get_node_content "" [tag])
    )
    else ""
  in
  let f a b = a@[(b site page)] in
    if debug then print_endline ("--------\n" ^ (Page.pagename page) ^ "\n--------");
    Wamtml.create_wamt_page_report (Page.pagename page)
      html_title
      (List.fold_left f [] page_tests)
      (Unix.gettimeofday ());;

(**
   Filter for processing files with variant type Wamtfile.Image
*)
let process_image_file file site =
  let site_dir = Site.sitedir site in
  match file with
      Wamtfile.Html fname -> ()
    | Wamtfile.Image fname -> (
        if debug_images then print_endline ("Image: " ^ fname);
        let normal_name = Stringlib.normalize_path fname site_dir in
        let gf = Graphicfile.create_from_file normal_name fname in
          Site.append_image site gf;
      )
    | Wamtfile.Stylesheet fname -> ()
    | Wamtfile.Script fname -> ()
    | _ -> raise Wamtfile.File_Type_Not_Supported;;

(**
   Filter for processing files with variant type Wamtfile.Html
*)
let process_html_file file site =
  let site_dir = Site.sitedir site in
  match file with
      Wamtfile.Html fname -> (
        let page = init_page fname site_dir in
        let page_tests = Wamttest.page_tests in
          Site.append_page_report site (run_page_report site page page_tests);
      )
    | Wamtfile.Image fname -> ()
    | Wamtfile.Stylesheet fname -> ()
    | Wamtfile.Script fname -> ()
    | _ -> raise Wamtfile.File_Type_Not_Supported;;

(**
   Given a site with a list of wamt_files, process each file, then
   run the site_tests and return the site report.
*)
let process_site site =
  let filelist = Site.filelist site in
  let f a =
    try
      process_image_file a site;
    with _ -> ();
  in
  let g a =
    try
      process_html_file a site;
    with _ -> ();
  in
    List.iter f filelist;
    List.iter g filelist;
    let site_tests = Wamttest.site_tests in
    let page_reports = Site.page_reports site in
    let run_site_tests a b =
      a@[(b site page_reports)]
    in
      Wamtml.create_wamt_site_report
        (Site.sitename site)
        !rpt_name
        !rpt_date
        !user
        !url_list
        !depth
        !span
        (page_reports)
        (List.fold_left run_site_tests [] site_tests);;

(** The main execution function for wamt. *)
let wamt () =

  Arg.parse arg_list (anon_fun) "";

  if (!site_dir = "")
  then (site_dir := "/home/nhoyt/sites/uiuc/");

  if debug
  then print_endline ("site_dir: " ^ !site_dir);

  if (!rpt_name = "")
  then (rpt_name := "Unspecified");

  if (!rpt_date = "")
  then (rpt_date := get_date_time());

  if (!user = "")
  then (user := "Unspecified");

  if (!url_list = "")
  then (url_list := "Unspecified");

  if (!depth = "")
  then (depth := "Unspecified");

  if (!span = "")
  then (span := "Unspecified");

  if debug
  then Printf.printf "start: %.3f\n" (Unix.gettimeofday ());

  let dir = !site_dir in
  let files = Wamtfile.get_wamt_file_list dir in
    if (List.length files) > 0
    then (
      let site = init_site dir files in
      let site_report = process_site site in
        if debug_images
        then (
          let fp a b = Printf.printf "Site.images: %s\n" a in
            Hashtbl.iter fp (Site.images site)
        );
        if debug
        then Printf.printf "end: %.3f\n" (Unix.gettimeofday ());
        let report_string = Wamtml.wamt_site_report_to_xml site_report in
          if !out_file = ""
          then (
            print_string report_string;
          )
          else (
            Filesys.write_file !out_file report_string;
          )
    )
    else (
      print_endline "No files";
    );;

wamt ();;

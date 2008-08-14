(**
   Functions used for creating the analysis results file
*)

open Str

exception Key_not_found of string;;

(** Returns true if arg is a numeric digit, false otherwise. *)
let is_digit = function
    '0' .. '9' -> true
  | c -> false;;

(** result: key/integer value pair *)
type result = string * int;;

(**
   wamt_test contains:
   id: string that identifies the test, and results: list of
   key/value pairs (association list) where key is a valid id
   attribute value on the r element (see results.dtd and
   entities.dtd), and value is an integer
*)
type wamt_test = {
  mutable id : string;
  mutable results : result list;
};;

(** Get the id of test w *)
let get_id w = w.id;;

(** Get the results list of test w *)
let get_results w = w.results;;

(** Given an id, a results list and a details list, create a new wamt_test. *)
let create_wamt_test n_id n_results = {
  id = n_id;
  results = n_results;
};;

(**
   wamt_page_report contains a page name and title, a list of
   wamt_tests and a timestamp.
*)
type wamt_page_report = {
  mutable page : string;
  mutable html_title : string;
  mutable test_results : wamt_test list;
  mutable timestamp : float;
};;

(**
   Given a string, some wamt_tests and a timestamp, create
   a new wamt_page_report.
*)
let create_wamt_page_report n_page n_html_title n_test_results n_timestamp = {
  page = Str.replace_first (Str.regexp "^/") "" n_page;
  html_title = n_html_title;
  test_results = n_test_results;
  timestamp = n_timestamp;
};;

(**
   Given a list of wamt_test ids, return a list containing
   the corresponding tests in report.
*)
let get_page_report_tests id_lst report =
  let rec f lst t_lst =
    match lst with
        hd::tl -> if List.mem (hd.id) id_lst
        then (f tl ([hd]@t_lst))
        else (f tl t_lst)
      | [] -> t_lst
  in
    f report.test_results [];;

(**
   wamt_site_report is used as a container for page reports
   and test results associated with the site as a whole.
*)
type wamt_site_report = {
  mutable site : string;
  mutable title : string;
  mutable date_time: string;
  mutable user : string;
  mutable urls : string;
  mutable depth : string;
  mutable span : string;
  mutable version : string;
  mutable pages : wamt_page_report list;
  mutable site_test_results : wamt_test list;
};;

(**
   Given a string, a list of page reports, and a list of wamt_tests,
   create a new wamt_site_report.
*)
let create_wamt_site_report n_site n_title n_date_time n_user n_urls n_depth n_span n_version n_pages n_site_test_results = {
  site = n_site;
  title = n_title;
  date_time = n_date_time;
  user = n_user;
  urls = n_urls;
  depth = n_depth;
  span = n_span;
  version = n_version;
  pages = n_pages;
  site_test_results = n_site_test_results;
};;

(** Replace characters that are considered special in XML. *)
let replace_xml_special_chars s =
  let s1 = global_replace (regexp "&") "&amp;" s in
  let s2 = global_replace (regexp "<") "&lt;" s1 in
    global_replace (regexp ">") "&gt;" s2;;

(**
   Given a wamt_test t, return an xml string
   representation of the test.
*)
let wamt_test_to_xml t =
  let r_elements =
    let f a (k, v) =
      a^"  <r id=\"" ^ k ^ "\">" ^ (string_of_int v) ^ "</r>\n" in
      List.fold_left f "" t.results
  in
    Printf.sprintf "<test id=\"%s\">\n%s</test>\n"
      t.id r_elements;;

(**
   Given a wamt_page_report, return an xml string representation
   of the page report.
*)
let wamt_page_report_to_xml r =
  let tests_string =
    let f a b =
      (a^(wamt_test_to_xml b)) in
      List.fold_left f "" r.test_results
  in
    Printf.sprintf "<page>\n<url>%s</url>\n<title>%s</title>\n%s<timestamp>%.3f</timestamp>\n</page>\n"
      (replace_xml_special_chars r.page) r.html_title tests_string r.timestamp;;

(**
   Given a string that has one or more URLs separated by comma-space (', '),
   return an XML fragment that encloses each URL in <url></url> tags.
*)
let urls_to_xml s =
  let rgx = Str.regexp ", " in
    "\n<url>" ^ (Str.global_replace rgx "</url>\n<url>" s) ^ "</url>\n";;

(**
   Given a wamt_site_report, return an xml string representation
   of the site report.
*)
let wamt_site_report_to_xml r =
  let pages_string =
    let f a b =
      (a^(wamt_page_report_to_xml b)) in
      List.fold_left f "" r.pages
  in
  let tests_string =
    let g a b =
      (a^(wamt_test_to_xml b)) in
      List.fold_left g "" r.site_test_results
  in
    Printf.sprintf "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<!DOCTYPE results SYSTEM \"../xml/results.dtd\">\n<results>\n<meta>\n<title>%s</title>\n<date>%s</date>\n<user>%s</user>\n<urls>%s</urls>\n<depth>%s</depth>\n<span>%s</span>\n<version>%s</version>\n</meta>\n%s<site>\n%s</site>\n</results>\n"
      (replace_xml_special_chars r.title)
      r.date_time
      r.user
      (urls_to_xml r.urls)
      r.depth
      r.span
      r.version
      pages_string
      tests_string;;

(* TEST RESULTS FUNCTIONS *)

let get_test_results_all_pages test_id pg_results =
  let f a = List.hd (get_page_report_tests [test_id] a) in
    List.map (f) pg_results;;

(**
   Get integer value by key from results list of test w.
   If key is not in the list, raise Key_not_found exception.
*)
let get_result_value key w =
  try List.assoc key w.results with Not_found ->
    raise (Key_not_found ("test: " ^ w.id ^ " key: " ^ key));;

let sum_result key test_id pg_results =
  let results_list = get_test_results_all_pages test_id pg_results in
  let f a b =
    let (sum_v, pg_count) = a in
    let v = get_result_value key b in
      (sum_v + v, pg_count + 1)
  in
    List.fold_left f (0, 0) results_list;;

let sum_results k1 k2 test_id pg_results =
  let results_list = get_test_results_all_pages test_id pg_results in
  let f a b =
    let (sum_v1, sum_v2, pg_count) = a in
    let v1 = get_result_value k1 b in
    let v2 = get_result_value k2 b in
      (sum_v1 + v1, sum_v2 + v2, pg_count + 1)
  in
    List.fold_left f (0, 0, 0) results_list;;

let sum_results_ratios k1 k2 test_id pg_results =
  let results_list = get_test_results_all_pages test_id pg_results in
  let f a b =
    let (sum_v1, sum_v2, sum_r, pg_count) = a in
    let v1 = get_result_value k1 b in
    let v2 = get_result_value k2 b in
    let r =
      if v1 = 0 || v2 = 0
      then 0.0
      else (float_of_int v1) /. (float_of_int v2)
    in
      (sum_v1 + v1, sum_v2 + v2, sum_r +. r, pg_count + 1)
  in
    List.fold_left f (0, 0, 0.0, 0) results_list;;

let sum_3_results k1 k2 k3 test_id pg_results =
  let results_list = get_test_results_all_pages test_id pg_results in
  let f a b =
    let (sum_v1, sum_v2, sum_v3, pg_count) = a in
    let v1 = get_result_value k1 b in
    let v2 = get_result_value k2 b in
    let v3 = get_result_value k3 b in
      (sum_v1 + v1, sum_v2 + v2, sum_v3 + v3, pg_count + 1)
  in
    List.fold_left f (0, 0, 0, 0) results_list;;

let sum_4_results k1 k2 k3 k4 test_id pg_results =
  let results_list = get_test_results_all_pages test_id pg_results in
  let f a b =
    let (sum_v1, sum_v2, sum_v3, sum_v4, pg_count) = a in
    let v1 = get_result_value k1 b in
    let v2 = get_result_value k2 b in
    let v3 = get_result_value k3 b in
    let v4 = get_result_value k4 b in
      (sum_v1 + v1, sum_v2 + v2, sum_v3 + v3, sum_v4 + v4, pg_count + 1)
  in
    List.fold_left f (0, 0, 0, 0, 0) results_list;;

let sum_6_results k1 k2 k3 k4 k5 k6 test_id pg_results =
  let results_list = get_test_results_all_pages test_id pg_results in
  let f a b =
    let (sum_v1, sum_v2, sum_v3, sum_v4, sum_v5, sum_v6, pg_count) = a in
    let v1 = get_result_value k1 b in
    let v2 = get_result_value k2 b in
    let v3 = get_result_value k3 b in
    let v4 = get_result_value k4 b in
    let v5 = get_result_value k5 b in
    let v6 = get_result_value k6 b in
      (sum_v1 + v1, sum_v2 + v2, sum_v3 + v3, sum_v4 + v4,
       sum_v5 + v5, sum_v6 + v6, pg_count + 1)
  in
    List.fold_left f (0, 0, 0, 0, 0, 0, 0) results_list;;

let sum_7_results k1 k2 k3 k4 k5 k6 k7 test_id pg_results =
  let results_list = get_test_results_all_pages test_id pg_results in
  let f a b =
    let (sum_v1, sum_v2, sum_v3, sum_v4, sum_v5, sum_v6, sum_v7, pg_count) = a in
    let v1 = get_result_value k1 b in
    let v2 = get_result_value k2 b in
    let v3 = get_result_value k3 b in
    let v4 = get_result_value k4 b in
    let v5 = get_result_value k5 b in
    let v6 = get_result_value k6 b in
    let v7 = get_result_value k7 b in
      (sum_v1 + v1, sum_v2 + v2, sum_v3 + v3, sum_v4 + v4,
       sum_v5 + v5, sum_v6 + v6, sum_v7 + v7, pg_count + 1)
  in
    List.fold_left f (0, 0, 0, 0, 0, 0, 0, 0) results_list;;

let sum_results_max k1 k2 k3 test_id pg_results =
  let results_list = get_test_results_all_pages test_id pg_results in
  let f a b =
    let (sum_v1, sum_v2, max_v3, pg_count) = a in
    let v1 = get_result_value k1 b in
    let v2 = get_result_value k2 b in
    let v3 = get_result_value k3 b in
      (sum_v1 + v1, sum_v2 + v2, max max_v3 v3, pg_count + 1)
  in
    List.fold_left f (0, 0, 0, 0) results_list;;

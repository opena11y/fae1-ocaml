(**
   Tests relating to scripting and automation
*)

open Html
open Page
open Testutil
open Wamtml

(* ---------------------------------------------------------------- *)
let has_mouseover_and_focus page =
  let onmouseover_list = Testutil.get_elements_with_attribute "onmouseover" page in
  let onfocus = Testutil.count_elements_with_attribute "onfocus" onmouseover_list in
  let onmouseover = List.length onmouseover_list in
    (onfocus, onmouseover);;

let has_mouseout_and_blur page =
  let onmouseout_list = Testutil.get_elements_with_attribute "onmouseout" page in
  let onblur = Testutil.count_elements_with_attribute "onblur" onmouseout_list in
  let onmouseout = List.length onmouseout_list in
    (onblur, onmouseout);;

(* ---------------------------------------------------------------- *)
(** 001p: How many elements with mouseover also have onfocus *)
let test001p site page =
  let test_id = "auto001p" in
    let (count, total) = has_mouseover_and_focus page in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 001s: Sitewide results for test001p *)
let test001s site pg_results=
  let test_id = "auto001s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "auto001p" pg_results
    in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 002p: How many elements with mouseout also have onblur *)
let test002p site page =
  let test_id = "auto002p" in
    let (count, total) = has_mouseout_and_blur page in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 002s: Sitewide results for test002p *)
let test002s site pg_results =
  let test_id = "auto002s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "auto002p" pg_results
    in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003p: Calls to document.write and document.writeln *)
let test003p site page =
  let test_id = "auto003p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let scripts =
      try
        Hashtbl.find tag_tbl "SCRIPT"
      with _ -> []
    in
    let write_re = Str.regexp_case_fold "document.write[ ]*(" in
    let writeln_re = Str.regexp_case_fold "document.writeln[ ]*(" in
    let f a b =
      let (write, writeln, total) = a in
      let script_children = Html.tag_children b in
      let script_text = Html.get_node_content "" script_children in
      let write_count = Testutil.count_matches write_re script_text in
      let writeln_count = Testutil.count_matches writeln_re script_text in
        (write + write_count,
         writeln + writeln_count,
         total + write_count + writeln_count)
    in
    let (write_count, writeln_count, total) =
      List.fold_left f (0,0,0) scripts
    in
    let results = [
      ("cnt1", write_count);
      ("cnt2", writeln_count);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003s: Sitewide results for test003p *)
let test003s site pg_results =
  let test_id = "auto003s" in
    let (sum_cnt1, sum_cnt2, pg_count) =
      Wamtml.sum_results "cnt1" "cnt2" "auto003p" pg_results
    in
    let total = sum_cnt1 + sum_cnt2 in
    let page_avg = Testutil.round (Testutil.ratio_of_ints total pg_count) in
    let results = [
      ("cnt1", sum_cnt1);
      ("cnt2", sum_cnt2);
      ("tot1", total);
      ("tot2", pg_count);
      ("avg1", page_avg)
    ] in
      Wamtml.create_wamt_test test_id results;;

(**
   Tests relating to text equivalents
*)

open Html
open Page
open Testutil
open Wamtml

let element_with_alt_text e_name page =
  let tbl = Html.tag_tbl (Page.document page) in
  let e_lst =
    try
      Hashtbl.find tbl e_name
    with _ -> []
  in
  let attr_pred a = (Html.attr_key a = "alt") &&
    not (Html.attr_value a = "") in
  let tag_pred t =
    let (lp, lf) =
      List.partition attr_pred (Html.tag_attributes t)
    in
      if (List.length lp > 0)
      then true
      else false
  in
  let (lp,lf) = List.partition tag_pred e_lst in
    ((List.length lp),(List.length e_lst));;

(* ---------------------------------------------------------------- *)
let element_with_children e_name page =
  let tbl = Html.tag_tbl (Page.document page) in
  let object_list =
    try Hashtbl.find tbl "OBJECT"
    with _ -> []
  in
  let f a b =
    if (List.length (Html.tag_children b) > 0)
    then (a + 1)
    else a
  in
  let objects_with_content = List.fold_left f 0 object_list in
  let object_count = List.length object_list in
  let ratio =
    if object_count = 0
    then 0.0
    else (float_of_int objects_with_content) /. (float_of_int object_count)
  in
    (ratio, objects_with_content, object_count);;

(* ANALYSIS FUNCTIONS *)

(* ---------------------------------------------------------------- *)
(** 001p: return num of images with alt text out of total images *)
let test001p site page =
  let test_id = "text001p" in
    Testutil.msg test_id;
    let (count, total) = element_with_alt_text "IMG" page in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 001s: sitewide aggregation of 001p results *)
let test001s site pg_results =
  let test_id = "text001s" in
    Testutil.msg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text001p" pg_results
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
(** 002p: return num of embed elements with alt text out of total
    embed elements *)
let test002p site page =
  let test_id = "text002p" in
    Testutil.msg test_id;
    let (count, total) = element_with_alt_text "EMBED" page in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 002s: sitewide aggregation of 002p results *)
let test002s site pg_results =
  let test_id = "text002s" in
    Testutil.msg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text002p" pg_results
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
(** 003p: return num applet elements with alt text out of total
    applet elements *)
let test003p site page =
  let test_id = "text003p" in
    Testutil.msg test_id;
    let (count, total) = element_with_alt_text "APPLET" page in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003s: sitewide aggregation of 003p results *)
let test003s site pg_results =
  let test_id = "text003s" in
    Testutil.msg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text003p" pg_results
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
(** 004p: return num object elements with alternative content
    out of total object elements. *)
let test004p site page =
  let test_id = "text004p" in
    Testutil.msg test_id;
    let (ratio, objects_with_content, object_count) =
      element_with_children "OBJECT" page
    in
    let results = [
      ("cnt1", objects_with_content);
      ("tot1", object_count);
      ("pct1", Testutil.pct_of_ratio ratio)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 004s: sitewide aggregation of 004p results *)
let test004s site pg_results =
  let test_id = "text004s" in
    Testutil.msg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text004p" pg_results
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
(** 005p: return num of images with alt attribute (empty or otherwise)
    out of total images *)
let test005p site page =
  let test_id = "text005p" in
    Testutil.msg test_id;
    let tbl = Html.tag_tbl (Page.document page) in
    let img_elements =
      try
        Hashtbl.find tbl "IMG"
      with _ -> []
    in
    let pred t = Html.has_attribute t "alt" in
    let img_with_alt = List.filter pred img_elements in
    let count = List.length img_with_alt in
    let total = List.length img_elements in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 005s: sitewide aggregation of 005p results *)
let test005s site pg_results =
  let test_id = "text005s" in
    Testutil.msg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text005p" pg_results
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
(** 006p: return num of images with alt attribute (empty or otherwise)
    out of total images *)
let test006p site page =
  let test_id = "text006p" in
    Testutil.msg test_id;
    let tbl = Html.tag_tbl (Page.document page) in
    let img_elements =
      try
        Hashtbl.find tbl "IMG"
      with _ -> []
    in
    let alt_exists t = Html.has_attribute t "alt" in
    let img_with_alt = List.filter alt_exists img_elements in
    let alt_non_empty t = Html.has_non_blank_attribute t "alt" in
    let img_with_non_empty_alt = List.filter alt_non_empty img_with_alt in
    let count = List.length img_with_non_empty_alt in
    let total = List.length img_with_alt in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 006s: sitewide aggregation of 006p results *)
let test006s site pg_results =
  let test_id = "text006s" in
    Testutil.msg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text006p" pg_results
    in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

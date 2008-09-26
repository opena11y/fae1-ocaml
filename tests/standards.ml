(**
   Tests relating to markup standards
*)

open Html
open Page
open Testutil
open Wamtml

(* ---------------------------------------------------------------- *)
let has_doctype page =
  String.length (Page.doctype page) > 0;;

(* ---------------------------------------------------------------- *)
let has_character_encoding page =
  let meta_list =
    try
      Hashtbl.find (Html.tag_tbl (Page.document page)) "META"
    with _ -> []
  in
  let check_meta a m =
    let attr_list = Html.tag_attributes m in
    let he_pred a b =
      if Html.attr_key b = "http-equiv" &&
        String.lowercase (Html.attr_value b) = "content-type"
      then true
      else a || false
    in
    let c_pred a b =
      if Html.attr_key b = "content" &&
        Testutil.contains (Html.attr_value b) "charset="
      then true
      else a || false
    in
      if List.fold_left he_pred false attr_list &&
        List.fold_left c_pred false attr_list
      then true
      else a || false
  in
    List.fold_left check_meta false meta_list;;

(* ---------------------------------------------------------------- *)
(** 001p: Is doctype declared on page? *)
let test001p site page =
  let test_id = "std001p" in
    Testutil.msg test_id;
    let results = [
      ("b1", Testutil.int_of_bool (has_doctype page))
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 001s: Sitewide results for 001p *)
let test001s site pg_results =
  let test_id = "std001s" in
    Testutil.msg test_id;
    let (count, total) =
      Wamtml.sum_result "b1" "std001p" pg_results
    in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 002p: Is character encoding declared with meta tag on page? *)
let test002p site page =
  let test_id = "std002p" in
    Testutil.msg test_id;
    let results = [
      ("b1", Testutil.int_of_bool (has_character_encoding page))
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 002s: Sitewide results for 002p *)
let test002s site pg_results =
  let test_id = "std002s" in
    Testutil.msg test_id;
    let (count, total) =
      Wamtml.sum_result "b1" "std002p" pg_results
    in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(**
   Tests relating to markup standards
*)

open Html
open Page
open Testutil
open Wamtml

let string_contains rgx s =
  let pos = try
    Str.search_forward rgx s 0
  with _ -> -1
  in (pos >= 0) ;;

(* ---------------------------------------------------------------- *)
let has_doc_type page =
  let rec contains_doc_type_node n =
    match n with
        Html.SpecialMarkup t ->
          if(string_contains (Str.regexp_string "DOCTYPE") t)
          then true
          else false
      | Html.Tag t ->
          let f a b =
            if(contains_doc_type_node b)
            then true
            else a || false
          in
            List.fold_left f false (Html.tag_children t)
      | _ -> false
  in
    List.length (List.filter (contains_doc_type_node)
                   (Html.doc_model (Page.document page))) > 0;;

(* ---------------------------------------------------------------- *)
let has_default_language page =
  let tbl = Html.tag_tbl (Page.document page) in
    try
      let html_tags = Hashtbl.find tbl "HTML" in
      let pred attr = (Html.attr_key attr = "lang" || Html.attr_key attr = "xml:lang")
        && not (Html.attr_value attr = "")
      in
      let f a b =
        if pred b
        then true
        else a || false
      in
      let g x y =
        if List.fold_left f false (Html.tag_attributes y)
        then true
        else x || false
      in List.fold_left g false html_tags
    with _ -> false;;

(* ---------------------------------------------------------------- *)
let has_character_encoding page =
  let meta_list =
    try
      Hashtbl.find (Html.tag_tbl (Page.document page)) "META"
    with _ -> []
  in
  let check_meta a m =
    let attr_list = Html.tag_attributes m in

    let rgx = Str.regexp_string "charset=" in
    let he_pred a b =
      if (Html.attr_key b = "http-equiv" &&
          (String.lowercase (Html.attr_value b)) = "content-type")
      then true
      else a || false
    in
    let c_pred a b =
      if(Html.attr_key b = "content" &&
          (string_contains rgx (Html.attr_value b)))
      then true
      else a || false
    in
      if (List.fold_left he_pred false attr_list &&
            List.fold_left c_pred false attr_list)
      then true
      else a || false
  in List.fold_left check_meta false meta_list;;

(* ---------------------------------------------------------------- *)
(** 001p: Is doctype declared on page? *)
let test001p site page =
  let test_id = "std001p" in
    Testutil.msg test_id;
    let results = [
      ("b1", Testutil.int_of_bool (has_doc_type page))
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
(** 002p: Is default language declared on page? *)
let test002p site page =
  let test_id = "std002p" in
    Testutil.msg test_id;
    let results = [
      ("b1", Testutil.int_of_bool (has_default_language page))
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

(* ---------------------------------------------------------------- *)
(** 003p: Is character encoding declared on page? *)
let test003p site page =
  let test_id = "std003p" in
    Testutil.msg test_id;
    let results = [
      ("b1", Testutil.int_of_bool (has_character_encoding page))
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003s: Sitewide results for 003p *)
let test003s site pg_results =
  let test_id = "std003s" in
    Testutil.msg test_id;
    let (count, total) =
      Wamtml.sum_result "b1" "std003p" pg_results
    in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

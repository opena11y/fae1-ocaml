(**
   Tests relating to style markup and CSS
*)

open Graphicfile
open Html
open Htmlutil
open Page
open Site
open Stringlib
open Testutil
open Wamtml

let debug = false
let msg = (Testutil.msg debug)

(* ---------------------------------------------------------------- *)
(** 001p: Percentage of content styled by inline style tags *)
let test001p site page =
  let test_id = "style001p" in
    let doc_model = Html.doc_model (Page.document page) in
    let style_tags = ["B";"I";
                      "FONT";"U";
                      "BLINK";"CENTER";
                      "BASEFONT";"STRIKE";"MARQUEE"]
    (*
    let style_tags = ["B"; "I"; "TT";
                      "BIG"; "SMALL";
                      "STRIKE"; "S"; "U";
                      "FONT"; "BASEFONT";
                      "BLINK"; "CENTER"; "MARQUEE"]
    *)
    in
    let rec f c_count t_lst =
      match t_lst with
          (Html.Tag t)::tl -> (
            if (List.mem (Html.tag_name t) style_tags)
            then f (c_count + Html.get_node_content_weight 0 [(Html.Tag t)]) tl
            else f c_count ((Html.tag_children t)@tl)
          )
        | hd::tl -> f c_count tl
        | [] -> c_count
    in
    let total_content = Html.get_node_content_weight 0 doc_model in
    let content_in_style = f 0 (doc_model) in
    let percent = Testutil.pct_of_ints content_in_style total_content in
    let results = [
      ("cnt1", content_in_style);
      ("tot1", total_content);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 001s: Site average of 001p *)
let test001s site pg_results =
  let test_id = "style001s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "style001p" pg_results
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
(** 002p: How many nested tables, and at what maximum depth *)
let test002p site page =
  let test_id = "style002p" in
    let doc_model = Html.doc_model (Page.document page) in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let table_count =
      try List.length (Hashtbl.find tag_tbl "TABLE")
      with Not_found -> 0
    in
    let (nested_count, max_depth) = Htmlutil.count_nested_tables 0 0 0 doc_model in
    let results = [
      ("cnt1", nested_count);
      ("tot1", table_count);
      ("r1", max_depth)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 002s: Site average of 002p *)
let test002s site pg_results =
  let test_id = "style002s" in
    let (nested, total, max, pg_count) =
      Wamtml.sum_results_max "cnt1" "tot1" "r1" "style002p" pg_results
    in
    let results = [
      ("cnt1", nested);
      ("tot1", total);
      ("r1", max);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003p: How many images appear to be for decorative purposes only *)
let test003p site page =
  let test_id = "style003p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let page_images =
      try Hashtbl.find tag_tbl "IMG"
      with _ -> []
    in
    let site_images = Site.images site in
    let f a b =
      let (count, found, total) = a in
        if (Html.has_attribute b "src")
        then (
          let attr_val = Html.attr_value (Html.get_attribute b "src") in
          let page_name = Page.pagename page in
          let img_path = Stringlib.construct_uri_path page_name attr_val in
            msg attr_val img_path;
            try
              let img = Hashtbl.find site_images img_path in
              let (h,w) = Graphicfile.get_graphic_file_dimensions img in
                msg "dimensions" (Printf.sprintf "%s: %d %d\n" img_path h w);
                if ( h < 4 || w < 4)
                then (count + 1, found + 1, total + 1)
                else (count, found + 1, total + 1)
            with Not_found -> (
              msg "Not found" img_path;
              (count, found, total + 1)
            )
              | _ -> (count, found, total + 1)
        )
        else (count, found, total + 1)
    in
    let (count, found, total) = List.fold_left f (0, 0, 0) page_images in
    let pct_found = Testutil.pct_of_ints found total in
    let results = [
      ("cnt1", count);
      ("cnt2", found);
      ("pct1", pct_found);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003s: Sitewide average of 003p *)
let test003s site pg_results =
  let test_id = "style003s" in
    let (sum_count, sum_found, sum_total, pg_count) =
      Wamtml.sum_3_results "cnt1" "cnt2" "tot1" "style003p" pg_results
    in
    let pct_found = Testutil.pct_of_ints sum_found sum_total in
    let results = [
      ("cnt1", sum_count);
      ("cnt2", sum_found);
      ("pct1", pct_found);
      ("tot1", sum_total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 004p: Counts of inline style tags found *)
let test004p site page =
  let test_id = "style004p" in
    let tbl = Html.tag_tbl (Page.document page) in

    let b        = Testutil.count_tags tbl "B" in
    let basefont = Testutil.count_tags tbl "BASEFONT" in
    let blink    = Testutil.count_tags tbl "BLINK" in
    let center   = Testutil.count_tags tbl "CENTER" in
    let font     = Testutil.count_tags tbl "FONT" in
    let i        = Testutil.count_tags tbl "I" in
    let marquee  = Testutil.count_tags tbl "MARQUEE" in
    let strike   = Testutil.count_tags tbl "STRIKE" in
    let u        = Testutil.count_tags tbl "U" in

    let total = (b + basefont + blink + center + font + i + marquee + strike + u) in

    let results = [
      ("r1", b);
      ("r2", basefont);
      ("r3", blink);
      ("r4", center);
      ("r5", font);
      ("r6", i);
      ("r7", marquee);
      ("r8", strike);
      ("r9", u);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 004s: Sitewide average of 004p *)
let test004s site pg_results =
  let test_id = "style004s" in
    let (sum_t, pg_count) =
      Wamtml.sum_result "tot1" "style004p" pg_results
    in
    let site_avg = Testutil.round (Testutil.ratio_of_ints sum_t pg_count) in
    let results = [
      ("tot1", sum_t);
      ("tot2", pg_count);
      ("avg1", site_avg)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 005p: Count of elements with color attribute *)
let test005p site page =
  let test_id = "style005p" in
    let elements = Testutil.get_elements_with_attribute "color" page in
    let results = [
      ("cnt1", List.length elements)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 005s: Sitewide average of 005p *)
let test005s site pg_results =
  let test_id = "style005s" in
    let (sum_c, pg_count) =
      Wamtml.sum_result "cnt1" "style005p" pg_results
    in
    let site_avg = Testutil.round (Testutil.ratio_of_ints sum_c pg_count) in
    let results = [
      ("tot1", sum_c);
      ("tot2", pg_count);
      ("avg1", site_avg)
    ] in
      Wamtml.create_wamt_test test_id results;;

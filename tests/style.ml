(**
   Tests relating to style markup and CSS
*)

let debug = true
let msg = (Testutil.msg debug)
let runmsg = (Testutil.msg debug "Running")

(* ---------------------------------------------------------------- *)
(** 001p: Count number of center and font elements. *)
let test001p site page =
  let test_id = "style001p" in
    runmsg test_id;
    let tbl = Html.tag_tbl (Page.document page) in

    let center = Testutil.count_tags tbl "CENTER" in
    let font   = Testutil.count_tags tbl "FONT" in
    let total  = (center + font) in

    let results = [
      ("cnt1", center);
      ("cnt2", font);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 002p: Count number of b elements. *)
let test002p site page =
  let test_id = "style002p" in
    runmsg test_id;
    let tbl = Html.tag_tbl (Page.document page) in
    let b = Testutil.count_tags tbl "B" in
    let results = [
      ("cnt1", b)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003p: Count number of i elements. *)
let test003p site page =
  let test_id = "style003p" in
    runmsg test_id;
    let tbl = Html.tag_tbl (Page.document page) in
    let i = Testutil.count_tags tbl "I" in
    let results = [
      ("cnt1", i)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 004p: Count number of u elements. *)
let test004p site page =
  let test_id = "style004p" in
    runmsg test_id;
    let tbl = Html.tag_tbl (Page.document page) in
    let u = Testutil.count_tags tbl "U" in
    let results = [
      ("cnt1", u)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 005p: Count number of center and font elements. *)
let test005p site page =
  let test_id = "style005p" in
    runmsg test_id;
    let tbl = Html.tag_tbl (Page.document page) in

    let blink   = Testutil.count_tags tbl "BLINK" in
    let marquee = Testutil.count_tags tbl "MARQUEE" in
    let total  = (blink + marquee) in

    let results = [
      ("cnt1", blink);
      ("cnt2", marquee);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 006p: How many nested tables, and at what maximum depth *)
let test006p site page =
  let test_id = "style006p" in
    runmsg test_id;
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
(** 006s: Site average of 006p *)
let test006s site pg_results =
  let test_id = "style006s" in
    runmsg test_id;
    let (nested, total, max, pg_count) =
      Wamtml.sum_results_max "cnt1" "tot1" "r1" "style006p" pg_results
    in
    let results = [
      ("cnt1", nested);
      ("tot1", total);
      ("r1", max);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

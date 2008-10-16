(**
   Tests relating to navigation and orientation
*)

open Wamtml
open Navutil

(* ---------------------------------------------------------------- *)
(** 001p: Does the page's html element have a lang attribute with a
    valid two-character IANA language code as its value?
*)
let test001p site page =
  let test_id = "nav001p" in
    let (has_lang_attr, is_valid_code) = Navutil.has_default_language page in
    let results = [
      ("b1", Testutil.int_of_bool has_lang_attr);
      ("b2", Testutil.int_of_bool is_valid_code)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003p: This test determines which tables on a page are data tables.
    It also stores some additional information about those tables so
    we don't have to process things multiple times. This test MUST be
    run before other tests with dependences on that additional info.
*)
let test003p site page =
  let test_id = "nav003p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let tables = try Hashtbl.find tag_tbl "TABLE" with _ -> [] in
    let f a b =
      let (total_tables, data_tables, thead_count, th_count) = a in
      let (is_data_table, (num_rows, num_columns), has_thead, has_th) =
        Htmlutil.is_data_table b in
        if is_data_table
        then (total_tables + 1, data_tables + 1,
              thead_count + (if has_thead then 1 else 0),
              th_count + (if has_th then 1 else 0))
        else (total_tables + 1, data_tables, thead_count, th_count)
    in
    let (table_count, data_table_count,with_thead_count, with_th_count) =
      List.fold_left f (0,0,0,0) tables in
    let results = [
      ("cnt1", data_table_count);
      ("tot1", table_count);
      ("cnt2", with_thead_count);
      ("cnt3", with_th_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 004p: How many of the frames do not have titles? *)
let test004p site page =
  let test_id = "nav004p" in
    let tag_tbl = (Html.tag_tbl (Page.document page)) in
    let frames = try Hashtbl.find tag_tbl "FRAME" with _ -> [] in
    let check_frames a b =
      if (Html.has_non_blank_attribute b "title")
      then a + 1
      else a
    in
    let count = List.fold_left check_frames 0 frames in
    let total = List.length frames in
    let offenders = total - count in
    let percent = Testutil.pct_of_ints offenders total in
    let results = [
      ("cnt1", offenders);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 004s: sitewide aggregation of 004p results *)
let test004s site pg_results =
  let test_id = "nav004s" in
    let (count_offenders, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav004p" pg_results
    in
    let percent = Testutil.pct_of_ints count_offenders total in
    let results = [
      ("cnt1", count_offenders);
      ("tot1", total);
      ("pct1", percent);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 005p: Number of framesets with duplicate frame title attribute values. *)
let test005p site page =
  let test_id = "nav005p" in
    let tag_tbl = (Html.tag_tbl (Page.document page)) in
    let framesets = try Hashtbl.find tag_tbl "FRAMESET" with _ -> [] in
    let get_frame_titles frameset =
      let frames = Testutil.get_named_child_elements frameset "FRAME" in
        Testutil.get_attribute_values "title" frames
    in
    let check_framesets a b =
      let titles = Stringlib.normalize_strings (get_frame_titles b) in
      let (cnt_unique, tot_titles) = Testutil.count_unique_strings titles in
      if cnt_unique = tot_titles
      then a + 1
      else a
    in
    let count = List.fold_left check_framesets 0 framesets in
    let total = List.length framesets in
    let offenders = total - count in
    let percent = Testutil.pct_of_ints offenders total in
    let results = [
      ("cnt1", offenders);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 005s: sitewide aggregation of 005p results *)
let test005s site pg_results =
  let test_id = "nav005s" in
    let (count_offenders, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav005p" pg_results
    in
    let percent = Testutil.pct_of_ints count_offenders total in
    let results = [
      ("cnt1", count_offenders);
      ("tot1", total);
      ("pct1", percent);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(* TITLE ELEMENT *)
(* ---------------------------------------------------------------- *)

(** 010p: Find the number of empty title elements.
    @return cnt1: number of empty title elements
    @return tot1: total number of title elements
*)
let test010p site page =
  let test_id = "nav010p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let titles = try Hashtbl.find tag_tbl "TITLE" with _ -> [] in
    let titles_with_content = List.filter Testutil.has_content titles in
    let total = List.length titles in
    let results = [
      ("cnt1", total - List.length titles_with_content);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 010s: sitewide aggregation of 010p results *)
let test010s site pg_results =
  let test_id = "nav010s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav010p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count);
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 011p: Test whether page has exactly one title element.
    @return b1: true if criterion met, false otherwise
    @return cnt1: number of title elements
*)
let test011p site page =
  let test_id = "nav011p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let titles = try Hashtbl.find tag_tbl "TITLE" with _ -> [] in
    let count = List.length titles in
    let results = [
      ("b1", Testutil.int_of_bool (count = 1));
      ("cnt1", count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 011s: sitewide aggregation of 011p results *)
let test011s site pg_results =
  let test_id = "nav011s" in
    let (count, pg_count) =
      Wamtml.sum_result "b1" "nav011p" pg_results
    in
    let percent = Testutil.pct_of_ints count pg_count in
    let results = [
      ("cnt1", count);
      ("tot1", pg_count);
      ("pct1", percent);
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 012s: Are all the title elements for all the pages unique? *)
let test012s site pg_results =
  let test_id = "nav012s" in
    let f a b =
      let title = b.html_title in
        a@[title]
    in
    let title_list =
      List.fold_left f [] (Site.page_reports site)
    in
    let rec g c lst =
      match lst with
          hd::tl ->
            let pred s = (
              let r = String.compare hd s = 0 in
                r)
            in
            let (lp, lf) = List.partition pred tl in
              if (List.length lp > 0)
              then (g c lf)
              else (g (c + 1) lf)
        | [] -> c
    in
    let count = g 0 title_list in
    let total = List.length title_list in
    let percent = Testutil.pct_of_ints count total in

    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(* H1 ELEMENTS *)
(* ---------------------------------------------------------------- *)

(** 020p: Test for empty h1 elements, taking img alt text into account.
    @return cnt1: number of empty elements
    @return tot1: total number of elements
*)
let test020p site page =
  let test_id = "nav020p" in
    let tbl = Html.tag_tbl (Page.document page) in
    let h1s = try Hashtbl.find tbl "H1" with _ -> [] in
    let h1s_with_content = List.filter Testutil.has_content_with_img_alt h1s in
    let total = List.length h1s in
    let results = [
      ("cnt1", total - List.length h1s_with_content);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 020s: sitewide aggregation of 020p results *)
let test020s site pg_results =
  let test_id = "nav020s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav020p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 021p: Find the number of empty h1 elements, excluding img alt text.
    @return cnt1: number of empty elements
    @return tot1: total number of elements
*)
let test021p site page =
  let test_id = "nav021p" in
    let tbl = Html.tag_tbl (Page.document page) in
    let h1s = try Hashtbl.find tbl "H1" with _ -> [] in
    let h1s_with_content = List.filter Testutil.has_content h1s in
    let total = List.length h1s in
    let results = [
      ("cnt1", total - List.length h1s_with_content);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 021s: sitewide aggregation of 021p results *)
let test021s site pg_results =
  let test_id = "nav021s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav021p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 022p: Test that page has at least one h1 element.
    @return b1: true if criterion met, false otherwise
    @return cnt1: number of h1 elements
*)
let test022p site page =
  let test_id = "nav022p" in
    let tbl = Html.tag_tbl (Page.document page) in
    let h1s = try Hashtbl.find tbl "H1" with _ -> [] in
    let count = List.length h1s in
    let results = [
      ("b1", Testutil.int_of_bool (count >= 1));
      ("cnt1", count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 022s: sitewide aggregation of 022p results *)
let test022s site pg_results =
  let test_id = "nav022s" in
    let (count, pg_count) =
      Wamtml.sum_result "b1" "nav022p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", pg_count);
      ("pct1", Testutil.pct_of_ints count pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 023p: Test that page has no more than two h1 elements.
    @return b1: true if criterion met
    @return cnt1: number of h1 elements
*)
let test023p site page =
  let test_id = "nav023p" in
    let tbl = Html.tag_tbl (Page.document page) in
    let h1s = try Hashtbl.find tbl "H1" with _ -> [] in
    let count = List.length h1s in
    let results = [
      ("b1", Testutil.int_of_bool (count <= 2));
      ("cnt1", count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 023s: sitewide aggregation of 023p results *)
let test023s site pg_results =
  let test_id = "nav023s" in
    let (count, pg_count) =
      Wamtml.sum_result "b1" "nav023p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", pg_count);
      ("pct1", Testutil.pct_of_ints count pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 024p: Test that the text content of each h1 matches all or part of
    the title content.
    @return cnt1: count of h1s that do not match
    @return tot1: total h1 elements on page
    @return pct1: percentage of h1s on page that do not match
    @return tot2: total title elements on page
*)
let test024p site page =
  let test_id = "nav024p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let titles = try Hashtbl.find tag_tbl "TITLE" with _ -> [] in
    let h1s = try Hashtbl.find tag_tbl "H1" with _ -> [] in
    let total_titles = List.length titles in
    let total_h1s = List.length h1s in
    let pass_h1s =
      if total_titles > 0 && total_h1s > 0
      then
        let title_content = Testutil.normalize_alphanumeric (Html.get_node_content "" [Html.Tag (List.hd titles)]) in
        let is_substring a b =
          let h1_content = Testutil.normalize_alphanumeric (Html.get_node_content_with_img_alt "" [Html.Tag b]) in
            if Testutil.icontains title_content h1_content
            then a + 1
            else a
        in
          List.fold_left is_substring 0 h1s
      else 0
    in
    let fail_h1s = total_h1s - pass_h1s in
    let results = [
      ("cnt1", fail_h1s);
      ("tot1", total_h1s);
      ("pct1", Testutil.pct_of_ints fail_h1s total_h1s);
      ("tot2", total_titles)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 024s: sitewide aggregation of 024p results *)
let test024s site pg_results =
  let test_id = "nav024s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav024p" pg_results in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(* SUBHEADING ELEMENTS *)
(* ---------------------------------------------------------------- *)

(** 030p: Find the number of empty subheading elements (h2..h6),
    taking img alt text into account.
    @return cnt1: number of empty elements
    @return tot1: total number of elements
*)
let test030p site page =
  let test_id = "nav030p" in
    let subheadings = get_subheading_elements page in
    let subheadings_with_content = List.filter Testutil.has_content_with_img_alt subheadings in
    let total = List.length subheadings in
    let results = [
      ("cnt1", total - List.length subheadings_with_content);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 030s: sitewide aggregation of 030p results *)
let test030s site pg_results =
  let test_id = "nav030s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav030p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 031p: Find the number of empty subheading elements (h2..h6),
    excluding img alt text.
    @return cnt1: number of empty elements
    @return tot1: total number of elements
*)
let test031p site page =
  let test_id = "nav031p" in
    let subheadings = get_subheading_elements page in
    let subheadings_with_content = List.filter Testutil.has_content subheadings in
    let total = List.length subheadings in
    let results = [
      ("cnt1", total - List.length subheadings_with_content);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 031s: sitewide aggregation of 031p results *)
let test031s site pg_results =
  let test_id = "nav031s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav031p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 032p: Of the heading elements that follow the last h1 element,
    find how many are improperly nested.
    @return cnt1: number of offenders
    @return tot1: total number of heading elements that follow the last h1
    @return cnt2: number of h1 elements
    @return tot2: total number of heading elements
*)
let test032p site page =
  let test_id = "nav032p" in

    (* get all of the heading elements in document order *)
    let names = ["H1"; "H2"; "H3"; "H4"; "H5"; "H6"] in
    let headings = Testutil.get_elements_with_names names page in

    (* count H1 elements *)
    let h1_count = Testutil.count_elements_with_name "H1" headings in

    (* get list of elements that follow the last H1 *)
    let successors = Testutil.get_successor_elements_last_occurrence "H1" headings in

    (* convert successors list to a simple list of strings *)
    let tagnames = List.map (fun t -> Html.tag_name t) successors in

    (* function that extracts the single digit from H1..H6 *)
    let ordinal s =
      int_of_string (String.sub s 1 1)
    in

    (* function that builds list of elements that aren't properly nested *)
    let rec get_offenders ele lst =
      match lst with
          hd :: tl ->
            let ord1 = ordinal ele in
            let ord2 = ordinal hd in
              if (ord2 - ord1) > 1 || ord2 = 1
              then hd :: get_offenders hd tl
              else get_offenders hd tl
        | [] -> []
    in

    (* analyze subheading tags by passing in H1 as first element *)
    let offenders = get_offenders "H1" tagnames in

    let results = [
      ("cnt1", List.length offenders);
      ("tot1", List.length successors);
      ("cnt2", h1_count);
      ("tot2", List.length headings)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 032s: sitewide aggregation of 032p results *)
let test032s site pg_results =
  let test_id = "nav032s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav032p" pg_results
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
(** 033p: Return total number of subheading elements and counts for h2..h6 *)
(*
let test033p site page =
  let test_id = "nav033p" in
    let tbl = Html.tag_tbl (Page.document page) in
    let h2 = try Hashtbl.find tbl "H2" with _ -> [] in
    let h3 = try Hashtbl.find tbl "H3" with _ -> [] in
    let h4 = try Hashtbl.find tbl "H4" with _ -> [] in
    let h5 = try Hashtbl.find tbl "H5" with _ -> [] in
    let h6 = try Hashtbl.find tbl "H6" with _ -> [] in
    let counts = List.map List.length [h2; h3; h4; h5; h6] in
    let total = List.fold_left (+) 0 counts in
    let results = [
      ("r2", List.length h2);
      ("r3", List.length h3);
      ("r4", List.length h4);
      ("r5", List.length h5);
      ("r6", List.length h6);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;
*)

(* ---------------------------------------------------------------- *)
(** 033s: sitewide aggregation for 033p results *)
(*
let test033s site pg_results =
  let test_id = "nav033s" in
    let (r2, r3, r4, r5, r6, total, pg_count) =
      Wamtml.sum_6_results "r2" "r3" "r4" "r5" "r6" "tot1" "nav033p" pg_results
    in
    let results = [
      ("r2", r2);
      ("r3", r3);
      ("r4", r4);
      ("r5", r5);
      ("r6", r6);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;
*)

(* ---------------------------------------------------------------- *)
(* NAVIGATION MENUS *)
(* ---------------------------------------------------------------- *)

(** 041p: Return the number of list navigation menus (ul or ol
    elements for which all or all but one of their li children contain
    only a link element OR a link element plus a nested navigation
    menu) that are not immediately preceded by a heading element.
*)
let test041p site page =
  let test_id = "nav041p" in
    let doc_model = Html.doc_model (Page.document page) in
    let h1s = Testutil.get_elements_with_name "H1" page in
    let cnt_h1s = List.length h1s in
    let (count, total_menus, total_ol_ul) =
      nav_menus_with_hdr_title is_list_elem is_nav_list cnt_h1s doc_model
    in
    let offenders = total_menus - count in
    let percent = Testutil.pct_of_ints offenders total_menus in
    let results = [
      ("cnt1", offenders);
      ("tot1", total_menus);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 041s: sitewide aggregation of 041p results *)
let test041s site pg_results =
  let test_id = "nav041s" in
    let (sum_c, sum_t, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav041p" pg_results
    in
    let site_pct = Testutil.pct_of_ints sum_c sum_t in
    let results = [
      ("cnt1", sum_c);
      ("tot1", sum_t);
      ("pct1", site_pct);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 042p: Return the number of map navigation menus (map elements
    that contain at least one area element) that are not immediately
    preceded by a heading element.
*)
let test042p site page =
  let test_id = "nav042p" in
    let doc_model = Html.doc_model (Page.document page) in
    let h1s = Testutil.get_elements_with_name "H1" page in
    let cnt_h1s = List.length h1s in
    let (count, total_menus, total_maps) =
      nav_menus_with_hdr_title is_map_elem is_nav_map cnt_h1s doc_model
    in
    let offenders = total_menus - count in
    let percent = Testutil.pct_of_ints offenders total_menus in
    let results = [
      ("cnt1", offenders);
      ("tot1", total_menus);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 042s: sitewide aggregation of 042p results *)
let test042s site pg_results =
  let test_id = "nav042s" in
    let (sum_c, sum_t, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav042p" pg_results
    in
    let site_pct = Testutil.pct_of_ints sum_c sum_t in
    let results = [
      ("cnt1", sum_c);
      ("tot1", sum_t);
      ("pct1", site_pct);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 043p: Check that all area tags have redundant text links.
    @return cnt1: number of area tags that do not have redundant text links
    @return tot1: total number of area tags
    @return pct1: percent of area tags that do not meet criteria
*)
let test043p site page =
  let test_id = "nav043p" in
    let tag_tbl = (Html.tag_tbl (Page.document page)) in
    let area_tags = try Hashtbl.find tag_tbl "AREA" with _ -> [] in
    let a_tags = try Hashtbl.find tag_tbl "A" with _ -> [] in
    let check_a_list s a b =
      if (Html.has_attribute_with_value b "href" s)
      then true
      else a
    in
    let f a b =
      try
        let href = Html.get_attribute b "href" in
          if (List.fold_left (check_a_list (Html.attr_value href)) false a_tags)
          then (a + 1)
          else (a)
      with _ -> a
    in
    let area_tag_red_count = List.fold_left f 0 area_tags in
    let total_area_tags = List.length area_tags in
    let offenders = total_area_tags - area_tag_red_count in
    let percent = Testutil.pct_of_ints offenders total_area_tags in
    let results = [
      ("cnt1", offenders);
      ("tot1", total_area_tags);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 043s: sitewide aggregation of 043p results *)
let test043s site pg_results =
  let test_id = "nav043s" in
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav043p" pg_results
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
(* FORM CONTROLS *)
(* ---------------------------------------------------------------- *)

(** 050p: Find the number of empty label and legend elements.
    @return cnt1: number of empty label elements
    @return tot1: total number of label elements
    @return cnt2: number of empty legend elements
    @return tot2: total number of legend elements
*)
let test050p site page =
  let test_id = "nav050p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    (* label elements *)
    let labels = try Hashtbl.find tag_tbl "LABEL" with _ -> [] in
    let labels_with_content = List.filter Testutil.has_content labels in
    let total_labels = List.length labels in
    (* legend elements *)
    let legends = try Hashtbl.find tag_tbl "LEGEND" with _ -> [] in
    let legends_with_content = List.filter Testutil.has_content legends in
    let total_legends = List.length legends in
    let results = [
      ("cnt1", total_labels - List.length labels_with_content);
      ("tot1", total_labels);
      ("cnt2", total_legends - List.length legends_with_content);
      ("tot2", total_legends);
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 050s: sitewide aggregation of 050p results *)
let test050s site pg_results =
  let test_id = "nav050s" in
    let (count_labels, total_labels, count_legends, total_legends, pg_count) =
      Wamtml.sum_4_results "cnt1" "tot1" "cnt2" "tot2" "nav050p" pg_results
    in
    let results = [
      ("cnt1", count_labels);
      ("tot1", total_labels);
      ("cnt2", count_legends);
      ("tot2", total_legends);
      ("tot3", pg_count);
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 051p: Test whether textarea, select, and input elements with type=
    text|password|checkbox|radio|file are referenced by label elements
    or have title attributes.
    @return cnt1: number of elements that do not meet criteria
    @return tot1: total number of form control elements
    @return pct1: percentage of elements that do not meet criteria
*)
let test051p site page =
  let test_id = "nav051p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let textareas = try Hashtbl.find tag_tbl "TEXTAREA" with _ -> [] in
    let selects = try Hashtbl.find tag_tbl "SELECT" with _ -> [] in
    let inputs =
      let all_inputs =
        try Hashtbl.find tag_tbl "INPUT" with _ -> [] in
      let f a b =
        if (Html.has_attribute_with_value b "type" "text") ||
          (Html.has_attribute_with_value b "type" "password") ||
          (Html.has_attribute_with_value b "type" "checkbox") ||
          (Html.has_attribute_with_value b "type" "radio") ||
          (Html.has_attribute_with_value b "type" "file")
        then a@[b]
        else a
      in
        List.fold_left f [] all_inputs
    in
    let controls = textareas@selects@inputs in
    let labels = try Hashtbl.find tag_tbl "LABEL" with _ -> [] in
    let check_label s l =
      Html.has_attribute_with_value l "for" s in
    let g a b =
      let (id_count, title_count) = a in
        (* Given a form control tag, we need to check for one of two things...
           it must either have (a) an 'id' attribute whose value matches the
           value of the 'for' attribute of one of the labels in the document... *)
      let id_match =
        try
          let id = Html.get_attribute b "id" in
            List.exists (check_label (Html.attr_value id)) labels
        with _ -> false
      in
        (* ...or (b) a non-empty 'title' attribute *)
        if (not id_match)
        then (
          if (Html.has_non_blank_attribute b "title")
          then (id_count, title_count + 1)
          else a
        )
        else (id_count + 1, title_count)
    in
    let (id_total, title_total) = List.fold_left g (0,0) controls in
    let total_controls = List.length controls in
    let offenders = total_controls - (id_total + title_total) in
    let percent = Testutil.pct_of_ints offenders total_controls in
    let results = [
      ("cnt1", offenders);
      ("tot1", total_controls);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 051s: sitewide aggregation of 051p results *)
let test051s site pg_results =
  let test_id = "nav051s" in
    let (offenders, control_count, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav051p" pg_results
    in
    let percent = Testutil.pct_of_ints offenders control_count in
    let results = [
      ("cnt1", offenders);
      ("tot1", control_count);
      ("pct1", percent);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 052p: Test whether input elements with type=button|reset|submit
    have a value or title attribute.
    @return cnt1: number of elements that do not meet criteria
    @return tot1: total number of form control elements
    @return pct1: percentage of elements that do not meet criteria
*)
let test052p site page =
  let test_id = "nav052p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let inputs =
      let all_inputs =
        try Hashtbl.find tag_tbl "INPUT" with _ -> [] in
      let f a b =
        if (Html.has_attribute_with_value b "type" "button") ||
          (Html.has_attribute_with_value b "type" "reset") ||
          (Html.has_attribute_with_value b "type" "submit")
        then a@[b]
        else a
      in
        List.fold_left f [] all_inputs
    in
    let g a b =
      let (value_count, title_count) = a in
        (* Given an input tag with one of the specified types, we need to
           check for one of two things... it must either have (a) a non-empty
           'value' attribute... *)
      let has_value_attrib = (Html.has_non_blank_attribute b "value") in
        (* ...or (b) a non-empty 'title' attribute *)
        if (not has_value_attrib)
        then (
          if (Html.has_non_blank_attribute b "title")
          then (value_count, title_count + 1)
          else a
        )
        else (value_count + 1, title_count)
    in
    let (value_total, title_total) = List.fold_left g (0,0) inputs in
    let total_inputs = List.length inputs in
    let offenders = total_inputs - (value_total + title_total) in
    let percent = Testutil.pct_of_ints offenders total_inputs in
    let results = [
      ("cnt1", offenders);
      ("tot1", total_inputs);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 052s: sitewide aggregation of 052p results *)
let test052s site pg_results =
  let test_id = "nav052s" in
    let (offenders, input_count, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav052p" pg_results
    in
    let percent = Testutil.pct_of_ints offenders input_count in
    let results = [
      ("cnt1", offenders);
      ("tot1", input_count);
      ("pct1", percent);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 053p: Test whether input elements with type=image have alt or title
    attribute
    @return cnt1: number of elements that do not meet criteria
    @return tot1: total number of form control elements
    @return pct1: percentage of elements that do not meet criteria
*)
let test053p site page =
  let test_id = "nav053p" in
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let inputs =
      let all_inputs =
        try Hashtbl.find tag_tbl "INPUT" with _ -> [] in
      let f a b =
        if (Html.has_attribute_with_value b "type" "image")
        then a@[b]
        else a
      in
        List.fold_left f [] all_inputs
    in
    let g a b =
      let (alt_count, title_count) = a in
        (* Given an input type=image tag, we need to check for one of two things...
           it must either have (a) a non-empty 'alt' attribute... *)
      let has_alt_attrib = (Html.has_non_blank_attribute b "alt") in
        (* ...or (b) a non-empty 'title' attribute *)
        if (not has_alt_attrib)
        then (
          if (Html.has_non_blank_attribute b "title")
          then (alt_count, title_count + 1)
          else a
        )
        else (alt_count + 1, title_count)
    in
    let (alt_total, title_total) = List.fold_left g (0,0) inputs in
    let total_inputs = List.length inputs in
    let offenders = total_inputs - (alt_total + title_total) in
    let percent = Testutil.pct_of_ints offenders total_inputs in
    let results = [
      ("cnt1", offenders);
      ("tot1", total_inputs);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 053s: sitewide aggregation of 053p results *)
let test053s site pg_results =
  let test_id = "nav053s" in
    let (offenders, input_count, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav053p" pg_results
    in
    let percent = Testutil.pct_of_ints offenders input_count in
    let results = [
      ("cnt1", offenders);
      ("tot1", input_count);
      ("pct1", percent);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

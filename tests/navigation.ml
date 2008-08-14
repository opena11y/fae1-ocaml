(**
   Tests relating to navigation and orientation
*)

open Html
open Htmlutil
open Page
open Site
open Testutil
open Wamtml

let debug = false;;

exception TagNotFound;;

(* HELPER FUNCTIONS *)

let get_heading_elements page =
  let tbl = Html.tag_tbl (Page.document page) in
  let h1 = try Hashtbl.find tbl "H1" with _ -> [] in
  let h2 = try Hashtbl.find tbl "H2" with _ -> [] in
  let h3 = try Hashtbl.find tbl "H3" with _ -> [] in
  let h4 = try Hashtbl.find tbl "H4" with _ -> [] in
  let h5 = try Hashtbl.find tbl "H5" with _ -> [] in
  let h6 = try Hashtbl.find tbl "H6" with _ -> [] in
    (h1@h2@h3@h4@h5@h6);;

let get_subheading_elements page =
  let tbl = Html.tag_tbl (Page.document page) in
  let h2 = try Hashtbl.find tbl "H2" with _ -> [] in
  let h3 = try Hashtbl.find tbl "H3" with _ -> [] in
  let h4 = try Hashtbl.find tbl "H4" with _ -> [] in
  let h5 = try Hashtbl.find tbl "H5" with _ -> [] in
  let h6 = try Hashtbl.find tbl "H6" with _ -> [] in
    (h2@h3@h4@h5@h6);;

(**
   Determine whether tag is a heading element.
*)
let is_heading_elem tag =
  let name = Html.tag_name tag in
    (name = "H1" || name = "H2" || name = "H3" ||
        name = "H4" || name = "H5" || name = "H6");;

(**
   Determine whether tag is an ordered or unordered list element.
*)
let is_list_elem tag =
  let name = Html.tag_name tag in
    name = "OL" || name = "UL";;

(**
  The functions is_item_link and is_nav_menu are mutually
  recursive, i.e., each calls the other.

  is_item_link tests whether an element is an 'li', and if
  so, whether its content weight equals that of its first
  link descendant, or, if not, whether it contains a nested
  list that is a nav_menu, and the containing 'li' content
  weight equals that of its first link descendant plus that
  of the nested nav_menu.

  is_nav_menu tests whether all or all but one of its 'li'
  child elements meet the is_item_link requirements.
*)
let rec is_item_link tag =
  if debug then print_endline ">>> is_item_link";
  if Html.tag_name tag = "LI"
  then
    let links = Testutil.get_descendants tag ["A"] in
      if (List.length links) > 0
      then (
        if debug then print_endline (Html.get_node_content "" [Html.Tag (List.hd links)]);
        let item_weight = Testutil.get_trimmed_content_weight tag in
        let link_weight = Testutil.get_trimmed_content_weight (List.hd links) in
          if item_weight = link_weight
          then true
          else
            let lists = Testutil.get_descendants tag ["OL"; "UL"] in
              if (List.length lists) > 0
              then (
                let nested_list = List.hd lists in
                let list_weight = Testutil.get_trimmed_content_weight nested_list in
                  (is_nav_menu nested_list) && (item_weight = link_weight + list_weight)
              )
              else false
      )
      else false
  else false
and is_nav_menu tag =
  if debug then print_endline ">>> is_nav_menu";
  let list_items = Testutil.get_child_elements tag in
  let rec count num lst =
    match lst with
        hd :: tl ->
          if is_item_link hd
          then count (num + 1) tl
          else count num tl
      | [] -> num
  in
  let items_with_links = count 0 list_items in
  let item_count = List.length list_items in
    if (items_with_links > 0) && (item_count - items_with_links) <= 1
    then (
      if debug then print_endline ">>> PASSED!!!";
      true
    )
    else (
      if debug then print_endline ">>> FAILED!!!";
      false
    );;

(**
   Count navigation menus that precede the last h1 element on the page and that
   are immediately preceded by a heading element.
   @param status   1 if heading element just seen at head of list, 0 otherwise
   @param cnt_m    number of navigation menus that are preceded by heading element
   @param tot_m    number of ol/ul elements that appear to be navigation menus
   @param tot_l    total number of ol/ul elements found
   @param cnt_h1s  number of h1 elements not yet seen
   @param lst      doc_model list
*)
let rec nav_menus_with_hdr_title status cnt_m tot_m tot_l cnt_h1s lst =
  if cnt_h1s = 0
  then (cnt_m, tot_m, tot_l)
  else (
    match lst with
        (Html.Tag t) :: tl ->
          if (Html.tag_name t) = "H1"
          then
            (* heading element just seen: set status flag *)
            nav_menus_with_hdr_title 1 cnt_m tot_m tot_l (cnt_h1s - 1) tl
          else (
            if is_list_elem t
            then (
              if is_nav_menu t
              then
                (* increment counters appropriately *)
                nav_menus_with_hdr_title 0 (cnt_m + status) (tot_m + 1) (tot_l + 1) cnt_h1s tl
              else
                (* increment total list elements only *)
                nav_menus_with_hdr_title 0 cnt_m tot_m (tot_l + 1) cnt_h1s ((Html.tag_children t) @ tl)
            )
            else (
              if is_heading_elem t
              then
                (* set status flag *)
                nav_menus_with_hdr_title 1 cnt_m tot_m tot_l cnt_h1s tl
              else
                (* unset status flag *)
                nav_menus_with_hdr_title 0 cnt_m tot_m tot_l cnt_h1s ((Html.tag_children t) @ tl)
            )
          )
      | hd :: tl ->
          (* we're only interested in objects of type Html.Tag *)
          nav_menus_with_hdr_title status cnt_m tot_m tot_l cnt_h1s tl
      | [] -> (cnt_m, tot_m, tot_l)
  );;

(* ANALYSIS FUNCTIONS *)

(* ---------------------------------------------------------------- *)
(** 003p: This test determines which tables on a page are data tables.
    It also stores some additional information about those tables so
    we don't have to process things multiple times. This test MUST be
    run before other tests with dependences on that additional info. *)
let test003p site page =
  let test_id = "nav003p" in
    Testutil.msg test_id;
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
(** 004p: How many of the frames have titles? *)
let test004p site page =
  let test_id = "nav004p" in
    Testutil.msg test_id;
    let tag_tbl = (Html.tag_tbl (Page.document page)) in
    let frames = try Hashtbl.find tag_tbl "FRAME" with _ -> [] in
    let check_frames a b =
      if (Html.has_attribute b "title")
      then a + 1
      else a
    in
    let count = List.fold_left check_frames 0 frames in
    let total = List.length frames in
    let percent = Testutil.pct_of_ints count total in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("pct1", percent)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 004s: sitewide aggregation of 004p results *)
let test004s site pg_results =
  let test_id = "nav004s" in
    Testutil.msg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav004p" pg_results
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
(* TITLE ELEMENT *)
(* ---------------------------------------------------------------- *)

(** 010p: Find the number of empty title elements.
    @return cnt1: number of empty title elements
    @return tot1: total number of title elements
*)
let test010p site page =
  let test_id = "nav010p" in
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
    let (count, total, pg_count) = Wamtml.sum_results "cnt1" "tot1" "nav024p" pg_results in
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;

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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
(* NAVIGATION BARS *)
(* ---------------------------------------------------------------- *)

(** 041p: Return the number of navigation menus (ul and ol elements for
    which all or all but one of their li children contain only a link
    element OR a link element plus a nested navigation menu) that are
    not immediately preceded by a heading element. *)
let test041p site page =
  let test_id = "nav041p" in
    Testutil.msg test_id;
    let doc_model = Html.doc_model (Page.document page) in
    let h1s = Testutil.get_elements_with_name "H1" page in
    let cnt_h1s = List.length h1s in
    let (count, total_menus, total_ol_ul) =
      nav_menus_with_hdr_title 0 0 0 0 cnt_h1s doc_model
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
    Testutil.msg test_id;
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
(** 044p: Check that all area tags have redundant text links.
    @return cnt1: number of area tags that do not have redundant text links
    @return tot1: total number of area tags
    @return pct1: percent of area tags that do not meet criteria
*)
let test044p site page =
  let test_id = "nav044p" in
    Testutil.msg test_id;
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
(** 044s: sitewide aggregation of 044p results *)
let test044s site pg_results =
  let test_id = "nav044s" in
    Testutil.msg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "nav044p" pg_results
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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
    Testutil.msg test_id;
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

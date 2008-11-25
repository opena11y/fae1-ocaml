(**
   Tests relating to text equivalents
*)

let debug = false
let msg = (Testutil.msg debug)
let runmsg = (Testutil.msg debug "Running")

(* ---------------------------------------------------------------- *)
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
(** 001p: number of img elements without an alt attribute *)
let test001p site page =
  let test_id = "text001p" in
    runmsg test_id;
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
    let results = [
      ("cnt1", total - count);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 001s: sitewide aggregation of 001p results *)
let test001s site pg_results =
  let test_id = "text001s" in
    runmsg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text001p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 002p: number of img elements with empty alt attribute *)
let test002p site page =
  let test_id = "text002p" in
    runmsg test_id;
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
    let results = [
      ("cnt1", total - count);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 002s: sitewide aggregation of 002p results *)
let test002s site pg_results =
  let test_id = "text002s" in
    runmsg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text002p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003p: How many images appear to be for decorative purposes only *)
let test003p site page =
  let test_id = "text003p" in
    runmsg test_id;
    let tag_tbl = Html.tag_tbl (Page.document page) in
    let page_images =
      try Hashtbl.find tag_tbl "IMG"
      with _ -> []
    in
    let site_images = Site.images site in
    let min_pixels = 8 in
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
                if ( h < min_pixels || w < min_pixels)
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
    let missing = total - found in
    let results = [
      ("cnt1", count);
      ("cnt2", missing);
      ("tot1", found);
      ("tot2", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 003s: Sitewide average of 003p *)
let test003s site pg_results =
  let test_id = "text003s" in
    runmsg test_id;
    let (sum_count, sum_found, sum_total, pg_count) =
      Wamtml.sum_3_results "cnt1" "tot1" "tot2" "text003p" pg_results
    in
    let sum_missing = sum_total - sum_found in
    let results = [
      ("cnt1", sum_count);
      ("cnt2", sum_missing);
      ("tot1", sum_found);
      ("tot2", sum_total);
      ("tot3", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 004p: number of area elements without an alt attribute *)
let test004p site page =
  let test_id = "text004p" in
    runmsg test_id;
    let tbl = Html.tag_tbl (Page.document page) in
    let area_elements =
      try
        Hashtbl.find tbl "AREA"
      with _ -> []
    in
    let pred t = Html.has_attribute t "alt" in
    let area_with_alt = List.filter pred area_elements in
    let count = List.length area_with_alt in
    let total = List.length area_elements in
    let results = [
      ("cnt1", total - count);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 004s: sitewide aggregation of 004p results *)
let test004s site pg_results =
  let test_id = "text004s" in
    runmsg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text004p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 010p: number of embed elements without alt text *)
let test010p site page =
  let test_id = "text010p" in
    runmsg test_id;
    let (count, total) = element_with_alt_text "EMBED" page in
    let results = [
      ("cnt1", total - count);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 010s: sitewide aggregation of 010p results *)
let test010s site pg_results =
  let test_id = "text010s" in
    runmsg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text010p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 011p: number of applet elements without alt text *)
let test011p site page =
  let test_id = "text011p" in
    runmsg test_id;
    let (count, total) = element_with_alt_text "APPLET" page in
    let results = [
      ("cnt1", total - count);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 011s: sitewide aggregation of 011p results *)
let test011s site pg_results =
  let test_id = "text011s" in
    runmsg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text011p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 012p: number of object elements without alternative content *)
let test012p site page =
  let test_id = "text012p" in
    runmsg test_id;
    let (ratio, objects_with_content, object_count) =
      element_with_children "OBJECT" page
    in
    let results = [
      ("cnt1", object_count - objects_with_content);
      ("tot1", object_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(** 012s: sitewide aggregation of 012p results *)
let test012s site pg_results =
  let test_id = "text012s" in
    runmsg test_id;
    let (count, total, pg_count) =
      Wamtml.sum_results "cnt1" "tot1" "text012p" pg_results
    in
    let results = [
      ("cnt1", count);
      ("tot1", total);
      ("tot2", pg_count)
    ] in
      Wamtml.create_wamt_test test_id results;;

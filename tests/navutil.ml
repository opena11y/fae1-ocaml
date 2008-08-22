(**
   Utility functions for navigation and orientation tests
*)

let debug = false;;

(**
   Return a list containing all subheading elements.
*)
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

(* MAP ELEMENTS AS NAVIGATION BARS *)

(**
   Determine whether tag is a map element.
*)
let is_map_elem tag =
  let name = Html.tag_name tag in
    name = "MAP";;

(**
   Criterion for a map element to be considered a navigation
   menu: It has to contain at least one area element.
*)
let is_nav_map tag =
  let areas = Testutil.get_descendants tag ["AREA"] in
    (List.length areas) > 0;;

(* LIST ELEMENTS AS NAVIGATION BARS *)

(**
   Determine whether tag is an ordered or unordered list element.
*)
let is_list_elem tag =
  let name = Html.tag_name tag in
    name = "OL" || name = "UL";;

(**
  The functions is_item_link and is_nav_list are mutually
  recursive, i.e., each calls the other.

  is_item_link tests whether an element is an 'li', and if
  so, whether its content weight equals that of its first
  link descendant, or, if not, whether it contains a nested
  list that is a nav_menu, and the containing 'li' content
  weight equals that of its first link descendant plus that
  of the nested nav_menu.

  is_nav_list tests whether all or all but one of its 'li'
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
                  (is_nav_list nested_list) && (item_weight = link_weight + list_weight)
              )
              else false
      )
      else (
        if debug then print_endline ">>> no links found";
        false
      )
  else (
    if debug then print_endline ">>> not an LI element";
    false
  )
and is_nav_list tag =
  if debug then print_endline ">>> is_nav_list";
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
    if debug then print_endline ("item_count: " ^ (string_of_int item_count) ^ " items_with_links: " ^ (string_of_int items_with_links));
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
   Count navigation menus that precede the last h1 element on the page
   and that are immediately preceded by a heading element.
   @param pred1      predicate for general case: element type being examined
   @param pred2      predicate for specific case: meets necessary criteria
   @param h1s        number of h1 elements not yet seen
   @param doc_model  doc_model list
*)
let nav_menus_with_hdr_title pred1 pred2 h1s doc_model =
  (**
     @param status   1 if heading element just seen at head of list, 0 otherwise
     @param cnt_m    number of navigation menus that are preceded by heading element
     @param tot_m    total number of elements that qualify as navigation menus
     @param tot_l    total number of elements examined
     @param cnt_h1s  number of h1 elements not yet seen
     @param lst      doc_model list
  *)
  let rec f status cnt_m tot_m tot_l cnt_h1s lst =
    if cnt_h1s = 0
    then (cnt_m, tot_m, tot_l)
    else (
      match lst with
          (Html.Tag t) :: tl ->
            if debug then print_endline ("nmwht: " ^ (Html.tag_name t));
            if (Html.tag_name t) = "H1"
            then
              (* heading element just seen: set status flag *)
              f 1 cnt_m tot_m tot_l (cnt_h1s - 1) ((Html.tag_children t) @ tl)
            else (
              if pred1 t (* general case: element type to be examined *)
              then (
                if pred2 t (* specific case: meets all criteria *)
                then (
                  if debug then print_endline ("+++ true: " ^ (string_of_int (cnt_m + status)));
                  if debug then print_endline ("+++ total: " ^ (string_of_int (tot_m + 1)));
                  (* increment counters appropriately *)
                  f 0 (cnt_m + status) (tot_m + 1) (tot_l + 1) cnt_h1s tl
                )
                else (
                  if debug then print_endline ("+++ false: " ^ (string_of_int cnt_m));
                  if debug then print_endline ("+++ total: " ^ (string_of_int tot_m));
                  (* increment total list elements only *)
                  f 0 cnt_m tot_m (tot_l + 1) cnt_h1s tl
                )
              )
              else (
                if is_heading_elem t
                then
                  (* set status flag *)
                  f 1 cnt_m tot_m tot_l cnt_h1s ((Html.tag_children t) @ tl)
                else
                  (* unset status flag *)
                  f 0 cnt_m tot_m tot_l cnt_h1s ((Html.tag_children t) @ tl)
              )
            )
        | hd :: tl ->
            (* we're only interested in objects of type Html.Tag *)
            f status cnt_m tot_m tot_l cnt_h1s tl
        | [] -> (cnt_m, tot_m, tot_l)
    )
  in
    f 0 0 0 0 h1s doc_model;;

(**
   Utility functions for navigation and orientation tests
*)

let debug = false

let msg label text =
  if debug then print_endline (">>> " ^ label ^ ": " ^  text)

let rec linefeed n =
  if debug then (
    if n > 0 then (
      print_newline ();
      linefeed (n - 1)
    )
  )

(* ---------------------------------------------------------------- *)
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
  msg "is_heading_elem" (Html.tag_name tag);
  match Html.tag_name tag with
    | "H1" -> true
    | "H2" -> true
    | "H3" -> true
    | "H4" -> true
    | "H5" -> true
    | "H6" -> true
    | _ -> false;;

(**
   Given an htmlItem, determine whether (a) it is a heading element, or
   (b) it has a descendant that is a heading element and all of its text
   content is contained by that element.
*)
let is_or_contains_only_heading_elem (item : Html.htmlItem) =
  match item with
      Html.Tag tag ->
        is_heading_elem tag
        || Testutil.all_text_content_in_named_descendant tag ["H1";"H2";"H3";"H4";"H5";"H6"]
    | _ -> false;;

(**
   Minimum number of links for map or list to be considered a navigation bar.
*)
let min_links = 1;;

(* ---------------------------------------------------------------- *)
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
  let areas = Testutil.get_named_descendants tag ["AREA"] in
  let is_menu = (List.length areas) >= min_links in
  let has_hdr = is_or_contains_only_heading_elem (Testutil.get_preceding_sibling tag) in
    (is_menu, has_hdr);;

(* ---------------------------------------------------------------- *)
(* LIST ELEMENTS AS NAVIGATION BARS *)

(**
   Determine whether tag is an ordered or unordered list element.
*)
let is_list_elem tag =
  let name = Html.tag_name tag in
    name = "OL" || name = "UL";;

(**
   Determine whether tag is a link element, or it has a descendant
   that is a link element and all of its text content is contained
   by that element.
*)
let is_or_contains_only_link_elem (item : Html.htmlItem) =
  match item with
      Html.Tag tag ->
        Html.tag_name tag = "A"
        || Testutil.all_text_content_in_named_descendant tag ["A"]
    | _ -> false;;

(**
   The functions is_item_link and is_nav_list are mutually
   recursive, i.e., each calls the other.

   is_item_link tests whether an element is an "li", and if so, whether
   its text content weight equals that of its first link descendant, or,
   if not, whether it contains the following sequence of elements:
   (1) a link (which may be encapsulated in another element) (2) an
   optional element (expected to be a heading element, which also may be
   encapsulated in another element) (3) a list element that is a nav menu,
   determined by calling is_nav_list on the list and its preceding element.

   The implications of these requirements are: a nested list pattern
   must always begin with a link, which must be followed by a nav menu
   which, in the case of a nested list, may be preceded only by the
   link, or may have an interposed heading element that follows the link
   and precedes the list. On the other hand, a non-nested (top-level)
   list must be preceded by a heading element.

   is_nav_list tests two separate conditions and returns a pair (tuple)
   of boolean values:
   1. Do all or all but one of its "li" child elements meet the
   is_item_link requirements (is_menu)?
   2. Does the element immediately preceding the list menu satisfy the
   requirements for heading/title (has_hdr)?
*)
let rec is_item_link tag =
  if not (Html.tag_name tag = "LI")
  then false
  else (
    (* First, see if the list item contains a single link. *)
    if Testutil.all_text_content_in_named_descendant tag ["A"]
    then true (* We're done! *)
    else (
      (* Examine child elements and look for nav list preceded by heading/link *)
      let children = Testutil.get_child_elements tag in
        if (List.length children) > 0
        then (
          let first_child = List.hd children in
            (* The first element must be a link. *)
            if not (is_or_contains_only_link_elem (Html.Tag first_child))
            then false
            else (
              (* First element is effectively a link. *)
              let remainder = List.tl children in
                (* Only two possibilities re. what follows: either a list,
                   or a heading element followed by a list. *)
                if (List.length remainder) > 0
                then (
                  let head_rem = List.hd remainder in
                    if is_list_elem head_rem && List.length remainder = 1
                    then (
                      let (is_menu, has_hdr) = is_nav_list head_rem in
                        is_menu && has_hdr
                    )
                    else (
                      let tail_rem = List.tl remainder in
                        if (List.length tail_rem) > 0
                        then (
                          let head_tail_rem = List.hd tail_rem in
                            if is_list_elem head_tail_rem && List.length tail_rem = 1
                            then (
                              let (is_menu, has_hdr) = is_nav_list head_tail_rem in
                                is_menu && has_hdr
                            )
                            else false
                        )
                        else false
                    )
                )
                else false
            )
        )
        else false
    )
  )
and is_nav_list tag =
  linefeed 3;
  msg "is_nav_list" ("entry tag_name: " ^ (Html.tag_name tag));
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
    msg "item_count" (string_of_int item_count);
    msg "items_with_links" (string_of_int items_with_links);
    if (items_with_links >= min_links) && (item_count - items_with_links) <= 1
    then (
      let preceding_sibling = Testutil.get_preceding_sibling tag in
        msg "preceding_sibling" (Testutil.item_to_string preceding_sibling);
        if is_or_contains_only_heading_elem preceding_sibling
        then (
          msg "is_nav_list" "T T";
          (true, true)
        )
        else (
          let parent = Html.tag_parent tag in
            match parent with
                Html.Tag t -> (
                  if (Html.tag_name t) = "LI" (* this is a nested list *)
                  then (
                    if is_or_contains_only_link_elem preceding_sibling
                    then (
                      msg "is_nav_list" "T T";
                      (true, true)
                    )
                    else (true, false)
                  )
                  else (true, false)
                )
              | _ -> (true, false)
        )
    )
    else (
      msg "is_nav_list" "F F";
      (false, false)
    );;

(* ---------------------------------------------------------------- *)
(* GENERIC FUNCTION FOR DETECTING MENUS *)
(**
   Count navigation menus that precede the last h1 element on the page
   and that are immediately preceded by a heading element.
   @param pred_elem  is this the element of interest?
   @param pred_menu  is the element a navigation menu with title?
   @param num_h1s    number of h1 elements not yet seen
   @param doc_model  doc_model list
*)
let nav_menus_with_hdr_title pred_elem pred_menu num_h1s doc_model =
  (**
     @param cnt_p    number of navigation menus preceded by heading element
     @param tot_m    total number of elements that qualify as navigation menus
     @param tot_l    total number of elements examined
     @param cnt_h1s  number of h1 elements not yet seen
     @param lst      doc_model list
  *)
  let rec f cnt_p tot_m tot_l cnt_h1s lst =
    if cnt_h1s = 0
    then (cnt_p, tot_m, tot_l)
    else (
      match lst with
          Html.Tag t :: tl ->
            if (Html.tag_name t) = "H1"
            then f cnt_p tot_m tot_l (cnt_h1s - 1) tl
            else (
              if pred_elem t
              then (
                let (is_menu, has_hdr) = pred_menu t in
                if is_menu
                then (
                  msg "is_menu T; tot_m" (string_of_int (tot_m + 1));
                  if has_hdr
                  then (
                    msg "has_hdr T; cnt_p" (string_of_int (cnt_p + 1));
                    f (cnt_p + 1) (tot_m + 1) (tot_l + 1) cnt_h1s tl
                  )
                  else (
                    msg "has_hdr F; cnt_p" (string_of_int cnt_p);
                    f cnt_p (tot_m + 1) (tot_l + 1) cnt_h1s tl
                  )
                )
                else (
                  msg "is_menu F; tot_m" (string_of_int tot_m);
                  f cnt_p tot_m (tot_l + 1) cnt_h1s tl
                )
              )
              else (
                f cnt_p tot_m tot_l cnt_h1s ((Html.tag_children t) @ tl)
              )
            )
        | hd :: tl ->
            (* we're only interested in objects of type Html.Tag *)
            f cnt_p tot_m tot_l cnt_h1s tl
        | [] -> (cnt_p, tot_m, tot_l)
    )
  in
    f 0 0 0 num_h1s doc_model;;

(* ---------------------------------------------------------------- *)

(**
   List of two-character language subtags defined in IANA registry
   dated 2008-08-18 with 190 two-character entries.
   See http://www.iana.org/assignments/language-subtag-registry.
*)
let language_codes = [
  "aa"; "ab"; "ae"; "af"; "ak"; "am"; "an"; "ar"; "as"; "av"; "ay"; "az";
  "ba"; "be"; "bg"; "bh"; "bi"; "bm"; "bn"; "bo"; "br"; "bs"; "ca"; "ce";
  "ch"; "co"; "cr"; "cs"; "cu"; "cv"; "cy"; "da"; "de"; "dv"; "dz"; "ee";
  "el"; "en"; "eo"; "es"; "et"; "eu"; "fa"; "ff"; "fi"; "fj"; "fo"; "fr";
  "fy"; "ga"; "gd"; "gl"; "gn"; "gu"; "gv"; "ha"; "he"; "hi"; "ho"; "hr";
  "ht"; "hu"; "hy"; "hz"; "ia"; "id"; "ie"; "ig"; "ii"; "ik"; "in"; "io";
  "is"; "it"; "iu"; "iw"; "ja"; "ji"; "jv"; "jw"; "ka"; "kg"; "ki"; "kj";
  "kk"; "kl"; "km"; "kn"; "ko"; "kr"; "ks"; "ku"; "kv"; "kw"; "ky"; "la";
  "lb"; "lg"; "li"; "ln"; "lo"; "lt"; "lu"; "lv"; "mg"; "mh"; "mi"; "mk";
  "ml"; "mn"; "mo"; "mr"; "ms"; "mt"; "my"; "na"; "nb"; "nd"; "ne"; "ng";
  "nl"; "nn"; "no"; "nr"; "nv"; "ny"; "oc"; "oj"; "om"; "or"; "os"; "pa";
  "pi"; "pl"; "ps"; "pt"; "qu"; "rm"; "rn"; "ro"; "ru"; "rw"; "sa"; "sc";
  "sd"; "se"; "sg"; "sh"; "si"; "sk"; "sl"; "sm"; "sn"; "so"; "sq"; "sr";
  "ss"; "st"; "su"; "sv"; "sw"; "ta"; "te"; "tg"; "th"; "ti"; "tk"; "tl";
  "tn"; "to"; "tr"; "ts"; "tt"; "tw"; "ty"; "ug"; "uk"; "ur"; "uz"; "ve";
  "vi"; "vo"; "wa"; "wo"; "xh"; "yi"; "yo"; "za"; "zh"; "zu"];;

(**
   Given an attribute value, if it contains a hyphen, which could act as
   a separator between language and region subtags, extract the language
   subtag prefix; otherwise return the original value.
*)
let get_language_subtag str =
  if String.contains str '-'
  then List.hd (Str.split (Str.regexp "-") str)
  else str;;

(**
   Test string for membership in the two-character language_codes list.
*)
let is_valid_language_code str =
  let subtag = get_language_subtag str in
    if String.length subtag = 2
    then List.mem (String.lowercase subtag) language_codes
    else false;;

(**
   Given a page, return a tuple pair with the following values:
   (1) boolean indicating whether the "lang" attribute is present
   on the "HTML" element, and (2) boolean indicating whether the
   value of that attribute is a valid two-character language code.
*)
let has_default_language page =
  let tbl = Html.tag_tbl (Page.document page) in
  let html_tags = Hashtbl.find tbl "HTML" in
    if List.length html_tags > 0
    then (
      let html_tag = List.hd html_tags in
      let (has_attribute, value) = Testutil.has_attribute_get_value html_tag "lang" in
        if has_attribute
        then (
          if is_valid_language_code value
          then (true, true)
          else (true, false)
        )
        else (false, false)
    )
    else (false, false);;

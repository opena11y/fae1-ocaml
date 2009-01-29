(**
   Utility functions for general use in page markup tests
*)

exception TagNotFound

(**
   Given a predicate to be applied to each Html.Tag in an htmlItem
   list, return the list of tags that satisfy the predicate.

   NOTE: This function is best used when operating on the entire
   doc_model list. If you want to operate on a particular subset of
   tags (for example all p tags), it's more efficient to get those
   from the Html.tag_tbl structure and then use List.filter.
*)
let rec get_elements pred lst =
  match lst with
    | Html.Tag t :: tl -> (
        if pred t
        then t :: get_elements pred ((Html.tag_children t) @ tl)
        else get_elements pred ((Html.tag_children t) @ tl)
      )
    | hd :: tl -> get_elements pred tl
    | [] -> [];;

(* ------------------------------------------------ *)
(* FUNCTIONS WITH PAGE ARG *)
(* ------------------------------------------------ *)

(**
   Given a list of element names and a page, return the list
   of elements with the specified names, in document order.
*)
let get_elements_with_names names page =
  let doc_model = Html.doc_model (Page.document page) in
  let pred t = List.mem (Html.tag_name t) names in
    get_elements pred doc_model;;

(**
   Given an attribute name and a page, return the list of
   elements with the specified attribute, in document order.
*)
let get_elements_with_attribute name page =
  let doc_model = Html.doc_model (Page.document page) in
  let pred t = Html.has_attribute t name in
    get_elements pred doc_model;;

(**
   Given an attribute name/value pair and a page, return
   the list of elements with the specified attribute and
   value, in document order.
*)
let get_elements_with_attribute_and_value (name, value) page =
  let doc_model = Html.doc_model (Page.document page) in
  let pred t = Html.has_attribute_with_value t name value in
    get_elements pred doc_model;;

(**
   Given an element name and a page, return the list of
   elements (tags) with the specified name.
*)
let get_elements_with_name name page =
  let tag_tbl = Html.tag_tbl (Page.document page) in
    try Hashtbl.find tag_tbl name with _ -> [];;

(* ------------------------------------------------ *)
(* FUNCTIONS WITH TAG LIST ARG *)
(* ------------------------------------------------ *)

(**
   Given an element name and a list of elements, return the
   list of all successor elements to the first element with
   the specified name, in document order.
*)
let rec get_successor_elements name lst =
  match lst with
    | hd :: tl ->
        if Html.tag_name hd = name
        then tl
        else get_successor_elements name tl
    | [] -> [];;

(**
   Given an element name and a list of elements, return the
   list of all successor elements to the last occurrence of
   the element with the specified name, in document order.
*)
let get_successor_elements_last_occurrence name lst =
  let lst1 = List.rev lst in
  let rec f lst1 lst2 =
    match lst1 with
      | hd :: tl ->
          if Html.tag_name hd = name
          then lst2
          else f tl (hd :: lst2)
      | [] -> []
  in
    f lst1 [];;

(**
   Given an element name and a list of elements, return true if
   an element with the specified name is in the list, otherwise
   return false.
*)
let rec is_member_element name lst =
  match lst with
    | hd :: tl ->
        if Html.tag_name hd = name
        then true
        else is_member_element name tl
    | [] -> false;;

(**
   Given an element name and a list of elements, return the
   number of elements in the list with the specified name.
*)
let count_elements_with_name name lst =
  let rec count n l =
    match l with
      | hd :: tl ->
          if Html.tag_name hd = n
          then hd :: count n tl
          else count n tl
      | [] -> []
  in
    List.length (count name lst);;

(**
   Given an attribute name and a list of elements, return the
   number of elements in the list with the specified attribute.
*)
let count_elements_with_attribute name lst =
  let rec count n l =
    match l with
      | hd :: tl ->
          if Html.has_attribute hd n
          then hd :: count n tl
          else count n tl
      | [] -> []
  in
    List.length (count name lst);;

(**
   Given an attribute name and a list of elements, return a list
   of strings where each member is the value of the named attribute
   if present on the corresponding element, or an empty string if not.
*)
let get_attribute_values name elements =
  let rec process lst =
    match lst with
      | hd :: tl ->
          (Stringlib.trim (Html.get_attribute_value hd name)) :: process tl
      | [] -> []
  in
    process elements;;

(**
   Given an element name and a list of elements, return a boolean
   value indicating whether all elements in the list have the
   specified name.
*)
let all_elements_named name lst =
  let pred t =
    Html.tag_name t = name
  in
    List.for_all pred lst;;

(* ------------------------------------------------ *)
(* FUNCTIONS WITH TAG ARG *)
(* ------------------------------------------------ *)

(**
   Given a tag and a list of names, return boolean value indicating
   whether the tag name is a member of the list of names.
*)
let is_named_element (tag : Html.htmlItem Html.tag) (names : string list) =
  List.mem (Html.tag_name tag) names

(**
   Given a tag and an attribute name, return a tuple pair with the
   following values: (1) boolean indicating whether the attribute was
   found on the element; (2) string value of the attribute (an empty
   string if the attribute was not found).
*)
let has_attribute_get_value tag name =
  if Html.has_attribute tag name
  then (true, (Stringlib.normalize_space (Html.get_attribute_value tag name)))
  else (false, "");;

(**
   Given a tag and a list of element names, return the list of
   elements that are descendants of tag and whose names match
   one of those in the list of names.
*)
let get_named_descendants tag names =
  let rec f lst =
    match lst with
      | Html.Tag t :: tl ->
          if List.mem (Html.tag_name t) names
          then t :: f ((Html.tag_children t) @ tl)
          else f ((Html.tag_children t) @ tl)
      | hd :: tl ->
          f tl
      | [] -> []
  in
    f (Html.tag_children tag);;

(**
   Given a tag and a list of element names, return the number
   of elements that are descendants of tag and whose names
   match one of those in the list of names.
*)
let count_named_descendants tag names =
  let rec count num lst =
    match lst with
      | Html.Tag t :: tl ->
          if List.mem (Html.tag_name t) names
          then count (num + 1) ((Html.tag_children t) @ tl)
          else count num ((Html.tag_children t) @ tl)
      | hd :: tl ->
          count num tl
      | [] -> num
  in
    count 0 (Html.tag_children tag);;

(**
   Given a tag, return all of its child elements.
*)
let get_child_elements tag =
  let rec f lst =
    match lst with
      | Html.Tag t :: tl ->
          t :: f tl
      | hd :: tl ->
          f tl
      | [] -> []
  in
    f (Html.tag_children tag);;

(**
   Given a tag and an element name, return all the tag's
   child elements that have the specified name.
*)
let get_named_child_elements tag name =
  let rec f lst =
    match lst with
      | Html.Tag t :: tl ->
          if Html.tag_name t = name
          then t :: f tl
          else f tl
      | hd :: tl -> f tl
      | [] -> []
  in
    f (Html.tag_children tag);;

(**
   Given a tag, return its first child element. If it has
   no child elements, raise TagNotFound exception.
*)
let get_first_child_element tag =
  let rec f lst =
    match lst with
      | Html.Tag t :: tl -> t
      | hd :: tl -> f tl
      | [] -> raise TagNotFound
  in
    f (Html.tag_children tag);;

(**
   Given a tag, return the weight (total number of characters)
   in all of its trimmed text and entity nodes.
*)
let get_trimmed_content_weight tag =
  let rec f w lst =
    match lst with
      | hd :: tl -> (
          match hd with
            | Html.Tag t -> f w ((Html.tag_children t) @ tl)
            | Html.Entity s -> f (w + 1) tl
            | Html.Text s -> f (w + (String.length (Stringlib.trim s))) tl
            | _ -> f w tl
        )
      | [] -> w
  in
    f 0 (Html.tag_children tag);;

(**
   Given a tag, test whether content weight is greater than zero.
*)
let has_content tag =
  let weight = get_trimmed_content_weight tag in
    weight > 0;;

(**
   Given a tag, test whether content weight, including img alt text, is greater than zero.
*)
let has_content_with_img_alt tag =
  let content = Html.get_node_content_with_img_alt "" [Html.Tag tag] in
    String.length (Stringlib.trim content) > 0;;

(**
   Given a tag and a list of names, test whether all text content of tag is
   contained within first descendant found with one of the specified names.
*)
let all_text_content_in_named_descendant tag names =
  let descendants = get_named_descendants tag names in
    if List.length descendants > 0
    then (
      let first_descendant = List.hd descendants in
      let inner_weight = get_trimmed_content_weight first_descendant in
      let outer_weight = get_trimmed_content_weight tag in
        inner_weight = outer_weight
    )
    else false;;

(**
   Predicate to determine whether an "A" element is focusable.
*)
let is_focusable_link (tag : Html.htmlItem Html.tag) =
  assert (Html.tag_name tag = "A");
  if Html.has_attribute tag "href"
  then true
  else (
    let (has_tabindex, tabindex_value) = has_attribute_get_value tag "tabindex" in
      if has_tabindex
      then
        let tabindex =
          try int_of_string tabindex_value
          with _ -> -1
        in
          tabindex >= 0
      else false
  )

(**
   Predicate to determine whether an element is focusable.
*)
let is_focusable (tag :  Html.htmlItem Html.tag) =
  if List.mem (Html.tag_name tag) ["AREA"; "BUTTON"; "INPUT"; "SELECT"; "TEXTAREA"]
  then true
  else (
    if Html.tag_name tag = "A"
    then is_focusable_link tag
    else false
  )

(**
   Given a tag and the name of the desired descendants, return a list
   of the descendant elements, excluding any that are also descendants
   of a nested element with the same tag name as the specified tag.
*)
let get_direct_descendants tag name =
  let stop_tag_name = Html.tag_name tag in
  let rec process lst =
    match lst with
      | Html.Tag t :: tl ->
          if Html.tag_name t = name
          then t :: process tl
          else (
            if Html.tag_name t = stop_tag_name
            then process tl
            else process ((Html.tag_children t) @ tl)
          )
      | hd :: tl -> process tl
      | [] -> []
  in
    process (Html.tag_children tag);;

(**
   Given a tag and the name of a descendant element, return a
   boolean value indicating whether the tag has a direct descendant
   of that name, excluding any that are also descendants of a nested
   element with the same tag name as the specified tag.
*)
let has_direct_descendant tag name =
  let stop_tag_name = Html.tag_name tag in
  let rec process lst =
    match lst with
      | Html.Tag t :: tl ->
          if Html.tag_name t = name
          then true
          else (
            if Html.tag_name t = stop_tag_name
            then process tl
            else process ((Html.tag_children t) @ tl)
          )
      | hd :: tl -> process tl
      | [] -> false
  in
    process (Html.tag_children tag);;

(* ------------------------------------------------ *)
(* FUNCTIONS WITH HASHTBL ARG *)
(* ------------------------------------------------ *)

(**
   Given the Html.tag_tbl for a page and a key (tag name),
   return the list of tags with the specified name.
*)
let get_tags tbl name =
  try Hashtbl.find tbl name with _ -> [];;

(**
   Given the Html.tag_tbl for a page and a key (tag name),
   return the number of tags with the specified name.
*)
let count_tags tbl name =
  let tags = get_tags tbl name in
    List.length tags;;

(* ------------------------------------------------ *)
(* FUNCTIONS WITH STRING LIST ARG *)
(* ------------------------------------------------ *)

(**
   Given a list of strings, return a pair (tuple) of ints with first
   equal to the number of unique strings in the list, and second the
   total number of strings.
*)
let count_unique_strings strings =
  let rec f cnt lst =
    match lst with
      | hd :: tl ->
          let pred s =
            String.compare hd s = 0
          in
          let (lp, lf) = List.partition pred tl in
            if (List.length lp > 0)
            then (f cnt lf)
            else (f (cnt + 1) lf)
      | [] -> cnt
  in
  let cnt_unique = f 0 strings in
    (cnt_unique, List.length strings);;

(* ------------------------------------------------ *)
(* STRING SEARCH FUNCTIONS *)
(* ------------------------------------------------ *)

(** Returns true if sub is found in str, false otherwise. *)
let contains str sub =
  if (String.length sub) = 0
  then false
  else (
    try (Str.search_forward (Str.regexp_string sub) str 0) >= 0
    with Not_found -> false
  );;

(** Case-insensitive version of contains. *)
let icontains str sub =
  if (String.length sub) = 0
  then false
  else (
    try (Str.search_forward (Str.regexp_string_case_fold sub) str 0) >= 0
    with Not_found -> false
  );;

(**
   Return the number of substrings that match regular
   expression re in string str. Note that after each match,
   count_matches resets its start position ahead one character
   past the beginning of the substring previously matched.
*)
let count_matches re str =
  let rec f count start =
    try
      let pos = Str.search_forward re str start in
        f (count + 1) (pos + 1)
    with Not_found -> count
  in
    f 0 0;;

(**
   Given a string s and a list of strings lst, return the number
   of occurrences of s in lst.
*)
let count_occurrences s lst =
  let f a b =
    if compare s b = 0
    then a + 1
    else a
  in
    List.fold_left f 0 lst;;

(**
   Replace all contiguous sequences of nonalphanumeric characters
   in s with a single space character.
*)
let replace_nonalphanumeric s =
  Str.global_replace (Str.regexp "[^a-zA-Z0-9]+") " " s;;

(**
   Same as replace_nonalphanumeric s, but with leading
   and trailing space characters removed.
*)
let normalize_alphanumeric s =
  Stringlib.trim (replace_nonalphanumeric s);;

(**
   Split the string s on one or more space characters
   and return the resulting list.
*)
let split_spaces s =
  Str.split (Str.regexp " +") s;;

(**
   Match each word in src string with tgt string, ignoring case,
   spacing and punctuation. If seq (sequential match) flag is set,
   begin the search for each word in src at the next character in
   tgt just beyond the previous word; otherwise search from the
   beginning of tgt for each word in src.
*)
let match_words src tgt =
  let words = split_spaces (replace_nonalphanumeric src) in
    if List.length words = 0 then false
    else (
      let rec f lst pos =
        match lst with
          | hd :: tl -> (
              let re = Str.regexp_case_fold hd in
                try
                  let idx = Str.search_forward re tgt pos in
                    f tl (idx + (String.length hd))
                with Not_found -> false
            )
          | [] -> true
      in
        f words 0
    );;

(* ------------------------------------------------ *)
(* CALCULATION AND CONVERSION FUNCTIONS *)
(* ------------------------------------------------ *)

(** Convert boolean value to integer. *)
let int_of_bool b = if b then 1 else 0;;

(** Convert integer to boolean value. *)
let bool_of_int i = if (i = 0) then false else true;;

(** Round float to nearest integer. *)
let round f =
  int_of_float (f +. 0.5);;

(** Return float representing the ratio of count to total. *)
let ratio_of_ints count total =
  if count = 0 || total = 0
  then 0.0
  else (float_of_int count) /. (float_of_int total);;

(** Convert float ratio to an integer percentage. *)
let pct_of_ratio ratio =
  let pct = 100.0 *. ratio in
    if (pct > 0.0 && pct < 1.0)
    then 1
    else int_of_float pct;;

(** Return integer representing the percentage of count to total. *)
let pct_of_ints count total =
  if count = 0 || total = 0
  then 0
  else if count = total
  then 100
  else (
    let pct = (100 * count) / total in
      if pct = 0
      then 1
      else pct
  );;

(** Return float representing the average of a sum of ratios based on count. *)
let avg_of_ratios sum_ratios count =
  if count = 0
  then 0.0
  else (sum_ratios /. (float_of_int count));;

(* ------------------------------------------------ *)
(* OUTPUT FUNCTIONS *)
(* ------------------------------------------------ *)

let msg debug_flag label text =
  if debug_flag
  then print_endline (">>> " ^ label ^ ": " ^  text)

let linefeed debug_flag n =
  if debug_flag
  then print_string (String.make n '\n')

(* ------------------------------------------------ *)
(* HTMLITEM FUNCTIONS *)
(* ------------------------------------------------ *)

let item_to_string (item : Html.htmlItem) =
  Stringlib.trim (Html.htmlItem_to_string "" item)

let is_or_contains_only (item : Html.htmlItem) (names : string list) =
  match item with
    | Html.Tag tag ->
        is_named_element tag names
        || all_text_content_in_named_descendant tag names
    | _ -> false;;

let is_printable_string str =
  String.length (Stringlib.normalize_space str) > 0

let is_printable_entity str =
  match str with
    | "#09"  -> false
    | "#10"  -> false
    | "#13"  -> false
    | "#32"  -> false
    | "nbsp" -> false
    | _ -> true

(**
   Given an htmlItem, return a boolean value indicating
   whether the item is of interest. For example, empty strings,
   non-printable entities, comments, processing instructions,
   etc. are ignored.
*)
let is_valid_item (item : Html.htmlItem) =
  match item with
    | Html.Tag tag   -> true
    | Html.Text s    -> is_printable_string s
    | Html.Entity e  -> is_printable_entity e
    | _              -> false

(**
   Given an htmlItem list, return the next item that passes
   the is_valid_item test, or Html.NULL if none is found.
*)
let rec next_valid_item (lst : Html.htmlItem list) =
  match lst with
    | hd :: tl ->
        if is_valid_item hd
        then hd
        else next_valid_item tl
    | [] -> Html.NULL

(**
   Given an htmlItem list, return the number of items
   that pass the is_valid_item test.
*)
let count_valid_items (lst : Html.htmlItem list) =
  List.length (List.filter is_valid_item lst)

(**
   Given an htmlItem and a list of htmlItems, return the sublist
   of htmlItems that follow the given item.
*)
let rec get_following_items (item : Html.htmlItem) (lst : Html.htmlItem list) =
  match lst with
    | Html.Tag tag :: tl ->
        if compare item (Html.Tag tag) = 0
        then tl
        else get_following_items item tl
    | hd :: tl ->
        get_following_items item tl
    | [] -> []

(**
   Given a tag and a list of names, test whether the tag is the first child
   of a parent element that has one of the specified names.
*)
let is_first_valid_child_of_named_parent (tag : Html.htmlItem Html.tag) (names : string list) =
  let parent = Html.tag_parent tag in
    match parent with
      | Html.Tag p ->
          if is_named_element p names
          then (
            let children = Html.tag_children p in
              compare (Html.Tag tag) (next_valid_item children) = 0
          )
          else false
      | _ -> false

(**
   Given a tag, return the htmlItem that immediately precedes it.
*)
let get_preceding_sibling (tag : Html.htmlItem Html.tag) =
  let parent = Html.tag_parent tag in
    match parent with
      | Html.Tag p ->
          let rev_siblings = List.rev (Html.tag_children p) in
          let predecessors = get_following_items (Html.Tag tag) rev_siblings in
            next_valid_item predecessors
      | _ -> Html.NULL

(**
   Given a tag and a list of names, determine whether the tag is immediately
   preceded by an element whose name is in the list of names, or by another
   element that contains only such an element. If the given tag is wrapped
   in a div element and is the first valid item of the div parent's children,
   then make the same determination for the div re. the preceding item.
*)
let is_preceded_by (tag : Html.htmlItem Html.tag) (names : string list) =
  let preceding_sibling = get_preceding_sibling tag in
    if is_or_contains_only preceding_sibling names
    then (
      true
    )
    else (
      if is_first_valid_child_of_named_parent tag ["DIV"]
      then (
        let parent = Html.tag_parent tag in
          match parent with
            | Html.Tag p ->
                let item_preceding_div = get_preceding_sibling p in
                  is_or_contains_only item_preceding_div names
            | _ -> false
      )
      else false
    )

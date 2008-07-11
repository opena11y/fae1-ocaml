(**
   Utility functions for general use in page markup tests
*)

let debug = false;;

(**
   Trim leading and trailing whitespace from string s;
   borrowed from PLEAC-Objective CAML.
*)
let trim s =
  let s' = Str.replace_first (Str.regexp "^[ \t\n]+") "" s in
    Str.replace_first (Str.regexp "[ \t\n]+$") "" s';;

(**
   Remove leading and trailing whitespace, and replace each
   sequence of multiple whitespace characters within s with
   a single space character.
*)
let normalize_space s =
  Str.global_replace (Str.regexp "[ \t\n]+") " " (trim s);;

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
      (Html.Tag t) :: tl -> (
        if pred t
        then t :: get_elements pred ((Html.tag_children t) @ tl)
        else get_elements pred ((Html.tag_children t) @ tl)
      )
    | hd :: tl -> get_elements pred tl
    | [] -> [];;

(* FUNCTIONS WITH PAGE ARG *)

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

(* FUNCTIONS WITH TAG LIST ARG *)

(**
  Given an element name and a list of elements, return the
  list of all successor elements to the first element with
  the specified name, in document order.
*)
let rec get_successor_elements name lst =
  match lst with
      hd :: tl ->
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
        hd :: tl ->
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
      hd :: tl ->
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
        hd :: tl ->
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
        hd :: tl ->
          if Html.has_attribute hd n
          then hd :: count n tl
          else count n tl
      | [] -> []
  in
    List.length (count name lst);;

(* FUNCTIONS WITH TAG ARG *)

(**
  Given a tag and a list of element names, return the list of
  elements that are descendants of tag and whose names match
  one of those in the list of names.
*)
let get_descendants tag names =
  let rec f lst =
    match lst with
        (Html.Tag t) :: tl ->
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
let count_descendants tag names =
  let rec count num lst =
    match lst with
        (Html.Tag t) :: tl ->
          if List.mem (Html.tag_name t) names
          then count (num + 1) ((Html.tag_children t) @ tl)
          else count num ((Html.tag_children t) @ tl)
      | hd :: tl ->
          count num tl
      | [] -> num
  in
    count 0 (Html.tag_children tag);;

(**
  Given a tag, return all of its child tag elements.
*)
let get_child_elements tag =
  let rec f lst =
    match lst with
        (Html.Tag t) :: tl ->
          t :: f tl
      | hd :: tl ->
          f tl
      | [] -> []
  in
    f (Html.tag_children tag);;

(**
  Given a tag, return the weight (total number of characters)
  in all of its trimmed text and entity nodes.
*)
let get_trimmed_content_weight tag =
  let rec f w lst =
    match lst with
        hd :: tl -> (
          match hd with
              Html.Tag t -> f w ((Html.tag_children t) @ tl)
            | Html.Entity s -> f (w + 1) tl
            | Html.Text s -> f (w + (String.length (trim s))) tl
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

(* FUNCTIONS WITH HASHTBL ARG *)

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

(* STRING SEARCH FUNCTIONS *)

(** Returns true if sub is found in str, false otherwise. *)
let contains str sub =
  let pattern = ".*" ^ sub ^ ".*" in
    Str.string_match (Str.regexp pattern) str 0;;

(** Case-insensitive version of contains. *)
let icontains str sub =
  let pattern = ".*" ^ (String.lowercase sub) ^ ".*" in
    Str.string_match (Str.regexp pattern) (String.lowercase str) 0;;

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
  trim (replace_nonalphanumeric s);;

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
            hd :: tl -> (
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

(* CALCULATION AND CONVERSION FUNCTIONS *)

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

(* OUTPUT FUNCTIONS *)

(** Debugging function controlled by variable debug. *)
let msg s =
  if debug
  then print_endline ("Running " ^ s);;

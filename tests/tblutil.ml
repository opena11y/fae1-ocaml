(* File: tblutil.ml *)

exception TagNotFound

let debug = false

(* ---------------------------------------------------------------- *)
let rec count_nested_tables count depth max t_lst =
  match t_lst with
    | Html.Tag t :: tl ->
        if Html.tag_name t = "TABLE"
        then (
          let (ch_count, ch_max) =
            let n_max = if depth > max then depth else max in
              if depth = 0
              then count_nested_tables 0 (depth + 1) n_max (Html.tag_children t)
              else count_nested_tables 1 (depth + 1) n_max (Html.tag_children t)
          in
          let new_max =
            if ch_max > max
            then ch_max
            else max
          in
            count_nested_tables (ch_count + count) depth new_max tl
        )
        else count_nested_tables count depth max ((Html.tag_children t)@tl)
    | hd::tl -> count_nested_tables count depth max tl
    | [] -> (count, max);;

(* ---------------------------------------------------------------- *)
(* ---------------------- SIMPLE DATA TABLES ---------------------- *)
(* ---------------------------------------------------------------- *)

let rec count_table_rows count tr_parent =
  let f a b =
    match b with
      | Html.Tag t ->
          if Html.tag_name t = "TR"
          then a + 1
          else a + count_table_rows 0 t
      | _ -> a
  in
    List.fold_left f 0 (Html.tag_children tr_parent);;

(* ---------------------------------------------------------------- *)
let rec count_table_columns count td_parent =
  let f a b =
    match b with
      | Html.Tag t ->
          if Html.tag_name t = "TH" || Html.tag_name t = "TD"
          then (
            let cols =
              try
                let s = Html.get_attribute t "colspan" in
                  int_of_string (Html.attr_value s)
              with _ -> 1
            in
              a + cols
          )
          else a + count_table_columns 0 t
      | _ -> a
  in
    List.fold_left f 0 (Html.tag_children td_parent);;

(* ---------------------------------------------------------------- *)
(**
   Given a TABLE element, return the first TR element found
   that is a direct descendant (i.e., do not process nested
   TABLE elements).
*)
let retrieve_first_row table =
  let stop_tag_name = "TABLE" in
  let rec process lst =
    match lst with
      | Html.Tag t :: tl ->
          if Html.tag_name t = "TR"
          then t
          else (
            if Html.tag_name t = stop_tag_name
            then process tl
            else process ((Html.tag_children t) @ tl)
          )
      | hd :: tl ->
          process tl
      | [] -> raise TagNotFound
  in
    process (Html.tag_children table);;

(* ---------------------------------------------------------------- *)
let get_table_dimensions table =
  let row_count = count_table_rows 0 table in
  let col_count =
    try
      let first_row = retrieve_first_row table in
        count_table_columns 0 first_row
    with _ -> 0
  in
    if debug
    then print_endline ((string_of_int row_count) ^ " : " ^ (string_of_int col_count));
    (row_count, col_count);;

(* ---------------------------------------------------------------- *)
let has_td_with_scope_or_headers table =
  let td_elements = Testutil.get_direct_descendants table "TD" in
  let pred tag =
    Html.has_attribute tag "scope" || Html.has_attribute tag "headers"
  in
  let rec process lst =
    match lst with
      | hd :: tl ->
          if pred hd
          then true
          else process tl
      | [] -> false
  in
    process td_elements;;

(* ---------------------------------------------------------------- *)
(**
   To qualify as a data table, the table element must:
   1. contain at least 2 rows and 2 columns, i.e.,
      2 tr elements and one tr element such that (tr/[count(td) + count(th) >= 2])
   2. have or contain at least one of the following:
      * summary attribute
      * caption element
      * thead element
      * th element
      * td element with scope or headers attribute
*)
let is_data_table table =
  let min_dim = 2 in
  let (num_rows, num_cols) = get_table_dimensions table in
    if num_rows >= min_dim && num_cols >= min_dim
    then (
      if Html.has_attribute table "summary"
      then true
      else (
        if Testutil.has_direct_descendant table "CAPTION"
        then true
        else (
          if Testutil.has_direct_descendant table "THEAD"
          then true
          else (
            if Testutil.has_direct_descendant table "TH"
            then true
            else (
              if has_td_with_scope_or_headers table
              then true
              else false
            )
          )
        )
      )
    )
    else false;;

(* ---------------------------------------------------------------- *)
(**
   Given a table element, return a boolean value indicating
   whether the table has a 'th' element as the first cell of
   each column.
*)
let has_th_1st_cell_for_all_columns table =
  try
    let first_row = retrieve_first_row table in
    let child_elements = Testutil.get_child_elements first_row in
      if List.length child_elements > 0
      then Testutil.all_elements_named "TH" child_elements
      else false
  with _ -> false;;

(* ---------------------------------------------------------------- *)
(**
   Given a table element, return a boolean value indicating
   whether the table has a 'th' element in each row (with
   position being arbitrary).
*)
let has_th_any_cell_for_all_rows table =
  let tr_elements = Testutil.get_direct_descendants table "TR" in
    if List.length tr_elements > 0
    then (
      let pred tag =
        let th_elements = Testutil.get_named_child_elements tag "TH" in
          List.length th_elements > 0
      in
        List.for_all pred tr_elements
    )
    else false;;

(* ---------------------------------------------------------------- *)
(* ---------------------- COMPLEX DATA TABLES --------------------- *)
(* ---------------------------------------------------------------- *)

(**
   Given a thead element, return a boolean value indicating whether
   it contains multiple 'tr' elements.
*)
let has_multiple_tr_descendants thead_elem =
  let tr_desc = Testutil.get_direct_descendants thead_elem "TR" in
    List.length tr_desc > 1;;

(* ---------------------------------------------------------------- *)
(**
   Given a tag and an attribute name, return a boolean value
   indicating whether the tag has an attribute with the specified
   name and its value is a number > 1.
*)
let has_attr_with_value_gt_one tag attr_name =
  try
    let attr = Html.get_attribute tag attr_name in
      try
        let value = int_of_string (Html.attr_value attr) in
          value > 1
      with _ -> false
  with _ -> false;;

(* ---------------------------------------------------------------- *)
(**
   Given a tag, return a boolean value indicating whether it has
   a 'rowspan' or 'colspan' attribute with a numeric value > 1.
*)
let spans_multiple_rows_or_columns tag =
  has_attr_with_value_gt_one tag "rowspan" ||
  has_attr_with_value_gt_one tag "colspan";;

(**
   Given a 'tr' element, return a boolean value indicating whether
   it has a 'td' or 'th' child that spans multiple rows or columns.
*)
let has_spanning_td_or_th_child tr_elem =
  let td_elements = Testutil.get_named_child_elements tr_elem "TD" in
  let th_elements = Testutil.get_named_child_elements tr_elem "TH" in
    List.exists spans_multiple_rows_or_columns td_elements ||
    List.exists spans_multiple_rows_or_columns th_elements;;

(**
   Given 'tr' element, return a boolean value indicating whether
   it contains both a 'td' and multiple 'th' children.
*)
let has_td_and_multiple_th_children tr_elem =
  let td_elements = Testutil.get_named_child_elements tr_elem "TD" in
  let th_elements = Testutil.get_named_child_elements tr_elem "TH" in
    List.length td_elements > 0 &&
    List.length th_elements > 1;;

(* ---------------------------------------------------------------- *)
(**
   Given a 'tr' element, return a boolean value indicating whether
   it contains only 'th' children.
*)
let has_only_th_children tag =
  let children = Testutil.get_child_elements tag in
  let count_th = Testutil.count_elements_with_name "TH" children in
    List.length children = count_th;;

(* ---------------------------------------------------------------- *)
(**
   Given a tag and an attribute name, return a boolean value
   indicating whether the tag has an attribute with the specified
   name and its value is comprised of more than two whitespace-
   separated substrings.
*)
let has_attr_with_multiple_values tag attr_name =
  let rgx = Str.regexp "[ \t]+" in
  try
    let attr = Html.get_attribute tag attr_name in
      try
        let value = Html.attr_value attr in
          List.length (Str.split rgx value) > 2
      with _ -> false
  with _ -> false;;

(**
   Given a tag, return a boolean value indicating whether it has
   a 'headers' attribute with value containing embedded whitespace
   characters.
*)
let has_multi_value_headers_attr tag =
  has_attr_with_multiple_values tag "headers";;

(**
   Given a 'tr' element, return a boolean value indicating whether
   it has a 'td' or 'th' child that has a 'headers' attribute that
   has multiple (space-separated) values.
*)
let has_multi_value_headers_td_or_th_child tr_elem =
  let td_elements = Testutil.get_named_child_elements tr_elem "TD" in
  let th_elements = Testutil.get_named_child_elements tr_elem "TH" in
    List.exists has_multi_value_headers_attr td_elements ||
    List.exists has_multi_value_headers_attr th_elements;;

(* ---------------------------------------------------------------- *)
(**
   To qualify as a complex data table, the table element must:
   1. qualify as a simple data table AND
   2. contain at least one of the following:
      * thead element that contains more than one tr element
      * tr element with a td or th element with rowspan or colspan
        attribute value > 1
      * tr element that contains both td elements AND more than one
        th element
      * two or more tr elements that contain only th elements
      * tr element with a td or th element with headers attribute
        that contains more than two idrefs
   Note: is_complex_data_table assumes that its argument has already
   been qualified as a simple data table!!!
*)
let is_complex_data_table table =
  let thead_list = Testutil.get_direct_descendants table "THEAD" in
  let tr_list = Testutil.get_direct_descendants table "TR" in
    List.exists has_multiple_tr_descendants thead_list ||
    List.exists has_spanning_td_or_th_child tr_list ||
    List.exists has_td_and_multiple_th_children tr_list ||
    List.length (List.filter has_only_th_children tr_list) > 1 ||
    List.exists has_multi_value_headers_td_or_th_child tr_list;;

(* ---------------------------------------------------------------- *)
(**
   Given a list of table elements, return a list of all 'th'
   elements that are their direct descendants.
*)
let rec get_all_th_elements lst =
  match lst with
    | hd :: tl -> Testutil.get_direct_descendants hd "TH" @ get_all_th_elements tl
    | [] -> [];;

(* ---------------------------------------------------------------- *)
(**
   Given a 'td' element and a list of 'id' values, return a boolean
   value indicating whether all of the IDREF substrings in the 'td'
   element's 'headers' attribute are members of the 'id' list.
*)
let has_headers_with_valid_idrefs td_elem id_list =
  try
    let attr = Html.get_attribute td_elem "headers" in
    let headers = Str.split (Str.regexp "[ \t]+") (Html.attr_value attr) in
    let count_refs a b =
      if List.mem b id_list
      then a + 1
      else a
    in
      List.fold_left count_refs 0 headers = List.length headers
  with _ -> false;;

(* ---------------------------------------------------------------- *)
(**
   Given a 'table' element, return a tuple whose first value is the
   number of 'td' elements it contains with a 'headers' attribute
   whose subvalues (IDREFS) each reference one of the table's 'th'
   elements via that element's 'id' attribute value, and whose
   second number is the total number of 'td' elements in the table.
*)
let count_td_elements_with_proper_headers table =
  let td_list = Testutil.get_direct_descendants table "TD" in
  let th_list = Testutil.get_direct_descendants table "TH" in
  let id_values = Testutil.get_attribute_values "id" th_list in
  let count a b =
    if has_headers_with_valid_idrefs b id_values
    then a + 1
    else a
  in
    (List.fold_left count 0 td_list, List.length td_list);;

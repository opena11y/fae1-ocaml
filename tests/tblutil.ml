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
let has_th_cell_for_all_columns table =
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
   whether the table has a 'th' element as the first cell of
   each row.
*)
let has_th_cell_for_all_rows table =
  let tr_elements = Testutil.get_direct_descendants table "TR" in
    if List.length tr_elements > 0
    then (
      let pred tag =
        try
          let first_cell = Testutil.get_first_child_element tag in
            Html.tag_name first_cell = "TH"
        with _ -> false
      in
        List.for_all pred tr_elements
    )
    else false;;

(* ---------------------------------------------------------------- *)
(**
   To qualify as a complex data table, the table element must:
   1. qualify as a simple data table AND
   2. contain at least one of the following:
      * thead element that contains more than one tr element
      * tr element with a td or th element with rowspan or colspan attribute value > 1
      * tr element that contains both td elements AND more than one th element
      * two or more tr elements that contain only th elements
      * tr element with a td or th element with headers attribute that contains more than two idrefs
*)
let is_complex_data_table table =
  true;;

(* File: htmlutil.ml *)

open Html

exception TagNotFound;;

let is_element item name =
  match item with
      Html.Tag t -> if (Html.tag_name t) = name
      then true
      else false
    |_ -> false ;;

let rec has_child_element_rec item_lst name stop_tag_name =
  match item_lst with
      (Html.Tag t)::tl ->
        if((Html.tag_name t) =  name)
        then true
        else if (Html.tag_name t) = stop_tag_name
        then (has_child_element_rec tl name stop_tag_name)
        else (has_child_element_rec ((Html.tag_children t)@tl)
                name stop_tag_name)
    | _::tl -> has_child_element_rec tl name stop_tag_name
    | [] -> false;;

let has_child_element tag name =
  let children = Html.tag_children tag in
    has_child_element_rec children name "";;

let has_child_element_with_stop tag name stop_tag_name =
  let children = Html.tag_children tag in
    has_child_element_rec children name stop_tag_name;;

let rec count_nested_tables count depth max t_lst =
  match t_lst with
      (Html.Tag t)::tl -> (
        if(Html.tag_name t = "TABLE")
        then (
          let (ch_count, ch_max) =
            let n_max = if depth > max then depth else max in
              if(depth = 0)
              then (
                count_nested_tables 0 (depth + 1) n_max (Html.tag_children t)
              )
              else (
                count_nested_tables 1 (depth + 1) n_max (Html.tag_children t)
              )
          in
          let new_max = if(ch_max > max) then ch_max else max in
            count_nested_tables (ch_count + count) depth new_max tl
        )
        else (
          count_nested_tables count depth max ((Html.tag_children t)@tl)
        )
      )
    | hd::tl -> count_nested_tables count depth max tl
    | [] -> (count, max)
;;

let rec count_table_rows tr_parent count =
  let f a b =
    if (is_element b "TR")
    then (a + 1)
    else (
      match b with
          Html.Tag t -> (a + count_table_rows t 0)
        |_ -> a
    )
  in
    List.fold_left f 0 (Html.tag_children tr_parent);;

let rec count_table_columns td_parent count =
  let f a b =
    if (is_element b "TH" || is_element b "TD")
    then (
      let cols =
        try
          match b with
              Html.Tag t -> (
                let s = Html.get_attribute t "colspan" in
                  (int_of_string (Html.attr_value s))
              )
            |_ -> 1
        with _ -> 1
      in
        a + cols
    )
    else (
      match b with
          Html.Tag t -> (a + count_table_columns t 0)
        |_ -> a
    )
  in
    List.fold_left f 0 (Html.tag_children td_parent);;

let get_table_dimensions table =
  let row_count = count_table_rows table 0 in
  let col_count =
    try
      let rec retrieve_first_row ch_lst =
        match ch_lst with
            hd::tl -> if(is_element hd "TR")
            then hd
            else (
              let add_lst =
                match hd with
                    Html.Tag t -> Html.tag_children t
                  |_ -> []
              in
                retrieve_first_row (add_lst@tl))
          |_ -> raise TagNotFound

      in
      let first_row = retrieve_first_row (Html.tag_children table) in
        match first_row with
            Html.Tag row -> count_table_columns row 0
          |_ -> 0
    with _ -> 0
  in
    (row_count, col_count);;

let is_data_table table =
  let dims = get_table_dimensions table in
  let (num_rows, num_cols) = dims in
  let dim_check = (num_rows > 1 || num_cols > 1) in
  let has_thead = has_child_element_with_stop table "THEAD" "TABLE" in
  let has_th = has_child_element_with_stop table "TH" "TABLE" in
    ((dim_check && (has_thead || has_th)), dims,
     has_thead, has_th);;

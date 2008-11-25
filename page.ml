(**
   Container for name and document model of page
*)

(**
   The type page is used to represent a single HTML page, which is
   composed of a name (filename), a doctype (DOCTYPE declaration)
   and a tree of htmlItems (htmlDoc).
*)
type page = {
  mutable pagename : string;
  mutable doctype  : string;
  mutable data_tables : Html.htmlItem Html.tag list;
  mutable document : Html.htmlDoc;
}

(** Generic constructor for pages *)
let create_page doc = {
  pagename = "";
  doctype  = "";
  data_tables = [];
  document = doc;
}

(** Set pagename of page p to string n *)
let set_pagename p n = p.pagename <- n;;

(** Set doctype of page p to string s *)
let set_doctype p s = p.doctype <- s;;

(** Set data_tables of page p to list lst *)
let set_data_tables p lst = p.data_tables <- lst;;

(** Get pagename of page p *)
let pagename p = p.pagename;;

(** Get doctype of page p *)
let doctype p = p.doctype;;

(** Get data_tables of page p *)
let data_tables p = p.data_tables;;

(** Get document of page p *)
let document p = p.document;;

(**
   Return a string representation of the page, which
   is the equivalent of the string representation of
   the htmlDoc document.
*)
let page_to_string p =
  let item_list = Html.doc_model p.document  in
    Html.fold_to_string item_list (Html.htmlItem_to_string "") "";;

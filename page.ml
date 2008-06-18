(**
   Container for name and document model of page
*)

open Html

(**
   The type page is used to represent a single html page, which is
   composed of a name (filename) and tree of htmlItems (htmlDoc).
*)
type page = {
  mutable pagename : string;
  mutable document : Html.htmlDoc;
}

(** Generic constructor for pages *)
let create_page doc = {
  pagename = "";
  document = doc;
}

(** Set pagename of page p to string n *)
let set_pagename p n = p.pagename <- n;;

(** Get pagename of page p *)
let pagename p = p.pagename;;

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

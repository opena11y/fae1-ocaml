(* File: testutil.mli *)

val trim :
  string -> string;;
val normalize_space :
  string -> string;;

val get_elements_with_names :
  string list -> Page.page -> Html.htmlItem Html.tag list;;
val get_elements_with_attribute :
  string -> Page.page -> Html.htmlItem Html.tag list;;
val get_elements_with_attribute_and_value :
  string * string -> Page.page -> Html.htmlItem Html.tag list;;
val get_elements_with_name :
  string -> Page.page -> Html.htmlItem Html.tag list;;

val get_successor_elements :
  string -> Html.htmlItem Html.tag list -> Html.htmlItem Html.tag list;;
val get_successor_elements_last_occurrence :
  string -> Html.htmlItem Html.tag list -> Html.htmlItem Html.tag list;;
val is_member_element :
  string -> Html.htmlItem Html.tag list -> bool;;
val count_elements_with_name :
  string -> Html.htmlItem Html.tag list -> int;;
val count_elements_with_attribute :
  string -> Html.htmlItem Html.tag list -> int;;

val get_descendants :
  Html.htmlItem Html.tag -> string list -> Html.htmlItem Html.tag list;;
val count_descendants :
  Html.htmlItem Html.tag -> string list -> int;;
val get_child_elements :
  Html.htmlItem Html.tag -> Html.htmlItem Html.tag list;;
val get_trimmed_content_weight :
  Html.htmlItem Html.tag -> int;;

val get_tags :
  (string, Html.htmlItem Html.tag list) Hashtbl.t -> string -> Html.htmlItem Html.tag list;;
val count_tags :
  (string, Html.htmlItem Html.tag list) Hashtbl.t -> string -> int;;

val contains :
  string -> string -> bool;;
val icontains :
  string -> string -> bool;;
val count_matches :
  Str.regexp -> string -> int;;

val int_of_bool :
  bool -> int;;
val bool_of_int :
  int -> bool;;

val round :
  float -> int;;
val ratio_of_ints :
  int -> int -> float;;
val pct_of_ratio :
  float -> int;;
val pct_of_ints :
  int -> int -> int;;
val avg_of_ratios :
  float -> int -> float;;

val msg :
  string -> unit;;

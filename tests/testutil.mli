(* File: testutil.mli *)

val get_elements_with_names :
  string list -> Page.page -> Html.htmlItem Html.tag list
val get_elements_with_attribute :
  string -> Page.page -> Html.htmlItem Html.tag list
val get_elements_with_attribute_and_value :
  string * string -> Page.page -> Html.htmlItem Html.tag list
val get_elements_with_name :
  string -> Page.page -> Html.htmlItem Html.tag list

val get_successor_elements :
  string -> Html.htmlItem Html.tag list -> Html.htmlItem Html.tag list
val get_successor_elements_last_occurrence :
  string -> Html.htmlItem Html.tag list -> Html.htmlItem Html.tag list
val is_member_element :
  string -> Html.htmlItem Html.tag list -> bool
val count_elements_with_name :
  string -> Html.htmlItem Html.tag list -> int
val count_elements_with_attribute :
  string -> Html.htmlItem Html.tag list -> int
val get_attribute_values :
  string -> Html.htmlItem Html.tag list -> string list

val is_named_element :
  Html.htmlItem Html.tag -> string list -> bool
val has_attribute_get_value :
  Html.htmlItem Html.tag -> string -> bool * string
val get_named_descendants :
  Html.htmlItem Html.tag -> string list -> Html.htmlItem Html.tag list
val count_named_descendants :
  Html.htmlItem Html.tag -> string list -> int
val get_child_elements :
  Html.htmlItem Html.tag -> Html.htmlItem Html.tag list
val get_named_child_elements:
  Html.htmlItem Html.tag -> string -> Html.htmlItem Html.tag list
val get_trimmed_content_weight :
  Html.htmlItem Html.tag -> int
val has_content :
  Html.htmlItem Html.tag -> bool
val has_content_with_img_alt :
  Html.htmlItem Html.tag -> bool
val all_text_content_in_named_descendant :
  Html.htmlItem Html.tag -> string list -> bool
val is_focusable :
  Html.htmlItem Html.tag -> bool

val get_tags :
  (string, Html.htmlItem Html.tag list) Hashtbl.t -> string -> Html.htmlItem Html.tag list
val count_tags :
  (string, Html.htmlItem Html.tag list) Hashtbl.t -> string -> int

val count_unique_strings:
  string list -> int * int

val contains :
  string -> string -> bool
val icontains :
  string -> string -> bool
val count_matches :
  Str.regexp -> string -> int
val replace_nonalphanumeric :
  string -> string
val normalize_alphanumeric :
  string -> string
val split_spaces :
  string -> string list
val match_words :
  string -> string -> bool

val int_of_bool :
  bool -> int
val bool_of_int :
  int -> bool

val round :
  float -> int
val ratio_of_ints :
  int -> int -> float
val pct_of_ratio :
  float -> int
val pct_of_ints :
  int -> int -> int
val avg_of_ratios :
  float -> int -> float

val msg :
  bool -> string -> string -> unit
val linefeed :
  bool -> int -> unit

val item_to_string :
  Html.htmlItem -> string
val is_or_contains_only :
  Html.htmlItem -> string list -> bool
val is_preceded_by :
  Html.htmlItem Html.tag -> string list -> bool

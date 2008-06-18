(* File: htmldtd.mli *)

val font_style : string list;;
val is_empty : string -> bool;;
val can_contain : string -> string -> bool;;
val optional_end_tag_match : string -> string -> string -> bool;;

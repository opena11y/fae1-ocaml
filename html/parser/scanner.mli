type scanner;;

val create_scanner : unit -> scanner;;

val load_string : scanner -> string -> unit;;
val token_buf : scanner -> string;;

val eof : scanner -> bool;;
val peek : scanner -> char;;
val peek_n : scanner -> int -> char;;
val next : scanner -> unit;;
val skip : scanner -> unit;;
val skip_n : scanner -> int -> unit;;
val skip_until : scanner -> (char -> bool) -> unit;;
val next_until : scanner -> (char -> bool) -> unit;;
val clear_skips : scanner -> unit;;

val set_marker : scanner -> unit;;
val clear_marker : scanner -> unit;;
val clear_markers : scanner -> unit;;
val clear_token_buf : scanner -> unit;;
val reset_to_marker : scanner -> unit;;

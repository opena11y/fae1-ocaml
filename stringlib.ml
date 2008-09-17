(**
   Miscellaneous string functions
*)

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
   Given a list of strings, return a copy with each member of the
   original list whitespace-normalized and converted to lowercase.
*)
let rec normalize_strings lst =
  match lst with
      hd :: tl ->
        (String.lowercase (normalize_space hd)) :: normalize_strings tl
    | [] -> [];;

(**
   Given a character c, return a string of length 1
   where the first character is c.
*)
let string_of_char c = String.make 1 c;;

(**
   Given a string s and delimiter d, remove the last
   item in the string, including the delimiter, if the string
   were to be delimited and transformed to a list with d.
*)
let remove_last_delimited s d =
  try
    let d_pos =
      Str.search_backward (Str.regexp_string d) s
        (String.length(s) - 2) in
      Str.string_before s (d_pos + 1)
  with Not_found -> s;;

(**
   Given a filename, return the filetype of the file.
*)
let get_web_file_suffix s =
  let l = String.length s in
  let dot =
    try
      Str.search_backward (Str.regexp_string ".") s (l)
    with Not_found -> (l)
  in
  let at =
    try
      Str.search_forward (Str.regexp_string "@") s dot
    with Not_found -> l
  in
    if (at > dot)
    then (
      try
        Str.string_after (Str.string_before s at) dot
      with Not_found -> ""
    )
    else ("");;

(**
   Given a fraction p (represented as a float), convert
   p to a percent (multiply by 100.0) and then return
   a string representing that percentage.
*)
let to_percent_string p =
  Printf.sprintf "%.2f%%" (p *. 100.0);;

(**
   Normalize filename by removing prefix (always FILE.) and
   suffix (e.g., .HTML, .CSS, .GIF, .JPEG, .PNG or .JS)
*)

let normalize_filename s =
  let dirname = Filename.dirname s in
  let basename = Filename.basename s in
  let prefix = Str.regexp "^FILE\\." in
  let suffix = Str.regexp "\\.[A-Z]+$" in
  let tmp = Str.replace_first prefix "" basename in
  let fname = Str.replace_first suffix "" tmp in
    dirname ^ "/" ^ fname;;

(**
   Remove from path the substring that precedes and includes s.
*)
let normalize_path path s =
  let rgx = Str.regexp ("^.*" ^ s ^ "/") in
    Str.replace_first rgx "" path;;

(**
   Given s, which is the concatenization of a starting directory path
   and a (possibly relative) file path, remove the necessary components
   from s so that it no longer contains relative path references.
*)
let rec normalize_relative_path s =
  let rgx = Str.regexp "[^/]+/\\.\\./" in
  let idx =
    try Str.search_forward rgx s 0
    with Not_found -> -1
  in
    if idx > -1
    then normalize_relative_path (Str.replace_first rgx "" s)
    else s;;

(**
   Given an HTML pagename, and a URI, both of which may include directory
   path information: If URI begins with 'http://' or 'https://' return it
   unmodified, otherwise return a normalized version of URI such that
   relative path references '../' have been replaced with the appropriate
   path information contained in pagename.

   For example, if the pagename is 'www.uiuc.edu/overview/index.html'
   and the uri is '../images/sitewide/logo.gif', return the string
   'www.uiuc.edu/images/sitewide/logo.gif'.
*)
let construct_uri_path pagename uri =
  if Str.string_match (Str.regexp "http[s]?://") uri 0
  then uri
  else normalize_relative_path ((Filename.dirname pagename) ^ "/" ^ uri);;

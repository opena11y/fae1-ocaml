(**
   Read and write files and directories
*)

(**
   Given a directory, return a list of files in that
   directory. Note that this function does not
   descend into subdirectories.
*)
let readdir_list dir =
  try
    let f a b = (a ^ "/")^b in
      List.map (f dir) (Array.to_list (Sys.readdir dir))
  with _ -> ["Error"]

(**
   Given a directory, return a list of files in that directory.
   If a subdirectory is encountered, recurse on that directory.
*)
let readdir_list_rec dir =
  let d1 = (readdir_list dir) in
  let rec rdl_rec d_list f_list =(
    match d_list with
        [] -> f_list
      | head::tail ->
          match (readdir_list head) with
              "Error"::[] -> rdl_rec tail (head::f_list)
            |_ -> List.append (rdl_rec (readdir_list head) []) (rdl_rec tail f_list)
  )
  in
    rdl_rec d1 []

(**
   Given a filename f, return a string containing the contents of
   file f.
*)
let read_file f =
  let ch = (open_in f) in
  let len = (in_channel_length ch) in
  let rec build_string s chan = (
    try
      let new_s = (s ^ (input_line chan)^"\n") in build_string new_s chan
    with End_of_file -> (close_in chan; s)

  ) in let s = build_string "" ch in close_in ch; s

(**
   Given a filename f and string s, write s to f.
*)
let write_file f s =
  let ch = (open_out f) in
    output_string ch s;
    flush ch;
    close_out ch;
    ();;

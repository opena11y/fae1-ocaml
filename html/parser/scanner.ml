(**
   Functions that work in conjunction with parser module
*)

(**
   type scanner is used to keep track of a string and
   the position in the string that is currently being
   evaluated.
   buf represents the current string being evaluated.
   token_buf is a buffer containing the current token that
   is being constructed.
   markers is a list of integers which represent positions in the
   string that have been 'marked'. Markers can be used along with
   certain convenience functions (discussed below) to allow developers
   to more easily jump around in a scanner.
   skip_list is a list of integers which represent positions in
   the string that were 'skipped' by the scanner. These are
   positions that the scanner passed over, and did not insert into
   token_buf when doing so.
   pos is an integer representing the current position of the scanner
   in buf.
   current_char is the character in buf at position pos.
   eof is a flag that is set to true if the scanner has reached
   the end of the buf.
*)
type scanner = {
  mutable buf : Buffer.t;
  mutable token_buf : Buffer.t;
  mutable markers : int list;
  mutable skip_list : int list;
  mutable pos : int;
  mutable current_char : char;
  mutable eof : bool;
};;

(**
   Generic constructor for a scanner. Results in
   and "empty" scanner.
*)
let create_scanner () = {
  buf = Buffer.create 80;
  token_buf = Buffer.create 32;
  markers = [];
  skip_list = [];
  pos = -1;
  current_char = '\000';
  eof = false;
};;

(**
   Takes a string s and sets up scanner scnr to
   scan that particular string.
*)
let load_string scnr s =
  ignore(Buffer.add_string scnr.buf s);
  ignore(scnr.pos <- -1 );
  ignore(scnr.eof <- false);
  ignore(scnr.skip_list <- []);
  ignore(scnr.current_char <- '\000');
  ignore(Buffer.reset scnr.token_buf);
  ();;

(** Not yet implemented *)
let load_channel scnr s = ();;

(** Get the eof flag for scanner scnr. *)
let eof scnr = scnr.eof;;

(** Set the position of the scanner scnr to position p *)
let set_pos scnr p =
  if(p >= Buffer.length scnr.buf)
  then (
    scnr.eof <- true;
    scnr.current_char <- '\000';
    scnr.pos <- Buffer.length scnr.buf;
  )
  else
    if(p < 0)
    then (
      scnr.eof <- false;
      scnr.current_char <- '\000';
      scnr.pos <- -1;
    )
    else (
      scnr.current_char <- Buffer.nth scnr.buf p;
      scnr.pos <- p;
      if scnr.eof then scnr.eof <- false;
    );;

(**
   Get the contents of the token_buf og scanner scnr as
   a string.
*)
let token_buf scnr = Buffer.contents scnr.token_buf;;

(** Get the current_char of scanner scnr *)
let current_char scnr = scnr.current_char;;

(**
   Look at the character at the current position
   of scanner scnr.
*)
let peek scnr =
  if(Buffer.length scnr.buf <= scnr.pos + 1)
  then '\000'
  else Buffer.nth scnr.buf (scnr.pos + 1);;

(**
   Look at the character at the current position + nth position
   in scanner scnr.
*)
let peek_n scnr n =
  if(Buffer.length scnr.buf <= scnr.pos + n)
  then '\000'
  else Buffer.nth scnr.buf (scnr.pos + n);;

(**
   Move the position of the scanner forward by one. Appends
   the next character to the token_buf.
*)
let next scnr =
  set_pos scnr (scnr.pos + 1);
  if(scnr.pos < Buffer.length scnr.buf)
  then(Buffer.add_char scnr.token_buf scnr.current_char);;

(**
   Move the position of the scanner scnr forward by one. Does
   not append the next character to the token_buf.
*)
let skip scnr  =
  set_pos scnr (scnr.pos + 1);
  if(scnr.pos < Buffer.length scnr.buf)
  then (scnr.skip_list <- [scnr.pos]@scnr.skip_list);;

(**
   Move forward in the scanner until the predicate is true.
   place all characters passed over into token_buf.
*)
let next_until scnr pred =
  while not (pred (peek scnr)) do
    next scnr;
  done;;

(**
   Move forward in the scanner until the predicate is true.
   Ignore all characters passed over.
*)
let skip_until scnr pred =
  while not (pred (peek scnr)) do
    skip scnr;
  done;;

(**
   Move to the current position + nth position of the
   scanner scnr. Do not insert any characters into
   the token_buf.
*)
let rec skip_n scnr n =
  if n > 0 then (skip scnr; skip_n scnr (n - 1));;

(**
   Append a marker to the markers list in scanner scnr at
   scnr's current position.
*)
let set_marker scnr =
  scnr.markers <- [scnr.pos]@scnr.markers;;

(** Remove the last marker in the markers list in scanner scnr. *)
let clear_marker scnr = scnr.markers <- List.tl scnr.markers;;

(** Remove all markers from scanner scnr's marker list. *)
let clear_markers scnr = scnr.markers <- [];;

(** Remove all skips from the scanner scnr's skip list. *)
let clear_skips scnr = scnr.skip_list <- [];;

(** Reset the token_buf in scanner scnr *)
let clear_token_buf  scnr = Buffer.reset scnr.token_buf;;

(**
   Move backward in the scanner by one. Also modify the
   list markers and skip_list, as well as the token_buf,
   appropriately.
*)
let back scnr =
  if(List.length scnr.skip_list > 0 &&
       List.hd scnr.skip_list = scnr.pos)
  then(
    scnr.skip_list <- List.tl scnr.skip_list;
    set_pos scnr (scnr.pos - 1);
  )
  else(
    set_pos scnr (scnr.pos - 1);
    if( (Buffer.length scnr.token_buf) > 0)
    then (
      let s = Buffer.sub scnr.token_buf 0
        ((Buffer.length scnr.token_buf) - 1) in
        Buffer.reset scnr.token_buf;
        Buffer.add_string scnr.token_buf s;
    );
  );;

(** Reset to the last marker in the markers list in scanner scnr. *)
let reset_to_marker scnr =
  let marker_pos = List.hd scnr.markers in
    while scnr.pos > marker_pos do
      back scnr;
    done;
    clear_marker scnr;
    ();;

(** Not implemented. *)
let reset_scanner scnr = ();;

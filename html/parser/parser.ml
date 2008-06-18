(**
   Build the document model
*)

open Tokenizer
open Htmldtd
open Html

(**
   type htmlParser is an individual htmlParser state. doc represents
   an htmlDoc type, which contains the tree of tags parsed thus far.
   tag_stack is a list of htmlItems. Each item in the stack is a child
   of the 'next' item in the stack. In other words, the stack is used
   to maintain the nested/tree structure of an HTML document. The
   current_tag is an htmlItem that can be considered the 'top' of the
   tag_stack (although the current_tag is not explicitly stored in
   tag_stack). The lines value represents the total number of lines
   processed thus far. This value is used to set the line_number value
   of tags when they are created.
*)
type htmlParser = {
  mutable doc : Html.htmlDoc;
  mutable current_tag : Html.htmlItem;
  mutable lines : int;
  mutable tag_stack : (Html.htmlItem) list;
};;

(** Take a tag c and append it to current_tag in parser p *)
let append_child_to_current_tag p c =
  p.current_tag <- Html.append_child p.current_tag c;;

(**
   Take an attribute attr and insert it into the current_tag
   in parser p.
*)
let add_attr_to_current_tag p attr =
  p.current_tag <- Html.add_attribute p.current_tag attr;;

(** Push the tag t onto the tag_stack of parser p. *)
let push_tag_stack p t =
  p.tag_stack <- ([t]@p.tag_stack);;

(**
   Pop the top tag off the tag_stack of parser p. Note
   that the tag is not returned when this happens.
*)
let pop_tag_stack p =
  p.tag_stack <- List.tl p.tag_stack;;

(** Increment the lines value in parser p by 1 *)
let line_incr p =
  p.lines <- p.lines + 1;;

(**
   The real meat of the parser starts here. This is the top level
   of a series of mutually recursive functions that are used to
   parse tokens and build the htmlItem tree.

   The top level function, process_token, accepts a parser and
   a list of tokens. The first token is removed from the list, and
   processed according to its token-type. Other processing functions
   are called, and passed the parser, the first token, and the tail
   of the token list.
*)

let rec process_token p tokens =
  try
    let hd = List.hd tokens in
    let tl = List.tl tokens in
      match hd with
          Tokenizer.StartTag t -> process_start_tag p t tl
        | Tokenizer.EndTag t -> process_end_tag p t tl
        | Tokenizer.Attribute t -> process_attribute p t tl
        | Tokenizer.AttributeValue t -> process_attribute p t tl
        | Tokenizer.Entity t -> process_entity p t tl
        | Tokenizer.Whitespace t -> process_whitespace p t tl
        | Tokenizer.Comment t -> process_comment p t tl
        | Tokenizer.Newline -> process_newline p tl
        | Tokenizer.Text t -> process_text p t tl
        | Tokenizer.SpecialMarkup t -> process_special_markup p t tl
        | Tokenizer.ProcessingInstruction t ->
            process_processing_instruction p t tl
        | Tokenizer.EOF -> p
  with _ -> p
and process_start_tag p t tokens =
  let t_name = Tokenizer.get_value t in
  let parent_name = Html.get_tag_name p.current_tag in
    (* Ok, there are a few things we need to check for here *)

  (* First, see if the current_tag can have this tag as
     a child. *)
  let child_allowed = true (*Htmldtd.can_contain parent_name t_name*) in
  let force_end = Htmldtd.optional_end_tag_match parent_name
    t_name "s" in
    if child_allowed && not force_end
    then (
      let new_tag = Html.Tag (Html.create_tag t_name []
                                p.lines [] p.current_tag) in
        append_child_to_current_tag p new_tag;
        push_tag_stack p p.current_tag;
        p.current_tag <- new_tag;
        process_token p tokens
    )
    else if force_end
    then (
      process_end_tag p
        (Tokenizer.create_token parent_name)
        ([(Tokenizer.StartTag t)]@tokens)
    )
    else (
      process_token p tokens
    )
and process_end_tag p t tokens =
  let et_name = Tokenizer.get_value t in
  let ct_name =
    match p.current_tag with
        Html.Tag ctag -> Html.tag_name ctag
      |_ -> "" in
    if( et_name = ct_name ||
        Htmldtd.optional_end_tag_match ct_name et_name "e")
    then (
      p.current_tag <- (List.hd p.tag_stack);
      pop_tag_stack p;
    );
    process_token p tokens
and process_attribute p t tokens =
  match (List.hd tokens) with
      Tokenizer.AttributeValue token ->
        let attr = Html.create_attribute (Tokenizer.get_value t)
          (Tokenizer.get_value token) in
          add_attr_to_current_tag p attr;
          process_token p (List.tl tokens)

    | _ -> let attr = Html.create_attribute (Tokenizer.get_value t)
        "" in
        add_attr_to_current_tag p attr;
        process_token p tokens

and process_entity p t tokens =
  append_child_to_current_tag p (Html.Entity (Tokenizer.get_value t));
  process_token p tokens
and process_whitespace p t tokens =
  append_child_to_current_tag p (Html.Text (Tokenizer.get_value t));
  process_token p tokens
and process_comment p t tokens =
  append_child_to_current_tag p (Html.Comment (Tokenizer.get_value t));
  process_token p tokens
and process_newline p tokens =
  line_incr p;
  process_token p tokens
and process_text p t tokens =
  append_child_to_current_tag p (Html.Text (Tokenizer.get_value t));
  process_token p tokens
and process_special_markup p t tokens =
  append_child_to_current_tag p
    (Html.SpecialMarkup (Tokenizer.get_value t));
  process_token p tokens
and process_processing_instruction p t tokens =
  append_child_to_current_tag p
    (Html.ProcessingInstruction (Tokenizer.get_value t));
  process_token p tokens;;

(**
   Wrapper function for the process_tokens function. This creates
   a new parser and parses a list of tokens using that parser,
   returning the htmlDoc that results from the parsing process.
 *)
let parse_tokens t_list =
  let p = {
    doc = Html.create_htmlDoc ();
    current_tag = Html.Tag {
      Html.name = "TOP";
      Html.attributes = [];
      Html.children = [];
      Html.line_number = 0;
      Html.parent = Html.NULL;
    };
    lines = 1;
    tag_stack = [];
  } in
    process_token p t_list;;

(**
   Main function for the parser module. Passing a string to this
   function results in tokenization of the string, and then the parsing
   of that list of tokens. The function returns an htmlDoc.
*)
let parse_string s =
  let tokens = Tokenizer.tokenize_string s  in

  let p = parse_tokens tokens in
    while((List.length p.tag_stack) > 0) do
      p.current_tag <- List.hd p.tag_stack;
      pop_tag_stack p;
    done;
    let ct  = p.current_tag in
    let doc = Html.create_htmlDoc () in
      Html.set_doc_model doc [ct];
      doc;;

(*
  Eventually some stream-handling capabilities should
  be added here for memory-efficiency.
*)

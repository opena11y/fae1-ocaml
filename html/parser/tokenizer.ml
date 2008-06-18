open Html
open Htmldtd
open Scanner

type token = {
  mutable value : string;
};;

let free_text_flag = ref false;;

let set_free_text_flag () = free_text_flag := true;;
let clear_free_text_flag () = free_text_flag := false;;

let current_tag_name = ref "";;
let set_current_tag_name s = current_tag_name := s;;
let clear_current_tag_name () = current_tag_name := "";;

let get_value t = t.value;;

let create_token s = {value = s;};;

type htmlToken =
    StartTag of token |
        EndTag of token |
            Attribute of token |
                AttributeValue of token |
                    Entity of token |
                        Whitespace of token |
                            Comment of token |
                                Newline |
                                    Text of token |
                                        SpecialMarkup of token |
                                            ProcessingInstruction of token |
                                                EOF;;

let print_token t =
  match t with
      StartTag tok -> print_string ("<"^tok.value^">\n")
    | EndTag tok -> print_string ("</"^tok.value^">\n")
    | Attribute tok -> print_string ("Attr: "^tok.value ^ "\n")
    | AttributeValue tok -> print_string ("= "^tok.value^"\n")
    | Entity tok -> print_string ("&"^tok.value^";\n")
    | Whitespace tok -> print_string ("")
    | Comment tok -> print_string ("<!-- "^tok.value^"-->\n")
    | Newline -> print_string ("Newline\n")
    | Text tok -> print_string ("Text:"^tok.value ^ "\n")
    | SpecialMarkup tok -> print_string ("<! "^tok.value^" >\n")
    | ProcessingInstruction tok -> print_string ("<?"^tok.value^">\n")
    | EOF -> print_string "----End of File----\n";;


let is_ascii_alpha c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');;

let is_ascii_space c =
  c = ' ' || c = '\t';;

let is_ascii_digit c =
  (c >= '0') && (c <= '9');;

let end_tag_check n =
  Htmldtd.is_empty n;;

let rec consume_text scn tokens =
  let term_chars = ['\n';'\r';'<';'&';'\000'] in
  let pred c = List.mem c term_chars in
    Scanner.next_until scn pred;
    let t = Text { value=(Scanner.token_buf scn); } in
      Scanner.clear_token_buf scn;
      tokens@[t]

and consume_entity scn tokens =
  Scanner.set_marker scn;
  (* We saw an '&' coming into this, skip it... *)
  Scanner.skip scn;
  let pred c =
    not (c = '#' || is_ascii_alpha c || is_ascii_digit c) in
    Scanner.next_until scn pred;
    let next_char = Scanner.peek scn in
      if next_char = ';'
      then (
        let t = Entity {value=(Scanner.token_buf scn);} in
          Scanner.skip scn;
          Scanner.clear_token_buf scn;
          tokens@[t]
      )
      else (Scanner.reset_to_marker scn;
            (* Get the ampersand *)
            Scanner.next scn;
            consume_text scn tokens)

and consume_markup scn tokens =
  (* Set a marker and skip the < *)
  Scanner.set_marker scn;
  Scanner.skip scn;


  let next_char = Scanner.peek scn in
    if (next_char = '!')
    then (
      Scanner.skip scn;
      let next_char2 = Scanner.peek scn in
        if next_char2 = '-'
        then (
          Scanner.skip scn;
          if('-' = Scanner.peek scn) then (Scanner.skip scn);
          consume_comment scn tokens
        )
        else (consume_special_markup scn tokens)
    )
    else if (next_char = '?')
    then consume_processing_instructions scn tokens
    else if (next_char = '/')
    then consume_end_tag scn tokens
    else if (is_ascii_alpha next_char)
    then consume_start_tag scn tokens
    else (
      Scanner.reset_to_marker scn;
      Scanner.next scn;
      consume_text scn tokens)

and consume_processing_instructions scn tokens =
  Scanner.skip scn;
  let pred c = ((c = '>') || (c = '\000')) in
    Scanner.next_until scn pred;
    let next_char = Scanner.peek scn in
      if next_char = '\000'
      then (
        Scanner.reset_to_marker scn;
        Scanner.next scn;
        Scanner.next scn;
        tokens
      )
      else (
        Scanner.skip scn;
        let t = ProcessingInstruction {value=Scanner.token_buf scn;} in
          tokens@[t]
      )

and consume_comment scn tokens =
  let dash_check c = ((c = '-') || (c = '\000')) in
  let n_char = Scanner.peek scn in
    while(not (Scanner.peek scn = '>') &&
            not (Scanner.peek scn = '\000')) do
      Scanner.next_until scn dash_check;
      Scanner.set_marker scn;
      Scanner.skip scn;
      let skip_spaces c = not (is_ascii_space c) in
        Scanner.skip_until scn skip_spaces;
        let next_char = Scanner.peek scn in
          if(not (next_char = '>') && not (next_char = '\000'))
          then (Scanner.reset_to_marker scn; Scanner.next scn)
    done;
    let n_char = Scanner.peek scn in
      if(n_char = '\000')
      then (Scanner.reset_to_marker scn;
            Scanner.reset_to_marker scn;
            Scanner.next scn; Scanner.next scn;
            [])
      else (
        Scanner.skip scn;
        let t = Comment {value=Scanner.token_buf scn;} in
          Scanner.clear_token_buf scn;
          tokens@[t])

and consume_special_markup scn tokens =
  let pred c = ((c = '>') || (c = '\000') || (c = '<')) in
    Scanner.next_until scn pred;

    let n_char = Scanner.peek scn in
      if(n_char = '\000')
      then (Scanner.reset_to_marker scn;
            Scanner.next scn; Scanner.next scn;
            [])
      else if n_char = '<'
      then []
      else (
        Scanner.skip scn;
        let t = SpecialMarkup {value=Scanner.token_buf scn;} in
          Scanner.clear_token_buf scn;
          tokens@[t])

and consume_end_tag scn tokens =
  Scanner.skip scn;
  let pred c = not ((is_ascii_alpha c) || (is_ascii_digit c)) in
    Scanner.next_until scn pred;
    Scanner.set_marker scn;
    let t = EndTag {value=String.uppercase (Scanner.token_buf scn);} in
    let pred2 c = (c = '>') || (c = '<') || (c = '\000') in
      Scanner.skip_until scn pred2;
      let next_char = Scanner.peek scn in
        if(next_char = '>')
        then (Scanner.skip scn)
        else if next_char = '\000'
        then (Scanner.next scn);
        tokens@[t]


and consume_start_tag scn tokens =
  let pred c = not ((is_ascii_alpha c) || (is_ascii_digit c)) in
    Scanner.next_until scn pred;
    Scanner.set_marker scn;
    (* We should now have the tag's name... *)
    let t_name = String.uppercase (Scanner.token_buf scn) in
    let t = StartTag {value=t_name} in
    let et = EndTag {value=t_name} in
    let n_tokens = consume_whitespace scn (tokens@[t]) in
      set_current_tag_name t_name;

      if (t_name = "SCRIPT" || t_name = "STYLE")
      then (set_free_text_flag ());

      let next_char = Scanner.peek scn in
        if next_char = '>'
        then (Scanner.skip scn; clear_current_tag_name ();
              if end_tag_check (t_name)
              then n_tokens@[et]
              else n_tokens
             )
        else if next_char = '<'
        then (clear_current_tag_name (); n_tokens)
        else if next_char = '/' && (Scanner.peek_n scn 2 = '>')
        then (Scanner.skip scn;
              Scanner.skip scn;
              clear_current_tag_name ();
              n_tokens@[et])
        else if next_char = '\000'
        then (Scanner.next scn;
              clear_free_text_flag ();
              clear_current_tag_name ();
              tokens)
        else (  Scanner.clear_token_buf scn;
                consume_attribute scn (n_tokens))

and consume_attribute scn tokens =
  (* We don't care about ascii spaces *)
  let tkns = consume_whitespace scn tokens in
  let slash_found = ref false in
  let pred c = ((c = '>') || (c = '<') ||
                  (c = '=') || (is_ascii_space c) ||
                  (c = '\000') ) in
    while(not (pred (Scanner.peek scn))) do
      let pred2 c = (pred c) || (c = '/') in
        Scanner.next_until scn pred2;
        if('/' = Scanner.peek scn)
        then (
          if((Scanner.peek_n scn 2) = '>')
          then (slash_found := true; Scanner.skip scn)
          else Scanner.next scn;
        )

    done;
    let att_name = String.lowercase (Scanner.token_buf scn) in
    let t = Attribute {value=att_name;} in
      Scanner.clear_token_buf scn;
      let next_char = Scanner.peek scn in
        if next_char = '>'
        then (Scanner.skip scn;
              if (end_tag_check (!current_tag_name)) ||
                (!slash_found)
              then tokens@[t;(EndTag {value=(!current_tag_name)})]
              else tokens@[t]
             )
        else if next_char = '<'
        then (
          if end_tag_check (!current_tag_name)
          then tokens@[t;(EndTag {value=(!current_tag_name)})]
          else tokens@[t]
        )
        else if next_char = '\000'
        then (Scanner.next scn; tokens)
        else if next_char = '='
        then (Scanner.skip scn; consume_attribute_value scn (tokens@[t]))
        else consume_attribute scn (tokens@[t])

and consume_attribute_value scn tokens =
  let n_tokens = consume_whitespace scn tokens in
  let start_char = Scanner.peek scn in
    if start_char = '<' || start_char = '>' || start_char = '\000'
    then (
      if end_tag_check (!current_tag_name)
      then tokens@[(EndTag {value=(!current_tag_name)})]
      else tokens
    )
    else if start_char = '/' && Scanner.peek_n scn 2 = '>'
    then (Scanner.skip scn;
          Scanner.skip scn;
          tokens@[(EndTag {value=(!current_tag_name)})]
         )
    else (
      if start_char = '"' || start_char = '\''
      then (  Scanner.skip scn;
              let pred c = (c = start_char || c = '\000') in
                Scanner.next_until scn pred;
                if(not ('\000' = Scanner.peek scn))
                then Scanner.skip scn;
           )
      else (
        let pred c = (is_ascii_space c) || (c = '<') || (c = '>') ||
          (c = '\000') ||
          (c = '/' && Scanner.peek_n scn 2 = '>') in
          Scanner.next_until scn pred;
      );
      let next_char = Scanner.peek scn in
      let t = AttributeValue {value=Scanner.token_buf scn;} in
        Scanner.clear_token_buf scn;
        consume_attribute scn (tokens@[t])
    )



and consume_whitespace scn tokens =
  let next_char = Scanner.peek scn in
    if next_char = '\r' || next_char = '\n'
    then (Scanner.skip scn;
          consume_whitespace scn tokens@[Newline])
    else if next_char = '\t' || next_char = ' '
    then (Scanner.skip scn;
          consume_whitespace scn tokens)
    else tokens

(* Special handler for text regions like script and
   style tags *)
and consume_special_text scn tokens =
  clear_free_text_flag ();
  let pred c = (c = '<') || (c = '\000') in
    while (not (pred (Scanner.peek scn))) do
      Scanner.next_until scn pred;
      let ch = Scanner.peek scn in
      let ch2 = Scanner.peek_n scn 2 in
        (* Eventually we should check for the
           actual tag name, but for now assume
           that if we see the start of an end tag,
           we are good to go... *)
        if not ((ch = '<' && ch2 = '/')
                || (ch = '\000'))
        then Scanner.next scn;
    done;

    let s = Scanner.token_buf scn in
    let new_t = Text {value = s;} in
      Scanner.clear_token_buf scn;
      Scanner.clear_markers scn;
      tokens@[new_t];;

let consume_next_tokens scn =
  let next_char = Scanner.peek scn in
  let next_tokens =
    if (!free_text_flag)
    then consume_special_text scn []
    else if next_char = '<'
    then consume_markup scn []

    (* Entities are dumb... - D. Linder (05/12/2005) *)
    else if next_char = '&'
    then consume_entity scn []
    else if next_char = '\n' || next_char = '\r'
    then (Scanner.skip scn; [Newline])
    else if next_char = '\000'
    then (Scanner.skip scn; [EOF])
    else consume_text scn []
  in
    clear_current_tag_name ();
    Scanner.clear_markers scn;
    Scanner.clear_token_buf scn;
    Scanner.clear_skips scn;
    next_tokens;;



let rec consume_tokens scn token_lst =
  if not (Scanner.eof scn)
  then (
    let new_tokens = consume_next_tokens scn  in
      consume_tokens scn (token_lst@new_tokens)
  )
  else
    token_lst;;

let tokenize_string s =
  let scn = Scanner.create_scanner () in
    Scanner.load_string scn s;
    let tokens = consume_tokens scn [] in
      tokens;;

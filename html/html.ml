(**
   Types and functions for handling HTML markup
*)

open Str

exception Attribute_Not_Found;;

(**
   type used to represent attributes found in
   HTML tags. HTML attributes are considered as
   having the form key=value with this type.
*)
type attribute = {
  mutable key : string;
  mutable value : string;
};;

(** Get the key of attribute a *)
let attr_key a = a.key;;

(** Get the value of attribute a *)
let attr_value a = a.value;;

(**
   type used to represent HTML tags in wamt.
   name is the tags name, and the entire string
   should always be uppercase. attributes represent
   a list of type attribute associated with the tag.
   line number indicates the line number this tag was
   found at in the original file. The children list
   represents all the children of this tag, and parent
   is the tag's parent in the tree (there can be only one!)
*)
type 'a tag = {
  mutable name : string;
  mutable attributes : attribute list;
  mutable line_number : int;
  mutable children : 'a list;
  mutable parent : 'a;
};;

(** Constructor for the tag type *)
let create_tag n attr l ch p =
  { name = n;
    attributes = attr;
    line_number = l;
    children = ch;
    parent = p;};;

(** Get tag t's name *)
let tag_name t = t.name;;

(** Get tag t's attributes *)
let tag_attributes t = t.attributes;;

(** Get tag t's line number *)
let tag_line_number t = t.line_number;;

(** Get tag t's children *)
let tag_children t = t.children;;

(** Get tag t's parent *)
let tag_parent t = t.parent;;

(**
   Checks to see if tag t has an attribute with key
   equal to the string attr. Returns true if such an
   attribute exists, false otherwise.
*)
let has_attribute t attr =
  let rec f lst =
    match lst with
        hd::tl -> if (hd.key = attr) then true
        else f tl
      | [] -> false
  in
    f (t.attributes);;

(**
   Returns the attribute (of attribute type) in
   the tag t where the key of the attribute equals
   the string attr. If no such attribute is found in
   the tag, an exception Attribute_Not_Found is thrown
*)
let get_attribute t attr =
  let rec f lst =
    match lst with
        hd::tl -> if (hd.key = attr) then hd
        else f tl
      | [] -> raise Attribute_Not_Found
  in
    f (t.attributes);;

(**
   If tag t has attribute with key attr, return the
   attribute value; otherwise return an empty string.
*)
let get_attribute_value t attr =
  try
    let a = get_attribute t attr in
      attr_value a
  with Attribute_Not_Found -> "";;

(**
   Return get_attribute_value t attr with space
   character padding if non-null.
*)
let get_attribute_value_with_padding t attr =
  let value = get_attribute_value t attr in
    if value = ""
    then value
    else " " ^ value ^ " ";;

(**
   Checks to see if the tag t contains an attribute
   with key attr where the value of the attribute is
   not the empty string. Returns true if such an
   attribute exists, false otherwise.
*)
let has_non_blank_attribute t attr =
  try
    let a = get_attribute t attr in
      if a.value = ""
      then false
      else true
  with _ -> false

(**
   Checks to see if the tag t contains an attribute
   with key attr where the value of the attribute is
   equal to attr_value. Returns true if such an attribute
   exists, false otherwise.
*)
let has_attribute_with_value t attr attr_value =
  try
    let a = get_attribute t attr in
      if String.uppercase a.value = String.uppercase attr_value
      then true
      else false
  with _ -> false

(**
   The type htmlItem is used to represent the major "entities"
   defined in the W3C HTML4.01 spec.
   Tag - An individual tag, containing it's attributes,
   child htmlItems, etc. See the type tag for more
   information.
   Entity - Represents HTML entities (&xxx;)
   Text - Ordinary text that should be treated as content.
   SpecialMarkup -
   ProcessingInstruction -
   Comment - <!-- xxx -->
   NULL - An empty htmlItem. Used here for processing
   convenience in certain operations.
*)
type htmlItem =
    Tag of htmlItem tag
  | Entity of string
  | Text of string
  | SpecialMarkup of string
  | ProcessingInstruction of string
  | Comment of string
  | NULL;;

(**
   Given an attribute a, return the string representation
   of that attribute (key and value).
*)
let attribute_to_string a =
  if a.value = ""
  then a.key
  else (a.key^"='"^a.value^"'");;

(**
   Given a list, a function, and a delimiter, fold the list
   using the string function, concatenating the list and placing
   the string delim between each pair of strings when folding.
*)
let fold_to_string (lst : 'a list) (str_fn : 'a -> string) (delim : string)  =
  let f a b =
    (str_fn a) ^ delim ^ b
  in
    List.fold_right f lst "";;

(**
   Given an htmlItem, return the string representation of
   that htmlItem. Apply padding_str to the left side
   of the string.
*)
let rec htmlItem_to_string padding_str h =
  match h with
      Tag t -> (
        let n_s = t.name in
        let attr_s = fold_to_string (t.attributes) (attribute_to_string) " " in
        let ch_s = fold_to_string (t.children) (htmlItem_to_string (padding_str^"  ")) "" in
          Printf.sprintf "\n%s<%s %s>%s%s</%s>\n" padding_str n_s  attr_s ch_s padding_str n_s
      )
    | Entity s -> (padding_str^"&"^s^";")
    | Text s -> (padding_str^s)
    | SpecialMarkup s -> (padding_str^"<! "^s^">\n")
    | ProcessingInstruction s -> (padding_str^"<? "^s^" ?>\n")
    | Comment s -> (padding_str^"<!-- "^s^" -->\n")
    | NULL -> "";;

(**
   Type htmlDoc is used to hold the overall tree (WAMT-DOM) of
   htmlItems. As well, htmlDoc contains other convenience structures,
   such as tag_tbl, a hashtable with html tag names as keys, and
   lists of all associated html tags as values for each key.
*)
type htmlDoc = {
  mutable doc_model :  htmlItem list;
  mutable tag_tbl : (string, (htmlItem tag) list) Hashtbl.t;

};;

(** Generic constructor for htmlDoc type *)
let create_htmlDoc () = {
  doc_model = [];
  tag_tbl = Hashtbl.create 5;
};;


(** Get the doc_model of htmlDoc h *)
let doc_model h = h.doc_model;;

(** Get the tag_tbl of htmlDoc h *)
let tag_tbl h = h.tag_tbl;;

(** Set the doc_model of htmlDoc h to htmlItem list d *)
let set_doc_model h d = h.doc_model <- d;;

(** Set the tag_tbl of htmlDoc h to the hashtable t *)
let set_tag_tbl h t = h.tag_tbl <- t;;

(**
   Given an htmlDoc, fill the htmlDoc's tag_tbl by
   identifying all tags in htmlDoc's doc_model.
*)
let generate_tag_table h =
  let tbl = h.tag_tbl in
  let rec f node =
    match node with
        Tag t ->
          if (Hashtbl.mem tbl t.name)
          then (
            let lst = Hashtbl.find tbl t.name in
              Hashtbl.replace tbl t.name (lst@[t])
          )
          else (Hashtbl.add tbl t.name [t]);
          List.iter f t.children;
      |_ -> () ;
  in
    List.iter f (h.doc_model);;

(**
   Given an htmlDoc, print the associated tag_tbl to
   standard output.
*)
let print_tag_table h =
  let f a b c =
    (Printf.sprintf "%s%s: %d\n"
       c a (List.length b))
  in
    print_string (Hashtbl.fold f h.tag_tbl "");;

(**
   Given an htmlItem parent, apply the htmlItem child to
   the tree where the root node is parent. Certain rules
   apply to certain htmlItem types, for example, if the parent
   is Text and the child is Text, combine the two rather than
   appending the child as a child of the parent.
*)
let append_child parent child =
  (match parent with
       Tag t -> (
         match child with
             Text x -> if List.length t.children > 0
             then (
               match (List.nth t.children
                        ((List.length t.children) - 1)) with
                   Text s -> (
                     let lst = List.rev t.children in
                       t.children <- List.rev ([(Text (s^x))]@List.tl lst);
                   );
                 | _ -> t.children <- t.children@[child];
             )
             else t.children <- t.children@[child];
           | NULL -> ();
           | _ -> t.children <- t.children@[child];
       )
     | _ -> ());
  parent

(** Given a tag t, add attribute attr to the tag *)
let add_attribute tag attr =
  (match tag with
       Tag t -> t.attributes <- t.attributes@[attr];
     |_     -> ());
  tag;;

(**
   Create an attribute where the key is equal to
   string k and the value is equal to string v.
*)
let create_attribute k v = {key=k;value=v;};;

(** Get the name of tag t *)
let get_tag_name t =
  match t with
      Tag tg -> tag_name tg
    |_ -> "";;

(**
   Given a node (an htmlItem), fold over
   the tree in a depth-first manner
*)
let rec fold_node f a b =
  match b with
      hd::tl -> (
        let children =
          match hd with
              Tag t -> tag_children t
            | _ -> []
        in
          fold_node (f) (f a hd) (children@tl)
      )
    | [] -> a;;

(**
   Calculate the weight of the content (Text and Entity types)
   in the trees where the roots of the trees are provided in n_lst.
*)
let rec get_node_content_weight w n_lst =
  match n_lst with
      hd::tl -> (
        match hd with
            Tag t -> let children = tag_children t in
              get_node_content_weight w (children@tl)
          | Entity s -> get_node_content_weight (w + 1) tl
          | Text s -> get_node_content_weight (w + (String.length s)) tl
          | _ -> get_node_content_weight w tl
      )
    | [] -> w;;

(** Replace characters that are considered special in XML. *)
let escapetoxml s =
  global_replace (regexp "&") "&amp;"
    (global_replace (regexp "<") "&lt;"
      (global_replace (regexp ">") "&gt;" s));;

(**
   Return a string representation of the content of a list of trees
   where the roots of those trees are provided in n_lst.
*)
let rec get_node_content c n_lst =
  match n_lst with
      hd::tl -> (
        match hd with
            Tag t -> let children = tag_children t in
              get_node_content c (children@tl)
          | Entity s -> get_node_content (c^"&"^s^";") tl
          | Text s -> get_node_content (c^(escapetoxml s)) tl
          | _ -> get_node_content c tl
      )
    | [] -> c;;

(**
   Return the text content of node list n_lst with the addition, in
   document order, of any ALT text found in any IMG tags.
*)
let rec get_node_content_with_img_alt c n_lst =
  match n_lst with
      hd::tl -> (
        match hd with
            Tag t -> let children = tag_children t in
              if t.name = "IMG"
              then get_node_content_with_img_alt (c^(get_attribute_value_with_padding t "alt")) (children@tl)
              else get_node_content_with_img_alt c (children@tl)
          | Entity s -> get_node_content_with_img_alt (c^"&"^s^";") tl
          | Text s -> get_node_content_with_img_alt (c^(escapetoxml s)) tl
          | _ -> get_node_content_with_img_alt c tl
      )
    | [] -> c;;

(**
   Given a tag child, check to see if the tag has an ancestor
   with the name parent_name.
*)
let rec has_parent_element child parent_name =
  let p = child.parent in
    match p with
      | Tag t -> if (t.name = parent_name)
        then true
        else has_parent_element t parent_name
      | _ -> false;;

(**
   Given a list of nodes and the name of an attribute, create
   a list of all nodes in the trees with roots in the list of nodes
   that have attributes with the key attr_name.
*)
let rec collect_elements_with_attr attr_name node_lst e_lst =
  match node_lst with
      hd::tl -> (
        match hd with
            Tag t ->
              let children = tag_children t in
                if has_attribute t attr_name
                then collect_elements_with_attr attr_name
                  (tl@children)
                  ([t]@e_lst)
                else collect_elements_with_attr attr_name
                  (tl@children)
                  (e_lst)
          | _ -> collect_elements_with_attr attr_name tl e_lst
      )
    | [] -> e_lst;;

(**
   Given a list of tags, count how many tags in that list
   have the attribute with key attr. If empty_allowed is true,
   an attribute with value set to the empty string will be
   counted, otherwise the tag will not be considered to have
   the attribute, even if the attribute key matches attr.
*)
let count_elements_with_attribute tag_lst attr empty_allowed =
  (* define predicate on which to filter input list *)
  let pred t =
    try
      let a = get_attribute t attr in
        (empty_allowed || not ((attr_value a) = ""))
    with _ -> false
  in
    (* get sublist of elements that satisfy predicate *)
  let lp = List.filter (pred) tag_lst in
  let count = List.length lp in
  let total = List.length tag_lst in
  let ratio =
    if (total = 0)
    then 0.0
    else (float_of_int count) /. (float_of_int total)
  in
    (ratio, count, total);;

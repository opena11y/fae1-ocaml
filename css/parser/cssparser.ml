open Tokenizer
open Css

module Cssparser =
  struct

    exception Parser_error;;
    exception Parser_stack_out_of_bounds;;

    type parser_stack = {
         mutable token_stack : Tokenizer.cssToken list;
         mutable current_pos : int;
    };;

    let debug_flag = true;;

    let d_msg s =
       if debug_flag
       then  (Printf.printf "DEBUG: %s\n" s);
      ;; 

    let create_parser_stack (tokens) = {token_stack = tokens; current_pos = 0;};;

    let ts_is_empty ps =
        ps.current_pos >= List.length ps.token_stack;;
    
    let get_pos ps =
        ps.current_pos;;

    let current_token ps =
        try
           List.nth ps.token_stack ps.current_pos
        with _ -> Tokenizer.Eof;;

    let advance_pos ps =
        ps.current_pos <- ps.current_pos + 1;;

    let go_to_pos ps pos =
        ps.current_pos <- pos;;
    

    let rec parse_stylesheet ps =
        d_msg "parse_stylesheet";
        let start_pos = get_pos ps in
        if (ts_is_empty ps)
        then ((true, []))
        else (
          let next_token_p = current_token ps in
            match next_token_p with
              Tokenizer.Cdo
            | Tokenizer.Cdc -> (
                 ignore(advance_pos ps);
                 parse_stylesheet ps
              )
            | _ -> (
                 let (stmt_success, stmt_lst) = 
                     p_statement ps in
                 if stmt_success
                 (* If we found a statement, return it, along
                    with all the other statements in the remaining
                    stylesheet (to be parsed) *)
                 then (
                  let (ss_success, css_lst) =
                     parse_stylesheet ps in
                    if(ss_success)
                    then (true, (stmt_lst@css_lst))
                    else (true,  stmt_lst)
                 )
                 (* If the statement failed, skip the token and 
                    try the rest of the stylesheet *)
                 else (
                    ignore(go_to_pos ps start_pos);
                    ignore(advance_pos ps);
                    parse_stylesheet ps
                 )
            )
        )
        and p_statement ps = 
            d_msg "p_statement";
            let start_pos = get_pos ps in
            (* Try as a ruleset *)
            let (success, css_lst) = p_ruleset ps in
                if(success)
                then(
                   (true, css_lst)
                )
                else (
                  ignore(go_to_pos ps start_pos);
            (* Try as an at-rule *)
                  (false, [])
                )
        and p_ruleset ps =
            d_msg "p_ruleset";
            let start_pos = get_pos ps in
            
            (* ruleset: selector_set ruleset_1 | ruleset_1 *)
            let (selector_success, selector_list) =
               p_selector_set ps in

            let ruleset_start_pos = get_pos ps in
            let (ruleset_1_success, property_list) =
               p_ruleset_1 ps in

            (* If both were good, return a fully-developed ruleset *)
            if selector_success && ruleset_1_success
            then (
               (true, [Css.RuleSet(selector_list, property_list)])
            )

            (* If the ruleset failed, but the selector succeeded,
               return an empty ruleset with the selector list. Note
               don't reset the position to the start of the ruleset. *)
            else if selector_success && (not ruleset_1_success)
            then (true, [Css.RuleSet(selector_list, [])])

            (* If the selector failed, but the rulset succeeded,
               return the ruleset with an empty selector set. *)
            else if (not selector_success) && ruleset_1_success
            then (true, [Css.RuleSet([],property_list)])
            
            (* If both failed, then fail *)
            else (ignore(go_to_pos ps start_pos); (false, []))

        and p_ruleset_1 ps = 
            d_msg "p_ruleset_1";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
            d_msg ("next token: "^(Tokenizer.cssToken_toString next_token));

            (* We must find an opening curly brace! *)
            match (next_token) with
              Tokenizer.OpenCurly -> (ignore(advance_pos ps); p_ruleset_2 ps)
            | x -> (false, [])

        and p_ruleset_2 ps =
            d_msg "p_ruleset_2";

            (* ruleset_2: declaration ruleset_3 | ruleset_3 *)
            let start_pos = get_pos ps in
            let (declaration_success, dec_list_1) =
                p_declaration ps in
            let ruleset_3_start_pos = get_pos ps in
            let (ruleset_3_success, dec_list_2) = 
                p_ruleset_3 ps in

            (* Return all successful declarations. If there were no
               successful declarations, fail. *)
            if declaration_success && ruleset_3_success
            then (true, (dec_list_1@dec_list_2))

            else if declaration_success && (not ruleset_3_success)
            then (true, dec_list_1)

            else if (not declaration_success) && ruleset_3_success
            then (true,dec_list_2)

            else (ignore(go_to_pos ps start_pos); (false, []))

        and p_ruleset_3 ps = 
            d_msg "p_ruleset_3"; 
            let start_pos = get_pos ps in
            match (current_token ps) with
              Tokenizer.SemiColon -> (
                 ignore(advance_pos ps);
                 p_ruleset_2 ps
              )
            | Tokenizer.CloseCurly -> (ignore(advance_pos ps); (true, []))
            | _ -> (ignore(go_to_pos ps start_pos); (false, []))

        and p_combinator ps = 
            d_msg "p_combinator";
            let start_pos = get_pos ps in
            let t = current_token ps in
            ignore(advance_pos ps);
            match (t) with
              Tokenizer.Plus -> (true, Css.Adjacent(Css.Universal, Css.Universal))
            | Tokenizer.Greater -> (true, Css.Child(Css.Universal, Css.Universal))
            | x -> (ignore(go_to_pos ps start_pos); (false, Css.Universal))


        (* Returns a list of selectors, where selectors are
           delimited by commas. *)
        and p_selector_set ps = 
            d_msg "p_selector_set";
            let start_pos = get_pos ps in
            let (selector_success, selector_1) =
                p_selector ps in
            if selector_success
            then (
               let selector_set_1_start_pos = get_pos ps in
               let (selector_set_1_success, selector_lst_2) =
                   p_selector_set_1 ps in
               if selector_set_1_success
               then (
                 (* Combine the selector with the returned selector list *)
                 (true, [selector_1]@selector_lst_2)
               )
               else (
                 (* Otherwise just return the selector *)
                 (true, [selector_1])
               )
            )
            else (go_to_pos ps start_pos; (false, []))

        (* Returns a list of selectors, where selectors are
           delimited by commas. This subparser starts with a
           comma. *)
        and p_selector_set_1 ps =
            d_msg "p_selector_set_1";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
            match next_token with
              Tokenizer.Comma -> (
                  ignore(advance_pos ps);
                  let (selector_set_success, selector_lst) = 
                      p_selector_set ps in
                  if selector_set_success
                  then (true, selector_lst)
                  else ( ignore(go_to_pos ps start_pos);
                         (* Skip the comma *)
                         ignore(advance_pos ps); 
                         (false, []))
              )
            | _ -> (false, [])

        (* This returns s single selector that is a either a 
           simple selector, or a set of simple selectors. *)
        and p_selector ps = 
            d_msg "p_selector";
            let start_pos = get_pos ps in
            let (simple_selector_success, simple_s) =
                p_simple_selector ps in
            if simple_selector_success
            then (
               let selector_1_start_pos = get_pos ps in
               let (selector_1_success, selector) =
                   p_selector_1 ps in
               if selector_1_success
               then (
                   match selector with
                     Css.Child(s1,s2) -> (true, Css.Child(simple_s,s2))
                   | Css.Adjacent(s1,s2) -> (true, Css.Adjacent(simple_s,s2))
                   | Css.Descendant(s1,s2) -> (true, Css.Descendant(simple_s,s2))
                   | _ -> (print_string "Error matching selector in p_selector";
                           raise Parser_error;)
               )
               else (
                   ignore(go_to_pos ps selector_1_start_pos);
                   (true, simple_s)
               )
            )
            else (ignore(go_to_pos ps start_pos); (false, Css.Universal))

        (* This returns, in all cases, a set of simple selectors.
           At this point the grammar acts as follows:
            combinator simple_selector selector_1
            CombinatorType(Universal,Descendant(simple_selector, selector_1))

            combinator simple_selector
            CombinatorType(Universal,simple_selector)

            simple_selector selector_1
            Descendant(Universal, selector_1Type(simple_selector, selector_1Val))

            simple_selector
            Descendant(Universal, simple_selector) *)
        and p_selector_1 ps = 
            d_msg "p_selector_1";
            let start_pos = get_pos ps in
            let (combinator_success, combinator) = 
                p_combinator ps in
            if combinator_success 
            then (
                let simple_selector_start_pos = get_pos ps in
                let (simple_selector_success, simple_s) =
                    p_simple_selector ps in
                if simple_selector_success
                then (
                   let selector_1_start_pos = get_pos ps in
                   let (selector_1_success, selector_1) =
                       p_selector_1 ps in
                   if selector_1_success
                   then (
                     let right_selector = (
                       match selector_1 with
                         Css.Adjacent(s1,s2) -> Css.Adjacent(simple_s,s2)
                       | Css.Child(s1,s2) -> Css.Child(simple_s, s2)
                       | Css.Descendant(s1,s2) -> Css.Descendant(simple_s, s2)
                       | _ -> (print_string "Error matching selector_1 in combinator portion of p_selector_1.\n"; raise Parser_error;)
                     ) in
                     match combinator with 
                       Css.Adjacent(s1,s2) -> 
                           (true, Css.Adjacent(s1,right_selector))
                     | Css.Child(s1,s2) ->
                           (true, Css.Child(s1,right_selector))
                     | _ -> (print_string "Error matching combinator in combinator portion of p_selector_1.\n"; raise Parser_error;)
                   )
                   else (
                     ignore(go_to_pos ps selector_1_start_pos);
                     match combinator with
                       Css.Adjacent(s1,s2) -> (true, Css.Adjacent(simple_s,s2))
                     | Css.Child(s1, s2) -> (true, Css.Child(simple_s,s2))
                     | _ -> (print_string "Error matching combinator in p_selector_1"; raise Parser_error;)
                   )
                )
                else (
                   ignore(go_to_pos ps start_pos);
                   (* Ignore the combinator *)
                   ignore(advance_pos ps);
                   (false, Css.Universal)
                )
            )
            else (
                ignore(go_to_pos ps start_pos);
                let (simple_selector_success, simple_s) =
                    p_simple_selector ps in
                if simple_selector_success
                then (
                   let selector_1_start_pos = get_pos ps in
                   let (selector_1_success, selector) =
                       p_selector_1 ps in
                   if selector_1_success
                   then (
                      let right_selector = (
                          match selector with
                            Css.Child(s1,s2) -> Css.Child(simple_s, s2)
                          | Css.Adjacent(s1,s2) -> Css.Adjacent(simple_s, s2)
                          | Css.Descendant(s1,s2) ->
                                      Css.Descendant(simple_s, s2)
                          | _ -> (print_string "Error matching selector in p_selector_1";
                                  raise Parser_error;)

                      ) in
                      (true, Css.Descendant(Css.Universal, right_selector))
                   )
                   else (
                      ignore(go_to_pos ps selector_1_start_pos);
                      (true, Css.Descendant(Css.Universal, simple_s))
                   )
                )
                else (
                   (* Failed to match anything... *)
                   ignore(go_to_pos ps start_pos);
                   (false, Css.Universal)
                )
            )
            

        (* Returns either a Type of Universal selector *)
        and p_element_name ps = 
            d_msg "p_element_name";
            let start_pos = get_pos ps in
            let t = current_token ps in
              match t with
                Tokenizer.Ident s -> (
                             ignore(advance_pos ps);
                             (true, Css.Type s)
                          )
              | Tokenizer.Aster -> (
                             ignore(advance_pos ps);
                             (true,Css.Universal)
                          )
              | _ -> (false, Css.Universal)

        (* Returns one of the following selectors:
           element_name
              Universal or Type
           element_name ss_1
              ss_1Type(element_name,ss_1_value)
           ss_1
              Hash, Class, Attribute, and Pseudo
        *)
        and p_simple_selector ps = 
            d_msg "p_simple_selector";
            let start_pos = get_pos ps in
            let (element_name_success, element) =
                p_element_name ps in
            if element_name_success
            then (
               let ss_1_start_pos = get_pos ps in
               let (ss_1_success, ss_1) = 
                   p_ss_1 ps in
               if ss_1_success
               then (
                    match ss_1 with
                      Css.Id(s1, s2) -> (true, Css.Id(element,s2))
                    | Css.Attribute(s1,s2) -> (true, Css.Attribute(element,s2))
                    | Css.Class(s1,s2) -> (true, Css.Class(element,s2))
                    | Css.Pseudo(s1,s2) -> (true, Css.Pseudo(element, s2))
                    | _ -> (print_string "Error matching ss_1 in simple_selector.\n"; raise Parser_error;)
               )
               else (
                    ignore(go_to_pos ps ss_1_start_pos);
                    (true, element)
               )
            )
            else (
                ignore(go_to_pos ps start_pos);
                let (ss_1_success, ss_1) = 
                    p_ss_1 ps in
                if ss_1_success
                then (
                   (* Just return what we found *)
                   (true, ss_1)
                )
                else (
                     (* Nothing matched... *)
                     ignore(go_to_pos ps start_pos);
                     (false, Css.Universal)
                )
            )

        (* Returns one of the following selectors:
           ss_2 ss_1
              ss_2Type(Universal, ss1_Type(ss2val, ss1val))
           ss_2
              ss2_Type(Universal,ss_2value), in other words, ss_2 *)
        and p_ss_1 ps = 
            d_msg "p_ss_1";
            let start_pos = get_pos ps in
            let (ss_2_success, ss_2) = 
                p_ss_2 ps in
            if ss_2_success
            then (
               d_msg "Entering p_ss_1 recursively...";
               let ss_1_start_pos = get_pos ps in
               let (ss_1_success, ss_1) = 
                   p_ss_1 ps in
               if ss_1_success
               then (
                  let right_selector = (
                      match ss_1 with
                        Css.Id(s1,s2) -> Css.Id(ss_2,s2)
                      | Css.Class(s1,s2) -> Css.Class(ss_2,s2)
                      | Css.Pseudo(s1,s2) -> Css.Pseudo(ss_2,s2)
                      | Css.Attribute(s1,s2) -> Css.Attribute(ss_2,s2)
                      | _ -> (print_string "Error matching right selector in p_ss_1";
                              raise Parser_error;)
                  ) in
                  match ss_2 with
                    Css.Id(s1,s2) ->
                        (true, Css.Id(Css.Universal,right_selector))
                  | Css.Class(s1,s2) ->
                        (true, Css.Class(Css.Universal,right_selector))
                  | Css.Pseudo(s1,s2) ->
                        (true, Css.Pseudo(Css.Universal,right_selector))
                  | Css.Attribute(s1,s2) -> 
                        (true, Css.Attribute(Css.Universal,right_selector))
                  | _ -> (print_string "Error matching ss_2 in p_ss_1.\n";
                          raise Parser_error;)
               )
               else (
                  ignore(go_to_pos ps ss_1_start_pos);
                  (true, ss_2)
               )
            )
            else (
               ignore(go_to_pos ps start_pos);
               (false, Css.Universal)
            )

        (* Returns one of the following types of selectors:
           Class(Universal,classname)
           Id(Universal,hashname)
           Attribute(Universal, attrib)
           Pseudo(Universal, pseudo_string) *)
        and p_ss_2 ps =
            d_msg "p_ss_2";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
                ignore(advance_pos ps);
            (* First try to match an Id selector *)
            match next_token with
              Tokenizer.Hash s -> (true, Css.Id(Css.Universal, Css.Name s))
            | _ -> (
                ignore(go_to_pos ps start_pos);
                let (class_success, class_selector) = 
                    p_class ps in
                if class_success
                then (
                         (true, class_selector)
                )
                else (
                   ignore(go_to_pos ps start_pos);
                   let (attrib_success, attrib) = 
                       p_attrib ps in
                   if attrib_success
                   then (
                         (true, Css.Attribute(Css.Universal, 
                                              Css.AttributeSet attrib))
                   )
                   else (
                      ignore(go_to_pos ps start_pos);
                      let (pseudo_success, pseudo) = 
                          p_pseudo ps in
                      if pseudo_success
                      then (
                         (true, Css.Pseudo(Css.Universal,Css.Name pseudo))
                      )
                      else (
                         (* Nothing matched, fail *)
                         ignore(go_to_pos ps start_pos);
                         (false, Css.Universal)
                      )
                   )
                )
            )
       
        (* class: Period class_1
            Returns Class(Universal, Name(class_1)) *)
        and p_class ps = 
            d_msg "p_class";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
                match next_token with 
                  Tokenizer.Period -> (
                     ignore(advance_pos ps);
                     let (class_1_success, class_1) = 
                         p_class_1 ps in
                     if class_1_success
                     then (
                        (true, Css.Class(Css.Universal,Css.Name class_1))
                     )
                     else (
                        (* Failed to match, skip the period *)
                        ignore(go_to_pos ps start_pos);
                        ignore(advance_pos ps);
                        (false, Css.Universal)
                     )
                  )
                | _ -> (false, Css.Universal)

        (* Returns a string *)
        and p_class_1 ps = 
            d_msg "p_class_1";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
            match next_token with
               Tokenizer.Ident s -> (
                            ignore(advance_pos ps);
                            (true, s)
                         )
            | _ -> (false, "")

        (* :pseudo_1 :
           Returns a string *)
        and p_pseudo ps = 
            d_msg "p_pseudo";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
            match next_token with
              Tokenizer.Colon -> (
                            ignore(advance_pos ps);
                            let (pseudo_1_success, pseudo_1) = 
                                p_pseudo_1 ps in
                            if pseudo_1_success 
                            then (
                               (true, pseudo_1)
                            )
                            else (
                               ignore(go_to_pos ps (start_pos + 1));
                               (false, "")
                            )
                        )
            | _ -> (false, "")
            

        (* Returns a string *)
        and p_pseudo_1 ps = 
            d_msg "p_pseudo_1";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
            match next_token with
              Tokenizer.Ident s -> (
                           ignore(advance_pos ps);
                           (true, s)
                        )
            | Tokenizer.Function s -> (
                           ignore(advance_pos ps);
                           let (pseudo_2_success, pseudo_2) =
                               p_pseudo_2 ps in
                           if pseudo_2_success
                           then (
                              (true, (s^pseudo_2))
                           )
                           else (
                              (* Assume the function is simply incomplete,
                                 and continue on... *)
                              (ignore(go_to_pos ps (start_pos + 1)));
                              (true, s)
                           )
                        )
            | _ -> (false, "")

        (* Returns a string *)
        and p_pseudo_2 ps =
            d_msg "p_pseudo_2";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
            match next_token with
              Tokenizer.Ident s -> (
                   ignore(advance_pos ps);
                   let (pseudo_3_success, pseudo_3) =
                       p_pseudo_3 ps in
                   if pseudo_3_success
                   then (
                     (true, ("("^s^")"))
                   )
                   else (
                     ignore(go_to_pos ps (start_pos + 1));
                     (true, ("("^s^")"))
                   )         
              )
            | _ -> (
              let (pseudo_3_success, pseudo_3) =
                  p_pseudo_3 ps in
              if pseudo_3_success
              then (
                (true, "()")
              )
              else (
                (false, "")
              )
            )
 
        (* Returns empty *)
        and p_pseudo_3 ps = 
            d_msg "p_pseudo_3";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
            match next_token with
              Tokenizer.CloseParens -> (ignore(advance_pos ps);(true, []))
            | _ -> (* Again, we'll just assume that the function is closed *)
                   (true, [])

        (* Returns an attr_selector *)
        and p_attrib ps = 
            d_msg "p_attrib";
            let start_pos = get_pos ps in
            (false, (Css.create_attr_selector "" "" 0))

        (* Ident attrib_2 
           Sets the attr in attrib_2 to ident *)
        and p_attrib_1 ps = 
            d_msg "p_attrib_1";
            let start_pos = get_pos ps in
            (true, ())

        (* Equals attrib_3
           Sets as_type in attrib_3 to 1
           
           Includes attrib_3
           Sets as_type in attrib_3 to 2

           Dashmatch attrib_3
           Sets as_type in attrib_3 to 3

           attrib_4
           Sets as_type in attrib_4 to 0 *)
        and p_attrib_2 ps = 
            d_msg "p_attrib_2";
            let start_pos = get_pos ps in
            (true, [])

        (* Ident attrib_4
           Sets value in attrib_4 to Ident string 

           String attrib_4
           Sets value in attrib_4 to String string *)
        and p_attrib_3 ps = 
            d_msg "p_attrib_3";
            let start_pos = get_pos ps in
            (true, [])

        (* Returns true or false depending on match, along with
           an empty attribute selector. *)
        and p_attrib_4 ps = 
            d_msg "p_attrib_4";
            let start_pos = get_pos ps in
            (true, [])

        and p_declaration ps = 
            d_msg "p_declaration";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
              if (Tokenizer.typeof (next_token, Tokenizer.Delim 'c'))
              then (ignore(advance_pos ps));
            let (p_declaration_1_success, p_declaration_1_lst) =
                  p_declaration_1 ps in
            if p_declaration_1_success
            then (true, p_declaration_1_lst)
            else (ignore(go_to_pos ps start_pos); (false, []))

        and p_declaration_1 ps =
            d_msg "p_declaration_1";
            let start_pos = get_pos ps in
            let (property_success, property_lst) =
                 p_property ps in

            if property_success
            then (
               let (Css.String prop_name) = List.hd property_lst in

               let declaration_2_start_pos = get_pos ps in
               let (declaration_2_success, value_lst) =
                    p_declaration_2 ps in
               if declaration_2_success
               then (
                 let (Css.String value_string) = List.hd value_lst in
                 let new_prop = Css.create_property prop_name value_string in
                   (true, [(new_prop)])
               )
               else (ignore(go_to_pos ps declaration_2_start_pos);
                     (true, [(Css.create_property prop_name "")]))
            )
            else (
               (ignore(go_to_pos ps start_pos); (false, []))
            )
            

        and p_declaration_2 ps = 
            d_msg "p_declaration_2";
            let start_pos = get_pos ps in
            let t = current_token ps in
                ignore(advance_pos ps);
            match (t) with
              Tokenizer.Colon -> (
                let value_start_pos = get_pos ps in
                let (value_success, value_lst) =
                   p_value ps in
                if value_success
                then (true, value_lst)
                else (ignore(go_to_pos ps value_start_pos); 
                      (true, [Css.String ""]))
              )
            | x -> (ignore(go_to_pos ps start_pos); (false, []))

        and p_property ps = 
            d_msg "p_property";
            let start_pos = get_pos ps in
            let t = current_token ps in
                ignore(advance_pos ps);
                d_msg (Tokenizer.cssToken_toString t);
            match (t) with
              Tokenizer.Ident s -> (
                 (true, [Css.String s])
              )
            | x -> (ignore(go_to_pos ps start_pos); (false, []))
            
        and p_value ps = 
            d_msg "p_value";
            let start_pos = get_pos ps in
            let (value_1_success, value_1_lst) = 
                p_value_1 ps in
            if value_1_success
            then (
               let value_start_pos = get_pos ps in
               let (value_success, value_lst) =
                   p_value ps in
               if value_success
               then (
                  let (Css.String left_value) = List.hd value_1_lst in
                  let (Css.String right_value) = List.hd value_lst in
                  let value_string = (left_value^" "^right_value) in
                  (true, [Css.String value_string])
               )
               else (
                  ignore(go_to_pos ps value_start_pos);
                  let (Css.String value_string) = List.hd value_1_lst in
                  (true, [Css.String value_string])
               )
            )
            else (
               ignore(go_to_pos ps start_pos);
               (true, [Css.String("")])
            )

        and p_value_1 ps = 
            d_msg "p_value_1";
            let start_pos = get_pos ps in
            let (any_success, any_lst) = 
                p_any ps in
            if any_success
            then (true, any_lst)
            else (ignore(go_to_pos ps start_pos); (false, []))

        and p_any ps = 
            d_msg "p_any";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
                ignore(advance_pos ps);
            match next_token with 
              Tokenizer.Percentage s -> (true, [Css.String (s^"%")])
            | Tokenizer.Hash s -> (true, [Css.String ("#"^s)])
            | Tokenizer.Ident s
            | Tokenizer.Dimension s
            | Tokenizer.String s
            | Tokenizer.Uri s
            | Tokenizer.Whitespace s
            | Tokenizer.UnicodeRange s
            | Tokenizer.Number s -> (true, [Css.String s])
            | Tokenizer.Delim c -> (true, [Css.String (String.make 1 c)]) 
            | Tokenizer.Period -> (true, [Css.String "."])
            | Tokenizer.Comma -> (true, [Css.String ","])
            | Tokenizer.Equals -> (true, [Css.String "="])
            | Tokenizer.Aster -> (true, [Css.String "*"])
            | Tokenizer.Greater -> (true, [Css.String ">"])
            | Tokenizer.Plus -> (true, [Css.String "+"])
            | Tokenizer.Function s -> (
                  let (any_func_success, any_func) =
                      p_any_func ps in
                  if any_func_success
                  then (
                      (true, [Css.String (s^"("^any_func)])
                  )
                  else (
                      (true, [Css.String (s^"()")])
                  )
              )
            | Tokenizer.OpenParens -> (true, [])
            | Tokenizer.OpenSquare -> (true, [])
            | _ -> (ignore(go_to_pos ps start_pos); (false, []))
            
        and p_any_func ps = 
            d_msg "p_any";
            let start_pos = get_pos ps in
            let next_token = current_token ps in
            match next_token with
              Tokenizer.CloseParens -> (ignore(advance_pos ps); (true, ")"))
            | _ -> (
              let (any_success, any) =
                  p_any ps in
              if any_success
              then (
                  let (Css.String any_s) = List.hd any in
                  let any_func_start_pos = get_pos ps in
                  let (any_func_success, any_func) = 
                       p_any_func ps in
                  if any_func_success
                  then (
                     (true, (any_s^" "^any_func))
                  )
                  else (
                      ignore(go_to_pos ps any_func_start_pos);
                      (true, (any_s^")"))
                  )
              )
              else (
                 ignore(go_to_pos ps start_pos);
                 (true, ")")
              )
            )
        and p_any_parens ps = (true, [])
        and p_any_square ps = (true, [])
    ;;

    let rec strip_tokens tokens stripped_lst = 
        match tokens with
	  (Tokenizer.Comment c)::tl -> strip_tokens tl stripped_lst
        | (Tokenizer.Whitespace s)::tl -> strip_tokens tl stripped_lst
        | hd::tl -> strip_tokens tl (stripped_lst@[hd])
        | [] -> stripped_lst;;

    let parse_string s =
        let tokens = Tokenizer.tokenize_string s in
        Printf.printf "initial size: %d\n" (List.length tokens);
        let tokens_cleared = strip_tokens tokens [] in
        Printf.printf "next size: %d\n" (List.length tokens);
        let ps = create_parser_stack (tokens_cleared) in
        let (result, stylesheet) = parse_stylesheet ps in
        stylesheet;;


  end;;

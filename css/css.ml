module Css =
  struct
    type property = {
       mutable name : string;
       mutable value : string ;
    };;
   
    let create_property n v = {name = n; value = v};;
 
    let property_name p = p.name;;
    let property_value p = p.value;;

    let property_to_string prop = 
        Printf.sprintf "        %s: %s;\n" prop.name prop.value;;

    type attr_selector = {
         mutable attr : string;
         mutable value : string;
         mutable as_type : int;
         (* Type is 0 for any
                    1 for exact
                    2 for space-separated
                    3 for hyphen-separated *)
    };;

    let create_attr_selector a v t = {attr=a;value=v;as_type=t;};;

    let attr_selector_to_string attr_sel = "";;

    type selector =
         Universal
       | Name of string (* For id's, pseudo, and hashes *)
       | AttributeSet of attr_selector (* For attribute selectors *)
       | Type of string
       | Class of selector * selector
       | Descendant of selector * selector
       | Child of selector * selector
       | Adjacent of selector * selector
       | Attribute of selector * selector
       | Id of selector * selector
       | Pseudo of selector * selector;;

    let rec selector_to_string sel = 
       match sel with
         Universal -> "*"
       | Name s
       | Type s -> s
       | Class(s1,s2) -> Printf.sprintf "%s.%s"
                         (selector_to_string s1)
                         (selector_to_string s2)
       | Descendant(s1,s2) -> 
                         Printf.sprintf "%s %s"
                         (selector_to_string s1)
                         (selector_to_string s2)
       | Child(s1,s2) -> 
                         Printf.sprintf "%s>%s"
                         (selector_to_string s1)
                         (selector_to_string s2)
       | Adjacent(s1,s2) -> 
                         Printf.sprintf "%s+%s"
                         (selector_to_string s1)
                         (selector_to_string s2)
       | AttributeSet a -> ("["^(attr_selector_to_string a)^"]")
       | Attribute(s1,s2) -> Printf.sprintf "%s%s"
                                   (selector_to_string s1)
                                   (selector_to_string s2)
       | Id(s1,s2) -> Printf.sprintf "%s#%s" 
                      (selector_to_string s1)
                      (selector_to_string s2)
       | Pseudo(s1,s2) -> 
                         Printf.sprintf "%s:%s"
                         (selector_to_string s1)
                         (selector_to_string s2) ;;

    type statement = 
         AtRule of string * string
       | AtRuleBlock of string * string * statement list
       | RuleSet of selector list * property list
       | Block of statement list;;

    let rec statement_to_string stmt = 
        match stmt with
          AtRule(s1,s2) -> ""
        | AtRuleBlock(s1,s2,lst) -> ""
        | RuleSet(selectors, properties) -> 
            let f a b =
              (a^", "^(selector_to_string b))
            in
            let g x y =
              (x^" "^(property_to_string y))
            in
            Printf.sprintf "%s {\n%s}\n" (List.fold_left f "" selectors)
                                       (List.fold_left g "" properties)
        | Block(lst) -> ""
    ;;

    type stylesheet = statement list;;


    type cssType = Stylesheet of stylesheet |
                   Statement of statement |
                   Selector of selector |
                   Prop of property |
                   String of string;;

    let rec stylesheet_to_string ss = 
        let f a b = 
            (a^(statement_to_string b))
        in
           match ss with
             Stylesheet lst -> List.fold_left f "" lst 
           | _ -> "" ;;
 
    let add_to_ruleset r t pre=
        let RuleSet(sl,pl) = r in
        match t with
          Selector s -> if pre then (RuleSet([s]@sl,pl)) 
                               else (RuleSet(sl@[s],pl))
        | Prop p -> if pre then (RuleSet(sl,[p]@pl))
                               else (RuleSet(sl,pl@[p]))
        |_ -> r;;


  end;;

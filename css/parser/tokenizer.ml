open Scanner
open Stringlib

module Tokenizer = 
  struct
    type cssToken = 
       Ident of string
     | AtKeyword of string
     | String of string
     | Invalid of string
     | Hash of string
     | Number of string
     | Percentage of string
     | Dimension of string
     | Uri of string
     | UnicodeRange of string
     | Cdo
     | Cdc
     | SemiColon
     | OpenCurly
     | CloseCurly
     | OpenParens
     | CloseParens
     | OpenSquare
     | CloseSquare
     | Whitespace of string
     | Comment of string
     | Function of string
     | Includes
     | Dashmatch
     | Period
     | Greater
     | Plus
     | Aster
     | Equals
     | Comma
     | Colon
     | Delim of char
     | Eof
    ;;


    let typeof = function
                       (Ident s1, Ident s2) -> true
                     | (AtKeyword s1, AtKeyword s2) -> true
                     | (String s1, String s2) -> true
                     | (Invalid s1, Invalid s2) -> true
                     | (Hash s1, Hash s2) -> true
                     | (Number s1, Number s2) -> true
                     | (Percentage s1, Percentage s2) -> true
                     | (Dimension s1, Dimension s2) -> true
                     | (Uri s1, Uri s2) -> true
                     | (UnicodeRange s1, UnicodeRange s2) -> true
                     | (Cdo , Cdo ) -> true
                     | (Cdc , Cdc ) -> true
                     | (SemiColon , SemiColon ) -> true
                     | (OpenCurly , OpenCurly ) -> true
                     | (CloseCurly , CloseCurly ) -> true
                     | (OpenParens , OpenParens ) -> true
                     | (CloseParens , CloseParens ) -> true
                     | (OpenSquare , OpenSquare ) -> true
                     | (CloseSquare , CloseSquare ) -> true
                     | (Whitespace s1, Whitespace s2) -> true
                     | (Comment s1, Comment s2) -> true
                     | (Function s1, Function s2) -> true
                     | (Includes , Includes ) -> true
                     | (Dashmatch , Dashmatch ) -> true
                     | (Period, Period) -> true
                     | (Greater, Greater) -> true
                     | (Comma, Comma) -> true
                     | (Plus, Plus) -> true
                     | (Aster, Aster) -> true
                     | (Colon, Colon) -> true
                     | (Equals, Equals) -> true
                     | (Delim c, Delim c2) -> true
                     | (Eof, Eof) -> true
                     | _ -> false
    ;;



   let cssToken_toString t = 
       match t with 
         Ident s -> ("Ident: "^s)
       | AtKeyword s -> ("AtKeyword: @"^s)
       | String s -> ("String: "^s)
       | Invalid s -> ("Invalid: "^s)
       | Hash s -> ("Hash: #"^s)
       | Number s -> ("Number: "^s)
       | Percentage s -> ("Percentage: "^s^"%")
       | Dimension s -> ("Dimension: "^s)
       | Uri s -> ("URI: "^s)
       | UnicodeRange s -> ("UR: "^s)
       | Cdo -> "<!--"
       | Cdc -> "-->"
       | SemiColon -> ";"
       | OpenCurly -> "{"
       | CloseCurly -> "}"
       | OpenParens -> "("
       | CloseParens -> ")"
       | OpenSquare -> "["
       | CloseSquare -> "]"
       | Whitespace s -> ("Whitespace: "^s)
       | Comment s -> ("Comment: "^s)
       | Function s -> ("Function: "^s)
       | Includes -> "~="
       | Dashmatch -> "|="
       | Period -> "."
       | Greater -> ">"
       | Plus -> "+"
       | Aster -> "*"
       | Equals -> "="
       | Comma -> ","
       | Colon -> ":"
       | Delim c -> Stringlib.string_of_char c
       | Eof -> ""
   ;;

   let is_ascii_alpha c =
        (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');;

   let is_ascii_space c =
        c = ' ' || c = '\t';;

   let is_ascii_newline c =
        c = '\r' || c = '\n';;

   let is_ascii_digit c =
        (c >= '0') && (c <= '9');;

   let scan_unicode_macro scn = 
       Scanner.set_marker scn;
       let rec unicode_chars count =
          if count = 6 
          then (
             true
          )
          else (
            let c = Scanner.peek scn in
              if (c >= '0' && c <= '9') || (c >= 'a' || c <= 'f')
              then (
                 Scanner.next scn;
                 unicode_chars (count + 1)
              )
              else (
                 false
              )
          )
       in
       let char1 = Scanner.peek scn in
       (* First check for the \ *)
       if(char1 = '\\')
       then (
          Scanner.next scn;
          (* Now read in the hex chars *)
          if (unicode_chars 0)
          then (
             (* Check for a newline *)
             let char2 = Scanner.peek scn in
                 if(char2 = '\r')
                 then (
                    Scanner.next scn;
                    let char3 = Scanner.peek scn in
                        if char3 = '\n' then (Scanner.next scn)
                 )
                 else if (char2 = '\n' ||  
                          char2 = '\t' || char2 = ' '
                 )
                 then (
                    Scanner.next scn
                 );
             (* Go ahead and return true now *)
             Scanner.clear_marker scn;
             true
          )
          else (
             Scanner.reset_to_marker scn;
             false
          )
          
       )
       else (
          Scanner.reset_to_marker scn;
          false
       )
       
       ;;

   let scan_escape_macro scn = 
       Scanner.set_marker scn;
       if(scan_unicode_macro scn)
       then (
          Scanner.clear_marker scn;
          true
       )
       else (
          let char1 = Scanner.peek scn in
          if(char1 = '\\')
          then ( 
             Scanner.next scn;
             let char2 = Scanner.peek scn in
                 if (not (char2 = '\n' || char2 = '\r' ||
                          (char2 >= '0' && char2 <= '9') ||
                          (char2 >= 'a' && char2 <= 'f') )
                 )
                 then (
                    Scanner.next scn;
                    Scanner.clear_marker scn;
                    true
                 )
                 else (
                    Scanner.reset_to_marker scn;
                    false
                 )
          )
          else (
             Scanner.reset_to_marker scn;
             false
          )
       )
   ;;

   let scan_nonascii_macro scn = 
       let char1 = Scanner.peek scn in
       if(Char.code(char1) < 0 || Char.code(char1) > 127)
       then (Scanner.next scn; true)
       else (false)
   ;;

   let scan_num_macro scn = 
       Scanner.set_marker scn;
       let char1 = Scanner.peek scn in
       let check_char c = (c >= '0' && c <= '9') in
       let ncheck_char c = not (check_char c) in
           if(check_char char1)
           then (
             Scanner.next_until scn ncheck_char;
             let char2 = Scanner.peek scn in
                 if(char2 = '.')
                 then (
                   Scanner.set_marker scn;
                   Scanner.next scn;
                   if(check_char (Scanner.peek scn))
                   then (
                      Scanner.next_until scn ncheck_char;
                      Scanner.clear_marker scn;
                      Scanner.clear_marker scn;
                      true
                   )
                   else (
                      Scanner.reset_to_marker scn;
                      Scanner.clear_marker scn;
                      true
                   )
                 ) 
                 else (
                   Scanner.clear_marker scn;
                   true
                 )
           )
           else if(char1 = '.')
           then (
             Scanner.next scn;
             let char2 = Scanner.peek scn in
             if(check_char char2)
             then(
                 Scanner.next_until scn ncheck_char;
                 Scanner.clear_marker scn;
                 true
             )
             else (
                 Scanner.reset_to_marker scn;
                 false
             )
           )
           else (
             Scanner.reset_to_marker scn;
             false
           )
   ;;

   let scan_nl_macro scn = 
       let char1 = Scanner.peek scn in
       if char1 = '\r' 
       then (
          Scanner.next scn;
          let char2 = Scanner.peek scn in
              if char2 = '\n'
              then (Scanner.next scn; true)
              else true
       )
       else if ( char1 = '\n' )
       then ( Scanner.next scn; true)
       else false
   ;;
   let scan_w_macro scn =
       let check_char c = not (
           c = ' ' || c = '\t' || c = '\r' ||
           c = '\n' ) in
       Scanner.next_until scn check_char;
       true
   ;;

   let scan_string_middle scn e_ch = 
       let ch1 = Scanner.peek scn in
       if( not 
           (ch1 = '\n' || ch1 = '\r' || ch1 = '\\' || ch1 = e_ch)
       )
       then (Scanner.next scn; true)
       else (
            if (scan_escape_macro scn)
            then (true)
            else if (ch1 = '\\')
            then (
               Scanner.set_marker scn;
               Scanner.next scn;
               if(scan_nl_macro scn)
               then(Scanner.clear_marker scn; true)
               else(Scanner.reset_to_marker scn; false)
            )
            else (
              false
            )
       )

   ;;

   let scan_invalid1_macro scn = 
       Scanner.set_marker scn;
       let s_ch = Scanner.peek scn in
       if s_ch = '"'
       then (
          Scanner.skip scn;
          while (scan_string_middle scn '"') do ()
          done;
          let e_ch = Scanner.peek scn in
            if( not (e_ch = '"') )
            then (Scanner.clear_marker scn; true)
            else (Scanner.reset_to_marker scn; false)
       )
       else (
          Scanner.reset_to_marker scn;
          false
       )
   ;;
   let scan_invalid2_macro scn = 
       Scanner.set_marker scn;
       let s_ch = Scanner.peek scn in
       if s_ch = '\''
       then (
          Scanner.skip scn;
          while (scan_string_middle scn '\'') do ()
          done;
          let e_ch = Scanner.peek scn in
            if( not(e_ch = '\'') )
            then (Scanner.clear_marker scn; true)
            else (Scanner.reset_to_marker scn; false)
       )
       else (
          Scanner.reset_to_marker scn;
          false
       )
   ;;
   let scan_invalid_macro scn = 
       if (scan_invalid1_macro scn)
       then true
       else if (scan_invalid2_macro scn)
       then true
       else false
   ;;

   let scan_string1_macro scn = 
       Scanner.set_marker scn;
       let s_ch = Scanner.peek scn in 
       if s_ch = '"' 
       then (
          Scanner.skip scn;
          while (scan_string_middle scn '"') do ()
          done;
          let e_ch = Scanner.peek scn in
            if( e_ch = '"' )
            then (Scanner.clear_marker scn; true)
            else (Scanner.reset_to_marker scn; false)
       )
       else (
          Scanner.reset_to_marker scn;
          false
       )
   ;;
   let scan_string2_macro scn = 
       Scanner.set_marker scn;
       let s_ch = Scanner.peek scn in
       if s_ch = '\''
       then (
          Scanner.skip scn;
          while (scan_string_middle scn '\'') do ()
          done;
          let e_ch = Scanner.peek scn in
            if( e_ch = '\'' )
            then (Scanner.clear_marker scn; true)
            else (Scanner.reset_to_marker scn; false)
       )
       else (
          Scanner.reset_to_marker scn;
          false
       )

   ;;
   let scan_string_macro scn = 
       if(scan_string1_macro scn)
       then true
       else if (scan_string2_macro scn)
       then true
       else false
   ;;

   let scan_nmchar_macro scn = 
       let ch1 = Scanner.peek scn in
           if( ch1 = '_' || ch1 = '-' ||
               (ch1 >= 'a' && ch1 <= 'z') ||
               (ch1 >= 'A' && ch1 <= 'Z') ||
               (ch1 >= '0' && ch1 <= '9') )
           then (Scanner.next scn; true)
           else if (scan_nonascii_macro scn)
           then true
           else if (scan_escape_macro scn)
           then true
           else false
   ;;

   let scan_nmstart_macro scn = 
       let ch1 = Scanner.peek scn in
           if( ch1 = '_' || ch1 = '-' ||
               (ch1 >= 'a' && ch1 <= 'z') ||
               (ch1 >= 'A' && ch1 <= 'Z') )
           then (Scanner.next scn; true)
           else if (scan_nonascii_macro scn)
           then true
           else if (scan_escape_macro scn)
           then true
           else false

   ;;

   let scan_name_macro scn = 
       if(scan_nmchar_macro scn)
       then (
          while (scan_nmchar_macro scn) do () done;
          true
       )
       else (
          false
       )
   ;;

   let scan_ident_macro scn = 
       Scanner.set_marker scn;

       if (Scanner.peek scn) = '-'
       then Scanner.next scn;

       if(scan_nmstart_macro scn)
       then (
          while(scan_nmchar_macro scn) do () done;
          Scanner.clear_marker scn;
          true
       )
       else (
         Scanner.reset_to_marker scn;
         false
       )
   ;;

   let rec scan_comment_macro scn =
       if (Scanner.peek scn) = '/'
       then (
         Scanner.set_marker scn;
         Scanner.skip scn;
         if (Scanner.peek scn) = '*'
         then (
            Scanner.skip scn;
            let aster_detect c = c = '*' || c = '\000' in
            let rec f () =
                Scanner.next_until scn aster_detect;
                Scanner.set_marker scn;
                Scanner.skip scn;
                if(Scanner.peek scn) = '/'
                then (
                   Scanner.skip scn;
                   Scanner.clear_markers scn;
                   true
                )
                else if (Scanner.peek scn) = '\000'
                then (
                   Scanner.reset_to_marker scn;
                   Scanner.reset_to_marker scn;
                   false
                )
                else (
                   Scanner.reset_to_marker scn;
                   Scanner.next scn;
                   f ()
                )
            in
              f ()
         )
         else (
            Scanner.reset_to_marker scn;
            false
         )
       )
       else (
         false
       )
   ;;

   let consume_next_token scn = 
       Scanner.clear_markers scn;
       Scanner.clear_token_buf scn;
       Scanner.clear_skips scn;

       let ch1 = Scanner.peek scn in
           (*Printf.printf "char: %d | %c\n" (Char.code ch1) ch1;*)
           if (scan_ident_macro scn)
           then (
              let ident_string = Scanner.token_buf scn in
              let ch2 = Scanner.peek scn in
                  if (ch2 = '(' )
                  then (Scanner.skip scn; [Function ident_string])
                  else [Ident ident_string]
           )
           else if (ch1 = '@')
           then (
              Scanner.skip scn;
              if(scan_ident_macro scn)
              then (
                [AtKeyword (Scanner.token_buf scn)]
              )
              else ([])
           )
           else if (scan_string_macro scn)
           then (
                 [String (Scanner.token_buf scn)]
           )
           else if (scan_invalid_macro scn)
           then (
                 [Invalid (Scanner.token_buf scn)]
           )
           else if (ch1 = '#')
           then (
              Scanner.skip scn;
              if(scan_ident_macro scn)
              then (
                [Hash (Scanner.token_buf scn)]
              )
              else ([])
           )
           else if (scan_num_macro scn)
           then (
              let ch2 = Scanner.peek scn in
              if ch2 = '%' 
              then (Scanner.skip scn; [Percentage (Scanner.token_buf scn)])
              else if (scan_ident_macro scn)
              then ([Dimension (Scanner.token_buf scn)])
              else [Number (Scanner.token_buf scn)]
           )
           else if (ch1 = 'U')
           then (
              Scanner.set_marker scn;
              Scanner.next scn;
              let check_char c = 
                  (c >= '0' && c <= '9') ||
                  (c >= 'A' && c <= 'F')
              in

              let rec collect_chars count =
                  if count > 0
                  then (
                     if check_char (Scanner.peek scn)
                     then (
                        Scanner.next scn;
                        collect_chars (count - 1)
                     )
                     else ()
                  )
                  else ()
              in
              let ch2 = Scanner.peek scn in
                if ch2 = '+'
                then (
                   Scanner.next scn;
                   if check_char (Scanner.peek scn)
                   then (
                      collect_chars 6;
                      Scanner.set_marker scn;
                      if (Scanner.peek scn) = '-'
                      then (
                         Scanner.next scn;
                         if check_char (Scanner.peek scn)
                         then (
                            collect_chars 6;
                            let ts = Scanner.token_buf scn in
                              Scanner.clear_markers scn;
                              [UnicodeRange ts]
                            
                         )
                         else (
                            Scanner.reset_to_marker scn;
                            let ts = Scanner.token_buf scn in
                                Scanner.clear_markers scn;
                                [UnicodeRange ts]
                         )
                      )
                      else (
                         let ts = Scanner.token_buf scn in
                         Scanner.clear_markers scn;
                         [UnicodeRange ts]
                      )
                   )
                   else (
                      Scanner.reset_to_marker scn;
                      Scanner.skip scn;
                      []
                   )
                )
                else (
                  Scanner.reset_to_marker scn;
                  Scanner.skip scn;
                  []
                )
           )
           else if (ch1 = '<')
           then (
              print_string "cdo\n";
              Scanner.set_marker scn;
              Scanner.next scn; Scanner.next scn; Scanner.next scn;
              Scanner.next scn;
              let ts = Scanner.token_buf scn in
                  if ts = "<!--"
                  then ([Cdo])
                  else (
                     Scanner.reset_to_marker scn;
                     Scanner.skip scn;
                     []
                  )
           )
           else if (ch1 = '-')
           then (
             print_string "cdc\n";
             Scanner.skip scn; []
           )
           else if (ch1 = ';')
           then (
             print_string "semicolon\n";
             Scanner.skip scn;
             [SemiColon]
           )
           else if (ch1 = '{')
           then (
             print_string "open_curly\n";
             Scanner.skip scn;
             [OpenCurly]
           )
           else if (ch1 = '}')
           then (
             Scanner.skip scn;
             [CloseCurly]
           )
           else if (ch1 = ':')
           then (
             Scanner.skip scn;
             [Colon]
           )
           else if (ch1 = '(')
           then (
             Scanner.skip scn;
             [OpenParens]
           )
           else if (ch1 = ')')
           then (
             Scanner.skip scn;
             [CloseParens]
           )
           else if (ch1 = '[')
           then (
             Scanner.skip scn;
             [OpenSquare]
           )
           else if (ch1 = ']')
           then (
             Scanner.skip scn;
             [CloseSquare]
           )
           else if (ch1 = '.')
           then (
             Scanner.skip scn;
             [Period]
           )
           else if (ch1 = ',')
           then (
             Scanner.skip scn;
             [Comma]
           )
           else if (ch1 = '*')
           then (
             Scanner.skip scn;
             [Aster]
           )
           else if (ch1 = '>')
           then (
             Scanner.skip scn;
             [Greater]
           )
           else if (ch1 = '+')
           then (
             Scanner.skip scn;
             [Plus]
           )
           else if (ch1 = '=')
           then (
             Scanner.skip scn;
             [Equals]
           )

           else if (ch1 = '\n' || ch1 = '\t' || ch1 = '\r' || ch1 = ' ')
           then (
             print_string "dealing with whitespace...\n";
             if(scan_w_macro scn)
             then (
               let ts = Scanner.token_buf scn in
                   [Whitespace ts]
             )
             else (
               Scanner.skip scn;
               []
             )
                 
           )
           else if (scan_comment_macro scn)
           then (
             let ts = Scanner.token_buf scn in
             [Comment ts]
           )
           else if (ch1 = '~')
           then (
              Scanner.set_marker scn;
              Scanner.next scn;
              if (Scanner.peek scn) = '='
              then (
                 Scanner.next scn;
                 Scanner.clear_markers scn;
                 [Includes]
              )
              else (
                 Scanner.reset_to_marker scn;
                 Scanner.skip scn;
                 []
              )
           )
           else if (ch1 = '|')
           then (
              Scanner.set_marker scn;
              Scanner.next scn;
              if (Scanner.peek scn) = '='
              then (
                 Scanner.next scn;
                 Scanner.clear_markers scn;
                 [Dashmatch]
              )
              else (
                 Scanner.reset_to_marker scn;
                 Scanner.skip scn;
                 []
              )
           )
           else (Scanner.skip scn; [Delim ch1] )
   ;;


   let rec consume_tokens scn token_lst =
         if not (Scanner.eof scn)
         then (
           let new_tokens = consume_next_token scn in
             consume_tokens scn (token_lst@new_tokens)
         )
         else
           token_lst;;

   let tokenize_string s = 
         let scn = Scanner.create_scanner () in
         Scanner.load_string scn s;
         let tokens = consume_tokens scn [] in
         tokens;

  end;;
(*
let f s =
    print_string "go!!!\n";
    let g t = print_string ((Tokenizer.cssToken_toString t)^"\n") in
    let tokens = Tokenizer.tokenize_string s in
    List.iter g tokens;;

f "


";
*)

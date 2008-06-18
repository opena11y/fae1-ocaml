open Stringlib;;
open Filesys;;
open Tokenizer;;
open Cssparser;;
open Css;;

let file_list = Filesys.readdir_list_rec "/home/mkramer1/wamt/css/parser/css_tests/" in
let f a =
    print_string ("Evaluating the file: "^a^"\n");
    print_string "--------------------------------------\n";
    let file = Filesys.read_file a in
    let g t = print_string ((Tokenizer.cssToken_toString t)^"\n") in
    let tokens = Tokenizer.tokenize_string file in
    List.iter g tokens;
    print_string "--------------------------------------\n";
    print_string "Now trying to parse...\n";
    print_string "--------------------------------------\n";
    let parsed = Cssparser.parse_string file in 
        print_string (Css.stylesheet_to_string (Css.Stylesheet(parsed)));
        print_string ("\n\n"^(string_of_int(List.length parsed))^"\n");
    print_string "--------------------------------------\n";
in List.iter f file_list;;

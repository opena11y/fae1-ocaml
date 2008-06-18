open Scanner

type token = {
  mutable value : string;
};;

val get_value : token -> string;;

val create_token : string -> token;;

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

val tokenize_string : string -> htmlToken list;;

(* File: htmldtd.ml *)

type element = {
  name : string;
  allowed_children: string list;
};;


(* Text Markup *)
let font_style = ["TT";"I";"B";"U";"S";"STRIKE";"BIG";"SMALL"];;
let phrase = ["EM";"STRONG";"DFN";"CODE";"SAMP";"KBD";"VAR";"CITE";
              "ABBR";"ACRONYM"];;
let special = ["A";"IMG";"APPLET";"OBJECT";"FONT";"BASEFONT";"BR";
               "SCRIPT";"MAP";"Q";"SUB";"SUP";"SPAN";"BDO";"IFRAME"];;
let form_ctrl = ["INPUT";"SELECT";"TEXTAREA";"LABEL";"BUTTON"];;
let inline = font_style@phrase@special@form_ctrl;;

let pre_exclusion = ["IMG";"OBJECT";"APPLET";"BIG";"SMALL";"SUB";
                     "SUP";"FONT";"BASEFONT"];;

(* HTML Content Models *)
let heading = ["H1";"H2";"H3";"H4";"H5";"H6"];;
let list_e = ["UL";"OL";"DIR";"MENU"];;
let preformatted = ["PRE"];;

let block = ["P";"DL";"DIV";"CENTER";"NOSCRIPT";"NOFRAMES";
             "BLOCKQUOTE";"FORM";"ISINDEX";"HR";"TABLE";"FIELDSET";
             "ADDRESS"]@heading@list_e@preformatted;;

let flow = block@inline;;

(* Head Tags *)
let head_content = ["TITLE";"ISINDEX";"BASE"];;
let head_misc = ["SCRIPT";"STYLE";"META";"LINK";"OBJECT"];;

(* HTML Tags *)
let html_content = ["HEAD";"FRAMESET";"BODY"];;

(* Empty Tags *)
let empty_tags = ["BASEFONT";"BR";"AREA";"LINK";"IMG";"PARAM";
                  "HR";"INPUT";"COL";"FRAME";"ISINDEX";"BASE";
                  "META"];;

let is_empty t_name = List.mem t_name empty_tags;;

let exclude_tags lst ex_lst =
  let pred a = not (List.mem a ex_lst) in
    List.filter pred lst;;



let optional_end_tags = Hashtbl.create 8;;

let load_optional_end_tags () =
  Hashtbl.add optional_end_tags "DT" [["DT";"DD"];
                                      ["DL"]];;
Hashtbl.add optional_end_tags "DD" [["DD";"DT"];
                                    ["DL"]];;
Hashtbl.add optional_end_tags "LI" [["LI"];
                                    ["UL";"OL"]];;
Hashtbl.add optional_end_tags "OPTION" [["OPTION"];
                                        ["SELECT"]];;
Hashtbl.add optional_end_tags "P" [block;block];;
Hashtbl.add optional_end_tags "TR" [["TR"];["TBODY";"TABLE"]];;
Hashtbl.add optional_end_tags "TD" [["TD";"TH";"TR"];["TBODY";"TABLE"]];;

let optional_end_tag_match parent (child : string) start_or_end =
  if(Hashtbl.length optional_end_tags <= 0)
  then(load_optional_end_tags ());
  try
    let tgs = Hashtbl.find optional_end_tags parent in
      if(start_or_end = "s")
      then (
        List.mem child (List.nth tgs 0)
      )
      else (
        List.mem child (List.nth tgs 1)
      )
  with _ -> false

let tag_list =
  [{name="A";allowed_children=inline};
   {name="ABBR";allowed_children=inline};
   {name="ACRONYM";allowed_children=inline};
   {name="ADDRESS";allowed_children=["P"]@inline};
   {name="APPLET";allowed_children=["PARAM"]@flow};
   {name="AREA";allowed_children=[]};
   {name="B";allowed_children=inline};
   {name="BASE";allowed_children=[]};
   {name="BASEFONT";allowed_children=[]};
   {name="BDO";allowed_children=inline};
   {name="BIG";allowed_children=inline};
   {name="BLOCKQUOTE";allowed_children=flow};
   {name="BODY";allowed_children=flow@["INS";"DEL"]};
   {name="BR";allowed_children=[]};
   {name="BUTTON";
    allowed_children= (exclude_tags flow
                         (["A";"FORM";"ISINDEX";"FIELDSET";
                           "IFRAME"]@form_ctrl))
   };
   {name="CAPTION";allowed_children=inline};
   {name="CENTER";allowed_children=flow};
   {name="CITE";allowed_children=inline};
   {name="CODE";allowed_children=inline};
   {name="COL";allowed_children=[]};
   {name="COLGROUP";allowed_children=["COL"]};
   {name="DD";allowed_children=flow};
   {name="DEL";allowed_children=flow};
   {name="DFN";allowed_children=inline};
   {name="DIR";allowed_children=(["LI"]@block)};
   {name="DIV";allowed_children=flow};
   {name="DL";allowed_children=["DT";"DD"]};
   {name="DT";allowed_children=inline};
   {name="EM";allowed_children=inline};
   {name="FIELDSET";allowed_children=["LEGEND"]@flow};
   {name="FONT";allowed_children=inline};
   {name="FORM";allowed_children=flow};
   {name="FRAME";allowed_children=[]};
   {name="FRAMESET";allowed_children=["FRAMESET";"FRAME";"NOFRAMES"]};
   {name="H1";allowed_children=inline};
   {name="H2";allowed_children=inline};
   {name="H3";allowed_children=inline};
   {name="H4";allowed_children=inline};
   {name="H5";allowed_children=inline};
   {name="H6";allowed_children=inline};
   {name="HEAD";allowed_children=head_content@head_misc};
   {name="HR";allowed_children=[]};
   {name="HTML";allowed_children=html_content};
   {name="I";allowed_children=inline};
   {name="IFRAME";allowed_children=flow};
   {name="IMG";allowed_children=[]};
   {name="INPUT";allowed_children=[]};
   {name="INS";allowed_children=flow};
   {name="ISINDEX";allowed_children=[]};
   {name="KBD";allowed_children=inline};
   {name="LABEL";allowed_children=(exclude_tags inline ["LABEL"])};
   {name="LEGEND";allowed_children=inline};
   {name="LI";allowed_children=flow};
   {name="LINK";allowed_children=[]};
   {name="MAP";allowed_children=block@["AREA"]};
   {name="MENU";allowed_children=["LI"]};
   {name="META";allowed_children=[]};
   {name="NOFRAMES";allowed_children=["BODY"]@flow};
   {name="NOSCRIPT";allowed_children=flow};
   {name="OBJECT";allowed_children=["PARAM"]@flow};
   {name="OL";allowed_children=["LI"]};
   {name="OPTGROUP";allowed_children=["OPTION"]};
   {name="OPTION";allowed_children=[]};
   {name="P";allowed_children=inline};
   {name="PARAM";allowed_children=[]};
   {name="PRE";allowed_children=(exclude_tags inline pre_exclusion)};
   {name="Q";allowed_children=inline};
   {name="S";allowed_children=inline};
   {name="SAMP";allowed_children=inline};
   {name="SCRIPT";allowed_children=[]};
   {name="SELECT";allowed_children=["OPTGROUP";"OPTION"]};
   {name="SMALL";allowed_children=inline};
   {name="SPAN";allowed_children=inline};
   {name="STRIKE";allowed_children=inline};
   {name="STRONG";allowed_children=inline};
   {name="STYLE";allowed_children=[]};
   {name="SUB";allowed_children=inline};
   {name="SUP";allowed_children=inline};
   (* Note that the TR tag is not actually allowed in
      the TABLE tag, but most people don't use TBODY tags,
      so we'll allow it... *)
   {name="TABLE";allowed_children=["CAPTION";"COL";"COLGROUP";
                                   "THEAD";"TFOOT";"TBODY";"TR"]};
   {name="TBODY";allowed_children=["TR"]};
   {name="TD";allowed_children=flow};
   {name="TEXTAREA";allowed_children=[]};
   {name="TFOOT";allowed_children=["TR"]};
   {name="TH";allowed_children=flow};
   {name="THEAD";allowed_children=["TR"]};
   {name="TITLE";allowed_children=[]};
   {name="TR";allowed_children=["TD";"TH"]};
   {name="TT";allowed_children=inline};
   {name="U";allowed_children=inline};
   {name="UL";allowed_children=["LI"]};
   {name="VAR";allowed_children=inline}];;

let get_tag t_name =
  let pred a = a.name = t_name in
    List.find pred tag_list;;

let can_contain parent child =
  try
    let p_tag = get_tag parent in
      List.mem child p_tag.allowed_children
  with _ -> true;;

let full_401_tag_list =
  ["A";
   "ABBR";
   "ACRONYM";
   "ADDRESS";
   "APPLET";
   "AREA";
   "B";
   "BASE";
   "BASEFONT";
   "BDO";
   "BIG";
   "BLOCKQUOTE";
   "BODY";
   "BR";
   "BUTTON";
   "CAPTION";
   "CENTER";
   "CITE";
   "CODE";
   "COL";
   "COLGROUP";
   "DD";
   "DEL";
   "DFN";
   "DIR";
   "DIV";
   "DL";
   "DT";
   "EM";
   "FIELDSET";
   "FONT";
   "FORM";
   "FRAME";
   "FRAMESET";
   "H1";
   "H2";
   "H3";
   "H4";
   "H5";
   "H6";
   "HEAD";
   "HR";
   "HTML";
   "I";
   "IFRAME";
   "IMG";
   "INPUT";
   "INS";
   "ISINDEX";
   "KBD";
   "LABEL";
   "LEGEND";
   "LI";
   "LINK";
   "MAP";
   "MENU";
   "META";
   "NOFRAMES";
   "NOSCRIPT";
   "OBJECT";
   "OL";
   "OPTGROUP";
   "OPTION";
   "P";
   "PARAM";
   "PRE";
   "Q";
   "S";
   "SAMP";
   "SCRIPT";
   "SELECT";
   "SMALL";
   "SPAN";
   "STRIKE";
   "STRONG";
   "STYLE";
   "SUB";
   "SUP";
   "TABLE";
   "TBODY";
   "TD";
   "TEXTAREA";
   "TFOOT";
   "TH";
   "THEAD";
   "TITLE";
   "TR";
   "TT";
   "U";
   "UL";
   "VAR"];;

let is_valid_tag t_name =
  List.mem full_401_tag_list t_name;;

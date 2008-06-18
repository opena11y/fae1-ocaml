open Bytein

let file_signature () = [0xFF;0xD8;0xFF;0xE0;-1;-1;0x4A;0x46;0x49;0x46;0x00];;

let rec parse_ff ch =
  if(input_byte ch = 0xFF)
  then parse_secondbyte ch
  else parse_ff ch
and parse_secondbyte ch =
  let b = input_byte ch in
    if(b = 0xC0 || b = 0xC2)
    then parse_xy ch
    else parse_ff ch
and parse_xy ch =
  ignore(input_byte ch); ignore(input_byte ch); ignore(input_byte ch);
  let y = (input_byte ch) + (256 * (input_byte ch)) in
  let x = (input_byte ch) + (256 * (input_byte ch)) in
    (y,x);;

let get_dimensions ch =
  try
    let dims = parse_ff ch in
      close_in ch;
      dims
  with End_of_file -> (
    seek_in ch 0;
    (try
       while true do
         Printf.printf "%d\n" (input_byte ch);
       done;
     with End_of_file -> print_string "end\n");
    (0,0));;

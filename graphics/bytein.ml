let rec skip_bytes ch n =
  if n > 0
  then (ignore(input_byte ch); skip_bytes ch (n - 1);)
  else ();;

let input_bytes ch n =
  let rec f lst =
    if List.length lst < n
    then f (lst@[(input_byte ch)])
    else lst
  in
    f [];;

let dump_bytes ch =
  seek_in ch 0;
  try
    while true do
      Printf.printf "%d\n" (input_byte ch);
    done;
  with _ -> print_string "end\n";

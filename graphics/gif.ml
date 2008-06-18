(* Note we don't care about 87a vs. 89a at this point *)
let file_signature () = [71;73;70];;

let get_dimensions ch =
  let rec f lst =
    let b = input_byte ch in
      if(List.length lst < 10)
      then (f (lst@[b])) else lst
  in
  let bytes = f [] in
    close_in ch;
    let w_base = List.nth bytes 6 in
    let w_mult = List.nth bytes 7 in
    let h_base = List.nth bytes 8 in
    let h_mult = List.nth bytes 9 in
    let dim = (h_base + (256 * h_mult), w_base + (256 * w_mult)) in
      dim;;

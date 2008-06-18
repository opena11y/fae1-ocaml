(*
  type chunk = {
  mutable length : int;
  mutable ch_type : int;
  mutable data : int list;
  mutable crc : int;
  }

  let png_file = {
  mutable ihdr : chunk;
  mutable plte : chunk;
  mutable idat : chunk list;
  mutable iend : chunk;
  } ;;
*)
let file_signature () = [137;80;78;71;13;10;26;10];;

let get_dimensions ch =
  let rec f c =
    let b = input_byte ch in
      if (List.length c) < 24 then f (c@[b]) else c
  in
  let bytes = f [] in
  let width = ((List.nth bytes 16) * 16777216) +
    ((List.nth bytes 17) * 65536) +
    ((List.nth bytes 18) * 256) +
    (List.nth bytes 19)
  in
  let height = ((List.nth bytes 20) * 16777216) +
    ((List.nth bytes 21) * 65536) +
    ((List.nth bytes 22) * 256) +
    (List.nth bytes 23)
  in
    (height,width);;
